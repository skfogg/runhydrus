block_b_water_flow_info <- function(hydrus_model,
                                    main_processes = main_processes,
                                    iteration_criteria = iteration_criteria,
                                    water_flow_bcs = water_flow_bcs,
                                    soil_hydraulics = soil_hydraulics,
                                    particle_tracking = particle_tracking
                                    ){


  selector_template <- readLines(file.path(hydrus_model$project_path, "SELECTOR.IN"))

  ## Make parameter lookup tables:
  top_bc_opts <- data.frame(top_bc_name = c("constant_pressure_head", "constant_flux", "atm_bc_with_surface_layer", "atm_bc_with_surface_runoff",
                                            "variable_pressure_head", "variable_pressure_head/flux"),
                            TopInf = c("f","f","t","t","t","t"),
                            WLayer = c("f","f","t","f","f","f"),
                            KodTop = c("1","-1","-1","-1","1","0"),
                            InitCond = c("f","f","f","f","f","f"))

  bottom_bc_opts <- data.frame(bottom_bc_name = c("contant_pressure_head", "contant_flux", "variable_pressure_head",
                                                  "variable_flux", "free_drainage", "deep_drainage",
                                                  "seepage_face", "horizontal_drains"),
                               BotInf = c("f", "f", "t", "t", "f", "f", "f", "f"),
                               qGWLF = c("f","f","f","f","f","t","f","f"),
                               FreeD = c("f","f","f","f","t","f","f","f"),
                               SeepF = c("f","f","f","f","f","f","t","f"),
                               KodBot = c(" ","-"," ","-","-","-","-","-"),
                               DrainF = c("f","f","f","f","f","f","f","t"),
                               hSeep = c("0","0","0","0","0","0","0","0"))

  #### *** BLOCK B: WATER FLOW INFORMATION *** ####
  ### Iteration Criteria:
  selector_template[grep("MaxIt ",  selector_template) + 1] <- str_flatten(c(" ", " ",
                                                                             iteration_criteria$maximum_iterations,
                                                                             rep(" ", times = 4),
                                                                             iteration_criteria$water_content_tol,
                                                                             rep(" ", times = 6),
                                                                             iteration_criteria$pressure_head_tol))
  # --------------------------------------------------------------------
  ### Assign top boundary conditions: TopInf WLayer KodTop InitCond ####
  top_boundary <- str_split(selector_template[grep("TopInf", selector_template) + 1], "", simplify = T)
  top_bc_slots <- c(2, 8, 16, 24)

  ## get bc options from user chosen top bc in options data frame
  specified_top_bc <- unlist(top_bc_opts[top_bc_opts$top_bc_name == water_flow_bcs$upper_bc, 2:length(top_bc_opts)])

  if(water_flow_bcs$initial_condition){
    specified_top_bc["InitCond"] <- "t"
  }

  ## assign options values in correct slots
  if(str_count(specified_top_bc["KodTop"]) == 1){
    top_boundary[,15] <- " "
    top_boundary[,top_bc_slots] <- specified_top_bc
  }else{
    top_bc_slots <- c(2, 8, 15, 16, 24)
    top_boundary[,top_bc_slots] <- c(specified_top_bc[1:2], str_split(specified_top_bc["KodTop"], "", simplify = T), specified_top_bc[4])
  }

  ## update selector template with new values
  selector_template[grep("TopInf", selector_template) + 1] <- str_flatten(top_boundary)

  ## TO DO: when upper bc == 'constant_flux', add rTop rBot rRoot
  ## TO DO: when upper bc is 'atm bc with surface layer/runoff" and triggered irrigaiton on, add Node Pressure Irrig_rate Duration Lag_period

  # --------------------------------------------------------------------------------------
  ### Assign bottom boundary conditions: BotInf qGWLF FreeD SeepF KodBot DrainF hSeep ####
  bottom_boundary <- str_split(selector_template[grep("BotInf", selector_template) + 1], "", simplify = T)
  bottom_bc_slots <- c(2, 8, 14, 20, 26, 34, 40)

  ## get bc options from user chosen bottom bc in options data frame
  specified_bottom_bc <- unlist(bottom_bc_opts[bottom_bc_opts$bottom_bc_name == water_flow_bcs$lower_bc, 2:length(bottom_bc_opts)])

  ## assign options values in correct slots
  bottom_boundary[,bottom_bc_slots] <- specified_bottom_bc

  ## update selector template with new values
  selector_template[grep("BotInf", selector_template) + 1] <- str_flatten(bottom_boundary)

  # -------------------------------------
  ## Insert internal interpolation tables
  if(iteration_criteria$internal_interpolation_tables){
    c(selector_template[1:grep("BotInf", selector_template) + 1],
      str_flatten(rep(" ", times = 4), "hTab1", rep(" ", times = 3), "hTabN"),
      str_flatten(rep(" ", times = 5), iteration_criteria$lower_limit_tension_interval, rep(" ", times = 3), iteration_criteria$upper_limit_tension_interval),
      selector_template[grep("BotInf", selector_template) + 2:length(selector_template)]
    )
  }

  # ------------------------------------
  ## Insert particle tracking parameters
  if(main_processes$particle_tracking){
    c(selector_template[1:grep("BotInf", selector_template) + 1],
      str_flatten(c(rep(" ", times = 7), "wInit", rep(" ", times = 8), "wPrec")),
      str_flatten(c(rep(" ", times = 9), particle_tracking$init_water_storage, rep(" ", times = 10), particle_tracking$cumulative_surface_flux)),
      selector_template[grep("BotInf", selector_template) + 2:length(selector_template)]
    )
  }

  # ----------------------------------------
  ## Assign soil hydraulic model and hysteresis
  soil_model <- str_split(selector_template[grep("Hysteresis", selector_template) + 1], "", simplify = T)
  soil_model[1,c(7,18)] <- as.character(c(soil_hydraulics$soil_hydraulic_model, soil_hydraulics$hysteresis))
  selector_template[grep("Hysteresis", selector_template) + 1] <- str_flatten(soil_model)

  ## Insert Kappa parameter when hysteresis selected
  if(soil_hydraulics$hysteresis > 0){
    selector_template <- c(selector_template[1:grep("Hysteresis", selector_template) + 1],
                           str_flatten(c(rep(" ", times = 4), "Kappa")),
                           ifelse(soil_hydraulics$initially_drying_curve,
                                  str_flatten(c(rep(" ", times = 5), "-1")),
                                  str_flatten(c(rep(" ", times = 6), "1"))),
                           selector_template[grep("Hysteresis", selector_template) + 2:length(selector_template)]
    )
  }

  # ----------------------------------------------------------------
  ## Assign soil hydraulic property parameters ####
  ## NOTE: this creates three spaces between each number, where the original spacing is mode variable--I'm not sure if it will matter
  for(i in 1:hydrus_model$geometry$material_numbers){
    selector_template[selector_template[grep("thr", selector_template)]+i] <- paste("  ",
                                                                                    as.character(
                                                                                      unlist(soil_hydraulics$soil_hydraulic_parameters[i,2:length(soil_hydraulics$soil_hydraulic_parameters)])
                                                                                    ), collapse = "    ")
  }

  ## Update Selector.in
  writeLines(selector_template, file.path(hydrus_model$project_path, "SELECTOR.IN"))
  cat("Updated BLOCK B: WATER FLOW INFORMATION of SELECTOR.IN file.")

}
