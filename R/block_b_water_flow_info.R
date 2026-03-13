#' Set Block B of SELECTOR.IN: Water Flow Information
#'
#' @param hydrus_model a hydrus model created with \code{\link{create_hydrus_project}}
#'
#' @returns edits SELECTOR.IN file
#' @noRd
#'
#' @examples block_b_water_flow_info(hydrus_model)
#'
#' @importFrom stringr str_flatten str_split str_count
block_b_water_flow_info <- function(hydrus_model){

  selector_template <- readLines(file.path(hydrus_model$hydrus_project$project_path, "SELECTOR.IN"))

  ## Make parameter lookup tables:
  top_bc_opts <- data.frame(top_bc_name = c("constant_pressure_head",
                                            "constant_flux",
                                            "atm_bc_with_surface_layer",
                                            "atm_bc_with_surface_runoff",
                                            "variable_pressure_head",
                                            "variable_pressure_head/flux"),
                            TopInf = c("f","f","t","t","t","t"),
                            WLayer = c("f","f","t","f","f","f"),
                            KodTop = c("1","-1","-1","-1","1","0"),
                            InitCond = c("f","f","f","f","f","f"))

  bottom_bc_opts <- data.frame(bottom_bc_name = c("contant_pressure_head",
                                                  "contant_flux",
                                                  "variable_pressure_head",
                                                  "variable_flux",
                                                  "free_drainage",
                                                  "deep_drainage",
                                                  "seepage_face",
                                                  "horizontal_drains"),
                               BotInf = c("f", "f", "t", "t", "f", "f", "f", "f"),
                               qGWLF = c("f","f","f","f","f","t","f","f"),
                               FreeD = c("f","f","f","f","t","f","f","f"),
                               SeepF = c("f","f","f","f","f","f","t","f"),
                               KodBot = c(" ","-"," ","-","-","-","-","-"),
                               DrainF = c("f","f","f","f","f","f","f","t"),
                               hSeep = c("0","0","0","0","0","0","0","0"))

  #### *** BLOCK B: WATER FLOW INFORMATION *** ####
  # ----------------------------------------------------------------------------
  ### Iteration Criteria:
  selector_template[grep("MaxIt ",  selector_template) + 1] <- stringr::str_flatten(c(" ", " ",
                                                                             hydrus_model$iteration_criteria$maximum_iterations,
                                                                             rep(" ", times = 4),
                                                                             hydrus_model$iteration_criteria$water_content_tol,
                                                                             rep(" ", times = 6),
                                                                             hydrus_model$iteration_criteria$pressure_head_tol))
  # ----------------------------------------------------------------------------
  ### Assign top boundary conditions: TopInf WLayer KodTop InitCond ####
  top_boundary <- stringr::str_split(selector_template[grep("TopInf", selector_template) + 1], "", simplify = T)
  top_bc_slots <- c(2, 8, 16, 24)

  ## get bc options from user chosen top bc in options data frame
  specified_top_bc <- unlist(top_bc_opts[top_bc_opts$top_bc_name == hydrus_model$water_flow_bcs$upper_bc, 2:length(top_bc_opts)])

  if(hydrus_model$water_flow_bcs$initial_condition){
    specified_top_bc["InitCond"] <- "t"
  }

  ## assign options values in correct slots
  if(stringr::str_count(specified_top_bc["KodTop"]) == 1){
    top_boundary[,15] <- " "
    top_boundary[,top_bc_slots] <- specified_top_bc
  }else{
    top_bc_slots <- c(2, 8, 15, 16, 24)
    top_boundary[,top_bc_slots] <- c(specified_top_bc[1:2],
                                     stringr::str_split(specified_top_bc["KodTop"], "", simplify = T),
                                     specified_top_bc[4])
  }

  ## update selector template with new values
  selector_template[grep("TopInf", selector_template) + 1] <- stringr::str_flatten(top_boundary)

  # ----------------------------------------------------------------------------
  ### Assign bottom boundary conditions: BotInf qGWLF FreeD SeepF KodBot DrainF hSeep ####
  bottom_boundary <- stringr::str_split(selector_template[grep("BotInf", selector_template) + 1], "", simplify = T)
  bottom_bc_slots <- c(2, 8, 14, 20, 26, 34, 41)

  ## get bc options from user chosen bottom bc in options data frame
  specified_bottom_bc <- unlist(bottom_bc_opts[bottom_bc_opts$bottom_bc_name == hydrus_model$water_flow_bcs$lower_bc, 2:length(bottom_bc_opts)])

  ## assign options values in correct slots
  bottom_boundary[,bottom_bc_slots] <- specified_bottom_bc

  ## update selector template with new values
  selector_template[grep("BotInf", selector_template) + 1] <- stringr::str_flatten(bottom_boundary)

  # -------------------------------------
  ## Insert internal interpolation tables if the lines do not exist
  if(hydrus_model$iteration_criteria$internal_interpolation_tables){
    if(length(grep("hTab1", selector_template)) == 0){
    selector_template <- c(selector_template[1:(grep("BotInf", selector_template) + 1)],
                           stringr::str_flatten(c(rep(" ", times = 4),
                                         "hTab1",
                                         rep(" ", times = 3),
                                         "hTabN")),
                           stringr::str_flatten(c(rep(" ", times = 5),
                                         hydrus_model$iteration_criteria$lower_limit_tension_interval,
                                         rep(" ", times = 3),
                                         hydrus_model$iteration_criteria$upper_limit_tension_interval)),
                           selector_template[(grep("BotInf", selector_template) + 2):length(selector_template)])
    }
  }

  #-------------------------------------------------
  ## Insert triggered irrigation parameters if the lines do not exist
  if(hydrus_model$water_flow_bcs$triggered_irrigation){

    if(!colnames(hydrus_model$water_flow_bcs$irrigation_params) %in% c('trigger_node',
                                                                       'trigger_pressure_head',
                                                                       'irrigation_rate',
                                                                       'irrigation_duration',
                                                                       'lag_time')){
      stop("Error in hydrus_model$water_flow_bcs$irrigation_params. The supplied data frame must have columns named: 'trigger_node', 'trigger_pressure_head', 'irrigation_rate', 'irrigation_duration', 'lag_time'.")
    }

    if(length(grep("Irrig_rate", selector_template)) == 0){
      selector_template <- c(selector_template[1:(grep("BotInf", selector_template) + 1)],
                             "Node  Pressure  Irrig_rate  Duration  Lag_period",
                             stringr::str_flatten(c(rep(" ", times = 3),
                                           hydrus_model$water_flow_bcs$irrigation_params$trigger_node,
                                           rep(" ", times = 6),
                                           hydrus_model$water_flow_bcs$irrigation_params$trigger_pressure_head,
                                           rep(" ", times = 9),
                                           hydrus_model$water_flow_bcs$irrigation_params$irrigation_rate,
                                           rep(" ", times = 9),
                                           hydrus_model$water_flow_bcs$irrigation_params$irrigation_duration,
                                           rep(" ", times = 9),
                                           hydrus_model$water_flow_bcs$irrigation_params$lag_time)),
                             selector_template[(grep("BotInf", selector_template) + 2):length(selector_template)])
    }
  }

  # ------------------------------------
  ## Insert particle tracking parameters
  if(hydrus_model$main_processes$particle_tracking){
    if(length(grep("wInit", selector_template)) == 0){
      selector_template <- c(selector_template[1:(grep("BotInf", selector_template) + 1)],
                             stringr::str_flatten(c(rep(" ", times = 7), "wInit", rep(" ", times = 8), "wPrec")),
                             stringr::str_flatten(c(rep(" ", times = 9),
                                           hydrus_model$particle_tracking$init_water_storage,
                                           rep(" ", times = 10),
                                           hydrus_model$particle_tracking$cumulative_surface_flux)),
                             selector_template[(grep("BotInf", selector_template) + 2):length(selector_template)]
      )
    }
  }

  ## ---------------------------------------------------------------------------
  ## Insert constant flux boundary condition parameters
  ### IF water_flux_bc$upper_bc = 'constant flux' then add 'rTop rBot rRoot' lines if they do not exist:
  if(hydrus_model$water_flow_bcs$upper_bc == "constant_flux"){
    if(length(grep("rTop", selector_template)) == 0){
      selector_template <- c(selector_template[1:(grep("BotInf", selector_template) + 1)],
                             stringr::str_flatten(c(rep(" ", times = 9),
                                           "rTop         rBot        rRoot")),
                             stringr::str_flatten(c(rep(" ", times = 11),
                                           hydrus_model$constant_boundary_fluxes$upper_bc_flux,
                                           rep(" ", times = 11),
                                           hydrus_model$constant_boundary_fluxes$lower_bc_flux,
                                           rep(" ", times = 11),
                                           hydrus_model$constant_boundary_fluxes$root_water_uptake_flux)),
                             selector_template[(grep("BotInf", selector_template) + 2):length(selector_template)])
    }
  }

  # ----------------------------------------
  ## Assign soil hydraulic model and hysteresis
  soil_model <- stringr::str_split(selector_template[grep("Hysteresis", selector_template) + 1], "", simplify = T)
  soil_model[1,c(7,18)] <- as.character(c(hydrus_model$soil_hydraulics$soil_hydraulic_model, hydrus_model$soil_hydraulics$hysteresis))
  selector_template[grep("Hysteresis", selector_template) + 1] <- stringr::str_flatten(soil_model)

  ## Insert Kappa parameter when hysteresis selected
  if(hydrus_model$soil_hydraulics$hysteresis > 0){
    selector_template <- c(selector_template[1:(grep("Hysteresis", selector_template) + 1)],
                           stringr::str_flatten(c(rep(" ", times = 4), "Kappa")),
                           ifelse(hydrus_model$soil_hydraulics$initially_drying_curve,
                                  stringr::str_flatten(c(rep(" ", times = 5), "-1")),
                                  stringr::str_flatten(c(rep(" ", times = 6), "1"))),
                           selector_template[(grep("Hysteresis", selector_template) + 2):length(selector_template)]
    )
  }

  # ----------------------------------------------------------------
  ## Assign soil hydraulic property parameters ####
  ## NOTE: this creates three spaces between each number, where the original spacing is mode variable--I'm not sure if it will matter
  block_c_and_below <- selector_template[grep("BLOCK C", selector_template):length(selector_template)]

  for(i in 1:hydrus_model$geometry$number_materials){
    selector_template[grep("thr ", selector_template)+i] <- paste0("  ",
                                                                 hydrus_model$soil_hydraulics$soil_hydraulic_parameters[i,c("theta_r", "theta_s", "alpha", "n", "K_s", "l")],
                                                                 collapse = "    ")
  }
  selector_template <- c(selector_template[1:(grep("thr ", selector_template)+hydrus_model$geometry$number_materials)],
                         block_c_and_below)

  # ----------------------------------------------------------------------------
  ## if model == 8 (duel permeability), then need a line to set the "Fraction_of surface flow into fracture":
  if(hydrus_model$soil_hydraulics$soil_hydraulic_model == 8){

    # thrFr   thsFr  AlfaFr     nFr       KsFr     lFr       W    beta   gamma       a        Ka
    # 0     0.8    0.08       2       2496     0.5     0.1      15     0.4     0.1    0.02496
    selector_template <- c(selector_template[1:(grep("thr ", selector_template) + hydrus_model$geometry$number_materials)],
                           "  thrFr   thsFr  AlfaFr     nFr       KsFr     lFr       W    beta   gamma       a        Ka")
    for(i in 1:hydrus_model$geometry$number_materials){
      selector_template[grep("thrFr ", selector_template)+i] <- paste0("  ",
                                                                       hydrus_model$soil_hydraulics$soil_hydraulic_parameters[i, c("theta_r_fr", "theta_s_fr", "alpha_fr", "n_fr", "K_s_fr", "l_fr", "w", "beta", "gamma", "a", "K_a")],
                                                                     collapse = "    ")
    }
    selector_template <- c(selector_template[1:(grep("thrFr ", selector_template)+hydrus_model$geometry$number_materials)],
                           block_c_and_below)

    if(length(grep("qTop", selector_template)) == 0){
      selector_template <- c(selector_template[1:(grep("BLOCK C", selector_template) - 1)],
                                           "     qTop",
                             stringr::str_flatten(c(rep(" ", times = 6),
                                           hydrus_model$soil_hydraulics$surface_flow_into_fracture)),
                             block_c_and_below)
    }
  }



    #model = 0; VGM;


    # model = 1; modified VGM
    # thr     ths    Alfa      n         Ks       l      thm     tha        thk      Kk
    # 0.078    0.43   0.036    1.56      24.96     0.5    0.43   0.078       0.43   24.96

    # model = 2; Brooks-Corey
    # thr     ths    Alfa      n         Ks       l
    # 0.078    0.43     0.1     0.2      24.96     0.5

    # model = 4; Kosugi
    # thr     ths    Alfa      n         Ks       l
    # 0.078    0.43     100       1      24.96     0.5

    # model = -4; Kosugi with Brunswick; Hysteresis must be 0
    # thr     ths    Alfa      n         Ks       l    Ks_nCapt   aFilm     pf0      hr
    # 0.078    0.43     100       1      24.96     0.5     0.1     1.5        6.8       1

    # model = 5; duel porosity (durner, the duel VGM model)
    # thr     ths    Alfa      n         Ks       l        w2    Alfa2       n2
    # 0.078    0.43   0.036    1.56      24.96     0.5     0.5    0.03        1.5

    # model = 6; dual-porosity, mobile-immoble water, water content mass transfer
    # thr     ths    Alfa      n         Ks       l     thrIm   thsIm      Omega
    # 0.078    0.43   0.036    1.56      24.96     0.5       0     0.1          0

    # model = 7; dual-porosity, mobile-immobile water, presssure head mass transfer
    # thr     ths    Alfa      n         Ks       l     thrIm   thsIm     AlfaIm     nIm      Omega
    # 0.078    0.43   0.036    1.56      24.96     0.5       0     0.1      0.015     1.5          0

    # model = 8; duel permeability

    # model = 9; Look up table
    # thr     ths       Ks
    # 0.078    0.43   24.96
    #

  ## Update Selector.in
  writeLines(selector_template, file.path(hydrus_model$hydrus_project$project_path, "SELECTOR.IN"))
  cat("Updated BLOCK B: WATER FLOW INFORMATION of SELECTOR.IN file... \n")


}
