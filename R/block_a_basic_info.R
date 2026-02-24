block_a_basic_info <- function(hydrus_model,
                               main_processes = main_processes,
                               units = units,
                               print_options = print_options

){

  ## Get SELECTOR.IN file of project
  selector_template <- readLines(file.path(hydrus_model$project_path, "SELECTOR.IN"))

  #### *** BLOCK A: BASIC INFORMATION *** ####
  ## insert project description
  selector_template[4] <- hydrus_model$description

  ## length unit, time unit, mass unit
  selector_template[grep("LUnit", selector_template) + 1] <- units$space_unit
  selector_template[grep("LUnit", selector_template) + 2] <- units$time_unit
  selector_template[grep("LUnit", selector_template) + 3] <- ifelse(is.null(units$mass_unit), "-", units$mass_unit)

  ## Start with all model options turned off:
  basic_opt_1 <- data.frame(keyword = str_split(selector_template[9], " {1,}", simplify = T)[1,],
                            idx_loc = sort(unique(unlist(str_locate_all(selector_template[10], c("t", "f"))))),
                            on = "f")
  basic_opt_2 <- data.frame(keyword = str_split(selector_template[11], " {1,}", simplify = T)[1,],
                            idx_loc = sort(unique(unlist(str_locate_all(selector_template[12], c("t", "f"))))),
                            on = "f")

  # Turn on options based on processes set in create_hydrus_project()
  ## First line:

  # Print option: Screen output is on by default:
  if(print_options$screen_output){
    basic_opt_1[basic_opt_1$keyword == "lScreen","on"] <- "t"
  }

  if(hydrus_model$processes$water_flow){
    basic_opt_1[basic_opt_1$keyword == "lWat","on"] <- "t"
  }
  if(hydrus_model$processes$solute_transport){
    basic_opt_1[basic_opt_1$keyword == "lChem","on"] <- "t"
  }
  if(hydrus_model$processes$heat_transport){
    basic_opt_1[basic_opt_1$keyword == "lTemp","on"] <- "t"
    basic_opt_1[basic_opt_1$keyword == "lScreen","on"] <- "f"
  }
  if(hydrus_model$processes$root_water_uptake){
    basic_opt_1[basic_opt_1$keyword == "lSink","on"] <- "t"
  }
  if(hydrus_model$processes$root_growth){
    basic_opt_1[basic_opt_1$keyword == "lRoot","on"] <- "t"
  }
  if(time_var_bc){
    basic_opt_1[basic_opt_1$keyword == "lVariabBC","on"] <- "t"
  }
  if(solute_transport_model$equilibrium){
    basic_opt_1[basic_opt_1$keyword == "lEquilib","on"] <- "t"
  }
  if(inverse){
    basic_opt_1[basic_opt_1$keyword == "lInverse", "on"] <- "t"
  }

  ## Second line:
  ## Print option: print fluxes is on by default unless heat transport is specified:
  if(print_options$print_fluxes_not_temp){
    basic_opt_2[basic_opt_2$keyword == "lFluxes","on"] <- "t"
  }
  if(hydrus_model$processes$heat_transport){
    basic_opt_2[basic_opt_2$keyword == "lFluxes","on"] <- "f"
  }

  if(snow_hydrology){
    basic_opt_2[basic_opt_2$keyword == "lSnow"] <- "t"
  }
  if(hydrus_model$processes$HP1){
    basic_opt_1[basic_opt_1$keyword == "lChem","on"] <- "t"
    basic_opt_2[basic_opt_2$keyword == "lHP1","on"] <- "t"
  }
  if(meteorological_data){
    basic_opt_2[basic_opt_2$keyword == "lMeteo"] <- "t"
  }
  if(vapor_flow){
    basic_opt_2[basic_opt_2$keyword == "lVapor"] <- "t"
  }
  if(root_water_uptake_opts$active_solute_uptake){
    basic_opt_2[basic_opt_2$keyword == "lActiveU"] <- "t"
  }

  if(water_flow_bcs$triggered_irrigation){
    basic_opt_2[basic_opt_2$keyword == "lIrrig"] <- "t"
  }
  if(particle_tracking){
    basic_opt_2[basic_opt_2$keyword == "lPart"] <- "t"
  }

  ## Properly space the SELECTOR.IN lines
  opt_1 <- str_split(selector_template[10], "", simplify = TRUE)
  for(i in 1:nrow(basic_opt_1)){
    opt_1[1, basic_opt_1$idx_loc[i]] <- basic_opt_1$on[i]
  }

  opt_2 <- str_split(selector_template[12], "", simplify = TRUE)
  for(i in 1:nrow(basic_opt_2)){
    opt_2[1, basic_opt_2$idx_loc[i]] <- basic_opt_2$on[i]
  }

  ## Assign to selector_template on the correct lines
  selector_template[grep("lWat", selector_template) + 1] <- str_flatten(opt_1[1,])
  selector_template[grep("lSnow", selector_template) + 1] <- str_flatten(opt_2[1,])

  ## set the number of materials
  geometry <- str_split(selector_template[grep("NMat", selector_template) + 1], "", simplify = T)
  geometry[1,3] <- as.character(geometry$number_materials)
  geometry[1,11] <- as.character(geometry$number_subregions)
  # CosAlpha is gravity
  selector_template[grep("NMat", selector_template) + 1] <- str_flatten(geometry)


  ## Update SELECTOR.IN
  writeLines(selector_template, file.path(hydrus_model$project_path, "SELECTOR.IN"))
  cat("Updated BLOCK A: BASIC INFORMATION of SELECTOR.IN file.")

}
