#' Update Block A of SELECTOR.IN: Basic Information
#'
#' @param hydrus_model a hydrus model created with \code{\link{create_hydrus_project}}
#'
#' @returns Edits BLOCK A of SELECTOR.IN file associated with the input hydrus_model
#' @noRd
#'
#' @examples block_a_basic_info(hydrus_model)
#'
#' @importFrom stringr str_split str_locate_all str_flatten
block_a_basic_info <- function(hydrus_model){

  # Uses main_processes, model_units, print_options, time_variable_bc, solute_transport, root_water_uptake, water_flow_bcs, geometry

  ## Get SELECTOR.IN file of project
  selector_template <- readLines(file.path(hydrus_model$hydrus_project$project_path, "SELECTOR.IN"))

  #### *** BLOCK A: BASIC INFORMATION *** ####
  ## insert project description
  selector_template[grep("Heading", selector_template) + 1] <- ifelse(is.null(hydrus_model$hydrus_project$description),
                                                                      paste("project name",
                                                                            hydrus_model$hydrus_project$project_name),
                                                                      hydrus_model$hydrus_project$description)

  ## length unit, time unit, mass unit
  selector_template[grep("LUnit", selector_template) + 1] <- hydrus_model$model_units$space_unit
  selector_template[grep("LUnit", selector_template) + 2] <- hydrus_model$model_units$time_unit
  selector_template[grep("LUnit", selector_template) + 3] <- ifelse(is.null(hydrus_model$model_units$mass_unit),
                                                                    "-",
                                                                    hydrus_model$model_units$mass_unit)

  ## Start with all model options turned off:
  basic_opt_1 <- data.frame(keyword = stringr::str_split(selector_template[grep("lWat", selector_template)], " {1,}", simplify = T)[1,],
                            idx_loc = sort(unique(unlist(stringr::str_locate_all(selector_template[grep("lWat", selector_template)+1], "t|f")))),
                            on = "f")
  basic_opt_2 <- data.frame(keyword = stringr::str_split(selector_template[grep("lSnow", selector_template)], " {1,}", simplify = T)[1,],
                            idx_loc = sort(unique(unlist(stringr::str_locate_all(selector_template[grep("lSnow", selector_template)+1], "t|f")))),
                            on = "f")

  # Turn on options based on processes set in create_hydrus_project()
  ## First line:

  # Print option: Screen output is on by default:
  if(hydrus_model$print_options$screen_output){
    basic_opt_1[basic_opt_1$keyword == "lScreen","on"] <- "t"
  }

  if(hydrus_model$main_processes$water_flow){
    basic_opt_1[basic_opt_1$keyword == "lWat","on"] <- "t"
  }
  if(hydrus_model$main_processes$solute_transport){
    basic_opt_1[basic_opt_1$keyword == "lChem","on"] <- "t"
  }
  if(hydrus_model$main_processes$heat_transport){
    basic_opt_1[basic_opt_1$keyword == "lTemp","on"] <- "t"
    basic_opt_1[basic_opt_1$keyword == "lScreen","on"] <- "f"
  }
  if(hydrus_model$main_processes$root_water_uptake){
    basic_opt_1[basic_opt_1$keyword == "lSink","on"] <- "t"
  }
  if(hydrus_model$main_processes$root_growth){
    basic_opt_1[basic_opt_1$keyword == "lRoot","on"] <- "t"
  }
  if(hydrus_model$time_variable_bc$time_variable_bc){
    basic_opt_1[basic_opt_1$keyword == "lVariabBC","on"] <- "t"
  }
  if(hydrus_model$solute_transport$solute_transport_model == "equilibrium_model"){
    basic_opt_1[basic_opt_1$keyword == "lEquil","on"] <- "t"
  }
  if(hydrus_model$main_processes$inverse){
    basic_opt_1[basic_opt_1$keyword == "lInverse", "on"] <- "t"
  }

  ## Second line:
  ## Print option: print fluxes is on by default unless heat transport is specified:
  if(hydrus_model$print_options$print_fluxes_not_temp){
    basic_opt_2[basic_opt_2$keyword == "lFluxes","on"] <- "t"
  }
  if(hydrus_model$main_processes$heat_transport){
    basic_opt_2[basic_opt_2$keyword == "lFluxes","on"] <- "f"
  }

  if(hydrus_model$main_processes$snow_hydrology){
    basic_opt_2[basic_opt_2$keyword == "lSnow", "on"] <- "t"
  }
  if(hydrus_model$main_processes$HP1){
    basic_opt_1[basic_opt_1$keyword == "lChem","on"] <- "t"
    basic_opt_2[basic_opt_2$keyword == "lHP1","on"] <- "t"
  }
  if(hydrus_model$time_variable_bc$meteorological_data){
    basic_opt_2[basic_opt_2$keyword == "lMeteo", "on"] <- "t"
  }
  if(hydrus_model$main_processes$vapor_flow){
    basic_opt_2[basic_opt_2$keyword == "lVapor", "on"] <- "t"
  }
  if(hydrus_model$root_water_uptake$active_solute_uptake){
    basic_opt_2[basic_opt_2$keyword == "lActiveU", "on"] <- "t"
  }

  if(hydrus_model$water_flow_bcs$triggered_irrigation){
    basic_opt_2[basic_opt_2$keyword == "lIrrig", "on"] <- "t"
  }
  if(hydrus_model$main_processes$particle_tracking){
    basic_opt_2[basic_opt_2$keyword == "lPart", "on"] <- "t"
  }

  ## Properly space the SELECTOR.IN lines
  opt_1 <- stringr::str_split(selector_template[grep("lWat", selector_template) + 1], "", simplify = TRUE)
  for(i in 1:nrow(basic_opt_1)){
    opt_1[1, basic_opt_1$idx_loc[i]] <- basic_opt_1$on[i]
  }

  opt_2 <- stringr::str_split(selector_template[grep("lSnow", selector_template) + 1], "", simplify = TRUE)
  for(i in 1:nrow(basic_opt_2)){
    opt_2[1, basic_opt_2$idx_loc[i]] <- basic_opt_2$on[i]
  }

  ## Assign to selector_template on the correct lines
  selector_template[grep("lWat", selector_template) + 1] <- stringr::str_flatten(opt_1[1,])
  selector_template[grep("lSnow", selector_template) + 1] <- stringr::str_flatten(opt_2[1,])

  ## set the number of materials
  geometry_line <- stringr::str_split(selector_template[grep("NMat", selector_template) + 1], "", simplify = T)
  geometry_line[1,3] <- as.character(hydrus_model$geometry$number_materials)
  geometry_line[1,11] <- as.character(hydrus_model$geometry$number_subregions)
  # CosAlpha is gravity
  selector_template[grep("NMat", selector_template) + 1] <- stringr::str_flatten(geometry_line)


  ## Update SELECTOR.IN
  writeLines(selector_template, file.path(hydrus_model$hydrus_project$project_path, "SELECTOR.IN"))
  cat("Updated BLOCK A: BASIC INFORMATION of SELECTOR.IN file... \n")

}
