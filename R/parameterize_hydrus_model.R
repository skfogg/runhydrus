#' Parameterize a HYDRUS model
#'
#' @param hydrus_model a hydrus model created with \code{\link{create_hydrus_project}}
#'
#' @returns Function return a list of a model parameters. Edit parameters with \code{\link{main_processes}}, \code{\link{model_units}}, \code{\link{geometry}}, \code{\link{print_options}},
#' \code{\link{time_parameters}}, \code{\link{time_variable_bc}}, \code{\link{iteration_criteria}}, \code{\link{soil_hydraulics}},
#' \code{\link{water_flow_bcs}}, \code{\link{root_water_uptake}}, \code{\link{root_growth}}, \code{\link{solute_transport}}, and \code{\link{particle_tracking}}.
#' @export
#'
#' @examples parameterize_hydrus_model(hydrus_model)
#'
parameterize_hydrus_model <- function(hydrus_model){

  selector_template <- readLines(file.path(hydrus_model$hydrus_project$project_path, "SELECTOR.IN"))
  profile_template <- readLines(file.path(hydrus_model$hydrus_project$project_path, "PROFILE.DAT"))

  # ## update file version to match HYDRUS version
  # selector_template[1] <- paste0("Pcp_File_Version=", hydrus_model$hydrus_project$hydrus_version)
  # profile_template[1] <- paste0("Pcp_File_Version=", hydrus_model$hydrus_project$hydrus_version)
  #
  # ## Write out updated templates to correct files:
  # write(selector_template, file = file.path(hydrus_model$hydrus_project$project_path, "SELECTOR.IN"))
  # write(profile_template, file = file.path(hydrus_model$hydrus_project$project_path, "PROFILE.DAT"))

  ### ---------------
  ### Error checking:
  ### ---------------

  check_for_model_errors(hydrus_model)

  ### --------------
  ### Edit DAT file:
  ### --------------
  edit_dat_file(hydrus_model)

  ### ------------------------------------------------------
  ### CREATE ADDITIONAL INPUT FILES BASED ON MODEL SELECTION
  ### ------------------------------------------------------

  if(hydrus_model$time_variable_bc$time_variable_bc){

    ## Get a basic ATMOSPH.IN template
    atmosph_template <- readLines(file("./templates/ATMOSPH.IN"))
    ## write to project_path
    write(atmosph_template, file = file.path(hydrus_model$hydrus_project$project_path, "ATMOSPH.IN"))

    if(hydrus_model$time_variable_bc$meteorological_data){
      ## Get a basic METEO.IN template
      meteo_template <- readLines(file("./templates/METEO.IN"))
      ## write to project_path
      write(meteo_template, file = file.path(hydrus_model$hydrus_project$project_path, "METEO.IN"))
    }
  }

  if(hydrus_model$main_processes$inverse){
    stop("hydrus_model$main_processes$inverse not available yet")
  }

  ### -------------------
  ### WRITE MODEL BLOCKS:
  ### -------------------

  #### *** BLOCK A: BASIC INFORMATION *** ####
  block_a_basic_info(hydrus_model)

  #### *** BLOCK B: WATER FLOW INFORMATION *** ####
  block_b_water_flow_info(hydrus_model)

  #### *** BLOCK C: TIME INFORMATION *** ####
  block_c_time_info(hydrus_model)

  #### *** BLOCK D: ROOT GROWTH INFORMATION *** ####
  if(hydrus_model$main_processes$root_water_uptake & hydrus_model$main_processes$root_growth){
    block_d_root_growth_info(hydrus_model)
  }

  #### *** BLOCK E: HEAT TRANSPORT INFORMATION *** ####
  if(hydrus_model$main_processes$heat_transport){
    stop("hydrus_model$main_processes$heat_transport not available yet")
  }

  if(hydrus_model$main_processes$solute_transport){
    block_f_solute_transport_info(hydrus_model)
  }

  #### *** BLOCK F: SOLUTE TRANSPORT INFORMATION *** ####
  if(hydrus_model$main_processes$solute_transport){
    stop("hydrus_model$main_processes$solute_transport not available yet")
  }

  #### **** BLOCK G: ROOT WATER UPTAKE INFORMATION *** ####
  if(hydrus_model$main_processes$root_water_uptake){
    block_g_root_water_uptake_info(hydrus_model)
  }
}
