#' Title
#'
#' @param hydrus_project a hydrus project object created with
#'   'create_hydrus_project'
#'
#' @returns Edits the .DAT file of the HYRDUS project
#' @export
#'
#' @examples edit_dat_file(hydrus_project, main_processes = main_processes, model_units = model_units, geometry = geometry, print_options = print_options, water_flow_bcs = water_flow_bcs)
edit_dat_file <- function(hydrus_model){

   main_args <- c("HYDRUS_Version",
                 "WaterFlow",
                 "SoluteTransport",
                 "Unsatchem",
                 "HP1",
                 "HeatTransport",
                 "EquilibriumAdsorption",
                 "MobileImmobile",
                 "RootWaterUptake",
                 "RootGrowth",
                 "MaterialNumbers",
                 "SubregionNumbers",
                 "SpaceUnit",
                 "TimeUnit",
                 "PrintTimes",
                 "NumberOfSolutes",
                 "InitialCondition"
                 # "CO2Transport",
                 # "SolutionConc",
                 # "AdsorbedConc",
                 # "PrecipConc"
                 )
  profile_args <- c("NumberOfNodes",
                    "ProfileDepth",
                    "ObservationNodes")

  dat_file_main_args <- paste0(main_args, "=", c(hydrus_model$hydrus_project$hydrus_version,
                                                 as.numeric(hydrus_model$main_processes$water_flow),
                                                 as.numeric(hydrus_model$main_processes$solute_transport),
                                                 as.numeric(hydrus_model$main_processes$unsatchem),
                                                 as.numeric(hydrus_model$main_processes$HP1),
                                                 as.numeric(hydrus_model$main_processes$heat_transport),
                                                 as.numeric(hydrus_model$solute_options$equilibrium_adsorption),
                                                 as.numeric(hydrus_model$soil_hydraulics$mobile_immobile),
                                                 as.numeric(hydrus_model$main_processes$root_water_uptake),
                                                 as.numeric(hydrus_model$main_processes$root_growth),
                                                 hydrus_model$geometry$number_materials,
                                                 hydrus_model$geometry$number_subregions,
                                                 hydrus_model$model_units$space_unit,
                                                 hydrus_model$model_units$time_unit,
                                                 as.numeric(nrow(hydrus_model$print_options$times_to_print)),
                                                 hydrus_model$solute_options$number_solutes,
                                                 as.numeric(hydrus_model$water_flow_bcs$initial_condition)
                                                 # "0",
                                                 # "0",
                                                 # "0",
                                                 # "0"
                                                 )
                               )

  dat_file_profile_args <- paste0(profile_args, "=", c(hydrus_model$geometry$number_nodes,
                                                       formatC(hydrus_model$geometry$profile_depth, format = "E", digits = 2),
                                                       ifelse(anyNA(hydrus_model$geometry$observation_nodes),
                                                              hydrus_model$geometry$number_nodes,
                                                              hydrus_model$geometry$observation_nodes_n))
                                  )


  # if description is NULL, then use project name as description
  description <- ifelse(is.null(hydrus_model$hydrus_project$description), paste("project name", hydrus_model$hydrus_project$project_name), hydrus_model$hydrus_project$description)

  dat_file_contents <- c(";", "[Main]", dat_file_main_args,
                         ";", "[Profile]", dat_file_profile_args)
  write(dat_file_contents, file = file.path(hydrus_model$hydrus_project$project_path, "HYDRUS1D.DAT"), append = FALSE)

  cat("DAT file updated with parameterization... \n")


}
