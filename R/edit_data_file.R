#' Title
#'
#' @param hydrus_project
#' @param main_processes
#' @param units
#' @param geometry
#' @param print_options
#' @param water_flow_bcs
#'
#' @returns
#' @export
#'
#' @examples
edit_dat_file <- function(hydrus_project,
                          main_processes = main_processes,
                          units = units,
                          geometry = geometry,
                          print_options = print_options,
                          water_flow_bcs = water_flow_bcs){

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
                 "InitialCondition",
                 "CO2Transport",
                 "SolutionConc",
                 "AdsorbedConc",
                 "PrecipConc")
  profile_args <- c("NumberOfNodes",
                    "ProfileDepth",
                    "ObservationNodes")

  dat_file_main_args <- paste0(main_args, "=", c(hydrus_model$hydrus_version,
                                                 as.numeric(main_processes$water_flow),
                                                 as.numeric(main_processes$solute_transport),
                                                 as.numeric(main_processes$unsatchem),
                                                 as.numeric(main_processes$HP1),
                                                 as.numeric(main_processes$heat_transport),
                                                 as.numeric(main_processes$equilibrium_adsorption),
                                                 as.numeric(mobile_immobile),
                                                 as.numeric(main_processes$root_water_uptake),
                                                 as.numeric(main_processes$root_growth),
                                                 geometry$number_materials,
                                                 geometry$number_subregions,
                                                 units$space_unit,
                                                 units$time_unit,
                                                 as.numeric(nrow(soil_hydraulics$soil_hydraulic_parameters)),
                                                 solute_options$number_solutes,
                                                 as.numeric(water_flow_bcs$initial_condition),
                                                 "1",
                                                 "1",
                                                 "1",
                                                 "1")
                               )

  dat_file_profile_args <- paste0(profile_args, "=", c(geometry$node_numbers,
                                                       formatC(geometry$profile_depth, format = "E", digits = 2),
                                                       ifelse(is.null(geometry$observation_nodes), geometry$node_numbers, geometry$observation_nodes))
                                  )


  # if description is NULL, then use project name as description
  description <- ifelse(is.null(hydrus_model$description), paste("project name", hydrus_model$project_name), hydrus_model$description)

  dat_file_contents <- c(";", "[Main]", dat_file_main_args,
                         ";", "[Profile]", dat_file_profile_args)
  write(dat_file_contents, file = project_dat_file, append = FALSE)

  cat("DAT file updated with parameterization.")

}
