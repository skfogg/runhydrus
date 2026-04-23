#' Set main processes in a HYDRUS model
#'
#' Function to set main_processes while maintaining unused default model parameters.
#'
#' @usage main_processes(
#'  water_flow = TRUE,
#'  vapor_flow = FALSE,
#'  snow_hydrology = FALSE,
#'  particle_tracking = FALSE,
#'  solute_transport = FALSE,
#'  unsatchem = FALSE,
#'  HP1 = FALSE,
#'  heat_transport = FALSE,
#'  root_water_uptake = FALSE,
#'  root_growth = FALSE,
#'  inverse = FALSE
#' )
#'
#' @param water_flow logical; if TRUE, model simulates water flow process. Default TRUE.
#' @param vapor_flow logical; if TRUE, model simulates vapor flow process. Default FALSE.
#' @param snow_hydrology logical; if TRUE, model uses snow hydrology process. Default FALSE. water_flow and heat_transport must both be TRUE.
#' @param particle_tracking logical; if TRUE, model simulates particle tracking. Default FALSE.
#' @param solute_transport logical; if TRUE, model simulates solute transport process. Default FALSE.
#' @param unsatchem logical; if TRUE, model uses UNSATCHEM add-on module. Default FALSE.
#' @param HP1 logical; if TRUE, model uses HP1 add-on module. Default FALSE.
#' @param heat_transport logical; if TRUE, model simulates heat transport processes. Default FALSE.
#' @param root_water_uptake logical; if TRUE, model simulates root water uptake process. Default FALSE.
#' @param root_growth logical; if TRUE, model simulates root growth process. Default FALSE.
#' @param inverse logical; if TRUE, model does an inverse simulation. Default FALSE.
#'
#' @returns list of parameters
#' @export
#'
#' @examples hydrus_model$main_processes <- main_processes(water_flow = TRUE,
#'                                                         heat_transport = TRUE,
#'                                                         snow_hydrology = TRUE)
#'
main_processes <- function(water_flow = TRUE,
                           vapor_flow = FALSE,
                           snow_hydrology = FALSE,
                           particle_tracking = FALSE,
                           solute_transport = FALSE,
                           unsatchem = FALSE,
                           HP1 = FALSE,
                           heat_transport = FALSE,
                           root_water_uptake = FALSE,
                           root_growth = FALSE,
                           inverse = FALSE){

  return(list(water_flow = water_flow,
              vapor_flow = vapor_flow,
              snow_hydrology = snow_hydrology,
              particle_tracking = particle_tracking,
              solute_transport = solute_transport,
              unsatchem = unsatchem,
              HP1 = HP1,
              heat_transport = heat_transport,
              root_water_uptake = root_water_uptake,
              root_growth = root_growth,
              inverse = inverse))
}

#' Set units in a HYDRUS model
#'
#' Function to set model_units while maintaining unused default model parameters.
#'
#' @usage model_units(
#'  time_unit = "days",
#'  space_unit = "cm",
#'  mass_unit = "mmol"
#' )
#'
#' @param time_unit character string indicating temporal unit. Must be "seconds", "minutes", "hours", "days" or "years"
#' @param space_unit character string indicating the spatial unit. Must be "mm", "cm", or "m".
#' @param mass_unit character string indicating the solute mass unit. Options include:
#'
#' @returns list of parameters
#' @export
#'
#' @examples hydrus_model$model_units <- model_units(time_unit = "seconds",
#'                                                   space_unit = "mm")
model_units <- function(time_unit = "days",
                        space_unit = "cm",
                        mass_unit= "mmol"){
  return(list(time_unit = time_unit,
              space_unit = space_unit,
              mass_unit = mass_unit))
}

#' Set units in a HYDRUS model
#'
#' Function to set model_units while maintaining unused default model parameters.
#'
#' @usage geometry(
#'  number_materials = 1,
#'  number_subregions = 0,
#'  number_nodes = 101,
#'  profile_depth = 100,
#'  observation_nodes_n = 0,
#'  observation_nodes = NA
#'  relative_gravity = 1.0
#' )
#'
#' @param number_materials integer. The number of materials in model.
#' @param number_subregions integer. The number of subregions in model.
#' @param number_nodes integer. The number of model nodes.
#' @param profile_depth integer. Depth of the model soil profile.
#' @param observation_nodes_n integer. The number of observation nodes
#' @param observation_nodes integer or vector of integers indicating the node number of observation nodes.
#' @param relative_gravity numeric. Gravity for Earth vertical = 1; Earth horizontal = 0; Moon = 0.166; Mars = 0.38.
#'
#' @returns list of parameters
#' @export
#'
#' @examples hydrus_model$geometry <- geometry(number_materials = 4,
#'                                             observation_nodes_n = 10,
#'                                             observation_nodes = c(2, 5, 9, 14, 18, 27, 41, 52, 64, 84))
geometry <- function(number_materials = 1,
                     number_subregions = 0,
                     number_nodes = 101,
                     profile_depth = 100,
                     observation_nodes_n = 0,
                     observation_nodes = NA,
                     relative_gravity = 1.0){
  return(list(number_materials = number_materials,
              number_subregions = number_subregions,
              number_nodes = number_nodes,
              profile_depth = profile_depth,
              observation_nodes_n = observation_nodes_n,
              observation_nodes = observation_nodes,
              relative_gravity = 1.0))

}

#' Set print options in a HYDRUS model
#'
#' Function to set print_options while maintaining unused default model parameters.
#'
#' @usage print_options(
#'  print_times = TRUE,
#'  screen_output = TRUE,
#'  print_fluxes_not_temp = TRUE,
#'  times_to_print = data.frame(times = c(100)),
#'  interval_output_option = FALSE,
#'  interval_output = 0.001,
#'  print_time_information = TRUE,
#'  time_info_print_every_n_time_steps = 1
#' )
#'
#' @param print_times logical. Print times (TRUE) on not (FALSE)
#' @param screen_output logical. Print output to screen (TRUE) or not (FALSE)
#' @param print_fluxes_not_temp logical. Print fluxes (TRUE) or temperature (FALSE)
#' @param times_to_print data.frame of times to print in time_units.
#' @param interval_output_option logical. Use interval output option (TRUE)
#' @param interval_output numeric. The interval to use when the interval output option is used.
#' @param print_time_information logical. Print time information (TRUE) or not (FALSE)
#' @param time_info_print_every_n_time_steps integer. T-level information every n time steps.
#'
#' @returns list of parameters
#' @export
#'
#' @examples hydrus_model$print_options <- print_options(interval_output_option = TRUE,
#'                                                       interval_output = 5)
print_options <- function(print_times = TRUE,
                          screen_output = TRUE,
                          print_fluxes_not_temp = TRUE,
                          times_to_print = data.frame(times = c(100)),
                          interval_output_option = FALSE,
                          interval_output = 0.001,
                          print_time_information = TRUE,
                          time_info_print_every_n_time_steps = 1){
  return(list(print_times = print_times,
              screen_output = screen_output,
              print_fluxes_not_temp = print_fluxes_not_temp,
              times_to_print = times_to_print,
              interval_output_option = interval_output_option,
              interval_output = interval_output,
              print_time_information = print_time_information,
              time_info_print_every_n_time_steps = time_info_print_every_n_time_steps))
}

#' Set time parameters in a HYDRUS model
#'
#' Function to set time_parameters while maintaining unused default model parameters.
#'
#' @usage time_parameters(
#'  initial_time_step = 0.001,
#'  minimum_time_step = 1e-05,
#'  maximum_time_step = 5,
#'  lower_time_step_mult = 1.3,
#'  upper_time_step_mult = 0.7,
#'  lower_optim_iter_range = 3,
#'  upper_optim_iter_range = 7,
#'  initial_model_time = 0,
#'  final_model_time = 100
#' )
#'
#' @param initial_time_step numeric. initial time step.
#' @param minimum_time_step numeric.
#' @param maximum_time_step numeric.
#' @param lower_time_step_mult numeric.
#' @param upper_time_step_mult numeric.
#' @param lower_optim_iter_range numeric.
#' @param upper_optim_iter_range numeric.
#' @param initial_model_time integer. Model time 1 in time_unit of model.
#' @param final_model_time integer. Last model time in time_unit of model.
#'
#' @returns list of parameters
#' @export
#'
#' @examples hydrus_model$time_parameters <- time_parameters(final_model_time = 365)
time_parameters <- function(initial_time_step = 0.001,
                            minimum_time_step = 1e-05,
                            maximum_time_step = 5,
                            lower_time_step_mult = 1.3,
                            upper_time_step_mult = 0.7,
                            lower_optim_iter_range = 3,
                            upper_optim_iter_range = 7,
                            initial_model_time = 0,
                            final_model_time = 100){
  return(list(initial_time_step = initial_time_step,
              minimum_time_step = minimum_time_step,
              maximum_time_step = maximum_time_step,
              lower_time_step_mult = lower_time_step_mult,
              upper_time_step_mult = upper_time_step_mult,
              lower_optim_iter_range = lower_optim_iter_range,
              upper_optim_iter_range = upper_optim_iter_range,
              initial_model_time = initial_model_time,
              final_model_time = final_model_time))
}


#' Set time variable boundary conditions parameters in a HYDRUS model
#'
#' Function to set time_variable_bc while maintaining unused default model parameters.
#'
#' @usage time_variable_bc(
#'  time_variable_bc = FALSE,
#'  meteorological_data = FALSE,
#'  repeat_bc_records = FALSE,
#'  repeat_bc_records_n = 1,
#'  daily_var_transpiration = FALSE,
#'  sinusoidal_var_precipitation = FALSE
#' )
#'
#' @param time_variable_bc logical. Use time variable boundary condition option (TRUE) or not (FALSE)
#' @param meteorological_data logical. Use meteorological data option (TRUE) or not (FALSE)
#' @param repeat_bc_records logical. Use repeat boundary conditions option (TRUE) or not (FALSE)
#' @param repeat_bc_records_n integer. Number of times to repeat boundary condition records
#' @param daily_var_transpiration logical. Option to use daily variations of transpiration during day generated by HYDRUS
#' @param sinusoidal_var_precipitation logical. Option to use sinusoidal variations of precipitation generated by HYDRUS
#'
#' @returns list pf parameters
#' @export
#'
#' @examples hydrus_model$time_variable_bc <- time_variable_bc(time_variable_bc = TRUE,
#'                                                             meteorological_data = TRUE)
time_variable_bc <- function(time_variable_bc = FALSE,
                             meteorological_data = FALSE,
                             repeat_bc_records = FALSE,
                             repeat_bc_records_n = 1,
                             daily_var_transpiration = FALSE,
                             sinusoidal_var_precipitation = FALSE){
  return(list(time_variable_bc = time_variable_bc,
              meteorological_data = meteorological_data,
              repeat_bc_records = repeat_bc_records,
              repeat_bc_records_n = repeat_bc_records_n,
              daily_var_transpiration = daily_var_transpiration,
              sinusoidal_var_precipitation = sinusoidal_var_precipitation))
}

#' Set iteration criteria parameters in a HYDRUS model
#'
#' Function to set iteration_criteria while maintaining unused default model parameters.
#'
#' @usage iteration_criteria(
#'  maximum_iterations = 10,
#'  water_content_tol = 0.001,
#'  pressure_head_tol = 1,
#'  internal_interpolation_tables = TRUE,
#'  lower_limit_tension_interval = 1e-06,
#'  upper_limit_tension_interval = 10000
#' )
#'
#' @param maximum_iterations integer.
#' @param water_content_tol numeric. water content tolerance.
#' @param pressure_head_tol numeric. pressure heat tolerance in model space_unit
#' @param internal_interpolation_tables logical. Enable internal interpolation tables
#' @param lower_limit_tension_interval numeric. Lower limit of the tension interval in space_unit
#' @param upper_limit_tension_interval numeric. Upper limit of the tension interval in space_unit
#'
#' @returns list of parameters
#' @export
#'
#' @examples hydrus_model$iteration_criteria <- iteration_criteria(maximum_iterations = 25)
iteration_criteria <- function(maximum_iterations = 10,
                               water_content_tol = 0.001,
                               pressure_head_tol = 1,
                               internal_interpolation_tables = TRUE,
                               lower_limit_tension_interval = 1e-06,
                               upper_limit_tension_interval = 10000){
  return(list(maximum_iterations = maximum_iterations,
              water_content_tol = water_content_tol,
              pressure_head_tol = pressure_head_tol,
              internal_interpolation_tables = internal_interpolation_tables,
              lower_limit_tension_interval = lower_limit_tension_interval,
              upper_limit_tension_interval = upper_limit_tension_interval))
}

#' Set soil hydraulic parameters in a HYDRUS model
#'
#' Function to set soil_hydraulics while maintaining unused default model parameters.
#'
#' @usage soil_hydraulics(
#'  soil_hydraulic_model = 0,
#'  mobile_immobile = FALSE,
#'  hysteresis = 0,
#'  initially_drying_curve = TRUE,
#'  soil_hydraulic_parameters = data.frame(material = 1,
#'                                         theta_r = 0.045,
#'                                         theta_s = 0.43,
#'                                         alpha = 0.145,
#'                                         n = 2.68,
#'                                         K_s = 712.8,
#'                                         l = 0.5)
#' )
#'
#' @param soil_hydraulic_model integer indicating which soil hydraulic model to use. See details.
#' @param mobile_immobile logical.
#' @param hysteresis integer indicating which hysteresis model to use. Default = 0; no hysteresis. See details.
#' @param initially_drying_curve logical. Only used when \code{hysteresis = TRUE}. Initially drying curve = TRUE; initially wetting curve = FALSE.
#' @param soil_hydraulic_parameters data.frame of soil hydraulics parameters. Column names must match the specified soil hydraulic model and number of rows must match the number of materials in the model. See details for parameters.
#' @param surface_flow_into_fracture numeric fraction in [0,1]. Used when \code{soil_hydraulic_model = 8}; the dual-permeability model.
#'
#' @returns list of parameters
#' @details \code{soil_hydraulic_model} options:
#' \describe{
#'   \item{\code{soil_hydraulic_model = 0}}{van Genuchten-Maulem single-porosity model (van Genuchten 1980). \code{soil_hydraulic_parameters}: \code{"material", "theta_r", "theta_s", "alpha", "n", "K_s", "l"}}
#'   \item{\code{soil_hydraulic_model = 1}}{Modified van Genuchten single-porosity model (Vogel and Cislerova, 1988). \code{soil_hydraulic_parameters}: \code{"material", "theta_r", "theta_s", "alpha", "n", "K_s", "l", "theta_m", "theta_a", "theta_k","K_k"}}
#'   \item{\code{soil_hydraulic_model = 2}}{Brooks-Corey (1964) single-porosity model. \code{soil_hydraulic_parameters}: \code{"material", "theta_r", "theta_s", "alpha", "n", "K_s", "l"}}
#'   \item{\code{soil_hydraulic_model = 3}}{van Genuchten-Maulem with air-entry value of -2cm. \code{soil_hydraulic_parameters}: \code{"material", "theta_r", "theta_s", "alpha", "n", "K_s", "l"}}
#'   \item{\code{soil_hydraulic_model = 4}}{Kosugi (1996) single-porosity (the log-normal model). \code{soil_hydraulic_parameters}: \code{"material", "theta_r", "theta_s", "alpha", "n", "K_s", "l"}}
#'   \item{\code{soil_hydraulic_model = -4}}{Kosugi single-porosity model with the Brunswick modification (Weber et al., 2019). \code{soil_hydraulic_parameters}: \code{"material", "theta_r", "theta_s", "alpha", "n", "K_s", "l", "K_s_ncapt", "a_film", "pf0", "hr"}}
#'   \item{\code{soil_hydraulic_model = 5}}{Dual-porosity (Durner, 1994; the dual van Genuchten-Maulem model). \code{soil_hydraulic_parameters}: \code{"material", "theta_r", "theta_s", "alpha", "n", "K_s", "l", "w_2", "alpha_2", "n_2".}}
#'   \item{\code{soil_hydraulic_model = 6}}{Dual-porosity nonequilibrium flow model (mobile-immobile water, water content mass transfer). \code{soil_hydraulic_parameters}: \code{"material", "theta_r", "theta_s", "alpha", "n", "K_s", "l", "theta_r_im", "theta_s_im", "omega".}}
#'   \item{\code{soil_hydraulic_model = 7}}{Dual-porosity nonequilibrium flow model (mobile-immobile water, pressure head mass transfer). \code{soil_hydraulic_parameters}: \code{"material", "theta_r", "theta_s", "alpha", "n", "K_s", "l", "theta_r_im", "theta_s_im", "alpha_im", "n_im", "omega".}}
#'   \item{\code{soil_hydraulic_model = 8}}{Dual-permeability nonequilibrium flow model. \code{soil_hydraulic_parameters}: \code{"material", "theta_r", "theta_s", "alpha", "n", "K_s", "l", "theta_r_fr", "theta_s_fr", "alpha_fr", "n_fr", "K_s_fr", "l_fr", "w", "beta", "gamma", "a", "K_a".}}
#'   \item{\code{soil_hydraulic_model = 9}}{Look-up Tables. \code{soil_hydraulic_parameters}: \code{"material", "theta_r", "theta_s", "K_s"}}
#' }
#' \code{hysteresis} options:
#' \describe{
#'  \item{\code{hysteresis = 0}}{No hysteresis}
#'  \item{\code{hysteresis = 1}}{Hysteresis in retention curve (Kool \& Parker, 1987)}
#'  \item{\code{hysteresis = 2}}{Hysteresis in retention curve and conductivity}
#'  \item{\code{hysteresis = 3}}{Hysteresis in retention curve (no pumping; Lenhard and Parker, 1992)}
#' }
#'
#'
#' @export
#'
#' @examples hydrus_model$soil_hydraulics <- soil_hydraulics(soil_hydraulic_model = 8,
#'                                                           soil_hydraulic_parameters =
#'                                                           data.frame(material = 1:3,
#'                                                                      theta_r = c(0.0792, 0.0813, 0.05),
#'                                                                      theta_s = c(0.47, 0.4231, 0.37),
#'                                                                      alpha = c(0.03, 0.019, 0.0353),
#'                                                                      n = c(1.35, 1.45, 3.18),
#'                                                                      K_s = c(6, 8, 642.98),
#'                                                                      l = 0.5,
#'                                                                      theta_r_fr = c(0.01298, 0, 0.01),
#'                                                                      theta_s_fr = c(0.43903, 0.3, 0.8),
#'                                                                      alpha_fr = c(0.017304, 0.079, 0.01),
#'                                                                      n_fr = c(1.3202, 1.6, 1.01),
#'                                                                      K_s_fr = c(289.89, 75.5, 386.52),
#'                                                                      l_fr = 0.5,
#'                                                                      w = 0.04,
#'                                                                      beta = c(4.74, 2, 1),
#'                                                                      gamma = c(0.01, 0.4, 0.4),
#'                                                                      a = c(2.197, 7.8, 10),
#'                                                                      K_a = c(1.4143, 0.01, 1.497))
soil_hydraulics <- function(soil_hydraulic_model = 0,
                            mobile_immobile = FALSE,
                            hysteresis = 0,
                            initially_drying_curve = TRUE,
                            soil_hydraulic_parameters = data.frame(material = 1,
                                                                   theta_r = 0.045,
                                                                   theta_s = 0.43,
                                                                   alpha = 0.145,
                                                                   n = 2.68,
                                                                   K_s = 712.8,
                                                                   l = 0.5),
                            surface_flow_into_fracture = 1){
  return(list(soil_hydraulic_model = soil_hydraulic_model,
              mobile_immobile = mobile_immobile,
              hysteresis = hysteresis,
              initially_drying_curve = initially_drying_curve,
              soil_hydraulic_parameters = soil_hydraulic_parameters,
              surface_flow_into_fracture = surface_flow_into_fracture))
}

#' Set water flow boundary conditions in a HYDRUS model
#'
#' Function to set water_flow_bcs while maintaining unused default model parameters.
#'
#' @usage water_flow_bcs(
#'  upper_bc = "constant_pressure_head",
#'  initial_condition = FALSE,
#'  lower_bc = "free_drainage",
#'  triggered_irrigation = FALSE,
#'  constant_boundary_fluxes = data.frame(upper_bc_flux = 0,
#'                                        lower_bc_flux = 0,
#'                                        root_water_uptake_flux = 0),
#'  irrigation_params = data.frame(trigger_node = 1,
#'                                 trigger_pressure_head = -100,
#'                                 irrigation_rate = 0,
#'                                 irrigation_duration = 0,
#'                                 lag_time = 0)
#' )
#'
#' @param upper_bc character string of the upper boundary condition. See details for options.
#' @param initial_condition logical.
#' @param lower_bc character string of the lower boundary condition. See details for options.
#' @param triggered_irrigation logical. Option to use triggered irrigation (TRUE), only available with \code{upper_bc = "atm_bc_with_surface_layer"} or \code{upper_bc = "atm_bc_with_surface_runoff"}.
#' @param constant_boundary_fluxes data.frame of fluxes when using the \code{upper_bc = "constant_boundary_flux"} option
#' @param irrigation_params data.frame of triggered irrigation parameters
#'
#' @returns list of parameters
#' @details \code{upper_bc} options:
#' \describe{
#'  \item{\code{upper_bc = "constant_pressure_head"}}{}
#'  \item{\code{upper_bc = "constant_flux"}}{Need \code{constant_boundary_fluxes} data.frame with column names \code{"upper_bc_flux", "lower_bc_flux", "root_water_uptake_flux"}.}
#'  \item{\code{upper_bc = "atm_bc_with_surface_layer"}}{When \code{triggered_irrigation = TRUE}, column names of \code{irrigation_params} must be: "trigger_node", "trigger_pressure_head", "irrigation_rate", "irrigation_duration", "lag_time".}
#'  \item{\code{upper_bc = "atm_bc_with_surface_runoff"}}{When \code{triggered_irrigation = TRUE}, column names of \code{irrigation_params} must be: "trigger_node", "trigger_pressure_head", "irrigation_rate", "irrigation_duration", "lag_time".}
#'  \item{\code{upper_bc = "variable_pressure_head"}}{}
#'  \item{\code{upper_bc = "variable_pressure_head/flux"}}{}
#' }
#' \code{lower_bc} options:
#' \itemize{
#'  \item{\code{lower_bc = "constant_pressure_head"}}
#'  \item{\code{lower_bc = "constant_flux"}}
#'  \item{\code{lower_bc = "variable_pressure_head"}}
#'  \item{\code{lower_bc = "variable_flux"}}
#'  \item{\code{lower_bc = "free_drainage"}}
#'  \item{\code{lower_bc = "deep_drainage"}}
#'  \item{\code{lower_bc = "seepage_face"}}
#'  \item{\code{lower_bc = "horizontal_drains"}}
#' }
#' @export
#'
#' @examples hydrus_model$water_flow_bcs <- water_flow_bcs(upper_bc = "constant_flux")
water_flow_bcs <- function(upper_bc = "constant_pressure_head",
                           initial_condition = FALSE, ## a top bc param
                           lower_bc = "free_drainage",
                           triggered_irrigation = FALSE,
                           constant_boundary_fluxes = data.frame(upper_bc_flux = 0,
                                                                 lower_bc_flux = 0,
                                                                 root_water_uptake_flux = 0),
                           irrigation_params = data.frame(trigger_node = 1,
                                                          trigger_pressure_head = -100,
                                                          irrigation_rate = 0,
                                                          irrigation_duration = 0,
                                                          lag_time = 0)){
  return(list(upper_bc = upper_bc,
              initial_condition = initial_condition,
              lower_bc = lower_bc,
              triggered_irrigation = triggered_irrigation,
              constant_boundary_fluxes = constant_boundary_fluxes,
              irrigation_params = irrigation_params))
}

#' Set root water uptake parameters in a HYDRUS model
#'
#' Function to set root_water_uptake while maintaining unused default model parameters.
#'
#' @usage root_water_uptake(
#'  root_water_uptake_model = 0,
#'  root_water_uptake_params = data.frame(critical_stress_index = 1,
#'                                        P0 = -10,
#'                                        POpt = -25,
#'                                        P2H = -200,
#'                                        P2L = -800,
#'                                        P3 = -8000,
#'                                        r2H = 0.5,
#'                                        r2L = 0.1),
#'  active_solute_uptake = FALSE
#' )
#'
#' @param root_water_uptake_model integer indicating which root water uptake model to use. See details for options.
#' @param root_water_uptake_params data frame of root water uptake parameters. See details for parameters for each model.
#' @param active_solute_uptake logical. Turn on active solute uptake (TRUE)
#'
#' @returns list of parameters
#' @details \code{root_water_uptake_model}  options:
#' \describe{
#' \item{\code{root_water_uptake_model = 0}}{Feddes model. Column names of \code{root_water_uptake_params}: \code{"critical_stress_index", "P0", "POpt", "P2H", "P2L", "P3", "r2H", "r2L"}}
#' \item{\code{root_water_uptake_model = 1}}{S-Shaped model. Column names of \code{root_water_uptake_params}: \code{"critical_stress_index", "h50", "P3"}}
#' \item{\code{root_water_uptake_model = 2}}{Nimah \& Hanks model. Column names of \code{root_water_uptake_params}: \code{"lHydRed", "P3"}}
#' \item{\code{root_water_uptake_model = 3}}{Couvreur model. Column names of \code{root_water_uptake_params}: \code{"hx_min", "rKrs", "rKComp"}}
#' \item{\code{root_water_uptake_model = 4}}{de Jong van Lier model. Column names of \code{root_water_uptake_params}: \code{"RootRad", "XylemRad", "Conductance", "RootCond", "a", "H-Wilt"}}
#' }
#'
#' @export
#'
#' @examples hydrus_model$root_water_uptake <- root_water_uptake(root_water_uptake_model = 1,
#'                                                               root_water_uptake_params = data.frame(
#'                                                               h50 = -800,
#'                                                               P3 = 3
#'                                                               ))
root_water_uptake <- function(root_water_uptake_model = 0,
                              root_water_uptake_params = data.frame(critical_stress_index = 1,
                                                                    P0 = -10,
                                                                    POpt = -25,
                                                                    P2H = -200,
                                                                    P2L = -800,
                                                                    P3 = -8000,
                                                                    r2H = 0.5,
                                                                    r2L = 0.1),
                              active_solute_uptake = FALSE){
  return(list(root_water_uptake_model = root_water_uptake_model,
         root_water_uptake_params = root_water_uptake_params,
         active_solute_uptake = active_solute_uptake))
}

#' Set root growth parameters in a HYDRUS model
#'
#' Function to set root_growth while maintaining unused default model parameters.
#'
#' @usage root_growth(
#'  root_growth_depth = 0,
#'  root_growth_params = NA,
#'  root_growth_factor = NA
#' )
#'
#' @param root_growth_depth integer indicating how root depth will be specified. See details for options.
#' @param root_growth_params a data.frame of root growth parameters for the given root depth specification and root growth factor option
#' @param root_growth_factor integer indicating how to specify the root growth factor when using a logisitc growth function to specufy root growth depth (\code{root_growth_depth = 2}). See details for options.
#'
#' @returns a list of parameters
#' @details \code{root_growth_depth} options:
#' \describe{
#' \item{\code{root_growth_depth = 0}}{Root depth specified with time-variable boundary conditions.}
#' \item{\code{root_growth_depth = 1}}{Root depth specified with a table. \code{root_growth_params}: "Time", "RootDepth"}
#' \item{\code{root_growth_depth = 2}}{Root depth specified using a logistic growth function.
#'    \code{root_growth_factor} options:
#'    \describe{
#'    \item{\code{root_growth_factor = 0}}{Root growth factor from given data. \code{root_growth_params}: "initial_root_growth_time", "time_root_data", "harvest_time", "initial_rooting_depth",
#'    "depth_root_data", "maximum_rooting_depth", "time_period".}
#'    \item{\code{root_growth_factor = 1}}{Root growth factor 50% after 50% growing season. \code{root_growth_params}: "initial_root_growth_time",
#'    "harvest_time", "initial_rooting_depth",
#'    "maximum_rooting_depth", "time_period".}
#'    }
#'  }
#' }
#'
#' @export
#'
#' @examples hydrus_model$root_growth <- root_growth(root_growth_depth = 2,
#'                                                   root_growth_params = data.frame(initial_root_growth = 115,
#'                                                   harvest_time = 245,
#'                                                   initial_rooting_depth = 0,
#'                                                   maximum_rooting_depth = 60,
#'                                                   time_period = 365
#'                                                   ))
root_growth <- function(root_growth_depth = 0,
                        root_growth_params = NA,
                        root_growth_factor = NA){
  return(list(root_growth_depth = root_growth_depth,
              root_growth_params = root_growth_params,
              root_growth_factor = root_growth_factor))
}

#' Set solute transport parameters in a HYDRUS model
#'
#' Function to set solute_transport while maintaining unused default model parameters.
#'
#' @usage solute_transport(solute_transport_model = "equilibrium_model",
#'  standard_solute_transport = TRUE,
#'  number_solutes = 0,
#'  equilibrium_adsorption = FALSE,
#'  time_weighting_scheme = "crank_nickolson",
#'  space_weighting_scheme = "galerkin_fe",
#'  pulse_duration = 0,
#'  stability_criterion = 1,
#'  use_tortuosity = TRUE,
#'  temperature_dependence = FALSE,
#'  temperature_dependence_params = data.frame(NULL),
#'  nonlinear_adsorption_iteration_criteria = data.frame(absolute_conc_tol = 1,
#'                                          relative_conc_tol = 0.001,
#'                                          maximum_n_iteration = 10),
#'  material_params = data.frame(bulk_density = 1,
#'                               longitudinal_dispersivity = 1.5,
#'                               fraction_adsorption_sites = 17.5,
#'                               immobile_water_content = 0),
#'  solute_params = data.frame(molecular_diffusion_free_water = 0,
#'                             molecular_diffusion_soil_air = 0),
#'  solute_reaction_params = data.frame(Kd = 0,
#'                                      Nu = 0,
#'                                      Beta = 1,
#'                                      Henry = 0,
#'                                      SnkL1 = 0,
#'                                      SnkS1 = 0,
#'                                      SnkG1 = 0,
#'                                      SnkL1_prime = 0,
#'                                      SnkS1_prime = 0,
#'                                      SnkG1_prime = 0,
#'                                      SnkL0 = 0,
#'                                      SnkS0 = 0,
#'                                      SnkG0 = 0,
#'                                      Alpha = 0),
#'  solute_transport_bcs = data.frame(upper_bc = "solute_flux_bc",
#'                                    lower_bc = "concentration_bc",
#'                                    in_total_concentrations = FALSE,
#'                                    stagnant_boundary_layer = NA,
#'                                    concentration_in_atmosphere = NA,
#'                                    fractionation_ratio = NA)
#'  )
#'
#' @param solute_transport_model character string indicating which solute transport model to use. See details for options.
#' @param number_solutes integer, number of solutes.
#' @param equilibrium_adsorption logical.
#' @param time_weighting_scheme character string indicating which temporal weighting coefficient to use. See details for options.
#' @param space_weighting_scheme character string indicating the spatial weighting scheme formulation. See details for options.
#' @param pulse_duration integer indicating the duration of pulse
#' @param stability_criterion numeric. Product of the dimensionless Peclet and Curant numbers. Used to add artificial dispersion when using the \code{"gfe_with_artificial_disp"} space weighting scheme. Used to limit the time step (causing lower Courant numbers for a given Peclet number) for the \code{"galerkin_fe"} space weighting scheme.
#' @param use_tortuosity logical. TRUE when molecular diffusion coefficients in the water and gas phases are to be multiplied by a tortuosity factor according to the formulation of either Millington and Quirk (1961) or Moldrup et al. (1997, 2000).
#' @param temperature_dependence logical. TRUE when solute transport and reation parameters are to be temperature dependent.
#' @param temperature_dependence_params data.frame of temperature dependence parameters. See details.
#' @param nonlinear_adsorption_iteration_criteria data.frame of non-linear adsorption iteration criteria. See detail.
#' @param material_params data.frame of solute transport parameters of each soil material. See details for required columns.
#' @param solute_params data.frame of solute specific parameters. Columns must be \code{"molecular_diffusion_free_water"} and \code{"molecular_diffusion_soil_air"}.
#' @param solute_reaction_params data.frame of solute reaction parameters. See details.
#' @param solute_transport_bcs data.frame of solute transport boundary conditions. See details.
#'
#' @returns list of parameters
#' @export
#'
#' @details
#' \code{solute_transport_model} options:
#' \describe{
#'  \item{\code{"equilibrium_model"}}{Standard solute transport model}
#'  \item{\code{"dual_perm_phys_non_equilibrium"}}{Dual permeability physical non-equilibrium model}
#'  \item{\code{"dual_permeability"}}{Dual-Permeability model with either immobile water in the matrix or kinetic soption (chemical and physical non-equilibrium)}
#' }
#' \code{time_weighting_scheme} options:
#' \describe{
#'  \item{\code{"crank-nicolson"}}{temporal weighting coefficient = 0.5. Recommended for solution precision.}
#'  \item{\code{"implicit_scheme"}}{temporal weighting coefficient = 1.0. May result in increased numerical dispersion, but reduce numerical instabilities.}
#' }
#' \code{space_weighting_scheme} options:
#' \describe{
#'  \item{\code{"galerkin_fe"}}{}
#'  \item{\code{"upstream_weighting_fe"}}{}
#'  \item{\code{"gfe_with_artificial_disp"}}{}
#' }
#'
#' \code{temperature_dependence_params} data.frame must have columns:
#'
#' \code{nonlinear_adsorption_iteration_criteria} data.frame must have columns: \code{"absolute_conc_tol"}, \code{"relative_conc_tol"}, and \code{"maximum_n_iteration"}. Recommended values are 0.001 for \code{"relative_conc_tol"} and 10 for \code{"maximum_n_iteration"}.
#'
#' \code{material_parameters} data.frame must have columns: \code{"bulk_density"}, \code{"longitudinal_dispersivity"}, \code{"fraction_adsorption_sites"}, and \code{"immobile_water_content"}.
#'
#' \code{solute_reaction_params} data.frame must have columns:
#' \itemize{
#'  \item{\code{"Kd"}}
#'  \item{\code{"Nu"}}
#'  \item{\code{"Beta"}}
#'  \item{\code{"Henry"}}
#'  \item{\code{"SnkL1"}}
#'  \item{\code{"SnkS1"}}
#'  \item{\code{"SnkG1"}}
#'  \item{\code{"SnkL1_prime"}}
#'  \item{\code{"SnkS1_prime"}}
#'  \item{\code{"SnkG1_prime"}}
#'  \item{\code{"SnkL0"}}
#'  \item{\code{"SnkS0"}}
#'  \item{\code{"SnkG0"}}
#'  \item{\code{"Alpha"}}
#'  }
#'
#' \code{solute_transport_bcs} data.frame must have columns:
#' \describe{
#' \item{upper_bc}{options include: \code{"concentration_bc"}, \code{"solute_flux_bc"}, \code{"stagnant_bc_volotile_solute"}, \code{"isotope_bc"}}
#' \item{lower_bc}{options include: \code{"concentration_bc"}, \code{"solute_flux_bc"}, \code{"zero_conc_gradient"}}
#' \item{in_total_concentrations}{logical indicating if initial conditions should be in total concentrations, mass of solute/mass of soil (TRUE), instead of liquid phase concnetrations, mass of solute/volume water (default; FALSE)}
#' \item{stagnant_boundary_layer}{only used when \code{upper_bc = "stagnant_bc_volotile_solute"}}
#' \item{concentration_in_atmosphere}{only used when \code{upper_bc = "stagnant_bc_volotile_solute"}}
#' \item{fractionation_ratio}{only used when \code{upper_bc = "isotope_bc"}}
#' }
#'
#' @examples hydrus_model$solute_transport <- solute_transport(number_solutes = 1)
solute_transport <- function(solute_transport_model = "equilibrium_model",
                             #standard_solute_transport = TRUE,
                             number_solutes = 0,
                             equilibrium_adsorption = FALSE,
                             time_weighting_scheme = "crank_nickolson",
                             space_weighting_scheme = "galerkin_fe",
                             pulse_duration = 0,
                             stability_criterion = 1,
                             use_tortuosity = TRUE,
                             temperature_dependence = FALSE,
                             temperature_dependence_params = data.frame(NULL),
                             nonlinear_adsorption_iteration_criteria = data.frame(absolute_conc_tol = 1,
                                                                                  relative_conc_tol = 0.001,
                                                                                  maximum_n_iteration = 10),
                             material_params = data.frame(material = 1,
                                                          bulk_density = 1.5,
                                                          longitudinal_dispersivity = 17.5,
                                                          fraction_adsorption_sites = 1,
                                                          immobile_water_content = 0),
                             solute_params = data.frame(molecular_diffusion_free_water = 0,
                                                        molecular_diffusion_soil_air = 0),
                             solute_reaction_params = data.frame(Kd = 0,
                                                                 Nu = 0,
                                                                 Beta = 1,
                                                                 Henry = 0,
                                                                 SnkL1 = 0,
                                                                 SnkS1 = 0,
                                                                 SnkG1 = 0,
                                                                 SnkL1_prime = 0,
                                                                 SnkS1_prime = 0,
                                                                 SnkG1_prime = 0,
                                                                 SnkL0 = 0,
                                                                 SnkS0 = 0,
                                                                 SnkG0 = 0,
                                                                 Alpha = 0),
                             solute_transport_bcs = data.frame(upper_bc = "solute_flux_bc",
                                                               lower_bc = "concentration_bc",
                                                               in_total_concentrations = FALSE,
                                                               stagnant_boundary_layer = NA,
                                                               concentration_in_atmosphere = NA,
                                                               fractionation_ratio = NA)
                             ){
  return(list(solute_transport_model = solute_transport_model,
              number_solutes = number_solutes,
              equilibrium_adsorption = equilibrium_adsorption,
              time_weighting_scheme = time_weighting_scheme,
              space_weighting_scheme = space_weighting_scheme,
              pulse_duration = pulse_duration,
              stability_criterion  = stability_criterion,
              use_tortuosity = use_tortuosity,
              temperature_dependence = temperature_dependence,
              temperature_dependence_params = temperature_dependence_params,
              nonlinear_adsorption_iteration_criteria = nonlinear_adsorption_iteration_criteria,
              material_params = material_params,
              solute_params = solute_params,
              solute_reaction_params = solute_reaction_params,
              solute_transport_bcs = solute_transport_bcs))
}

#' Set particle tracking parameters in a HYDRUS model
#'
#' Function to set particle_tracking while maintaining unused default model parameters.
#'
#' @param init_water_storage numeric. Initial water storage.
#' @param cumulative_surface_flux numeric. Cumulative surface flux
#'
#' @returns list a parameters
#' @export
#'
#' @examples hydrus_model$particle_tracking <- particle_tracking(init_water_storage = 1)
particle_tracking <- function(init_water_storage = 2,
                              cumulative_surface_flux = 2){
  return(list(init_water_storage = init_water_storage,
              cumulative_surface_flux = cumulative_surface_flux))
}

