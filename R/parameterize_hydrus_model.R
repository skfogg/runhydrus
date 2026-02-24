#' Title
#'
#' @param hydrus_model
#' @param main_processes
#' @param units
#' @param geometry
#' @param print_options
#' @param time_parameters
#' @param time_variable_bc
#' @param iteration_criteria
#' @param soil_hydraulics
#' @param water_flow_bcs
#'
#' @returns
#' @export
#'
#' @examples
parameterize_hydrus_model <- function(hydrus_project,
                                      main_processes = list(water_flow = TRUE,
                                                            vapor_flow = FALSE,
                                                            snow_hydrology = FALSE,
                                                            particle_tracking = FALSE,
                                                            solute_transport = FALSE,
                                                            unsatchem = FALSE,
                                                            HP1 = FALSE,
                                                            heat_transport = FALSE,
                                                            root_water_uptake = FALSE,
                                                            root_growth = FALSE,
                                                            inverse = FALSE),
                                      units = list(time_unit = "days",
                                                   space_unit = "cm",
                                                   mass_unit= "mmol"),
                                      geometry = list(number_materials = 1,
                                                      number_subregions = 0,
                                                      # number_solutes = 0,
                                                      number_nodes = 101,
                                                      profile_depth = 100,
                                                      observation_nodes = NULL),
                                      print_options = list(print_times = TRUE,
                                                           screen_output = TRUE,
                                                           print_fluxes_not_temp = TRUE,
                                                           times_to_print = data.frame(times = c(100)),
                                                           interval_output_option = FALSE,
                                                           interval_output = 0.001,
                                                           print_time_information = TRUE,
                                                           time_info_print_every_n_time_steps = 1),
                                      time_parameters = list(initial_time_step = 0.001,
                                                             minimum_time_step = 1e-05,
                                                             maximum_time_step = 5,
                                                             lower_time_step_mult = 1.3,
                                                             upper_time_step_mult = 0.7,
                                                             lower_optim_iter_range = 3,
                                                             upper_optim_iter_range = 7,
                                                             initial_model_time = 0,
                                                             final_model_time = 100),
                                      time_variable_bc = list(time_variable_bc = FALSE,
                                                              meteorological_data = FALSE),
                                      iteration_criteria = list(maximum_iterations = 10,
                                                                water_content_tol = 0.001,
                                                                pressure_head_tol = 1,
                                                                internal_interpolation_tables = TRUE,
                                                                lower_limit_tension_interval = 1e-06,
                                                                upper_limit_tension_interval = 10000),
                                      soil_hydraulics = c(soil_hydraulic_model = 0,
                                                          mobile_immobile = FALSE,
                                                          hysteresis = 0,
                                                          initially_drying_curve = TRUE,
                                                          soil_hydraulic_parameters = data.frame(material = 1,
                                                                                                 theta_r = 0.045,
                                                                                                 theta_s = 0.43,
                                                                                                 alpha = 0.145,
                                                                                                 n = 2.68,
                                                                                                 K_s = 712.8,
                                                                                                 l = 0.5)
                                                          ),
                                      water_flow_bcs = list(upper_bc = "constant_pressure_head",
                                                            initial_condition = FALSE, ## a top bc param
                                                            lower_bc = "free_drainage",
                                                            triggered_irrigation = FALSE),
                                      root_water_uptake = list(root_water_uptake_model = 0,
                                                               root_water_uptake_params = data.frame(critical_stress_index = 1,
                                                                                                     P0 = -10,
                                                                                                     POpt = -25,
                                                                                                     P2H = -200,
                                                                                                     P2L = -800,
                                                                                                     P3 = -8000,
                                                                                                     r2H = 0.5,
                                                                                                     r2L = 0.1)),
                                      root_growth = list(root_growth_depth = 0,
                                                         root_growth_params = NULL,
                                                         root_growth_factor = NULL),
                                      solute_options = list(number_solutes = 0,
                                                            equilibrium_adsorption = FALSE),
                                      particle_tracking = list(init_water_storage = 2,
                                                               cumulative_surface_flux = 2)

){

  ## Read in requirements
  require(stringr)

  source("R/edit_data_file.R")
  source("R/block_a_basic_info.R")
  source("R/block_b_water_flow_info.R")
  source("R/block_c_time_info.R")
  source("R/block_d_root_growth_info.R")
  source("R/block_g_root_water_uptake_info.R")


  ### ---------------
  ### Error checking:
  ### ---------------

  ## Errors in main_processes:
  if(main_processes$snow_hydrology){
    if(!main_processes$water_flow | !main_processes$heat_transport){
      stop("Error in main_processes: Both water_flow and heat_transport must be turned on (TRUE) to model snow_hydrology.")
    }
  }
  if(main_processes$vapor_flow & !main_processes$water_flow){
    stop("Error in main_processes: Water flow must be turned on for vapor flow option.")
  }
  if(main_processes$particle_tracking & !main_processes$water_flow){
    stop("Error in main_processes: Water flow must be turned on for particle tracking option.")
  }
  if(main_processes$HP1 & !main_processes$solute_transport){
    stop("Error in main_processes: Solute transport must be turned on to use HP1 (Hydrus + PHREEQC).")
  }

  ## Errors in time_variable_bc:
  if(time_variable_bc$meteorological_data & !time_variable_bc$time_variable_bc){
    stop("Error in time_variable_bc: Time variable boundary condition must be turned on for meteorological data option.")
  }

  # if(root_water_uptake_opts$active_solute_uptake & !hydrus_model$processes$solute_transport & !hydrus_model$processes$root_water_uptake){
  #   stop("Error: Solute transport and root water uptake must be turned on for active solute uptake option.")
  # }

  ## Errors in soil_hydraulics:
  if(soil_hydraulics$hysteresis > 0 & !soil_hydraulics$soil_hydraulic_model %in% c(0,3)){
    stop("Error: Hysteresis can only be used with soil hydraulic models 0 and 3.")
  }

  if(soil_hydraulics$soil_hydraulic_model %in% c(0, 3)){
    # VGM
    if(soil_hydraulics$hysteresis == 0){
      if(!all(names(soil_hydraulics$soil_hydraulic_parameters) %in% c("theta_r", "theta_s", "alpha", "n", "K_s", "l"))){
        stop("Error in soil_hydraulics$soil_hydraulic_parameters: Soil hydraulic model is 0 or 3 with no hysteresis. Required parameters (data.frame columns) are: 'theta_r', 'theta_s', 'alpha', 'n', 'K_s', and 'l'.")
      }
    }else{
      if(!all(names(soil_hydraulic_parameters) %in% c("theta_r", "theta_s", "alpha", "n", "K_s", "l", "theta_m", "theta_sW", "alphaW", "K_sW"))){
        stop("Error in soil_hydraulics$soil_hydraulic_parameters: Soil hydraulic model is 0 or 3 with hysteresis. Required parameters (data.frame columns)  are: 'theta_r', 'theta_s', 'alpha', 'n', 'K_s', 'l', 'theta_m', 'theta_sW', 'alphaW', 'K_sW.")
      }
    }
  }
  if(soil_hydraulics$soil_hydraulic_model == 1){
    # modified VGM
    if(!all(names(soil_hydraulics$soil_hydraulic_parameters) %in% c("theta_r", "theta_s", "alpha", "n", "K_s", "l", "theta_m", "theta_a", "theta_k", "K_k"))){
      stop("Error in soil_hydraulics$soil_hydraulic_parameters: Soil hydraulic model is 1 (modified van Genuchten). Required parameters are: 'theta_r', 'theta_s', 'alpha', 'n', 'K_s', 'l', 'theta_m', 'theta_a', 'theta_k', 'K_k.")
    }
  }
  if(soil_hydraulics$soil_hydraulic_model == 2){
    # Brooks-Corey
    if(!all(names(soil_hydraulics$soil_hydraulic_parameters) %in% c("theta_r", "theta_s", "alpha", "n", "K_s", "l"))){
      stop("Error in soil_hydraulics$soil_hydraulic_parameters: Soil hydraulic model is 2 (Brooks-Corey). Required parameters are: 'theta_r', 'theta_s', 'alpha', 'n', 'K_s', and 'l'.")
    }
  }
  if(soil_hydraulics$soil_hydraulic_model == 4){
    # Kosugi
    if(!all(names(soil_hydraulics$soil_hydraulic_parameters) %in% c("theta_r", "theta_s", "alpha", "n", "K_s", "l"))){
      stop("Error in soil_hydraulics$soil_hydraulic_parameters: Soil hydraulic model is 4 (Kosugi). Required parameters are: 'theta_r', 'theta_s', 'alpha', 'n', 'K_s', and 'l'.")
    }
  }
  if(soil_hydraulics$soil_hydraulic_model == -4){
    # Kosugi + Brunswick modification
    if(!all(names(soil_hydraulics$soil_hydraulic_parameters) %in% c("theta_r", "theta_s", "alpha", "n", "K_s", "l", "K_s_ncap", "aFilm", "f0", "hr"))){
      stop("Error in soil_hydraulics$soil_hydraulic_parameters: Soil hydraulic model is -4 (Kosugi with Brunswick modification). Required parameters are: 'theta_r', 'theta_s', 'alpha', 'n', 'K_s', 'l', 'K_s_ncap', 'aFilm', 'f0', and 'hr'.")
    }
  }

  if(geometry$number_materials != nrow(soil_hydraulics$soil_hydraulic_parameters)){
    stop("Error in soil_hydraulics$soil_hydraulic_parameters or geometry$number_materials: Number of materials does not match number of rows in soil hydraulic parameters table.")
  }

  ## Errors in water_flow_bcs:
  if(water_flow_bcs$triggered_irrigation & !water_flow_bcs$upper_bc %in% c("atm_bc_with_surface_layer", "atm_bc_with_surface_runoff")){
    stop("Error: Triggered irrigation can only be used when the upper water flow boundary condition is set to 'atm_bc_with_surface_layer' or 'atm_bc_with_surface_runoff'.")
  }


  ### --------------
  ### Edit DAT file:
  ### --------------
  edit_dat_file(hydrus_project,
                main_processes = main_processes,
                units = units,
                geometry = geometry,
                print_options = print_options,
                water_flow_bcs = water_flow_bcs)

  ### -------------------
  ### WRITE MODEL BLOCKS:
  ### -------------------

  #### *** BLOCK A: BASIC INFORMATION *** ####
  block_a_basic_info(hydrus_project,
                     main_processes = main_processes,
                     units = units,
                     print_options = print_options)

  #### *** BLOCK B: WATER FLOW INFORMATION *** ####
  block_b_water_flow_info(hydrus_project,
                          main_processes = main_processes,
                          iteration_criteria = iteration_criteria,
                          soil_hydraulics = soil_hydraulics)

  #### *** BLOCK C: TIME INFORMATION *** ####
  block_c_time_info(hydrus_project,
                    time_parameters = time_parameters,
                    print_options = print_options)

  #### *** BLOCK D: ROOT GROWTH INFORMATION *** ####
  if(main_processes$root_water_uptake & main_processes$root_growth){
    block_d_root_growth_info(hydrus_project,
                             root_growth = root_growth)
  }

  #### *** BLOCK E: HEAT TRANSPORT INFORMATION *** ####
  if(main_processes$heat_transport){
    stop("heat transport not available yet")
  }

  #### *** BLOCK F: SOLUTE TRANSPORT INFORMATION *** ####
  if(main_processes$solute_transport){
    stop("solute transport not available yet")
  }

  #### **** BLOCK G: ROOT WATER UPTAKE INFORMATION *** ####
  if(main_processes$root_water_uptake){
    block_g_root_water_uptake_info(hydrus_project,
                                   root_water_uptake = root_water_uptake)
  }




}
