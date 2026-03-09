check_for_model_errors <- function(hydrus_model){
  if(hydrus_model$geometry$observation_nodes_n > 0){
    if(anyNA(hydrus_model$geometry$observation_nodes)){
      stop("Error in hydrus_model$geometry: Need to supply observation node z values (geometry$observation_nodes) when observation_nodes_n is greater than 0.")
    }
  }

  ## Errors in main_processes:
  if(hydrus_model$main_processes$snow_hydrology){
    if(!hydrus_model$main_processes$water_flow | !hydrus_model$main_processes$heat_transport){
      stop("Error in main_processes: Both water_flow and heat_transport must be turned on (TRUE) to model snow_hydrology.")
    }
  }
  if(hydrus_model$main_processes$vapor_flow & !hydrus_model$main_processes$water_flow){
    stop("Error in main_processes: Water flow must be turned on for vapor flow option.")
  }
  if(hydrus_model$main_processes$particle_tracking & !hydrus_model$main_processes$water_flow){
    stop("Error in main_processes: Water flow must be turned on for particle tracking option.")
  }
  if(hydrus_model$main_processes$HP1 & !hydrus_model$main_processes$solute_transport){
    stop("Error in main_processes: Solute transport must be turned on to use HP1 (Hydrus + PHREEQC).")
  }

  ## Errors in time_variable_bc:
  if(hydrus_model$time_variable_bc$meteorological_data & !hydrus_model$time_variable_bc$time_variable_bc){
    stop("Error in time_variable_bc: Time variable boundary condition must be turned on for meteorological data option.")
  }

  # if(root_water_uptake_opts$active_solute_uptake & !hydrus_model$processes$solute_transport & !hydrus_model$processes$root_water_uptake){
  #   stop("Error: Solute transport and root water uptake must be turned on for active solute uptake option.")
  # }

  ## Errors in soil_hydraulics:
  if(hydrus_model$soil_hydraulics$hysteresis > 0 & !hydrus_model$soil_hydraulics$soil_hydraulic_model %in% c(0,3)){
    stop("Error: Hysteresis can only be used with soil hydraulic models 0 and 3.")
  }

  if(hydrus_model$soil_hydraulics$soil_hydraulic_model %in% c(0, 3)){
    # VGM
    if(hydrus_model$soil_hydraulics$hysteresis == 0){
      if(!all(names(hydrus_model$soil_hydraulics$soil_hydraulic_parameters) %in% c("material", "theta_r", "theta_s", "alpha", "n", "K_s", "l"))){
        stop("Error in soil_hydraulics$soil_hydraulic_parameters: Soil hydraulic model is 0 or 3 with no hysteresis.
             Required parameters (data.frame columns) are: 'material', theta_r', 'theta_s', 'alpha', 'n', 'K_s', and 'l'.")
      }
    }else{
      if(!all(names(hydrus_model$soil_hydrualics$soil_hydraulic_parameters) %in% c("material", "theta_r", "theta_s", "alpha", "n", "K_s", "l", "theta_m", "theta_sW", "alphaW", "K_sW"))){
        stop("Error in soil_hydraulics$soil_hydraulic_parameters: Soil hydraulic model is 0 or 3 with hysteresis.
             Required parameters (data.frame columns)  are: 'material', 'theta_r', 'theta_s', 'alpha', 'n', 'K_s', 'l', 'theta_m', 'theta_sW', 'alphaW', 'K_sW.")
      }
    }
  }
  if(hydrus_model$soil_hydraulics$soil_hydraulic_model == 1){
    # modified VGM
    if(!all(names(hydrus_model$soil_hydraulics$soil_hydraulic_parameters) %in% c("material", "theta_r", "theta_s", "alpha", "n", "K_s", "l", "theta_m", "theta_a", "theta_k", "K_k"))){
      stop("Error in soil_hydraulics$soil_hydraulic_parameters: Soil hydraulic model is 1 (modified van Genuchten).
           Required parameters are: 'theta_r', 'theta_s', 'alpha', 'n', 'K_s', 'l', 'theta_m', 'theta_a', 'theta_k', 'K_k.")
    }
  }
  if(hydrus_model$soil_hydraulics$soil_hydraulic_model == 2){
    # Brooks-Corey
    if(!all(names(hydrus_model$soil_hydraulics$soil_hydraulic_parameters) %in% c("material", "theta_r", "theta_s", "alpha", "n", "K_s", "l"))){
      stop("Error in soil_hydraulics$soil_hydraulic_parameters: Soil hydraulic model is 2 (Brooks-Corey).
           Column names of soil_hydraulic_parameters must include: 'theta_r', 'theta_s', 'alpha', 'n', 'K_s', and 'l'.")
    }
  }
  if(hydrus_model$soil_hydraulics$soil_hydraulic_model == 4){
    # Kosugi
    if(!all(names(hydrus_model$soil_hydraulics$soil_hydraulic_parameters) %in%
            c("material", "theta_r", "theta_s", "alpha", "n", "K_s", "l"))){
      stop("Error in hydrus_model$soil_hydraulics$soil_hydraulic_parameters:
      Soil hydraulic model is set to the Kosugi model, soil_hydraulic_model = 4.
      Column names of soil_hydraulic_parameters must include: 'theta_r', 'theta_s', 'alpha', 'n', 'K_s', and 'l'.")
    }
  }
  if(hydrus_model$soil_hydraulics$soil_hydraulic_model == -4){
    # Kosugi + Brunswick modification
    if(isFALSE(all(names(hydrus_model$soil_hydraulics$soil_hydraulic_parameters) %in%
            c("material", "theta_r", "theta_s", "alpha", "n", "K_s", "l", "K_s_ncapt", "a_film", "pf0", "hr")))){
      stop("Error in hydrus_model$soil_hydraulics$soil_hydraulic_parameters:
      Soil hydraulic model is set to Kosugi with Brunswick modification, soil_hydraulic_model = -4.
      Column names of soil_hydraulic_parameters must include: 'theta_r', 'theta_s', 'alpha', 'n', 'K_s', 'l', 'K_s_ncapt', 'a_film', 'pf0', 'hr'."
           )
    }
  }

  if(hydrus_model$geometry$number_materials != nrow(hydrus_model$soil_hydraulics$soil_hydraulic_parameters)){
    stop("Error in soil_hydraulics$soil_hydraulic_parameters or geometry$number_materials:
         Number of materials does not match number of rows in soil hydraulic parameters table.")
  }

  ## Errors in water_flow_bcs:
  if(hydrus_model$water_flow_bcs$triggered_irrigation & !hydrus_model$water_flow_bcs$upper_bc %in% c("atm_bc_with_surface_layer", "atm_bc_with_surface_runoff")){
    stop("Error: Triggered irrigation can only be used when the upper water flow boundary condition is set to 'atm_bc_with_surface_layer' or 'atm_bc_with_surface_runoff'.")
  }

}
