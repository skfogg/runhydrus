#' Create and edit SELECTOR.IN file
#'
#' Further set model parameters that are edited and set in the SELECTOR.IN file
#' of the HYDRUS project. Some model parameters are taken from the hydrus model
#' object creates using 'create_hydrus_project' and many others are set within
#' this function. the hydrus model object is edited to reflect the newly set
#' parameters.
#'
#' The 'soil_hydraulic_model' options are: Single-porosity van Genuchten-Maulem
#' = 0 Single-porosity van Genuchten-Maulem with air-entry value of -2 cm = 3
#' Single-porosity modified van Genuchten (Vogel and Cislerova 1988) = 1
#' Single-porosity Brooks-Corey (1964) = 2 Kosugi (1996) (the log normal model)
#' = 4 Kosugi (1996) (the log normal model) with Brunswick model modification
#' (Weber et al 2019) = -4 Dual-porosity (Durner, 1994; the dual van
#' Genuchten-Maulem model) = 5 Dual-porosity (mobile-immobile water, water
#' content mass transfer) = 6 Dual-porosity (mobile-immobile water, pressure
#' head mass transfer) = 7 Dual-permeability (add-on Module) = 8 Look-up Table
#' with Hydraulic capacity calculated or provided = 9
#'
#' Hysteresis options (only available for the single-porosity van
#' Genchten-Maulem model with or without air entry value of -2 cm): No
#' hysteresis = 0 (default) Hysteresis in retention curve (Kool & Parker, 1987)
#' = 1 Hysteresis in retention curve and conductivity = 2 Hysteresis in
#' retention curve (no pumping; Lenhard and Parker, 1992) = 3
#'
#' When hysteresis is turned on, user must specify if the curve is initially
#' drying ('initially_drying_curve = TRUE') or initially wetting
#' ('initially_drying_curve = FALSE')
#'
#' 'water_flow_boundary_condition' options for 'upper_bc' include:
#' "constant_pressure_head" (default), "constant_flux",
#' "atm_bc_with_surface_layer", "atm_bc_with_surface_runoff",
#' "variable_pressure_head", "variable_pressure_head/flux".
#' 'water_flow_boundary_condition' options for 'lower_bc' include:
#' "constant_pressure_head", "constant_flux", "variable_pressure_head",
#' "variable_flux", "free_drainage" (default), "deep_drainage", "seepage_face",
#' "horizontal_drains".
#'
#' @param hydrus_model a hydrus model object created with
#'   create_hydrus_project()
#' @param inverse logical. Regular, forward model is FALSE, inverse model is
#'   TRUE
#' @param time_var_bc logical. Use time variable boundary conditions (TRUE) or
#'   not (FALSE).
#' @param meteorological_data logical. Option to use meteorological data in time
#'   variable boundary condition?
#' @param snow_hydrology logical. Snow hydrology option of water flow model.
#'   Heat transport must also be turned on.
#' @param vapor_flow logical. Vapor flow option of water flow model.
#' @param particle_tracking logical. Particle tracking option of water flow
#'   model.
#' @param solute_transport_model list of solute transport model options
#' @param root_water_uptake_opts list of root water uptake options
#' @param soil_hydraulic_model integer indicating which soil hydraulic model to
#'   use. See details.
#' @param hysteresis integer indicating the type of hysteresis in a water flow
#'   model using van Genchten-Maulem hydraulics.
#' @param initially_drying_curve logical. Is the hysteresis initially drying
#'   (TRUE) or initially wetting curve (FALSE)?
#' @param soil_hydraulic_parameters data.frame indicating the proper parameters
#'   needed for the soil hydraulic model. Parameter values for each material
#'   separated by row.
#' @param print_options list of print options.
#'
#' @returns
#' @export
#'
#' @examples
create_selector_in <- function(hydrus_model,

                               time_var_bc = FALSE,
                               meteorological_data = FALSE,



                               # solute_transport_model = list(equilibrium = TRUE),
                               # root_water_uptake_opts = list(active_solute_uptake = FALSE),

                               internal_interpolation_tables = TRUE,
                               internal_interp_params = list(lower_limit_tension_interval = 1e-06,
                                                              upper_limit_tension_interval = 10000),

                               soil_hydraulic_model = 0,
                               hysteresis = 0,
                               initially_drying_curve = TRUE,

                               soil_hydraulic_parameters = data.frame(material = 1,
                                                                      theta_r = 0.045,
                                                                      theta_s = 0.43,
                                                                      alpha = 0.145,
                                                                      n = 2.68,
                                                                      K_s = 712.8,
                                                                      l = 0.5),
                               # print_options = list(screen_output = TRUE,
                               #                      print_fluxes_not_temp = TRUE),

                               iteration_criteria = list(maximum_iterations = 10,
                                                         water_content_tol = 0.001,
                                                         pressure_head_tol = 1),
                               water_flow_bcs = list(upper_bc = "constant_pressure_head",
                                                     lower_bc = "free_drainage",
                                                     triggered_irrigation = FALSE),
                               particle_tracking_params = list(init_water_storage = 2,
                                                            cumulative_surface_flux = 2),
                               time_parameters = list(initial_time_step = 0.001,
                                                      minimum_time_step = 1e-05,
                                                      maximum_time_step = 5,
                                                      lower_time_step_mult = 1.3,
                                                      upper_time_step_mult = 0.7,
                                                      lower_optim_iter_range = 3,
                                                      upper_optim_iter_range = 7,
                                                      initial_model_time = 0,
                                                      final_model_time = 100)
                               # print_options = list(print_times = data.frame(times = c(100)),
                               #                      interval_output_option = FALSE,
                               #                      interval_output = 0.001,
                               #                      print_time_information = TRUE,
                               #                      time_info_print_every_n_time_steps = 1)


                               ){
  require(stringr)
  # bottom_bc_opts <- readRDS("data/bottom_bc_opts.rds")
  # top_bc_opts <- readRDS("data/top_bc_opts.rds")

  ## Model set-up error checking:
  if(snow_hydrology){
    if(!hydrus_model$processes$water_flow | !hydrus_model$processes$heat_transport){
      stop("Error: Both water flow and heat transport must be turned on to model snow hydrology.")
    }
  }
  if(vapor_flow & !hydrus_model$processes$water_flow){
    stop("Error: Water flow must be turned on for vapor flow option.")
  }
  if(particle_tracking & !hydrus_model$processes$water_flow){
    stop("Error: Water flow must be turned on for particle tracking option.")
  }
  if(hydrus_model$processes$HP1 & !hydrus_model$processes$solute_transport){
    stop("Error: Solute transport must be turned on to run HP1.")
  }
  if(meteorological_data & !time_var_bc){
    stop("Error: Time variable boundary condition must be turned on for meteorological data option.")
  }
  if(root_water_uptake_opts$active_solute_uptake & !hydrus_model$processes$solute_transport & !hydrus_model$processes$root_water_uptake){
    stop("Error: Solute transport and root water uptake must be turned on for active solute uptake option.")
  }
  if(hysteresis > 0 & !soil_hydraulic_model %in% c(0,3)){
    stop("Error: Hysteresis can only be used with soil hydraulic models 0 and 3.")
  }

  if(soil_hydraulic_model %in% c(0, 3)){
    # VGM
    if(hysteresis == 0){
      if(!all(names(soil_hydraulic_parameters) %in% c("theta_r", "theta_s", "alpha", "n", "K_s", "l"))){
        stop("Error in soil_hydraulic_parameters: Soil hydraulic model is 0 or 3 with no hysteresis. Required parameters are: 'theta_r', 'theta_s', 'alpha', 'n', 'K_s', and 'l'.")
      }
    }else{
      if(!all(names(soil_hydraulic_parameters) %in% c("theta_r", "theta_s", "alpha", "n", "K_s", "l", "theta_m", "theta_sW", "alphaW", "K_sW"))){
        stop("Error in soil_hydraulic_parameters: Soil hydraulic model is 0 or 3 with hysteresis. Required parameters are: 'theta_r', 'theta_s', 'alpha', 'n', 'K_s', 'l', 'theta_m', 'theta_sW', 'alphaW', 'K_sW.")
      }
    }
  }
  if(soil_hydraulic_model == 1){
    # modified VGM
    if(!all(names(soil_hydraulic_parameters) %in% c("theta_r", "theta_s", "alpha", "n", "K_s", "l", "theta_m", "theta_a", "theta_k", "K_k"))){
      stop("Error in soil_hydraulic_parameters: Soil hydraulic model is 1 (modified van Genuchten). Required parameters are: 'theta_r', 'theta_s', 'alpha', 'n', 'K_s', 'l', 'theta_m', 'theta_a', 'theta_k', 'K_k.")
    }
  }
  if(soil_hydraulic_model == 2){
    # Brooks-Corey
    if(!all(names(soil_hydraulic_parameters) %in% c("theta_r", "theta_s", "alpha", "n", "K_s", "l"))){
      stop("Error in soil_hydraulic_parameters: Soil hydraulic model is 2 (Brooks-Corey). Required parameters are: 'theta_r', 'theta_s', 'alpha', 'n', 'K_s', and 'l'.")
    }
  }
  if(model == 4){
    # Kosugi
    if(!all(names(soil_hydraulic_parameters) %in% c("theta_r", "theta_s", "alpha", "n", "K_s", "l"))){
      stop("Error in soil_hydraulic_parameters: Soil hydraulic model is 4 (Kosugi). Required parameters are: 'theta_r', 'theta_s', 'alpha', 'n', 'K_s', and 'l'.")
    }
  }
  if(model == -4){
    # Kosugi + Brunswick modification
    if(!all(names(soil_hydraulic_parameters) %in% c("theta_r", "theta_s", "alpha", "n", "K_s", "l", "K_s_ncap", "aFilm", "f0", "hr"))){
      stop("Error in soil_hydraulic_parameters: Soil hydraulic model is -4 (Kosugi with Brunswick modification). Required parameters are: 'theta_r', 'theta_s', 'alpha', 'n', 'K_s', 'l', 'K_s_ncap', 'aFilm', 'f0', and 'hr'.")
    }
  }

  if(hydrus_model$geometry$material_numbers != nrow(soil_hydraulic_parameters)){
    stop("Error: Material number does not match number of rows in soil hydraulic parameter table.")
  }

  if(water_flow_bcs$triggered_irrigation & !water_flow_bcs$upper_bc %in% c("atm_bc_with_surface_layer", "atm_bc_with_surface_runoff")){
    stop("Error: Triggered irrigation can only be used when the upper water flow boundary condition is set to 'atm_bc_with_surface_layer' or 'atm_bc_with_surface_runoff'.")
  }



  ##### SELECTOR.IN EDITING #####

  ## Create empty SELECTOR.IN file in the project path
  selector_in_file <- file.path(hydrus_model$project_path, "SELECTOR.IN")
  file.create(selector_in_file)

  ## Get a basic SELECTOR.IN template
  selector_template <- readLines(file("./templates/SELECTOR.IN"))

  ## update file version to match hydrus version
  selector_template[1] <- paste0("Pcp_File_Version=", hydrus_model$hydrus_version)

  #### *** BLOCK A: BASIC INFORMATION *** ####

  #### *** BLOCK B: WATER FLOW INFORMATION *** ####






#
#   #### *** BLOCK C: TIME INFORMATION *** ####
#   block_c_time_info(hydrus_model,
#                     time_parameters = time_parameters,
#                     print_options = print_options)
#
#   #### *** BLOCK D: ROOT GROWTH INFORMATION *** ####
#   block_d_root_growth_info(hydrus_model,
#                            root_growth_depth = 0,
#                            root_growth_params = NULL,
#                            root_growth_factor = NULL)
#
#   #### *** BLOCK E: HEAT TRANSPORT INFORMATION *** ####
#
#   #### *** BLOCK F: SOLUTE TRANSPORT INFORMATION *** ####
#
#   #### **** BLOCK G: ROOT WATER UPTAKE INFORMATION *** ####
#   block_g_root_water_uptake_info(hydrus_model,
#                                  root_water_uptake_model = 0,
#                                  root_water_uptake_params = data.frame(critical_stress_index = 1,
#                                                                        P0 = -10,
#                                                                        POpt = -25,
#                                                                        P2H = -200,
#                                                                        P2L = -800,
#                                                                        P3 = -8000,
#                                                                        r2H = 0.5,
#                                                                        r2L = 0.1))
#


}


# BLOCK A
# lWat = water flow
# lChem = standard solute transport
# lTemp = heat transport
# lSink = Root water uptake option
# lRoot = Root growth option under root water uptake (only available when root water uptake on)
# lShort =
# lWDep =
# lScreen = screen output option of print options (default on)
# lVariabBC = Time-variable boundary conditions
# lEquil = Equilibrium model (standard solute transport model), solute must be on
# lInverse = Inverse solution

# (lEquil slot) lCO2 = t when CO2 transport (UnsatChem, Add-on module)
# (lInverse slot) lKRed =


# lSnow = Snow hydrology option under water flow option, only available when water flow and heat transport are both on
# lHP1 = Hydrus + PHREEQC add-on module
# lMeteo = Meteorological Data option under time-variable boundary conditions
# lVapor = vapor flow option under water flow
# lActiveU = Active solute uptake model on (part of root water and solute uptake model)
# lFluxes = print fluxes (instead of Temp) for Observations Nodes, part of print options (default on); default "f" when heat transport is turned on
# lIrrig =
# lPart = particle tracking option with water flow
# lCosmic = Cosmic-Ray Neutrons (Cosmic, Add-On module)
# lDummy =

## BLOCK B

# "constant_pressure_head" = c(f,f,1,f),
# "constant_flux" = c(f,f,-1,f); added parameters: rTop rBot rRoot
# "atm_bc_with_surface_layer" = c(t,t,-1,f); "triggered_irrigation" option (lIrrig = t), adds parameters: Node Pressure Irrig_rate Duration Lag_period
# "atm_bc_with_surface_runoff" = c(t,f,-1,f); "triggered_irrigation" option (lIrrig = t), adds parameters: Node Pressure Irrig_rate Duration Lag_period
# "variable_pressure_head" = c(t,f,1,f)
# "variable_pressure_head/flux" = c(t,f,0,f)

## BotInf qGWLF FreeD SeepF KodBot DrainF hSeep

# "constant_pressure_head" = c(f,f,f,f,1,f,0) "constant_flux" =
# c(f,f,f,f,-1,f,0); added parameters: rTop rBot rRoot
# "variable_pressure_head" = c(t,f,f,f,1,f,0) "variable_flux"  =
# c(t,f,f,f,-1,f,0) "free_drainage" (default) = c(f,f,t,f,-1,f,0)
# "deep_drainage" = c(f,t,f,f,-1,f,0); added parameters: GWL0L Aqh Bqh
# "seepage_face" = c(f,f,f,t,-1,f,0); last param (hSeep) is the user specified
# seepage face height in lunit [cm] "horizontal_drains" = c(f,f,f,f,-1,t,0); 3
# added parameter lines: Drain System; Drain Depth, Drain Spacing, Entrance
# Resistance; Drain Parameters (HAVE THIS BE NOT AVIALABLE RN BECAUSE TOO MANY
# PARAMETERS)

# Model = 0 Single-porosity van Genuchten - Maulem
# Model = 3 Single-porosity van Genuchten - Maulem with air-entry value of -2 cm
# Model = 1 Single-porosity modified van Genuchten (Vogel and Cislerova 1988)
# Model = 2 Single-porosity Brooks-Corey (1964)
# Model = 4 Kosugi (1996) (the log normal model)
# Model = -4 Kosugi (1996) (the log normal model) with Brunswick model modification (Weber et al 2019)
# Model = 5 Dual-porosity (Durner, 1994; the dual van Genuchten-Maulem model)
# Model = 6 Dual-porosity (mobile-immobile water, water content mass transfer)
# Model = 7 Dual-porosity (mobile-immobile water, pressure head mass transfer)
# Model = 8 Dual-permeability (add-on Module)
# Model = 9 Look-up Table with Hydraulic capacity calculated
# Model = 9 Look-up Table with Hydraulic capacity provided [not sure what changed in selector.in file]


## Hysteresis option only available for the following models: 0, 3
# Hysteresis = 0 No Hysteresis
# Hysteresis = 1 Hysteresis in retention curve (Kool & Parker, 1987)
# Hysteresis = 2 Hysteresis in Retention Curve and conductivity
# Hysteresis = 3 Hysteresis in retention curve (no pumpingl; Lenhard and Parker, 1992)

## only when hystersis turned on:
# Kappa = -1 initially drying curve
# Kappa = 1 initially wetting curve

## BLOCK C
# dt = initial time step
# dtMin = minimum time step
# dtMax = maximum time step
# DMul = lower time step multiplication factor
# DMul2 = upper time step multiplication factor
# ItMin = lower optimal iteration range
# ItMax = upper optimal iteration range
# MPL = max print length (?): number of time steps to print out (n_print)
# tInit = initial time
# tMax = final time

# lPrintD = interval output option (t), otherwise f
# nPrintSteps = T-level information Every n time steps
# tPrintInterval = time interval for interval output option
# lEnter = Press Enter at the End (default on, t)
