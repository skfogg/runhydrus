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
                               inverse = FALSE,
                               time_var_bc = FALSE,
                               meteorological_data = FALSE,
                               snow_hydrology = FALSE,
                               vapor_flow = FALSE,
                               particle_tracking = FALSE,
                               solute_transport_model = list(equilibrium = TRUE),
                               root_water_uptake_opts = list(active_solute_uptake = FALSE),

                               internal_interpolation_tables = TRUE,
                               internal_interp_params <- list(lower_limit_tension_interval = 1e-06,
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
                               print_options = list(screen_output = TRUE,
                                                    print_fluxes_not_temp = TRUE),

                               iteration_criteria = list(maximum_iterations = 10,
                                                         water_content_tol = 0.001,
                                                         pressure_head_tol = 1),
                               water_flow_bcs = list(upper_bc = "constant_pressure_head",
                                                     lower_bc = "free_drainage",
                                                     triggered_irrigation = FALSE),
                               particle_tracking_params = list(init_water_storage = 2,
                                                            cumulative_surface_flux = 2)
                               ){
  require(stringr)
  bottom_bc_opts <- readRDS("data/bottom_bc_opts.rds")
  top_bc_opts <- readRDS("data/top_bc_opts.rds")

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
  }if(soil_hydraulic_model == 1){
    # modified VGM
    if(!all(names(soil_hydraulic_parameters) %in% c("theta_r", "theta_s", "alpha", "n", "K_s", "l", "theta_m", "theta_a", "theta_k", "K_k"))){
      stop("Error in soil_hydraulic_parameters: Soil hydraulic model is 1 (modified van Genuchten). Required parameters are: 'theta_r', 'theta_s', 'alpha', 'n', 'K_s', 'l', 'theta_m', 'theta_a', 'theta_k', 'K_k.")
    }
  }if(soil_hydraulic_model == 2){
    # Brooks-Corey
    if(!all(names(soil_hydraulic_parameters) %in% c("theta_r", "theta_s", "alpha", "n", "K_s", "l"))){
      stop("Error in soil_hydraulic_parameters: Soil hydraulic model is 2 (Brooks-Corey). Required parameters are: 'theta_r', 'theta_s', 'alpha', 'n', 'K_s', and 'l'.")
    }
  }if(model == 4){
    # Kosugi
    if(!all(names(soil_hydraulic_parameters) %in% c("theta_r", "theta_s", "alpha", "n", "K_s", "l"))){
      stop("Error in soil_hydraulic_parameters: Soil hydraulic model is 4 (Kosugi). Required parameters are: 'theta_r', 'theta_s', 'alpha', 'n', 'K_s', and 'l'.")
    }
  }if(model == -4){
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
  ## insert project description
  selector_template[4] <- hydrus_model$description

  ## length unit, time unit, mass unit
  selector_template[6] <- hydrus_model$discritization$space_unit
  selector_template[7] <- hydrus_model$discritization$time_unit
  selector_template[8] <- ifelse(is.null(hydrus_model$discritization$mass_unit), "-", hydrus_model$discritization$mass_unit)

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
  selector_template[10] <- str_flatten(opt_1[1,])
  selector_template[12] <- str_flatten(opt_2[1,])

  ## set the number of materials
  geometry <- str_split(selector_template[14], "", simplify = T)
  geometry[1,3] <- as.character(hydrus_model$geometry$material_numbers)
  selector_template[14] <- str_flatten(geometry)

  #### *** BLOCK B: WATER FLOW INFORMATION *** ####
  ### Iteration Criteria: ####
  selector_template[grep("MaxIt ",  selector_template) + 1] <- str_flatten(c(" ", " ",
                                                                             iteration_criteria$maximum_iterations,
                                                                             " ", " ", " ", " ",
                                                                             iteration_criteria$water_content_tol,
                                                                             " ", " ", " ", " ", " ", " ",
                                                                             iteration_criteria$pressure_head_tol))
  # --------------------------------------------------------------------
  ### Assign top boundary conditions: TopInf WLayer KodTop InitCond ####
  top_boundary <- str_split(selector_template[grep("TopInf", selector_template) + 1], "", simplify = T)
  top_bc_slots <- c(2, 8, 16, 24)

  ## get bc options from user chosen top bc in options data frame
  specified_top_bc <- unlist(top_bc_opts[top_bc_opts$top_bc_name == water_flow_bcs$upper_bc, 2:length(top_bc_opts)])
  if(hydrus_model$initial_condition){
    specified_top_bc["InitCond"] <- "t"
  }

  ## assign options values in correct slots
  if(str_count(specified_top_bc["KodTop"]) == 1){
    top_boundary[,15] <- " "
    top_boundary[,top_bc_slots] <- specified_top_bc
  }else{
    top_bc_slots <- c(2, 8, 15, 16, 24)
    top_boundary[,top_bc_slots] <- c(specified_top_bc[1:2], str_split(specified_top_bc["KodTop"], "", simplify = T), specified_top_bc[4])
  }

  ## update selector template with new values
  selector_template[grep("TopInf", selector_template) + 1] <- str_flatten(top_boundary)

  ## TO DO: when upper bc == 'constant_flux', add rTop rBot rRoot
  ## TO DO: when upper bc is 'atm bc with surface layer/runoff" and triggered irrigaiton on, add Node Pressure Irrig_rate Duration Lag_period

  # --------------------------------------------------------------------------------------
  ### Assign bottom boundary conditions: BotInf qGWLF FreeD SeepF KodBot DrainF hSeep ####
  bottom_boundary <- str_split(selector_template[grep("BotInf", selector_template) + 1], "", simplify = T)
  bottom_bc_slots <- c(2, 8, 14, 20, 26, 34, 40)

  ## get bc options from user chosen bottom bc in options data frame
  specified_bottom_bc <- unlist(bottom_bc_opts[bottom_bc_opts$bottom_bc_name == water_flow_bcs$lower_bc, 2:length(bottom_bc_opts)])

  ## assign options values in correct slots
  bottom_boundary[,bottom_bc_slots] <- specified_bottom_bc

  ## update selector template with new values
  selector_template[grep("BotInf", selector_template) + 1] <- str_flatten(bottom_boundary)

  # -------------------------------------
  ## Insert internal interpolation tables
  if(internal_interpolation_tables){
    c(selector_template[1:grep("BotInf", selector_template) + 1],
      str_flatten(rep(" ", times = 4), "hTab1", rep(" ", times = 3), "hTabN"),
      str_flatten(rep(" ", times = 5), internal_interp_params$lower_limit_tension_interval, rep(" ", times = 3), internal_interp_params$upper_limit_tension_interval),
      selector_template[grep("BotInf", selector_template) + 2:length(selector_template)]
      )
  }

  # ------------------------------------
  ## Insert particle tracking parameters
  if(particle_tracking){
    c(selector_template[1:grep("BotInf", selector_template) + 1],
      str_flatten(c(rep(" ", times = 7), "wInit", rep(" ", times = 8), "wPrec")),
      str_flatten(c(rep(" ", times = 9), particle_tracking_params$init_water_storage, rep(" ", times = 10), particle_tracking_params$cumulative_surface_flux)),
      selector_template[grep("BotInf", selector_template) + 2:length(selector_template)]
    )
  }

  # ----------------------------------------
  ## Assign soil hydraulic model and hysteresis
  soil_model <- str_split(selector_template[grep("Hysteresis", selector_template) + 1], "", simplify = T)
  soil_model[1,c(7,18)] <- as.character(c(soil_hydraulic_model, hysteresis))
  selector_template[grep("Hysteresis", selector_template) + 1] <- str_flatten(soil_model)

  ## Insert Kappa parameter when hysteresis selected
  if(hysteresis > 0){
    selector_template <- c(selector_template[1:grep("Hysteresis", selector_template) + 1],
                           str_flatten(c(rep(" ", times = 4), "Kappa")),
                           ifelse(initially_drying_curve,
                                  str_flatten(c(rep(" ", times = 5), "-1")),
                                  str_flatten(c(rep(" ", times = 6), "1"))),
                           selector_template[grep("Hysteresis", selector_template) + 2:length(selector_template)]
                           )
  }

  # ----------------------------------------------------------------
  ## Assign soil hydraulic property parameters ####
  ## NOTE: this creates three spaces between each number, where the original spacing is mode variable--I'm not sure if it will matter
  for(i in 1:hydrus_model$geometry$material_numbers){
    selector_template[selector_template[grep("thr", selector_template)]+i] <- paste("  ",
                                                                                    as.character(
                                                                                      unlist(soil_hydraulic_parameters[i,2:length(soil_hydraulic_parameters)])
                                                                                    ), collapse = "    ")
  }




  ## *** BLOCK C: TIME INFORMATION ***
  # lPrintD
  # nPrintSteps
  # tPrintInterval
  # lEnter = Press Enter at the End (default on)

  ## *** BLOCK F: SOLUTE TRANSPORT INFORMATION ***

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
