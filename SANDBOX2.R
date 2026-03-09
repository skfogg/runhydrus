
### Replicate Meghan's Model Test ###


fairfield_duel_perm <-  create_hydrus_project(project_name = "fairfield_duel_perm",
                                              parent_dir = "C:/Users/skati/Documents/runhydrus/tests",
                                              description = "Replicate Fairfield duel perm model using runhydrus")

# fairfield_duel_perm$main_processes$particle_tracking <- TRUE

fairfield_duel_perm$main_processes <- main_processes(water_flow = T, root_water_uptake = T, root_growth = T)
fairfield_duel_perm$geometry <- geometry(number_materials = 3,
                                         profile_depth = 175,
                                         observation_nodes_n = 10,
                                         observation_nodes = c(2, 5, 9, 14, 18, 27, 41, 52, 64, 84),
                                         number_subregions = 1)
fairfield_duel_perm$root_growth <- root_growth(root_growth_factor = 1,
                                   root_growth_depth = 2,
                                   root_growth_params = data.frame("initial_root_growth_time" = 115,
                                                                   "harvest_time" = 245,
                                                                   "initial_rooting_depth" = 0,
                                                                   "maximum_rooting_depth" = 60,
                                                                   "time_period" = 365))

fairfield_duel_perm$time_variable_bc <- time_variable_bc(time_variable_bc = T,
                                                         meteorological_data = T)
fairfield_duel_perm$water_flow_bcs <- water_flow_bcs(upper_bc = "atm_bc_with_surface_runoff")
fairfield_duel_perm$print_options <- print_options(times_to_print = data.frame(times = 861),
                                                   interval_output = 1)
fairfield_duel_perm$time_parameters <- time_parameters(final_model_time = 861)
fairfield_duel_perm$root_water_uptake <- root_water_uptake(root_water_uptake_model = 1,
                                                           root_water_uptake_params = data.frame(critical_stress_index = 1,
                                                                                                 h50 = -800,
                                                                                                 P3 = 3))
## STUFF that i'm changing to match meghans's input files:
fairfield_duel_perm$solute_options <- solute_options(equilibrium_adsorption = TRUE)

fairfield_duel_perm$soil_hydraulics <- soil_hydraulics(soil_hydraulic_model = 8,
                                                       surface_flow_into_fracture = 0.15,
                                                       soil_hydraulic_parameters = data.frame(material = 1:3,
                                                                                              theta_r = c(0.0792, 0.0813, 0.05),
                                                                                              theta_s = c(0.47, 0.4231, 0.37),
                                                                                              alpha = c(0.03, 0.019, 0.0353),
                                                                                              n = c(1.35, 1.45, 3.18),
                                                                                              K_s = c(6, 8, 642.98),
                                                                                              l = 0.5,
                                                                                              theta_r_fr = c(0.01298, 0, 0.01),
                                                                                              theta_s_fr = c(0.43903, 0.3, 0.8),
                                                                                              alpha_fr = c(0.017304, 0.079, 0.01),
                                                                                              n_fr = c(1.3202, 1.6, 1.01),
                                                                                              K_s_fr = c(289.89, 75.5, 386.52),
                                                                                              l_fr = 0.5,
                                                                                              w = 0.04,
                                                                                              beta = c(4.74, 2, 1),
                                                                                              gamma = c(0.01, 0.4, 0.4),
                                                                                              a = c(2.197, 7.8, 10),
                                                                                              K_a = c(1.4143, 0.01, 1.497)))




# fairfield_duel_perm$time_parameters$final_model_time <- 861
## NEED TO PUT A CHECK ON THIS ^ time to print must be less than final model time




# fairfield_duel_perm$root_water_uptake$root_water_uptake_params$critical_stress_index <- 1
## NEED TO ADD CHECK ON CRITICAL STRESS INDEX to be 1, 0

## NEED TO ADD SOMETHING THAT WILL ALLOW ME TO RERUN PARAMETERIZE HYDRUS MODEL MULITPLE TIMES WIHTOUT FUCKING UP SELECTOR.IN

parameterize_hydrus_model(fairfield_duel_perm)

edit_atmosph_file(fairfield_duel_perm,
                  atm_time_series = data.frame(time = 1,
                                               precip = 0,
                                               evap = 0,
                                               min_pressure_head = 100000),
                  max_h_at_surface = 0)

edit_meteo_file(fairfield_duel_perm,
                radiation_type = "potential_radiation", # solar_radiation, net_radiation
                et_equation = "penman-monteith", # hargreaves
                cloudiness_option = "sunshine", #cloudiness, transmission_c, solar_radiation
                penman_montieth_option = "relative_humidity", # vapor_pressure
                crop_option = "no_crop", # constant, tables, daily
                latitude = 40,
                altitude = 110,
                sw_angstrom_a = 0.25,
                sw_angstrom_b = 0.5,
                sw_cloudiness_a = 1.35,
                sw_cloudiness_b = -0.35,
                lw_cloudiness_a = 0.9,
                lw_cloudiness_b = 0.1,
                lw_emissivity_a = 0.34,
                lw_emissivity_b = -0.139,
                windspeed_meas_height = 200,
                temp_meas_height = 200,
                albedo = 0.23,
                meteo_time_series = data.frame(time = 1,
                                               radiation = 300,
                                               temp_max = 20,
                                               temp_min = 10,
                                               rh_mean = 95,
                                               windspeed = 30,
                                               sun_hours = 4,
                                               crop_height = 0,
                                               albedo = 0,
                                               lai = 0,
                                               root_depth = 0))


soil_profile_mesh(fairfield_duel_perm,
                  mesh_density = data.frame(fixed_points = c(0, -175),
                                            upper_relative_size_fe = 1.0,
                                            lower_relative_size_fe = 1.0),
                  # mesh_nodes = data.frame(number = 1:fairfield_duel_perm$geometry$number_nodes,
                  #                         z = 0:-fairfield_duel_perm$geometry$profile_depth),
                  nodal_soil_properties = list(h = -175,
                                                     root = 0,
                                                     a_xz = 1,
                                                     b_xz = 1,
                                                     d_xz = 1,
                                                     mat = 1,
                                                     lay = 1,
                                                     temp = 20,
                                                     conc = 0))



run_hydrus_1D(fairfield_duel_perm)



fairfield_t2 <- list(hydrus_project = list(project_path = "C:/Users/skati/Documents/runhydrus/tests/input_from_hydrus",
                                           hydrus_version = 5),
                     soil_hydraulics = list(soil_hydraulic_model = 8)
                     )
run_hydrus_1D(fairfield_t2)


