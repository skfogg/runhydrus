## SANDBOX ##

source("C:/Users/skati/Documents/runhydrus/R/create_hydrus_project.R")

hydrus_project <- create_hydrus_project(project_name = "my_test_model_2",
                                         parent_dir = "C:/Users/skati/Documents/runhydrus/tests")

hydrus_model <- parameterize_hydrus_model(hydrus_project,
                                          time_variable_bc = list(time_variable_bc = TRUE,
                                                                  meteorological_data = FALSE,
                                                                  repeat_bc_records = FALSE,
                                                                  repeat_bc_records_n = 1,
                                                                  daily_var_transpiration = FALSE,
                                                                  sinusoidal_var_precipitation = FALSE),
                                          water_flow_bcs = list(upper_bc = "atm_bc_with_surface_layer",
                                                                initial_condition = FALSE, ## a top bc param
                                                                lower_bc = "free_drainage",
                                                                triggered_irrigation = FALSE))

edit_soil_profile(hydrus_model,
                  mesh_density = data.frame(fixed_points = c(0, -hydrus_model$geometry$profile_depth),
                                            upper_relative_size_fe = 2.0,
                                            lower_relative_size_fe = 2.0))
edit_atmosph_file(hydrus_model,
                  atm_time_series = data.frame(time = c(1,2,3),
                                               precip = c(4, 5, 6),
                                               evap = c(7, 8 , 9),
                                               min_pressure_head = 100))
edit_meteo_file(hydrus_model,
                meteo_time_series = data.frame(time = c(1,2),
                                               radiation = c(300, 300),
                                               temp_max = c(20,20),
                                               temp_min = c(10,10),
                                               rh_mean = c(95,95),
                                               windspeed = c(30,30),
                                               sun_hours = 4,
                                               crop_height = 0,
                                               albedo = 0,
                                               lai = 0,
                                               root_depth = 0))
