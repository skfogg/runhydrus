##
## Meghan's Dual-permeability model of Fairfield
##

## To install from github:
# devtools::install_github("runhydrus", username = "skfogg")
library(runhydrus)
library(readr)

## FIRST FOLLOW INSTRUCTIONS IN THE README OF THE RUNHYDRUS PACKAGE!
## Must do so to set permissions correctly for HYDRUS to run

## Create a new HYDRUS project:
### * parent_directory needs to be the full path and not use dot expansion
fairfield_dual_perm_kbr <-  create_hydrus_project(project_name = "fairfield_dual_perm_kbr",
                                                  parent_dir = "C:/Users/skati/Documents/runhydrus/examples",
                                                  description = "Replicate Fairfield dual perm model using runhydrus, add artificial KBr solute")
## Parameterize model:
## Model Units
fairfield_dual_perm_kbr$model_units <- model_units(mass_unit = "mmol")
## Main processes
fairfield_dual_perm_kbr$main_processes <- main_processes(water_flow = T,
                                                         root_water_uptake = T,
                                                         root_growth = T,
                                                         solute_transport = T)
## Geometry:
fairfield_dual_perm_kbr$geometry <- geometry(number_materials = 3,
                                             profile_depth = 175,
                                             observation_nodes_n = 10,
                                             observation_nodes = c(2, 5, 9, 14, 18, 27, 41, 52, 64, 84),
                                             number_subregions = 1)
## Root Growth:
fairfield_dual_perm_kbr$root_growth <- root_growth(root_growth_factor = 1,
                                                   root_growth_depth = 2,
                                                   root_growth_params = data.frame("initial_root_growth_time" = 115,
                                                                                   "harvest_time" = 245,
                                                                                   "initial_rooting_depth" = 0,
                                                                                   "maximum_rooting_depth" = 60,
                                                                                   "time_period" = 365))
## Time Variable Boundary Conditions
fairfield_dual_perm_kbr$time_variable_bc <- time_variable_bc(time_variable_bc = T)
## Water Flow Boundary Conditions
fairfield_dual_perm_kbr$water_flow_bcs <- water_flow_bcs(upper_bc = "atm_bc_with_surface_runoff")
## Print Options
fairfield_dual_perm_kbr$print_options <- print_options(times_to_print = data.frame(times = 1334:1698),
                                                       print_times = FALSE,
                                                       interval_output = 1)
## Time Parameters
fairfield_dual_perm_kbr$time_parameters <- time_parameters(final_model_time = 1698)
## Root Water Uptake
fairfield_dual_perm_kbr$root_water_uptake <- root_water_uptake(root_water_uptake_model = 1,
                                                           root_water_uptake_params = data.frame(critical_stress_index = 0,
                                                                                                 h50 = -800,
                                                                                                 P3 = 3))
## Solute Transport
fairfield_dual_perm_kbr$solute_transport <- solute_transport(equilibrium_adsorption = FALSE,
                                                             number_solutes = 1,
                                                             stability_criterion = 2,
                                                             pulse_duration = 1698,
                                                             nonlinear_adsorption_iteration_criteria = data.frame(absolute_conc_tol = 0,
                                                                                                                  relative_conc_tol = 0,
                                                                                                                  maximum_n_iteration = 1),
                                                             material_params = data.frame(bulk_density = 1.5,
                                                                                          longitudinal_dispersivity = 17.5,
                                                                                          fraction_adsorption_sites = 1,
                                                                                          immobile_water_content = 0),
                                                             solute_params = data.frame(molecular_diffusion_free_water = 1.74,
                                                                                        molecular_diffusion_soil_air = 0),
                                                             solute_transport_bcs = data.frame(upper_bc = "concentration_bc",
                                                                                               lower_bc = "zero_conc_gradient",
                                                                                               in_total_concentrations = FALSE,
                                                                                               stagnant_boundary_layer = NA,
                                                                                               concentration_in_atmosphere = NA,
                                                                                               fractionation_ratio = NA))


## Soil Hydraulic Model
fairfield_dual_perm_kbr$soil_hydraulics <- soil_hydraulics(soil_hydraulic_model = 8,
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
## Apply the Updated Parameterization:
parameterize_hydrus_model(fairfield_dual_perm_kbr)

## Alter necessary input files:
## Time-Variable Atmospheric Inputs:
meghans_atmosph <- readr::read_csv("./examples/meghan_atmosph_full_data.csv")
kbr_conc <- 100 # mmol/cm
kbr_bc_top <- meghans_atmosph$Prec * kbr_conc
# readr::write_csv(data.frame(mmol_per_cm = kbr_bc_top), "./examples/kbr_bc_top.csv")

edit_atmosph_file(fairfield_dual_perm_kbr,
                  atm_time_series = data.frame(time = meghans_atmosph$tAtm,
                                               precip = meghans_atmosph$Prec,
                                               evap = meghans_atmosph$rSoil,
                                               transpiration = meghans_atmosph$rRoot,
                                               min_pressure_head = meghans_atmosph$hCritA,
                                               top_conc = kbr_bc_top,
                                               bot_conc = 0),
                  max_h_at_surface = 0)

## Soil Profile Mesh:
## mat: nodes 1-6 = material 1 (0 to -8.75 cm), nodes 7-87 = material 2,
##      nodes 88-101 = material 3 (-152.25 to -175 cm)
edit_soil_profile(fairfield_dual_perm_kbr,
                  mesh_density = data.frame(fixed_points = c(0, -175),
                                            upper_relative_size_fe = 1.0,
                                            lower_relative_size_fe = 1.0),
                  nodal_soil_properties = list(h = -150,
                                               root = 0,
                                               a_xz = 1,
                                               b_xz = 1,
                                               d_xz = 1,
                                               mat = c(rep(1, 6), rep(2, 81), rep(3, 14)),
                                               lay = 1,
                                               fracture_conc = 0))

run_hydrus_1D(fairfield_dual_perm_kbr)


## Run HYDRUS:
# slag <- list(hydrus_project = list(hydrus_version = 5,
#                                    project_path = "C:/Users/skati/Documents/runhydrus/examples/fairfield_dual_perm_kbr"),
#              soil_hydraulics = list(soil_hydraulic_model = 8))
# run_hydrus_1D(slag)

