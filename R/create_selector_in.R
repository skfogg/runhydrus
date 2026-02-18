create_selector_in <- function(hydrus_model,
                               soil_hydraulic_model = 0,
                               hysteresis = NULL,
                               initial_drying = FALSE,
                               soil_hydraulic_parameters = data.frame(theta_r = 0.045,
                                                                      theta_s = 0.43,
                                                                      alpha = 0.145,
                                                                      n = 2.68,
                                                                      K_s = 712.8,
                                                                      l = 0.5),
                               ){
  require(stringr)

  selector_in_file <- file.path(hydrus_model$project_path, "SELECTOR.IN")
  file.create(selector_in_file)

  selector_template <- readLines(file("./templates/SELECTOR.IN"))

  ## update file version to match hydrus version
  selector_template[1] <- paste0("Pcp_File_Version=", hydrus_model$hydrus_version)

  selector_template[4] <- hydrus_model$description



  str_subset(selector_template, "Version")

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
  # Hysteresis = 1 Hysteresis in retention curve (Kool & Parker, 1987)
  # Hysteresis = 2 Hysteresis in Retention Curve and conductivity
  # Hysteresis = 3 Hysteresis in retention curve (no pumpingl; Lenhard and Parker, 1992)

  ## only when hystersis turned on:
  # Kappa = -1 initially drying curve
  # Kappa = 1 initially wetting curve

}
