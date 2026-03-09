edit_meteo_file <- function(hydrus_model,
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
                                                           root_depth = 0)){


  ## NEED TO ADD ERROR CHECKING

  ## Get a basic MATEO.IN template
  meteo_template <- readLines(file("./templates/METEO.IN"))
  ## write to project_path
  write(meteo_template, file = file.path(hydrus_model$hydrus_project$project_path, "METEO.IN"))

  if(radiation_type == "potential_radiation"){
    radiation_type_code <- "0"
  }
  if(radiation_type == "solar_radiation"){
    radiation_type_code <- "1"
  }
  if(radiation_type == "net_radiation"){
    radiation_type_code <- "2"
  }

  if(et_equation == "penman-monteith"){
    et_eq_code <- "f"
  }
  if(et_equation == "hargreaves"){
    et_eq_code <- "t"
  }

  # MeteoRecords Radiation Penman-Hargreaves
  # 7        0       f

  ## Radiation = 0 for "potential radiation"
  ## Radiation = 1 for "solar radiation"
  ## Radiation = 2 for " net radiation"; when this options, options are reduced to the next line and then WindHeight line and down

  meteo_template[grep("MeteoRecords", meteo_template) + 1] <- str_flatten(c(rep(" ", times = 11),
                                                                            nrow(meteo_time_series),
                                                                            rep(" ", times = 8),
                                                                            radiation_type_code,
                                                                            rep(" ", times = 8),
                                                                            et_eq_code))


  #  lEnBal  lDaily  lDummy  lDummy  lDummy  lDummy  lDummy  lDummy  lDummy  lDummy
  #       f       f       f       f       f       t       f       f       f       f

  # Latitude  Altitude
  # 40        110
  meteo_template[grep("Latitude", meteo_template) + 1] <- str_flatten(c(rep(" ", times = 7),
                                                                        latitude,
                                                                        rep(" ", times = 7),
                                                                        altitude))
  ## Latitude is in degrees with north + and south -
  ## altitude in meters

  # ShortWaveRadA  ShortWaveRadB
  # 0.25            0.5

  ## shortwave radiation angstrom values a and b
  meteo_template[grep("ShortWaveRadA", meteo_template) + 1] <- str_flatten(c(rep(" ", times = 10),
                                                                             sw_angstrom_a,
                                                                             rep(" ", times = 10),
                                                                             sw_angstrom_b))

  # LongWaveRadA   LongWaveRadB
  # 0.9            0.1

  ## cloudiness effects on longwave radiation (set 0 if input transmisssivity coeff)
  meteo_template[grep("LongWaveRadA  ", meteo_template) + 1] <- str_flatten(c(rep(" ", times = 10),
                                                                             lw_cloudiness_a,
                                                                             rep(" ", times = 10),
                                                                             lw_cloudiness_b))

  # LongWaveRadA1  LongWaveRadB1
  # 0.34         -0.139

  ## emissivity effect on long wave radiation
  meteo_template[grep("LongWaveRadA1", meteo_template) + 1] <- str_flatten(c(rep(" ", times = 10),
                                                                            lw_emissivity_a,
                                                                            rep(" ", times = 10),
                                                                            lw_emissivity_b))

  # WindHeight     TempHeight
  # 200            200

  ## measurement heights cm
  meteo_template[grep("WindHeight", meteo_template) + 1] <- str_flatten(c(rep(" ", times = 10),
                                                                             windspeed_meas_height,
                                                                             rep(" ", times = 10),
                                                                             temp_meas_height))

  ## WHAT ELSE NEEEDS TO BE DONE WITH ICROP??
  # iCrop (=0: no crop, =1: constant, =2: table, =3: daily)  SunShine  RelativeHum
  # 0                                                0         0
  ## SunShine = 0 for "sunshine"
  ## SunShine = 1 for "cloudiness"
  ## SunShine = 2 for "transmission c"
  ## SunShine = 3 for "solar radiation"; add extra row after this one with: CloudFactAC    CloudFactBC (cloudiness factor for solar radiation)

  ## RelativeHum = 0 use relative humidity in the penman-monteith eq
  ## RelativeHum = 1 use vapor pressure in the penman-monteith eq

  if(crop_option == "no_crop"){
    crop_opt_code <- 0
  }
  if(crop_option == "constant"){
    crop_opt_code <- 1
  }
  if(crop_option == "table"){
    crop_opt_code <- 2
  }
  if(crop_option == "daily"){
    crop_opt_code <- 3
  }

  if(cloudiness_option == "sunshine"){
    cloud_opt_code <- 0
  }
  if(cloudiness_option == "cloudiness"){
    cloud_opt_code <- 1
  }
  if(cloudiness_option == "transmission_c"){
    cloud_opt_code <- 2
  }
  if(cloudiness_option == "solar_radiation"){
    cloud_opt_code <- 3
  }

  if(penman_montieth_option == "relative_humidity"){
    rh_option <- 0
  }
  if(penman_montieth_option == "vapor_pressue"){
    rh_option <- 1
  }

  icrop_line <- str_split(meteo_template[grep("iCrop", meteo_template) + 1], "", simplify = T)
  icrop_line[,10] <- crop_opt_code
  icrop_line[,59] <- cloud_opt_code
  icrop_line[,69] <- rh_option
  meteo_template[grep("iCrop", meteo_template) + 1] <- str_flatten(icrop_line)


  ## Insert shortwave cloudiness factors if the radiation type used is "solar_radiation"
  if(radiation_type == "solar_radiation"){
    c(meteo_template[1:grep("iCrop", meteo_template) + 1],
      "  CloudFactAC    CloudFactBC",
      rep(" ", times = 10),
      sw_cloudiness_a,
      rep(" ", times = 10),
      sw_cloudiness_b,
      meteo_template[grep("Albedo", meteo_template)[1]:length(meteo_template)])
  }

  # Albedo
  # 0.23
  meteo_template[grep("Albedo", meteo_template)[1] + 1] <- str_flatten(c(rep(" ", times = 6),
                                                                       albedo))

  # Daily values
  # t        Rad        TMax        TMin     RHMean      Wind    SunHours CropHeight     Albedo   LAI(SCF)      rRoot
  # [T]  [MJ/m2/d]       [C]         [C]       [%]     [km/d]     [hour]      [L]           [-]        [-]        [L]

  ## CropHeight, Albedo, LAI and rRoot not needed when no crop growth

  end_line <- meteo_template[length(meteo_template)]


  ## Assign meteo time series ####

  # make sure the columns are in correct order:
  met_ts <- meteo_time_series %>%
    dplyr::select(
      time, radiation, temp_max, temp_min, rh_mean, windspeed, sun_hours, crop_height, albedo, lai, root_depth
    )

  # insert into template
  for(i in 1:nrow(met_ts)){
    meteo_template[grep("TMax", meteo_template)+(1+i)] <- paste(rep(" ", times = 6),
                                                                as.character(unlist(met_ts[i,])),
                                                                collapse = str_flatten(rep(" ", times =  10)))
  }

  # add end line
  meteo_template[grep("TMax", meteo_template)+nrow(met_ts)+2] <- end_line

  ## cut template to just the first end of file line (remove everything else)
  meteo_template <- meteo_template[1:grep("end", meteo_template)[1]]

  ## Update METEO.IN
  writeLines(meteo_template, file.path(hydrus_model$hydrus_project$project_path, "METEO.IN"))
  cat("Updated METEO.IN file... \n")

}
