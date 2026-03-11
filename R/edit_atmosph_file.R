edit_atmosph_file <- function(hydrus_model,
                              atm_time_series = data.frame(time = 1,
                                                               precip = 0,
                                                               evap = 0,
                                                               min_pressure_head = 100000),
                              max_h_at_surface = 0


){

  # To be used when time variable bc selected, after the parameterize_hydrus_model has been run, therefore, first check that there is time var bc option on

  if(!hydrus_model$time_variable_bc$time_variable_bc){
    stop("Error in hydrus_model parameterization. time_variable_bc$time_variable_bc must be TRUE to edit time-variable atmospheric boundary conditions.")
  }

  ## you can click that you want atmospheric boundary conditions, but if the
  ## upper bc is one of the 'constant' options, then there is nothing to vary.
  ## --> have a check that you can't use time variable options when you have
  ##     chosen constant upper bcs
  if(stringr::str_detect(hydrus_model$water_flow_bcs$upper_bc, "constant")){
    stop("Error in hydrus_model parameterization. Time-variable boundary conditions cannot be used when the upper flow boundary condition is constant.")
  }

  ## Error check correct input data given upper water flow bc
  if(hydrus_model$water_flow_bcs$upper_bc == "atm_bc_with_surface_layer"){
    if(!all(colnames(atm_time_series) %in% c("time", "evap", "precip", "min_pressure_head"))){
      stop("Error in atm_time_series. When upper boundary condition of water flow is set to 'atm_bc_with_surface_layer',
           then atmospheric time series must have columns 'time', 'precip', 'evap', and 'min_pressure_head'.")
    }
    if(max_h_at_surface != 0){
      warning("Warning: max haight at soil surface option was changed from the default 0. This option is only used when the upper water
              flow bc is set to 'atm_bc_with_surface_layer'.")
    }
  }

  if(hydrus_model$water_flow_bcs$upper_bc == "atm_bc_with_surface_runoff"){
    if(!all(colnames(atm_time_series) %in% c("time", "evap", "precip", "min_pressure_head"))){
      stop("Error in atm_time_series. When upper boundary condition of water flow is set to 'atm_bc_with_surface_runoff',
           then atmospheric time series must have columns 'time', 'precip', 'evap', and 'min_pressure_head'.")
    }
  }

  if(hydrus_model$water_flow_bcs$upper_bc == "variable_pressure_head"){
    if(!all(colnames(atm_time_series) %in% c("time", "pressure_head"))){
      stop("Error in atm_time_series. When upper boundary condition of water flow is set to 'variable_pressure_head',
           then atmospheric time series must have columns 'time' and 'pressure_head'.")
    }
  }

  if(hydrus_model$water_flow_bcs$upper_bc == "variable_pressure_head/flux"){
    if(!all(colnames(atm_time_series) %in% c("time", "flux_top", "min_pressure_head", "pressure_head", "flux_or_head"))){
      stop("Error in atm_time_series. When upper boundary condition of water flow is set to 'variable_pressure_head/flux',
           then atmospheric time series must have columns 'time', 'flux_top', 'min_pressure_head', pressure_head', and 'flux_or_head'.")
    }
  }

  ## Get a basic ATMOSPH.IN template
  atmosph_template <- readLines(file("./R/templates/ATMOSPH.IN"))
  ## write to project_path
  write(atmosph_template, file = file.path(hydrus_model$hydrus_project$project_path, "ATMOSPH.IN"))

  ## update the number of atmospheric observations (based on the number of rows in the given data.frame)
  atmosph_template[grep("MaxAL", atmosph_template) + 1] <- stringr::str_flatten(c(rep(" ", times = 6),
                                                                       nrow(atm_time_series)))

  time_var_bc_options <- stringr::str_split(atmosph_template[grep("DailyVar", atmosph_template) + 1], "", simplify = T)

  if(hydrus_model$time_variable_bc$daily_var_transpiration){
    time_var_bc_options[,8] <- "t"
    atmosph_template[grep("DailyVar", atmosph_template) + 1] <- stringr::str_flatten(time_var_bc_options)
  }
  if(hydrus_model$time_variable_bc$sinusoidal_var_precipitation){
    time_var_bc_options[,16] <- "t"
    atmosph_template[grep("DailyVar", atmosph_template) + 1] <- stringr::str_flatten(time_var_bc_options)
  }
  if(hydrus_model$time_variable_bc$repeat_bc_records){
    time_var_bc_options[,32] <- "t"
    atmosph_template[grep("DailyVar", atmosph_template) + 1] <- stringr::str_flatten(time_var_bc_options)

    ## add in Cycles lines
    atmosph_template <- c(atmosph_template[1:grep("DailyVar", atmosph_template) + 1],
                          " Number of Cycles",
                          rep(" ", times = 7),
                          hydrus_model$time_variable_bc$repeat_bc_records_n,
                          atmosph_template[grep("DailyVar", atmosph_template) + 2:length(atmosph_template)])
  }

  if(hydrus_model$water_flow_bcs$upper_bc == "atm_bc_with_surface_layer"){
    atmosph_template[grep("hCritS", atmosph_template) + 1] <- stringr::str_flatten(c(rep(" ", times = 6),
                                                                            max_h_at_surface))
  }

  if(hydrus_model$water_flow_bcs$upper_bc %in% c("atm_bc_with_surface_layer", "atm_bc_with_surface_runoff")){
    fill_in_df <- data.frame(tAtm = atm_time_series$time,
                             Prec = atm_time_series$precip,
                             rSoil = atm_time_series$evap,
                             rRoot = 0,
                             hCritA = atm_time_series$min_pressure_head,
                             rB = 0, hB = 0,
                             ht = 0, RootDepth = 0)
  }
  if(hydrus_model$water_flow_bcs$upper_bc == "variable_pressure_head"){
    fill_in_df <- data.frame(tAtm = atm_time_series$time,
                             Prec = 0,
                             rSoil = 0,
                             rRoot = 0,
                             hCritA = 0,
                             rB = 0,
                             hB = 0,
                             ht = atm_time_series$pressure_head,
                             RootDepth = 0)
  }
  if(hydrus_model$water_flow_bcs$upper_bc == "variable_pressure_head"){
    fill_in_df <- data.frame(tAtm = atm_time_series$time,
                             Prec = 0,
                             rSoil = 0,
                             rRoot = 0,
                             hCritA = 0,
                             rB = 0,
                             hB = 0,
                             ht = atm_time_series$pressure_head,
                             RootDepth = 0)
  }
  if(hydrus_model$water_flow_bcs$upper_bc == "variable_pressure_head/flux"){
    fill_in_df <- data.frame(tAtm = atm_time_series$time,
                             Prec = atm_time_series$flux_top,
                             rSoil = atm_time_series$flux_or_head,
                             rRoot = 0,
                             hCritA = atm_time_series$min_pressure_head,
                             rB = 0,
                             hB = 0,
                             ht = atm_time_series$pressure_head,
                             RootDepth = 0)
  }

  end_line <- atmosph_template[length(atmosph_template)]


  ## Assign soil hydraulic property parameters ####
  ## NOTE: this creates three spaces between each number, where the original spacing is mode variable--I'm not sure if it will matter
  for(i in 1:nrow(atm_time_series)){
    atmosph_template[grep("tAtm", atmosph_template)+i] <- paste(rep(" ", times = 7),
                                                                 as.character(unlist(fill_in_df[i,])),
                                                                 collapse = stringr::str_flatten(rep(" ", times =  10)))
  }

  atmosph_template[grep("tAtm", atmosph_template)+nrow(atm_time_series)+1] <- end_line

  ## cut template to just the first end of file line (remove everything else)
  atmosph_template <- atmosph_template[1:grep("end", atmosph_template)[1]]

  ## Update ATMOISPH.IN
  writeLines(atmosph_template, file.path(hydrus_model$hydrus_project$project_path, "ATMOSPH.IN"))
  cat("Updated ATMOSPH.IN file... \n")




  # when upper bc ==
  # "atm bc with surface layer" var_bc_params = c(time, precip, evap, hCritA) with 'max h at soil surface' option
  # hCritS                 (max. allowed pressure head at the soil surface)
  # 8

  # "atm bc with runoff" var_bc_params = c(time, precip, evap, hCritA)
  # "variable pressure head" var_bc_params = c(time, hTop),
  # "variable pressure head/flux" var_bc_params = c(time, FluxTop, hCritA, hTop, KodTop)
  #        tAtm        Prec       rSoil       rRoot      hCritA          rB          hB          ht    RootDepth

  # min pressure head at soil surface = hCritA

  ## constant pressure head changes to water content only

  ## rSoil = Evap.
  ## ht = hTop
  ## Precip column used for FluxTop in variable head/flux opt.
  ## rSoil column used for KodTop in variable head/flux opt.

  ## when meteorological option on, 'evap' not used in atmosph.in file. (numbers in file did not change)

  ## DailyVar  SinusVar  lLay  lBCCycles lInterc lHeadInt lDummy  lDummy  lDummy  lDummy
  # default, all f
  # DailyVar = "Daily variations in transpiration during day generated by hydrus'
  # SinusVar = "sinusoidal variations in transpiration generated by Hydrus'
  #
  # Repeat the same set of BC records n number of times: (not available when meteoro option on)
  # added below ^ line :
  # Number of Cycles
  # 3
}
