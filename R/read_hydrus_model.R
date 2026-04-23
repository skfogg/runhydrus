#' Read a HYDRUS model from an existing project directory
#'
#' Reads parameters from SELECTOR.IN in an existing HYDRUS project directory
#' and returns a hydrus_model list compatible with other runhydrus functions.
#'
#' @param project_path Character string. Full absolute path to the HYDRUS project directory.
#'
#' @returns A \code{hydrus_model} list with parameters parsed from SELECTOR.IN and,
#'   when a PROFILE.DAT file is present in the directory, from PROFILE.DAT as well.
#'   PROFILE.DAT supplies \code{geometry$number_nodes}, \code{geometry$profile_depth},
#'   \code{geometry$observation_nodes_n}, and \code{geometry$observation_nodes}.
#' @export
#'
#' @examples read_hydrus_model("C:/Users/skati/Documents/runhydrus/examples/fairfield_dual_perm_kbr")
read_hydrus_model <- function(project_path) {

  ## ----------- helpers ---------------------------------------------------
  parse_tf  <- function(x) unname(x == "t")
  split_ws  <- function(line) unlist(strsplit(trimws(line), "\\s+"))

  selector <- readLines(file.path(project_path, "SELECTOR.IN"))

  ## -----------------------------------------------------------------------
  ## hydrus_project
  ## -----------------------------------------------------------------------
  hydrus_version <- as.integer(sub("Pcp_File_Version=", "", selector[1]))
  project_name   <- basename(project_path)
  description    <- trimws(selector[grep("^Heading", selector) + 1])

  ## -----------------------------------------------------------------------
  ## BLOCK A
  ## -----------------------------------------------------------------------

  ## Units
  lunit_idx  <- grep("^LUnit", selector)
  space_unit <- trimws(selector[lunit_idx + 1])
  time_unit  <- trimws(selector[lunit_idx + 2])
  mass_unit  <- trimws(selector[lunit_idx + 3])
  if (mass_unit == "-") mass_unit <- NULL

  ## lWat line flags
  lwat_hdr_idx <- grep("lWat ", selector)
  lwat_hdr     <- split_ws(selector[lwat_hdr_idx])
  lwat_val     <- split_ws(selector[lwat_hdr_idx + 1])
  names(lwat_val) <- lwat_hdr

  ## lSnow line flags
  lsnow_hdr_idx <- grep("lSnow", selector)
  lsnow_hdr     <- split_ws(selector[lsnow_hdr_idx])
  lsnow_val     <- split_ws(selector[lsnow_hdr_idx + 1])
  names(lsnow_val) <- lsnow_hdr

  ## Geometry: NMat NLay CosAlpha
  nmat_vals         <- split_ws(selector[grep("^NMat", selector) + 1])
  number_materials  <- as.integer(nmat_vals[1])
  number_subregions <- as.integer(nmat_vals[2])

  ## -----------------------------------------------------------------------
  ## BLOCK B
  ## -----------------------------------------------------------------------

  ## Iteration criteria
  maxit_vals         <- split_ws(selector[grep("MaxIt ", selector) + 1])
  maximum_iterations <- as.integer(maxit_vals[1])
  water_content_tol  <- as.numeric(maxit_vals[2])
  pressure_head_tol  <- as.numeric(maxit_vals[3])

  ## Internal interpolation tables (hTab1 / hTabN)
  has_htab <- length(grep("hTab1", selector)) > 0
  if (has_htab) {
    htab_vals                    <- split_ws(selector[grep("hTab1", selector) + 1])
    lower_limit_tension_interval <- as.numeric(htab_vals[1])
    upper_limit_tension_interval <- as.numeric(htab_vals[2])
  } else {
    lower_limit_tension_interval <- 1e-6
    upper_limit_tension_interval <- 10000
  }

  ## Top boundary condition
  topinf_hdr_idx <- grep("^TopInf", selector)
  topinf_hdr     <- split_ws(selector[topinf_hdr_idx])
  topinf_val     <- split_ws(selector[topinf_hdr_idx + 1])
  names(topinf_val) <- topinf_hdr

  TopInf   <- topinf_val["TopInf"]
  WLayer   <- topinf_val["WLayer"]
  KodTop   <- as.integer(topinf_val["KodTop"])
  InitCond <- parse_tf(topinf_val["InitCond"])

  top_bc_opts <- data.frame(
    name   = c("constant_pressure_head", "constant_flux",
               "atm_bc_with_surface_layer", "atm_bc_with_surface_runoff",
               "variable_pressure_head", "variable_pressure_head/flux"),
    TopInf = c("f","f","t","t","t","t"),
    WLayer = c("f","f","t","f","f","f"),
    KodTop = c(1L,-1L,-1L,-1L,1L,0L),
    stringsAsFactors = FALSE
  )
  top_match <- which(top_bc_opts$TopInf == TopInf &
                     top_bc_opts$WLayer == WLayer &
                     top_bc_opts$KodTop == KodTop)
  upper_bc  <- if (length(top_match) > 0) top_bc_opts$name[top_match[1]] else "constant_pressure_head"

  ## Bottom boundary condition
  botinf_hdr_idx <- grep("^BotInf", selector)
  botinf_hdr     <- split_ws(selector[botinf_hdr_idx])
  botinf_val     <- split_ws(selector[botinf_hdr_idx + 1])
  names(botinf_val) <- botinf_hdr

  bot_bc_opts <- data.frame(
    name   = c("contant_pressure_head","contant_flux",
               "variable_pressure_head","variable_flux",
               "free_drainage","deep_drainage",
               "seepage_face","horizontal_drains"),
    BotInf = c("f","f","t","t","f","f","f","f"),
    qGWLF  = c("f","f","f","f","f","t","f","f"),
    FreeD  = c("f","f","f","f","t","f","f","f"),
    SeepF  = c("f","f","f","f","f","f","t","f"),
    DrainF = c("f","f","f","f","f","f","f","t"),
    stringsAsFactors = FALSE
  )
  bot_match <- which(bot_bc_opts$BotInf == botinf_val["BotInf"] &
                     bot_bc_opts$qGWLF  == botinf_val["qGWLF"]  &
                     bot_bc_opts$FreeD  == botinf_val["FreeD"]  &
                     bot_bc_opts$SeepF  == botinf_val["SeepF"]  &
                     bot_bc_opts$DrainF == botinf_val["DrainF"])
  lower_bc  <- if (length(bot_match) > 0) bot_bc_opts$name[bot_match[1]] else "free_drainage"

  ## Soil hydraulic model & hysteresis
  hyst_vals           <- split_ws(selector[grep("Hysteresis", selector) + 1])
  soil_hydraulic_model <- as.integer(hyst_vals[1])
  hysteresis           <- as.integer(hyst_vals[2])

  ## Soil hydraulic parameters (matrix domain)
  thr_hdr_idx <- grep("^ *thr ", selector)[1]
  soil_rows   <- lapply(seq_len(number_materials), function(i) as.numeric(split_ws(selector[thr_hdr_idx + i])))
  soil_mat    <- do.call(rbind, soil_rows)
  shp_df <- data.frame(material = seq_len(number_materials),
                       theta_r  = soil_mat[, 1],
                       theta_s  = soil_mat[, 2],
                       alpha    = soil_mat[, 3],
                       n        = soil_mat[, 4],
                       K_s      = soil_mat[, 5],
                       l        = soil_mat[, 6])

  surface_flow_into_fracture <- 1
  if (soil_hydraulic_model == 8) {
    thrfr_hdr_idx <- grep("^ *thrFr ", selector)[1]
    fr_rows       <- lapply(seq_len(number_materials), function(i) as.numeric(split_ws(selector[thrfr_hdr_idx + i])))
    fr_mat        <- do.call(rbind, fr_rows)
    shp_df$theta_r_fr <- fr_mat[, 1]
    shp_df$theta_s_fr <- fr_mat[, 2]
    shp_df$alpha_fr   <- fr_mat[, 3]
    shp_df$n_fr       <- fr_mat[, 4]
    shp_df$K_s_fr     <- fr_mat[, 5]
    shp_df$l_fr       <- fr_mat[, 6]
    shp_df$w          <- fr_mat[, 7]
    shp_df$beta       <- fr_mat[, 8]
    shp_df$gamma      <- fr_mat[, 9]
    shp_df$a          <- fr_mat[, 10]
    shp_df$K_a        <- fr_mat[, 11]

    qtop_idx <- grep("^ *qTop", selector)
    if (length(qtop_idx) > 0) {
      surface_flow_into_fracture <- as.numeric(split_ws(selector[qtop_idx[1] + 1])[1])
    }
  }

  ## -----------------------------------------------------------------------
  ## BLOCK C
  ## -----------------------------------------------------------------------

  ## dt line
  dt_vals                <- split_ws(selector[grep("^ *dt ", selector)[1] + 1])
  initial_time_step      <- as.numeric(dt_vals[1])
  minimum_time_step      <- as.numeric(dt_vals[2])
  maximum_time_step      <- as.numeric(dt_vals[3])
  lower_time_step_mult   <- as.numeric(dt_vals[4])
  upper_time_step_mult   <- as.numeric(dt_vals[5])
  lower_optim_iter_range <- as.integer(dt_vals[6])
  upper_optim_iter_range <- as.integer(dt_vals[7])

  ## tInit line
  tinit_vals         <- split_ws(selector[grep("^ *tInit", selector)[1] + 1])
  initial_model_time <- as.numeric(tinit_vals[1])
  final_model_time   <- as.numeric(tinit_vals[2])

  ## lPrintD line
  lprintd_vals                        <- split_ws(selector[grep("^ *lPrintD", selector)[1] + 1])
  interval_output_option              <- parse_tf(lprintd_vals[1])
  time_info_print_every_n_time_steps  <- as.integer(lprintd_vals[2])
  interval_output                     <- as.numeric(lprintd_vals[3])
  print_times                         <- parse_tf(lprintd_vals[4])

  ## TPrint values
  tprint_start <- grep("TPrint", selector)[1] + 1
  block_starts <- grep("^\\*\\*\\*", selector)
  tprint_end   <- min(block_starts[block_starts >= tprint_start]) - 1
  tprint_raw   <- unlist(lapply(selector[tprint_start:tprint_end], split_ws))
  tprint_vals  <- as.numeric(tprint_raw[!is.na(suppressWarnings(as.numeric(tprint_raw)))])
  times_to_print <- data.frame(times = tprint_vals)

  ## -----------------------------------------------------------------------
  ## BLOCK D: Root Growth
  ## -----------------------------------------------------------------------

  has_block_d <- length(grep("BLOCK D", selector)) > 0
  rg_depth    <- 0
  rg_factor   <- NA
  rg_params   <- NA

  if (has_block_d) {
    ire_idx  <- grep("iRootDepthEntry", selector)[1]
    rg_depth <- as.integer(split_ws(selector[ire_idx + 1])[1])

    if (rg_depth == 1) {
      ngrowth_idx <- grep("nGrowth", selector)[1]
      n_rg_rows   <- as.integer(split_ws(selector[ngrowth_idx + 1])[1])
      rd_hdr_idx  <- grep("Time  RootDepth", selector)[1]
      time_col  <- numeric(n_rg_rows)
      depth_col <- numeric(n_rg_rows)
      for (i in seq_len(n_rg_rows)) {
        v          <- split_ws(selector[rd_hdr_idx + i])
        time_col[i]  <- as.numeric(v[1])
        depth_col[i] <- as.numeric(v[2])
      }
      rg_params <- data.frame(Time = time_col, RootDepth = depth_col)
      rg_factor <- NA

    } else if (rg_depth == 2) {
      irfak_vals <- split_ws(selector[grep("iRFak", selector)[1] + 1])
      rg_factor  <- as.integer(irfak_vals[1])
      if (rg_factor == 1) {
        rg_params <- data.frame(
          initial_root_growth_time = as.numeric(irfak_vals[2]),
          harvest_time             = as.numeric(irfak_vals[4]),
          initial_rooting_depth    = as.numeric(irfak_vals[5]),
          maximum_rooting_depth    = as.numeric(irfak_vals[7]),
          time_period              = as.numeric(irfak_vals[8])
        )
      } else {
        rg_params <- data.frame(
          initial_root_growth_time = as.numeric(irfak_vals[2]),
          time_root_data           = as.numeric(irfak_vals[3]),
          harvest_time             = as.numeric(irfak_vals[4]),
          initial_rooting_depth    = as.numeric(irfak_vals[5]),
          depth_root_data          = as.numeric(irfak_vals[6]),
          maximum_rooting_depth    = as.numeric(irfak_vals[7]),
          time_period              = as.numeric(irfak_vals[8])
        )
      }
    }
  }

  ## -----------------------------------------------------------------------
  ## BLOCK F: Solute Transport
  ## -----------------------------------------------------------------------

  has_block_f <- length(grep("BLOCK F", selector)) > 0
  st_params   <- solute_transport()   # defaults

  if (has_block_f) {

    ## Epsi line
    epsi_vals <- split_ws(selector[grep("^ *Epsi", selector)[1] + 1])
    Epsi      <- as.numeric(epsi_vals[1])
    lUpW      <- epsi_vals[2]
    lArtD     <- epsi_vals[3]
    lTDep     <- epsi_vals[4]
    cTolA     <- as.numeric(epsi_vals[5])
    cTolR     <- as.numeric(epsi_vals[6])
    MaxItC    <- as.integer(epsi_vals[7])
    PeCr      <- as.numeric(epsi_vals[8])
    n_solutes <- as.integer(epsi_vals[9])
    lTort     <- epsi_vals[10]

    tws_scheme <- switch(as.character(Epsi),
                         "0.5" = "crank_nickolson",
                         "1"   = "implicit_scheme",
                         "0"   = "explicit_scheme",
                         "crank_nickolson")
    sws_scheme <- if (lUpW == "t") "upstream_weighting_fe" else
                  if (lArtD == "t") "gfe_with_artificial_disp" else "galerkin_fe"

    ## iNonEqul line: iNonEqul lWatDep lDualNEq lInitM lInitEq lTort lHP1BC ...
    inonequl_vals <- split_ws(selector[grep("^ *iNonEqul", selector)[1] + 1])
    iNonEqul      <- as.integer(inonequl_vals[1])
    in_total_conc <- parse_tf(inonequl_vals[4])   # lInitM

    solute_model <- switch(as.character(iNonEqul),
                           "0" = "equilibrium_model",
                           "7" = "dual_perm_phys_non_equilibrium",
                           "8" = "dual_permeability",
                           "equilibrium_model")

    ## Bulk.d. — read first material row (scalar params applied to all materials)
    bulkd_hdr_idx <- grep("Bulk\\.d\\.", selector)[1]
    bulkd_vals    <- split_ws(selector[bulkd_hdr_idx + 1])
    material_params_df <- data.frame(
      bulk_density              = as.numeric(bulkd_vals[1]),
      longitudinal_dispersivity = as.numeric(bulkd_vals[2]),
      fraction_adsorption_sites = as.numeric(bulkd_vals[3]),
      immobile_water_content    = as.numeric(bulkd_vals[4])
    )

    ## DifW / DifG — first solute row
    difw_vals <- split_ws(selector[grep("^ *DifW", selector)[1] + 1])
    solute_params_df <- data.frame(
      molecular_diffusion_free_water = as.numeric(difw_vals[1]),
      molecular_diffusion_soil_air   = as.numeric(difw_vals[2])
    )

    ## Kd row — first material row
    kd_vals <- split_ws(selector[grep("^ *Kd", selector)[1] + 1])
    solute_reaction_params_df <- data.frame(
      Kd          = as.numeric(kd_vals[1]),
      Nu          = as.numeric(kd_vals[2]),
      Beta        = as.numeric(kd_vals[3]),
      Henry       = as.numeric(kd_vals[4]),
      SnkL1       = as.numeric(kd_vals[5]),
      SnkS1       = as.numeric(kd_vals[6]),
      SnkG1       = as.numeric(kd_vals[7]),
      SnkL1_prime = as.numeric(kd_vals[8]),
      SnkS1_prime = as.numeric(kd_vals[9]),
      SnkG1_prime = as.numeric(kd_vals[10]),
      SnkL0       = as.numeric(kd_vals[11]),
      SnkS0       = as.numeric(kd_vals[12]),
      SnkG0       = as.numeric(kd_vals[13]),
      Alpha       = as.numeric(kd_vals[14])
    )

    ## kTopSolute / kBotSolute
    ktop_vals   <- split_ws(selector[grep("kTopSolute", selector)[1] + 1])
    kTopSolute  <- as.integer(ktop_vals[1])
    kBotSolute  <- as.integer(ktop_vals[3])

    upper_bc_solute_map <- c("1" = "concentration_bc", "-1" = "solute_flux_bc",
                             "-2" = "stagnant_bc_volotile_solute", "-3" = "isotope_bc")
    lower_bc_solute_map <- c("1" = "concentration_bc", "-1" = "solute_flux_bc",
                             "0" = "zero_conc_gradient")
    upper_bc_solute <- unname(upper_bc_solute_map[as.character(kTopSolute)])
    lower_bc_solute <- unname(lower_bc_solute_map[as.character(kBotSolute)])

    ## tPulse
    pulse_duration <- as.numeric(split_ws(selector[grep("^ *tPulse", selector)[1] + 1])[1])

    st_params <- list(
      solute_transport_model = solute_model,
      number_solutes         = n_solutes,
      equilibrium_adsorption = parse_tf(lwat_val["lEquil"]),
      time_weighting_scheme  = tws_scheme,
      space_weighting_scheme = sws_scheme,
      pulse_duration         = pulse_duration,
      stability_criterion    = PeCr,
      use_tortuosity         = parse_tf(lTort),
      temperature_dependence = parse_tf(lTDep),
      temperature_dependence_params = data.frame(NULL),
      nonlinear_adsorption_iteration_criteria = data.frame(
        absolute_conc_tol   = cTolA,
        relative_conc_tol   = cTolR,
        maximum_n_iteration = MaxItC
      ),
      material_params          = material_params_df,
      solute_params            = solute_params_df,
      solute_reaction_params   = solute_reaction_params_df,
      solute_transport_bcs = data.frame(
        upper_bc                    = upper_bc_solute,
        lower_bc                    = lower_bc_solute,
        in_total_concentrations     = in_total_conc,
        stagnant_boundary_layer     = NA,
        concentration_in_atmosphere = NA,
        fractionation_ratio         = NA,
        stringsAsFactors = FALSE
      )
    )
  }

  ## -----------------------------------------------------------------------
  ## BLOCK G: Root Water Uptake
  ## -----------------------------------------------------------------------

  has_block_g <- length(grep("BLOCK G", selector)) > 0
  rwu_model   <- 0
  rwu_params  <- data.frame(critical_stress_index = 1, P0 = -10, POpt = -25,
                             P2H = -200, P2L = -800, P3 = -8000, r2H = 0.5, r2L = 0.1)
  active_solute_uptake <- FALSE

  if (has_block_g) {
    ## Model line: Model (0-Feddes, 1-S shape) cRootMax OmegaC
    feddes_vals <- split_ws(selector[grep("Feddes", selector)[1] + 1])
    rwu_model   <- as.integer(feddes_vals[1])
    cRootMax    <- as.numeric(feddes_vals[2])

    if (rwu_model == 0) {
      p0_vals   <- split_ws(selector[grep("^ *P0", selector)[1] + 1])
      popt_vals <- split_ws(selector[grep("POptm", selector)[1] + 1])
      rwu_params <- data.frame(
        critical_stress_index = cRootMax,
        P0   = as.numeric(p0_vals[1]),
        POpt = as.numeric(popt_vals[1]),
        P2H  = as.numeric(p0_vals[2]),
        P2L  = as.numeric(p0_vals[3]),
        P3   = as.numeric(p0_vals[4]),
        r2H  = as.numeric(p0_vals[5]),
        r2L  = as.numeric(p0_vals[6])
      )
    } else if (rwu_model == 1) {
      h50_vals <- split_ws(selector[grep("^ *h50", selector)[1] + 1])
      rwu_params <- data.frame(
        critical_stress_index = cRootMax,
        h50 = as.numeric(h50_vals[1]),
        P3  = as.numeric(h50_vals[2])
      )
    } else if (rwu_model == 2) {
      lhyd_vals <- split_ws(selector[grep("lHydRed", selector)[1] + 1])
      rwu_params <- data.frame(
        lHydRed = parse_tf(lhyd_vals[1]),
        P3      = as.numeric(lhyd_vals[2])
      )
    } else if (rwu_model == 3) {
      hxmin_vals <- split_ws(selector[grep("hx_min", selector)[1] + 1])
      rwu_params <- data.frame(
        hx_min = as.numeric(hxmin_vals[1]),
        rKrs   = as.numeric(hxmin_vals[2]),
        rKComp = as.numeric(hxmin_vals[3])
      )
    } else if (rwu_model == 4) {
      rr_vals <- split_ws(selector[grep("RootRad", selector)[1] + 1])
      rwu_params <- data.frame(
        RootRad     = as.numeric(rr_vals[1]),
        XylemRad    = as.numeric(rr_vals[2]),
        Conductance = as.numeric(rr_vals[3]),
        RootCond    = as.numeric(rr_vals[4]),
        a           = as.numeric(rr_vals[5]),
        `H-Wilt`    = as.numeric(rr_vals[6]),
        check.names = FALSE
      )
    }

    active_solute_uptake <- parse_tf(lsnow_val["lActiveU"])
  }

  ## -----------------------------------------------------------------------
  ## PROFILE.DAT: geometry fields not stored in SELECTOR.IN
  ## -----------------------------------------------------------------------

  profile_dat_path <- file.path(project_path, "PROFILE.DAT")
  prof                <- read_profile_dat(project_path)
  number_nodes        <- prof$number_nodes
  profile_depth       <- prof$profile_depth
  observation_nodes_n <- prof$observation_nodes_n
  observation_nodes   <- prof$observation_nodes


  ## -----------------------------------------------------------------------
  ## Assemble hydrus_model
  ## -----------------------------------------------------------------------

  hydrus_model <- list(
    hydrus_project = list(
      project_name   = project_name,
      project_path   = project_path,
      hydrus_version = hydrus_version,
      description    = description
    ),
    main_processes = list(
      water_flow        = parse_tf(lwat_val["lWat"]),
      vapor_flow        = parse_tf(lsnow_val["lVapor"]),
      snow_hydrology    = parse_tf(lsnow_val["lSnow"]),
      particle_tracking = parse_tf(lsnow_val["lPart"]),
      solute_transport  = parse_tf(lwat_val["lChem"]),
      unsatchem         = FALSE,
      HP1               = parse_tf(lsnow_val["lHP1"]),
      heat_transport    = parse_tf(lwat_val["lTemp"]),
      root_water_uptake = parse_tf(lwat_val["lSink"]),
      root_growth       = parse_tf(lwat_val["lRoot"]),
      inverse           = parse_tf(lwat_val["lInverse"])
    ),
    model_units = list(
      time_unit  = time_unit,
      space_unit = space_unit,
      mass_unit  = mass_unit
    ),
    geometry = list(
      number_materials    = number_materials,
      number_subregions   = number_subregions,
      number_nodes        = number_nodes,
      profile_depth       = profile_depth,
      observation_nodes_n = observation_nodes_n,
      observation_nodes   = observation_nodes
    ),
    print_options = list(
      print_times                        = print_times,
      screen_output                      = parse_tf(lwat_val["lScreen"]),
      print_fluxes_not_temp              = parse_tf(lsnow_val["lFluxes"]),
      times_to_print                     = times_to_print,
      interval_output_option             = interval_output_option,
      interval_output                    = interval_output,
      print_time_information             = TRUE,
      time_info_print_every_n_time_steps = time_info_print_every_n_time_steps
    ),
    time_parameters = list(
      initial_time_step      = initial_time_step,
      minimum_time_step      = minimum_time_step,
      maximum_time_step      = maximum_time_step,
      lower_time_step_mult   = lower_time_step_mult,
      upper_time_step_mult   = upper_time_step_mult,
      lower_optim_iter_range = lower_optim_iter_range,
      upper_optim_iter_range = upper_optim_iter_range,
      initial_model_time     = initial_model_time,
      final_model_time       = final_model_time
    ),
    time_variable_bc = list(
      time_variable_bc             = parse_tf(lwat_val["lVariabBC"]),
      meteorological_data          = parse_tf(lsnow_val["lMeteo"]),
      repeat_bc_records            = FALSE,
      repeat_bc_records_n          = 1,
      daily_var_transpiration      = FALSE,
      sinusoidal_var_precipitation = FALSE
    ),
    iteration_criteria = list(
      maximum_iterations            = maximum_iterations,
      water_content_tol             = water_content_tol,
      pressure_head_tol             = pressure_head_tol,
      internal_interpolation_tables = has_htab,
      lower_limit_tension_interval  = lower_limit_tension_interval,
      upper_limit_tension_interval  = upper_limit_tension_interval
    ),
    soil_hydraulics = list(
      soil_hydraulic_model       = soil_hydraulic_model,
      mobile_immobile            = FALSE,
      hysteresis                 = hysteresis,
      initially_drying_curve     = TRUE,
      soil_hydraulic_parameters  = shp_df,
      surface_flow_into_fracture = surface_flow_into_fracture
    ),
    water_flow_bcs = list(
      upper_bc             = upper_bc,
      initial_condition    = InitCond,
      lower_bc             = lower_bc,
      triggered_irrigation = parse_tf(lsnow_val["lIrrig"]),
      constant_boundary_fluxes = data.frame(upper_bc_flux = 0,
                                            lower_bc_flux = 0,
                                            root_water_uptake_flux = 0),
      irrigation_params = data.frame(trigger_node = 1,
                                     trigger_pressure_head = -100,
                                     irrigation_rate = 0,
                                     irrigation_duration = 0,
                                     lag_time = 0)
    ),
    root_water_uptake = list(
      root_water_uptake_model  = rwu_model,
      root_water_uptake_params = rwu_params,
      active_solute_uptake     = active_solute_uptake
    ),
    root_growth = list(
      root_growth_depth  = rg_depth,
      root_growth_params = rg_params,
      root_growth_factor = rg_factor
    ),
    solute_transport  = st_params,
    particle_tracking = list(
      init_water_storage      = 2,
      cumulative_surface_flux = 2
    )
  )

  class(hydrus_model) <- c("hydrus_model", class(hydrus_model))
  return(hydrus_model)
}
