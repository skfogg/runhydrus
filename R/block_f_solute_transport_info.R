#' Set Block F of SELECTOR.IN: Solute Transport Information
#'
#' @param hydrus_model a hydrus model created with \code{\link{create_hydrus_project}}
#'
#' @returns edits SELECTOR.IN file
#' @noRd
#'
#' @examples block_f_solute_transport_info(hydrus_model)
#'
#' @importFrom stringr str_flatten str_split
block_f_solute_transport_info <- function(hydrus_model){

  selector_template <- readLines(file.path(hydrus_model$hydrus_project$project_path, "SELECTOR.IN"))

  if(length(selector_template[grep("BLOCK F", selector_template)]) == 0){
    ## Add in BLOCK F:
    selector_template <- c(selector_template[1:(grep("END OF INPUT", selector_template)-1)],
                           readLines(base::system.file("templates", "BLOCK_F_SOLUTE_TRANSPORT", package = "runhydrus"),
                                     n = -1L,
                                     encoding = "unknown")
                           )
  }
  ## Look-up tables:
  tws_options <- data.frame(option = c("crank-nickolson", "implicit_scheme", "explicit_scheme"),
                            Epsi = c(0.5, 1, 0))
  sws_options <- data.frame(option = c("galerkin_fe", "upstream_weighting_fe", "gfe_with_artificial_disp"),
                            lUpW = c("f", "t", "f"),
                            lArtD = c("f", "f", "t"))
  # ADD IN OTHER MODELS LATER
  model_options <- data.frame(option = c("equilibrium_model", "dual_perm_phys_non_equilibrium", "dual_permeability"),
                              iNonEqul = c("0", "7", "8"))
  upper_bc_options <- data.frame(option = c("concentration_bc", "solute_flux_bc", "stagnant_bc_volotile_solute", "isotope_bc"),
                                 kTopSolute = c(1,-1,-2,-3))
  lower_bc_options <- data.frame(option = c("concentration_bc", "solute_flux_bc", "zero_conc_gradient"),
                                 kBotSolute = c(1,-1, 0))

  #   Epsi  lUpW  lArtD lTDep    cTolA    cTolR   MaxItC    PeCr  No.Solutes  lTort   iBacter   lFiltr  nChPar
  # 0.5     f     f     f         0         0     1        2        1       t       0        f       16
  selector_template[grep("Epsi", selector_template) + 1] <- stringr::str_flatten(c(rep(" ", times = 2),
                                                                          paste0(
                                                                            c(tws_options[option == hydrus_model$solute_transport$time_weighting_scheme, "Epsi"],
                                                                              sws_options[option == hydrus_model$solute_transport$space_weighting_scheme, "lUpW"],
                                                                              sws_options[option == hydrus_model$solute_transport$space_weighting_scheme, "lArtD"],
                                                                              ifelse(hydrus_model$solute_transport$temperature_dependence, "t", "f"),
                                                                              hydrus_model$solute_transport$special_iteration_criteria$absolute_conc_tol,
                                                                              hydrus_model$solute_transport$special_iteration_criteria$relative_conc_tol,
                                                                              hydrus_model$solute_transport$special_iteration_criteria$maximum_n_iteration,
                                                                              hydrus_model$solute_transport$stability_criterion,
                                                                              hydrus_model$solute_transport$number_solutes,
                                                                              ifelse(hydrus_model$solute_transport$use_tortuosity, "t", "f"),
                                                                              "0",
                                                                              "f",
                                                                              "16"),
                                                                            collapse = "     "
                                                                          )))

  # iNonEqul lWatDep   lDualNEq   lInitM   lInitEq    lTort    lHP1BC   lFumigant    lPFAS   lSurfact   lCFTr
  # 0         f         f         f         f         f         t         f         f         f         f
  line_4 <- stringr::str_split(selector_template[grep("iNonEqul", selector_template) + 1], "", simplify = T)
  line_4[,4] <- model_options[option == hydrus_model$solute_transport$solute_transport_model, "iNonEqul"]
  line_4[,34] <- ifelse(hydrus_model$solute_transport$solute_tranport_bcs$in_total_concentrations, "t", "f")
  selector_template[grep("iNonEqul", selector_template) + 1] <- line_4

  # Bulk.d.     DisperL.      Frac      Mobile WC (1..NMat)
  # 1.5        17.5           1           0
  # 1.5        17.5           1           0
  # 1.5        17.5           1           0
  hold_rest_of_block_f <- selector_template[(grep("DifW", selector_template)):length(selector_template)]
  for(i in 1:hydrus_model$geometry$number_materials){
    selector_template[grep("Bulk.d.", selector_template) + i] <- stringr::str_flatten(rep(" ", times = 8),
                                                                                      paste0(c(hydrus_model$solute_transport$material_params$bulk_density,
                                                                                               hydrus_model$solute_transport$material_params$longitudinal_dispersivity,
                                                                                               hydrus_model$solute_transport$material_params$fraction_of_adsorption_sites,
                                                                                               hydrus_model$solute_transport$material_params$immobile_water_content),
                                                                                             collapse = "        "))
  }
  selector_template <- c(selector_template[1:(grep("Bulk.d.", selector_template) + hydrus_model$geometry$number_materials)],
                         hold_rest_of_block_f)

  # DifW       DifG                n-th solute
  # 0           0
  hold_rest_of_block_f <- selector_template[(grep("Kd", selector_template)):length(selector_template)]
  for(i in 1:hydrus_model$geometry$number_solutes){
    selector_template[grep("DifW", selector_template) + i] <- stringr::str_flatten(rep(" ", times = 9),
                                                                                      paste0(c(hydrus_model$solute_transport$solute_params$molecular_diffusion_free_water,
                                                                                               hydrus_model$solute_transport$solute_params$molecular_diffusion_soil_air),
                                                                                             collapse = "        "))
  }
  selector_template <- c(selector_template[1:(grep("DifW", selector_template) + hydrus_model$geometry$number_materials)],
                         hold_rest_of_block_f)

  # Kd          Nu        Beta       Henry       SnkL1       SnkS1       SnkG1       SnkL1'      SnkS1'      SnkG1'      SnkL0       SnkS0       SnkG0        Alfa
  #           0           0           1           0           0           0           0           0           0           0           0           0           0           0
  hold_rest_of_block_f <- selector_template[grep("kTopSolute", selector_template):length(selector_template)]
  for(i in 1:hydrus_model$geometry$number_materials){
    selector_template[grep("Kd", selector_template) + i] <- stringr::str_flatten(rep(" ", times = 10),
                                                                                      paste0(c(hydrus_model$solute_transport$solute_reaction_params$Kd,
                                                                                               hydrus_model$solute_transport$solute_reaction_params$Nu,
                                                                                               hydrus_model$solute_transport$solute_reaction_params$Beta,
                                                                                               hydrus_model$solute_transport$solute_reaction_params$Henry,
                                                                                               hydrus_model$solute_transport$solute_reaction_params$SnkL1,
                                                                                               hydrus_model$solute_transport$solute_reaction_params$SnkS1,
                                                                                               hydrus_model$solute_transport$solute_reaction_params$SnkG1,
                                                                                               hydrus_model$solute_transport$solute_reaction_params$SnkL1_prime,
                                                                                               hydrus_model$solute_transport$solute_reaction_params$SnkS1_prime,
                                                                                               hydrus_model$solute_transport$solute_reaction_params$SnkG1_prime,
                                                                                               hydrus_model$solute_transport$solute_reaction_params$SnkL0,
                                                                                               hydrus_model$solute_transport$solute_reaction_params$SnkS0,
                                                                                               hydrus_model$solute_transport$solute_reaction_params$SnkG0,
                                                                                               hydrus_model$solute_transport$solute_reaction_params$Alpha),
                                                                                             collapse = "        "))
  }
  selector_template <- c(selector_template[1:(grep("Kd", selector_template) + hydrus_model$geometry$number_materials)],
                         hold_rest_of_block_f)

  #       kTopSolute  SolTop    kBotSolute  SolBot
  #          -1           0           0           0
  selector_template[grep("kTopSolute", selector_template) + 1] <- stringr::str_flatten(c(rep(" ", time = 9),
                                                                                       upper_bc_options[option == hydrus_model$solute_transport$solute_transport_bcs$upper_bc, "kTopSolute"],
                                                                                       "0",
                                                                                       lower_bc_options[option == hydrus_model$solute_transport$solute_transport_bcs$lower_bc, "kBotSolute"],
                                                                                       "0"))
  if(hydrus_model$solute_transport$solute_transport_bcs$upper_bc == "stagnant_bc_volotile_solute"){
    selector_template <- c(selector_template[1:(grep("kTopSolute", selector_template) + 1)],
                           "      dSurf          cAtm",
                           stringr::str_flatten(rep(" ", times = 10),
                                                hydrus_model$solute_transport$solute_transport_bcs$stagnant_boundary_layer,
                                                rep(" ", times = 11),
                                                hydrus_model$solute_transport$solute_transport_bcs$concentration_in_atmosphere),
                           selector_template[grep("tPulse", selector_template):length(selector_template)])
  }
  if(hydrus_model$solute_transport$solute_transport_bcs$upper_bc == "isotope_bc"){
    selector_template <- c(selector_template[1:(grep("kTopSolute", selector_template) + 1)],
                           "  FractRatio",
                           c(rep(" ", times = 10), hydrus_model$solute_transport$solute_transport_bcs$fractionation_ratio),
                           selector_template[grep("tPulse", selector_template):length(selector_template)])
  }


  selector_template[grep("tPulse", selector_template) + 1] <- stringr::str_flatten(c(rep(" ", time = 8),
                                                                                     hydrus_model$solute_transport$pulse_duration))

  selector_template <- c(selector_template,
                         "*** END OF INPUT FILE 'SELECTOR.IN' ************************************")

  ## Update Selector.in
  writeLines(selector_template, file.path(hydrus_model$hydrus_project$project_path, "SELECTOR.IN"))
  cat("Updated BLOCK F: SOLUTE TRANSPORT INFORMATION of SELECTOR.IN file... \n")

}




# time_weighting_scheme
# # options =
# # "crank-nickolson", Epsi = 0.5
# # "implicit_scheme", Epsi = 1
# # grey-ed out: "explicit-scheme"
#
# space_weighting_scheme
# # options =
# # "galerkin_fe", lUpW = "f"
# # "upstream_weighting_fe", lUpW = "t"
# # "gfe_with_artificial_disp", lUpW = "f", lArtD = "t"
#
# solute_information
# # "number_solutes"
# # "mass_units"
# # tPulse = "pulse_duration"
# # PeCr = 1, "stability_criterion"
#
# # check-box-options:
# temperature_dependence
# ## temperature dependence of solute transport and reaction params, lTDep = "t"
# #### ADDS above kTopSolute:
# # Temperature Dependence
# # DifW       DifG                n-th solute
# # 0           0
# # Kd          Nu        Beta       Henry       SnkL1       SnkS1       SnkG1       SnkL1'      SnkS1'      SnkG1'      SnkG1'      SnkL0       SnkS0       SnkG0        Alfa
# # 0           0           0           0           0           0           0           0           0           0           0           0           0           0
#
# ## use tortuosity (checked default) = lTort = "t" on top row (lTort on bottom row "f" either option)
#
# solute_transport_model
# # options: "equilibrium_model" (standard solute transport), iNonEqul = 0
# # "dual_perm_phys_non_equilibrium", iNonEqul = 7
# # "dual_permeability" (model with either immoblie water in
# # the matrix or kinetic sorption (physical and chemical nonequilibrium)), iNonEqul = 8
#
# special_iteration_criteria #(needed only for nonlinear adsorption or blocking functions)
# # cTolA = absolute_concentration_tolerance
# # cTolR = relative_concentration_tolerance (num between 0 and 1)
# # MaxItC = maximum_n_iteration
#
# material_params
# ## FOR EQUILIbrium model (value per material):
# # Bulk.d. = "bulk_density"
# # DisperL. = "longitudinal_dispersivity"
# # Frac = "fraction_of_adsorption_sites"
# # Mobile WC (1..NMat) = "immobile_water_content"
#
# ## value per solute: (specific for equilibirium model)
# # DifW - molecular diffusion coef in free water
# # DifG - molecular diffusion coef in soil air
#
# solute_reaction_params
# # Kd = adsorptions isotherm coefficient
# # Nu = adsorption isotherm coefficient, the Langmuir coeff
# # Beta = adsorption isotherm coeficient, Freundlich coeff
# # Henry = equilibrium distribution constant between liquid and gaseous phases, k_g
# # SnkL1 = first order rate constant for dissolved phase, mu_w
# # SnkS1 = first order rate constant for solid phase, mu_s
# # SnkG1 = first order rate constant for gas phase, mu_g
# # SnkL1' = first order rate constnat for dissolved phase represneting the chain reaction
# # SnkS1' = first order rate constant for solid phase representing the chain reaction
# # SnkG1' = first order rate constant for gas phase representing the chain reaction
# # SnkL0 = zero order rate constant for the dissolved phase, gamma_s
# # SnkS0 = zero order rate constant for the solid phase
# # SnkG0 = zero order rate constant for the gas phase
# # Alfa =  first order rate coefficient for one site or two site nonequilibrium adsorption, mass transfer coeff for solute exhcange between mobile and immobile liquid regions
#
# solute_transport_bcs
# # upper_bc
# ## options:
# ## kTopSolute = 1, concentration (Dirichlet) BC
# ## kTopSolute = -1, solute flux (Couchy) BC
# ## kTopSolute = -2, stagnant BC for volotile solutes; add row below: dSurf (stagnant boundary layer) cAtm (concentration in the atmosphere)
# ## kTopSolute = -3, isotope bc; add row below: FractRatio (fractionation ratio)
#
# # lower_bc
# # kBotSolute = 1, concentration (Dirichlet) BC
# # kBotSolute = -1, solute flux (Couchy) BC
# # kBotSolute = 0, zero-concentration gradient
#
# ## initial conditions
# ## lInitM = "f", in liquid phase concentrations (mass solute/ volume water)
# ## lInitM = "t", in total concentrations (mass solute/volume soil)
#
# ### STANDARD SOLUTE TRANSPORT:
#
# # *** BLOCK F: SOLUTE TRANSPORT INFORMATION *****************************************************
# #   Epsi  lUpW  lArtD lTDep    cTolA    cTolR   MaxItC    PeCr  No.Solutes  lTort   iBacter   lFiltr  nChPar
# # 0.5     f     f     f         0         0     1        2        1       t       0        f       16
#
# # iNonEqul lWatDep   lDualNEq   lInitM   lInitEq    lTort    lHP1BC   lFumigant    lPFAS   lSurfact   lCFTr
# # 0         f         f         f         f         f         t         f         f         f         f
#
# # Bulk.d.     DisperL.      Frac      Mobile WC (1..NMat)
# # 1.5        17.5           1           0
# # 1.5        17.5           1           0
# # 1.5        17.5           1           0
#
# # DifW       DifG                n-th solute
# # 0           0
#
# # Kd          Nu        Beta       Henry       SnkL1       SnkS1       SnkG1       SnkL1'      SnkS1'      SnkG1'      SnkL0       SnkS0       SnkG0        Alfa
# #           0           0           1           0           0           0           0           0           0           0           0           0           0           0
# #           0           0           1           0           0           0           0           0           0           0           0           0           0           0
# #           0           0           1           0           0           0           0           0           0           0           0           0           0           0
#
# #       kTopSolute  SolTop    kBotSolute  SolBot
# #          -1           0           0           0
#
# #       tPulse
# #         861
#
#
# ###############
# ## Part of Block g:
# # default is 'Passive Root Solute Uptake'
# # Maximum allowed concentration for passive root solute uptake
#  ## = cRootMax
# # The partitioning between passive and active uptake is controlled by the a priori defined concentration value cRootMax
#
# # Active solute uptake model
#
