#' Set Block G of SELECTOR.IN: Root Water Uptake Information
#'
#' @param hydrus_model a hydrus model created with \code{\link{create_hydrus_project}}
#'
#' @returns edits BLOCK G of SELECTOR.IN
#' @export
#'
#' @examples block_g_root_water_uptake_info(hydrus_model, root_water_uptake)
block_g_root_water_uptake_info <- function(hydrus_model
                                           # root_water_uptake = root_water_uptake
){

  selector_template <- readLines(file.path(hydrus_model$hydrus_project$project_path, "SELECTOR.IN"))

  if(length(selector_template[grep("BLOCK G", selector_template)]) == 0){
  ## Add in BLOCK G correctly formatted based on root_water_uptake_model selected:
  selector_template <- c(selector_template[1:(grep("END OF INPUT", selector_template)-1)],
                         readLines(file.path(paste0("./templates/BLOCK_G_ROOT_WATER_UPTAKE_",
                                                    hydrus_model$root_water_uptake$root_water_uptake_model))))
  }

  # *** BLOCK G: ROOT WATER UPTAKE INFORMATION ***
  #  Model  (0 - Feddes, 1 - S shape)  cRootMax    OmegaC
  # 0                                   1
  selector_template[grep("Feddes", selector_template) + 1] <- str_flatten(c(rep(" ", times = 8),
                                                                            hydrus_model$root_water_uptake$root_water_uptake_model,
                                                                            rep(" ", times = 36),
                                                                            ifelse(hydrus_model$root_water_uptake$root_water_uptake_model %in% c(0,1),
                                                                                   hydrus_model$root_water_uptake$root_water_uptake_params$critical_stress_index,
                                                                                   " "),
                                                                            rep(" ", times = 11),
                                                                            " "))# OmegaC


  ## cRootMax = CRITICAL STESS INDEX (JARVIS) option. Must be between 0.01 and 1. Default 1. Only available for Feddes and S-Shaped
  ## controls partitioning between passive and active root uptake, concentration value.

  if(hydrus_model$root_water_uptake$root_water_uptake_model == 0){
    ### MODEL = 0 FEDDES:
    # P0       P2H       P2L       P3          r2H        r2L
    # -10      -200      -800     -8000         0.5         0.1
    # POptm(1),POptm(2),...,POptm(NMat)
    # -25
    if(any(!colnames(hydrus_model$root_water_uptake$root_water_uptake_params) %in% c("critical_stress_index","P0","POpt","P2H","P2L","P3","r2H","r2L"))){
      stop("Error in root_water_uptake$root_water_uptake_params. For the Feddes model (0) column names must be 'critical_stress_index','P0','POpt','P2H','P2L','P3','r2H','r2L'.")
    }
    selector_template[grep("P0", selector_template) + 1] <- str_flatten(c(rep(" ", times = 6),
                                                                          hydrus_model$root_water_uptake$root_water_uptake_params$P0,
                                                                          rep(" ", times = 6),
                                                                          hydrus_model$root_water_uptake$root_water_uptake_params$P2H,
                                                                          rep(" ", times = 6),
                                                                          hydrus_model$root_water_uptake$root_water_uptake_params$P2L,
                                                                          rep(" ", times = 6),
                                                                          hydrus_model$root_water_uptake$root_water_uptake_params$P3,
                                                                          rep(" ", times = 6),
                                                                          hydrus_model$root_water_uptake$root_water_uptake_params$r2H,
                                                                          rep(" ", times = 6),
                                                                          hydrus_model$root_water_uptake$root_water_uptake_params$r2L))

    ## TO DO: ADD IN OPTION OF SETTING MORE THAN ONE VALUE HERE:
    selector_template[grep("POptm", selector_template) + 1] <- str_flatten(c(rep(" ", times = 6),
                                                                           hydrus_model$root_water_uptake$root_water_uptake_params$POpt))
  }

  if(hydrus_model$root_water_uptake$root_water_uptake_model == 1){
    ### MODEL = 1 S-Shaped:
    # h50       P3
    # -800         3
    if(any(!colnames(hydrus_model$root_water_uptake$root_water_uptake_params) %in% c("critical_stress_index","h50","P3"))){
      stop("Error in root_water_uptake$root_water_uptake_params. For the S-Shaped model (1) column names must be 'critical_stress_index','h50','P3'.")
    }
    selector_template[grep("h50", selector_template) + 1] <- str_flatten(c(rep(" ", times = 6),
                                                                           hydrus_model$root_water_uptake$root_water_uptake_params$h50,
                                                                           rep(" ", times = 6),
                                                                           hydrus_model$root_water_uptake$root_water_uptake_params$P3))
  }

  if(hydrus_model$root_water_uptake$root_water_uptake_model == 2){
    ### MODEL = 2 Nimah & Hanks
    # lHydRed       P3
    # f     -8000
    if(any(!colnames(hydrus_model$root_water_uptake$root_water_uptake_params) %in% c("lHydRed","P3"))){
      stop("Error in root_water_uptake$root_water_uptake_params. For the Nimah & Hanks model (2) column names must be 'lHydRed','P3'.")
    }
    if(!is.logical(hydrus_model$root_water_uptake$root_water_uptake_params$lHydRed)){
      stop("Error in root_water_uptake$root_water_uptake_params. Parameter lHydRed of the Nimah & Hanks model must be either TRUE to allow hydraulic redistribution or FALSE to prevent hydraulic redistribution. ")
    }
    selector_template[grep("lHydRed", selector_template) + 1] <- str_flatten(c(rep(" ", times = 6),
                                                                               ifelse(hydrus_model$root_water_uptake$root_water_uptake_params$lHydRed, 't', 'f'),
                                                                               rep(" ", times = 6),
                                                                               hydrus_model$root_water_uptake$root_water_uptake_params$P3))
  }

  if(hydrus_model$root_water_uptake$root_water_uptake_model == 3){
    ### MODEL = 3 Couvreur
    # hx_min       rKrs    rKComp
    # -15000     0.001     0.001
    if(any(!colnames(hydrus_model$root_water_uptake$root_water_uptake_params) %in% c("hx_min","rKrs","rKComp"))){
      stop("Error in root_water_uptake$root_water_uptake_params. For the Couvreur model (3) column names must be 'hx_min','rKrs','rKComp'.")
    }
    selector_template[grep("hx_min", selector_template) + 1] <- str_flatten(c(rep(" ", times = 6),
                                                                              hydrus_model$root_water_uptake$root_water_uptake_params$hx_min,
                                                                              rep(" ", times = 6),
                                                                              hydrus_model$root_water_uptake$root_water_uptake_params$rKrs,
                                                                              rep(" ", times = 6),
                                                                              hydrus_model$root_water_uptake$root_water_uptake_params$rKComp))
  }

  if(hydrus_model$root_water_uptake$root_water_uptake_model == 4){
    ###  = 4 de Jong van Lier
    # RootRad   XylemRad   Conductance RootCond   a      H-Wilt
    # 0.05      0.02    0.0001   3.5e-06       0.5     -8000
    if(any(!colnames(hydrus_model$root_water_uptake$root_water_uptake_params) %in% c("RootRad","XylemRad","Conductance", "RootCond", "a", "H-Wilt"))){
      stop("Error in root_water_uptake$root_water_uptake_params. For the de Jong van Lier  (4) column names must be 'RootRad','XylemRad','Conductance', 'RootCond', 'a', 'H-Wilt'.")
    }
    selector_template[grep("RootRad", selector_template) + 1] <- str_flatten(c(rep(" ", times = 6),
                                                                               hydrus_model$root_water_uptake$root_water_uptake_params$RootRad,
                                                                               rep(" ", times = 6),
                                                                               hydrus_model$root_water_uptake$root_water_uptake_params$XylemRad,
                                                                               rep(" ", times = 6),
                                                                               hydrus_model$root_water_uptake$root_water_uptake_params$Conductance,
                                                                               rep(" ", times = 6),
                                                                               hydrus_model$root_water_uptake$root_water_uptake_params$RootCond,
                                                                               rep(" ", times = 6),
                                                                               hydrus_model$root_water_uptake$root_water_uptake_params$a,
                                                                               rep(" ", times = 6),
                                                                               hydrus_model$root_water_uptake$root_water_uptake_params$H-Wilt))
  }

  ## Update SELECTOR.IN
  writeLines(selector_template, file.path(hydrus_model$hydrus_project$project_path, "SELECTOR.IN"))
  cat("Updated BLOCK G: ROOT WATER UPTAKE INFORMATION of SELECTOR.IN file... \n")

}
