#' Set Block D of SELECTOR.IN: Root Growth Information
#'
#' @param hydrus_model a hydrus model created with \code{\link{create_hydrus_project}}
#'
#' @returns edits BLOCK D of SELECTOR.IN
#' @noRd
#'
#' @examples block_d_root_growth_info(hydrus_model)
block_d_root_growth_info <- function(hydrus_model
                                     # root_growth = root_growth

){

  selector_template <- readLines(file.path(hydrus_model$hydrus_project$project_path, "SELECTOR.IN"))


  if(length(selector_template[grep("BLOCK D", selector_template)]) == 0){
    ## Add in BLOCK D correctly formatted based on root_growth_depth model selected:
    selector_template <- c(selector_template[1:(grep("END OF INPUT", selector_template)-1)],
                           readLines(base::system.file("R",
                                                       "inst",
                                                       "templates",
                                                       paste0("BLOCK_D_ROOT_GROWTH_",
                                                              hydrus_model$root_growth$root_growth_depth),
                                                       package = "runhydrus"),
                                     n = -1L,
                                     encoding = "unknown")
                           )
  }

  ## Error checking:
  if(is.na(hydrus_model$root_growth$root_growth_factor)|!hydrus_model$root_growth$root_growth_factor %in% c(0,1)){
    stop("Error in root_growth_factor. Value must be NA, 0, or 1.")
  }

  ## Specify the root growth depth model:
  selector_template[grep("iRootDepthEntry", selector_template) + 1] <- stringr::str_flatten(c(rep(" ", times = 8),
                                                                                   as.character(hydrus_model$root_growth$root_growth_depth)))

  ### Root Depth specified with time variable boundary conditions:
  # iRootDepthEntry
  # 0


  if(hydrus_model$root_growth$root_growth_depth == 1){
    ### Root Depth specified using a table
    # iRootDepthEntry
    # 1
    # nGrowth
    # 2
    # Time  RootDepth
    # 50         30
    # 100         30

    if(!colnames(hydrus_model$root_growth$root_growth_params) %in% c("Time", "RootDepth")){
      stop("Error in root_growth$root_growth_params. Root depth table must have column names 'Time' and 'RootDepth'.")
    }

    selector_template[grep("nGrowth", selector_template) + 1] <- stringr::str_flatten(c(rep(" ", times = 8),
                                                                               nrow(hydrus_model$root_growth$root_growth_params)))

    for(i in 1:nrow(hydrus_model$root_growth$root_growth_params)){
      selector_template[grep("Time  RootDepth", selector_template) + i] <- stringr::str_flatten(c(rep(" ", times = 8),
                                                                                         hydrus_model$root_growth$root_growth_params[i, "Time"],
                                                                                         rep(" ", times = 8),
                                                                                         hydrus_model$root_growth$root_growth_params[i, "RootDepth"]))
    }
  }

  if(hydrus_model$root_growth$root_growth_depth == 2){
    ### Root depth specified using a logistic growth function & root growth factor 50/50
    # iRootDepthEntry
    # 2
    # iRFak     tRMin     tRMed     tRMax     xRMin     xRMed     xRMax   tPeriod
    # 1        115         0       245      0.01         0        60      365

    # iRFak is 1 when '50% after 50% growing season' option and is 0 when root growth factor is from given data


    if(hydrus_model$root_growth$root_growth_factor == 0){
      selector_template[grep("iRFak", selector_template) + 1] <- stringr::str_flatten(c(rep(" ", times = 8),
                                                                               hydrus_model$root_growth$root_growth_factor))

      if(any(!colnames(hydrus_model$root_growth$root_growth_params) %in% c("initial_root_growth_time", "time_root_data", "harvest_time", "initial_rooting_depth", "depth_root_data", "maximum_rooting_depth", "time_period"))|is.null(hydrus_model$root_growth$root_growth_params)){
        stop("Error in root_growth$root_growth_params. Root depth data.frame must have column names
             'initial_root_growth_time', 'time_root_data', 'harvest_time', 'initial_rooting_depth',
             'depth_root_data', 'maximum_rooting_depth', 'time_period'.")
      }

      selector_template[grep("iRFak", selector_template) + 1] <- stringr::str_flatten(c(rep(" ", times = 17),
                                                                               hydrus_model$root_growth$root_growth_params$initial_root_growth_time,
                                                                               rep(" ", times = 8),
                                                                               hydrus_model$root_growth$root_growth_params$time_root_data,
                                                                               rep(" ", times = 8),
                                                                               hydrus_model$root_growth$root_growth_params$harvest_time,
                                                                               rep(" ", times = 8),
                                                                               hydrus_model$root_growth$root_growth_params$initial_rooting_depth,
                                                                               rep(" ", times = 8),
                                                                               hydrus_model$root_growth$root_growth_params$depth_root_data,
                                                                               rep(" ", times = 8),
                                                                               hydrus_model$root_growth$root_growth_params$maximum_rooting_depth,
                                                                               rep(" ", times = 8),
                                                                               hydrus_model$root_growth$root_growth_params$time_period))

    }

    if(hydrus_model$root_growth$root_growth_factor == 1){
      # selector_template[grep("iRFak", selector_template) + 1] <- stringr::str_flatten(c(rep(" ", times = 8),
      #                                                                          root_growth$root_growth_factor))

      if(any(!colnames(hydrus_model$root_growth$root_growth_params) %in% c("initial_root_growth_time", "harvest_time", "initial_rooting_depth", "maximum_rooting_depth", "time_period"))|is.null(hydrus_model$root_growth$root_growth_params)){
        stop("Error in root_growth$root_growth_params. Root depth data.frame must have column names 'initial_root_growth_time', 'harvest_time', 'initial_rooting_depth', 'maximum_rooting_depth', 'time_period'.")
      }

      selector_template[grep("iRFak", selector_template) + 1] <- stringr::str_flatten(c(rep(" ", times = 8),
                                                                               hydrus_model$root_growth$root_growth_factor,
                                                                               rep(" ", times = 8),
                                                                               hydrus_model$root_growth$root_growth_params$initial_root_growth_time,
                                                                               rep(" ", times = 8),
                                                                               "0",
                                                                               rep(" ", times = 8),
                                                                               hydrus_model$root_growth$root_growth_params$harvest_time,
                                                                               rep(" ", times = 8),
                                                                               hydrus_model$root_growth$root_growth_params$initial_rooting_depth,
                                                                               rep(" ", times = 8),
                                                                               "0",
                                                                               rep(" ", times = 8),
                                                                               hydrus_model$root_growth$root_growth_params$maximum_rooting_depth,
                                                                               rep(" ", times = 8),
                                                                               hydrus_model$root_growth$root_growth_params$time_period))

    }
  }

  ## Update SELECTOR.IN
  writeLines(selector_template, file.path(hydrus_model$hydrus_project$project_path, "SELECTOR.IN"))
  cat("Updated BLOCK D: ROOT GROWTH of SELECTOR.IN file... \n")

}

#root_growth_depth how root growth should be specified. See details for options. Default '0', root depth specified using time-variable boundary conditions.
# @param root_growth_params data.frame of parameters for the root growth specification. Default NULL because no additional parameters when root_growth_depth = 0.
# @param root_growth_factor either '0' for root growth factor from given data or '1' for a root growth factor of 50% after 50% growing season.
#
