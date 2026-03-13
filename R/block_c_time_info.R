#' Set Time Parameters in BLOCK C of SELECTOR.IN
#'
#' @param hydrus_model a hydrus model created with \code{\link{create_hydrus_project}}
#'
#' @returns edits BLOCK C of SELECTOR.IN
#' @export
#'
#' @importFrom stringr str_flatten
#'
#' @examples block_c_time_info(hydrus_model)
block_c_time_info <- function(hydrus_model                              ){


  selector_template <- readLines(file.path(hydrus_model$hydrus_project$project_path, "SELECTOR.IN"))

  #### *** BLOCK C: TIME INFORMATION *** ####
  # dt  dtMin  dtMax  DMul  DMul2  ItMin   ItMax  MPL
  selector_template[grep("dt", selector_template) + 1] <- stringr::str_flatten(c(rep(" ", times = 6),
                                                                        hydrus_model$time_parameters$initial_time_step,
                                                                        rep(" ", times = 7),
                                                                        hydrus_model$time_parameters$minimum_time_step,
                                                                        rep(" ", times = 11),
                                                                        hydrus_model$time_parameters$maximum_time_step,
                                                                        rep(" ", times = 5),
                                                                        hydrus_model$time_parameters$lower_time_step_mult,
                                                                        rep(" ", times = 5),
                                                                        hydrus_model$time_parameters$upper_time_step_mult,
                                                                        rep(" ", times = 5),
                                                                        hydrus_model$time_parameters$lower_optim_iter_range,
                                                                        rep(" ", times = 5),
                                                                        hydrus_model$time_parameters$upper_optim_iter_range,
                                                                        rep(" ", times = 5),
                                                                        nrow(hydrus_model$print_options$times_to_print)))
  # tInit  tMax
  selector_template[grep("tInit", selector_template) + 1] <- stringr::str_flatten(c(rep(" ", times = 10),
                                                                           hydrus_model$time_parameters$initial_model_time,
                                                                           rep(" ", times = 9),
                                                                           hydrus_model$time_parameters$final_model_time))

  # lPrintD  nPrintSteps  tPrintInterval  lEnter
  selector_template[grep("lPrintD", selector_template) + 1] <- stringr::str_flatten(c(rep(" ", times = 5),
                                                                             ifelse(hydrus_model$print_options$interval_output_option, "t", "f"),
                                                                             rep(" ", times = 11),
                                                                             hydrus_model$print_options$time_info_print_every_n_time_steps,
                                                                             rep(" ", times = 13),
                                                                             hydrus_model$print_options$interval_output,
                                                                             rep(" ", times = 7),
                                                                             ifelse(hydrus_model$print_options$print_times, "t", "f")))

  # TPrint(1),TPrint(2),...,TPrint(MPL)
  selector_template[grep("TPrint", selector_template) + 1] <- stringr::str_flatten(c(rep(" ", times = 9),
                                                                          paste0(as.character(hydrus_model$print_options$times_to_print$times)),
                                                                          collapse = "         "))

  ## Update SELECTOR.IN
  writeLines(selector_template, file.path(hydrus_model$hydrus_project$project_path, "SELECTOR.IN"))
  cat("Updated BLOCK C: TIME INFORMATION of SELECTOR.IN file... \n")

}

#Time parameters are all numeric and in the 'time_unit' of the 'hydrus_model'.
# Time parameters include 'initial_time_step', 'minimum time step', 'maximum
# time step', 'lower_time_step_mult', 'upper_time_step_mult',
# 'lower_optim_iter_range', 'upper_optim_iter_range', 'initial model time',
# 'final_model_time'. Print options include 'print_times', a data.frame with one column of 'times' to
# print out. 'interval_output_option' is a logical indicating to turn on interval output
# (default is off, FALSE). 'interval_output' is used when
# 'interval_output_option = TRUE' and specfies the interval to output.
# 'print_time_interval' is a logical indicating whether or not to print time
# information (default on, TRUE). 'time_info_print_every_n_time_steps', default 1, print time interval.
#


