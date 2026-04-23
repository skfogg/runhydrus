#' Read an ATMOSPH.IN file from an existing HYDRUS project
#'
#' Reads the ATMOSPH.IN file from an existing project directory and returns a
#' list of arguments that can be passed directly to \code{\link{edit_atmosph_file}}.
#'
#' @param project_path Character string. Full absolute path to the HYDRUS project directory.
#'
#' @returns A named list with:
#'   \describe{
#'     \item{\code{atm_time_series}}{data.frame with columns \code{time}, \code{precip},
#'       \code{evap}, \code{transpiration}, \code{min_pressure_head}. When solute
#'       transport columns are present in the file, \code{top_conc} and \code{bot_conc}
#'       are included (or \code{top_conc1}/\code{bot_conc1}/etc. for multiple solutes).
#'       When the \code{ht} column contains non-zero values (variable pressure head BC),
#'       a \code{pressure_head} column is also included.}
#'     \item{\code{max_h_at_surface}}{Numeric. Maximum allowed pressure head at the soil surface (hCritS).}
#'   }
#' @export
#'
#' @examples
#' atm <- read_atmosph_file("C:/path/to/project")
#' edit_atmosph_file(hydrus_model,
#'                   atm_time_series  = atm$atm_time_series,
#'                   max_h_at_surface = atm$max_h_at_surface)
read_atmosph_file <- function(project_path) {

  split_ws <- function(line) unlist(strsplit(trimws(line), "\\s+"))

  atmosph <- readLines(file.path(project_path, "ATMOSPH.IN"))

  ## max_h_at_surface (hCritS)
  hcrits_idx       <- grep("hCritS", atmosph)[1]
  max_h_at_surface <- as.numeric(split_ws(atmosph[hcrits_idx + 1])[1])

  ## Column names from tAtm header line
  tatm_hdr_idx   <- grep("tAtm", atmosph)[1]
  file_col_names <- split_ws(atmosph[tatm_hdr_idx])

  ## Data rows: from header+1 until the "end***" footer (or end of file)
  end_idx    <- grep("^end\\*\\*\\*", atmosph)[1]
  if (is.na(end_idx)) end_idx <- length(atmosph) + 1L
  data_lines <- atmosph[(tatm_hdr_idx + 1L):(end_idx - 1L)]
  data_lines <- data_lines[nzchar(trimws(data_lines))]

  ## Parse into a data.frame; RootDepth appears in the header but has no data
  ## column, so only take as many names as there are actual columns.
  data_mat   <- do.call(rbind, lapply(data_lines, function(l) as.numeric(split_ws(l))))
  used_cols  <- file_col_names[seq_len(ncol(data_mat))]
  colnames(data_mat) <- used_cols
  data_df    <- as.data.frame(data_mat)

  ## Build atm_time_series with edit_atmosph_file column names
  atm_ts <- data.frame(
    time              = data_df[["tAtm"]],
    precip            = data_df[["Prec"]],
    evap              = data_df[["rSoil"]],
    transpiration     = data_df[["rRoot"]],
    min_pressure_head = data_df[["hCritA"]]
  )

  ## Add pressure_head when ht has non-zero values (variable_pressure_head BC)
  if ("ht" %in% names(data_df) && any(data_df[["ht"]] != 0)) {
    atm_ts$pressure_head <- data_df[["ht"]]
  }

  ## Add solute concentration columns when present
  ## Single solute: cTop/cBot → top_conc/bot_conc
  ## Multiple solutes: cTop1/cBot1/... → top_conc1/bot_conc1/...
  ctop_cols <- grep("^cTop", names(data_df), value = TRUE)
  cbot_cols <- grep("^cBot", names(data_df), value = TRUE)
  if (length(ctop_cols) == 1L) {
    atm_ts$top_conc <- data_df[[ctop_cols]]
    atm_ts$bot_conc <- data_df[[cbot_cols]]
  } else if (length(ctop_cols) > 1L) {
    for (s in seq_along(ctop_cols)) {
      atm_ts[[paste0("top_conc", s)]] <- data_df[[ctop_cols[s]]]
      atm_ts[[paste0("bot_conc", s)]] <- data_df[[cbot_cols[s]]]
    }
  }

  return(list(
    atm_time_series  = atm_ts,
    max_h_at_surface = max_h_at_surface
  ))
}
