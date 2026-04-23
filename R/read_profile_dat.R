#' Read a PROFILE.DAT file from an existing HYDRUS project
#'
#' Reads the PROFILE.DAT file from an existing project directory and returns a
#' list of arguments that can be passed directly to \code{\link{edit_soil_profile}},
#' along with geometry values needed to update the \code{hydrus_model}.
#'
#' @param project_path Character string. Full absolute path to the HYDRUS project directory.
#'
#' @returns A named list with:
#'   \describe{
#'     \item{\code{mesh_density}}{data.frame with columns \code{fixed_points},
#'       \code{upper_relative_size_fe}, \code{lower_relative_size_fe}.}
#'     \item{\code{set_mesh_nodes_manually}}{Always \code{TRUE}: node positions are
#'       returned as read from the file.}
#'     \item{\code{mesh_nodes_manual}}{data.frame with columns \code{number} and \code{z}
#'       giving the exact node positions in the file.}
#'     \item{\code{nodal_soil_properties}}{Named list of per-node vectors: \code{h},
#'       \code{root}, \code{a_xz}, \code{b_xz}, \code{d_xz}, \code{mat}, \code{lay}.
#'       Includes \code{temp} when heat transport was active, \code{conc} when solute
#'       transport was active, and \code{fracture_conc} for dual-permeability models
#'       with solute transport.}
#'     \item{\code{number_nodes}}{Integer. Number of nodes — use to update
#'       \code{hydrus_model$geometry$number_nodes}.}
#'     \item{\code{profile_depth}}{Numeric. Profile depth — use to update
#'       \code{hydrus_model$geometry$profile_depth}.}
#'     \item{\code{observation_nodes_n}}{Integer. Number of observation nodes — use to
#'       update \code{hydrus_model$geometry$observation_nodes_n}.}
#'     \item{\code{observation_nodes}}{Integer vector of observation node IDs — use to
#'       update \code{hydrus_model$geometry$observation_nodes}.}
#'   }
#' @export
#'
#' @examples
#' prof <- read_profile_dat("C:/path/to/project")
#'
#' ## Update geometry slots that are stored only in PROFILE.DAT:
#' hydrus_model$geometry$number_nodes        <- prof$number_nodes
#' hydrus_model$geometry$profile_depth       <- prof$profile_depth
#' hydrus_model$geometry$observation_nodes_n <- prof$observation_nodes_n
#' hydrus_model$geometry$observation_nodes   <- prof$observation_nodes
#'
#' ## Re-write PROFILE.DAT from the read parameters:
#' edit_soil_profile(hydrus_model,
#'                   mesh_density            = prof$mesh_density,
#'                   set_mesh_nodes_manually = prof$set_mesh_nodes_manually,
#'                   mesh_nodes_manual       = prof$mesh_nodes_manual,
#'                   nodal_soil_properties   = prof$nodal_soil_properties)
read_profile_dat <- function(project_path) {

  split_ws <- function(line) unlist(strsplit(trimws(line), "\\s+"))

  profile <- readLines(file.path(project_path, "PROFILE.DAT"))

  ## -----------------------------------------------------------------------
  ## Mesh density fixed points (lines 3 to 2 + n_fixed)
  ## -----------------------------------------------------------------------
  n_fixed <- as.integer(split_ws(profile[2])[1])
  md_rows <- lapply(seq_len(n_fixed), function(i) {
    v <- as.numeric(split_ws(profile[2L + i]))
    v[2:4]   # drop row index; keep fixed_point, upper_rel, lower_rel
  })
  md_mat <- do.call(rbind, md_rows)
  mesh_density <- data.frame(
    fixed_points           = md_mat[, 1],
    upper_relative_size_fe = md_mat[, 2],
    lower_relative_size_fe = md_mat[, 3]
  )

  ## -----------------------------------------------------------------------
  ## Header line: n_nodes  lChem  lHeat  KodCB  <col labels...>
  ## -----------------------------------------------------------------------
  hdr_idx  <- 2L + n_fixed + 1L
  hdr_vals <- split_ws(profile[hdr_idx])
  n_nodes  <- as.integer(hdr_vals[1])
  lChem    <- as.integer(hdr_vals[2])
  lHeat    <- as.integer(hdr_vals[3])
  has_sconc <- grepl("SConc", profile[hdr_idx], ignore.case = TRUE)

  ## -----------------------------------------------------------------------
  ## Node rows
  ## -----------------------------------------------------------------------
  node_start <- hdr_idx + 1L
  node_end   <- node_start + n_nodes - 1L

  node_mat <- do.call(rbind, lapply(profile[node_start:node_end], function(l) {
    as.numeric(split_ws(l))
  }))

  ## Columns always present: number, z, h, mat, lay, root, a_xz, b_xz, d_xz
  ci <- 1L  # column iterator
  node_number <- as.integer(node_mat[, ci]); ci <- ci + 1L
  node_z      <-            node_mat[, ci];  ci <- ci + 1L
  node_h      <-            node_mat[, ci];  ci <- ci + 1L
  node_mat_id <- as.integer(node_mat[, ci]); ci <- ci + 1L
  node_lay    <- as.integer(node_mat[, ci]); ci <- ci + 1L
  node_root   <-            node_mat[, ci];  ci <- ci + 1L
  node_a_xz   <-            node_mat[, ci];  ci <- ci + 1L
  node_b_xz   <-            node_mat[, ci];  ci <- ci + 1L
  node_d_xz   <-            node_mat[, ci];  ci <- ci + 1L

  ## Optional columns depending on active processes
  node_temp  <- NULL
  node_conc  <- NULL
  node_sconc <- NULL

  if (lHeat == 1L) {
    node_temp <- node_mat[, ci]; ci <- ci + 1L
  }
  if (lChem == 1L) {
    node_conc <- node_mat[, ci]; ci <- ci + 1L
    if (has_sconc) {
      node_sconc <- node_mat[, ci]
    }
  }

  ## Build nodal_soil_properties list
  nsp <- list(
    h    = node_h,
    root = node_root,
    a_xz = node_a_xz,
    b_xz = node_b_xz,
    d_xz = node_d_xz,
    mat  = node_mat_id,
    lay  = node_lay
  )
  if (!is.null(node_temp))  nsp$temp          <- node_temp
  if (!is.null(node_conc))  nsp$conc          <- node_conc
  if (!is.null(node_sconc)) nsp$fracture_conc <- node_sconc

  ## -----------------------------------------------------------------------
  ## Observation nodes (lines after the node block)
  ## -----------------------------------------------------------------------
  obs_line_idx <- node_end + 1L
  obs_n        <- as.integer(split_ws(profile[obs_line_idx])[1])
  obs_nodes    <- integer(0)
  if (obs_n > 0L && length(profile) >= obs_line_idx + 1L) {
    obs_nodes <- as.integer(split_ws(profile[obs_line_idx + 1L]))
  }

  ## profile_depth: absolute value of the deepest mesh-density fixed point
  profile_depth <- abs(mesh_density$fixed_points[nrow(mesh_density)])

  ## -----------------------------------------------------------------------
  ## Return
  ## -----------------------------------------------------------------------
  return(list(
    mesh_density            = mesh_density,
    set_mesh_nodes_manually = TRUE,
    mesh_nodes_manual       = data.frame(number = node_number, z = node_z),
    nodal_soil_properties   = nsp,
    number_nodes            = n_nodes,
    profile_depth           = profile_depth,
    observation_nodes_n     = obs_n,
    observation_nodes       = obs_nodes
  ))
}
