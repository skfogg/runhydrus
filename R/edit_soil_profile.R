#' Set PROFILE.DAT file
#'
#' @usage edit_soil_profile(
#'  hydrus_model,
#'  mesh_density = data.frame(fixed_points = c(0, -100),
#'                            upper_relative_size_fe = 1.0,
#'                            lower_relative_size_fe = 1.0),
#'  set_mesh_nodes_manually = FALSE,
#'  mesh_nodes_manual = data.frame(number = 1:101,
#'                                 z = -0:-100),
#'  nodal_soil_properties = list(h = -100,
#'                               root = 0,
#'                               a_xz = 1,
#'                               b_xz = 1,
#'                               d_xz = 1,
#'                               mat = 1,
#'                               lay = 1,
#'                               fracture_conc = 0)
#' )
#'
#' @param hydrus_model a hydrus model created with \code{\link{create_hydrus_project}}
#' @param mesh_density data.frame of parameters needed for the mesh density calculation. See details.
#' @param set_mesh_nodes_manually logical. If FALSE, will use the parameters in the \code{mesh_density} data.frame. If TRUE, you set your own mesh node locations in the \code{mesh_nodes_manual} data.frame.
#' @param mesh_nodes_manual data.frame of mesh number and locations when setting mesh nodes manually.
#' @param nodal_soil_properties list of nodal soil properties. Scalar values are recycled to all nodes; vectors must have length equal to the number of nodes.
#'
#' @returns edits PROFILE.DAT file
#' @export
#'
#' @details
#' \describe{
#'  \item{\code{mesh_density}}{Column names of data.frame must be \code{"fixed_points"}, \code{"upper_relative_size_fe"}, and \code{"lower_relative_size_fe"}. Finite element (FE) sizes are proportionally distributed according to \code{"upper_relative_size_fe"} and \code{"lower_relative_size_fe"} between the fixed points provided.}
#'  \item{\code{mesh_nodes_manual}}{Column names of data.frame must be \code{"number"} and \code{"z"}. Number is the node ID number and z is the coordinate depth (negative, in cm).}
#'  \item{\code{nodal_soil_properties}}{Named list with elements \code{"h"} (initial pressure head), \code{"root"} (root water uptake distribution; 0 outside root zone), \code{"a_xz"} (scaling factor for pressure head), \code{"b_xz"} (scaling factor for saturated hydraulic conductivity), \code{"d_xz"} (scaling factor for water content), \code{"mat"} (material number; scalar or per-node vector), \code{"lay"} (subregion). Optionally include \code{"temp"} (initial nodal temperature) when heat transport is enabled. For dual-permeability models with solute transport (\code{soil_hydraulic_model = 8} and \code{main_processes$solute_transport = TRUE}), must also include \code{"fracture_conc"} (initial solute concentration in the fracture domain). Scalar values are recycled to all nodes; vectors must have length equal to the number of nodes.}
#' }
#'
#'
#' @examples edit_soil_profile(hydrus_model)
edit_soil_profile <- function(hydrus_model,
                              mesh_density = data.frame(fixed_points = c(0, -100),
                                                        upper_relative_size_fe = 1.0,
                                                        lower_relative_size_fe = 1.0),
                              set_mesh_nodes_manually = FALSE,
                              mesh_nodes_manual = data.frame(number = 1:101,
                                                             z = -0:-100),
                              nodal_soil_properties = list(h = -100,
                                                           root = 0,
                                                           a_xz = 1,
                                                           b_xz = 1,
                                                           d_xz = 1,
                                                           mat = 1,
                                                           lay = 1)){

  n_nodes <- hydrus_model$geometry$number_nodes

  ## Generate mesh nodes
  if(set_mesh_nodes_manually){
    mesh_nodes <- mesh_nodes_manual
  } else {
    default_spacing <- hydrus_model$geometry$profile_depth / (n_nodes - 1)
    mesh_nodes <- data.frame(number = 1:n_nodes,
                             z = seq(0, -hydrus_model$geometry$profile_depth,
                                     by = -default_spacing))
  }

  ## Build full nodal data frame; recycle scalar properties to all nodes
  nodal_df <- data.frame(number = mesh_nodes$number, z = mesh_nodes$z)
  for(nm in names(nodal_soil_properties)){
    val <- nodal_soil_properties[[nm]]
    nodal_df[[nm]] <- if(length(val) == 1L) rep(val, n_nodes) else val
  }

  ## Determine HYDRUS PROFILE.DAT flags:
  ## lChem = 1 if solute transport is active (writes Conc/SConc columns per node)
  ## lHeat = 1 if heat transport is active OR solute transport is active (writes Temp column)
  ## KodCB = 0 when solute transport is active, 1 otherwise
  lChem <- as.integer(isTRUE(hydrus_model$main_processes$solute_transport))
  lHeat <- as.integer(isTRUE(hydrus_model$main_processes$heat_transport) || lChem == 1L)
  KodCB <- if(lChem == 1L) 0L else 1L
  is_dual_perm <- isTRUE(hydrus_model$soil_hydraulics$soil_hydraulic_model == 8)

  ## Output file path
  profile_path <- file.path(hydrus_model$hydrus_project$project_path, "PROFILE.DAT")

  ## --- Build output lines ---
  out <- character(0)

  ## Line 1: version
  out <- c(out, "Pcp_File_Version=5")

  ## Line 2: number of fixed points in mesh density
  out <- c(out, sprintf("%5d", nrow(mesh_density)))

  ## Mesh density rows
  md_sci <- hydrus_sci_format(mesh_density)
  for(i in seq_len(nrow(mesh_density))){
    out <- c(out, paste0(
      sprintf("%5d", i),
      formatC(md_sci$fixed_points[i],              width = 15),
      formatC(md_sci$upper_relative_size_fe[i],    width = 15),
      formatC(md_sci$lower_relative_size_fe[i],    width = 15)
    ))
  }

  ## Node count / flag header line
  ## Base labels always present; Temp added when lHeat=1; Conc+SConc added when lChem=1
  col_labels <- " x         h      Mat  Lay      Beta           Axz            Bxz            Dxz          Temp          Conc "
  if(lChem == 1L && is_dual_perm) col_labels <- paste0(col_labels, "           SConc")

  out <- c(out, paste0(
    sprintf("%5d%5d%5d%5d", n_nodes, lChem, lHeat, KodCB),
    col_labels
  ))

  ## Build additional nodal columns for heat and solute transport
  sci_cols <- c("z", "h", "root", "a_xz", "b_xz", "d_xz")
  if(lHeat == 1L){
    if(!"temp" %in% names(nodal_df)) nodal_df$temp <- 20.0
    sci_cols <- c(sci_cols, "temp")
  }
  if(lChem == 1L){
    if(!"conc" %in% names(nodal_df)) nodal_df$conc <- 0.0
    sci_cols <- c(sci_cols, "conc")
    if(is_dual_perm){
      if(!"fracture_conc" %in% names(nodal_df))
        stop("nodal_soil_properties must include 'fracture_conc' for dual-permeability models with solute transport.")
      nodal_df$sconc <- nodal_df$fracture_conc
      sci_cols <- c(sci_cols, "sconc")
    }
  }
  nodal_sci <- hydrus_sci_format(nodal_df[, sci_cols, drop = FALSE])

  for(i in seq_len(n_nodes)){
    row <- paste0(
      sprintf("%5d",    nodal_df$number[i]),
      formatC(nodal_sci$z[i],    width = 15),
      formatC(nodal_sci$h[i],    width = 15),
      sprintf("%5d%5d", as.integer(nodal_df$mat[i]), as.integer(nodal_df$lay[i])),
      formatC(nodal_sci$root[i], width = 15),
      formatC(nodal_sci$a_xz[i], width = 15),
      formatC(nodal_sci$b_xz[i], width = 15),
      formatC(nodal_sci$d_xz[i], width = 15)
    )
    if(lHeat == 1L) row <- paste0(row, formatC(nodal_sci$temp[i], width = 15))
    if(lChem == 1L) row <- paste0(row, formatC(nodal_sci$conc[i], width = 15))
    if(lChem == 1L && is_dual_perm) row <- paste0(row, formatC(nodal_sci$sconc[i], width = 15))
    out <- c(out, row)
  }

  ## Observation nodes
  obs_n <- hydrus_model$geometry$observation_nodes_n
  out <- c(out, sprintf("%5d", obs_n))
  if(obs_n > 0){
    out <- c(out, paste(sprintf("%5d", hydrus_model$geometry$observation_nodes), collapse = ""))
  }

  ## Write to file
  writeLines(out, profile_path)

  cat("Updated PROFILE.DAT file... \n")

}
