#' Set PROFILE.DAT file
#'
#' @usage edit_soil_profile(
#'  hydrus_model,
#'  mesh_density = data.frame(fixed_points = c(0, -101),
#'                            upper_relative_size_fe = 1.0,
#'                            lower_relative_size_fe = 1.0),
#'  set_mesh_nodes_manually = FALSE,
#'  mesh_nodes_manual = data.frame(number = 1:101,
#'                                 z = -0:-100),
#'  nodal_soil_properties = list(h = -101,
#'                               root = 0,
#'                               a_xz = 1,
#'                               b_xz = 1,
#'                               d_xz = 1,
#'                               mat = 1,
#'                               lay = 1,
#'                               temp = 20,
#'                               conc = 0)
#' )
#'
#' @param hydrus_model a hydrus model created with \code{\link{create_hydrus_project}}
#' @param mesh_density data.frame of parameters needed for the mesh density calculation. See details.
#' @param set_mesh_nodes_manually logical. If FALSE, will use the parameters in the \code{mesh_density} data.frame. If TRUE, you set your own mesh node locations in the \code{mesh_nodes_manual} data.frame.
#' @param mesh_nodes_manual data.frame of mesh number and locations when setting mesh nodes manually.
#' @param nodal_soil_properties data.frame of nodal soil properties
#'
#' @returns edits PROFILE.DAT file
#' @export
#'
#' @details
#' \describe{
#'  \item{\code{mesh_density}}{Column names of data.frame must be \code{"fixed_points"}, \code{"upper_relative_size_fe"}, and \code{"lower_relative_size_fe"}. Finite element (FE) sizes are proportionally distributed according to \code{"upper_relative_size_fe"} and \code{"lower_relative_size_fe"} between the fixed points provided.}
#'  \item{\code{mesh_nodes_manual}}{Column names of data.frame must be \code{"number"} and \code{"z"}. Number is the nodes ID number and z is the coordinate depth.}
#'  \item{\code{nodal_soil_properties}}{Column names of data.frame must be \code{"h"} (initial pressure head or water content), \code{"root"} (depth distribution of root water uptake, equal to 0 is node lies outside of root zone), \code{"a_xz"} (nodal value of dimensionless scaling factor associated with pressure head), \code{"b_xz"} (nodal value of dimensionless scaling factor associated with saturated hydraulic conductivity), \code{"d_xz"} (nodal value of dimensionless scaling factor associated with water content), \code{"mat"} (material), \code{"lay"} (subregion), \code{"temp"} (iniital nodal temperature), and \code{"conc"} (initial nodal concentration).
#'  }
#' }
#'
#'
#' @examples edit_soil_profile(hydrus_model)
edit_soil_profile <- function(hydrus_model,
                              mesh_density = data.frame(fixed_points = c(0, -101),
                                                        upper_relative_size_fe = 1.0,
                                                        lower_relative_size_fe = 1.0),
                              set_mesh_nodes_manually = FALSE,
                              mesh_nodes_manual = data.frame(number = 1:101,
                                                                   z = -0:-100),
                              nodal_soil_properties = list(h = -101,
                                                                 root = 0,
                                                                 a_xz = 1,
                                                                 b_xz = 1,
                                                                 d_xz = 1,
                                                                 mat = 1,
                                                                 lay = 1,
                                                                 temp = 20,
                                                                 conc = 0)){

  ## default node spacing
  default_node_spacing <- hydrus_model$geometry$profile_depth/(hydrus_model$geometry$number_nodes-1)

  if(set_mesh_nodes_manually){
    mesh_nodes <- mesh_nodes_manual
  }else{
    mesh_nodes <- data.frame(number = 1:hydrus_model$geometry$number_nodes,
                             z = seq(0, hydrus_model$geometry$profile_depth, by = default_node_spacing))
  }


  ## Get PROFILE.DAT file of project
  profile_connection <- file.path(paste0(hydrus_model$hydrus_project$project_path, "/PROFILE.DAT"))
  profile_template <- readLines(system.file("templates", "PROFILE.DAT", package = "runhydrus"),
                                n = -1L, encoding = "unknown")

  profile_template[grep("Pcp", profile_template)+1] <- stringr::str_flatten(c(rep(" ", times = 4),
                                                                   nrow(mesh_density)))

  ## Convert to Scientific notation with 6 trailing 0s
  mesh_density <- hydrus_sci_format(mesh_density)

  # Add row.name column to mesh_density
  mesh_density <- cbind(data.frame(no = row.names(mesh_density)),
                        mesh_density)

  # write to profile_template to accommodate for variable number of rows:
  write(profile_template[1:(grep("Pcp", profile_template)+1)], profile_connection)
  for(i in 1:nrow(mesh_density)){
    write(paste0(mesh_density[i,], collapse = "    "), profile_connection, append = TRUE)
  }
  write(profile_template[grep("Mat", profile_template):length(profile_template)], profile_connection, append = TRUE)

  ## update profile_template
  profile_template <- readLines(profile_connection)

  ## nodal flow properties
  ## Convert some columns to scientific notation with 6 trailing 0s
  nodal_soil_properties <- cbind(mesh_nodes, nodal_soil_properties)

  nodal_soil_properties <- as.data.frame(lapply(nodal_soil_properties, as.numeric))
  nodal_sci <- hydrus_sci_format(nodal_soil_properties[,c("z", "h", "root", "a_xz", "b_xz", "d_xz", "temp", "conc")])

  nodal_soil_properties <- cbind(nodal_soil_properties$number,
                                 nodal_sci[, c("z","h")],
                                 nodal_soil_properties[, c("mat", "lay")],
                                 nodal_sci[, c("root","a_xz", "b_xz", "d_xz", "temp", "conc")])

  write(profile_template[1:grep("Mat", profile_template)], profile_connection)
  for(i in 1:nrow(nodal_soil_properties)){
    write(paste0(nodal_soil_properties[i,], collapse = "    "), profile_connection, append = TRUE)
  }
  write(as.character(hydrus_model$geometry$observation_nodes_n), profile_connection, append = TRUE)
  if(hydrus_model$geometry$observation_nodes_n > 0){
    # obs_subset <- subset(nodal_soil_properties, z %in% hydrus_model$geometry$observation_nodes)
    # write(paste0(obs_subset$number, collapse = "   "), profile_connection, append = TRUE)
    #
    write(paste0(hydrus_model$geometry$observation_nodes, collapse = "   "), profile_connection, append = TRUE)
  }

  cat("Updated PROFILE.DAT file... \n")

}
