#' Initiates a HYDRUS project
#'
#' @param project_name Name of the HYDRUS project to create
#' @param parent_dir Path to the project folder
#' @param hydrus_version Integer indicating which HYDRUS version to use. Must be version 4 or 5.
#' @param description Optional project description character string
#'
#' @returns an object of hydrus_project class with all model info set
#' @export
#'
#' @examples
create_hydrus_project <- function(project_name,
                                  parent_dir,
                                  hydrus_version = 5,
                                  description = NULL
                                  ){

  ## Create new directory if one does not exist:
  project_path <- file.path(parent_dir, project_name)

  if(dir.exists(project_path)) {

    dir_answer <- readline(prompt = paste("Folder",
                                          project_name,
                                          "already exists in project directory. All files will be deleted in",
                                          project_name,
                                          ". Proceed? y/n \n"))
    dir_answer <- substr(toupper(dir_answer), start = 1, stop = 1)

    if(dir_answer == "Y") {
      unlink(project_path, recursive = TRUE, force = TRUE)
      dir.create(project_path)
    } else {
      stop("HYDRUS project not created\n")
    }
  } else {
    dir.create(project_path)
  }

  ### Create DESCRIPT.TXT when HYDRUS version 4
  if(hydrus_version == 4){
    file.create(file.path(project_path, "DESCRIPT.TXT"))
    description_contents <- c("Pcp_File_Version=1", ifelse(is.null(description), paste("project name", project_name), description))
    write(description_contents, file = file.path(project_path, "DESCRIPT.TXT"), append = FALSE)
  }

  #### Create HYDRUS1d.DAT file
  file.create(file.path(project_path, "HYDRUS1D.DAT"))

  ## Create empty SELECTOR.IN file in the project path
  file.create(file.path(project_path, "SELECTOR.IN"))

  ## Get a basic SELECTOR.IN template and write to SELECTOR.IN file in project
  selector_template <- readLines(file("./templates/basic/SELECTOR.IN"))
  write(selector_template, file = selector_in_file)

  ## update file version to match hydrus version
  selector_template[1] <- paste0("Pcp_File_Version=", hydrus_version)

  cat("New HYDRUS project created in", project_path)

  return(list(project_name = project_name,
              project_path = project_path,
              # class = "hydrus_project",
              hydrus_version = hydrus_version,
              description = description))

}

# @param time_unit Character string indicating the temporal unit. Must be "seconds", "minutes", "hours", "days", or "years".
# @param space_unit Character string indicating spatial unit. Must be either "m", "cm", or "mm".
# @param print_times Integer indicating how many output times to print.
# @param water_flow Logical indicating whether or not to simulate water flow
# @param solute_transport Logical indicating whether or not to simulate solute transport
# @param unsatchem Logical indicating whether or not t0 use the UnsatChem module
# @param HP1 Logical indicating whether or not to use the HP1, Hydrus + PHREEQC, module
# @param heat_transport Logical indicating whether or not to use the heat transport module
# @param equilibrium_adsorption ?
# @param mobile_immobile ?
# @param root_water_uptake Logical indicating simulation of root water uptake
# @param root_growth Logical indicating simulation of root growth
# @param material_numbers Integer indicating the number of materials in model. Default 1.
# @param subregion_numbers Integer indicating the number of subregions in model. Default 0.
# @param number_solutes Integer indicating the number of solutes in simulation.
# @param initial_condition Logical. Initial conditions in pressure heads (FALSE; default) or water contents (TRUE).
# @param number_of_nodes Integer indicating the number of model nodes. Default 101.
# @param profile_depth Numeric indicating the depth of the soil profile. Default 100.
# @param observation_nodes Integer indicating the number of observation nodes. If NULL, then the number of model nodes is used.

# # TRASH? DOES not work properly
# args_vec = as.list(match.call())
# args_vec = lapply(args_vec[-1], FUN = function(x) unlist(x))
# # args_vec = unlist(unclass(args_vec))
# args_vec = do.call("c", args_vec)
# args_vec = ifelse(args_vec == TRUE, 1, args_vec)
# args_vec = ifelse(args_vec == FALSE, 0, args_vec)
#
# names(args_vec) = gsub("processes.", "", names(args_vec), fixed = TRUE)
# names(args_vec) = gsub("geometry.", "", names(args_vec), fixed = TRUE)
# names(args_vec) = gsub("initial_cond.", "", names(args_vec), fixed = TRUE)
#
#
# args_vec["ProfileDepth"] = toupper(format2sci(as.numeric(args_vec["ProfileDepth"]),
#                                               ndec = 2, power.digits = 3))

# args_names = names(args_vec)
#
# h1d_args_names = args_names[!(args_names %in% c("project_name", "parent_dir", "description"))]
# # h1d_args_names = gsub("Profile.", "", h1d_args_names, fixed = TRUE)
#
#
# hydrus1d_template = system.file("templates/HYDRUS1D.DAT", package = "hydrusR")
# h1d_dat = readLines(hydrus1d_template, n = -1L, encoding = "unknown")
# #
# discript_vec = c("Pcp_File_Version=1", description)
#
# for(a in 1:length(h1d_args_names)){
#   arg_a = h1d_args_names[a]
#   arg_value = args_vec[arg_a]
#   arg_index = grep(arg_a, h1d_dat)
#   h1d_dat[arg_index] = paste0(arg_a, "=", arg_value)
#
# }
#
# write(discript_vec, file = discript_file, append = FALSE)
# write(h1d_dat, file = h1ddat_file, append = FALSE)

# ###
# selector_in_file <- system.file("templates/SELECTOR.IN", package = "hydrusR")
# selector_in <- readLines(selector_in_file, n = -1L, encoding = "unknown")
#
# lunit_loc <- grep("LUnit", selector_in)
# unit_lines <- lunit_loc + 1:2
# selector_in[unit_lines] <- c(space_unit, time_unit)
#
# timeinfo_loc <- grep("BLOCK C", selector_in, fixed = TRUE)
# timeinfo_data = selector_in[timeinfo_loc+2]
#
# timeinfo_split = unlist(strsplit(x = timeinfo_data, split = " "))
# timeinfo_split = timeinfo_split[timeinfo_split != ""]
# timeinfo_new = as.numeric(timeinfo_split)
#
# names(timeinfo_split) = c("dt", "dtMin",  "dtMax", "DMul", "DMul2", "ItMin", "ItMax", "MPL")
# names(timeinfo_new) = c("dt", "dtMin",  "dtMax", "DMul", "DMul2", "ItMin", "ItMax", "MPL")
#
# ## incorrect understanding of time info
# if(time_unit == "hours"){
#
#   timeinfo_new[c("dt", "dtMin", "dtMax")] = 24*timeinfo_new[c("dt", "dtMin", "dtMax")]
#
# } else if (time_unit == "minutes") {
#   timeinfo_new[c("dt", "dtMin", "dtMax")] = 60*24*timeinfo_new[c("dt", "dtMin", "dtMax")]
#
# } else if(time_unit == "seconds"){
#   timeinfo_new[c("dt", "dtMin", "dtMax")] = 3600*24*timeinfo_new[c("dt", "dtMin", "dtMax")]
#
# } else if(time_unit == "years") {
#   timeinfo_new[c("dt", "dtMin", "dtMax")] = 1/365*timeinfo_new[c("dt", "dtMin", "dtMax")]
#
# }
#
# timeinfo_new[c("dt", "dtMin", "dtMax")] = format2sci(timeinfo_new[c("dt", "dtMin", "dtMax")], ndec = 3, power.digits = 3)
# fmt_space = c(12, 13, 12, 8, 8, 6, 6, 6)
# fmt_vec = paste("%", fmt_space, "s", sep = "")
# timeinfo_new_fmt = sprintf(fmt = fmt_vec, timeinfo_new)
# timeinfo_new_str = paste(timeinfo_new_fmt, collapse = "")
#
# selector_data[timeinfo_ind + 2] = timeinfo_new_str
#
# write(selector_data, file = file.path(project_path, basename(selector_in)), append = F)
