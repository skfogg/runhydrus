#' Initiates a HYDRUS project
#'
#' @param project_name Name of the HYDRUS project to create
#' @param parent_dir Path to the project folder
#' @param hydrus_version Integer indicating which HYDRUS version to use. Must be version 4 or 5.
#' @param description Optional project description character string
#' @param time_unit Character string indicating the temporal unit. Must be "seconds", "minutes", "hours", "days", or "years".
#' @param space_unit Character string indicating spatial unit. Must be either "m", "cm", or "mm".
#' @param print_times Integer indicating how many output times to print.
#' @param water_flow Logical indicating whether or not to simulate water flow
#' @param solute_transport Logical indicating whether or not to simulate solute transport
#' @param unsatchem Logical indicating whether or not t0 use the UnsatChem module
#' @param HP1 Logical indicating whether or not to use the HP1, Hydrus + PHREEQC, module
#' @param heat_transport Logical indicating whether or not to use the heat transport module
#' @param equilibrium_adsorption ?
#' @param mobile_immobile ?
#' @param root_water_uptake Logical indicating simulation of root water uptake
#' @param root_growth Logical indicating simulation of root growth
#' @param material_numbers Integer indicating the number of materials in model. Default 1.
#' @param subregion_numbers Integer indicating the number of subregions in model. Default 0.
#' @param number_solutes Integer indicating the number of solutes in simulation.
#' @param initial_condition ?
#' @param number_of_nodes Integer indicating the number of model nodes. Default 101.
#' @param profile_depth Numeric indicating the depth of the soil profile. Default 100.
#' @param observation_nodes Integer indicating the number of observation nodes. If NULL, then the number of model nodes is used.
#'
#' @returns an object of runhydrus_model class with all model info set
#' @export
#'
#' @examples
create_hydrus_project <- function(project_name,
                                  parent_dir,
                                  hydrus_version = 5,
                                  description = NULL,
                                  time_unit = "days",
                                  space_unit = "cm",
                                  print_times = 1,
                                  water_flow = TRUE,
                                  solute_transport = FALSE,
                                  unsatchem = FALSE,
                                  HP1 = FALSE,
                                  heat_transport = FALSE,
                                  equilibrium_adsorption = FALSE,
                                  mobile_immobile = FALSE,
                                  root_water_uptake = FALSE,
                                  root_growth = FALSE,
                                  material_numbers = 1,
                                  subregion_numbers = 0,
                                  number_solutes = 0,
                                  initial_condition = FALSE,
                                  number_of_nodes = 101,
                                  profile_depth = 100,
                                  observation_nodes = NULL){

  main_args <- c("HYDRUS_Version",
                 "WaterFlow",
                 "SoluteTransport",
                 "Unsatchem",
                 "HP1",
                 "HeatTransport",
                 "EquilibriumAdsorption",
                 "MobileImmobile",
                 "RootWaterUptake",
                 "RootGrowth",
                 "MaterialNumbers",
                 "SubregionNumbers",
                 "SpaceUnit",
                 "TimeUnit",
                 "PrintTimes",
                 "NumberOfSolutes",
                 "InitialCondition")
  profile_args <- c("NumberOfNodes",
                    "ProfileDepth",
                    "ObservationNodes")

  dat_file_main_args <- paste0(main_args, "=", c(hydrus_version,
                           as.numeric(water_flow),
                           as.numeric(solute_transport),
                           as.numeric(unsatchem),
                           as.numeric(HP1),
                           as.numeric(heat_transport),
                           as.numeric(equilibrium_adsorption),
                           as.numeric(mobile_immobile),
                           as.numeric(root_water_uptake),
                           as.numeric(root_growth),
                           material_numbers,
                           subregion_numbers,
                           space_unit,
                           time_unit,
                           print_times,
                           number_solutes,
                           as.numeric(initial_condition)))

  dat_file_profile_args <- paste0(profile_args, "=", c(number_of_nodes,
                                                       formatC(profile_depth, format = "E", digits = 2),
                                                       ifelse(is.null(observation_nodes), number_of_nodes, observation_nodes)
                                                       )
                                  )

  #
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


  if(hydrus_version == 4){
    description_dat_file <- file.path(project_path, "DESCRIPT.TXT")
    file.create(description_dat_file)
    description_contents <- c("Pcp_File_Version=1", description)
    write(description_contents, file = description_dat_file, append = FALSE)
  }

  project_dat_file <- file.path(project_path, "HYDRUS1D.DAT")
  file.create(project_dat_file)

  # if description is NULL, then use project name as description
  description <- ifelse(is.null(description), paste("project title", project_name), description)

  dat_file_contents <- c(";", "[Main]", dat_file_main_args,
                         ";", "[Profile]", dat_file_profile_args)
  write(dat_file_contents, file = project_dat_file, append = FALSE)

  cat("New HYDRUS project created in", project_path)

  return(list(project_name = project_name,
                       parent_dir = parent_dir,
                       # class = "runhydrus_model",
                       hydrus_version = hydrus_version,
                       description = description,
                       discritization = c(time_unit = time_unit,
                                          space_unit = space_unit),
                       observations = c(print_times = print_times,
                                        observation_nodes = NULL),
                       processes = c(water_flow = TRUE,
                                     solute_transport = FALSE,
                                     unsatchem = FALSE,
                                     HP1 = FALSE,
                                     heat_transport = FALSE,
                                     equilibrium_adsorption = FALSE,
                                     mobile_immobile = FALSE,
                                     root_water_uptake = FALSE,
                                     root_growth = FALSE),
                       geometry = c(material_numbers = 1,
                                    subregion_numbers = 0,
                                    number_of_nodes = 101,
                                    profile_depth = 100),
                       number_solutes = 0,
                       initial_condition = FALSE
  ))



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

}
