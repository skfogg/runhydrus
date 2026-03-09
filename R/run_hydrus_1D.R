#' Executes HYDRUS 1D executable
#'
#' @param hydrus_model hydrus model
#' @param hydrus_path Path to the Hydrus 1D executable. NULL will use default install path used by HYDRUS.
#' @param show_output Logical (Default = TRUE) whether the shell
#'                    output should be visible on R console (relevant only on windows)
#' @param ...
#'
#' @return
#' @export
#'
#' @examples

run_hydrus_1D <- function(hydrus_model,
                          hydrus_path = NULL,
                          show_output = TRUE){

  os_type <- .Platform$OS.type

  if(is.null(hydrus_path)) {
    if(os_type == "windows"){

      if(hydrus_model$hydrus_project$hydrus_version == 4){
        win_def_hdir <- "C:/Program Files (x86)/PC-Progress"
        h1d_version_dir <- list.files(win_def_hdir, full.names = T, pattern = "Hydrus-1D")
      }
      if(hydrus_model$hydrus_project$hydrus_version == 5){
        win_def_hdir <- "C:/Program Files/PC-Progress"
        h1d_version_dir <- list.files(win_def_hdir, full.names = T, pattern = "HYDRUS")
      }

      h1d_versions <- sapply(basename(h1d_version_dir),
                            function(x) {
                              name_split = unlist(strsplit(x, " |\\."))
                              return(as.numeric(name_split[3]))
                            })

      hydrus_path <- h1d_version_dir[which.max(h1d_versions)]

    }

  }

  if(hydrus_model$hydrus_project$hydrus_version == 4){
    hydrus_exe <- "H1D_CALC.EXE"  #### Windows specific executable name
  }
  if(hydrus_model$hydrus_project$hydrus_version == 5){
    hydrus_exe <- "H1D_Calc64.EXE"  #### Windows specific executable name
    if(hydrus_model$soil_hydraulics$soil_hydraulic_model == 8){
      hydrus_exe <- "H1D_Dual64.EXE"
    }
  }

  oldwd <- getwd()

  #system2(paste0("icacls '",  hydrus_path, "\\Bin /grant 'skati':(F)"))
  #Sys.chmod(paste0(hydrus_path, "/Bin"), "777")
  file_level01 <- file.path(paste0(hydrus_path, "/Bin"), "LEVEL_01.DIR")
  #
  # if(!file.exists(file_level01)) file(file_level01, "w+")
  #
  write(x = noquote(hydrus_model$hydrus_project$project_path),
        file = file_level01,
        append = F)

  if(hydrus_model$hydrus_project$hydrus_version == 4){
    setwd(hydrus_path)
  }
  if(hydrus_model$hydrus_project$hydrus_version == 5){
    setwd(paste0(hydrus_path, "/Bin"))
  }


  if(os_type == "unix") {
    system(paste0("./", hydrus_exe))
  } else {
    system(hydrus_exe,
           show.output.on.console = show_output,
           minimized = TRUE,
           invisible = TRUE)
  }

  setwd(oldwd)

}
