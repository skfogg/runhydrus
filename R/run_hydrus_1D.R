#' Executes HYDRUS 1D executable
#'
#' @param hydrus_model hydrus model
#' @param hydrus_path Path to the Hydrus 1D executable. NULL will use default install path used by HYDRUS.
#' @param show_output Logical (Default = TRUE) whether the shell
#'                    output should be visible on R console (relevant only on windows)
#'
#' @details
#' The user must have HYDRUS installed and the installation directory of HYDRUS
#' must have read/write permissions for the user.
#'
#' For a Windows user, this can be done by running PowerShell as an administrator and
#' executing the following:
#'
#'# Define the folder path to the HYDRUS executable (.exe)
#' \code{$folderPath = "C:\Program Files\PC-Progress\HYDRUS 5.06 64-bit\Bin"}
#'
#'# Get the current ACL (Access Control List)
#' \code{$acl = Get-Acl $folderPath}
#'
#' # Define the access rule: Everyone, FullControl, Inheritance, Allow
#'\code{$accessRule = New-Object System.Security.AccessControl.FileSystemAccessRule(
#'     "Everyone",
#'     "FullControl",
#'     "ContainerInherit,ObjectInherit",
#'     "None",
#'     "Allow")}
#'
#'   # Add the new rule to the ACL
#'   \code{$acl.SetAccessRule($accessRule)}
#'
#'   # Apply the updated ACL to the folder
#'   \code{Set-Acl -Path $folderPath -AclObject $acl}
#'
#'   # Check permissions (optional)
#'   \code{Get-Acl $folderPath}
#'
#'
#' @return executes a HYDRUS model
#' @export
#'
#' @examples run_hydrus_1D(hydrus_model)

run_hydrus_1D <- function(hydrus_model,
                          hydrus_path = NULL,
                          show_output = TRUE){


  ## expand to full path:
  hydrus_project_path  <- normalizePath(hydrus_model$hydrus_project$project_path)

  ## determine OS
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
                              return(name_split[3])
                            })
      if(h1d_versions == "xx"){
        hydrus_path <- h1d_version_dir
      }else{
        h1d_versions <- sapply(basename(h1d_version_dir),
                               function(x) {
                                 name_split = unlist(strsplit(x, " |\\."))
                                 return(as.numeric(name_split[3]))
                                 })
        hydrus_path <- h1d_version_dir[which.max(h1d_versions)]
      }
    }
  }

  if(file.access(hydrus_path, mode = 2) == -1){
    stop(cat(paste0("Write permissions to ", hydrus_path, " denied.\nRead help file for run_hydrus_1D() to fix. ")))
  }

  if(hydrus_model$hydrus_project$hydrus_version == 4){
    hydrus_exe <- "H1D_CALC.EXE"  #### Windows specific executable name
    if(hydrus_model$soil_hydraulics$soil_hydraulic_model == 9){
      hydrus_exe <- "H1D_Dual.EXE"
    }
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
  if(hydrus_model$hydrus_project$hydrus_version == 5){
    file_level01 <- file.path(paste0(hydrus_path, "/Bin"), "LEVEL_01.DIR")
  }
  if(hydrus_model$hydrus_project$hydrus_version == 4){
    file_level01 <- file.path(hydrus_path, "LEVEL_01.DIR")
  }

  #
  # if(!file.exists(file_level01)) file(file_level01, "w+")
  #
  if (file.exists(file_level01)) {
    info <- file.info(file_level01)
    print(info$mode)  # Numeric mode (UNIX-style permissions)
  } else {
    cat("File does not exist.\n")
  }

  write(x = noquote(hydrus_project_path),
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
