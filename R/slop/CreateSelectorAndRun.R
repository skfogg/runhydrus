# Create Selector/Run Function ####
#' Title
#'
#' @param my.time.int
#' @param met.data1
#' @param vg
#' @param Zf
#' @param SS.name
#' @param model.number
#' @param tv.info
#'
#' @returns
#' @export
#'
#' @examples
CreateSelectorAndRun <-
  function(my.time.int,
           met.data1,
           vg,
           Zf,
           # rotation_name, # omit for now (2024-10-08)
           SS.name,
           model.number,
           tv.info)
  {
    if(length(day.check)!=length(met.data1$Date)+10)
    {
      my.time.int = time.int.vector[i]
      create_selector(met = met.data1,
                      vg = vg,
                      time.int = my.time.int,
                      model.number = model.number,
                      tv.info= tv.info,
                      material.number = material.number)

      # Run Hydrus

      if(model.number != 9){

        system("H1D_CALC.EXE", timeout= 600)

      }

      if(model.number == 9){

        system("H1D_Dual.exe", timeout = 600)

      }


      my.time <- Sys.time()-60*60*6
      my.time <- paste0(substr(my.time,1,10),"_",
                        substr(my.time,12,13),
                        substr(my.time,15,16),
                        substr(my.time,18,19))
      Zf.3digit <- str_pad(string=as.character(Zf), width=3, side="left", pad=0)
      # my.filename <- paste0(rotation_name,"_",Zf.3digit,"_",SS.name,"_",my.time) # rot name removed
      my.filename <- paste0(Zf.3digit,"_",SS.name,"_",my.time)
      # if(!is.na(output.name.override)) # omit for now (2024-10-08)
      # {
      #   my.filename <- output.name.override
      # }
      print(my.filename)
      print(my.time.int)
      day.check <- readLines('./T_Level.out')
      list.temp <- list(day.check = day.check,
                        my.time.int = my.time.int,
                        my.filename = my.filename)
    }
    return(list.temp)
  }
