#' Title
#'
#' @param Zf
#' @param obs9
#' @param material.number
#' @param material.divide
#' @param model.number
#'
#' @returns
#' @export
#'
#' @examples
create_profile_modelinputs <- function(Zf,obs9,material.number, material.divide,
                                       model.number){

  header.size = 5 # rows above data
  end.size = 195 # more than necessary
  profile.file = file("./PROFILE.DAT","r")
  before.text = readLines(profile.file,n=header.size)
  profile.data <- readLines(profile.file,n= 176)
  after.text = readLines(profile.file,n=end.size)
  close(profile.file)

  #load end conditions from run 1
  end.cond.file <- file("./Nod_Inf.out", "r")

  if(model.number != 9){
    end.cond.text.d1 <- readLines(end.cond.file, n=end.size+3)
  }

  if(model.number == 9){
    end.cond.text.d1 <- readLines(end.cond.file, n=end.size+1)
  }

  end.cond.text.dl <- readLines(end.cond.file)
  close(end.cond.file)

  #For just 1 soil material
  if(material.number == 1){

    # set material 1 for 1 cm to max soil depth
    substr(profile.data[1:Zf],40,40) <- "1"

    #everything below fine textured soil (material 2, gravel layer)
    substr(profile.data[(Zf+1):176],40,40) <- "2"

  }

  if(material.number >= 2){

    for(i in 1:material.number-1){

      # Intermediate materials
      substr(profile.data[material.divide[i]+1:material.divide[i+1]],40,40) <- as.character(i)

    }

    #last material before bottom of fine textured soil
    substr(profile.data[material.divide[material.number]+1:Zf],40,40) <- as.character(material.number)

    #everything below fine textured soil(gravel)
    substr(profile.data[(Zf+1):176],40,40) <- as.character(material.number+1)


  }


  #add pressure head from previous run
  next.col <- unlist(gregexpr("0\\.", end.cond.text.dl[2]))[1]

  substr(profile.data[1:176],22,36) <- str_pad(substr(end.cond.text.dl[1:176],
                                                      15,
                                                      next.col-1),
                                               width= 14, side= "both", pad= " ") # pressure head from last day of previous model run

  # last obs point 2 cm above Zf
  obs.depth <- Zf - 2
  obs.vector <- c(obs9,obs.depth)

  after.text[2] <- sprintf(
    "%s %s %s %s %s %s %s %s %s %s",
    obs.vector[1],obs.vector[2],obs.vector[3],
    obs.vector[4],obs.vector[5],obs.vector[6],
    obs.vector[7],obs.vector[8],obs.vector[9],
    obs.vector[10]
  )
  length(obs.vector)
  # after.text[2] <- paste0(
  #   '    2','  6','  11','  21','  31','  51','  71','  91','  111 ',  obs.depth)

  # write file
  profile.new.file = file("./PROFILE.DAT","w")
  writeLines(before.text,profile.new.file)
  write.table(profile.data,profile.new.file,
              row.names=FALSE,col.names=FALSE,
              sep="\t", quote = FALSE)
  writeLines(after.text,profile.new.file)
  close(profile.new.file)

}

