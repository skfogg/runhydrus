# Function to create PROFILE.DAT file ####
# cat("\014") # clear console
# rm(list=ls()); # remove all objects from workspace
# dev.off() # clear plots (turn off graphing window)
# Sys.setenv(TZ='Utc')  # R don't mess with my date times!!!

# temporary inputs ####
# Zf = 15
# obs9=c(2,6,11,21,31,46,71,91,111)

# create function ####
#' Title
#'
#' @param Zf
#' @param obs9
#' @param material.number
#' @param material.divide
#'
#' @returns
#' @export
#'
#' @examples
create_profile <- function(Zf,obs9,material.number,material.divide){
  header.size = 5 # rows above data
  end.size = 195 # more than necessary
  profile.file = file("./PROFILE.DAT","r")
  before.text = readLines(profile.file,n=header.size)
  profile.data <- readLines(profile.file,n= 176)
  after.text = readLines(profile.file,n=end.size)
  close(profile.file)

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

  # set initial pressure for all depths to default head
  substr(profile.data[1:176],22,36) <- "-1.500000e+002"

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
  return(obs.vector)
}

# obs.vector <- c(2,6,11,21,31,51,71,91,111)

# create_profile(Zf = 15, obs.vector)

# End ####




