#' Create atmospheric boundary conditions input file, including irrigation
#'
#' @param met.data a data.frame of meteorological data, including PET, min, max, mean temps, and precip
#' @param generalized logical indicating if irrigation model has start/end dates based on temperature (TRUE) or if you want to run irrigation with seeding and harvest dates (FALSE), which are supplied in crop.schedule
#' @param crop.schedule
#' @param irrigation.schedule the dates and quantities of irrigation
#' @param modelroots logical. TRUE = grow roots based on GDD model, FALSE = use observed field data supplied in root.info
#' @param root.info
#' @param model.number
#'
#' @returns
#' @export
#'
#' @examples
irrigation.atm <- function(met.data,
                           generalized = TRUE,
                           crop.schedule = NULL,
                           irrigation.schedule,
                           modelroots = TRUE,
                           root.info = NULL,
                           model.number = model.number){

  met.data$Date <- as.POSIXct(met.data$Date)

  #data <- data[, -c(1,10)]

  #Add DOY and year
  met.data$doy <- as.numeric(strftime(met.data$Date, format = "%j"))
  met.data$year <- format(met.data$Date, format= "%Y")

  #If not using specific seeding/harvest dates
  if(generalized == TRUE){

    #find fall crop end date based on max temp being below zero degrees C
    fall.negative <- met.data[met.data$MaxTemp < 0 & met.data$doy > 150 ,]
    fall.end <- aggregate(fall.negative$Date,
                          by= list(fall.negative$year), min)
    colnames(fall.end) <- c('year', "EndDate")

    #find spring start date for gdd and crop emergence
    #spring.positive <- met.data[met.data$MaxTemp < 0 & met.data$doy < 150 ,]

    met.data$max7 <- -999

    for(i in 7:length(met.data$Date))
    {
      met.data[i,'max7'] <- mean(met.data[(i-7):i,'MaxTemp'])
    }
    spr.neg.max <- met.data[met.data$max7 < 0 &
                              met.data$doy < 150,]
    max7.stays.over.zero <- aggregate(spr.neg.max$Date,
                                      by = list(spr.neg.max$year),
                                      max)
    colnames(max7.stays.over.zero) <- c('year', 'Start.Date')
    max7.stays.over.zero$spring.start.doy <-
      as.numeric(strftime(max7.stays.over.zero$spring.start, format = "%j"))

    #Add start and end dates for each year to main weather data frame
    met.data <- merge(met.data, fall.end, by= "year")
    met.data <- merge(met.data, max7.stays.over.zero, by= "year")

    #Add growing degree days (start at zero)
    met.data$gdd <- 0

    for(j in 1:length(unique(met.data$year))){
      start <- as.POSIXct(max7.stays.over.zero$StartDate[j])
      end <- as.POSIXct(fall.end$EndDate[j])

      for(i in 1:length(met.data$gdd)){

        if(met.data$Date[i] > start &
           met.data$Date[i] < end){

          met.data$gdd[i] <- met.data$gdd[i-1] + met.data$AvgTemp[i]

        }
      }
    }

  }

  if(generalized == FALSE){

    #add growing degree days (starting at zero) from seeding date to harvest
    met.data$gdd <- 0
    met.data$StartDate <- as.POSIXct("2020-01-01")
    met.data$EndDate <- as.POSIXct("2020-01-01")

    #assign seeding and harvest dates from each year to a specific weather year

    for(i in 1:length(crop.schedule$Year)){

      year <- crop.schedule$Year[i]

      if(year %in% c(unique(met.data$year)) == TRUE){

        StartDate <- as.POSIXct(crop.schedule$Seeding[i], format= "%Y-%m-%d")
        EndDate <- as.POSIXct(crop.schedule$Harvest[i], format= "%Y-%m-%d")

        met.data[met.data$year == year, "StartDate"] <- StartDate
        met.data[met.data$year == year, "EndDate"] <- EndDate
      }

    }

    #calculate cumulative gdd for growing season
    for(i in 1:length(met.data$gdd)){

      start <- as.POSIXct(met.data$StartDate)[i]
      end <- as.POSIXct(met.data$EndDate)[i]

      if(met.data$Date[i] > start &
         met.data$Date[i] <= end){

        met.data$gdd[i] <- met.data$gdd[i-1] + met.data$AvgTemp[i]

      }

    }


  }

  #Add color for precipitation for future plotting
  met.data$col <- "blue"

  #Add irrigation events
  irrigation.schedule$Date <- as.POSIXct(irrigation.schedule$Date, format= "%Y-%m-%d")

  for(i in 1:length(irrigation.schedule$Date)){

    date.check <- irrigation.schedule$Date[i]

    for(j in 1:length(met.data$doy)){

      if(met.data$Date[j] == date.check){

        #add irrigation to daily precipitation, designate color for these days for future plotting
        met.data$Precip[j] <- met.data$Precip[j] + (irrigation.schedule$Amount_in[i]*2.54)
        met.data$col[j] <- "darkorange2"
      }

    }

  }

  #Grow roots
  #By model if using modelroots=TRUE
  if(modelroots == TRUE){

    years <- unique(crop.schedule$Year)

    #Start root depth at zero before seeding/model start date
    met.data$root.depth_cm <- 0
    met.data$StartDate <- as.POSIXct(met.data$StartDate)
    met.data$EndDate <- as.POSIXct(met.data$EndDate)

    for(j in 1:length(years)){

      year <- years[j]

      if(year %in% c(unique(met.data$year)) == TRUE){

        crop <- crop.schedule$Crop[j]

        #Add an amount of root growth scaled based on daily temperature if date is during growing season
        #and root depth is below maximum possible for the crop
        #if roots are at max depth, maintain for rest of season (don't add)
        #if after harvest/end date, return root depth to zero
        for(i in 2:length(met.data$Date)){
          if(met.data[i,'AvgTemp'] > 0 &
             met.data[i, 'year']== years[j] &
             met.data[i-1,'root.depth_cm'] < crop_growth_param[crop_growth_param$crop == crop,'root.depth.max'] &
             met.data[i,'doy'] > as.numeric(format(met.data[i,"StartDate"], "%j"))
          ){
            met.data[i,"root.depth_cm"] <- met.data[i - 1, 'root.depth_cm'] + met.data[i,"AvgTemp"]*0.0665
          }

          if(met.data[i,'AvgTemp'] > 0 &
             met.data[i, 'year']== years[j] &
             met.data[i-1,'root.depth_cm'] >= crop_growth_param[crop_growth_param$crop == crop,'root.depth.max'] &
             met.data[i,'doy'] > as.numeric(format(met.data[i,"StartDate"], "%j"))
          ){

            met.data[i,"root.depth_cm"] <- met.data[i - 1, 'root.depth_cm']
          }

          if(met.data[i,'doy'] > as.numeric(format(met.data[i,"EndDate"], "%j"))){
            met.data[i, "root.depth_cm"] <- 0
          }


        }

      }

    }

  }


  #Grow roots by field observation
  if(modelroots == FALSE){

    #start root depth at zero
    met.data$root.depth_cm <- 0
    root.info$Date <- as.POSIXct(root.info$Date, format= "%Y-%m-%d")

    #Calculate the root growth rate between 2 field visits based on observed depths
    root.info$yr <- format(root.info$Date, format= "%Y")

    years <- unique(root.info$yr)

    for(k in 1:length(years)){

      root.mini <- root.info[root.info$yr == years[k] ,]
      harvest <- as.POSIXct(crop.schedule[crop.schedule$Year == years[k], "Harvest" ],
                            format= "%Y-%m-%d")

      for(j in 2:length(root.mini$Date)){

        date <- root.mini$Date[j]
        lastdate <- root.mini$Date[j-1]
        growthrate <- (root.mini$Depth[j]-root.mini$Depth[j-1])/as.numeric(difftime(time1= date, time2= lastdate,
                                                                                    units = "days"))
        final <- root.mini[which(root.mini$Depth == max(root.mini$Depth)), "Date"]

        #Add length to roots based on observed growth rate
        for(i in 2:length(met.data$Date)){

          if(met.data$Date[i] <= date
             & met.data$Date[i] > lastdate
             & met.data$Date[i] < final[1]){

            met.data$root.depth_cm[i] <- met.data$root.depth[i-1]+growthrate

          }

          #hold at max observed depth
          if(met.data$Date[i] >= final[1]
             & met.data$Date[i] < harvest){
            met.data$root.depth_cm[i] <- met.data$root.depth_cm[i-1]
          }

          #until harvest
          if(met.data$Date[i] >= harvest){
            met.data$root.depth_cm[i] <- 0
          }
        }

      }
    }




  }

  #Calculate crop coefficients

  #Set initial transpiration coefficient to zero (no transpiration pre-crop)

  years <- unique(crop.schedule$Year)

  for(j in 1:length(years)){

    crop <- crop.schedule$Crop[j]

    met.data$t_coef <- 0

    max.coef <- crop_growth_param[crop_growth_param$crop == crop, "max.coef"]

    for(i in 1:length(met.data$Date))
    {
      # add coefficient for crop emergence to canopy close based on growing degree days
      if(met.data[i, 'gdd'] > crop_growth_param[crop_growth_param$crop == crop,'gdd2emergence'] &
         met.data[i, 'gdd'] <= crop_growth_param[crop_growth_param$crop == crop,'gdd2close'])
      {
        met.data[i, 't_coef'] <- (met.data[i,"gdd"] - crop_growth_param[crop_growth_param$crop == crop,'gdd2emergence'])*
          max.coef/(crop_growth_param[crop_growth_param$crop == crop,'gdd2close'] -
                      crop_growth_param[crop_growth_param$crop == crop,'gdd2emergence'])
      }
      # add coefficient for canopy close to crop maturity
      if(met.data[i, 'gdd'] > crop_growth_param[crop_growth_param$crop == crop,'gdd2close'] &
         met.data[i, 'gdd'] <= crop_growth_param[crop_growth_param$crop == crop,'gdd2mature'])
      {
        met.data[i, 't_coef'] <- max.coef
      }
      # add crop coefficient for mature to harvest
      if(met.data[i, 'gdd'] > crop_growth_param[crop_growth_param$crop == crop,'gdd2mature'] &
         met.data[i, 'gdd'] <= crop_growth_param[crop_growth_param$crop == crop,'gdd2harvest'])
      {
        met.data[i, 't_coef'] <- max.coef -
          (met.data[i,"gdd"] - crop_growth_param[crop_growth_param$crop == crop,'gdd2mature'])/
          (crop_growth_param[crop_growth_param$crop == crop,'gdd2harvest'] -
             crop_growth_param[crop_growth_param$crop == crop,'gdd2mature'])
      }

    }
  }

  #Scale all with crop coefficient relative to reference crop
  #data$t_coef <- data$t_coef*crop_growth_param[crop_growth_param$crop == 'S.Grain', 'max.coef']

  #Calculate evaporation coefficient
  met.data$e_coef <- 0

  #Assume evaporation is only significant before canopy closure,
  #Scale evaporation coefficient from fallow years based on crop growth early in the season
  met.data[met.data$t_coef <=
             crop_growth_param[crop_growth_param$crop == 'Fallow',
                               'max.coef'],
           "e_coef"] <-
    crop_growth_param[crop_growth_param$crop == 'Fallow',
                      'max.coef'] - met.data[met.data$t_coef <=
                                               crop_growth_param[crop_growth_param$crop == 'Fallow',
                                                                 'max.coef'], "t_coef"]
  #Set evaporation to zero after canopy closure
  met.data[met.data$t_coef >
             crop_growth_param[crop_growth_param$crop == 'Fallow',
                               'max.coef'], "e_coef"] <- 0

  # calculate potential evaporation and potential transpiration with rPET and coefs
  met.data$pot_e_cm <- met.data$ET_cm * met.data$e_coef
  met.data$pot_t_cm <- met.data$ET_cm * met.data$t_coef


  # WW grow the roots ####
  # for(i in 2:length(met$date))
  # {
  #   if(met[i,'crop_name'] == 'W.Wheat')
  #   {
  #     if(met[i,'mean.temp_c'] >0 &
  #        met[i-1,'root.depth_cm'] < crop_growth_param[crop_growth_param$crop == 'W.Wheat','root.depth.max'] &
  #        met[i,'doy'] > met[i,"spring.start.doy"]
  #     )
  #     {
  #       met[i,"root.depth_cm"] <- met[i - 1, 'root.depth_cm'] + met[i,"mean.temp_c"]*root.rate
  #     } else met[i,'root.depth_cm'] <- met[i - 1, 'root.depth_cm']
  #   }
  # }
  #
  # # WW december 31 to zero ####
  # for(i in 2:length(met$date))
  # {
  #   if(met[i,'crop_name'] == 'W.Wheat' & met[i,'doy'] %in% c(365,366))
  #   {
  #     met[i,"root.depth_cm"] <- 0
  #   }
  # }

  # WW years
  # for(i in 2:length(met$date))
  # {
  #   if(met[i,'doy'] < 364)
  #   {
  #     if(met[i, 'crop_name'] == 'W.Wheat' &
  #        met[i, 'mean.temp_c'] > 0 &
  #        met[i, 'doy'] > met[i, "spring.start.doy"])
  #     {
  #       met[i,'gdd.cum'] <-
  #         met[i-1, "gdd.cum"] + met[i, "mean.temp_c"]
  #     } else met[i,"gdd.cum"] <- met[i-1,"gdd.cum"]
  #   }
  # }
  #
  # calc ww crop coef ####
  # met$t_coef <- 0
  # for(i in 1:length(met$date))
  # {
  #   if(met[i, 'crop_name'] == 'W.Wheat')
  #   {
  #     # emergence to close
  #     if(met[i, 'gdd.cum'] > crop_growth_param[crop_growth_param$crop == 'W.Wheat','gdd2emergence'] &
  #        met[i, 'gdd.cum'] <= crop_growth_param[crop_growth_param$crop == 'W.Wheat','gdd2close'])
  #     {
  #       met[i, 't_coef'] <- (met[i,"gdd.cum"] - crop_growth_param[crop_growth_param$crop == 'W.Wheat','gdd2emergence'])*
  #         1/(crop_growth_param[crop_growth_param$crop == 'W.Wheat','gdd2close'] -
  #              crop_growth_param[crop_growth_param$crop == 'W.Wheat','gdd2emergence'])
  #     }
  #     # close to mature
  #     if(met[i, 'gdd.cum'] > crop_growth_param[crop_growth_param$crop == 'W.Wheat','gdd2close'] &
  #        met[i, 'gdd.cum'] <= crop_growth_param[crop_growth_param$crop == 'W.Wheat','gdd2mature'])
  #     {
  #       met[i, 't_coef'] <- 1
  #     }
  #     # mature to harvest
  #     if(met[i, 'gdd.cum'] > crop_growth_param[crop_growth_param$crop == 'W.Wheat','gdd2mature'] &
  #        met[i, 'gdd.cum'] <= crop_growth_param[crop_growth_param$crop == 'W.Wheat','gdd2harvest'])
  #     {
  #       met[i, 't_coef'] <- 1 -
  #         (met[i,"gdd.cum"] - crop_growth_param[crop_growth_param$crop == 'W.Wheat','gdd2mature'])/
  #         (crop_growth_param[crop_growth_param$crop == 'W.Wheat','gdd2harvest'] -
  #            crop_growth_param[crop_growth_param$crop == 'W.Wheat','gdd2mature'])
  #     }
  #   }
  # }
  #
  #Generate input text file
  data.row.count = length(met.data$Date)
  #read in template
  atm.file = file(paste0("./ATMOSPH_HeadFoot.IN"),"r") # r makes a sequential read start where the previous ended
  before.text = readLines(atm.file,n=9)
  before.text[4] <- paste0('   ',data.row.count)
  atm.data <- rep("hi",data.row.count)
  after.text = readLines(atm.file,n=2)

  close(atm.file)

  #Fill precipitation, PE, PT, and root depth as time variable conditions

  if(model.number != 9){

    for(i in 1:length(met.data$Date)) {
      atm.data[i] <- sprintf("%.f %.2f %.4f %.4f %.f %.f %.f %.f %.f",
                             i,
                             met.data$Precip[i],
                             met.data$pot_e_cm[i],
                             met.data$pot_t_cm[i],
                             100000,
                             0,
                             0,
                             0,
                             met.data$root.depth_cm[i]
      )
    }
  }

  if(model.number == 9){

    for(i in 1:length(met.data$Date)) {
      atm.data[i] <- sprintf("%.f %.2f %.4f %.4f %.f %.f %.f %.f",
                             i,
                             met.data$Precip[i],
                             met.data$pot_e_cm[i],
                             met.data$pot_t_cm[i],
                             100000,
                             0,
                             0,
                             0
      )
    }
  }
  #Save atmosphere input text file
  atm.new.file = file("ATMOSPH.IN", "w")
  writeLines(before.text,atm.new.file)
  write.table(atm.data, atm.new.file,
              row.names = FALSE, col.names=FALSE,
              sep="\t", quote = FALSE)
  writeLines(after.text,atm.new.file)
  close(atm.new.file)

  return(met.data)

}

