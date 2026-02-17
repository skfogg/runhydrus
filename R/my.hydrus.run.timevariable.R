# Create the Run Hydrus Function ####
#' Title
#'
#' @param met.data dataframe(date, precip, pet, min temp, max temp)
#' @param model.start1 date model 1 starts (must be within met.data date range)
#' @param model.end1 date model ends 1 (must be within met.data date range)
#' @param model.start2 date additional model(s) start
#' @param model.end2 date additional model(s) end
#' @param vgs list of 18 van genuchten parameters; named: thetar,thetas,alpha,n,ksat,l,thetar2,thetas2,alpha2,n2,omega
#' @param SS.name name of scenario set (unique combo of uncertainty paramters, VG etc.)
#' @param obs9 data frame with plotting info for observation nodes
#' @param modelroots True if using roots grown by growing degree days
#' @param root.info Observed root depths if using modelroots=FALSE
#' @param generalized True if using season start/end dates based on temperature
#' @param crop.schedule Seeding and harvest dates if generalized=FALSE
#' @param irrigation.schedule Dates and quantities for irrigation
#' @param model.number 7= dual porosity, 0= single porosity, 9= dual permeability
#' @param time.variable TRUE if using multiple vg parameters over time
#' @param timestep.short TRUE if using atm function that applies irrigation over short time
#' @param irrigation.time.mins
#' @param tv.info.all
#' @param material.number
#' @param material.divide minutes pivot is over spot if using timestep.short
#'
#' @returns
#' @export
#'
#' @examples
my.hydrus.run.timevariable <- function(
    met.data,
    model.start1,
    model.end1,
    model.start2 = NULL,
    model.end2 = NULL,
    vgs,
    SS.name,
    obs9 = obs$depth,
    modelroots= TRUE,
    root.info= NULL,
    generalized= TRUE,
    crop.schedule= NULL,
    irrigation.schedule,
    model.number,
    time.variable,
    timestep.short= FALSE,
    irrigation.time.mins=NULL,
    tv.info.all= NULL,
    material.number,
    material.divide)

# manual variable definitions for troubleshooting function code
# met.data = power.long # dataframe(date, precip, pet)
# model.start1 = as.POSIXct('2021-01-01') # date model starts (must be within met.data date range)
# model.end1 = as.POSIXct('2023-12-31') # date model ends (must be within met.data date range)
# model.start2 = as.POSIXct('2024-01-01') # date model starts (must be within met.data date range)
# model.end2 = as.POSIXct('2024-10-31') # date model ends (must be within met.data date range
# vg = textures[1,c(4:14)] # vector of 6 van genuchten parameters; named: ThetaR,ThetaS,Alpha,n,Ksat,l
# SS.name = "test" # name of scenario set (unique combo of uncertainty paramters, VG etc.)
# obs9 = c(2,6,11,21,31,46,71,91,111)
# model.number= 7
# modelroots= FALSE
# root.info= root.obs
# generalized= FALSE
# crop.schedule= crop.info
# irrigation.schedule= mitch.4yr
{

  # met.data$precip_mm <- met.data$precip_mm/10 # try keeping it in mm

  # create Atmopsh.in ####
  #met.data <- met.data[, c(2:7)]

  #below code is commented out if running many scenarios with same atmosphere inputs

  if(timestep.short == TRUE){

    met.data <- irrigation.timestep.atm(met = met.data,
                                        generalized = generalized,
                                        crop.schedule = crop.schedule,
                                        irrigation.schedule = irrigation.schedule,
                                        modelroots= modelroots,
                                        root.info = root.info,
                                        irrigation.time.mins = irrigation.time.mins)

  }

  if(timestep.short == FALSE){

    met.data <- irrigation.atm(met = met.data,
                               generalized = generalized,
                               crop.schedule = crop.schedule,
                               irrigation.schedule = irrigation.schedule,
                               modelroots= modelroots,
                               root.info = root.info,
                               model.number = model.number)

  }


  met.data1 <- met.data[met.data$Date >= model.start1 &
                          met.data$Date <= model.end1 ,]

  atmall <- file("./ATMOSPH.IN", "r")
  intro <- readLines(atmall, n=9)
  atm1 <- readLines(atmall, n=length(met.data1$Date))

  atm2 <- readLines(atmall, n=(length(met.data$Date)-length(met.data1$Date)))
  end <- readLines(atmall)

  intro.1 <- intro
  intro.1[4] <- str_pad(length(met.data1$Date),
                        width= 6, side= "left")

  close(atmall)

  atm.new.file = file("ATMOSPH.IN", "w")
  writeLines(intro.1,atm.new.file)
  write.table(atm1, atm.new.file,
              row.names = FALSE, col.names=FALSE,
              sep="\t", quote = FALSE)
  writeLines(end,atm.new.file)
  close(atm.new.file)

  # Create PROFILE.DAT file ####
  obs.vector1 <- create_profile(Zf = Zf,obs9=obs$depth,
                                material.number = material.number,
                                material.divide = material.divide)

  # Create Selector/Run Function ####
  ## KF: broke out into own function

  # Create SELECTOR.IN and Run Hydrus ####
  # loop through time.int vector to get convergence
  day.check <- 0 # start with zero before going through time.int

  # i = 1
  for(i in 1:length(time.int.vector))  # 2024-11-16 looping until model converges
  {
    list.temp <- CreateSelectorAndRun(met.data1 = met.data1,
                                      vg = vgs[[1]],
                                      my.time.int = time.int.vector[i],
                                      Zf = Zf,
                                      # rotation_name = rotation_name, # omit for now (2024-10-08)
                                      SS.name = SS.name,
                                      model.number= model.number,
                                      tv.info = tv.info.all[1,])
    my.time.int <- list.temp$my.time.int
    day.check <- list.temp$day.check
    my.filename <-  list.temp$my.filename
  }

  # H1D Results Merge w Met
  # if(length(day.check)==length(met.data$date)+10) # edit omit to make work 2024-10-08
  if(timestep.short == FALSE){

    if(model.number != 9){

      if(length(day.check)==length(met.data1$Date)+10)
      {
        T_Level = read.table(file = "./T_Level.out",
                             header = FALSE, skip = 9, nrows = length(met.data1$Date))
        T_Level_headings = read.table(file = "./T_Level.out",
                                      header = FALSE, skip = 6, nrows = 1)
        h1d_daily <- T_Level[,c(1,5,6,15,19)]
        colnames(h1d_daily) <- c("h1d_day", "h1d_t_mm",
                                 "h1d_dp_mm", "h1d_runoff_mm",
                                 "h1d_e_cum_mm")
        h1d_daily$h1d_e_mm <- -999
        h1d_daily$h1d_e_mm[1] <- h1d_daily$h1d_e_cum_mm[1]
        for(i in 2:length(h1d_daily$h1d_e_mm))
        {
          h1d_daily$h1d_e_mm[i] <- h1d_daily[i,"h1d_e_cum_mm"]- h1d_daily[i-1,"h1d_e_cum_mm"]
        }
        h1d_daily <- h1d_daily[,-5]
        h1d_daily$Date <- as.POSIXct("1950-01-01")
        h1d_daily$Date[1:length(h1d_daily$Date)] <- seq(model.start1, model.end1, by="days")

        met.data1$Date <- as.POSIXct(met.data1$Date) # 2024-11-16 added
        results_daily <- merge(met.data1, h1d_daily, by = 'Date') # 2024-11-16 added by date

        results_daily$month <- month(floor_date(results_daily$Date, 'month'))
        results_daily$year <- year(floor_date(results_daily$Date, 'year'))

        #### commented out 2024-11-16
        # results_daily$crop_yr <- results_daily$year
        # results_daily[results_daily$month %in% c(1,2,3), "crop_yr"] <-
        #   results_daily[results_daily$month %in% c(1,2,3), "year"] - 1
        # crop_yr__crop_years <- crop_years
        # colnames(crop_yr__crop_years)
        # colnames(crop_yr__crop_years)  <- c("crop_yr","crop_yr__crop_name",
        #                                     "crop_yr__c.vs.f","crop_yr__last_crop")
        # results_daily <- merge(results_daily, crop_yr__crop_years)
        #
        # nitrate$crop_yr__c.vs.f <- nitrate$c.vs.f # simply for merge colname matching
        # results_daily <- merge(results_daily, nitrate, by="crop_yr__c.vs.f")
        # results_daily <- results_daily[with(results_daily,order(date)),]
        # # results_daily[85:110,]
        # results_daily$leachN_kg.ha.day <- results_daily$h1d_dp_mm * results_daily$no3n /10
        # results_daily$same <- "same"

        # Observation Point VWC ####
        obs.daily = read.table(file = "./Obs_Node.out",
                               header = FALSE, skip = 11, nrows = length(met.data1$Date))
        obs.headings = read.table(file = "./Obs_Node.out",
                                  header = FALSE, skip = 10, nrows = 1,
                                  stringsAsFactors = FALSE)
        obs.headings <- as.character(obs.headings[1,])
        obs.headings <- paste0(c('',rep("obs_01_",3),
                                 rep('obs_02_',3),
                                 rep('obs_03_',3),
                                 rep('obs_04_',3),
                                 rep('obs_05_',3),
                                 rep('obs_06_',3),
                                 rep('obs_07_',3),
                                 rep('obs_08_',3),
                                 rep('obs_09_',3),
                                 rep('obs_10_',3)),
                               obs.headings)
        colnames(obs.daily) <- obs.headings
        obs.daily$Date <- as.POSIXct("1950-01-01")
        obs.daily$Date[1:length(obs.daily$Date)] <- seq(model.start1, model.end1, by="days")
        results_daily_1 <- merge(results_daily, obs.daily)

        ##### save daily output 1 #####
        write.csv(x = results_daily_1, file = paste0('../../4_Results/2csvs/', SS.name, '1.csv'))

        results_daily_all <- results_daily_1

        if(time.variable == TRUE){

          for(j in 1:length(model.start2)){

            #Create Atmosphere 2 ####
            met.data.2.all <- met.data[met.data$Date >= model.start2[1] ,]

            met.data2 <- met.data[met.data$Date >= model.start2[j] &
                                    met.data$Date <= model.end2[j] ,]

            intro.2 <- intro
            intro.2[4] <- str_pad(length(met.data2$Date),
                                  width= 6, side= "left")

            start2 <- which(met.data.2.all$Date == model.start2[j])
            end2 <- which(met.data.2.all$Date == model.end2[j])

            atm.temp.2 <- atm2[start2:end2]

            padwidth <- unlist(gregexpr("\\d \\d", atm.temp.2[length(atm.temp.2)] ))[1]

            substr(atm.temp.2[1:length(atm.temp.2)], 1, padwidth) <- str_pad(seq(1:length(atm.temp.2)),
                                                                             width= padwidth, pad= " ",
                                                                             side= "right")

            atm.new.file = file("ATMOSPH.IN", "w")
            writeLines(intro.2,atm.new.file)
            write.table(atm.temp.2, atm.new.file,
                        row.names = FALSE, col.names=FALSE,
                        sep="\t", quote = FALSE)
            writeLines(end,atm.new.file)
            close(atm.new.file)

            # Create PROFILE.DAT file ####
            obs.vector2 <- create_profile_modelinputs(Zf = Zf,obs9)

            # Create SELECTOR.IN and Run Hydrus ####
            # loop through time.int vector to get convergence
            day.check <- 0 # start with zero before going through time.int

            # i = 1
            for(i in 1:length(time.int.vector))  # 2024-11-16 looping until model converges
            {
              list.temp <- CreateSelectorAndRun(met.data1 = met.data2,
                                                vg = vg[(j+1) ,],
                                                my.time.int = time.int.vector[i],
                                                Zf = Zf,
                                                # rotation_name = rotation_name, # omit for now (2024-10-08)
                                                SS.name = SS.name,
                                                model.number= model.number)
              my.time.int <- list.temp$my.time.int
              day.check <- list.temp$day.check
              my.filename <-  list.temp$my.filename
            }

            # H1D Results Merge w Met
            # if(length(day.check)==length(met.data$date)+10) # edit omit to make work 2024-10-08
            if(length(day.check)==length(met.data2$Date)+10)
            {
              T_Level = read.table(file = "./T_Level.out",
                                   header = FALSE, skip = 9, nrows = length(met.data2$Date))
              T_Level_headings = read.table(file = "./T_Level.out",
                                            header = FALSE, skip = 6, nrows = 1)
              h1d_daily <- T_Level[,c(1,5,6,15,19)]
              colnames(h1d_daily) <- c("h1d_day", "h1d_t_mm",
                                       "h1d_dp_mm", "h1d_runoff_mm",
                                       "h1d_e_cum_mm")
              h1d_daily$h1d_e_mm <- -999
              h1d_daily$h1d_e_mm[1] <- h1d_daily$h1d_e_cum_mm[1]
              for(i in 2:length(h1d_daily$h1d_e_mm))
              {
                h1d_daily$h1d_e_mm[i] <- h1d_daily[i,"h1d_e_cum_mm"]- h1d_daily[i-1,"h1d_e_cum_mm"]
              }
              h1d_daily <- h1d_daily[,-5]
              h1d_daily$Date <- as.POSIXct("1950-01-01")
              h1d_daily$Date[1:length(h1d_daily$Date)] <- seq(model.start2[j], model.end2[j], by="days")

              met.data2$Date <- as.POSIXct(met.data2$Date) # 2024-11-16 added
              results_daily <- merge(met.data2, h1d_daily, by = 'Date') # 2024-11-16 added by date

              results_daily$month <- month(floor_date(results_daily$Date, 'month'))
              results_daily$year <- year(floor_date(results_daily$Date, 'year'))

              #### commented out 2024-11-16
              # results_daily$crop_yr <- results_daily$year
              # results_daily[results_daily$month %in% c(1,2,3), "crop_yr"] <-
              #   results_daily[results_daily$month %in% c(1,2,3), "year"] - 1
              # crop_yr__crop_years <- crop_years
              # colnames(crop_yr__crop_years)
              # colnames(crop_yr__crop_years)  <- c("crop_yr","crop_yr__crop_name",
              #                                     "crop_yr__c.vs.f","crop_yr__last_crop")
              # results_daily <- merge(results_daily, crop_yr__crop_years)
              #
              # nitrate$crop_yr__c.vs.f <- nitrate$c.vs.f # simply for merge colname matching
              # results_daily <- merge(results_daily, nitrate, by="crop_yr__c.vs.f")
              # results_daily <- results_daily[with(results_daily,order(date)),]
              # # results_daily[85:110,]
              # results_daily$leachN_kg.ha.day <- results_daily$h1d_dp_mm * results_daily$no3n /10
              # results_daily$same <- "same"

              # Observation Point VWC ####
              obs.daily = read.table(file = "./Obs_Node.out",
                                     header = FALSE, skip = 11, nrows = length(met.data2$Date))
              obs.headings = read.table(file = "./Obs_Node.out",
                                        header = FALSE, skip = 10, nrows = 1,
                                        stringsAsFactors = FALSE)
              obs.headings <- as.character(obs.headings[1,])
              obs.headings <- paste0(c('',rep("obs_01_",3),
                                       rep('obs_02_',3),
                                       rep('obs_03_',3),
                                       rep('obs_04_',3),
                                       rep('obs_05_',3),
                                       rep('obs_06_',3),
                                       rep('obs_07_',3),
                                       rep('obs_08_',3),
                                       rep('obs_09_',3),
                                       rep('obs_10_',3)),
                                     obs.headings)
              colnames(obs.daily) <- obs.headings
              obs.daily$Date <- as.POSIXct("1950-01-01")
              obs.daily$Date[1:length(obs.daily$Date)] <- seq(model.start2[j], model.end2[j], by="days")
              results_daily_2 <- merge(results_daily, obs.daily)

              ##### save daily output 2#####
              write.csv(x = results_daily_2, file = paste0('../../4_Results/2csvs/', SS.name, j+1, '.csv'))

              #save data for both
              results_daily_all <- rbind(results_daily_all, results_daily_2)

            }
          }

          write.csv(x= results_daily_all, file = paste0('../../4_Results/2csvs/', SS.name, 'All.csv'))

        }

        #load data (for Meghan's laptop that can't run Hydrus)
        #runs <- list.files('../../4_Results/1csvs')
        #for(i in 1:length(runs))
        #{
        # print(runs[i])
        #results_daily <- read.csv(paste0('../../4_Results/1csvs/', runs[i]))
        #results_daily$date <- as.POSIXct(results_daily$date)
        #}


        #this behavior is what is making plotting weird
        #paste(colnames(textures)[1], colnames(textures)[2]) #makes 1 string
        #paste(colnames(textures)[1:2]) #makes 2 strings
        #need to use collapse argument to paste column names into 1 string

        # plot volumetric water content results ####
        # i = 1

        # windows(1600,800)
        results_daily <- results_daily_all

        png(width = 1600, height = 800, res = 144,
            filename = paste0('../../4_Results/1VWCplots/', SS.name, '.png'))
        plot(results_daily$obs_01_theta ~ results_daily$Date, type = 'n', # no symbology
             main = SS.name, ylab = 'VWC',
             ylim = c(0,0.5))

        for(i in 1:9){
          if(obs[i,'plot.yn'] == 'y') {
            points(results_daily[,obs[i,'df.name']] ~ results_daily$Date,
                   pch = obs[i,"pch"], col = obs[i,'col'])
            lines(results_daily[,obs[i,'df.name']] ~ results_daily$Date,
                  pch = obs[i,"pch"], col = obs[i,'col'])
          }
        }
        # points(results_daily$obs_01_theta ~ results_daily$date)

        text(x = as.POSIXct('2021-02-01'), y = 0.3,
             paste0('Obs1= ', min(results_daily$obs_01_theta), " to ",
                    max(results_daily$obs_01_theta)))

        # print VG parameters on plot
        # labels.names <- str_pad(colnames(textures)[3:length(colnames(textures))], width= 12)
        # labels.values <- str_pad(textures[i, 3:length(colnames(textures))],
        #                                                      width= 10)

        # labels.names <- str_pad(colnames(vg), width= 12)
        # labels.values <- str_pad(vg, width= 10)
        #
        # mtext(side = 3, line= 0.8, at= as.POSIXct("2022-07-01"),
        #       text= paste0(labels.names, collapse= " "),
        #       cex= 0.8)
        # mtext(side = 3, line= 0.05, at= as.POSIXct('2022-07-01'),
        #       text= paste0(labels.values, collapse= " ", sep= " "),
        #       cex= 0.8)

        dev.off()


        # calculate cumulative fluxes ####
        # calc cumulative deep perc
        results_daily$dp_cum <- 0
        results_daily[1,'dp_cum'] <- results_daily[1,'h1d_dp_mm']
        for(i in 2:length(results_daily$dp_cum)){
          results_daily[i,'dp_cum'] <- results_daily[i-1,'dp_cum'] +
            results_daily[i,'h1d_dp_mm']
        }

        # calc cumulative runoff
        results_daily$runoff_cum <- 0
        results_daily[1,'runoff_cum'] <- results_daily[1,'h1d_runoff_mm']
        for(i in 2:length(results_daily$runoff_cum)){
          results_daily[i,'runoff_cum'] <- results_daily[i-1,'runoff_cum'] +
            results_daily[i,'h1d_runoff_mm']
        }


        # calc cumulative evaporation
        results_daily$evap_cum <- 0
        results_daily[1,'evap_cum'] <- results_daily[1,'h1d_e_mm']
        for(i in 2:length(results_daily$evap_cum)){
          results_daily[i,'evap_cum'] <- results_daily[i-1,'evap_cum'] +
            results_daily[i,'h1d_e_mm']
        }

        # calc cumulative transpiration
        results_daily$tran_cum <- 0
        results_daily[1,'tran_cum'] <- results_daily[1,'h1d_t_mm']
        for(i in 2:length(results_daily$tran_cum)){
          results_daily[i,'tran_cum'] <- results_daily[i-1,'tran_cum'] +
            results_daily[i,'h1d_t_mm']
        }

        # plot cumulative fluxes ####
        # windows(1600,800)

        png(width = 1600, height = 800, res = 144,
            filename = paste0('../../4_Results/2fluxplots/', SS.name, '.png'))
        cum.min <- min(tail(results_daily[,'dp_cum'],1),
                       tail(results_daily[,'runoff_cum'],1),
                       tail(results_daily[,'evap_cum'],1),
                       tail(results_daily[,'tran_cum'],1))
        cum.max <- max(tail(results_daily[,'dp_cum'],1),
                       tail(results_daily[,'runoff_cum'],1),
                       tail(results_daily[,'evap_cum'],1),
                       tail(results_daily[,'tran_cum'],1))

        plot(results_daily$dp_cum ~ results_daily$Date, type = 'n', # no symbology
             main = SS.name,
             ylim = c(cum.min,cum.max),
             ylab = 'cumualtive partitioning (mm)',
             xlab = 'date')

        # points(results_daily$dp_cum ~ results_daily$date,
        #        col = 'black')
        # points(results_daily$runoff_cum ~ results_daily$date,
        #        col = 'blue')
        # points(results_daily$evap_cum ~ results_daily$date,
        #        col = 'gray30')
        # points(results_daily$tran_cum ~ results_daily$date,
        #        col = 'gray80')
        lines(results_daily$dp_cum ~ results_daily$Date,
              col = 'black', lty = 2)
        lines(results_daily$runoff_cum ~ results_daily$Date,
              col = 'blue')
        lines(results_daily$evap_cum ~ results_daily$Date,
              col = 'gray30')
        lines(results_daily$tran_cum ~ results_daily$Date,
              col = 'green')
        legend('topleft', legend = c('deep perc', 'runoff', 'evap', 'tran'),
               col = c('black', 'blue', 'gray30', 'green'), lty = c(2,1,1,1))

        legend('top', bty= "n",
               legend= c(paste("Cumulative Evaporation=",
                               round(tail(results_daily[,'evap_cum'],1),2)),
                         paste("Cumulative Transpiration=",
                               round(tail(results_daily[,'tran_cum'],1),2)),
                         paste("Cumulative DP=",
                               round(tail(results_daily[,'dp_cum'],1),2)),
                         paste("Cumulative Runoff=",
                               round(tail(results_daily[,'runoff_cum'],1),2))))

        dev.off()

        #Make combined plot ####
        ymax1 <- max(results_daily$Precip, results_daily$ET_cm)
        ymin1 <- min(results_daily$Precip, results_daily$ET_cm)

        date.seq <- seq(model.start1, model.end1, by= "months")
        date.seq <- date.seq[seq(from= 1, to= length(date.seq), by= 3)]

        png(width = 1600, height = 800, res = 144,
            filename = paste0('../../4_Results/2Combinedplots/', SS.name, '.png'))

        layout(matrix(c(1,2,3), ncol= 1))
        par(mar= c(1,4,2,4))
        par(oma=c(0,0,2,0))

        plot(results_daily$Precip ~ results_daily$Date,
             main= paste("Weather and Irrigation"),
             xaxt= "n",
             xlab= "",
             xaxs= "i",
             ylab= "",
             ylim= c(ymax1, ymin1),
             yaxs= "i",
             col.axis= "blue",
             type= "h",
             col= results_daily$col,
             lwd= 2)

        mtext(text= expression(bold("Precip (cm)")),
              side= 2, line=2, cex= 1, col= "blue")

        par(new= TRUE)
        plot(results_daily$ET_cm ~ results_daily$Date,
             xaxt= "n",
             xlab= "",
             xaxs= "i",
             yaxt= "n",
             ylab= "",
             ylim= c(ymin1, ymax1),
             yaxs= "i",
             type= "l",
             lwd= 2)

        abline(v= c(date.seq),
               lty= 2,
               lwd= 2,
               col= "gray75")

        label.list <- seq(0, round(ymax1, 0), 0.5)

        axis(side= 4, labels= c(label.list),
             at= c(label.list))

        mtext(text= expression(bold("ET (cm)")),
              side= 4, line=3, cex= 1)

        mtext(text= SS.name,
              side= 3, line= 1.5, cex= 1.5)

        par(mar= c(1,4,3,4))

        plot(results_daily$obs_01_theta ~ results_daily$Date,
             type = 'n', # no symbology
             main = "",
             ylab = 'VWC',
             xlab= "",
             xaxs= "i",
             xaxt= "n",
             ylim = c(0,0.5))

        title(main = "Soil Moisture", line= 1.5)

        for(i in 1:9){
          if(obs[i,'plot.yn'] == 'y') {
            points(results_daily[,obs[i,'df.name']] ~ results_daily$Date,
                   pch = obs[i,"pch"], col = obs[i,'col'])
            lines(results_daily[,obs[i,'df.name']] ~ results_daily$Date,
                  pch = obs[i,"pch"], col = obs[i,'col'])
          }
        }
        # points(results_daily$obs_01_theta ~ results_daily$date)

        text(x = as.POSIXct(model.start1+(60*60*24*50)), y = 0.2,
             paste0('Obs1= ', min(results_daily$obs_01_theta), " to ",
                    max(results_daily$obs_01_theta)))

        leg.info <- obs[obs$plot.yn == "y" ,]

        legend("bottomleft", legend= c(leg.info$depth),
               pch= c(leg.info$pch), col= c(leg.info$col),
               lwd= 2, cex= 0.8)

        # print VG parameters on plot
        # labels.names <- str_pad(colnames(textures)[3:length(colnames(textures))], width= 12)
        # labels.values <- str_pad(textures[i, 3:length(colnames(textures))],
        #                                                      width= 10)

        if(model.number==0){

          # labels.names <- str_pad(colnames(vg)[1:6], width= 12)
          # labels.values <- str_pad(vg[1:6], width= 10)
          #
        }else{

          labels.names <- str_pad(colnames(vg), width= 12)
          labels.values <- str_pad(vg, width= 10)

        }

        midpoint <- mean(c(model.start1, model.end1))

        # mtext(side = 3, line= 0.65, at= as.POSIXct(midpoint),
        #       text= paste0(labels.names, collapse= " "),
        #       cex= 0.7)
        # mtext(side = 3, line= 0.05, at= as.POSIXct(midpoint),
        #       text= paste0(labels.values, collapse= " ", sep= " "),
        #       cex= 0.7)

        abline(v= c(date.seq),
               lty= 2,
               lwd= 2,
               col= "gray75")

        par(mar= c(3,4,1,4))

        plot(results_daily$dp_cum ~ results_daily$Date,
             type = 'n', # no symbology
             main = "Cumulative Fluxes",
             xlab= "",
             xaxs= "i",
             xaxt= "n",
             ylim = c(cum.min,cum.max),
             ylab = 'cumualtive partitioning (mm)')

        lines(results_daily$dp_cum ~ results_daily$Date,
              col = 'black', lty = 2, lwd= 2)
        lines(results_daily$runoff_cum ~ results_daily$Date,
              col = 'blue', lwd= 2)
        lines(results_daily$evap_cum ~ results_daily$Date,
              col = 'gray30', lwd= 2)
        lines(results_daily$tran_cum ~ results_daily$Date,
              col = 'green', lwd= 2)
        legend('topleft', legend = c('deep perc', 'runoff', 'evap', 'tran'),
               col = c('black', 'blue', 'gray30', 'green'), lty = c(2,1,1,1))

        abline(v= c(date.seq),
               lty= 2,
               lwd= 2,
               col= "gray75")

        legend('top', bty= "n",
               legend= c(paste("Cumulative Evaporation=",
                               round(tail(results_daily[,'evap_cum'],1),2)),
                         paste("Cumulative Transpiration=",
                               round(tail(results_daily[,'tran_cum'],1),2)),
                         paste("Cumulative DP=",
                               round(tail(results_daily[,'dp_cum'],1),2)),
                         paste("Cumulative Runoff=",
                               round(tail(results_daily[,'runoff_cum'],1),2))))

        x.axis.year.month(Xrange= c(model.start1, model.end1),
                          year.cex = 1,
                          month.cex = 0.7,
                          axis.line = 0,
                          year.line = 2,
                          month.line = 0)
        dev.off()


        #Make Flux Direction plot ####
        ymax1 <- max(results_daily$Precip, results_daily$ET_cm)
        ymin1 <- min(results_daily$Precip, results_daily$ET_cm)

        date.seq <- seq(model.start1, model.end1, by= "months")
        date.seq <- date.seq[seq(from= 1, to= length(date.seq), by= 3)]

        png(width = 1600, height = 800, res = 144,
            filename = paste0('../../4_Results/2fluxdirectionplots/', SS.name, '.png'))

        layout(matrix(c(1,2,3,4), ncol= 1))
        par(mar= c(1,4,2,4))
        par(oma=c(0,0,2,0))

        plot(results_daily$Precip ~ results_daily$Date,
             main= paste("Weather and Irrigation"),
             xaxt= "n",
             xlab= "",
             xaxs= "i",
             ylab= "",
             ylim= c(ymax1, ymin1),
             yaxs= "i",
             col.axis= "blue",
             type= "h",
             col= results_daily$col,
             lwd= 2)

        mtext(text= expression(bold("Precip (cm)")),
              side= 2, line=2, cex= 1, col= "blue")

        par(new= TRUE)
        plot(results_daily$ET_cm ~ results_daily$Date,
             xaxt= "n",
             xlab= "",
             xaxs= "i",
             yaxt= "n",
             ylab= "",
             ylim= c(ymin1, ymax1),
             yaxs= "i",
             type= "l",
             lwd= 2)

        abline(v= c(date.seq),
               lty= 2,
               lwd= 2,
               col= "gray75")

        label.list <- seq(0, round(ymax1, 0), 0.5)

        axis(side= 4, labels= c(label.list),
             at= c(label.list))

        mtext(text= expression(bold("ET (cm)")),
              side= 4, line=3, cex= 1)

        mtext(text= SS.name,
              side= 3, line= 1.5, cex= 1.5)

        par(mar= c(1,4,3,4))

        plot(results_daily$obs_03_theta ~ results_daily$Date,
             main= "15 cm Modeled Soil Moisture and Flux",
             xlab= "",
             xaxs= "i",
             ylab= "VWC",
             ylim= c(0, 0.5),
             yaxs= "i",
             type= "l",
             lwd= 1,
             col= "blue")

        par(new=TRUE)

        plot(results_daily$obs_03_Flux ~ results_daily$Date,
             main= "15 cm Modeled Soil Moisture and Flux",
             xlab= "",
             xaxs= "i",
             yaxt= "n",
             ylab= "",
             #ylim= c(0, 0.5),
             type= "l",
             lwd= 1,
             col= "black")

        abline(v= c(date.seq),
               lty= 2,
               lwd= 2,
               col= "gray75")

        abline(h= 0, lty=2, lwd= 2)

        label.list <- seq(round(min(results_daily$obs_03_Flux), 3),
                          round(max(results_daily$obs_03_Flux), 3), 0.1)

        axis(side= 4, labels= c(label.list),
             at= c(label.list))

        mtext(text= "Flux",
              side= 4, line=3, cex= 1)

        plot(results_daily$obs_08_theta ~ results_daily$Date,
             main= "91 cm Modeled Soil Moisture and Flux",
             xlab= "",
             xaxs= "i",
             ylab= "VWC",
             ylim= c(0, 0.5),
             yaxs= "i",
             type= "l",
             lwd= 1,
             col= "blue")

        par(new=TRUE)

        plot(results_daily$obs_08_Flux ~ results_daily$Date,
             main= "91 cm Modeled Soil Moisture and Flux",
             xlab= "",
             xaxs= "i",
             yaxt= "n",
             ylab= "",
             #ylim= c(0, 0.5),
             type= "l",
             lwd= 1,
             col= "black")

        abline(v= c(date.seq),
               lty= 2,
               lwd= 2,
               col= "gray75")

        abline(h= 0, lty=2, lwd= 2)

        label.list <- seq(round(min(results_daily$obs_08_Flux), 3),
                          round(max(results_daily$obs_08_Flux), 3), 0.1)

        axis(side= 4, labels= c(label.list),
             at= c(label.list))

        mtext(text= "Flux",
              side= 4, line=3, cex= 1)

        plot(results_daily$obs_10_theta ~ results_daily$Date,
             main= "148 cm Modeled Soil Moisture and Flux",
             xlab= "",
             xaxs= "i",
             ylab= "VWC",
             ylim= c(0, 0.5),
             yaxs= "i",
             type= "l",
             lwd= 1,
             col= "blue")

        par(new=TRUE)

        plot(results_daily$obs_10_Flux ~ results_daily$Date,
             main= "148 cm Modeled Soil Moisture and Flux",
             xlab= "",
             xaxs= "i",
             yaxt= "n",
             ylab= "",
             #ylim= c(0, 0.5),
             type= "l",
             lwd= 1,
             col= "black")

        abline(h= 0, lty=2, lwd= 2)

        abline(v= c(date.seq),
               lty= 2,
               lwd= 2,
               col= "gray75")

        legend("bottomleft", legend= c("Soil Moisture", "Flux"),
               lty= c(1,1), col= c("blue", "black"))

        label.list <- seq(round(min(results_daily$obs_10_Flux), 3),
                          round(max(results_daily$obs_10_Flux), 3), 0.1)

        axis(side= 4, labels= c(label.list),
             at= c(label.list))

        mtext(text= "Flux",
              side= 4, line=3, cex= 1)

        dev.off()

        # simplify for now be omitting the following list output (2024-10-08)
        #   metadata <- list(vg = vg, crop_growth_param = crop_growth_param,
        #                    nitrate = nitrate, time.int = my.time.int,
        #                    obs.vector = obs.vector)
        #   mydata <- list(daily = results_daily, metadata = metadata)
        #   save(mydata, file = paste0("../../4_Results/1_RawDaily/",
        #                              out.subfolder.name, "/", my.filename,".RData"))
        #   return(list(daily = results_daily))
        # } else
        # metadata <- list(vg = vg, crop_growth_param = crop_growth_param,
        #                      nitrate = nitrate, time.int = my.time.int,
        #                  obs.vector = obs.vector)
        # mydata <- list(daily = "fail", metadata = metadata)
        # save(x =mydata, file = paste0("../../4_Results/1_RawDaily/",
        #                           out.subfolder.name, "/", my.filename,"FAIL",".RData"))
      } else
      {
        write.csv(x = 1:3, file = paste0('../../4_Results/2fails/', SS.name, 'FAIL.csv'))
      }
    }

    if(model.number == 9){

      if(length(day.check)==length(met.data1$Date)+10)
      {
        T_Level = read.table(file = "./T_Level.out",
                             header = FALSE, skip = 9, nrows = length(met.data1$Date))
        T_Level_headings = read.table(file = "./T_Level.out",
                                      header = FALSE, skip = 6, nrows = 1)

        h1d_daily <- T_Level[,c(1,4,5,6,15,17,21,23)]
        colnames(h1d_daily) <- c("h1d_day", "h1d_surfaceflux_cm",
                                 "h1d_t_cm",
                                 "h1d_dp_cm",
                                 "h1d_macro_surface_cm",
                                 "h1d_matrix_surface_cm", "h1d_macro_dp_cm",
                                 "h1d_matrix_dp_cm")


        #h1d_daily <- h1d_daily[,-5]
        h1d_daily$Date <- as.POSIXct("1950-01-01")
        h1d_daily$Date[1:length(h1d_daily$Date)] <- seq(model.start1, model.end1, by="days")

        met.data1$Date <- as.POSIXct(met.data1$Date) # 2024-11-16 added
        results_daily <- merge(met.data1, h1d_daily, by = 'Date') # 2024-11-16 added by date

        results_daily$month <- month(floor_date(results_daily$Date, 'month'))
        results_daily$year <- year(floor_date(results_daily$Date, 'year'))

        results_daily$h1d_e_cm <- results_daily$h1d_surfaceflux_cm

        for(e in 1:length(results_daily$Precip)){

          if(results_daily$Precip[e] > 0){

            results_daily$h1d_e_cm[e] <- results_daily$Precip[e] + results_daily$h1d_surfaceflux_cm[e]

          }
        }


        results_daily$h1d_e_cm[results_daily$h1d_e_cm < 0.001] <- 0

        #### commented out 2024-11-16
        # results_daily$crop_yr <- results_daily$year
        # results_daily[results_daily$month %in% c(1,2,3), "crop_yr"] <-
        #   results_daily[results_daily$month %in% c(1,2,3), "year"] - 1
        # crop_yr__crop_years <- crop_years
        # colnames(crop_yr__crop_years)
        # colnames(crop_yr__crop_years)  <- c("crop_yr","crop_yr__crop_name",
        #                                     "crop_yr__c.vs.f","crop_yr__last_crop")
        # results_daily <- merge(results_daily, crop_yr__crop_years)
        #
        # nitrate$crop_yr__c.vs.f <- nitrate$c.vs.f # simply for merge colname matching
        # results_daily <- merge(results_daily, nitrate, by="crop_yr__c.vs.f")
        # results_daily <- results_daily[with(results_daily,order(date)),]
        # # results_daily[85:110,]
        # results_daily$leachN_kg.ha.day <- results_daily$h1d_dp_mm * results_daily$no3n /10
        # results_daily$same <- "same"

        # Observation Point VWC ####
        obs.daily = read.table(file = "./Obs_Node.out",
                               header = FALSE, skip = 11, nrows = length(met.data1$Date))
        obs.headings = read.table(file = "./Obs_Node.out",
                                  header = FALSE, skip = 10, nrows = 1,
                                  stringsAsFactors = FALSE)
        obs.headings <- as.character(obs.headings[1,])
        obs.headings <- paste0(c('',rep("obs_01_",3),
                                 rep('obs_02_',3),
                                 rep('obs_03_',3),
                                 rep('obs_04_',3),
                                 rep('obs_05_',3),
                                 rep('obs_06_',3),
                                 rep('obs_07_',3),
                                 rep('obs_08_',3),
                                 rep('obs_09_',3),
                                 rep('obs_10_',3)),
                               obs.headings)
        colnames(obs.daily) <- obs.headings
        obs.daily$Date <- as.POSIXct("1950-01-01")
        obs.daily$Date[1:length(obs.daily$Date)] <- seq(model.start1, model.end1, by="days")

        obs.daily.f = read.table(file = "./Obs_NodF.out",
                                 header = FALSE, skip = 11, nrows = length(met.data1$Date))
        obs.headings.f = read.table(file = "./Obs_NodF.out",
                                    header = FALSE, skip = 10, nrows = 1,
                                    stringsAsFactors = FALSE)
        obs.headings.f <- as.character(obs.headings.f[1,])
        obs.headings.f <- paste0(c('',rep("obs_01_macro",3),
                                   rep('obs_02_macro',3),
                                   rep('obs_03_macro',3),
                                   rep('obs_04_macro',3),
                                   rep('obs_05_macro',3),
                                   rep('obs_06_macro',3),
                                   rep('obs_07_macro',3),
                                   rep('obs_08_macro',3),
                                   rep('obs_09_macro',3),
                                   rep('obs_10_macro',3)),
                                 obs.headings.f)
        colnames(obs.daily.f) <- obs.headings.f
        obs.daily.f$date <- as.POSIXct("1950-01-01")
        obs.daily.f$date[1:length(obs.daily.f$date)] <- seq(model.start1, model.end1, by="days")
        obs.daily.f <- obs.daily.f[, c(32, 1:31)]

        results_daily <- merge(results_daily, obs.daily)
        results_daily <- merge(results_daily, obs.daily.f)

        results_daily$obs_01_theta_total <- results_daily$obs_01_macrotheta +
          results_daily$obs_01_theta
        results_daily$obs_02_theta_total <- results_daily$obs_02_macrotheta +
          results_daily$obs_02_theta
        results_daily$obs_03_theta_total <- results_daily$obs_03_macrotheta +
          results_daily$obs_03_theta
        results_daily$obs_04_theta_total <- results_daily$obs_04_macrotheta +
          results_daily$obs_04_theta
        results_daily$obs_05_theta_total <- results_daily$obs_05_macrotheta +
          results_daily$obs_05_theta
        results_daily$obs_06_theta_total <- results_daily$obs_06_macrotheta +
          results_daily$obs_06_theta
        results_daily$obs_07_theta_total <- results_daily$obs_07_macrotheta +
          results_daily$obs_07_theta
        results_daily$obs_08_theta_total <- results_daily$obs_08_macrotheta +
          results_daily$obs_08_theta
        results_daily$obs_09_theta_total <- results_daily$obs_09_macrotheta +
          results_daily$obs_09_theta
        results_daily$obs_10_theta_total <- results_daily$obs_10_macrotheta +
          results_daily$obs_10_theta

        results_daily_1 <- results_daily

        ##### save daily output 1 #####
        write.csv(x = results_daily_1, file = paste0('../../4_Results/2csvs/', SS.name, '1.csv'))

        results_daily_all <- results_daily_1

        if(time.variable == TRUE){

          for(j in 1:length(model.start2)){

            #Create Atmosphere 2 ####
            met.data.2.all <- met.data[met.data$Date >= model.start2[1] ,]

            met.data2 <- met.data[met.data$Date >= model.start2[j] &
                                    met.data$Date <= model.end2[j] ,]

            intro.2 <- intro
            intro.2[4] <- str_pad(length(met.data2$Date),
                                  width= 6, side= "left")

            start2 <- which(met.data.2.all$Date == model.start2[j])
            end2 <- which(met.data.2.all$Date == model.end2[j])

            atm.temp.2 <- atm2[start2:end2]

            padwidth <- unlist(gregexpr("\\d \\d", atm.temp.2[length(atm.temp.2)] ))[1]

            substr(atm.temp.2[1:length(atm.temp.2)], 1, padwidth) <- str_pad(seq(1:length(atm.temp.2)),
                                                                             width= padwidth, pad= " ",
                                                                             side= "right")

            atm.new.file = file("ATMOSPH.IN", "w")
            writeLines(intro.2,atm.new.file)
            write.table(atm.temp.2, atm.new.file,
                        row.names = FALSE, col.names=FALSE,
                        sep="\t", quote = FALSE)
            writeLines(end,atm.new.file)
            close(atm.new.file)

            # Create PROFILE.DAT file ####
            obs.vector2 <- create_profile_modelinputs(Zf = Zf,obs9=obs$depth,
                                                      material.number = material.number,
                                                      material.divide = material.divide,
                                                      model.number = model.number)

            # Create SELECTOR.IN and Run Hydrus ####
            # loop through time.int vector to get convergence
            day.check <- 0 # start with zero before going through time.int

            # i = 1
            for(i in 1:length(time.int.vector))  # 2024-11-16 looping until model converges
            {
              list.temp <- CreateSelectorAndRun(met.data1 = met.data2,
                                                vg = vgs[[(j+1)]],
                                                my.time.int = time.int.vector[i],
                                                Zf = Zf,
                                                # rotation_name = rotation_name, # omit for now (2024-10-08)
                                                SS.name = SS.name,
                                                model.number= model.number,
                                                tv.info = tv.info.all[(j+1) ,])
              my.time.int <- list.temp$my.time.int
              day.check <- list.temp$day.check
              my.filename <-  list.temp$my.filename
            }

            # H1D Results Merge w Met
            # if(length(day.check)==length(met.data$date)+10) # edit omit to make work 2024-10-08
            if(length(day.check)==length(met.data2$Date)+10)
            {
              T_Level = read.table(file = "./T_Level.out",
                                   header = FALSE, skip = 9, nrows = length(met.data2$Date))
              T_Level_headings = read.table(file = "./T_Level.out",
                                            header = FALSE, skip = 6, nrows = 1)

              h1d_daily <- T_Level[,c(1,4,5,6,15,17,21,23)]
              colnames(h1d_daily) <- c("h1d_day", "h1d_surfaceflux_cm",
                                       "h1d_t_cm",
                                       "h1d_dp_cm",
                                       "h1d_macro_surface_cm",
                                       "h1d_matrix_surface_cm", "h1d_macro_dp_cm",
                                       "h1d_matrix_dp_cm")

              #h1d_daily <- h1d_daily[,-5]
              h1d_daily$Date <- as.POSIXct("1950-01-01")
              h1d_daily$Date[1:length(h1d_daily$Date)] <- seq(model.start2[j], model.end2[j], by="days")

              met.data2$Date <- as.POSIXct(met.data2$Date) # 2024-11-16 added
              results_daily <- merge(met.data2, h1d_daily, by = 'Date') # 2024-11-16 added by date

              results_daily$month <- month(floor_date(results_daily$Date, 'month'))
              results_daily$year <- year(floor_date(results_daily$Date, 'year'))

              results_daily$h1d_e_cm <- results_daily$h1d_surfaceflux_cm

              for(e in 1:length(results_daily$Precip)){

                if(results_daily$Precip[e] > 0){

                  results_daily$h1d_e_cm[e] <- results_daily$Precip[e] + results_daily$h1d_surfaceflux_cm[e]

                }
              }

              results_daily$h1d_e_cm[results_daily$h1d_e_cm < 0.001] <- 0

              #### commented out 2024-11-16
              # results_daily$crop_yr <- results_daily$year
              # results_daily[results_daily$month %in% c(1,2,3), "crop_yr"] <-
              #   results_daily[results_daily$month %in% c(1,2,3), "year"] - 1
              # crop_yr__crop_years <- crop_years
              # colnames(crop_yr__crop_years)
              # colnames(crop_yr__crop_years)  <- c("crop_yr","crop_yr__crop_name",
              #                                     "crop_yr__c.vs.f","crop_yr__last_crop")
              # results_daily <- merge(results_daily, crop_yr__crop_years)
              #
              # nitrate$crop_yr__c.vs.f <- nitrate$c.vs.f # simply for merge colname matching
              # results_daily <- merge(results_daily, nitrate, by="crop_yr__c.vs.f")
              # results_daily <- results_daily[with(results_daily,order(date)),]
              # # results_daily[85:110,]
              # results_daily$leachN_kg.ha.day <- results_daily$h1d_dp_mm * results_daily$no3n /10
              # results_daily$same <- "same"

              # Observation Point VWC ####
              obs.daily = read.table(file = "./Obs_Node.out",
                                     header = FALSE, skip = 11, nrows = length(met.data2$Date))
              obs.headings = read.table(file = "./Obs_Node.out",
                                        header = FALSE, skip = 10, nrows = 1,
                                        stringsAsFactors = FALSE)
              obs.headings <- as.character(obs.headings[1,])
              obs.headings <- paste0(c('',rep("obs_01_",3),
                                       rep('obs_02_',3),
                                       rep('obs_03_',3),
                                       rep('obs_04_',3),
                                       rep('obs_05_',3),
                                       rep('obs_06_',3),
                                       rep('obs_07_',3),
                                       rep('obs_08_',3),
                                       rep('obs_09_',3),
                                       rep('obs_10_',3)),
                                     obs.headings)
              colnames(obs.daily) <- obs.headings
              obs.daily$Date <- as.POSIXct("1950-01-01")
              obs.daily$Date[1:length(obs.daily$Date)] <- seq(model.start2[j],
                                                              model.end2[j],
                                                              by="days")

              obs.daily.f = read.table(file = "./Obs_NodF.out",
                                       header = FALSE, skip = 11, nrows = length(met.data2$Date))
              obs.headings.f = read.table(file = "./Obs_NodF.out",
                                          header = FALSE, skip = 10, nrows = 1,
                                          stringsAsFactors = FALSE)
              obs.headings.f <- as.character(obs.headings.f[1,])
              obs.headings.f <- paste0(c('',rep("obs_01_macro",3),
                                         rep('obs_02_macro',3),
                                         rep('obs_03_macro',3),
                                         rep('obs_04_macro',3),
                                         rep('obs_05_macro',3),
                                         rep('obs_06_macro',3),
                                         rep('obs_07_macro',3),
                                         rep('obs_08_macro',3),
                                         rep('obs_09_macro',3),
                                         rep('obs_10_macro',3)),
                                       obs.headings.f)
              colnames(obs.daily.f) <- obs.headings.f
              obs.daily.f$date <- as.POSIXct("1950-01-01")
              obs.daily.f$date[1:length(obs.daily.f$date)] <- seq(model.start2[j],
                                                                  model.end2[j],
                                                                  by="days")
              obs.daily.f <- obs.daily.f[, c(32, 1:31)]

              results_daily <- merge(results_daily, obs.daily)
              results_daily <- merge(results_daily, obs.daily.f)

              results_daily$obs_01_theta_total <- results_daily$obs_01_macrotheta +
                results_daily$obs_01_theta
              results_daily$obs_02_theta_total <- results_daily$obs_02_macrotheta +
                results_daily$obs_02_theta
              results_daily$obs_03_theta_total <- results_daily$obs_03_macrotheta +
                results_daily$obs_03_theta
              results_daily$obs_04_theta_total <- results_daily$obs_04_macrotheta +
                results_daily$obs_04_theta
              results_daily$obs_05_theta_total <- results_daily$obs_05_macrotheta +
                results_daily$obs_05_theta
              results_daily$obs_06_theta_total <- results_daily$obs_06_macrotheta +
                results_daily$obs_06_theta
              results_daily$obs_07_theta_total <- results_daily$obs_07_macrotheta +
                results_daily$obs_07_theta
              results_daily$obs_08_theta_total <- results_daily$obs_08_macrotheta +
                results_daily$obs_08_theta
              results_daily$obs_09_theta_total <- results_daily$obs_09_macrotheta +
                results_daily$obs_09_theta
              results_daily$obs_10_theta_total <- results_daily$obs_10_macrotheta +
                results_daily$obs_10_theta

              results_daily_2 <- results_daily
              ##### save daily output 2#####
              write.csv(x = results_daily_2, file = paste0('../../4_Results/2csvs/', SS.name, j+1, '.csv'))

              #save data for both
              results_daily_all <- rbind(results_daily_all, results_daily_2)

            }else{
              break
            }
          }

          write.csv(x= results_daily_all, file = paste0('../../4_Results/2csvs/', SS.name, 'All.csv'))

        }

        #load data (for Meghan's laptop that can't run Hydrus)
        #runs <- list.files('../../4_Results/1csvs')
        #for(i in 1:length(runs))
        #{
        # print(runs[i])
        #results_daily <- read.csv(paste0('../../4_Results/1csvs/', runs[i]))
        #results_daily$date <- as.POSIXct(results_daily$date)
        #}


        #this behavior is what is making plotting weird
        #paste(colnames(textures)[1], colnames(textures)[2]) #makes 1 string
        #paste(colnames(textures)[1:2]) #makes 2 strings
        #need to use collapse argument to paste column names into 1 string

        # plot volumetric water content results ####
        # i = 1

        # windows(1600,800)
        results_daily <- results_daily_all

        png(width = 1600, height = 800, res = 144,
            filename = paste0('../../4_Results/1VWCplots/', SS.name, '.png'))
        plot(results_daily$obs_01_theta ~ results_daily$Date, type = 'n', # no symbology
             main = SS.name, ylab = 'VWC',
             ylim = c(0,0.5))

        for(i in 1:9){
          if(obs[i,'plot.yn'] == 'y') {
            # points(results_daily[,obs[i,'df.name']] ~ results_daily$date,
            #        pch = obs[i,"pch"], col = obs[i,'col'])
            lines(results_daily[,obs[i,'df.name']] ~ results_daily$date,
                  col = obs[i,'col'],
                  lty= 2, lwd= 2)

            name <- obs[i, 'df.name']
            macro <- paste0(substr(name, 1, 7), "macro", substr(name, 8, 12))
            total <- paste0(name, "_total")

            lines(results_daily[, macro] ~ results_daily$date,
                  col = obs[i,'col'],
                  lty= 3, lwd= 2)

            points(results_daily[, total] ~ results_daily$date,
                   pch = obs[i,"pch"], col = obs[i,'col'])
            lines(results_daily[,total] ~ results_daily$date,
                  col = obs[i,'col'],
                  lwd= 2)
          }
        }

        legend("topleft", legend= c("Matrix", "Macropore", "Total"),
               lty= c(2,3,1))

        # points(results_daily$obs_01_theta ~ results_daily$date)

        text(x = as.POSIXct('2023-02-01'), y = 0.4,
             paste0('Obs1= ', min(results_daily$obs_01_theta), " to ",
                    max(results_daily$obs_01_theta)))

        # print VG parameters on plot
        # labels.names <- str_pad(colnames(textures)[3:length(colnames(textures))], width= 12)
        # labels.values <- str_pad(textures[i, 3:length(colnames(textures))],
        #                                                      width= 10)

        # labels.names <- str_pad(colnames(vgs), width= 12)
        # labels.values <- str_pad(vgs, width= 10)
        #
        # mtext(side = 3, line= 0.8, at= as.POSIXct("2023-07-01"),
        #       text= paste0(labels.names, collapse= " "),
        #       cex= 0.8)
        # mtext(side = 3, line= 0.05, at= as.POSIXct('2023-07-01'),
        #       text= paste0(labels.values, collapse= " ", sep= " "),
        #       cex= 0.8)
        dev.off()


        # calculate cumulative fluxes ####
        # calc cumulative deep perc
        results_daily$dp_cum <- 0
        results_daily[1,'dp_cum'] <- results_daily[1,'h1d_dp_cm']
        for(i in 2:length(results_daily$dp_cum)){
          results_daily[i,'dp_cum'] <- results_daily[i-1,'dp_cum'] +
            results_daily[i,'h1d_dp_cm']
        }

        # calc cumulative evaporation
        results_daily$evap_cum <- 0
        results_daily[1,'evap_cum'] <- results_daily[1,'h1d_e_cm']
        for(i in 2:length(results_daily$evap_cum)){
          results_daily[i,'evap_cum'] <- results_daily[i-1,'evap_cum'] +
            results_daily[i,'h1d_e_cm']
        }

        # calc cumulative transpiration
        results_daily$tran_cum <- 0
        results_daily[1,'tran_cum'] <- results_daily[1,'h1d_t_cm']
        for(i in 2:length(results_daily$tran_cum)){
          results_daily[i,'tran_cum'] <- results_daily[i-1,'tran_cum'] +
            results_daily[i,'h1d_t_cm']
        }

        # plot cumulative fluxes ####
        # windows(1600,800)

        png(width = 1600, height = 800, res = 144,
            filename = paste0('../../4_Results/2fluxplots/', SS.name, '.png'))
        cum.min <- min(tail(results_daily[,'dp_cum'],1),
                       tail(results_daily[,'evap_cum'],1),
                       tail(results_daily[,'tran_cum'],1))
        cum.max <- max(tail(results_daily[,'dp_cum'],1),
                       tail(results_daily[,'evap_cum'],1),
                       tail(results_daily[,'tran_cum'],1))

        plot(results_daily$dp_cum ~ results_daily$Date, type = 'n', # no symbology
             main = SS.name,
             ylim = c(cum.min,cum.max),
             ylab = 'cumualtive partitioning (cm)',
             xlab = 'date')

        # points(results_daily$dp_cum ~ results_daily$date,
        #        col = 'black')
        # points(results_daily$runoff_cum ~ results_daily$date,
        #        col = 'blue')
        # points(results_daily$evap_cum ~ results_daily$date,
        #        col = 'gray30')
        # points(results_daily$tran_cum ~ results_daily$date,
        #        col = 'gray80')
        lines(results_daily$dp_cum ~ results_daily$Date,
              col = 'black', lty = 2)
        lines(results_daily$evap_cum ~ results_daily$Date,
              col = 'gray30')
        lines(results_daily$tran_cum ~ results_daily$Date,
              col = 'green')
        legend('topleft', legend = c('deep perc', 'runoff', 'evap', 'tran'),
               col = c('black', 'blue', 'gray30', 'green'), lty = c(2,1,1,1))

        legend('top', bty= "n",
               legend= c(paste("Cumulative Evaporation=",
                               round(tail(results_daily[,'evap_cum'],1),2)),
                         paste("Cumulative Transpiration=",
                               round(tail(results_daily[,'tran_cum'],1),2)),
                         paste("Cumulative DP=",
                               round(tail(results_daily[,'dp_cum'],1),2))
               ))

        dev.off()

        #Make combined plot ####
        ymax1 <- max(results_daily$Precip, results_daily$ET_cm)
        ymin1 <- min(results_daily$Precip, results_daily$ET_cm)

        date.seq <- seq(model.start1, model.end1, by= "months")
        date.seq <- date.seq[seq(from= 1, to= length(date.seq), by= 3)]

        png(width = 1600, height = 800, res = 144,
            filename = paste0('../../4_Results/2Combinedplots/', SS.name, '.png'))

        layout(matrix(c(1,2,3), ncol= 1))
        par(mar= c(1,4,2,4))
        par(oma=c(0,0,2,0))

        plot(results_daily$Precip ~ results_daily$Date,
             main= paste("Weather and Irrigation"),
             xaxt= "n",
             xlab= "",
             xaxs= "i",
             ylab= "",
             ylim= c(ymax1, ymin1),
             yaxs= "i",
             col.axis= "blue",
             type= "h",
             col= results_daily$col,
             lwd= 2)

        mtext(text= expression(bold("Precip (cm)")),
              side= 2, line=2, cex= 1, col= "blue")

        par(new= TRUE)
        plot(results_daily$ET_cm ~ results_daily$Date,
             xaxt= "n",
             xlab= "",
             xaxs= "i",
             yaxt= "n",
             ylab= "",
             ylim= c(ymin1, ymax1),
             yaxs= "i",
             type= "l",
             lwd= 2)

        abline(v= c(date.seq),
               lty= 2,
               lwd= 2,
               col= "gray75")

        label.list <- seq(0, round(ymax1, 0), 0.5)

        axis(side= 4, labels= c(label.list),
             at= c(label.list))

        mtext(text= expression(bold("ET (cm)")),
              side= 4, line=3, cex= 1)

        mtext(text= SS.name,
              side= 3, line= 1.5, cex= 1.5)

        par(mar= c(1,4,3,4))

        plot(results_daily$obs_01_theta ~ results_daily$Date,
             type = 'n', # no symbology
             main = "",
             ylab = 'VWC',
             xlab= "",
             xaxs= "i",
             xaxt= "n",
             ylim = c(0,0.5))

        title(main = "Soil Moisture", line= 1.5)

        for(i in 1:9){
          if(obs[i,'plot.yn'] == 'y') {
            points(results_daily[,obs[i,'df.name']] ~ results_daily$Date,
                   pch = obs[i,"pch"], col = obs[i,'col'])
            lines(results_daily[,obs[i,'df.name']] ~ results_daily$Date,
                  pch = obs[i,"pch"], col = obs[i,'col'])
          }
        }
        # points(results_daily$obs_01_theta ~ results_daily$date)

        text(x = as.POSIXct(model.start1+(60*60*24*50)), y = 0.2,
             paste0('Obs1= ', min(results_daily$obs_01_theta), " to ",
                    max(results_daily$obs_01_theta)))

        leg.info <- obs[obs$plot.yn == "y" ,]

        legend("bottomleft", legend= c(leg.info$depth),
               pch= c(leg.info$pch), col= c(leg.info$col),
               lwd= 2, cex= 0.8)

        # print VG parameters on plot
        # labels.names <- str_pad(colnames(textures)[3:length(colnames(textures))], width= 12)
        # labels.values <- str_pad(textures[i, 3:length(colnames(textures))],
        #                                                      width= 10)

        if(model.number==0){

          # labels.names <- str_pad(colnames(vgs)[1:6], width= 12)
          # labels.values <- str_pad(vgs[1:6], width= 10)

        }else{

          labels.names <- str_pad(colnames(vgs), width= 12)
          labels.values <- str_pad(vgs, width= 10)

        }

        midpoint <- mean(c(model.start1, model.end1))

        # mtext(side = 3, line= 0.65, at= as.POSIXct(midpoint),
        #       text= paste0(labels.names, collapse= " "),
        #       cex= 0.7)
        # mtext(side = 3, line= 0.05, at= as.POSIXct(midpoint),
        #       text= paste0(labels.values, collapse= " ", sep= " "),
        #       cex= 0.7)

        abline(v= c(date.seq),
               lty= 2,
               lwd= 2,
               col= "gray75")

        par(mar= c(3,4,1,4))

        plot(results_daily$dp_cum ~ results_daily$Date,
             type = 'n', # no symbology
             main = "Cumulative Fluxes",
             xlab= "",
             xaxs= "i",
             xaxt= "n",
             ylim = c(cum.min,cum.max),
             ylab = 'cumualtive partitioning (cm)')

        lines(results_daily$dp_cum ~ results_daily$Date,
              col = 'black', lty = 2, lwd= 2)
        # lines(results_daily$runoff_cum ~ results_daily$Date,
        #       col = 'blue', lwd= 2)
        lines(results_daily$evap_cum ~ results_daily$Date,
              col = 'gray30', lwd= 2)
        lines(results_daily$tran_cum ~ results_daily$Date,
              col = 'green', lwd= 2)
        legend('topleft', legend = c('deep perc', 'runoff', 'evap', 'tran'),
               col = c('black', 'blue', 'gray30', 'green'), lty = c(2,1,1,1))

        abline(v= c(date.seq),
               lty= 2,
               lwd= 2,
               col= "gray75")

        legend('top', bty= "n",
               legend= c(paste("Cumulative Evaporation=",
                               round(tail(results_daily[,'evap_cum'],1),2)),
                         paste("Cumulative Transpiration=",
                               round(tail(results_daily[,'tran_cum'],1),2)),
                         paste("Cumulative DP=",
                               round(tail(results_daily[,'dp_cum'],1),2))
                         #paste("Cumulative Runoff=",
                         #     round(tail(results_daily[,'runoff_cum'],1),2))
               )
        )

        x.axis.year.month(Xrange= c(model.start1, model.end1),
                          year.cex = 1,
                          month.cex = 0.7,
                          axis.line = 0,
                          year.line = 2,
                          month.line = 0)
        dev.off()


        #Make Flux Direction plot ####
        ymax1 <- max(results_daily$Precip, results_daily$ET_cm)
        ymin1 <- min(results_daily$Precip, results_daily$ET_cm)

        date.seq <- seq(model.start1, model.end1, by= "months")
        date.seq <- date.seq[seq(from= 1, to= length(date.seq), by= 3)]

        png(width = 1600, height = 800, res = 144,
            filename = paste0('../../4_Results/2fluxdirectionplots/', SS.name, '.png'))

        layout(matrix(c(1,2,3,4), ncol= 1))
        par(mar= c(1,4,2,4))
        par(oma=c(0,0,2,0))

        plot(results_daily$Precip ~ results_daily$Date,
             main= paste("Weather and Irrigation"),
             xaxt= "n",
             xlab= "",
             xaxs= "i",
             ylab= "",
             ylim= c(ymax1, ymin1),
             yaxs= "i",
             col.axis= "blue",
             type= "h",
             col= results_daily$col,
             lwd= 2)

        mtext(text= expression(bold("Precip (cm)")),
              side= 2, line=2, cex= 1, col= "blue")

        par(new= TRUE)
        plot(results_daily$ET_cm ~ results_daily$Date,
             xaxt= "n",
             xlab= "",
             xaxs= "i",
             yaxt= "n",
             ylab= "",
             ylim= c(ymin1, ymax1),
             yaxs= "i",
             type= "l",
             lwd= 2)

        abline(v= c(date.seq),
               lty= 2,
               lwd= 2,
               col= "gray75")

        label.list <- seq(0, round(ymax1, 0), 0.5)

        axis(side= 4, labels= c(label.list),
             at= c(label.list))

        mtext(text= expression(bold("ET (cm)")),
              side= 4, line=3, cex= 1)

        mtext(text= SS.name,
              side= 3, line= 1.5, cex= 1.5)

        par(mar= c(1,4,3,4))

        plot(results_daily$obs_03_theta ~ results_daily$Date,
             main= "15 cm Modeled Soil Moisture and Flux",
             xlab= "",
             xaxs= "i",
             ylab= "VWC",
             ylim= c(0, 0.5),
             yaxs= "i",
             type= "l",
             lwd= 1,
             col= "blue")

        par(new=TRUE)

        plot(results_daily$obs_03_Flux ~ results_daily$Date,
             main= "15 cm Modeled Soil Moisture and Flux",
             xlab= "",
             xaxs= "i",
             yaxt= "n",
             ylab= "",
             #ylim= c(0, 0.5),
             type= "l",
             lwd= 1,
             col= "black")

        abline(v= c(date.seq),
               lty= 2,
               lwd= 2,
               col= "gray75")

        abline(h= 0, lty=2, lwd= 2)

        label.list <- seq(round(min(results_daily$obs_03_Flux), 3),
                          round(max(results_daily$obs_03_Flux), 3), 0.1)

        axis(side= 4, labels= c(label.list),
             at= c(label.list))

        mtext(text= "Flux",
              side= 4, line=3, cex= 1)

        plot(results_daily$obs_08_theta ~ results_daily$Date,
             main= "91 cm Modeled Soil Moisture and Flux",
             xlab= "",
             xaxs= "i",
             ylab= "VWC",
             ylim= c(0, 0.5),
             yaxs= "i",
             type= "l",
             lwd= 1,
             col= "blue")

        par(new=TRUE)

        plot(results_daily$obs_08_Flux ~ results_daily$Date,
             main= "91 cm Modeled Soil Moisture and Flux",
             xlab= "",
             xaxs= "i",
             yaxt= "n",
             ylab= "",
             #ylim= c(0, 0.5),
             type= "l",
             lwd= 1,
             col= "black")

        abline(v= c(date.seq),
               lty= 2,
               lwd= 2,
               col= "gray75")

        abline(h= 0, lty=2, lwd= 2)

        label.list <- seq(round(min(results_daily$obs_08_Flux), 3),
                          round(max(results_daily$obs_08_Flux), 3), 0.1)

        axis(side= 4, labels= c(label.list),
             at= c(label.list))

        mtext(text= "Flux",
              side= 4, line=3, cex= 1)

        plot(results_daily$obs_10_theta ~ results_daily$Date,
             main= "148 cm Modeled Soil Moisture and Flux",
             xlab= "",
             xaxs= "i",
             ylab= "VWC",
             ylim= c(0, 0.5),
             yaxs= "i",
             type= "l",
             lwd= 1,
             col= "blue")

        par(new=TRUE)

        plot(results_daily$obs_10_Flux ~ results_daily$Date,
             main= "148 cm Modeled Soil Moisture and Flux",
             xlab= "",
             xaxs= "i",
             yaxt= "n",
             ylab= "",
             #ylim= c(0, 0.5),
             type= "l",
             lwd= 1,
             col= "black")

        abline(h= 0, lty=2, lwd= 2)

        abline(v= c(date.seq),
               lty= 2,
               lwd= 2,
               col= "gray75")

        legend("bottomleft", legend= c("Soil Moisture", "Flux"),
               lty= c(1,1), col= c("blue", "black"))

        label.list <- seq(round(min(results_daily$obs_10_Flux), 3),
                          round(max(results_daily$obs_10_Flux), 3), 0.1)

        axis(side= 4, labels= c(label.list),
             at= c(label.list))

        mtext(text= "Flux",
              side= 4, line=3, cex= 1)

        dev.off()

        # simplify for now be omitting the following list output (2024-10-08)
        #   metadata <- list(vg = vg, crop_growth_param = crop_growth_param,
        #                    nitrate = nitrate, time.int = my.time.int,
        #                    obs.vector = obs.vector)
        #   mydata <- list(daily = results_daily, metadata = metadata)
        #   save(mydata, file = paste0("../../4_Results/1_RawDaily/",
        #                              out.subfolder.name, "/", my.filename,".RData"))
        #   return(list(daily = results_daily))
        # } else
        # metadata <- list(vg = vg, crop_growth_param = crop_growth_param,
        #                      nitrate = nitrate, time.int = my.time.int,
        #                  obs.vector = obs.vector)
        # mydata <- list(daily = "fail", metadata = metadata)
        # save(x =mydata, file = paste0("../../4_Results/1_RawDaily/",
        #                           out.subfolder.name, "/", my.filename,"FAIL",".RData"))
      } else
      {
        write.csv(x = 1:3, file = paste0('../../4_Results/2fails/', SS.name, 'FAIL.csv'))
      }

    }

  }

  if(timestep.short == TRUE){
    if(length(day.check)== met.data1$hydrus.day[length(met.data1$hydrus.day)]+10)
    {
      T_Level = read.table(file = "./T_Level.out",
                           header = FALSE, skip = 9,
                           nrows = met.data1$hydrus.day[length(met.data1$hydrus.day)])
      T_Level_headings = read.table(file = "./T_Level.out",
                                    header = FALSE, skip = 6, nrows = 1)
      h1d_daily <- T_Level[,c(1,5,6,15,16,19)]
      colnames(h1d_daily) <- c("h1d_day", "h1d_t_mm",
                               "h1d_dp_mm", "h1d_runoff_cm",
                               "h1d_runoff_cum_cm",
                               "h1d_e_cum_mm")

      #get daily runoff
      for(i in 2:length(h1d_daily$h1d_runoff_cm)){

        h1d_daily$h1d_runoff_cm[i] <- h1d_daily$h1d_runoff_cum_cm[i]-h1d_daily$h1d_runoff_cum_cm[i-1]
      }

      h1d_daily$h1d_e_mm <- -999
      h1d_daily$h1d_e_mm[1] <- h1d_daily$h1d_e_cum_mm[1]
      for(i in 2:length(h1d_daily$h1d_e_mm))
      {
        h1d_daily$h1d_e_mm[i] <- h1d_daily[i,"h1d_e_cum_mm"]- h1d_daily[i-1,"h1d_e_cum_mm"]
      }
      h1d_daily <- h1d_daily[,-5]
      h1d_daily$Date <- as.POSIXct("1950-01-01")
      h1d_daily$Date[1:length(h1d_daily$Date)] <- seq(model.start1, model.end1, by="days")

      #aggregate irrigation data
      met.data1$Date <- as.POSIXct(met.data1$Date) # 2024-11-16 added
      #
      # mesonet1 <- aggregate(met.data$preciponly, by= list(met.data$day), sum)
      # colnames(mesonet1)[2] <- "Precip"
      # mesonet2 <- aggregate(met.data$gdd, by= list(met.data$day), sum)
      # colnames(mesonet2)[2] <- "gdd"
      # mesonet3 <- aggregate(met.data$ET_cm, by= list(met.data$day), sum)
      # colnames(mesonet3)[2] <- "ET_cm"
      # mesonet4 <- aggregate(met.data$root.depth_cm, by= list(met.data$day), sum)
      # colnames(mesonet4)[2] <- "root.depth_cm"
      # mesonet5 <- aggregate(met.data$StartDate, by= list(met.data$day), unique)
      # colnames(mesonet5)[2] <- "StartDate"
      # mesonet6 <- aggregate(met.data$EndDate, by= list(met.data$day), unique)
      # colnames(mesonet6)[2] <- "EndDate"
      # mesonet7 <- aggregate(met.data$t_coef, by= list(met.data$day), sum)
      # colnames(mesonet7)[2] <- "t_coef"
      # mesonet8 <- aggregate(met.data$e_coef, by= list(met.data$day), sum)
      # colnames(mesonet8)[2] <- "e_coef"
      # mesonet9 <- aggregate(met.data$pot_e_cm, by= list(met.data$day), sum)
      # colnames(mesonet9)[2] <- "pot_e_cm"
      # mesonet10 <- aggregate(met.data$pot_t_cm, by= list(met.data$day), sum)
      # colnames(mesonet10)[2] <- "pot_t_cm"
      # mesonet16 <- aggregate(met.data$Date, by= list(met.data$day), unique)
      # colnames(mesonet16)[2] <- "Date"
      # mesonet17 <- aggregate(met.data$irrigation, by= list(met.data$day), sum)
      # colnames(mesonet17)[2] <- "Irrigation"
      #
      # mesonet16$Date <- seq(as.POSIXct(startday), as.POSIXct(endday),
      #                       by= "days")
      #
      #
      # mesonet <- merge(mesonet1, c(mesonet2, mesonet3, mesonet4,
      #                              mesonet5, mesonet6, mesonet7,
      #                              mesonet8, mesonet9, mesonet10,
      #                              mesonet11, mesonet12, mesonet13,
      #                              mesonet14, mesonet15, mesonet16,
      #                              mesonet17),
      #                  by= "Group.1")
      #
      # mesonet <- mesonet[, -c(which(str_detect(colnames(mesonet), "Group.1")))]
      #

      results_daily <- merge(met.data1, h1d_daily, by = 'Date') # 2024-11-16 added by date

      results_daily$month <- month(floor_date(results_daily$Date, 'month'))
      results_daily$year <- year(floor_date(results_daily$Date, 'year'))

      #### commented out 2024-11-16
      # results_daily$crop_yr <- results_daily$year
      # results_daily[results_daily$month %in% c(1,2,3), "crop_yr"] <-
      #   results_daily[results_daily$month %in% c(1,2,3), "year"] - 1
      # crop_yr__crop_years <- crop_years
      # colnames(crop_yr__crop_years)
      # colnames(crop_yr__crop_years)  <- c("crop_yr","crop_yr__crop_name",
      #                                     "crop_yr__c.vs.f","crop_yr__last_crop")
      # results_daily <- merge(results_daily, crop_yr__crop_years)
      #
      # nitrate$crop_yr__c.vs.f <- nitrate$c.vs.f # simply for merge colname matching
      # results_daily <- merge(results_daily, nitrate, by="crop_yr__c.vs.f")
      # results_daily <- results_daily[with(results_daily,order(date)),]
      # # results_daily[85:110,]
      # results_daily$leachN_kg.ha.day <- results_daily$h1d_dp_mm * results_daily$no3n /10
      # results_daily$same <- "same"

      # Observation Point VWC ####
      obs.daily = read.table(file = "./Obs_Node.out",
                             header = FALSE, skip = 11,
                             nrows = met.data1$hydrus.day[length(met.data1$hydrus.day)])
      obs.headings = read.table(file = "./Obs_Node.out",
                                header = FALSE, skip = 10, nrows = 1,
                                stringsAsFactors = FALSE)
      obs.headings <- as.character(obs.headings[1,])
      obs.headings <- paste0(c('',rep("obs_01_",3),
                               rep('obs_02_',3),
                               rep('obs_03_',3),
                               rep('obs_04_',3),
                               rep('obs_05_',3),
                               rep('obs_06_',3),
                               rep('obs_07_',3),
                               rep('obs_08_',3),
                               rep('obs_09_',3),
                               rep('obs_10_',3)),
                             obs.headings)
      colnames(obs.daily) <- obs.headings
      obs.daily$Date <- as.POSIXct("1950-01-01")
      obs.daily$Date[1:length(obs.daily$Date)] <- seq(model.start1, model.end1, by="days")
      results_daily_1 <- merge(results_daily, obs.daily)

      ##### save daily output 1 #####
      write.csv(x = results_daily_1, file = paste0('../../4_Results/2csvs/', SS.name, '1.csv'))

      results_daily_all <- results_daily_1

      if(time.variable == TRUE){

        for(j in 1:length(model.start2)){

          #Create Atmosphere 2 ####
          met.data.2.all <- met.data[met.data$Date >= model.start2[1] ,]

          met.data2 <- met.data[met.data$Date >= model.start2[j] &
                                  met.data$Date <= model.end2[j] ,]

          intro.2 <- intro
          intro.2[4] <- str_pad(length(met.data2$Date),
                                width= 6, side= "left")

          start2 <- which(met.data.2.all$Date == model.start2[j])
          end2 <- which(met.data.2.all$Date == model.end2[j])

          atm.temp.2 <- atm2[start2:end2]

          padwidth <- unlist(gregexpr("\\d \\d", atm.temp.2[length(atm.temp.2)] ))[1]

          substr(atm.temp.2[1:length(atm.temp.2)], 1, padwidth) <- str_pad(seq(1:length(atm.temp.2)),
                                                                           width= padwidth, pad= " ",
                                                                           side= "right")

          atm.new.file = file("ATMOSPH.IN", "w")
          writeLines(intro.2,atm.new.file)
          write.table(atm.temp.2, atm.new.file,
                      row.names = FALSE, col.names=FALSE,
                      sep="\t", quote = FALSE)
          writeLines(end,atm.new.file)
          close(atm.new.file)

          # Create PROFILE.DAT file ####
          obs.vector2 <- create_profile_modelinputs(Zf = Zf,obs9)

          # Create SELECTOR.IN and Run Hydrus ####
          # loop through time.int vector to get convergence
          day.check <- 0 # start with zero before going through time.int

          # i = 1
          for(i in 1:length(time.int.vector))  # 2024-11-16 looping until model converges
          {
            list.temp <- CreateSelectorAndRun(met.data1 = met.data2,
                                              vg = vg[(j+1) ,],
                                              my.time.int = time.int.vector[i],
                                              Zf = Zf,
                                              # rotation_name = rotation_name, # omit for now (2024-10-08)
                                              SS.name = SS.name,
                                              model.number= model.number)
            my.time.int <- list.temp$my.time.int
            day.check <- list.temp$day.check
            my.filename <-  list.temp$my.filename
          }

          # H1D Results Merge w Met
          # if(length(day.check)==length(met.data$date)+10) # edit omit to make work 2024-10-08
          if(length(day.check)==length(met.data2$Date)+10)
          {
            T_Level = read.table(file = "./T_Level.out",
                                 header = FALSE, skip = 9, nrows = length(met.data2$Date))
            T_Level_headings = read.table(file = "./T_Level.out",
                                          header = FALSE, skip = 6, nrows = 1)
            h1d_daily <- T_Level[,c(1,5,6,15,19)]
            colnames(h1d_daily) <- c("h1d_day", "h1d_t_mm",
                                     "h1d_dp_mm", "h1d_runoff_mm",
                                     "h1d_e_cum_mm")
            h1d_daily$h1d_e_mm <- -999
            h1d_daily$h1d_e_mm[1] <- h1d_daily$h1d_e_cum_mm[1]
            for(i in 2:length(h1d_daily$h1d_e_mm))
            {
              h1d_daily$h1d_e_mm[i] <- h1d_daily[i,"h1d_e_cum_mm"]- h1d_daily[i-1,"h1d_e_cum_mm"]
            }
            h1d_daily <- h1d_daily[,-5]
            h1d_daily$Date <- as.POSIXct("1950-01-01")
            h1d_daily$Date[1:length(h1d_daily$Date)] <- seq(model.start2[j], model.end2[j], by="days")

            met.data2$Date <- as.POSIXct(met.data2$Date) # 2024-11-16 added
            results_daily <- merge(met.data2, h1d_daily, by = 'Date') # 2024-11-16 added by date

            results_daily$month <- month(floor_date(results_daily$Date, 'month'))
            results_daily$year <- year(floor_date(results_daily$Date, 'year'))

            #### commented out 2024-11-16
            # results_daily$crop_yr <- results_daily$year
            # results_daily[results_daily$month %in% c(1,2,3), "crop_yr"] <-
            #   results_daily[results_daily$month %in% c(1,2,3), "year"] - 1
            # crop_yr__crop_years <- crop_years
            # colnames(crop_yr__crop_years)
            # colnames(crop_yr__crop_years)  <- c("crop_yr","crop_yr__crop_name",
            #                                     "crop_yr__c.vs.f","crop_yr__last_crop")
            # results_daily <- merge(results_daily, crop_yr__crop_years)
            #
            # nitrate$crop_yr__c.vs.f <- nitrate$c.vs.f # simply for merge colname matching
            # results_daily <- merge(results_daily, nitrate, by="crop_yr__c.vs.f")
            # results_daily <- results_daily[with(results_daily,order(date)),]
            # # results_daily[85:110,]
            # results_daily$leachN_kg.ha.day <- results_daily$h1d_dp_mm * results_daily$no3n /10
            # results_daily$same <- "same"

            # Observation Point VWC ####
            obs.daily = read.table(file = "./Obs_Node.out",
                                   header = FALSE, skip = 11, nrows = length(met.data2$Date))
            obs.headings = read.table(file = "./Obs_Node.out",
                                      header = FALSE, skip = 10, nrows = 1,
                                      stringsAsFactors = FALSE)
            obs.headings <- as.character(obs.headings[1,])
            obs.headings <- paste0(c('',rep("obs_01_",3),
                                     rep('obs_02_',3),
                                     rep('obs_03_',3),
                                     rep('obs_04_',3),
                                     rep('obs_05_',3),
                                     rep('obs_06_',3),
                                     rep('obs_07_',3),
                                     rep('obs_08_',3),
                                     rep('obs_09_',3),
                                     rep('obs_10_',3)),
                                   obs.headings)
            colnames(obs.daily) <- obs.headings
            obs.daily$Date <- as.POSIXct("1950-01-01")
            obs.daily$Date[1:length(obs.daily$Date)] <- seq(model.start2[j], model.end2[j], by="days")
            results_daily_2 <- merge(results_daily, obs.daily)

            ##### save daily output 2#####
            write.csv(x = results_daily_2, file = paste0('../../4_Results/2csvs/', SS.name, j+1, '.csv'))

            #save data for both
            results_daily_all <- rbind(results_daily_all, results_daily_2)

          }
        }

        write.csv(x= results_daily_all, file = paste0('../../4_Results/2csvs/', SS.name, 'All.csv'))

      }

      #load data (for Meghan's laptop that can't run Hydrus)
      #runs <- list.files('../../4_Results/1csvs')
      #for(i in 1:length(runs))
      #{
      # print(runs[i])
      #results_daily <- read.csv(paste0('../../4_Results/1csvs/', runs[i]))
      #results_daily$date <- as.POSIXct(results_daily$date)
      #}


      #this behavior is what is making plotting weird
      #paste(colnames(textures)[1], colnames(textures)[2]) #makes 1 string
      #paste(colnames(textures)[1:2]) #makes 2 strings
      #need to use collapse argument to paste column names into 1 string

      # plot volumetric water content results ####
      # i = 1

      # windows(1600,800)
      results_daily <- results_daily_all

      png(width = 1600, height = 800, res = 144,
          filename = paste0('../../4_Results/1VWCplots/', SS.name, '.png'))
      plot(results_daily$obs_01_theta ~ results_daily$Date, type = 'n', # no symbology
           main = SS.name, ylab = 'VWC',
           ylim = c(0,0.5))

      for(i in 1:9){
        if(obs[i,'plot.yn'] == 'y') {
          points(results_daily[,obs[i,'df.name']] ~ results_daily$Date,
                 pch = obs[i,"pch"], col = obs[i,'col'])
          lines(results_daily[,obs[i,'df.name']] ~ results_daily$Date,
                pch = obs[i,"pch"], col = obs[i,'col'])
        }
      }
      # points(results_daily$obs_01_theta ~ results_daily$date)

      text(x = as.POSIXct('2021-02-01'), y = 0.3,
           paste0('Obs1= ', min(results_daily$obs_01_theta), " to ",
                  max(results_daily$obs_01_theta)))

      # print VG parameters on plot
      # labels.names <- str_pad(colnames(textures)[3:length(colnames(textures))], width= 12)
      # labels.values <- str_pad(textures[i, 3:length(colnames(textures))],
      #                                                      width= 10)

      # labels.names <- str_pad(colnames(vg), width= 12)
      # labels.values <- str_pad(vg, width= 10)
      #
      # mtext(side = 3, line= 0.8, at= as.POSIXct("2022-07-01"),
      #       text= paste0(labels.names, collapse= " "),
      #       cex= 0.8)
      # mtext(side = 3, line= 0.05, at= as.POSIXct('2022-07-01'),
      #       text= paste0(labels.values, collapse= " ", sep= " "),
      #       cex= 0.8)

      dev.off()


      # calculate cumulative fluxes ####
      # calc cumulative deep perc
      results_daily$dp_cum <- 0
      results_daily[1,'dp_cum'] <- results_daily[1,'h1d_dp_mm']
      for(i in 2:length(results_daily$dp_cum)){
        results_daily[i,'dp_cum'] <- results_daily[i-1,'dp_cum'] +
          results_daily[i,'h1d_dp_mm']
      }

      # calc cumulative runoff
      results_daily$runoff_cum <- 0
      results_daily[1,'runoff_cum'] <- results_daily[1,'h1d_runoff_cm']
      for(i in 2:length(results_daily$runoff_cum)){
        results_daily[i,'runoff_cum'] <- results_daily[i-1,'runoff_cum'] +
          results_daily[i,'h1d_runoff_cm']
      }


      # calc cumulative evaporation
      results_daily$evap_cum <- 0
      results_daily[1,'evap_cum'] <- results_daily[1,'h1d_e_mm']
      for(i in 2:length(results_daily$evap_cum)){
        results_daily[i,'evap_cum'] <- results_daily[i-1,'evap_cum'] +
          results_daily[i,'h1d_e_mm']
      }

      # calc cumulative transpiration
      results_daily$tran_cum <- 0
      results_daily[1,'tran_cum'] <- results_daily[1,'h1d_t_mm']
      for(i in 2:length(results_daily$tran_cum)){
        results_daily[i,'tran_cum'] <- results_daily[i-1,'tran_cum'] +
          results_daily[i,'h1d_t_mm']
      }

      # plot cumulative fluxes ####
      # windows(1600,800)

      png(width = 1600, height = 800, res = 144,
          filename = paste0('../../4_Results/2fluxplots/', SS.name, '.png'))
      cum.min <- min(tail(results_daily[,'dp_cum'],1),
                     tail(results_daily[,'runoff_cum'],1),
                     tail(results_daily[,'evap_cum'],1),
                     tail(results_daily[,'tran_cum'],1))
      cum.max <- max(tail(results_daily[,'dp_cum'],1),
                     tail(results_daily[,'runoff_cum'],1),
                     tail(results_daily[,'evap_cum'],1),
                     tail(results_daily[,'tran_cum'],1))

      plot(results_daily$dp_cum ~ results_daily$Date, type = 'n', # no symbology
           main = SS.name,
           ylim = c(cum.min,cum.max),
           ylab = 'cumualtive partitioning (cm)',
           xlab = 'date')

      # points(results_daily$dp_cum ~ results_daily$date,
      #        col = 'black')
      # points(results_daily$runoff_cum ~ results_daily$date,
      #        col = 'blue')
      # points(results_daily$evap_cum ~ results_daily$date,
      #        col = 'gray30')
      # points(results_daily$tran_cum ~ results_daily$date,
      #        col = 'gray80')
      lines(results_daily$dp_cum ~ results_daily$Date,
            col = 'black', lty = 2)
      lines(results_daily$runoff_cum ~ results_daily$Date,
            col = 'blue')
      lines(results_daily$evap_cum ~ results_daily$Date,
            col = 'gray30')
      lines(results_daily$tran_cum ~ results_daily$Date,
            col = 'green')
      legend('topleft', legend = c('deep perc', 'runoff', 'evap', 'tran'),
             col = c('black', 'blue', 'gray30', 'green'), lty = c(2,1,1,1))

      legend('top', bty= "n",
             legend= c(paste("Cumulative Evaporation=",
                             round(tail(results_daily[,'evap_cum'],1),2)),
                       paste("Cumulative Transpiration=",
                             round(tail(results_daily[,'tran_cum'],1),2)),
                       paste("Cumulative DP=",
                             round(tail(results_daily[,'dp_cum'],1),2)),
                       paste("Cumulative Runoff=",
                             round(tail(results_daily[,'runoff_cum'],1),2))))

      dev.off()

      #Make combined plot ####
      ymax1 <- max(results_daily$Precip, results_daily$ET_cm)
      ymin1 <- min(results_daily$Precip, results_daily$ET_cm)

      date.seq <- seq(model.start1, model.end1, by= "months")
      date.seq <- date.seq[seq(from= 1, to= length(date.seq), by= 3)]

      png(width = 1600, height = 800, res = 144,
          filename = paste0('../../4_Results/2Combinedplots/', SS.name, '.png'))

      layout(matrix(c(1,2,3), ncol= 1))
      par(mar= c(1,4,2,4))
      par(oma=c(0,0,2,0))

      plot(results_daily$Precip ~ results_daily$Date,
           main= paste("Weather and Irrigation"),
           xaxt= "n",
           xlab= "",
           xaxs= "i",
           ylab= "",
           ylim= c(ymax1, ymin1),
           yaxs= "i",
           col.axis= "blue",
           type= "h",
           col= results_daily$col,
           lwd= 2)

      mtext(text= expression(bold("Precip (cm)")),
            side= 2, line=2, cex= 1, col= "blue")

      par(new= TRUE)
      plot(results_daily$ET_cm ~ results_daily$Date,
           xaxt= "n",
           xlab= "",
           xaxs= "i",
           yaxt= "n",
           ylab= "",
           ylim= c(ymin1, ymax1),
           yaxs= "i",
           type= "l",
           lwd= 2)

      abline(v= c(date.seq),
             lty= 2,
             lwd= 2,
             col= "gray75")

      label.list <- seq(0, round(ymax1, 0), 0.5)

      axis(side= 4, labels= c(label.list),
           at= c(label.list))

      mtext(text= expression(bold("ET (cm)")),
            side= 4, line=3, cex= 1)

      mtext(text= SS.name,
            side= 3, line= 1.5, cex= 1.5)

      par(mar= c(1,4,3,4))

      plot(results_daily$obs_01_theta ~ results_daily$Date,
           type = 'n', # no symbology
           main = "",
           ylab = 'VWC',
           xlab= "",
           xaxs= "i",
           xaxt= "n",
           ylim = c(0,0.5))

      title(main = "Soil Moisture", line= 1.5)

      for(i in 1:9){
        if(obs[i,'plot.yn'] == 'y') {
          points(results_daily[,obs[i,'df.name']] ~ results_daily$Date,
                 pch = obs[i,"pch"], col = obs[i,'col'])
          lines(results_daily[,obs[i,'df.name']] ~ results_daily$Date,
                pch = obs[i,"pch"], col = obs[i,'col'])
        }
      }
      # points(results_daily$obs_01_theta ~ results_daily$date)

      text(x = as.POSIXct(model.start1+(60*60*24*50)), y = 0.2,
           paste0('Obs1= ', min(results_daily$obs_01_theta), " to ",
                  max(results_daily$obs_01_theta)))

      leg.info <- obs[obs$plot.yn == "y" ,]

      legend("bottomleft", legend= c(leg.info$depth),
             pch= c(leg.info$pch), col= c(leg.info$col),
             lwd= 2, cex= 0.8)

      # print VG parameters on plot
      # labels.names <- str_pad(colnames(textures)[3:length(colnames(textures))], width= 12)
      # labels.values <- str_pad(textures[i, 3:length(colnames(textures))],
      #                                                      width= 10)

      if(model.number==0){

        # labels.names <- str_pad(colnames(vg)[1:6], width= 12)
        # labels.values <- str_pad(vg[1:6], width= 10)

      }else{

        labels.names <- str_pad(colnames(vg), width= 12)
        labels.values <- str_pad(vg, width= 10)

      }

      midpoint <- mean(c(model.start1, model.end1))

      # mtext(side = 3, line= 0.65, at= as.POSIXct(midpoint),
      #       text= paste0(labels.names, collapse= " "),
      #       cex= 0.7)
      # mtext(side = 3, line= 0.05, at= as.POSIXct(midpoint),
      #       text= paste0(labels.values, collapse= " ", sep= " "),
      #       cex= 0.7)

      abline(v= c(date.seq),
             lty= 2,
             lwd= 2,
             col= "gray75")

      par(mar= c(3,4,1,4))

      plot(results_daily$dp_cum ~ results_daily$Date,
           type = 'n', # no symbology
           main = "Cumulative Fluxes",
           xlab= "",
           xaxs= "i",
           xaxt= "n",
           ylim = c(cum.min,cum.max),
           ylab = 'cumualtive partitioning (mm)')

      lines(results_daily$dp_cum ~ results_daily$Date,
            col = 'black', lty = 2, lwd= 2)
      lines(results_daily$runoff_cum ~ results_daily$Date,
            col = 'blue', lwd= 2)
      lines(results_daily$evap_cum ~ results_daily$Date,
            col = 'gray30', lwd= 2)
      lines(results_daily$tran_cum ~ results_daily$Date,
            col = 'green', lwd= 2)
      legend('topleft', legend = c('deep perc', 'runoff', 'evap', 'tran'),
             col = c('black', 'blue', 'gray30', 'green'), lty = c(2,1,1,1))

      abline(v= c(date.seq),
             lty= 2,
             lwd= 2,
             col= "gray75")

      legend('top', bty= "n",
             legend= c(paste("Cumulative Evaporation=",
                             round(tail(results_daily[,'evap_cum'],1),2)),
                       paste("Cumulative Transpiration=",
                             round(tail(results_daily[,'tran_cum'],1),2)),
                       paste("Cumulative DP=",
                             round(tail(results_daily[,'dp_cum'],1),2)),
                       paste("Cumulative Runoff=",
                             round(tail(results_daily[,'runoff_cum'],1),2))))

      x.axis.year.month(Xrange= c(model.start1, model.end1),
                        year.cex = 1,
                        month.cex = 0.7,
                        axis.line = 0,
                        year.line = 2,
                        month.line = 0)
      dev.off()



      # simplify for now be omitting the following list output (2024-10-08)
      #   metadata <- list(vg = vg, crop_growth_param = crop_growth_param,
      #                    nitrate = nitrate, time.int = my.time.int,
      #                    obs.vector = obs.vector)
      #   mydata <- list(daily = results_daily, metadata = metadata)
      #   save(mydata, file = paste0("../../4_Results/1_RawDaily/",
      #                              out.subfolder.name, "/", my.filename,".RData"))
      #   return(list(daily = results_daily))
      # } else
      # metadata <- list(vg = vg, crop_growth_param = crop_growth_param,
      #                      nitrate = nitrate, time.int = my.time.int,
      #                  obs.vector = obs.vector)
      # mydata <- list(daily = "fail", metadata = metadata)
      # save(x =mydata, file = paste0("../../4_Results/1_RawDaily/",
      #                           out.subfolder.name, "/", my.filename,"FAIL",".RData"))
    } else
    {
      write.csv(x = 1:3, file = paste0('../../4_Results/2fails/', SS.name, 'FAIL.csv'))
    }
  }


}

# End ####
