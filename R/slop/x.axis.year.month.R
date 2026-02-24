
# Function to plot Years and Month labels on X axis
library(lubridate)

#' Title
#'
#' @param Xrange
#' @param year.cex
#' @param month.cex
#' @param tick.length.yr
#' @param tick.length.m
#' @param month.line
#' @param year.line
#' @param axis.line
#' @param year.lab.doy
#' @param months.labeled
#' @param month.letters
#'
#' @returns
#' @export
#'
#' @examples
x.axis.year.month <- function(Xrange = c(model.start,model.end), # vector, length 2, POSIXct format
                              year.cex = 2, # size of text for years
                              month.cex = 1, # size of text for months
                              tick.length.yr = -0.5, # length of tick for year
                              tick.length.m = -0.01, # length of tick for months
                              month.line = -0.5, # distance month label from axis
                              year.line = 3, # distance year label from axis
                              axis.line = -0.5, # position of axis line in y direction
                              year.lab.doy = 180, # day of year for year label location
                              months.labeled = c(1,4,7,10), # numbers of months to label
                              month.letters = 1) # month abbreviation if not 1, defaults to 3
{
  date.values <- seq(Xrange[1], Xrange[2], "day")
  # year labels
  years.all <- floor_date(date.values, "year") # floor all days to year
  years <- data.frame(unique(years.all)) # get unique list of years
  colnames(years)[which(names(years) ==
                          "unique.years.all.")] <- "date.day1"
  years$date.year.label <- years$date.day1 + 60*60*24*year.lab.doy # calc year label days
  years <- years[years$date.year.label  >= Xrange[1] & # subset for mid years in range
                   years$date.year.label  <= Xrange[2],]
  years$date.year.label <- floor_date(years$date.year.label, "day")
  years$year.txt <- substr(as.character(years$date.day1),1,4) # years as text
  mtext(years$year.txt, at = years$date.year.label, side=1, font=2, col='gray20',
        cex=year.cex, line=year.line) # print years at July 1)

  # month labels
  months.all <- floor_date(date.values, "month")
  months <- data.frame(unique(months.all))
  colnames(months)[which(names(months) ==
                           "unique.months.all.")] <- "date.day1"
  months$date.mid.month <- months$date.day1 + 15*24*60*60 # +15 days worth of secs to first day of months
  months <- months[months$date.mid.month >= Xrange[1] &
                     months$date.mid.month <= Xrange[2],]
  months$month.name1 <- format(months$date.day1, "%b")
  months$month.name2 <- substr(months$month.name1,1,1)
  months$month.number <- month(months$date.day1)
  # str(months)
  # head(months)

  # month labels
  if(month.letters == 1){
    axis.POSIXct(side = 1, at=months[months$month.number %in% months.labeled, 'date.mid.month'],
                 labels=months[months$month.number %in% months.labeled,'month.name2'],
                 cex.axis= month.cex, tck=0,
                 line=month.line, lwd=1, xaxs = "i")
  } else
    axis.POSIXct(side = 1, at=months[months$month.number %in% months.labeled, 'date.mid.month'],
                 labels=months[months$month.number %in% months.labeled,'month.name1'],
                 cex.axis= month.cex, tck=0,
                 line=month.line, lwd=0, xaxs = "i", las = 2)
  # month ticks
  axis.POSIXct(side = 1, at=months$date.day1,
               labels="", cex.axis= month.cex, tck=tick.length.m,
               line=axis.line, lwd=1, xaxs = "i")
  # year ticks
  axis.POSIXct(side = 1, at=years$date.day1,
               labels="", cex.axis= year.cex, tck=tick.length.yr,
               line=axis.line, lwd=1, xaxs = "i")
}


