# Copyright 2016 The Board of Trustees of the Leland Stanford Junior University.
# Direct inquiries to Sam Borgeson (sborgeson@stanford.edu)
# or professor Ram Rajagopal (ramr@stanford.edu)

#' @title
#' S3 class that holds and normalizes weather data
#' 
#' @description
#' Standard format and functions for loading, manipulating, and visualizing weather data in VISDOM.
#'
#' @section Parameters:
#'
#' @param geocode Primary geographic locator used by the data source. This is
#'   very often a zip code, but data sources can implement it as census block
#'   or any geographic category that covers all meters, with time series
#'   observations of weather data associated with a geocode.
#' @param doMeans Indicator of whether to calculate and retain daily and monthly
#'   mean values of observations. Disable when not in use for faster performance.
#' @param useCache Data caching option indicating to the data source whether cached
#'   data should be used to populate the class. Data sources implement their own
#'   caching, but the expectation is that they will hit the underlying data base
#'   once to get the data in the first place and then save it to a local cache from
#'   which it will be retrieved on subsequent calls. See \link{run.query} for details.
#' @param doSG Option indicating whether expensive solar geometry calculations
#'   that rely on the \link{solaR} package should be performed. Off by default for
#'   performance reasons. See \link{solarGeom} for details.
#'
#' @details
#' \code{WeatherData} is compatible by default with the output (i.e. database table) of
#'   python weather data scraping code that draws upon NOAA's NOAA Quality Controlled 
#'   Local Climatological Data (QCLCD) ftp server. See \code{http://github.com/sborgeson/local-weather}
#'   for code to download and convert weathre data into CSV files, the weather data zip files at
#'   \code{http://www.ncdc.noaa.gov/orders/qclcd/}, and the QCLCD homepage here
#'   \code{https://www.ncdc.noaa.gov/data-access/land-based-station-data/land-based-datasets/quality-controlled-local-climatological-data-qclcd}
#' @export
#' @seealso \code{\link{MeterDataClass}}
#' @examples
#' \dontrun{
#' DATA_SOURCE = TestData(10)
#' weather = WeatherData(geocode='12601')
#' plot(weather)
#' }
WeatherClass = function(geocode,doMeans=T,useCache=F,doSG=F){
  raw = DATA_SOURCE$getWeatherData(geocode,useCache=useCache)
  if(length(raw)==0) stop(paste('No data found for geocode',geocode))
  rawData = data.frame(
    dates = as.POSIXct(raw[,1],tz="America/Los_Angeles",origin='1970-01-01','%Y-%m-%d %H:%M:%S'),
    tout = raw[,'temperaturef'],
    pout = raw[,'pressure'],
    rain = raw[,'hourlyprecip'],
    dp   = raw[,'dewpointf']
    #wind = raw[,'windspeed']
  )
  days = unique(as.Date(rawData$dates))


  sg = list()
  if(doSG) {
    #TODO: make solarGeom generic to different geo codes
    sg = solarGeom(rawData$dates,zip=geocode)
  }


  # FYI, spring forward causes NA dates to find these:
  # which(is.na(dates))

  # TODO: do we need to do anything about the NA values?

  dayMeans    = c()
  dayMins     = c()
  dayMaxs     = c()
  dayLengths  = c()
  if(doMeans) {
    dayMeans  = dailyMeans(rawData)
    dayMins   = dailyMins(rawData)
    dayMaxs   = dailyMaxs(rawData)
    if(doSG) {
      dayLengths = dailySums(sg[,c('dates','daylight')])
    }
  }

  obj = list (
    zip      = geocode,
    geocode  = geocode,
    days     = days,
    dates    = rawData$dates,
    tout     = rawData$tout,
    sg       = sg,
    daylight = sg$daylight,
    rawData  = rawData,
    dayMeans = dayMeans,
    dayMins  = dayMins,
    dayMaxs  = dayMaxs,
    dayLengths = dayLengths,
    get      = function(x) obj[[x]],
    # Not sure why <<- is used here
    # <<- searches parent environments before assignment
    # http://stat.ethz.ch/R-manual/R-patched/library/base/html/assignOps.html
    set      = function(x, value) obj[[x]] <<- value,
    props    = list()
  )

  # returns relative humidity as decimal from 0 to 1 given temperature and dewpoint
  # using August-Roche-Magnus approximation: http://andrew.rsmas.miami.edu/bmcnoldy/humidity_conversions.pdf
  obj$rh = function(tout,dp) {
    a = 17.271
    b = 237.7
    tout = (tout - 32) * 5/9
    dp   = (dp   - 32) * 5/9
    rh = exp(a*dp/(b + dp)) / exp(a*tout/(b + tout))
  }

  obj$resample = function(newDates,name='tout') {
    # approx returns both the newDates and the interpreted values
    # but we only need the values
    if(! 'POSIXct' %in% class(newDates)) {
      newDates = as.POSIXct(newDates)
    }
    if(all(is.na(obj$rawData[,name]))) {
      return( rep(NA,length(newDates)))
    }
    # if all the dates match, return the clean data, otherwise, interpolate
    dateMatch = obj$dates %in% newDates
    
    #miss = which(! newDates %in% obj$dates)
    #print(paste(sum(dateMatch),length(newDates)))

    if(sum(dateMatch) == length(newDates)) {
      a = obj$rawData[,name][dateMatch]
      return(a)
    }
    a = approx(obj$dates, obj$rawData[,name], newDates, method="linear")[[2]]
    #b = a[2]
    if (all(is.na(a)) & name == 'tout'){
      print(paste(obj$dates[1],obj$dates[length(obj$dates)]))
      print(paste(newDates[1], newDates[length(newDates)]))
      stop("No weather data available")
    }
    return(a)
  }

  #obj <- list2env(obj)
  class(obj) = "WeatherClass"
  return(obj)
}


#' @export
plot.WeatherClass = function(w,colorMap=NA,main=NULL,issueTxt='',type='tout',...) {
  # needs a list, called w with:
  # w$geocode geocode for the title
  # w$days (1 date per day of data)
  # w$tout (vector of outside temperature readings)

  if(type=='tout') {
    if(is.null(main)) { main <- paste(w$geocode,'weather info',sep='') }
    if(length(colorMap) < 2) { colorMap = rev(colorRampPalette(RColorBrewer::brewer.pal(11,"RdBu"))(100)) } #colorMap = heat.colors(100)
    op <- par(no.readonly = TRUE)
    #par( mfrow=c(1,1), oma=c(2,2,3,0),mar=c(2,2,2,2))# Room for the title

    # image is messed up. we need to reverse the rows, convert to a matrix and transpose the data
    # to get the right orientation!
    image(matrix(w$tout,nrow=24),col=colorMap,axes=F,main='F')
    axis(1, at = seq(0, 1, by = 1/6),labels=0:6 * 4,mgp=c(1,0,0),tcl=0.5)
    if(length(w$days) > 16) {
      axis(2, at = seq(1,0, by = -1/15),labels=format(w$days[seq(1/16, 1, by = 1/16) * length(w$days)],'%m/%d/%y'),las=1,mgp=c(1,0,0),tcl=0.5)
    } else {
      axis(2, at = seq(1,0, by = -1/(length(w$days)-1)),labels=format(w$days,'%m/%d/%y'),las=1,mgp=c(1,0,0),tcl=0.5)
    }
    par(op)
  }
}
