# Copyright 2016 The Board of Trustees of the Leland Stanford Junior University.
# Direct inquiries to Sam Borgeson (sborgeson@stanford.edu)
# or professor Ram Rajagopal (ramr@stanford.edu)

#' @title
#' Implementation of a data source that generates synthetic data for testing and examples
#' 
#' @description
#' Example implementation of the data source functions as a coherent data source. This one 
#' simply generates random synthetic data that conform to the formats required by \code{\link{MeterDataClass}}
#' and \code{\link{WeatherClass}}, returning valid, but meaningless data for all calls.
#'
#' @param n Number of meters for the data source to provide synthetic data for: determines 
#'   the return values for functions like \code{DATA_SOURCE$getIds()} and and \code{DATA_SOURCE$getAllData()}
#'
#' @details
#' \code{TestData} is instantiated with a certain number of meters to generate data for. It then generates
#' random data for that number and consumes that data to support the rest of its functions. This class
#' can be used for examples, learning how data sources work, and providing data for tests of feature algorithms
#' and meter data related classes.
#' 
#' @seealso \code{\link{DataSource}}
#' 
#' @examples
#' \dontrun{
#' DATA_SOURCE = TestData(n=100)
#' idList = DATA_SOURCE$getIds()
#' dataMatrix = DATA_SOURCE$getAllData()
#' }
#' @export
TestData = function( n=100 ) {
  obj = DataSource( )

  obj$n = n

  obj$getHourlyAlignedData = function( n=NULL ) {
    # generate n meters worth of testable data
    if(is.null(n)) { n = obj$n }
    dates = as.Date('2013-01-01') + 0:364
    data = data.frame(  id         = rep(paste('meter',1:n,sep='_'),each=365),
                        customerID = rep(paste('cust',1000 + 1:n,sep='_'),each=365),
                        geocode    = '94305',
                        dates      = rep(dates,n))
    # no factors...
    data$id         = as.character(data$id)
    data$customerID = as.character(data$customerID)
    data$geocode = as.character(data$geocode)
    # generate fake data nx24
    obs = matrix(rnorm(n*365*24,0,2) + rnorm(1,10,1.5),ncol=24)
    data = cbind(data,obs)
    names(data)[5:28] = paste('H',1:24,sep='')
    return(data)
  }
  
  obj$getIdMetaData = function(useCache=TRUE) {
    ids = obj$getIds()
    return( data.frame( id = ids, status='good', income = 'myob' ) )
  }

  obj$getAllData = function(geocode,useCache=TRUE) {
    return( obj$getHourlyAlignedData( ) )
  }

  obj$getMeterData = function(id, geo=NULL,useCache=TRUE) {
    return( obj$getHourlyAlignedData( n=1 ) )
  }

  obj$getIds = function(geocode=NULL, useCache=TRUE) {
    # here we ignore the geo code, but a real data source would return 
    # just the ids associated with the passed geocode (if applicable) 
    return( unique(obj$getHourlyAlignedData()$id ) )
  }
  
  obj$getGeocodes = function( useCache=TRUE ) {
    return(c('94305'))
  }

  obj$getGeoForId = function(id, useCache=TRUE) {
    return('94305')
  }

  obj$getWeatherData = function( geocode, useCache=T ) {
    dates = as.POSIXct('2013-01-01',tz = "America/Los_Angeles" ) + 0:(365 * 24 - 1) * 3600
    data = data.frame(
      date         = dates,
      temperaturef = (c(0:5,5:0)*3)[as.POSIXlt(dates)$mon + 1] + rep(rnorm(n=365, mean=0, sd=0.5), each=24) + rep( rep(c(54,55,56,57,58,56), each=4), 365 ),
      pressure     = rep( rep(19,24), 365 ),
      hourlyprecip = rep( c( rep(0,12),rep(1,2),rep(0,10) ), 365 ),
      dewpointf    = rep( rep(55,24), 365 )
    )

    return(data)
  }

  class(obj) = append(class(obj),"TestData")

  return(obj)
}
