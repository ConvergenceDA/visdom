# Copyright 2016 The Board of Trustees of the Leland Stanford Junior University.
# Direct inquiries to Sam Borgeson (sborgeson@stanford.edu) 
# or professor Ram Rajagopal (ramr@stanford.edu)

#' @title
#' Main S3 class that defines the data source interface
#' 
#' @description 
#' The functions defined in the DataSource class must be implemented in the custom data source for each 
#' new data set, taking steps to convert data and normalize data access. It is good practice to ensure that
#' custom data sources "extend" DataSource by using the \code{DataSource()} constructor as the line that creates the
#' custom DataSource S3 objects, and then overriding functions as required.
#'
#' @details
#' \code{DataSource} Functions as a sort of interface definition and/or default implementation for 
#' all data source functions. It is good practice to have custom data sources extend this one because
#' it provides default implementations for all functions and a handfull of useful utility functions.
#' 
#' @seealso \code{\link{TestData}}
#' @export
DataSource = function() {
  obj = list (
    # to cache query results for performance reasons, a query cache directory must be specified
    #CACHE_DIR    = file.path(getwd(),'QUERY_CACHE'),
    # dataSources also require a config which is a named list
    # typically loaded using dbUtil$dbCfg with key=value pairs for
    # dbType (passed to dbDriver(dbType)),
    # host, dbname, user, password or other
    # driver specific dbConnect parameters
    #DB_CFG       = dbCfg('db.cfg')
    providesGas=F
  )
  
  obj$geoColumnName = 'geocode'
  obj$dateFormat = '%Y-%m-%d %H:%M'
  
  # utility function that returns a list of all the geographic code (ie zipcodes or census tracts) in the data set
  obj$getGeocodes     = function( )      { return(data.frame(c())) }
  obj$getIds          = function(geo=NA) { return(data.frame(c())) }
  obj$getGeoCounts    = function( )      { return(data.frame(c())) }
  obj$getAllData      = function(geo=NA) { return(data.frame(c())) }
  obj$getMeterData    = function(id,geo) { return(data.frame(c())) }
  obj$getGeoMetaData  = function(geo)    { return(data.frame(c())) }
  obj$getGeoForId     = function(id)     { return('badGEO') }

  obj$getAllGasData   = function()       { return(data.frame(c())) }
  obj$getGasMeterData = function(id,geo) { return(data.frame(c())) }
  obj$getWeatherData  = function(geo)    { return(data.frame(c())) }

  obj$getMeterDataClass = function(id, geocode=NULL, weather=NULL, data=NULL, rawData=NULL ) {

    r = MeterDataClass(  id       = id,
                         geocode  = geocode,
                         weather  = weather,
                         data     = data,
                         rawData  = rawData,
                         useCache = T,
                         doSG     = F  )
    return(r)
  }

  class(obj) = "dataSource"
  return(obj)
}



