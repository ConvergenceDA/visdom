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

#' @title
#' Function to exercise the core features and calls of a DataSource
#' 
#' @description 
#' This function exercises the core functions of a \code{\link{DataSource}}, allowing DataSource authors to 
#' check if their DataSource meets the minimal obligations of data retrieval and formatting.
#' It can also be used to pre-populate data caches before a feature run.
#'
#' @param DATA_SOURCE The DataSource to be tested. In VISDOM, there is a convention to put the 
#' active \code{DataSource} into the global environment with the name DATA_SOURCE, so this code follows that
#' naming convention, even though you are passing in the data source.
#' 
#' @param useCache Boolean to control whether or not the DataSource should rely upon its data cache to
#' retrieve data. If True and called with an empty cache, the function can be used to pre-populate the 
#' caches with id lists, geo code, meter data by geo code, and weather data by geo code.
#'
#' @details
#' While these checks are not comprehensive, they represent the main functions exercised during a feature 
#' extraction run. DataSource authors should feel free to add functions to their DataSource and alter 
#' existing function signatures to meet their individual requirements. As long as this function still runs
#' without errors, generic iterator functions should still work. The iterator relies on the ability to break
#' ids and meter data down to geo code level sub-groups, to request all weather and meter data for a zip code,
#' and to be able to instantiate a MeterDataClass object with the returned meter data and weather data. 
#' 
#' If this function no longer runs on your \code{DataSource}, then advanced users can write their own 
#' iteration methods that break down the feature extraction problem differently.
#' 
#' @seealso \code{\link{DataSource}}
#' @export
sanityCheckDataSource = function(DATA_SOURCE, useCache=F) {
  print('DATA_SOURCE summary:')
  print( paste('providesGas:',  DATA_SOURCE$providesGas))
  print( paste('geoColumnName:',DATA_SOURCE$geoColumnName))
  print( paste('dateFormat:',   DATA_SOURCE$dateFormat))
  
  # array of all known ids. These are typically meter ids but can also be 
  # account id if the time series meter data includes the time varying account
  # assignments.
  ids = DATA_SOURCE$getIds(useCache=useCache)
  
  # Array of all geo codes in the data set.
  # These is typically zip codes, but can be other geographies
  # as long as they are consistent. 
  # Note that the census data matching and the mapping functionality of 
  # the feature browser web interface rely on geo code being zip code.
  geos = DATA_SOURCE$getGeocodes(useCache=useCache)
  
  # Arrays of the subsets of ids found in each geo code location
  for(geo in geos) {
    ids = DATA_SOURCE$getIds(geo,useCache = useCache)
  }
  
  # Customer account data associated with the meter ids, will typically have custom columns
  # based on whatever is know about the customers and is not tightly bound to the 
  # generic feature extraction process. This supplemental customer data, such as demographics,
  # customer types, rate plans, program participation flags, etc. will often become 
  # pass-through features that are added to the feature data frame prior to saving it.
  accounts = DATA_SOURCE$getAccountData(useCache=useCache)
  
  # Reverse lookup of goe code given an id
  geo = DATA_SOURCE$getGeoForId(ids[1],useCache=useCache)
  
  # data frame of meter data for an indiividual customer
  mData = DATA_SOURCE$getMeterData(id = ids[1])
  
  # assert some things about the structure of the meter data here,...
  # must have an id column
  testthat::expect_true('id' %in% names(mData))
  # must have a geo code column
  testthat::expect_true(DATA_SOURCE$geoColumnName %in% names(mData),info = DATA_SOURCE$geoColumnName)
  # must have a dates column
  testthat::expect_true('dates' %in% names(mData))
  
  # check parsing behavior
  day = as.POSIXct(paste(mData[1,'dates'],'00:00'),tz="America/Los_Angeles", DATA_SOURCE$dateFormat)
  print(paste('First day:',day))
  
  geocode = mData[1,DATA_SOURCE$geoColumnName]
  print(paste('Geo code:',geocode))

  ncol = ncol(mData)
  readings = NA
  if(ncol > 97) {
    print('Meter data has more than 97 columns, assuming that the readings are 96 intervals per day (15 min)')
    readings = mData[1,(ncol-96+1):ncol] # last 96 columns are assumed to be the readings.
    print(readings)
  } else {
    print('Meter data has 97 or fewer columns, assuming that the readings are 24 intervals per day')
    readings = mData[1,(ncol-24+1):ncol] # last 24 columns are assumed to be the readings.
  }
  print('First day of readings')
  print(readings)
  testthat::expect_true( all( sapply(readings,class) %in% c('numeric','integer') ) )
  
  
  # data frame of all meter data for a given geo code location
  allGeoData = DATA_SOURCE$getAllData(geocode = geos[1],useCache = useCache)
  
  # loop over all geo codes to load the meter data for each.
  # The is a full data sample check that things are working and 
  # can be used to populate data caches.
  n = length(geos)
  i = 0
  for (geo in geos) {
    i = i+1
    print(paste('Working on meter data for',geo,'(',i,'/',n,')'))
    tic('geo load')
    geoData = DATA_SOURCE$getAllData(geocode = geo,useCache = useCache)
    print(paste(nrow(geoData),'days of data.'))
    toc('geo load')
  }
  
  # loop over all geo codes to load the weather data for each.
  # The is a full data sample check that things are working and 
  # can be used to populate data caches.
  n = length(geos)
  i = 0
  for (geo in geos) {
    i = i+1
    print(paste('Working on weather for',geo,'(',i,'/',n,')'))
    tic('geo load')
    geoData = DATA_SOURCE$getWeather(geocode = geo,useCache = T)
    print(paste(nrow(geoData),'weather observations.'))
    toc('geo load')
  }
}


