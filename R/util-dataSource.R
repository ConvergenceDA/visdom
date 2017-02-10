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
#' @seealso \code{\link{TestData}}, \code{\link{sanityCheckDataSource}}
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
  obj$getGeocodes     = function( useCache=FALSE )          { return( c('default_geocode') ) }
  obj$getIds          = function(geo=NULL, useCache=FALSE ) { return( c() ) }
  obj$getGeoCounts    = function( useCache=FALSE )          { return( data.frame(c()) ) }
  obj$getAllData      = function(geo=NULL, useCache=FALSE)  { return( data.frame(c()) ) }
  obj$getMeterData    = function(id,geo, useCache=FALSE)    { return( data.frame(c()) ) }
  obj$getGeoMetaData  = function(geo=NULL, useCache=FALSE)  { return( data.frame(c()) ) }
  obj$getIdMetaData   = function(id=NULL, useCache=FALSE)   { return( data.frame(c()) ) }
  obj$getGeoForId     = function(id, useCache=FALSE)        { return( 'default_geocode' ) }

  obj$getAllGasData   = function(geo=NULL, useCache=FALSE)     { return(data.frame( c()) ) }
  obj$getGasMeterData = function(id, geo=NULL, useCache=FALSE) { return(data.frame( c()) ) }
  obj$getWeatherData  = function(geo, useCache=FALSE)          { return(data.frame( c()) ) }

  obj$getMeterDataClass = function(id, geocode=NULL, weather=NULL, data=NULL, rawData=NULL, useCache=FALSE ) {

    r = visdom::MeterDataClass(  id       = id,
                                 geocode  = geocode,
                                 weather  = weather,
                                 data     = data,
                                 rawData  = rawData,
                                 useCache = useCache,
                                 doSG     = FALSE  )
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
sanityCheckDataSource = function(DATA_SOURCE, useCache=FALSE) {
  print('DATA_SOURCE summary:')
  print( paste('providesGas:',  DATA_SOURCE$providesGas))
  print( paste('geoColumnName:',DATA_SOURCE$geoColumnName))
  print( paste('dateFormat:',   DATA_SOURCE$dateFormat))
  
  # array of all known ids. These are typically meter ids but can also be 
  # account id if the time series meter data includes the time varying account
  # assignments.
  print('Running getIds()')
  ids = DATA_SOURCE$getIds(geocode=NULL, useCache=useCache)
  
  if(class(ids) != 'character') {
    print(head(ids))
    stop(paste('DATA_SOURCE$getIds() should return character values. There are too many ways',
               'to loose leading zeros or blow past the maximum numeric size with data base identifiers',
               'and many ids are truly mixed character stringc, so it is important to treat them all the same way.',
               'NOTE: unless you are loosing leading zeros, you probably just need to call as.character() on',
               'your return values.'))
  }
  
  # Array of all geo codes in the data set.
  # These is typically zip codes, but can be other geographies
  # as long as they are consistent. 
  # Note that the census data matching and the mapping functionality of 
  # the feature browser web interface rely on geo code being zip code.
  print('Running getGeocodes()')
  geos = DATA_SOURCE$getGeocodes(useCache=useCache)
  
  if(class(geos) != 'character') {
    print(head(geos))
    stop('DATA_SOURCE$getGeocodes() should return character values. i.e. zip codes can have leading zeros - they are not numbers!')
  }
  
  
  # Reverse lookup of goe code given an id
  print('Running getGeoForId() - reverse lookup of geo code by id')
  geo = DATA_SOURCE$getGeoForId(ids[1],useCache=useCache)
  
  # data frame of meter data for an indiividual customer
  print('Running getMeterData() - loading meter data for a single id')
  mData = DATA_SOURCE$getMeterData(id = ids[1])
  
  sanityCheckMeterDataDF( mData )
  
  # data frame of all meter data for a given geo code location
  print('Running getAllData() - loads data for all ids assocaited with an optional geo code, or all available data when geo code is not specified.')
  allGeoData = DATA_SOURCE$getAllData(geocode = geos[1],useCache = useCache)
  
  sanityCheckMeterDataDF( allGeoData )
  
  # data frame of all weather data for a given geo code location
  print('Running getWeather() - loads weather data assocaited with an optional geo code, or all available weather data when geo code is not specified.')
  geoWeatherData = DATA_SOURCE$getWeather(geocode = geos[1], useCache = useCache)
  
  sanityCheckWeatherDataDF( geoWeatherData )
  
  print('Running getMeterDataClass() - loadas a meter data class object by id')
  mdc = DATA_SOURCE$getMeterDataClass(id=ids[1], useCache = useCache)
  
  print('Running plot.MeterDataClass() on loaded meter data')
  plot(mdc)
  
  testthat::expect_true(class(mdc$id) == 'character', 
                        'MeterDataClass$id should be a character string with class(mdc$id) == "character"')
  
  testthat::expect_true(class(mdc$geocode) == 'character',
                        'MeterDataClass$geocode should be a character string with class(mdc$geocode) == "character"')
  
  testthat::expect_true(class(mdc$weather$geocode) == 'character',
                        'MeterDataClass$weather$geocode should be a character string with class(mdc$weather$geocode) == "character"')
  
  # Arrays of the subsets of ids found in each geo code location
  print('Running getIds() for each geocode')
  for(geo in geos) {
    geoids = DATA_SOURCE$getIds(geo,useCache = useCache)
    sprintf('  %s (%d ids)', geo, length(geoids))
  }
  
  # Customer account data associated with the meter ids, will typically have custom columns
  # based on whatever is know about the customers and is not tightly bound to the 
  # generic feature extraction process. This supplemental customer data, such as demographics,
  # customer types, rate plans, program participation flags, etc. will often become 
  # pass-through features that are added to the feature data frame prior to saving it.
  print('Running getIdMetaData() - meta data available at the id level, for example rate type or NAICS code')
  accounts = DATA_SOURCE$getIdMetaData(useCache=useCache)
  print(dim(accounts))
  
  # loop over all geo codes to load the meter data for each.
  # The is a full data sample check that things are working and 
  # can be used to populate data caches.
  print('Sanity checking data loading for all geo codes')
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
  print('Sanity checking weather data loading for all geo codes')
  n = length(geos)
  i = 0
  for (geo in geos) {
    i = i+1
    print(paste('Working on weather for',geo,'(',i,'/',n,')'))
    tic('geo load')
    geoData = DATA_SOURCE$getWeather(geocode = geo,useCache = useCache)
    print(paste(nrow(geoData),'weather observations.'))
    toc('geo load')
  }
}

sanityCheckWeatherDataDF = function(wData) {
  requiredCols = c("temperaturef", "pressure", "dewpointf", "hourlyprecip"  )
  dateCols    = c('date', 'dates')
  if( ! all( requiredCols %in% names(wData) ) | ! any(dateCols %in% names(wData)) ) {
    missing = paste( requiredCols[which(! requiredCols %in% names(wData))], collapse=', ' )
    stop(paste('Required named weather data columns are missing:', missing,
               'Note that the columns are required, but they can be empty if no data is available.'))
  }
  if( ! any(dateCols %in% names(wData)) ) {
    missing = paste( dateCols, collapse=', ' )
    stop(paste('A date column in', missing, 'is required'))
  }
  print('Checking date parsing for weather data')
  dateCol = names(wData) %in% dateCols
  dates = as.POSIXct(wData[,dateCol],tz="America/Los_Angeles",origin='1970-01-01','%Y-%m-%d %H:%M:%S')
}

# assert many things about the structure of the 'raw' meter data here...
sanityCheckMeterDataDF = function(mData) {
  # must have an id column
  print('test that meter data has an id column')
  testthat::expect_true('id' %in% names(mData))
  # must have a geo code column
  print('test that meter data has a geo code column')
  testthat::expect_true(DATA_SOURCE$geoColumnName %in% names(mData),info = DATA_SOURCE$geoColumnName)
  # must have a dates column
  print('test that meter data has a dates column')
  testthat::expect_true('dates' %in% names(mData))
  
  # check parsing behavior
  print('test that date format is parsable')
  day = as.POSIXct(paste(mData[1,'dates'],'00:00'),tz="America/Los_Angeles", DATA_SOURCE$dateFormat)
  print(paste('First day:',day))
  
  geocode = mData[1,DATA_SOURCE$geoColumnName]
  print(paste('Geo code:',geocode))
  
  ncol = ncol(mData)
  readings = NA
  if(ncol > 97) {
    print('Meter data has more than 97 columns, assuming that the readings are 96 intervals per day (15 min)')
    readings = mData[1,(ncol-96+1):ncol] # last 96 columns are assumed to be the readings.
  } else {
    print('Meter data has 97 or fewer columns, assuming that the readings are 24 intervals per day')
    readings = mData[1,(ncol-24+1):ncol] # last 24 columns are assumed to be the readings.
  }
  print('First day of readings')
  print(readings)
  print('test that meter readings are numeric')
  print(paste('The way the code parses out the meter readings form the rest of the data',
              'is that it pulls the last 24 or 96 columns. If you have appended columns',
              'after the meter data, they will cause errors. Here we try to catch that by',
              'checking that all values are numeric. However, if you have added a numeric',
              'column after the meter readings, it will pass this test while ultimately',
              'corrupting your data.'))
  testthat::expect_true( all( sapply(readings,class) %in% c('numeric','integer') ) )
}
