require(RMySQL) # assuming data is in a MySQL database
library(visdom) # includes generic database support from util-dbUtil.R for things like connection management

MyDataSource = function(dbConfig='data_db.cfg', queryCache=paste(getwd(),'/','DATA_CACHE','/',sep='')){
  obj = DataSource()
  obj$CACHE_DIR    = queryCache
  obj$DB_CFG       = dbCfg(dbConfig)
  obj$db           = 'visdom_data'

  print(queryCache)
  #print(obj$DB_CFG)

  obj$accountTable = 'account'
  obj$meterTable   = 'meter_data'
  obj$weatherTable = 'local_weather'
  obj$interventionTable = 'intervention'
  
  obj$geoColumnName = 'zip5'
  
  # utility function that returns a list of all the zipcodes in the data set
  obj$getGeocodes = function(useCache=F,forceRefresh=F) {
    query    <- paste("select distinct zip5 from",obj$accountTable,'order by zip5')
    cacheFile = NULL
    if(useCache) { cacheFile='zipList.RData' }
    return(run.query(query,obj$DB_CFG,cacheDir=obj$CACHE_DIR,cacheFile=cacheFile,forceRefresh=forceRefresh)[[1]])
  }
  
  obj$getIds = function(geocode=NULL,useCache=F,forceRefresh=F) {
    cacheFile = NULL
    if(is.null(geocode)) { 
      if(useCache) {
        cacheFile = paste('meterids_all.RData',sep='') # only cache sp list for individual geos
      }
      # note that this query uses meter ids from the account table. You can also use the ids from the actual meter_table 
      # if the account table is unreliable, but these queries will take longer.
      query <- paste("select distinct meter_uuid as id from",obj$accountTable,'order by id') 
    } else {
      if(useCache) {
        cacheFile = paste('meterids_',geocode,'.RData',sep='') # only cache sp list for individual geos
      }
      query  <- paste("select distinct meter_uuid as id from ",obj$account," where zip5='",geocode,"' order by id",sep='') 
    }
    return(run.query(query,obj$DB_CFG,cacheDir=obj$CACHE_DIR,cacheFile=cacheFile,forceRefresh=forceRefresh,debug=T)[[1]])
  }

  
  obj$getAllData = function(geocode,useCache=F,forceRefresh=F) {
    cacheFile = NULL
    if(useCache) { cacheFile=paste('meterData_',geocode,'.RData',sep='') }
    query = paste(
      # note that VISDOM expects the unique identifiers to be called 'id' and dates to be called 'dates'
      # in the data framse returned from here
      'SELECT 
      meter_uuid as id, zip5, date as dates, 
      h1, h2, h3, h4, h5, h6, h7, h8, h9, h10,h11,h12,
      h13,h14,h15,h16,h17,h18,h19,h20,h21,h22,h23,h24 
      FROM ',obj$meterTable,
      " where zip5 ='",geocode,"' ",
      ' ORDER BY id, dates',sep='')
    data = run.query(query,obj$DB_CFG,cacheDir=obj$CACHE_DIR,cacheFile=cacheFile,forceRefresh=forceRefresh, debug=F)
    # convert last 24 columns to kWh from Wh
    data[,c(-23:0) + ncol(data)] = data[,c(-23:0) + ncol(data)] / 1000
    return(data)
  }
  
  
  obj$getAccountData = function(useCache=F, forceRefresh=F) {
    cacheFile = NULL
    if(useCache) { cacheFile=paste('accountData.RData',sep='') }
    # note that with a select * statement, this generic function returns whatever you are able to cram into the acocunt table.
    # this allows for generic support for merging in account features with the other features by account id.
    # however, note that the column names can't be controlled by * queries, so all, including the ids will have their db names.
    query = 'SELECT * from account order by meter_uuid, acocunt_uuid'
    data = run.query(query,obj$DB_CFG,cacheDir=obj$CACHE_DIR,cacheFile=cacheFile,forceRefresh=forceRefresh, debug=F)
    return(data)
  }

  obj$getInterventionData = function(useCache=F,forceRefresh=F) {
    cacheFile = NULL
    if(useCache) { cacheFile=paste('interventionData.RData',sep='') }
    query = paste(
      'SELECT * FROM ', obj$interventionTable,
      ' ORDER BY meter_uuid, account_uuid, install_date',sep='')
    data = run.query(query,obj$DB_CFG,cacheDir=obj$CACHE_DIR,cacheFile=cacheFile,forceRefresh=forceRefresh, debug=F)
    return(data)
  }
  
  obj$getMeterData = function(id,geocode=NULL) {
    # note that VISDOM expects the unique identifiers to be called 'id' and dates to be called 'dates'
    # in the data framse returned from here
    query = paste(
      'SELECT 
      meter_uuid as id, zip5, date as dates,
      h1, h2, h3, h4, h5, h6, h7, h8, h9, h10,h11,h12,
      h13,h14,h15,h16,h17,h18,h19,h20,h21,h22,h23,h24 
      FROM ',obj$meterTable,
      " where meter_uuid ='",id,"'",
      ' ORDER BY dates', sep='')
    data = run.query(query, obj$DB_CFG, debug=T)
    # convert last 24 columns to kW from W
    data[,c(-23:0) + ncol(data)] = data[,c(-23:0) + ncol(data)] / 1000
    if( 'date' %in% names(data)) {
      names(data)[which(names(data) %in% 'date')] = 'dates'
    }
    return(data)
  }
  
  obj$getWeatherData = function(geocode,useCache=F,forceRefresh=F) {
    cacheFile = NULL
    if(useCache) { cacheFile=paste('weather_',geocode,'.RData',sep='') }
    query = paste(
      'SELECT `date`, TemperatureF, Pressure, DewpointF, HourlyPrecip, WindSpeed
      FROM ',obj$weatherTable," where zip5 ='",geocode,"' ORDER BY DATE",sep='')
    data = run.query(query,DATA_SOURCE$DB_CFG,cacheDir=obj$CACHE_DIR,cacheFile=cacheFile,forceRefresh=forceRefresh)
    names(data) = tolower(names(data))
    return(data)
  }
  
  obj$getGeoForId = function(id,useCache=F, forceRefresh=F) {
    query    <- paste("select meter_uuid as id, zip5 from",obj$accountTable,' group by meter_uuid order by meter_uuid')
    cacheFile = NULL
    if(useCache) { cacheFile='idZipList.RData' }
    idZipData = run.query(query,obj$DB_CFG,cacheDir=obj$CACHE_DIR,cacheFile=cacheFile,forceRefresh=forceRefresh)
    return( idZipData[idZipData$id == id, 'zip5'] )
  }
  
  class(obj) = append(class(obj),"MyDataSource")
  
  return(obj)
}