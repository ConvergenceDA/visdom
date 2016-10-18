# Copyright 2016 The Board of Trustees of the Leland Stanford Junior University.
# Direct inquiries to Sam Borgeson (sborgeson@stanford.edu)
# or professor Ram Rajagopal (ramr@stanford.edu)


#' @title Prepare data frame column names for export
#' 
#' @description Removes punctuation from data frame column names, replacing all with underscores
#' and removing underscores that are repeated one after another
#' 
#' @param df The data frame whose columns are to be renamed
#' 
#' @param prefix An optional prefix to place in front of all column names
#' 
#' @return A data frame identical to the one passed in, but with new column names.
#' 
#' @export
fixNames = function(df,prefix='') {
  if('data.frame'%in% class(df) ) {
    nms = names(df)
  } else if ('character' %in% class(df) ) {
    nms = df
  } else {
    stop(paste("Unrecognized class. Can't figure out names to be fixed:",class(df)))
  }
  fixed = gsub('[[[:punct:] ]','_',nms) # change ! " # $ % & ' ( ) * + , - . / : ; < = > ? @ [ \ ] ^ _ ` { | } ~ to underscores
  fixed = gsub('__+','_',fixed) # remove double or more underscores
  fixed = gsub('_$','',fixed)   # remove trailing underscores
  fixed = gsub('^_','',fixed)   # remove leading underscores
  fixed = paste(prefix,fixed,sep='')
  return(fixed)
}

#' @title Merge load shape features into feature data frame
#' 
#' @description Pulls load shape features from a shape results object and appends them to 
#' an existing feature data frame
#' 
#' @param features Data frame of feature data
#' 
#' @param shape.results Load shape clustering and assignment results object to pull features from
#' 
#' @return A data frame identical to the one passed in, but with new laod shape feature columns
#' 
#' @export
mergeShapeFeatures = function(features,shape.results) {
  newFeatures = merge(features,shape.results$shape.features[,c('id','entropy')],by.x='id',by.y='id',all.x=T)
  catCounts = shape.results$shape.stats$category.counts
  catTotals = rowSums(catCounts[,-1])
  catCounts[,-1] = catCounts[,-1] / catTotals
  names(catCounts) = gsub(pattern='count',replacement='pct',names(catCounts))
  newFeatures = merge(newFeatures,catCounts,by.x='id',by.y='id',all.x=T)
  return(newFeatures)
}

#' @title Clean up feature data in preparation for saving
#' 
#' @description This function renames data columns for export via fixNames(), converts factors 
#' into characters, and checks for id and zip5 colums
#' 
#' @param features The data frame of feature to be cleaned up
#' 
#' @return A copy of the original data frame that is cleaned up
#' 
#' @export
cleanFeatureDF = function(features) {
  names(features) <- fixNames(features)
  # convert any factors to regular characters (otherwise the values are the factor indices)
  i <- sapply(features, is.factor)
  features[i] <- lapply(features[i], as.character)
  if( ! c('id') %in% names(features)) {
    stop('id column required for exported data')
  }
  if( ! c('zip5') %in% names(features)) {
    print('WARNING: VISDOM-web requires a zip5 geography column to produce maps')
  }
  return(features)
}

#' @title Write feature data frame to an hdf5 file
#' 
#' @description Write feature data frame to an hdf5 file
#' 
#' @param data The feature data frame to be written
#' 
#' @param fName The name of the hdf5 formatted file to write the data to
#' 
#' @param label The name of the data table within the hdf5 file
#' 
#' @export
writeH5Data = function(data,fName,label) {
  #source("https://bioconductor.org/biocLite.R")
  #biocLite("rhdf5")
  require(rhdf5)
  if(! file.exists(fName)) { h5createFile(fName) }
  h5write(data,fName,label)
}

#' @title Write feature data frame to a database
#' 
#' @description Write feature data frame to a database
#' 
#' @param data The feature data frame to be written
#' 
#' @param tableName The name of the table to write the data to
#' 
#' @param label Unused, but present for compatibility with other write* fucntions
#' 
#' @param conn A DBI dbConnection object to the database that will host the table
#'
#' @param overwrite Boolean indicator for whether the data written should overwrite any existing table or append it
#' 
#' @export
writeDatabaseData = function(data, tableName, label=NULL, conn, overwrite=TRUE) { # con <- dbConnect(SQLite(), dbname="filename.sqlite")
  print(paste(tableName))
  dbWriteTable(conn=conn, name=tableName, value=data, row.names=F, overwrite=overwrite) # write data frame to table
  dbDisconnect(conn)
  #print('No SQLite support yet!')
  #if (require("RSQLite")) {
  #  con <- dbConnect(RSQLite::SQLite(), ":memory:")
    
  #  dbDisconnect(con)
  #}
  #dbWriteTable(conn=db, name="allBasics", value=basicsPlus, row.names=F, overwrite=T) # write data frame to table
  #dbListFields(db,"allBasics")                                                   # list of column names
  #db_allBasics = dbReadTable(conn=db, name="allBasics")                           # load data frame from table
  #dbDisconnect(db)                                                                # cleanup: close the file
  
}


#' @title Write feature data frame to a csv file
#' 
#' @description Write feature data frame to a csv file
#' 
#' @param data The feature data frame to be written
#' 
#' @param fName The name of the csv file to write the data to
#' 
#' @param label Unused, but present for compatibility with other write* fucntions
#' 
#' @export
writeCSVData = function(data,fName,label=None) {
  write.csv(data, file=fName, row.names=F)
}

#' @title Save load shape results 
#' 
#' @description Exports standardized load shape clustering and assignment data into a 
#' corresponding set of exported data tables
#' 
#' @export
exportShapes = function(shape.results,prefix='',format='hdf5') {
  name = paste(prefix,'LoadShape',sep='')
  exportData(shape.results$shape.stats$cluster.counts,  name, 'counts',          format)
  exportData(shape.results$shape.stats$cluster.energy,  name, 'sums',            format)
  exportData(shape.results$encoding.dict,               name, 'centers',         format)
  exportData(shape.results$encoding.dict.category.info, name, 'categoryMapping', format)
}

#' @title Export feature data into a selection of formats
#' 
#' @description Runs the export function for a given data format on feature data
#' 
#' @param df Data frame of feature data to export
#' 
#' @param name Primary name of export, meaning file name or database table name
#' 
#' @param label Data label required for some export formats. For example the name 
#' of the data table within an hdf5 file.
#' 
#' @param format On of the supported formats for data export, currently 'hdf5', 'csv', or 'database'
#' 
#' @param ... Pass through parameters for specific export methods. For example, 
#' database export requires a conn object.
#' 
#' @export
exportData = function(df,name,label=None,format='hdf5', ...) {
  ext = list(hdf5='h5',hdf='h5',h5='h5',csv='csv',database='')
  fn = list(  hdf5=writeH5Data,
              hdf=writeH5Data,
              h5=writeH5Data,
              csv=writeCSVData, 
              database=writeDatabaseData)
  fName = paste(name,'.',ext[[format]],sep='')
  df = cleanFeatureDF(df)
  fn[[format]](df, fName, label, ... ) # call the format appropriate export function
}

#' @title Export feature run and load shape results 
#' 
#' @description Loads feature data and load shape clustering data from RData files and 
#' saves them into the selected export format
#' 
#' @param feature.rdata File path to an RData file with feature data
#' 
#' @param shape.results.rdata Optional file path to an RData file containing load shape clustering results
#' 
#' @param format Export data format - one of the ones supported by exportData()
#' 
#' @param prefix Optional prefix to put n froun of all feature names
#' 
#' @export
exportFeatureAndShapeResults = function(feature.rdata, shape.results.rdata=NULL, format='hdf5', prefix='') {

  load(feature.rdata)       # should provide var named featureDF
  if(! is.null(shape.results.rdata)) {

    print(paste('Using shape data',shape.results.rdata))
    load(shape.results.rdata) # should provide var named shape.results

    print('Merging shape features into basic features')
    featureDF = mergeShapeFeatures(featureDF,shape.results)

    print(paste('Writing load shape data to',format))
    exportShapes(shape.results,prefix,format)
  }
  featureDF = cleanFeatureDF(featureDF)

  print(paste('Writing feature data frame to',format))
  exportData(featureDF,paste(prefix,'Basics',sep=''),'basics',format)
}

