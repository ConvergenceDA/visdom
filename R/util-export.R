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
    stop(paste("Unrecognized class.", class(df),"Can't figure out names to be fixed"))
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
#' @param checkId boolean indicating whether to enforce a check for an id column with an error message. This should
#' be true when exporting features or other id matched data and false otherwise.
#'
#' @param checkGeo boolean indicating whether to enforce a check zip5 columns with a warning message. This should
#' be true when exporting features that will be mapped.
#'
#' @return A copy of the original data frame that is cleaned up
#'
#' @export
cleanFeatureDF = function(features, checkId=TRUE, checkGeo=TRUE) {
  names(features) <- fixNames(features)
  # convert any factors to regular characters (otherwise the values are the factor indices)
  i <- sapply(features, is.factor)
  features[i] <- lapply(features[i], as.character)
  if(checkId) {
    if( ! c('id') %in% names(features)) {
      stop('id column required for exported data')
    }
  }
  if(checkGeo) {
    if( ! c('zip5') %in% names(features)) {
      print('[cleanFeatureDF] WARNING: VISDOM-web requires a zip5 geography column in features to produce maps')
    }
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
#' @param filePath optional path to the location where exported files should be written (if applicable). Default is \code{getwd()}
#'
#' @export
writeH5Data = function(data,fName,label, filePath=NA) {
  if( ! 'rhdf5' %in% rownames(installed.packages()) ) {
    print('Package rhdf5 does not appear to be installed. This comes from outside of CRAN.')
    print('Run the following to install it')
    print('source("https://bioconductor.org/biocLite.R")')
    print('biocLite("rhdf5")')
  }
  #source("https://bioconductor.org/biocLite.R")
  #biocLite("rhdf5")
  require(rhdf5)
  fName = paste(fName,'h5',sep='.')
  if( ! is.na(filePath ) ){
    dir.create(filePath, showWarnings = FALSE)
    fName = file.path( filePath, fName)
  }
  if(! file.exists(fName)) { rhdf5::h5createFile(fName) }
  rhdf5::h5write(data,fName,label)
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
  DBI::dbWriteTable(conn=conn, name=tableName, value=data, row.names=F, overwrite=overwrite, append=!overwrite) # write data frame to table
  DBI::dbDisconnect(conn)
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
#' @param filePath optional path to the location where exported files should be written (if applicable). Default is \code{getwd()}
#'
#' @export
writeCSVData = function(data, fName, label=NA, filePath=NA) {
  if( ! is.na(label) ) {
    fName = paste(fName, label, sep='_')
  }
  fName = paste(fName, 'csv', sep='.')
  if( ! is.na(filePath ) ){
    dir.create(filePath, showWarnings = FALSE)
    fName = file.path( filePath, fName)
  }
  write.csv(data, file=fName, row.names=F)
}

#' @title Save load shape results
#'
#' @description Exports standardized load shape clustering and assignment data into a
#' corresponding set of exported data tables
#' 
#' @param shape.results the shape feature results to export. These should be in the format returned by 
#' \code{visdomloadshape::shapeFeatures()}, as in 
#' \code{shapeFeatures(shapeCategoryEncoding(rawData=DATA_SOURCE$getAllData(), metaCols=1:4, encoding.dict=someDict))}
#' 
#' @param prefix a prefix to apply to the feature column names
#' 
#' @param format the data format for export. One of the values supported by the \code{format} paramater in \code{exportData()}
#' 
#' @param filePath optional path to the location where exported files should be written (if applicable). Default is \code{getwd()}
#'
#' @export
exportShapes = function(shape.results,prefix='',format='hdf5', filePath='.') {
  name = paste(prefix,'LoadShape',sep='')
  exportData(df=shape.results$shape.stats$cluster.counts,
             name=name,
             label='counts',
             format=format,
             checkId=TRUE,
             checkGeo=FALSE,
             filePath=filePath)
  exportData(df=shape.results$shape.stats$cluster.energy,
             name=name, label='sums',
             format=format,
             checkId=TRUE,
             checkGeo=FALSE,
             filePath=filePath)
  exportData(df=as.data.frame(shape.results$encoding.dict),
             name=name,
             label='centers',
             format=format,
             checkId=FALSE,
             checkGeo=FALSE,
             filePath=filePath)
  exportData(df=shape.results$encoding.dict.category.info,
             name=name,
             label='categoryMapping',
             format=format,
             checkId=FALSE,
             checkGeo=FALSE,
             filePath=filePath)
}

#' @title Export feature data into a selection of formats
#'
#' @description Runs the export function for a given data format on feature data
#'
#' @param df Data frame of feature data to export
#'
#' @param name Primary name of export, meaning file name or database table name
#'
#' @param label Optional data label for export formats. For example if not NA, this would be the name
#' of the data table within an hdf5 file or a suffix to the csv file name, as in \code{paste(name, label, sep='_')}
#'
#' @param format One of the supported formats for data export, currently 'hdf5', 'csv', or 'database'
#' 
#' @param checkId boolean control over whether to error out with a \code{stop()} if an id column is not present
#' 
#' @param checkGeo boolean control over whether to warn if a geographic field, \code{zip5} in this case, is not present.
#'
#' @param ... Pass through parameters for specific export methods. For example,
#' database export requires a conn object.
#'
#' @export
exportData = function(df,name,label=NA,format='hdf5', checkId=TRUE, checkGeo=TRUE, ...) {
  if ('matrix' %in% class(df) ) {
    print('Warning. Converting matrix to data.frame')
    df = as.data.frame(df)
  }
  print(paste('Exporting', name, label))
  fn = list(  hdf5=writeH5Data,
              hdf=writeH5Data,
              h5=writeH5Data,
              csv=writeCSVData,
              database=writeDatabaseData)
  df = cleanFeatureDF(df, checkId, checkGeo)
  fn[[format]](df, name, label, ... ) # call the format appropriate export function
}

#' @title Export feature run and load shape results
#'
#' @description Loads feature data and load shape clustering data from RData files and
#' saves them into the selected export format
#'
#' @param feature.data File path to an RData file with feature data data frame or the data frame itself
#'
#' @param shape.results.data Optional file path to an RData file containing load shape clustering results
#' or the results object itself. i.e. results from
#' \code{visdomloadshape::shapeFeatures(visdomloadshape::shapeCategoryEncoding())}
#'
#' @param format Export data format - one of the ones supported by exportData()
#'
#' @param prefix Optional prefix to put n froun of all feature names
#' 
#' @param filePath optional path to the directory where exported data should be written if the export type is a file. '.' by default.
#'
#' @export
exportFeatureAndShapeResults = function(feature.data, shape.results.data=NULL, format='hdf5', prefix='', filePath='.') {
  if( 'character' %in% class(feature.data) ) { # if character, it must be a file path for loading feature data
    print(paste('Using feature data from file', feature.data))
    load(feature.data)       # should provide var named featureDF
  } else {
    print('Using passed feature data')
    featureDF = feature.data
    rm('feature.data')
  }
  if(! is.null(shape.results.data)) {
    if( 'character' %in% class(shape.results.data) ) { # if character, it must be a file path for loading shape data
      print(paste('Using shape data from file',shape.results.data))
      load(shape.results.data) # should provide var named shape.results
    } else {
      shape.results = shape.results.data
      rm('shape.results.data')
    }
    print('Merging shape features into basic features')
    featureDF = mergeShapeFeatures(featureDF,shape.results)

    print(paste('Writing load shape data to',format))
    exportShapes(shape.results,prefix,format, filePath)
  }

  print(paste('Writing feature data frame to',format))
  exportData(df = featureDF,
             name = paste(prefix,'Basics',sep=''),
             label = 'basics',
             format = format,
             filePath=filePath)
}

