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

# Internal function for parsing name=value configuration file
parseConfig = function(config_path) {
     cfg = read.csv(config_path,sep="=",header=F)
     cfg[] <- lapply(cfg, as.character)
     cfg = as.list(setNames(cfg$V2,cfg$V1))
     return(cfg)
}

#' @title Get the database id of a feature_set/run combination
#'
#' @description Load user-specified run config file and return a unique numeric
#'   id from the feature_runs metadata table. Create the feature_runs table if
#'   it does not exist.
#'
#' @param conn A database connection, usually obtained from \code{conf.dbCon} or \code{\link{DBI::dbConnect}}
#'
#' @param runConfig The run configuration file with key-value pairs of
#'   feature_set, feature_set_description, run_name and run_description. See
#'   1inst/feature_set_run_conf/exampl_feature_set.conf1 for an example.
#'
#' @export
getRunId = function(conn, runConfig) {
     cfg = parseConfig(runConfig)
     
     # Create the feature_runs table if it doesn't exist.
     table_name = "feature_runs"
     if( ! DBI::dbExistsTable(conn, table_name) ) {
          sql_dialect = getSQLdialect(conn)
          create_table_path = file.path(system.file(package='visdom'), "sql", 
                                        paste("feature_runs.create.", sql_dialect, ".sql", sep=""))
          if( file.exists(create_table_path) ) {
               sql_create = readChar(create_table_path, file.info(create_table_path)$size)
          } else {
               stop(sprintf(paste(
                    "The sql file to create the %s table does not exist for",
                    "SQL dialect %s at path %s. You need to manually create the feature_run",
                    "table in your database, or store the create statement(s) at that path."),
                    table_name, sql_dialect, create_table_path))
          }
          rows_affected = DBI::dbExecute(conn, sql_create)
     }
     
     # Validate incoming data. Start by determining length of varchar columns. The length column may or may not be available outside of the MySQL database driver.
     sql_schema_query = DBI::sqlInterpolate(
          conn,
          paste("SELECT *", 
                "FROM", DBI::dbQuoteIdentifier(conn, table_name), 
                "limit 0"))
     rs <- DBI::dbSendQuery(conn, sql_schema_query)
     column_info = DBI::dbColumnInfo(rs)
     DBI::dbClearResult(rs)
     # Now check length of data against length of columns.
     for(column in names(cfg)) {
          dat_length = nchar(cfg[column])
          allowed_length = column_info[column_info$name == column, ]$length
          if(dat_length > allowed_length) {
               stop(sprintf(paste(
                    "Data for %s from run config file %s is too long.", 
                    "Max character length is %d, and the data is %d characters long."),
                    column, runConfig, allowed_length, dat_length))
          }
     }
     
     # Look for an existing record in the table based on feature_set and run_name
     sql_query = DBI::sqlInterpolate(
          conn,
          paste("SELECT *", 
                "FROM", DBI::dbQuoteIdentifier(conn, table_name), 
                "WHERE feature_set = ?feature_set and run_name = ?run_name"),
          feature_set=cfg$feature_set,
          run_name=cfg$run_name)
     dat = DBI::dbGetQuery(conn, sql_query)
     
     # Insert a record for this run if one does not exist
     if( nrow(dat) == 0 ) {
          sql_insert = DBI::sqlInterpolate(
               conn,
               paste("INSERT INTO", DBI::dbQuoteIdentifier(conn, table_name), 
                     "(feature_set, feature_set_description, run_name, run_description)",
                     "VALUES (?feature_set, ?feature_set_description, ?run_name, ?run_description)"),
               feature_set = cfg$feature_set,
               feature_set_description = cfg$feature_set_description,
               run_name = cfg$run_name,
               run_description = cfg$run_description)
          rows_affected = DBI::dbExecute(conn, sql_insert)
          if( rows_affected != 1) {
               stop(sprintf("Error inserting into %s table data from %s.", table_name, runConfig))
          }
          # Look up the id now.
          dat = DBI::dbGetQuery(conn, sql_query)
     }
     
     return(dat$id)
}

#' @title Write feature data frame to a database
#'
#' @description Write feature data frame to a database using a \code{\link{DBI::dbWriteTable}} call
#'
#' @param data The feature data frame to be written
#'
#' @param name Unused, but present for compatibility with other write* fucntions
#'
#' @param label Unused, but present for compatibility with other write* fucntions
#'
#' @param conn A DBI dbConnection object to the database that will host the table
#'
#' @param overwrite Boolean indicator for whether the data written should overwrite any existing table or append it
#' 
#' @param runConfig Path to a run configuration file with names and descriptions
#'   of the feature set and run. See 
#'   `inst/feature_set_run_conf/exampl_feature_set.conf` for an example.
#'
#' @export
writeDatabaseData = function(data, name=NULL, label=NULL, conn, overwrite=TRUE, runConfig) { # con <- dbConnect(SQLite(), dbname="filename.sqlite")
  # Use cbind so runId is the first column. 
  data = cbind(runId=getRunId(conn, runConfig), data)
  tableName = parseConfig(runConfig)$feature_set
  DBI::dbWriteTable(conn=conn, name=tableName, value=data, row.names=F, overwrite=overwrite, append=!overwrite) # write data frame to table
  
  # Update the time in the metadata table
  table_name = "feature_runs"
  sql_update = DBI::sqlInterpolate(
       conn,
       paste("UPDATE", DBI::dbQuoteIdentifier(conn, table_name), 
             "SET update_time=CURRENT_TIMESTAMP"))
  rows_affected = DBI::dbExecute(conn, sql_update)
  if( rows_affected != 1) {
       stop(sprintf("Error updating time in %s table.", table_name))
  }

  # DBI::dbDisconnect(conn)
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
  print(paste('Exporting name:', name, 'label:', label))
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

