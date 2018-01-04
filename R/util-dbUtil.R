# Copyright 2016 The Board of Trustees of the Leland Stanford Junior University.
# Direct inquiries to Sam Borgeson (sborgeson@stanford.edu) 
# or professor Ram Rajagopal (ramr@stanford.edu)

#' @title
#' Parse standard database configuration file format
#' 
#' @description
#' Read database config file with key=value format:
#' \code{key=value
#' key2=value2}
#' etc. into a named list.
#' Keys must include \code{dbType} passed in to \code{DBI::dbDriver} to load the appropriate driver (i.e. MySQL, PostgreSQL, SQLite, etc.)
#' and any parameters like \code{host}, \code{user}, \code{password}, \code{dbname} that should be passed into the DBI::dbConnect function call for your driver.
#'
#' @param filePath The file path to the config file to be read in. Must be absolute or relative to the working directory from \code{getwd()}
#'
#' @details
#' The result of this function call is just a list with named entries. As long as the entries are valid, it can be generated in other ways.
#' Note that the ability to specify a single file that contains database configuration information allows for 
#' multiple users to share a code based, but provide their personal credentials in their copy of the relevant cfg file.
#' Due to the inevitability of code conflicts with these files where user names and passwords are different for each user
#' and the fact that it is insecure and unwise to include database credentials in any version control system, this means 
#' that such dbCfg files should not be checked into version control.
#'   
#' @seealso \code{\link{conf.dbCon}}, \code{\link{run.query}}
#'   
#' @export
dbCfg = function(filePath){
  cfg = properties::read.properties(filePath)
  #cfg = read.csv(filePath,sep="=",header=F)
  #cfg[] <- lapply(cfg, as.character)
  #cfg = as.list(setNames(cfg$V2,cfg$V1)) # convert data frame to named list values
  if('port' %in% names(cfg)) { cfg$port = as.numeric(cfg$port) }
  if('client.flag' %in% names(cfg)) { cfg$client.flag = as.numeric(cfg$client.flag) }
  if(is.null(cfg$dbType)) {
    print("[dbUtil$dbCfg] Warning: config must contain dbType in ('MySQL','PostgreSQL',etc.)")
  }
  return(cfg)
}

# Internal helper functions to determine the SQL dialect which can be used to determine 
# the file path to the appropriate sql file. Can accept either a connection or a configuration
# file as an argument. 
#' @export
getSQLdialect = function(conn) {
  #return(tolower(dbCfg(filePath)$dbType))
  if(DBI::dbIsValid(conn)) {
    return(switch(class(conn)[1],
                  "MySQLConnection" = "mysql",
                  "generic"))
  } else {
    # Use tolower to better support file systems that are not case sensitive
    return(tolower(dbCfg(filePath)$dbType))
  }
}

#' @title
#' Get a database connection
#' 
#' @description
#' Utility function that opens a database connection using the standard \code{dbCfg} format for all configuration options. 
#' 
#' @param cfg a named list of configuration options, often loaded from a flat configuration file using \code{\link{dbCfg}}.
#'
#' @details
#' \code{conf.dbCon} is called by passing in a names list that contains \code{dbType} providing the name of the desired database driver,
#'   which is passed directly into \code{\link{DBI::dbDriver}} to get an instance of the required driver. The rest of the parameters are
#'   driver specific, but typically include \code{host}, \code{user}, \code{password}, and \code{dbname}. These are passed as arguments 
#'   into a call to \code{\link{DBI::dbConnect}}, using the named driver to get a database connection and return it.
#'   
#' @seealso \code{\link{dbDfg}}, \code{\link{run.query}}
#'   
#' @export
conf.dbCon = function(cfg) {
  drvName = cfg$dbType
  if( is.null(drvName) ) stop('[conf.dbCon] No dbType specified')
  driver = NULL
  driver = tryCatch( DBI::dbDriver(drvName), error = function(e) {} )
  if( is.null(driver) ) driver = eval(parse(text=drvName))
  cfg$dbType <- NULL         # remove the dbType entry from the list that will be passed as arguments
  # cfg params, a named list likely read in through dbCfg, are passed as parameters to the DBI::dbConnect
  con = do.call(DBI::dbConnect,c(driver,cfg))
  if( DBI::dbIsValid(con) ) {
    return(con)
  } else {
    print("Error making database connection!")
  }
}



#' @title
#' Close and clear all connections
#' 
#' @description
#' Utility function to close/clear all active db connections, which can sometimes get left open, especially when errors
#' prevent connections from being closed.
#' 
#' @param cfg a named list of configuration options, often loaded from a flat configuration file using \code{\link{dbCfg}}.
#' 
#' @export
clearCons = function(cfg) {
  all_cons <- DBI::dbListConnections(DBI::dbDriver(cfg$dbType))
  for(con in all_cons) {
    res <- DBI::dbListResults(con)
    if(length(res)>0) {
      DBI::dbClearResult(res[[1]])
      rm(res)
    }
    DBI::dbDisconnect(con)
  }
}

#' @title 
#' List all open database connections
#' 
#' @description
#' Use this to see whether too many connections are open
#' 
#' @param cfg parsed connection configuration data
#' 
#' @export
showCons = function(cfg) {
  all_cons <- dbListConnections(DBI::dbDriver(cfg$dbType))
  print(dim(all_cons))
  print(all_cons)
  s = dbGetQuery(all_cons[[1]], "show processlist")
  print(s)
}

#' @title
#' Utility function that assembles a where clause from a list of named values. All values are 'AND'ed together. 
#' Does not support nested expressions or 'OR's, except if the values are length() > 1, they are put into an
#' in() clause enumerating those values.
#' 
#' @param config List of named values to be included in the where clause. Supports caracter and numeric classes, 
#' using in() syntax for those that are length > 1.
#' 
#' @param other_clause An arbitrary clause that is prepended to the list of clauses that are 'AND'ed together to form the where statement
#' 
#' @param suffix Arbitrary text that is appended to the where clause after it has been generated. For example 'limit 100' or 'order by id'
#' 
#' @details
#' \code{buildWhere} is a utility function builds up a where clause using a passes list with named values. 
#' Values of different types require different handling with respect to quotation marks and valus that are arrays
#' are turned into in( ... ) clauses. In the corner case where nothing is passed in, it returns empty string,
#' so it should be friendly to concatenation to a select statement regardless of whether there are values.
#'   
#' @usage buildWhere( config = list(a='foo', b=20, c=c('one','two','three'), d = 1:9) )
#'        buildWhere( config = list(a='foo', b=20, c=c('one','two','three'), d = 1:9), 
#'                    other_clause = '(name = "jimmy" or name = "johnny")', 
#'                    suffix='limit 100' )
#' 
#' @export
buildWhere = function(config, other_clause=NULL, suffix=NULL) {
  fields = names(config)
  clauses = c()
  for( field in fields) {
    #print(field)
    val = config[[field]]
    cls = class(val)
    if( ! cls %in% c('numeric', 'integer', 'character') ) { stop(paste(cls, 'class of values not supported')) }
    # use the in() syntax
    if(length(val) == 1) {
      if(cls == 'character') {
        newClause = paste(field, "='", val, "'", sep='')
      } else if (cls %in% c('numeric', 'integer') ) {
        newClause = paste(field, "=", val, sep='')
      }
    } else if(length(val) > 1) {
      if(cls == 'character') {
        newClause = paste(field, " in('", paste(val, collapse="','", sep=''), "')", sep='')
      } else if (cls %in% c('numeric', 'integer') ) {
        newClause = paste(field, " in(", paste(val, collapse=",", sep=''), ")", sep='')
      }
    } else {
      stop(paste(field, 'has no values for constructing where clause'))
    }
    clauses = c(clauses, newClause)
  }
  if( ! is.null(other_clause) ) {
    clauses = c(other_clause, clauses)
  }
  where = ''
  #print(clauses)
  if(length(clauses > 0)) {
    where = paste('where', paste(clauses,collapse=' and '))
  }
  if( ! is.null(suffix) ) {
    where = paste(where, suffix)
  }
  return(where)
}

#' @title
#' Utility function that runs and returns an arbitrary database query
#' 
#' @param query a string representation of the SLQ query to be run.
#' 
#' @param cfg a named list of connection configuration options, often loaded from a flat configuration file using \code{\link{dbCfg}}.
#' 
#' @param cacheDir an optional directory path to a location where query results can be cached as RData for future use without querying the database again.
#' 
#' @param cacheFile an optional file name that signals that the results of the query should be cached as RData using the passed cacheFile name. 
#'    Note that if there already exists an RData file with that name in the cache directory, the contents will be returned without running the query.
#'    This it is imerative that the \code{cacheFile} names are unique to each unique query that should be run.
#' 
#' @param forceRefresh an optional parameter for use when a \code{cacheFile} has been specified. If True (default is False), the query is run against
#'   the database with results overwriting any existing cached data. This is similar behavior to entering the cache directory and erasing the
#'   cached data before calling the \code{run.query} function. 
#'
#' @param debug print out diagnostic information about the cache path, file, and status, along with the full text of any SQL query executed.
#' 
#' @details
#' \code{run.query} is a utility function that is very often used in \code{DataSource} implementations. It automatically connects to a database
#' with configuration that can be read from a config file, runs queries, and returns results as data.frames. It also supports caching of query results
#' in a query cache directory, using passed cacheFile names, which must be carefully managed by the data source author to ensure that the cacheFile names 
#' are truly unique to each unique query made. For example, the cache file for query to load meter data for an individual meter would need to include
#' the meter's id (or similar) to ensure that it isn't mistaken for data from another meter already cached. The purpose of all this cacheing logic is,
#' of course, to improve the performance of data retrieval for large sets of data where query times can significantly impact performance. Thus it is often good 
#' practice to load and cache data in larger chunks than individual meter data.
#'   
#' @seealso \code{\link{dbDfg}}, \code{\link{conf.dbCon}}
#'   
#' @export
run.query = function(query,cfg,cacheDir=NULL,cacheFile=NULL,forceRefresh=F,debug=F) {
  if(debug) {
    print(paste('cacheDir:',cacheDir))
    print(paste('cacheFile:',cacheFile))
  }
  QUERY_RESULT <- c()
  cachePath = NULL
  if( ! is.null(cacheFile) ) {
    # try to load from disk
    dir.create(file.path(cacheDir),showWarnings=FALSE)
    cachePath = file.path(cacheDir,cacheFile)
    if(debug) { print(cachePath) }
    if(file.exists(cachePath)  & ! forceRefresh) {
      if(debug) { print(paste('Data cache found. Loading data from',cacheFile)) }
      load(cachePath) # this should load into the variable QUERY_RESULT
      return(QUERY_RESULT)
    }
  }
  if( length(QUERY_RESULT) == 0 ) { # skip the DB stuff if it has been loaded from disk
    if(debug) { print(query) }
    con = NULL
    tryCatch({
      con  <- conf.dbCon(cfg)
      res  <- dbGetQuery(con, query)
      if(length(res)>0) QUERY_RESULT  <- res
    },
    error = function(e) {print(paste('Error in run.query:',e))},
    finally = {
      # close the results set if necessary
      resultSet <- dbListResults(con)
      if(length(resultSet)>0) {
        dbClearResult(resultSet[[1]])
        rm(resultSet)
      }
      dbDisconnect(con)
      rm(con)
    } )
    if(! is.null(cachePath) & ! is.null(QUERY_RESULT)) {          # save results to disk cache for future use
      save(QUERY_RESULT,file=cachePath)
    }
  }
  return(QUERY_RESULT)
}

