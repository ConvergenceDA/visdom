# Copyright 2016 The Board of Trustees of the Leland Stanford Junior University.
# Direct inquiries to Sam Borgeson (sborgeson@stanford.edu)
# or professor Ram Rajagopal (ramr@stanford.edu)

#' @title 
#' Run feature algorithms specified in the ctx environment 
#' 
#' @description
#' This function loads the meter data associated with the passed \code{meterId} and creates 
#' a MeterDataClass instance using that data. It then passes that meter data object into
#' every feature function in the list found in the ctx environment under \code{fnVector}, 
#' concatenating all the results into a single list with named values.
#' 
#' @param meterId The id that the data source can use to lookup the meter data of interest.
#' @param ctx The ctx environment that configures feature runs and provides a place to store and pass data across feature function calls.
#' @param ... Additional arguments that will be passed into the feature functions.
#' 
#' @export
iterator.callAllFromCtx = function(meterId,ctx,...) {
  out = list()
  fnVector = ctx$fnVector
  for(i in 1:length(fnVector)) {
    f = fnVector[[i]]
    out = c(out, f(meterId,ctx,...))
  }
  return(out)
}

# iterator.todf = function(lofl) {
#   blanks = which( laply(lofl,length)<2)
#   print(paste('Removing',length(blanks),'blank entries of',length(lofl)))
#   bdf = as.data.frame(do.call(rbind,lofl[-blanks]))
#   for (col in names(bdf)) {
#     bdf[,name] = as.numeric(unlist(unlist(bdf[,name])))
#   }
#   return(bdf)
# }

#' @title 
#' Flatten feature data returned by iterator functions into a \code{data.frame}.
#' 
#' @description
#' Itertor functions like \code{iterator.iterateMeters} return lists of lists of derived features indexed by meterIds. 
#' This function flattens the scalar values in these lists into data.frame, with one row per meterId and columns for every named feature found.
#' 
#' @param lofl The list of lists for feature values that should be flattened into a single data frame, with one row per meterId.
#' 
#' @details 
#' This function ignores non-scalars, so diagnostic data and other complex data structures can be returned by feature algorithms without 
#' interfering with the task of creating clean vectors of features for each meter. The columns of the returned data.frame are
#' a super set of all the features returned for every MeterDataClass with computed features. Missing features are given values of NA.
#' 
#' @export
iterator.todf = function(lofl) {
  tic('list of lists to data.frame')
  #print(length(lofl))
  print('Removing non-scalars')
  lofdf = plyr::llply( lofl,function(x) {
    data.frame(x[plyr::laply(x,length)==1]) }, # remove non-scalars before creating the data frame
    .progress='text')
  
  if("dplyr" %in% rownames(installed.packages())) {
    print('Running dplyr::bind_rows on the remaining data (fast)')
    fulldf = dplyr::bind_rows(lofdf)
  } else {
    print('Running plyr::rbind.fill on the remaining data (slow)')
    fulldf = plyr::rbind.fill(lofdf)
  }
  toc('list of lists to data.frame')
  return(fulldf)
}

#' @title 
#' Optimize single meter lookup in a data.frame containing many meters
#' 
#' @description
#' This function does a fast search for the first and last indices for each meter's data in a data.frame 
#' with many meters and caches the resulting indices. These indices can be used to very quickly access data 
#' for individual meters.
#' 
#' @param ctx The ctx environment that configures a feature run. This function is specifically looking 
#' to build an index for \code{ctx$RAW_DATA}, if present and saves the results as \code{ctx$idxLookup}. It allows
#'  for RAW_DATA to be set as a data.frame with data from a large number of meters loaded at once and in 
#'  advance of the feature extraction runs to reduce runtimes.
#' 
#' @details 
#' Standard methods of searching for all data for a given meterId would use boolean expressions like 
#' \code{ctx$RAW_DATA[ctx$RAW_DATA$meterId == 'some_id',]}. It turns out that this is pretty inefficient 
#' for large data.frames becaus it generates values for all rows and then does the necessary comparisons for
#' all rows. Direct numerical indexing avoids this overhead and such indices can be quickly computed using 
#' \code{\link{match}} and the fact that all data for each meter must be returned from the data source in 
#' contigious rows. Finally, the constructor for a \code{MeterDataClass} checks for these \code{ctx$idxLookup} if 
#' \code{ctx$RAW_DATA} is found and uses them to pull the subset of data associated with the meterId passed in 
#' to that constructor.
#' 
#' @export
iterator.build.idx = function(ctx) {
  ids = unique(ctx$RAW_DATA$id)
  first = match(ids,ctx$RAW_DATA$id)
  last = c(first[-1]-1,length(ctx$RAW_DATA$id))
  idxLookup = data.frame(ids,first,last)
  ctx$idxLookup = idxLookup
  return(idxLookup)
}

iternator.rbind.scalars = function(a,b) {
  # trim a down to scalars is.character, is.numeric
  # trim b down to scalars
}

#' @title Iterate over zip codes, extracting features for meters in each
#' 
#' @description
#' Function that iterates through all passed zip codes, looks up local weather data (once) and 
#' looks up the list of meter ids for each and calls iterator.iterateMeters with those meter 
#' ids and the per-loaded weather data. This runs faster than calling ids individually, which 
#' can load similar weather data over and over.
#' 
#' @param zipList List of all zip codes to iterate over
#' 
#' @param custFn The feature function to call on \code{MeterDataClass} instances from within each zip code
#' 
#' @param cacheResults A boolean flag that indicates whether feature results should be cached as RData. 
#' If true, the cached data will be looked up prior to feature extraction to bypass running 
#' features for the given zip code while returning the cached results. This can prevent duplicate processing
#' and allow an interrupted batch process to resume processing where it left off.
#' The cache directory is `getwd()` by default, but can be overridden using `ctx$resultsCache`.
#' 
#' @param ctx The ctx environment that configures the feature run.
#' 
#' @param ... Arguments to be passed into the feature function(s).
#' 
#' @export
iterator.iterateZip = function(zipList,custFn,cacheResults=F,ctx=NULL,...) {
  tic(name='zip code run')
  if(is.null(ctx)) { ctx = list() }
  out = list()
  n = length(zipList)
  i = 0
  for (z in zipList) {
    i = i + 1
    print( paste('Processing zip ',z,' (',i,'/',n,')',sep='') )
    tic(name='zip')
    out = c(out,iterator.runZip(z,custFn,cacheResults=cacheResults,ctx=ctx,...))
  }
  toc( name='zip code run' )
  return(out)
}

#' @title Iterate over all meters in a zip code, extracting features for each in a performance optimized manner
#' 
#' @description
#' Function that looks up local weather and all meter data for the passed zip code
#' (once) and caches them and then looks up all meter ids in the passed zip code
#' calls iterator.iterateMeters with those meter ids and the pre-loaded weather and meter data. 
#' This runs faster than calling ids individually, which load individual meter data and similar 
#' weather data over and over.
#' 
#' @param zip The zip code to use for the weather, meter ids, and meter data lookups
#' 
#' @param custFn The feature function(s) to call on \code{MeterDataClass} instances from within the zip code
#' 
#' @param cacheResults A boolean flag that indicates whether results should be cached as RData. 
#' If true, the cached data will be looked up prior to feature extraction to bypass running 
#' features for the given zip code while returning the cached results. This can prevent duplicate processing
#' and allow an interrupted batch process to resume processing where it left off.
#' The cache directory is `getwd()` by default, but can be overridden using `ctx$resultsCache`.
#' 
#' @param ctx The ctx environment that configures the feature run.
#' 
#' @param ... Arguments to be passed into the feature function(s).
#' 
#' @export
iterator.runZip = function(zip,custFn,cacheResults=F,ctx=NULL,...) {
  featureList = NULL
  weatherFeatures = NULL
  if(is.null(ctx$weatherFeatures)) { ctx$weatherFeatures = list() }
  cacheFile = paste(ctx$resultsCache,'/results',zip,'.RData',sep='')
  ctx$zip = zip
  if ( ! is.null(ctx$CLIMATE_ZIP) ) { # if there is a climate zip map, use it add the climate zone
    zone = ctx$CLIMATE_ZIP[ctx$CLIMATE_ZIP[,1] == ctx$zip,2]
    ctx$cz = paste('Z',zone,sep='')
  }
  
  if(cacheResults & file.exists(cacheFile)) {
    tryCatch( expr =  {  
                print(paste('Loading cache file',cacheFile))
                load(file=cacheFile) 
                featureList = featureList.saved
                weatherFeatures = weatherFeatures.saved
                print(paste( length(featureList), 'features retrieved from cache for',zip))
              }, 
              error = function(e) { print(e) } ) # pass. If the load fails, we will run the features.
  }
  if( ! is.null(weatherFeatures) & ! is.null(featureList) ) { # if the data load from the cache was successful
    print(paste('Features and weather features for',zip,'loaded from cache',cacheFile))
    ctx$weatherFeatures[[zip]] = weatherFeatures
  } else {
    tryCatch( expr= {
      ctx$weather = getWeather(zip)
      tryCatch( expr = { 
                  weatherFeatures = weatherFeatures(ctx$weather)
                  ctx$weatherFeatures[[zip]] = weatherFeatures },
                error = function(e) {
                  print(paste('  WARNING: Could not load weather features for zipcode:',zip))
                  print(e)
                })
      print('[iterator$iterateZip] weather data loaded')
      # load all raw data for the zip code and indicate that it has not yet been date filtered
      ctx$RAW_DATA = DATA_SOURCE$getAllData(ctx$zip,useCache=T)
      ctx$ALREADY_DATE_FILTERED = F
      
      # it is important that this happens after any filtering ocurrs
      idx = iterator.build.idx(ctx)
      print('[iterator$iterateZip] raw zip code usage data loaded')
      zipIds = DATA_SOURCE$getIds(ctx$zip,useCache=T)
      featureList = iterator.iterateMeters(zipIds,custFn,ctx,...)
      toc( name='zip',prefixStr=paste('Processed',length(zipIds),'entries in zipcode',zip) )
      if(cacheResults) {
        featureList.saved = featureList
        weatherFeatures.saved = weatherFeatures
        save(list=c('featureList.saved','weatherFeatures.saved'),file=cacheFile)
      }
    },
    error = function(e) {
        print(paste('  WARNING: Could not load data for zipcode:',zip))
        print(e)
      },
    finally = function() {  } )
  }
  return(featureList)
}

#' @title Run features for a single meterId
#' 
#' @description
#' Utility function that runs the configured set of feature functions on a single passed 
#' meter. This is useful for testing and feature development, but also as the function 
#' called by parallelizable methods like apply, and the *ply function of plyr to run feature
#' extraction in on multiple cores of a computer.
#' 
#' @param meterId The meterId to use to instantiate a \code{MeterDataClass} object
#' 
#' @param custFn The feature extraction function to run on the meter data.
#' 
#' @param ctx The ctx environment for the feature extraction.
#' 
#' @param ... Additional arguments to be passed to the feature extraction function.
#'
#' @export
iterator.runMeter = function(meterId,custFn,ctx,...) {
  print( paste('  Loading meter data ',meterId,sep='') )
  tic(name='  process meter')
  meterData = NULL
  tryCatch(
    expr={
      meterData = getMeterDataClass(meterId, ctx)
      issues = validateRes(meterData,minDays=60) # validate function from classes-customer.R checks zeros, too few observations

      if(length(issues)>1) # all issues will return with an id, so > 1 indicates a problem
      {
        print(paste('  WARNING: Bad or insufficient data. Skipping:',paste(colnames(issues),collapse=', ')))
      }
      else {
        return( custFn(meterData,ctx,...) )
        toc(name='  process meter')
      }
    },
    error = function(e) {
      print(paste('  WARNING: Could not load meter data. Skipping:',meterId))
      print(e)
    },
    finally = function() { return(NA) }
  )
}

#' @title Run features for a list of meter ids
#' 
#' @description
#' Utility function that runs the configured set of feature functions on the data from a list of passed 
#' meter ids. This is useful for scripting feature extraction on an arbitrary number of meters.
#' 
#' @param meterList The list of meterIds to use to instantiate a \code{MeterDataClass} objects
#' 
#' @param custFn The feature extraction function to run on each instance of \code{MeterDataClass}.
#' 
#' @param ctx The ctx environment for the feature extraction.
#' 
#' @param ... Additional arguments to be passed to the feature extraction function.
#'
#' @export
iterator.iterateMeters = function(meterList,custFn,ctx,...) {
  out = list()
  i = 1
  tic('iterator.iterateMeters')
  n = length(meterList)
  for (meterId in meterList) {
    toc('iterator.iterateMeters')
    print( paste('  Loading meter ',meterId, ' ', ctx$zip,' (',i,'/',n,')',sep='') )
    i = i + 1

    tic(name='  process meterData')
    meterData = NULL
    tryCatch(
      expr={
        meterData = getMeterDataClass(meterId, ctx)
        #toc(name='  process meterData',prefixStr='getMeterDataClass')
        issues = validateRes(meterData,minDays=60) # validate function from classes-customer.R checks zeros, too few observations

        if(length(issues)>1) # all issues will return with an id, so > 1 indicates a problem
        {

          print(paste('  WARNING: Bad or insufficient data. Skipping:',paste(colnames(issues),collapse=', ')))
        }
        else {
          tic('features')
          out[[i]] = custFn(meterData,ctx,...)
          toc('features')
          toc(name='  process meterData')
        }
      },
      error = function(e) {
        print(paste('  WARNING: Could not load meter data. Skipping:',meterId))
        print(e)
      }
    )
  }
  toc('iterator.iterateMeters',prefixStr=paste(n,'meters'))
  return(out)
}

# returns a list of meter ids associated with a zip code
getMeterIds = function(zip) {
  return( DATA_SOURCE$getSPs(zip, useCache=T) )
}

#' runs applyDateFilter if the appropriate values are found in the ctx and the data is not yet
#' flagged as filtered.
#' @export
runDateFilterIfNeeded = function(ctx) {
  # filters only apply to RAW_DATA
  if(is.null( ctx$RAW_DATA) ) {
    return()
  }
  # to ensure that date filtering can be applied to the raw data once and only once
  # we track whether it has been applied with a boolean flag
  if( ! is.null(ctx$ALREADY_DATE_FILTERED)) {
    if (! ctx$ALREADY_DATE_FILTERED) {
      flt = ctx$dateFilter
      print('Applying date filter')
      norig = nrow(ctx$RAW_DATA)
      ctx$RAW_DATA = applyDateFilters( ctx$RAW_DATA, flt ) # if flt is NULL, this will just return the original data
      print( sprintf('Filtered from %d -> %d', norig, nrow(ctx$RAW_DATA) ) )
      ctx$ALREADY_DATE_FILTERED = T
      # it is important that this happens after any filtering ocurrs
      idx = iterator.build.idx(ctx)
    }
  }
  return()
}

# returns a meter's populated data class, using the unique identifier meterId
# which should be returned by getMeterIds and other related functions.
# Supports optional data from the ctx for speeding up access to zip code, weather
# data, and meter data from the relevant zip code
getMeterDataClass = function(meterId,ctx) {
  meterData = NULL
  # if the data was already looked up in advance and passed into the context
  # use the subset belonging to the specified meter
  
  runDateFilterIfNeeded(ctx)
  
  if(!is.null(ctx$idxLookup)) {
    tic('fast id lookup')
    idxVals = ctx$idxLookup[match(meterId,ctx$idxLookup$ids),]
    meterData = ctx$RAW_DATA[idxVals$first:idxVals$last,]
    
    toc('fast id lookup')
  }
  else {
    rawData = ctx$RAW_DATA # rd
    if (!is.null(rawData)) {
      tic('id subrows')
      subrows = which(rawData$id == meterId)
      meterData = rawData[subrows,]
      toc('id subrows')
    }
  }
  md = DATA_SOURCE$getMeterDataClass(  id       = meterId,
                                       geo      = ctx$zip,
                                       weather  = ctx$weather,
                                       data     = meterData )
  return(md)
}

getWeather = function(zip) {
  print(paste('Loading weather for',zip))
  weather = WeatherClass(zip,useCache=T,doSG=F)
  return(weather)
}
