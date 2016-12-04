# Copyright 2016 The Board of Trustees of the Leland Stanford Junior University.
# Direct inquiries to Sam Borgeson (sborgeson@stanford.edu)
# or professor Ram Rajagopal (ramr@stanford.edu)


.onLoad <- function(libname, pkgname) {
     # Set default package options if they aren't already defined.
     op <- options()
     op.visdom <- list(
          visdom.acs.cache = file.path(system.file(package=pkgname), "extdata", "census.cache")
     )
     toset <- !(names(op.visdom) %in% names(op))
     if(any(toset)) options(op.visdom[toset])

     # Set up a cache directory. Any other code using R.cache will also write to this directory.
     dir.create(getOption("visdom.acs.cache"), recursive=T, showWarnings=F)
     R.cache::setCacheRootPath(getOption("visdom.acs.cache"))

     if( ! acs::api.key.exists() ) {
          warning(
               "Before using downloading census data, you need to set a census API key. ",
               "You can get one from http://api.census.gov/data/key_signup.html ",
               "Once you have obtained a key, set it up like so: ",
               "acs::api.key.install(key='PASTE_YOUR_KEY_HERE')"
          )
     }
     
     invisible()
}

#' @title
#'  Download census data, employing caching to avoid repeated downloads.
#' @description 
#'   A thin caching wrapper around \code{acs.fetch(...)} in the acs package. 
#' @examples
#'   acs.fetch.and.cache(...)
#' @export
# This was the lightweight memoization. Super easy, but no extra error protection
# or cache customization.
# acs.fetch.and.cache = R.cache::addMemoization(acs::acs.fetch)
# This is a slightly thicker caching wrapper.
acs.fetch.and.cache = function(...) {
     key = list(...)
     result = R.cache::loadCache(key=key, dirs="visdom.acs")
     if( ! is.null(result) && acs::is.acs(result)) { 
          return(result) 
     }
     if( ! acs::api.key.exists() ) {
          stop("Before using downloading census data, you need to set a census API key. ",
               "You can get one from http://api.census.gov/data/key_signup.html ",
               "Once you have obtained a key, set it up like so: ",
               "acs::api.key.install(key='PASTE_YOUR_KEY_HERE')")
     }
     acs_results = acs::acs.fetch(...)
     # We could check for other error conditions if we knew what to test for.
     # This test will differentiate between NA and an object of the "acs" class.
     if( ! acs::is.acs(acs_results)) { 
          stop("The census API returned an error. No data is currently available.")
     }
     R.cache::saveCache(acs_results, key=key, dirs="visdom.acs")
     return(acs_results)
}

#' @title
#' Flush the cache directory that stores downloaded census data.
#' @examples
#'   clear_acs_cache()
#' @export
clear_acs_cache = function() {
     file.remove(Sys.glob(file.path(getOption("visdom.acs.cache"), "*.Rcache")))
     invisible()
}

#' @title
#' Retrieve census data from the American Community Survey.
#' 
#' @description
#' Download data from the Census API and cache it locally on disk. This is a
#' thin wrapper around the \code{acs.fetch} function in the [acs
#' package](https://cran.r-project.org/web/packages/acs/index.html). The results
#' of acs.fetch() are converted from a fancy S4 class to a simple data frame.
#' The geographical designations (zip code, state, census block, etc) are specified
#' in the row names, and if zip code (actually ZCTA) is used for the geography, the
#' numeric value is also available as a column.
#'
#' @param filterErr=T
#'   Remove the standard error columns from the results
#' @param endyear=2014
#'   Specify which year of data to download. Currently, 2013 & 2014 are available.
#' @param span=5
#'   Whether you want the 3-year or the 5-year estimates. See census website for more explanations.
#' @param geography
#'   The geographic aggregations and search terms to use with the \code{acs.fetch}
#'   interface. This will default to all zip codes.
#' @param acs_vars_to_load
#'   A dataframe to specify which variables to load and what to name the resultant columns.
#'   The dataframe should have columns "census_var" and "label" as characters. See example below.
#'   This will default to the variables specified in data-raw/census/census_vars.txt
#'
#' @details
#' Before using these functions, you will need to set a census API key. You can get one from
#' http://api.census.gov/data/key_signup.html Once you have obtained a key, set it up like so:
#' \code{
#' acs::api.key.install(key='PASTE_YOUR_KEY_HERE')
#' }
#' If you are a developer who frequently reinstalls packages, you may want to
#' copy this into your .Rprofile
#' 
#' The path to the cache directory is stored as an R option. You may customize
#' it by creating a .Rprofile file in either your project or home directory.
#' \code{
#' options("visdom.acs.cache"="/new/path/to/cache/dir")
#' getOption("visdom.acs.cache")
#' clear_acs_cache()
#' }
#' 
#' The default set of Data Profile variables are specified in 
#' raw-data/census/census_vars.txt. If you edit that list, don't forget to 
#' re-execute migrateData.R. You can look at what variables are available by
#' going to 
#' \url{http://www.census.gov/data/developers/data-sets/acs-5year.2014.html},
#' choosing the appropriate year, searching the page for "ACS Data Profile API
#' Variables", and clicking 
#' \href{http://api.census.gov/data/2014/acs5/profile/variables.html}{html}. As of
#' November 11, 2016, the API only works reliably for 2013 and 2014.
#' 
#' @seealso \code{\link{DataSource}}
#' 
#' @examples
#'   library(acs)
#'   # Load data by state
#'   loadACS(geography = acs::geo.make(state='*'))
#'   # Load data for 2 zip codes
#'   loadACS(geography = c(acs::geo.make(zip.code='94709'), acs::geo.make(zip.code='94710')))
#'   # Load data for all tracts, restricted to California.
#'   loadACS(geography = acs::geo.make(state='CA', county='*', tract='*'))
#'   # Unfortunately, Zip codes can't be restricted to a single state like tracts can.
#'   
#'   # Selecting different variables
#'   acs_vars_to_load = data.frame(
#'        census_var = c("DP02_0015", "DP04_0047"),
#'        label = c("avg_hh_size", "owner_hh_size"),
#'        stringsAsFactors = F )
#'   loadACS(acs_vars_to_load = acs_vars_to_load)
#######
# Developer note:
# If the acs code is ever acting buggy, try importing the whole package. If you
# don't import anything, then acs's code won't work properly (it was failing
# with applying cbind on two acs objects). I don't know if these classes will be
# sufficient to make all of the acs code work properly, but it passes some smoke
# tests and Hadley discourages importing entire packages.
# ' @import acs
#' @importClassesFrom acs acs geo geo.set acs.lookup
#' @export
loadACS = function(filterErr=T, endyear=2014, span=5, geography=NULL, acs_vars_to_load=NULL) {
  # Set default arguments
  if(is.null(geography)) {
       # geography = acs::geo.make(zip.code='94709') # Tiny dataset for debugging.
       geography = acs::geo.make(zip.code='*')
  }
  if(is.null(acs_vars_to_load)){
       curEnv=environment()
       data(list = c('CENSUS_VARS_OF_INTEREST'), package='visdom', envir=curEnv)
       acs_vars_to_load = curEnv[['CENSUS_VARS_OF_INTEREST']]
  }

  # Load data
  acs.data = acs.fetch.and.cache(
       endyear = endyear, span = span, geography = geography,
       variable = acs_vars_to_load$census_var,
       col.names = acs_vars_to_load$label,
       dataset = "acs_dp")

  # Copy the acs object's estimates and standard error into a data frame
  if(filterErr) {
       dat = as.data.frame(estimate(acs.data))
  } else {
       stderr = as.data.frame(standard.error(acs.data))
       colnames(stderr) = paste(colnames(stderr), '.stderr', sep='')
       dat = as.data.frame(cbind(as.data.frame(estimate(acs.data)), stderr))
  }
  # If zip codes were specified, parse the ZCTA value from row names. 
  if( substr(row.names(dat)[1], 1, 6) == "ZCTA5 " ) {
       dat$ZCTA = substr(row.names(dat), 7, 12) 
  }
  return(dat)
}

# Utility functions to merge census data into other data frames with ZCTA or plain ZIP columns
# Basic idea behind merging files: Thanks to https://www.braintreepayments.com/braintrust/vaulted-credit-card-maps-with-R
# and https://gist.github.com/braintreeps/5006126#file-map-r
# To use this YOU MUST download the crosswalk file Zip_to_ZCTA_Crosswalk_2011_JSI.csv from http://udsmapper.org/ziptozctacrosswalk.cfm (now http://udsmapper.org/docs/zip_to_zcta_2015.xlsx)
# into the 'census' directory off your current working directory
# To run a simple example, you can download the census gazeteer file from http://www.census.gov/geo/maps-data/data/gazetteer2010.html
# It needs to be "saved as" csv in the 'census' directory


# Load a data frame that maps from "ZIP" to "ZCTA" using a crosswalk file from http://udsmapper.org/ziptozctacrosswalk.cfm (now http://udsmapper.org/docs/zip_to_zcta_2015.xlsx)
# usage: df = zipToZCTA()
zipToZCTA = function() {
  data('ZIP_TO_ZCTA', package='visdom', envir = environment()) # loads ZIP_TO_ZCTA
  return(ZIP_TO_ZCTA)
}

# load location, etc data from the 2010 census gazeteer file
# not needed for standard mapping of ZIP to ZCTA, but useful if you also want population, lat/lon , etc.
CENSUS_GAZ = NULL
censusGaz = function() {
  if(is.null(CENSUS_GAZ)) {
    data('CENSUS_GAZ',package='visdom', envir = environment()) # loads CENSUS_GAZ
  }
  return(CENSUS_GAZ)
}

# add a ZCTA column to an arbitrary dataframe with zip codes in a column specified by zipCol
#' @export
addZCTA = function(df,zipCol='zip5') {
  #print(names(zipToZCTA()))
  return( merge(df,zipToZCTA()[,c('ZIP','ZCTA')],by.x=zipCol,by.y='ZIP',all.x=T))
}

# merge zip code level census data into an arbitrary dataframe, using a named zipCol
# to identify the zip code data to match on.
# Note that this code uses 'merge' and order is not preserved: if a zip code is unrecognized, its row is filled
# with NAs and appended to the end of the data frame returned
#' @export
mergeCensus = function(df,zctaCol='ZCTA',zipCol='zip5',censusStats=NULL) {
  # if one is not already present, use the zip code column to add a ZCTA column
  if(! 'ZCTA' %in% colnames(df)) {
    df = addZCTA(df,zipCol)
  }
  # 99% use case: the default is to just load the standard ACS 2011 data
  if(is.null(censusStats)) { censusStats = loadACS() }
  # merge the dataframe passed in with ZCTA
  return( merge(df,censusStats,by.x='ZCTA',by.y=zctaCol,all.x=T))
}

# add 2010 census gazeteer population, lat, lon, etc values to zip codes that match ZCTAs
# Note that this code uses 'merge' and order is not preserved: if a zip code is unrecognized, its row is filled
# with NAs and appended to the end of the data frame returned
# Column 1   GEOID 	Five digit ZIP Code Tabulation Area Census Code
# Column 2 	POP10 	2010 Census population count.
# Column 3 	HU10 	2010 Census housing unit count.
# Column 4 	ALAND 	Land Area (square meters) - Created for statistical purposes only
# Column 5 	AWATER 	Water Area (square meters) - Created for statistical purposes only
# Column 6 	ALAND_SQMI	Land Area (square miles) - Created for statistical purposes only
# Column 7 	AWATER_SQMI	Water Area (square miles) - Created for statistical purposes only
# Column 8 	INTPTLAT	Latitude (decimal degrees) First character is blank or "-" denoting North or South latitude respectively
# Column 9 	INTPTLONG	Longitude (decimal degrees) First character is blank or "-" denoting East or West longitude respectively
#' @export
mergeGazeteer = function(df,zctaCol='ZCTA',zipCol='zip5') {
  # if one is not already present, use the zip code column to add a ZCTA column
  if(! 'ZCTA' %in% colnames(df)) {
    df = addZCTA(df,zipCol)
  }
  # merge the dataframe passed in with ZCTA
  return( merge(df,censusGaz(),by.x='ZCTA',by.y=zctaCol,all.x=T))
}

# Usage (uncomment to run):
#zipdf = data.frame(zip5=93000:94000)           # some data frame with a zip code column
#zipCensusdf = mergeCensus(zipdf,zipCol='zip5') # add ACS census data cols to the original data frame

#zipCensusGazdf = mergeGazeteer(zipCensusdf)    # optionally keep going and add population and lat/lon data
#zipGazdf = mergeGazeteer(zipdf)                # or just add lat/lon directly

