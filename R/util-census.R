# Copyright 2016 The Board of Trustees of the Leland Stanford Junior University.
# Direct inquiries to Sam Borgeson (sborgeson@stanford.edu)
# or professor Ram Rajagopal (ramr@stanford.edu)


#############################
# ACS data summary
#
# Written by Sam Borgeson
# sborgeson@berkeley.edu
#
# last updated 7/25/2013
#
# Data files that contain demographic, housing, income, etc. data by zip code tabulation area (ZCTA) are from the
# American Community Survey (ACS) 2011. The 5yr estimate data spans 2007 to 2012.
# To download, go here: http://factfinder2.census.gov/faces/nav/jsf/pages/searchresults.xhtml
# choose 5-digit Zip Code Tabulation Areas under "Geographies" search option,
# then choose CA, then "all 5-digit...within CA"
# to locate ACS summary data, choose "Data Profile" under topic : "Product Type"
# This will produce a very manageable list, but to be very specific, you can also choose
# "2011 ACS 5-year estimates" under topic: "Dataset" as well
# the resulting DP* data files will be aggregated by ZCTA and span the breadth of topics covered by the ACS
# Check boxes next to all the data you want to download and wait for the zip file to be generated and download
# Unzip that file into a directory off your working directory called census/ACS_11/
#
# The tables of greates interest are:
# DP02: SELECTED SOCIAL CHARACTERISTICS IN THE UNITED STATES (household types, marital status, education, citizenship, ancestry)
# DP03: SELECTED ECONOMIC CHARACTERISTICS (employment status, job, income, poverty)
# DP04: SELECTED HOUSING CHARACTERISTICS (occupancy, units, vintage, tenure, value, costs, fuels)
# DP05: ACS DEMOGRAPHIC AND HOUSING ESTIMATES (sex, age, race)
# file naming convention is ACS_<yr>_<span>_<table>_with_ann.csv
# for example: ACS_11_5YR_DP02_with_ann.csv is the table with social characteristics
# columns are named HC01_VC03, HC02_VC03, HC03_VC03, HC04_VC03, etc.
# HC01 is for the estimated value
# HC02 is for the margin of error for the estimated value
# HC03 is for the estimated percentage of the value
# HC04 is for the percentage margin of error
#
# ACS_<yr>_<span>_<table>_metadata.csv provides human readable names for the coded columns in the data files
# heirarchical levels of categorical breakdowns are separate by '-',
# with the top level also containing the type of observation followed by a ';'
# For example: HC04_VC12 is Percent Margin of Error; HOUSEHOLDS BY TYPE - Family households (families) - Female householder, no husband present, family - With own children under 18 years
# So this field provides the % margin of error for:
# HOUSEHOLDS BY TYPE
#   Family households (families)
#     Female householder, no husband present, family
#       With own children under 18 years
#
# For insipiration, you can look at even more human friendly data lists here (same names can be searched in the metadata):
# http://factfinder2.census.gov/faces/help/jsf/pages/metadata.xhtml?lang=en&type=table&id=table.en.ACS_11_5YR_DP02#
# http://factfinder2.census.gov/faces/help/jsf/pages/metadata.xhtml?lang=en&type=table&id=table.en.ACS_11_5YR_DP03#
# http://factfinder2.census.gov/faces/help/jsf/pages/metadata.xhtml?lang=en&type=table&id=table.en.ACS_11_5YR_DP04#
# http://factfinder2.census.gov/faces/help/jsf/pages/metadata.xhtml?lang=en&type=table&id=table.en.ACS_11_5YR_DP05#
#
# for entries in the data files:
# (X) means NA
# - means too few samples to compute an estimate
# ** means too few samples to calc margin of error (error cols only)
# a - following a median means the median is in the lowest interval of the distro
# a + following a median means the median is in the highest interval of the distro
# a *** in the margin of error column means that the estimate is in the lowest or highest interval (i.e. stat test inappropriate)
# a ***** in the margin of error means that the estimate is controlled, so a statistical test is inappropriate
# an 'N' in the margin of error indicates that the data cannot be displayed because the sample size is too small
# margin of error means that prob that the true value lies in the range defined by the estimate +- the margin is 90%
# the upshot is essentailly that values that cannot be parsed as numerical can be treated as missing data for the puroses of analysis
# these will be turned into NA values so lm, and functions with na.rm, etc. will ignore them.


ACS_DIR = file.path('data/census/ACS_11')

# this is the main function to call, but its results are configured by the functions below it
# the filterErr boolean determines whether or not to filter the columns that contain error metrics
# from the data frame returned.
loadACS = function(filterErr=T) {
  #print('Load acs')
  ACS = acsSocial(filterErr=filterErr)
  print(dim(ACS))
  ACS = merge(ACS,acsEcon(filterErr=filterErr))     # will merge on both GEO.id and ZCTA
  print(dim(ACS))
  ACS = merge(ACS,acsHousing(filterErr=filterErr))  # will merge on both GEO.id and ZCTA
  ACS = merge(ACS,acsDemo(filterErr=filterErr))     # will merge on both GEO.id and ZCTA
  return(ACS)
}

# DP02: SELECTED SOCIAL CHARACTERISTICS IN THE UNITED STATES (household types, marital status, education, citizenship, ancestry)
acsSocial = function(filterErr=T) {
  fields = c('VC20','VC17','VC18','VC93','VC94','VC118','VC119')
  names  = c('avg_hh_size','hh_under_18','hh_over_65','pop_past_highschool','pop_past_bachelors','res_same_1yr','res_diff_1yr')

  return (loadACSTable(table='DP02',colList=fields,colNames=names,filterErr=filterErr))
}

# DP03: SELECTED ECONOMIC CHARACTERISTICS (employment status, job, income, poverty)
acsEcon = function(filterErr=T) {
  fields = c('VC85','VC86','VC112','VC113','VC115','VC156')
  names  = c('median_hh_income','mean_hh_income','median_fam_income','mean_fam_income','per_cap_income','pct_below_poverty')

  return (loadACSTable(table='DP03',colList=fields,colNames=names,filterErr=filterErr))
}

# DP04: SELECTED HOUSING CHARACTERISTICS (occupancy, units, vintage, tenure, value, costs, fuels)
acsHousing = function(filterErr=T) {
  fields = c('VC04','VC63','VC64','VC66','VC67','VC125','VC48')
  names  = c('occupied_units','owner_occupied','renter_occupied','owner_hh_size','renter_hh_size','median_home_value','median_rooms')

  return (loadACSTable(table='DP04',colList=fields,colNames=names,filterErr=filterErr))
}

# DP05: ACS DEMOGRAPHIC AND HOUSING ESTIMATES (sex, age, race)
acsDemo = function(filterErr=T) {
  fields = c('VC21','VC23','VC29','VC30','VC26','VC33','VC34')
  names  = c('median_pop_age','pop_above_18','pop_above_18_M','pop_above_18_F','pop_above_65','pop_above_65_M','pop_above_65_F')

  return (loadACSTable(table='DP05',colList=fields,colNames=names,filterErr=filterErr))
}

# this is the generic function that loads the data from the ACS data files. Note the hard coded relative path
# forces the search for the data files into the census/ACS_11 directory.
# table is the DP* name of the table of interest
# colList is an optional list of field codes to narrow the returned data (just the VC* parts. The HC* parts are automatically handled)
# if a col list is provided, HC01-HC04 are replaced with these suffixes: 'val','valerr','pct','pcterr'
# colNames is an optional list corresponding 1:1 with colList providing the human readable column names to substitute
# filterErr is a boolean value that, when T, removes the two columns per field that provide the error statistics

loadACSTable = function(table='DP02',colList=NULL,colNames=NULL,filterErr=T){
  # data only loads string literal names, so the dynamic name must be placed in a list
  name = c(paste('ACS_11_5YR_',table,sep=''))
  curEnv = environment()
  data(list = name, package='visdom', envir=curEnv)
  
  ACS = curEnv[[name]]
  #print(name)
  #print(dim(ACS))

  if(filterErr) {
    valCols = grep('^HC01',colnames(ACS))
    pctCols = grep('^HC03',colnames(ACS))
    ACS = ACS[,c(1,2,valCols,pctCols)]
  }
  if(!is.null(colList)) {
    newCols = c()
    newNames = c()
    suffix = c('val','valerr','pct','pcterr')
    for(i in 1:4) {
      prefix = paste('HC0',i,sep='')
      end    = suffix[i]
      #print(prefix)
      subCols = as.vector(sapply(colList,FUN=function(x) grep(paste(prefix,'_',x,sep=''),colnames(ACS))))
      if(is.numeric(subCols)) { # sapply with no grep matches returns a list, which should be ignored
        #print(class(subCols))
        newCols = c(newCols,subCols)
        #print(newCols)
        if(! is.null(colNames) ) {
          newNames = c(newNames,paste(colNames,'_',end,sep=''))
          #print(newNames)
        }
      }
    }
    ACS = ACS[,c(1,2,newCols)]
    colnames(ACS)[c(-1,-2)] <- newNames
  }

  # remove all the entries that are non-numeric and convert all data columns to numeric before returning
  # the removal is like running the following, but it also turns max value entries, like '1,000,000+'
  # into their numerical equivalents
  #ACS[ACS == '(X)']        = NA
  #ACS[ACS == '-']          = NA
  #ACS[ACS == '**']         = NA
  #ACS[ACS == '***']        = NA
  #ACS[ACS == '*****']      = NA
  #ACS[ACS == 'N']          = NA
  for (colName in colnames(ACS)[c(-1,-2)]) { # don't modify the geographic codes
    data = ACS[,colName]
    if(is.character(data)) {
      data = gsub('[^0-9]','',data)
    }
    ACS[,colName] = as.numeric(data)
  }
  return(ACS)
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
  data('ZIP_TO_ZCTA', package='visdom') # loads ZIP_TO_ZCTA
  return(ZIP_TO_ZCTA)
}

# load location, etc data from the 2010 census gazeteer file
# not needed for standard mapping of ZIP to ZCTA, but useful if you also want population, lat/lon , etc.
CENSUS_GAZ = NULL
censusGaz = function() {
  if(is.null(CENSUS_GAZ)) {
    data('CENSUS_GAZ',package='visdom') # loads CENSUS_GAZ
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

