# Copyright 2016 The Board of Trustees of the Leland Stanford Junior University.
# Direct inquiries to Sam Borgeson (sborgeson@stanford.edu) 
# or professor Ram Rajagopal (ramr@stanford.edu)

#' Smart Meter data analysis tools for R
#'
#' @section Core functions:
#'
#' VISDOM relies most heavily on the following core functions. See the example vignettes for examples of their usage:
#'
#' \itemize{
#'   \item \code{\link{DataSource}}: An S3 class that implements the set of standard data source function for your data.
#'
#'
#'   \item \code{\link[visdom]{MeterDataClass}}: S3 class that holds meter data in both vector time series and daily matrix formats, associated weather data, and several supporting functions. 
#'
#'
#'   \item \code{\link{TestData}}: Example data source S3 class that implements all required data source functions and generates random synthetic data for use in testing and examples.
#'
#'
#'   \item \code{\link[visdom]{WeatherClass}}: S3 class that holds weather data and related functions.
#'
#'
#'   \item \code{\link{basicFeatures}}: Function that implements a full suite of "basic" feature calculations, which include annual, seasonal, monthly, hour of day averages and variances, and other simple statistics, like simple correlation between outside temperature and consumption.
#'
#'
#'   \item \code{\link{dbCfg}}: S3 class that can parse a database config from text file. This is used by the util-dbUtil.R file to connect to the configired database.
#'
#'
#'   \item \code{\link{run.query}}: Main function used to run SQL queries to load and cache data.
#'
#'
#'   \item \code{\link{iterator.callAllFromCtx}}: Function that iterates through all feature algorithms listed in the configuration ctx environment and returns a concatenated named list of all resulting features for a given MeterDataClass instance.
#'
#'
#'   \item \code{\link{iterator.iterateMeters}}: Function that iterates through all passed meter ids to instantiate MEterDataClass for each and call one or more feature extraction functions on each. This requires a properly configured data source, database connection (if applicable) and is further configured using fields in the ctx context object.
#'
#'
#'   \item \code{\link{iterator.iterateZip}}: Function that iterates through all passed zip codes, looks up local weather data (once) and looks up the list of meter ids for each and calls iterator.iterateMeters with those meter ids and the per-loaded weather data. This runs faster than calling ids individually, which can load similar weather data over and over. 
#'
#'
#'   \item \code{\link{iterator.runMeter}}: Utility function that runs the configured set of featture functions on a single passed meter. This is useful for testing and feature development, but also as the function called by parallelizable methods like apply, and the *ply function of plyr.
#'
#'
#'   \item \code{\link{iterator.todf}}: The iterator.iterate* functions return lists of feature lists, indexed by meter id. This function converts all the scalar features in this data structure into a single data.frame, one row per meter id.
#'
#' }
#'
#' @docType package
#' @name visdom
NULL
