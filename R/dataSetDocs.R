# Copyright 2016 The Board of Trustees of the Leland Stanford Junior University.
# Direct inquiries to Sam Borgeson (sborgeson@stanford.edu)
# or professor Ram Rajagopal (ramr@stanford.edu)

# documentation for package data

#' Zipcode to location mapping
#'
#' A dataset containing columns with US zip codes and the corresponding locaiton information
#'
#' @usage data(ERLE_ZIP_LOCATION)
#' @format A data frame with 43191 rows and 7 variables:
#' \describe{
#'   \item{zip}{zip code, as a number}
#'   \item{city}{city the zip code is in}
#'   \item{state}{two letter abbreviation for the state the zip code is in}
#'   \item{latitude}{latitude of the zip code center (post office?)}
#'   \item{longitude}{longitude of the zip code center (post office?)}
#'   \item{timezone}{timesone, as offste relative to GMT time}
#'   \item{dst}{indicator for participation in daylight savings time}
#' }
#' @source \url{http://can.not.remember/}
"ERLE_ZIP_LOCATION"

#' A zip code to CA climate zone mapping
#'
#' A dataset containing columns with US zip codes and the corresponding CA CEC climate zones
#'
#' @usage data(CA_ZIP_CLIMATE)
#' @format A data frame with 1706 rows and 2 variables:
#' \describe{
#'   \item{ZIP.Code}{zip code, as a number}
#'   \item{Building.Climate.Zone}{the CA CEC climate zone the zip code is in}
#' }
#' @source \url{http://CEC.web.site.somewhere/}
"CA_ZIP_CLIMATE"

#' A zip code to census zip code tabulation area mapping
#'
#' A dataset containing columns with US zip codes and the corresponding ZCTA ids
#'
#' @usage data(ZIP_TO_ZCTA)
#'
#' @format A data frame with 41979 rows and 5 variables:
#' \describe{
#'   \item{ZIP}{zip code, as a 0 padded string}
#'   \item{ZIPType}{zip code type}
#'   \item{CityName}{Name of the city the zip code is in}
#'   \item{StateAbbr}{Two letter abbreviation for the state the zip code is in}
#'   \item{ZCTA}{the census zip code tabulation area (ZCTA) the zip code most overlaps with}
#' }
#' @source \url{http://some.random.helpful.blog/}
"ZIP_TO_ZCTA"

#' A summary of census statistics for each ZCTA in the census
#'
#' A dataset containing summary census statistics for each ZCTA in the census
#'
#' @usage data(CENSUS_GAZ)
#'
#' @format A data frame with 33120 rows and 9 variables:
#' \describe{
#'   \item{ZCTA}{zip code tabulation area as a 0 padded string}
#'   \item{POP10}{Population in 2010}
#'   \item{ALAND}{Land area in sqft?}
#'   \item{AWATER}{Water area in sqft?}
#'   \item{ALAND_SQMI}{Land area in sqare miles}
#'   \item{AWATER_SQMI}{Water area in square miles}
#'   \item{INTPTLAT}{Latitude of the area}
#'   \item{INTPTLONG}{Longitude of the area}
#' }
#' @source \url{http://us.census.link/}
"CENSUS_GAZ"
