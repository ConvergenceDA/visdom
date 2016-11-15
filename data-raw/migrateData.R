# Zipcode to location mapping
ERLE_ZIP_LOCATION = read.csv(file.path("Erle_zipcodes.csv"), header=TRUE)
devtools::use_data(ERLE_ZIP_LOCATION, overwrite = TRUE)

CA_ZIP_CLIMATE = read.csv(file.path("BuildingClimateZonesByZIPCode.csv"), header=TRUE)
devtools::use_data(CA_ZIP_CLIMATE, overwrite = TRUE)

CENSUS_VARS_OF_INTEREST = read.table(file.path("census", "census_vars.txt"), header=TRUE, stringsAsFactors=F)
devtools::use_data(CENSUS_VARS_OF_INTEREST, overwrite = TRUE)

#read in the "gazeteer" file that has population, latitude, and longitude for the zcta's
CENSUS_GAZ <- read.table(file.path("census/ACS_11","Gaz_zcta_national.txt"), header=TRUE, colClasses=c("character", rep("numeric", 8)))
# fun fact: the geoid in the gazeteer file IS the ZCTA
colnames(CENSUS_GAZ)[1] = c('ZCTA')
devtools::use_data(CENSUS_GAZ, overwrite = TRUE)
