# Zipcoe to location mapping
ERLE_ZIP_LOCATION = read.csv(file.path("Erle_zipcodes.csv"), header=TRUE)
devtools::use_data(ERLE_ZIP_LOCATION)

CA_ZIP_CLIMATE = read.csv(file.path("BuildingClimateZonesByZIPCode.csv"), header=TRUE)
devtools::use_data(CA_ZIP_CLIMATE)

ACS_DIR = "census/ACS_11"
#read in the zip to zcta mapping (crosswalk) file
ZIP_TO_ZCTA <-read.csv(file.path(ACS_DIR,"Zip_to_ZCTA_Crosswalk_2011_JSI.csv"), header=TRUE, colClasses=c(rep("character", 5)))
colnames(ZIP_TO_ZCTA)[5] = 'ZCTA'
devtools::use_data(ZIP_TO_ZCTA)

#read in the "gazeteer" file that has population, latitude, and longitude for the zcta's
CENSUS_GAZ <- read.table(file.path(ACS_DIR,"Gaz_zcta_national.txt"), header=TRUE, colClasses=c("character", rep("numeric", 8)))
# fun fact: the geoid in the gazeteer file IS the ZCTA
colnames(CENSUS_GAZ)[1] = c('ZCTA')
devtools::use_data(CENSUS_GAZ)

# read in 5 year census data
# DP02, DP03, DP04, DP05
fileName = file.path(ACS_DIR,paste('ACS_11_5YR_','DP02','_with_ann.csv',sep=''))
ACS_11_5YR_DP02 <-read.csv(fileName, header=TRUE,as.is=T,na.strings=c('(X)','-','**','***','*****','N'))
# GEO.id2 is the ZCTA
colnames(ACS_11_5YR_DP02)[2] <- 'ZCTA'

fileName = file.path(ACS_DIR,paste('ACS_11_5YR_','DP03','_with_ann.csv',sep=''))
ACS_11_5YR_DP03 <-read.csv(fileName, header=TRUE,as.is=T,na.strings=c('(X)','-','**','***','*****','N'))
# GEO.id2 is the ZCTA
colnames(ACS_11_5YR_DP03)[2] <- 'ZCTA'

fileName = file.path(ACS_DIR,paste('ACS_11_5YR_','DP04','_with_ann.csv',sep=''))
ACS_11_5YR_DP04 <-read.csv(fileName, header=TRUE,as.is=T,na.strings=c('(X)','-','**','***','*****','N'))
# GEO.id2 is the ZCTA
colnames(ACS_11_5YR_DP04)[2] <- 'ZCTA'

fileName = file.path(ACS_DIR,paste('ACS_11_5YR_','DP05','_with_ann.csv',sep=''))
ACS_11_5YR_DP05 <-read.csv(fileName, header=TRUE,as.is=T,na.strings=c('(X)','-','**','***','*****','N'))
# GEO.id2 is the ZCTA
colnames(ACS_11_5YR_DP05)[2] <- 'ZCTA'

devtools::use_data(ACS_11_5YR_DP02)
devtools::use_data(ACS_11_5YR_DP03)
devtools::use_data(ACS_11_5YR_DP04)
devtools::use_data(ACS_11_5YR_DP05)
