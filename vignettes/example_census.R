VISDOM_R_PATH = 'C:/dev/VISDOM-R/eneRgy/R'
setwd( file.path(VISDOM_R_PATH) )

source('util-census.R')                 # ability to load and merge census data
ACS_DIR = '../../census/ACS_11'         # use the local census data path
a = data.frame(zippy=c(94611,93304))    # create a data frame with a column of zip codes.
aPlus = mergeCensus(a,zipCol = "zippy") # uses zip column (aka "zippy" in this example)
                                        # to match ZCTA from census and return all the 
                                        # ACS 2011 stats for the zips in question
names(aPlus)                            # look at all the census data columns added
aPlus                                   # pring everything out

# see util-census.R for mode useful census data features.