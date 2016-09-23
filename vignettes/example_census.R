library(visdom)
# use the local census data path
a = data.frame(zippy=c(94611,93304))    # create a data frame with a column of zip codes.
aPlus = mergeCensus(a,zipCol = "zippy") # uses zip column (aka "zippy" in this example)
                                        # to match ZCTA from census and return all the 
                                        # ACS 2011 stats for the zips in question
names(aPlus)                            # look at all the census data columns added
aPlus                                   # print everything out

# see util-census.R for mode useful census data features.