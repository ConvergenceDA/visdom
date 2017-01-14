# Copyright 2016 The Board of Trustees of the Leland Stanford Junior University.
# Direct inquiries to Sam Borgeson (sborgeson@stanford.edu)
# or professor Ram Rajagopal (ramr@stanford.edu)

#' @export
weatherFeatures = function(w){ # w is an instance of WeatherClass
  summerMon = 4:8 # May through Sept - zero based
  summerSubset = as.POSIXlt(w$rawData$dates)$mon %in% summerMon

  yMeans = colMeans(w$rawData[,-1],na.rm=T) # annual means
  sMeans = colMeans(subset(w$rawData[,-1],subset =  summerSubset),na.rm=T) # just summer
  wMeans = colMeans(subset(w$rawData[,-1],subset = !summerSubset),na.rm=T) # just winter
  names(sMeans) = paste('summer.',names(sMeans),sep='')
  names(wMeans) = paste('winter.',names(wMeans),sep='')
  features = append(list(zip5=w$zip),as.list(c(yMeans,wMeans,sMeans)))
  
  # TODO: Add HDD, CDD
  
  return(features)
}

#' @export
getWeatherSummary = function() {
  summaryFile = paste(DATA_SOURCE$CACHE_DIR,'/weatherSummary.RData',sep='')
  if(file.exists(summaryFile)) {
    load(summaryFile)
  }
  else {
    zips = DATA_SOURCE$getZips(useCache=T)
    i = 0
    n = length(zips)
    wList = as.list(rep(NA,length(zips)))
    for(zip in zips) {
      i = i+1
      print(paste(zip,'(',i,'/',n,')'))
      tryCatch({
        wList[[i]] = weatherFeatures(WeatherClass(zip,doMeans=F,useCache=T))
      },
      error = function(e) {print(paste('Error in weatherFeatures:',e))},
      finally = {} )

    }
    weatherSummary = data.frame(do.call(rbind,wList))
    colnames(weatherSummary)[1] <- c('zip5')
    for(col in colnames(weatherSummary)) {
      if(is.factor(weatherSummary[[col]])) {
        # as.numeric(levels(f))[f] is the preferred way to convert factors to numerics
        weatherSummary[[col]] = as.numeric(levels(weatherSummary[[col]] ))[weatherSummary[[col]]]
      }
    }
    save(weatherSummary,file=summaryFile)
  }
  weatherSummary$toutC = (weatherSummary$tout - 32) * 5/9
  weatherSummary$summer.toutC = (weatherSummary$summer.tout - 32) * 5/9
  weatherSummary$winter.toutC = (weatherSummary$winter.tout - 32) * 5/9
  weatherSummary$rain = weatherSummary$rain * 365 * 24
  weatherSummary$rain[weatherSummary$rain > 120] = NA # there is junk rain data (suprise!!)
  return(weatherSummary)
}

#print(weatherFeatures(WeatherClass(94610,useCache=T)))
