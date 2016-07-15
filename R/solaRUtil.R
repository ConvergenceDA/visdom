# Copyright 2016 The Board of Trustees of the Leland Stanford Junior University.
# Direct inquiries to Sam Borgeson (sborgeson@stanford.edu)
# or professor Ram Rajagopal (ramr@stanford.edu)

orig_TZ = Sys.timezone() #Sys.getenv(x='TZ')
require('solaR')    # this changes the timezone to UTC!!!!
Sys.setenv(tz=orig_TZ) # change timezone back



# zone elevation and azimuth breaks.
# elevation greater than the max here are assugned to the overheat zone z0
# elevation < 0 are assigned to the night zone
eBr = c(60,30,0)
aBr = c(seq(0,360,45))

nameZone = function(azim,elev,azimBreaks,elevBreaks) {
  if(elev <= 0) return('night')
  if(elev >= max(elevBreaks)) return('z0')
  for (i in 1:(length(azimBreaks)-1)) {
    for (j in 1:(length(elevBreaks)-1)) {
      aBounds = sort(c(azimBreaks[i],azimBreaks[i+1])) # ensure we know which is smallest
      eBounds = sort(c(elevBreaks[j],elevBreaks[j+1]))
      if( (azim >= aBounds[1] & aBounds[2] > azim) &
            (elev >= eBounds[1] & eBounds[2] > elev) ) return(paste('z',i,'.',j,sep=''))
    }
  }
  return('?')
}

#' @export
solarGeom = function(dates,zip=NULL,lat=NULL,azimBreaks=seq(0,360,45),elevBreaks=c(45,0)) {
  # Used to load through direct file access
  #ZIP_LOCATION <- read.csv(file.path("data/Erle_zipcodes.csv"), header=TRUE)
  # now laods as a part of the VISDOM package data
  if( ! exists('ERLE_ZIP_LOCATION')) {
    data('ERLE_ZIP_LOCATION') # loads ERLE_ZIP_LOCATION mapping between zip code and lat/lon
  }
  blanks = is.na(dates)        # dates have na's for daylight savings spring forward day
  dates[blanks] <- dates[1]    # they must be overridden as real dates for calcSol to work
  if(is.null(lat)) {
    lat=37.87
    zip = as.integer(zip)
    print( paste('Deriving lat data from',zip) )
    zipRow = ERLE_ZIP_LOCATION[ERLE_ZIP_LOCATION$zip == zip,]
    if(dim(zipRow)[1]==0) {
      print( paste("Couldn't find zip information. Using lat",lat) )
    } else {
      lat = zipRow[1,'latitude']
    }

  }
  print(paste('Latitude:',lat))

  # Berkeley 37.8717 N, 122.2728 W
  # calcSol computes the angles which describe the intradaily apparent movement of the Sun from the Earth
  # solObj is an S4 class with lots of solar info
  # recall that 'slots' in S4 objects are accessed via the @ operator
  # suppressWarnings because zoo complains about duplicate dates caused by daylight savings time
  solObj = suppressWarnings(calcSol(lat,sample="hour",BTi=dates,EoT=T,keep.night=T))
  sg = as.data.frameI(solObj)
  # the solI slot is a zoo object and we want the time stamps
  sg$dates     = time(solObj@solI)
  sg$elevation = sg$AlS      * 180/pi   # convert from radians
  sg$azimuth   = sg$AzS      * 180/pi   # convert from radians
  sg$azimuth   = sg$azimuth %% 360      # no negative or > 360 degrees

  # get named zones based on breaks into a factor
  sg$zone      = factor(apply(as.matrix(sg[,c('azimuth','elevation')]),1,
                                   function(X) nameZone(X[1],X[2],azimBreaks,elevBreaks)))
  sg$daylight = sg$elevation > 0
  #sg$AlS is the solar elevation
  #sg$AzS is the solar asimuth
  class(sg) <- c('solarGeom',class(sg))
  sg[blanks,] <- NA # put the blanks back in for the overridden values
  return(sg)
}

#' @export
plot.solarGeom = function(solarGeom,azimBreaks=seq(0,360,45),elevBreaks=c(45,0),color=NULL) {
  require('ggplot2')
  require(gridExtra)
  g = ggplot(data.frame(azimuth=c(0,360),elevation=rep(max(elevBreaks),2)),aes(y=elevation,x=azimuth)) +
              geom_polygon(color="grey",size=1,fill=NA) +
              coord_polar(start=pi) +
              scale_x_continuous(breaks=c(seq(0,315,by=45)),limits=c(0,360), # Note that 0 is S in this case.
                  labels=paste(c('S','SW','W','NW','N','NE','E','SE'),seq(0,315,by=45)) ) +
              scale_y_reverse(breaks=c(seq(0,90,by=15)),limits=c(90,0)) +
              labs(title="Annual solar path for each of 24 hours of the day") +
              geom_text(x=0,y=-90,label='z0',color='grey') # no reverse axis for text make negative instead

  for (i in 1:(length(azimBreaks)-1)) {
    for (j in 1:(length(elevBreaks)-1)) {
      zone = data.frame(elevation=c(elevBreaks[j],elevBreaks[j+1],elevBreaks[j+1],elevBreaks[j],  elevBreaks[j]),
                         azimuth =c(azimBreaks[i],azimBreaks[i],  azimBreaks[i+1],azimBreaks[i+1],azimBreaks[i]))
      g = g + geom_polygon(data=zone, colour="gray",size=1,fill=NA) +
              geom_text(x=mean(c(azimBreaks[i],azimBreaks[i+1])),
                        y=-1*mean(c(elevBreaks[j],elevBreaks[j+1])), # no reverse axis for text make negative instead
                        label=paste('z',i,'.',j,sep=''),color='grey')

    }
  }
  cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

  if (is.null(color)) {
    g = g + geom_point(data=solarGeom,aes(x=azimuth,y=elevation))
  }
  else if (color == 'zone') {
    g = g + geom_point(data=solarGeom,aes(x=azimuth,y=elevation,color=zone)) +
            scale_color_brewer(palette='YlGnBu') #color=lubridate::hour(dates) )) color=zone))

  }
  else if(color == 'hour') {
    g = g + geom_point(data=solarGeom,aes(x=azimuth,y=elevation,color=lubridate::hour(dates))) +
      scale_colour_gradientn(colours=cbPalette) #color=lubridate::hour(dates) )) color=zone))
  }
  else {
    g = g + geom_point(data=solarGeom,aes(x=azimuth,y=elevation))
  }
  grid.arrange(g)
  #return(g)
}

TEST=F
if(TEST) {
  # View of whole path, including below horizon
  r = MeterDataClass(553991005,93304)
  sg = r$weather$sg
  plot(sg,color='junk')
}
