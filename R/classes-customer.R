# Copyright 2016 The Board of Trustees of the Leland Stanford Junior University.
# Direct inquiries to Sam Borgeson (sborgeson@stanford.edu)
# or professor Ram Rajagopal (ramr@stanford.edu)

# this file contains all the major knowledge about the cannonical structure
# of the meter data the methods and classes required to access and manipulate
# that data.
#
# The two main VISDOM data classes are WeatherClass and MeterDataClass, which provide a simple
# object interface to the data in the database.

#vsource('solaRUtil.R')
#vsource('classes-weather.R')


#' @title
#' Main S3 class that holds and normalizes VISDOM customer meter data
#'
#' @description 
#' Data structures sand functions used to store, manipulate, and visualize customer
#' meter data. This class provides structure and standardization to the representation
#' of meter data in VISDOM, with support for passing in pre-loaded version of data 
#' that is otherwise expensive to load individually. It therefore enables feature 
#' functions, figures, etc. to all be written to draw upon standard structures and
#' functions and centralizes work on performance.
#' 
#' @param id Unique identifier used by data source to identify a customer meter.
#'   Even with numerical id's in a database, it is a good idea to make these
#'   character strings in R.
#' @param geocode Primary geographic locator used by the data source. This is
#'   very often a zip code, but data sources can implement it as census block
#'   or any geographic category that covers all meters, with each meter
#'   associated with only one geocode. If it is not provided, the data source
#'   is used to query for the meter's location.
#' @param weather Reference to instance of \link{WeatherData} object that provides
#'   weather data for the meter's location. The DATA_SOURCE is used to try to
#'   load this information for the relevant geocode if it is not provided.
#' @param data Tabluar format dataframe of electric meter data for the meter identified
#'   by \code{id}, with each row covering all observations for a meter for a day, with
#'   columns including at least: id, date, and hourly or 15 minute readings.
#' @param gasData Tabluar format dataframe of gas meter data, with each row covering all
#'   observations for a meter for a day, with columns including at least: id, date
#'   and daily readings. If this is not provided, the gas meter data is
#'   loaded using the data source if the data source supports gas data.
#'   \code{gasData} is typically passed in as an optimmization
#'   to avoid unnecessary database queries during bulk meter feature extraction.
#' @param useCache Data caching option indicating to the data source whether cached
#'   data should be used to populate the class. Data sources implement their own
#'   caching, but the expectation is that they will hit the underlying data base
#'   once to get the data in the first place and then save it to a local cache from
#'   which it will be retrieved on subsequent calls. See \link{run.query} for details.
#' @param doSG Option indicating whether expensive solar geometry calculations
#'   that rely on the \link{solaR} package should be performed. Off by default for
#'   performance reasons. See \link{solarGeom} for details.
#' @param rawData Tabluar format dataframe of electric meter data, with each row covering all
#'   observations for a meter for a day, with columns including at least: id, date,
#'   and hourly or 15 minute readings. If this is not provided and \code{data} is not provided,
#'   the meter data is loaded using the data source. \code{rawData} is typically passed in as
#'   an optimization to avoid unnecessary database queries during bulk meter feature
#'   extraction.

#' @details
#' MeterDataClass is often called within the iterator framework or using DATA_SOURCE$getMEterDataClass(id) for a data source.
#' @export
#' @seealso \code{\link{WeatherClass}}
#' @examples
#' \dontrun{
#' DATA_SOURCE = TestData(10)
#' cust = MeterDataClass(id='14')
#' plot(cust)
#' }
MeterDataClass = function(id,geocode=NULL,weather=NULL,data=NULL,gasData=NULL,useCache=T,doSG=F,rawData=NULL){
  tic('init')
  if(is.null(geocode) || geocode == 'unknown') {
    geocode = DATA_SOURCE$getGeoForId(id)
    print(paste('[MeterDataClass] looked up geocode',geocode))
  }
  if(is.null(data) || nrow(data) == 0) {
    # if raw data has been passed in, subset to just the data for this meter
    if (! is.null(rawData)) {
      subRows = which(rawData$id == id)
      data = rawData[subRows,] }
    else {
      if(useCache) {
        print('[MeterDataClass] Running get all data')
        rawData = DATA_SOURCE$getAllData(geocode,useCache=useCache)
        subRows = which(rawData$id == id)
        data = rawData[subRows,]
      } else {
        print(paste('[MeterDataClass] Running getMeterData', id, geocode))
        data = DATA_SOURCE$getMeterData(id,geocode)
      }
    }
  }
  if(is.null(data)) stop(paste('[meterDataClass] No data found for id',id))
  if(nrow(data)==0) stop(paste('[meterDataClass] No data found for id',id))
  days = as.POSIXct(paste(data[,'dates'],'00:00'),tz="America/Los_Angeles", format=DATA_SOURCE$dateFormat)

  gasTout = NULL
  gasData =data.frame(c())
  if(is.null(gasData) & DATA_SOURCE$providesGas) {
    if(useCache) {
      rawGasData = DATA_SOURCE$getAllGasData(geocode,useCache=T)
      gasData = rawGasData[rawGasData$id == id,]
    } else {
      gasData = DATA_SOURCE$getGasMeterData(id,geocode)
    }
  }
  gasDays = NULL
  if(nrow(gasData)>0) {
    gasDays = as.POSIXct(paste(gasData[,'dates'],'00:00'),tz="America/Los_Angeles", DATA_SOURCE$dateFormat)
  }

  # some days are duplicated in the DB. Remove all but the first one.
  # todo: should they instead be summed?
  dup = which(diff(days) == 0)
  if(any(dup > 0)) {
    data = data[-dup,] # delete the row
    days = as.POSIXct(paste(data[,'dates'],'00:00'),tz="America/Los_Angeles", DATA_SOURCE$dateFormat)
  }

  geocode = data[1,DATA_SOURCE$geoColumnName]
  kwMat96 = NULL
  kwMat   = NULL
  # for now, assume that long rows of data are 15 minute
  ncol = dim(data)[2]
  if(ncol > 40) {
    kwMat96 = as.matrix(data[,(ncol-96+1):ncol])
    kwMat = sapply(t(1:24),function(x) rowMeans(kwMat96[,(4*(x-1)+1):(4*x)],na.rm=T))
  } else {
    kwMat = data[,(ncol-24+1):ncol]
  }

  # reshape the kW readings into a vector matching the dates
  kw    = as.vector(t(kwMat))

  if (is.null(weather)) {
    tic('weather')
    weather = WeatherClass(geocode,doSG=doSG,doMeans=T,useCache=useCache)
    toc('weather')
  }
  # Normalize dates to day + 0:23 hrs add 1 hour 24 times using lubridate to get datetimes
  # note that this skips the fall back hour and has an NA for the spring forward hour
  #hod = lubridate::hours(1) * (0:23) # there is some sort of lubridate bug with hour()
  hod = 3600 * (0:23)
  options(lubridate.fasttime=T)
  #tic() # too slow!!!
  #dateMat = sapply(days,FUN=function(x) x + hod)
  #datesct = as.POSIXct(as.vector(dateMat),origin='1970-01-01')
  #toc()
  # flatten into a vector and re-convert into date objects
  datesct = as.POSIXct(rep(hod,length(days)) + rep(days,each=length(hod)),origin='1970-01-01')
  dates = as.POSIXlt(datesct)
  tout = weather$resample(datesct,'tout')
  pout = weather$resample(datesct,'pout')
  rain = weather$resample(datesct,'rain')
  dp   = weather$resample(datesct,'dp')
  #wind = weather$resample(datesct,'wind')
  rh   = weather$rh(tout,dp)
  # TODO: clear out obviously bad readings
  #keepers   = which(kw > 0)
  #toc('init','weather resample')

  obj = list (
    id      = id,
    dates   = dates,
    kw      = kw,
    kwMat   = kwMat,
    kwMat96 = kwMat96,
    days    = days,
    gasDays = gasDays,
    therms  = gasData$therms,
    gasTout = gasTout,
    zipcode = geocode,
    geocode = geocode,
    weather = weather,
    tout = tout,
    pout = pout,
    rain = rain,
    dp   = dp,
    #wind = wind,
    rh   = rh,
    toutMat = matrix(tout,ncol=24,byrow=TRUE),
    get = function(x) obj[[x]],
    # Not sure why <<- is used here
    # <<- searches parent environments before assignment
    # http://stat.ethz.ch/R-manual/R-patched/library/base/html/assignOps.html
    set = function(x, value) obj[[x]] <<- value,
    props = list()
  )

  obj$w = function(name='tout') {
    return( obj$weather$resample(datesct,name) )
  }

  obj$df = function() {
    return( data.frame(kw=obj$kw,
                       tout=obj$tout,
                       dates=obj$dates ) )
  }

  obj$norm = function(data) {
    # divide data by the 97th %ile
    return (data/quantile(data,0.97,na.rm=TRUE))
  }

  obj$add = function(name, value) {
    obj[[name]] = value
  }

  # note how list manipulation requires the use of assign
  # not sure why values can't be set in place, but it
  # appears to have to do with variable scoping
  obj$addProp = function(name, value) {
    p <- obj$props
    p[[name]] <- value
    assign('props', p, envir=obj)
  }

  obj$matchDates = function(newDates) {
    a = approx(obj$dates, obj$tout, newDates, method="linear" )[[2]]
    return(a)
  }

  obj$daily = function(var='kw',fun=mean) {
    numDays = dim(obj$kwMat)[1]
    daily = sapply(1:numDays, function(x) fun(obj[[var]][(24*(x-1)+1):(24*x)],na.rm=T))
    return(daily)
  }

  #obj <- list2env(obj)
  class(obj) = "MeterDataClass"
  toc('init')
  return(obj)
}

# if the passed value is unassigned, assign it from the passed context.
# If that doesn't work use the provided default.
default_from_ctx = function( value, name, ctx=NULL, default=NULL ) {
  if( is.null(value) ) {
    value = default
    if( ! is.null(ctx) ) {
      if( exists(name, envir = ctx) ) {
        value = get( name, envir = ctx)
      }
    }
  }
  return(value)
}

#' @export
validateRes = function(r, minDays=NULL, minKwMean=NULL, maxZeroPct=NULL, ctx=NULL) {
  minDays    = default_from_ctx(minDays,    name='validate.minDays',    ctx=ctx, default=180)
  minKwMean  = default_from_ctx(minKwMean,  name='validate.minKwMean',  ctx=ctx, default=0.110)
  maxZeroPct = default_from_ctx(maxZeroPct, name='validate.maxZeroPct', ctx=ctx, default=0.15)

  issues = data.frame(id=r$id)
  #timeDiffs = diff(r$dates)
  #units(timeDiffs) <- "hours"
  #maxtd = max(timeDiffs) / 24
  #span = difftime(tail(r$dates, n=1),r$dates[1],units='days')
  zerospct = sum((r$kw == 0)*1,na.rm=TRUE) / length(r$kw)
  kwmean = mean(r$kw,na.rm=TRUE)
  dayCount = length(r$days)
  if( dayCount < minDays)     issues[paste('days',minDays,sep='')]= dayCount # less than 180 days (could be non-consecutive)
  #if( span < 270 )           issues$span270    = span # spanning less than 270 days total
  #if( maxtd > 60 )           issues$bigdiff    = maxtd # more than 2 months of missing data
  if( kwmean < minKwMean )    issues$lowmean    = kwmean # mean less than 180W is almost always empty or bad readings
  if( zerospct > maxZeroPct ) issues[paste('zerospct',maxZeroPct,sep='')] = zerospct # over 15% of readings are zero
  return(issues)
}

mapColors = function(data,colorMap=NA,log=FALSE) {
  if(length(colorMap) < 2) { colorMap = heat.colors(100) }
  if(is.numeric(data)) {
    mn = min(data,na.rm=TRUE)
    data_mn = data - mn + 0.001
    if(log) {
      data_mn = log(data_mn + 1) # nothing below zero after we take the log
    }
    idx = ceiling(data_mn / max(data_mn,na.rm=TRUE) * length(colorMap))
  }
  if(is.character(data)) { data = factor(data) }
  if(is.factor(data)) { idx = data }
  return(colorMap[idx])
}

hmap = function(data,colorMap=NULL,yvals=NA,xvals=NA,xlabs=NULL,ylabs=NULL,cex.axis=1,...) {
  if('data.frame' %in% class(data)) { 
    data = data.matrix(data)
  }
  if(! 'matrix' %in% class(data) ) {
    data = matrix(data, ncol=24,byrow = T)
  }
  n = dim(data)[1]
  m = dim(data)[2]
  # defailt values
  if(is.null(colorMap)) { colorMap = rev(colorRampPalette(RColorBrewer::brewer.pal(11,"RdBu"))(100)) }
  image(t(data),col=colorMap,axes=F,...)
  if(!is.null(xlabs)){ axis(1, at=(1:length(xlabs) - 1)/(length(xlabs)-1), labels=xlabs, cex.axis=cex.axis) }
  if(!is.null(ylabs)){ axis(2, at=(1:length(ylabs) - 1)/(length(ylabs)-1), labels=ylabs, cex.axis=cex.axis) }
}

# quickly find the estimates for a simple change point model with one cp
quickEst = function(cp,const,b,tlim=c(0,100)) {
  tlim=c(floor(tlim[1]),ceiling(tlim[2]))
  tRng = tlim[1]:tlim[2]
  print(cp)
  print(b)
  pieces = regressor.piecewise(tRng,cp)
  X = cbind(1,pieces)
  y = X %*% c(const,b)
  #print(y)
  #y = c(c(const + tlim[1]:cp * lower),c(const + cp * lower + ((cp+1):tlim[2] -cp) * upper))
  return(cbind(tRng,y))
}

save.png.plot = function(r,path,issues=NULL) {
  tryCatch( {
    png(path)
    colorMap = rev(colorRampPalette(RColorBrewer::brewer.pal(11,"RdBu"))(100))
    issueTxt = ''
    if(length(issues)>1) { issueTxt = paste(colnames(issues)[-1],collapse=', ') }
    plot( r, colorMap=colorMap,
             main=paste( r$geocode, r$id ),
             issueTxt=issueTxt,
             estimates=toutChangePointFast( df= as.daily.df(r) ) )
  },
  error = function(e) { print(e) },
  finally = { dev.off() } )
}

save.png.plot.temp = function(r,path,issues=NULL) {
  tryCatch( {
    png(path)
    colorMap = rev(colorRampPalette(RColorBrewer::brewer.pal(11,"RdBu"))(100))
    issueTxt = ''
    if(length(issues)>1) { issueTxt = paste(colnames(issues)[-1],collapse=', ') }
    plot( r, type='temp', main=paste(r$geocode, r$id),issueTxt=issueTxt)
  },
  error = function(e) { print(e) },
  finally = { dev.off() } )
}

#' @export
plot.MeterDataClass = function(r,colorMap=NA,main=NULL,issueTxt='',type='summary',colorBy='hours',estimates=NULL,extraPoints=NULL,...) {
  # needs a list, called r with:
  # r$id unique identifier (just for the title)
  # r$geocode geocode for the title
  # r$days (1 date per day of data)
  # r$kw (vector of kw readings)
  # r$kwMat (matrix of kw readings with 24 columns)
  # r$toutMat (matrix of Tout readings with 24 columns)
  if(type=='summary') {
    if(is.null(main)) { main <- paste(r$id,' (',r$geocode,') summary info',sep='') }
    if(length(colorMap) < 2) { colorMap = rev(colorRampPalette(RColorBrewer::brewer.pal(11,"RdBu"))(100)) } #colorMap = heat.colors(100)
    op <- par(no.readonly = TRUE)
    par( mfrow=c(2,2), oma=c(2,2,3,0),mar=c(2,2,2,2))# Room for the title
    #plot(r$kw,xlab='Date',ylab='kWh/h',main='Raw usage')

    # image is messed up. we need to reverse the rows, convert to a matrix and transpose the data
    # to get the right orientation!
    image(t(as.matrix(r$kwMat[rev(1:dim(r$kwMat)[1]),])/1000.0),col=colorMap,axes=F,main='kW')
    axis(1, at = seq(0, 1, by = 1/6),labels=0:6 * 4,mgp=c(1,0,0),tcl=0.5)
    if(length(r$days) > 16) {
      axis(2, at = seq(1,0, by = -1/15),labels=format(r$days[seq(1/16, 1, by = 1/16) * length(r$days)],'%m/%d/%y'),las=1,mgp=c(1,0,0),tcl=0.5)
    } else {
      axis(2, at = seq(1,0, by = -1/(length(r$days)-1)),labels=format(r$days,'%m/%d/%y'),las=1,mgp=c(1,0,0),tcl=0.5)
    }

    #hmap(,yvals=r$days,colorMap=colorMap,log=TRUE,main='Heatmap',mgp=c(1,0,0),tcl=0.5) # axis label on row 1, axis and ticks on 0, with ticks facing in
    end = min(length(r$kw),240)
    plot(r$dates[1:end],r$kw[1:end],type='l',xlab='Date',ylab='kWh/h',main='Raw usage zoom',mgp=c(1,0,0),tcl=0.5)
    toutMeans = rowMeans(r$toutMat)
    kWh       = rowSums(r$kwMat)
    plot(r$days,toutMeans,col='grey',axes=F,ylab='',xlab='',,mgp=c(1,0,0),tcl=0.5)
    axis(4, pretty(c(0, 1.1*toutMeans),n=5), col='grey',col.axis='grey',mgp=c(1,0,0),tcl=0.5)
    mtext("T out (F)", side=4, line=1, cex=0.9, col='grey')
    par(new=T) # plot the next plot call on the same figure as previous
    plot(r$days,kWh,ylab='kWh/day',xlab='Day',main='kWh/day',mgp=c(1,0,0),tcl=0.5)

    xlm = c(min(toutMeans,na.rm=TRUE), max(toutMeans,na.rm=TRUE))
    ylm = c(min(kWh,  na.rm=TRUE), max(kWh,  na.rm=TRUE))
    plot(toutMeans,rowSums(r$kwMat),main='kWh/day vs mean outside temp (F)',
         xlab='mean T (degs F)',ylab='kWh/day',
         xlim=xlm,ylim=ylm, # use a well defined set of ranges so they can be matched by any estimates below
         mgp=c(1,0,0),tcl=0.5)
    txtCol = 'black'
    if(nchar(issueTxt) > 0) {
      main = paste(main,issueTxt)
      txtCol = 'red'
    }
    mtext(paste(main,issueTxt), line=0, font=2, col=txtCol, cex=1.2, outer=TRUE)
    if(length(estimates) > 0) {
      #print(estimates)
      par(new=T)
      fit = estimates
      color = 'blue'
      if (fit['AIC_0'] < fit['AIC_cp']) color = 'gray'
      if (fit['nullModelTest'] > 0.1)   color = 'red'
      cp = fit[grep('^cp',names(fit))]
      middle = fit[grep('^middle',names(fit))]
      qe = quickEst(cp,fit['(Intercept)'],c(fit['lower'],middle,fit['upper']),
                  c(min(toutMeans,na.rm=T),max(toutMeans,na.rm=T)))
      #print(qe[,1])
      plot(qe[,1],qe[,2],
           type='l',
           xlim=xlm,ylim=ylm,
           axes=F,lwd=2,col=color )
    }
    par(new=F)
    par(op)
    #heatmap(as.matrix(r$kwMat),Rowv=NA,Colv=NA,labRow=NA,labCol=NA)
  } else if(type=='temp') {
    if(is.null(main)) { main <- paste(r$id,' (',r$geocode,') temperature info',sep='') }
    #colors = rainbow(24)
    colorBy = r$dates$hour
    legendLabs = paste("hr ", 1:24)
    colors = colorRampPalette(RColorBrewer::brewer.pal(11,"PRGn"))(length(unique(colorBy)))
    colors = colors[(1:length(colors) + 8) %% length(colors)] # shift values backwards by 8 hours (so color discontinuity is in the afternoon)
    plot(r$tout,r$kw,xlab='Tout',ylab='kW',main=main,type='p',col=mapColors(colorBy,colors),cex=0.8,pch=19) # rainbow(n, start=2/6, end=1)
    legend('right', legendLabs, fill=colors, ncol = 1, cex = 0.5)
    if(!is.null(extraPoints)) {
      color = 'blue'
      points(extraPoints$x,extraPoints$y,col=color )
           #lwd=2 )
    }
  } else if(type=='hourly') {
    if(is.null(main)) { main <- paste(r$id,' (',r$geocode,') hourly info',sep='') }
    op <- par(no.readonly = TRUE)
    grid = cbind(matrix(c(1:24),nrow=4,ncol=6,byrow=TRUE),c(25,25,25,25))
    layout(grid,widths=c(rep(2,6),3))
    par(oma=c(4,4,3,0),mar=c(1,0,1,0.5))# Room for the title

    pallete = colorRampPalette(RColorBrewer::brewer.pal(11,"Spectral"))(12) #rainbow(12) #,start=2/6, end=1)
    colvals = r$dates$mon
    #colvals = r$dates$wday
    #colvals = r$dates$wday == 0 | r$dates$wday == 6 # 0 = Sun, 6 = Sat
    colors = mapColors(colvals,pallete)
    xlm = c(min(r$tout,na.rm=TRUE), max(r$tout,na.rm=TRUE))
    ylm = c(min(r$kw,  na.rm=TRUE), max(r$kw,  na.rm=TRUE))
    for(i in 0:23) {
      yax = 'n' # supress axis plotting
      xax = 'n'
      if(i %%  6 == 0) yax = 's' # standard y-axis for first of row
      if(i %/% 6 == 3) xax = 's' # standard x-axis for bottom row
      sub = r$dates$hour==i
      plot(subset(r$tout,sub),subset(r$kw,sub),
           col=subset(colors,sub),
           #col='black',
           pch=subset(colvals,sub),
           xlim=xlm,ylim=ylm,yaxt=yax,xaxt=xax,cex=1.0,...
           )
      if(length(estimates) > 1) {
        if(length(estimates[,i+1]) > 1) {
          fit = estimates[,i+1]
          color = 'black'
          if (fit['AIC_0'] < fit['AIC_cp']) color = 'gray'
          if (fit['nullModelTest'] > 0.1)   color = 'red'
          par(new=T)
          print(fit)
          #cp,const,b,tlim=c(0,100)
          qe = quickEst(fit['cp'],fit['(Intercept)'],c(fit['lower'],fit['upper']),
                   c(min(subset(r$tout,sub),na.rm=T),max(subset(r$tout,sub),na.rm=T)))
          #print(qe[,1])
          plot(qe[,1],qe[,2],
                type='l',
                xlim=xlm,ylim=ylm,axes=F,lwd=2,col=color )
          par(new=F)
        }
      }
      grid()
      text(mean(xlm),ylm[2] * 0.95,paste('hr',i),font=2, cex=1.2)
    }
    mtext(main, line=0, font=2, cex=1.2,outer=TRUE)
    mtext('Outside temperature', line=2, font=2, cex=1,side=1,outer=TRUE)
    mtext('Hourly mean kW', line=2, font=2, cex=1,side=2,outer=TRUE)
    #par(xpd=TRUE)
    #plot.new()
    plot(1, type = "n", axes=FALSE, xlab="", ylab="")
    legend('center', month.abb, col=pallete, pch=1:24, ncol = 1, cex = 1.3)
    #par(xpd=FALSE)
    par(op)
  }

}

sumDay = function(df) {
  sums = 24 * colMeans(df,na.rm=T) # calculate the sums for all columns
}

meanDay = function(df) {
  means = colMeans(df,na.rm=T) # calculate the means for all columns
  means['rain'] = 24*means['rain'] # rain should be the total, not mean
  return(means)
}

minDay = function(df) {
  # see: http://stackoverflow.com/questions/13676878/fastest-way-to-get-min-from-every-column-in-a-matrix
  #mins = do.call(pmin,c(lapply(1:nrow(df), function(i)df[i,]),na.rm=T)) # calculate the minsacross all rows
  mins = suppressWarnings(apply(df, 2, min, na.rm=T))
  mins[mins == Inf] = NA
  return(mins)
}

maxDay = function(df) {
  #maxs = do.call(pmax,c(lapply(1:nrow(df), function(i)df[i,]),na.rm=T)) # calculate the maxs across all rows
  maxs = suppressWarnings(apply(df, 2, max, na.rm=T))
  maxs[maxs == -Inf] = NA
  return(maxs)
}

dailySummary = function(rawData,fn) {
  days = factor(as.Date(rawData$dates,tz="PST8PDT"))
  dayMeans = do.call(rbind,as.list(by(rawData[-1],days,fn))) # -1 to get rid of the dates
  dayMeans[is.nan(dayMeans)] = NA
  dayMeans = data.frame(dayMeans)
  dayMeans$day = as.Date(rownames(dayMeans))
  rownames(dayMeans) <- c()
  return(dayMeans)
}

dailyMeans = function(rawData) {
  return(dailySummary(rawData,meanDay))
}

dailyMins = function(rawData) {
  return(dailySummary(rawData,minDay))
}

dailyMaxs = function(rawData) {
  return(dailySummary(rawData,maxDay))
}

dailySums = function(rawData) {
  return(dailySummary(rawData,sumDay))
}
