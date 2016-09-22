# Copyright 2016 The Board of Trustees of the Leland Stanford Junior University.
# Direct inquiries to Sam Borgeson (sborgeson@stanford.edu)
# or professor Ram Rajagopal (ramr@stanford.edu)

#' @title
#' Implementation of several basic statistical features
#' 
#' @description
#' Function that implements a set of common statistical features that can be run on an instance of \code{\link{MeterDataClass}}.
#'
#' @param meterData An instance of \code{\link{MeterDataClass}} that provides the data used for all the implemented feature calculations.
#'
#' @details
#' \code{basicFeatures} is called by passing in an instance of a MeterDataClass, which provides the meter data in both linear series 
#'   and day-per-row matrix formats. These data structures and some associated indices and intermediate data structures are
#'   re-used throughout this method, which tries to take advantage of the performance benefits of such re-use.
#'   This function is often called in the context of the \code{iterator.*} suite of functions, which can loop through large sets of 
#'   meters, calling feature functions on each.
#' 
#' @examples
#' \dontrun{
#' DATA_SOURCE = TestData(n=10)
#' meterData = DATA_SOURCE$getMeterDataClass(DATA_SOURCE$getIds()[1])
#' features = basicFeatures(meterData)
#' names(features)
#' class(features)
#' }
#' @export
basicFeatures = function(meterData){ # r is an instance of MeterDataClass
  data <- as.matrix(meterData$kwMat)
  id   <- meterData$id

  monthTotals = sapply(split(meterData$kw,factor(format(meterData$dates,'%b'),levels=month.abb)),mean,na.rm=T) * 30 * 24
  names(monthTotals) <- paste('kw.total.',names(monthTotals),sep='')
  monthTotals = as.list(monthTotals) # needs to be a list ofr concatenation to work

  summerMon = 4:8 # May through Sept - zero based
  summerSubset = as.POSIXlt(meterData$dates)$mon %in% summerMon

  # todo: apply/quantile is slow. Is it worth softening min,max this way?
  qtle     <- apply(data,1,FUN=quantile,c(0.01,0.99),na.rm=T)
  dMin     <- qtle[1,]
  dMax     <- qtle[2,]

  hMean    <- colMeans(data,na.rm=T)
  names(hMean) <- paste('HOD.mean.',1:24,sep='')
  dMean    <- rowMeans(data,dims=1,na.rm=T) # rowMeans is faster than apply with FUN=mean ?rowMeans for details
  tMean    <- rowMeans(as.matrix(meterData$toutMat),dims=1,na.rm=T)
  # index of the hottest 10% of days
  t90Idx   <- which(tMean > quantile(tMean,0.9,na.rm=T))
  # index (hour) of the max demand on the hottest days
  t90kwhr = apply(data[t90Idx,],1,FUN=which.max)
  # index (hour) of the min demand on the hottest days
  t90kwhr_min = apply(data[t90Idx,],1,FUN=which.min)
  # index (hour) of the max temperature on the hottest days
  t90thr = apply(meterData$toutMat[t90Idx,],1,FUN=which.max)
  # index (hour) of the min temperature on the hottest days
  t90thr_min = apply(meterData$toutMat[t90Idx,],1,FUN=which.min)

  # index of the coldest 10% of days
  t10Idx   <- which(tMean < quantile(tMean,0.1,na.rm=T))
  # index (hour) of the max demand on the coldest days
  t10kwhr = apply(data[t10Idx,],1,FUN=which.max)
  # index (hour) of the max demand on the coldest days
  t10kwhr_min = apply(data[t10Idx,],1,FUN=which.min)
  # index (hour) of the max temperature on the coldest days
  t10thr = apply(meterData$toutMat[t10Idx,],1,FUN=which.max)
  # index (hour) of the min temperature on the coldest days
  t10thr_min = apply(meterData$toutMat[t10Idx,],1,FUN=which.min)

  # index of the top hours of consumption
  kw90Idx = which(meterData$kw > quantile(meterData$kw,0.9,na.rm=T))
  kw90hr = meterData$dates[kw90Idx]$hour

  maxHOD    <- as.numeric(which.max(hMean))
  kw90      <- Mode(kw90hr)
  t90kw     <- Mode(t90kwhr)
  t90kw_min <- Mode(t90kwhr_min)
  t90t      <- Mode(t90thr)
  t90t_min  <- Mode(t90thr_min)
  t10kw     <- Mode(t10kwhr)
  t10kw_min <- Mode(t10kwhr_min)
  t10t      <- Mode(t10thr)
  t10t_min  <- Mode(t10thr_min)

  dRange   <- dMax - dMin
  dHalfway <- dMin + (dMax - dMin) / 2
  dHighD   <- rowSums(data > dHalfway,dims=1)
  dMn2mx   <- dMin / dMax
  dN2d     <- rowMeans(data[,2:5],na.rm=T) / rowMeans(data[,16:19],na.rm=T) # 2-5am comp to 4-7pm
  nv2dv    <- var(as.vector(data[,2:5]),na.rm=T) / var(as.vector(data[,16:19]),na.rm=T) # var for night 2-5am comp to evening 4-7pm
  # we need dates for this one...
  #wkend = something to do with the DOW in the dates
  #wkdays = ! wkend
  #wknd2wk = mean(wkend) / mean(wkdays)

  soft <- quantile(meterData$kw,c(0.03,0.97),na.rm=T)
  max  <- soft[2]
  min  <- soft[1]
  nObs <- prod(dim(data))
  kw.mean.annual = NA

  enddt = as.POSIXlt(meterData$dates[length(meterData$dates)])
  begindt = enddt
  begindt$year = begindt$year - 1
  lastYear = meterData$dates >= begindt
  if(sum(lastYear,na.rm=T) > 365*24*0.9) { # observations 90% intact
    #print('enough annual kw')
    kw.mean.annual = mean(meterData$kw[lastYear],na.rm=T)
  }

  kw.mean        = mean(meterData$kw,na.rm=T)
  kw.mean.summer = mean(meterData$kw[ summerSubset],na.rm=T)
  kw.mean.winter = mean(meterData$kw[!summerSubset],na.rm=T)

  nObs.gas <- if(length(meterData$therms) > 0 ) { sum(!is.na(meterData$therms)) } else { 0 }
  therm.mean.annual = NA
  therm.mean = NA
  therm.mean.summer = NA
  therm.mean.winter = NA
  therm.min = NA
  date.first.gas = NA
  date.last.gas  = NA

  if(length(meterData$therms > 0)) {
    therm.mean     = mean(meterData$therms,na.rm=T)
    gasSummer      = as.POSIXlt(meterData$gasDays)$mon %in% summerMon
    therm.mean.summer = mean(meterData$therms[ gasSummer],na.rm=T)
    therm.mean.winter = mean(meterData$therms[!gasSummer],na.rm=T)
    thermdf = data.frame(therm=meterData$therms,day=meterData$gasDays,month=as.POSIXlt(meterData$gasDays)$mon)
    thermMonth = dcast(thermdf,month ~ .,mean,value.var='therm')
    names(thermMonth) <- c('month','therm')
    therm.min = mean(sort(thermMonth$therm,decreasing=F)[1:3]) # mean of three lowest months
    #therm.min = mean(thermMonth$therm[thermMonth$month %in% 5:7]) # this is unreliable!!
    date.first.gas = as.POSIXct(meterData$gasDays[1])
    date.last.gas  = as.POSIXct(meterData$gasDays[length(meterData$gasDays)])

    if(nObs.gas >= 365) {
      enddt = as.POSIXlt(meterData$gasDays[length(meterData$gasDays)])
      begindt = enddt
      begindt$year = begindt$year - 1
      lastYear = meterData$gasDays >= begindt
      if(sum(lastYear,na.rm=T) > 365*0.9) {  # observations 90% intact
        #print('enough annual therms')
        therm.mean.annual = mean(meterData$therms[lastYear],na.rm=T)
      }
    }


  }
  maxIdx = which.max(meterData$kw)
  minIdx = which.min(meterData$kw)

  max.hr.kw      = meterData$kw[maxIdx]
  min.hr.kw      = meterData$kw[minIdx]

  max.hr.tout = meterData$tout[maxIdx]
  min.hr.tout = meterData$tout[minIdx]

  #print(meterData$dates[maxIdx])
  max.hr.date = as.POSIXct(meterData$dates[maxIdx])
  min.hr.date = as.POSIXct(meterData$dates[minIdx])

  maxIdx = which.max(dMean)
  minIdx = which.min(dMean)

  max.day.kw      = as.numeric(dMean[maxIdx] * 24)
  min.day.kw      = as.numeric(dMean[minIdx] * 24)

  max.day.tout = tMean[maxIdx]
  min.day.tout = tMean[minIdx]

  max.day.date = meterData$days[maxIdx]
  min.day.date = meterData$days[minIdx]

  date.first = as.POSIXct(meterData$days[1])
  date.last  = as.POSIXct(meterData$days[length(meterData$days)])

  # calculate percentiles of touts during max/min days
  #print(max.day.tout)
  #print(min.day.tout)
  max.day.pct = mean(tMean <= max.day.tout)
  min.day.pct = mean(tMean <= min.day.tout)

  kw.tout.cor        = cor(meterData$kw,meterData$tout,               use='complete.obs') # correlation between tout and kw
  kw.pout.cor = NA
  #if(any(! is.na(meterData$w('pout')))) {
  #  kw.pout.cor = cor(meterData$kw,meterData$w('pout'), use='complete.obs') # correlation between pressure and kw
  #}
  kw.var             = var(meterData$kw/kw.mean,                use='complete.obs') # normed by mean of kW
  kw.var.summer      = var(meterData$kw[summerSubset]/kw.mean,na.rm=T)   # normed by mean of kW
  kw.var.winter      = var(meterData$kw[!summerSubset]/kw.mean,na.rm=T)  # normed by mean of kW

  daily.kw.var       = var(rowMeans(meterData$kwMat)/kw.mean,   use='complete.obs') # normed by mean of kW
  daily.kw.min.var   = var(dMin/kw.mean,                use='complete.obs') # normed by mean of kW
  daily.kw.max.var   = var(dMax/kw.mean,                use='complete.obs') # normed by mean of kW

  lags = 0:24
  lag.cor = apply(as.matrix(lags),  1,function(x) cor(meterData$kw,lag(meterData$tout,x),use='complete.obs'))
  lag.ma  = apply(as.matrix(lags[-1]),1,function(x) cor(meterData$kw,ma(meterData$tout,x), use='complete.obs')) # no width 0
  names(lag.cor) <- c(paste('lag',lags,  sep=''))
  lag.cor = as.list(lag.cor)
  
  max.MA = which.max(lag.ma)
  
  names(lag.ma)  <- c(paste('ma', lags[-1],sep=''))
  lag.ma = as.list(lag.ma)

  basics = list(id=id,
             nObs=nObs,
             nObs.gas=nObs.gas,
             kw.mean=kw.mean,
             kw.mean.summer=kw.mean.summer,
             kw.mean.winter=kw.mean.winter,
             kw.mean.annual=kw.mean.annual,
             therm.mean.annual=therm.mean.annual,
             kw.total=kw.mean * 365 * 24,
             therm.mean=therm.mean,
             therm.mean.summer=therm.mean.summer,
             therm.mean.winter=therm.mean.winter,
             therm.total=therm.mean * 365 * 24,
             therm.min=therm.min,
             max=max,  # will be named 'max.97.' due to quantile origin
             min=min,  # will be called 'min.3.' due to quantile origin
             kw.var=kw.var,
             kw.var.summer=kw.var.summer,
             kw.var.winter=kw.var.winter,
             max.hr.kw=max.hr.kw,
             min.hr.kw=min.hr.kw,
             max.hr.tout=max.hr.tout,
             min.hr.tout=min.hr.tout,
             max.hr.date=max.hr.date,
             min.hr.date=min.hr.date,
             max.day.kw=max.day.kw,
             min.day.kw=min.day.kw,
             max.day.tout=max.day.tout,
             min.day.tout=min.day.tout,
             max.day.date=max.day.date,
             min.day.date=min.day.date,
             max.day.pct=max.day.pct,
             min.day.pct=min.day.pct,
             t90kw=t90kw,
             t90kw_min=t90kw_min,
             t90t=t90t,
             t90t_min=t90t_min,
             t10kw=t10kw,
             t10kw_min=t10kw_min,
             t10t=t10t,
             t10t_min=t10t_min,
             maxHOD=maxHOD,
             kw90=kw90,
             daily.kw.var=daily.kw.var,
             daily.kw.min.var=daily.kw.min.var,
             daily.kw.max.var=daily.kw.max.var,
             kw.pout.cor=kw.pout.cor,
             kw.tout.cor=kw.tout.cor,
             max.MA=max.MA,
             nv2dv=nv2dv,
             date.first=date.first,
             date.last=date.last,
             date.first.gas=date.first.gas,
             date.last.gas=date.last.gas
             )
  #print(class(basics))
  dailyFeatures <- cbind(dMax,dMin,dMean,dRange,dHighD,dMn2mx,dN2d)
  dailyFeatures[dailyFeatures == Inf] = NA # these will be caught by the na.rm in the mean fn
  colnames(dailyFeatures) <- c('max','min','mean','range','dur','mn2mx','n2d')
  monthFeatures = t(sapply(split(data.frame(dailyFeatures),factor(format(meterData$days,'%b'),levels=month.abb)),colMeans,na.rm=T))
  # return the means across all days
  augF = monthFeatures[c('Aug'),]
  names(augF) <- paste('Aug_',names(augF),sep='')
  janF = monthFeatures[c('Jan'),]
  names(janF) <- paste('Jan_',names(janF),sep='')
  meanDailyFeatures = as.list(apply(dailyFeatures,2,FUN=mean,na.rm=T))
  featureList <- c(basics, meanDailyFeatures, monthTotals, hMean, lag.cor, lag.ma, augF, janF)
  #print(featureList)
  return(featureList)
}
