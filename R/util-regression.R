# Copyright 2016 The Board of Trustees of the Leland Stanford Junior University.
# Direct inquiries to Sam Borgeson (sborgeson@stanford.edu)
# or professor Ram Rajagopal (ramr@stanford.edu)

#library(sandwich) # Contains the NeweyWest function for calculating a modified vcov
#library(lmtest)   # provides coeftest, which can perform the z/t/p tests using NW
#library(cvTools)  # tools for cross validation cvFit

# Model Descriptor and related classes ------------------------------------
#
# Model Descriptors specify all the information required to run one or more
# regression (or other) models and implement a 'run' method that runs the
# model using data from the MeterDataClass instance and returns its resutls
# in a well specified format, as implemented in summarizeModel.
#' @export
ModelDescriptor = function(name,formula,subset=NULL,preRun=NULL,cvReps=0,keepResiduals=F,step=F,datesToIgnore=NULL){
  if (is.null(preRun)){ preRun = function(r,df) { return( c()) } }
  if (is.null(subset)){ subset     = list(all="TRUE")                }
  obj = list (
    name     = name,
    formula  = formula,
    subset   = subset,
    preRun   = preRun,
    cvReps   = cvReps,
    keepResiduals = keepResiduals
  )

  obj$run = function(resData,df=NULL,doOccModel=F,datesToIgnore=NULL) {
    summaries <- c()
    if(is.null(df)) { df = regressorDF(resData,norm=FALSE) }
    if(! is.null(datesToIgnore)) {
      idZip=paste(resData$id,resData$zip,sep='.')
      #print(paste('datesToIgnore',idZip))
      xDates = datesToIgnore[[idZip]] # idZip lookup
      if (! is.null(xDates)) {
        xd = xDates$dateCounts       # awkward. I forgot that the outliers are saved with lots of additional info
        xdts = xd$dates[xd$Freq > 2]    # dateCounts has the dates and their frequency between 1 and 3 from sliding window regressions
        #print(paste('removing ',length(xdts),'dates'))
        #print(paste('from',nrow(df)))
        df = df[! df$dates %in% xdts,] # keep only entries we are not supposed to ignore
        #print(paste('to',nrow(df)))
      }
    }
    #print(paste('[ModelDescriptor.run]',obj$name,'(',names(obj$subset),'):',obj$formula))
    for(snm in names(obj$subset)) {
      df$sub    = eval(parse(text=obj$subset[[snm]]),envir=df)  # load the subset criteria as text and parse as booleans into the data.frame
      lm.result = lm(obj$formula,df,x=T,y=T,subset=sub==T,na.action=na.exclude) # run the lm on the subset indicated in the data frame
      if(step) {
        lmr = lm('kwh ~ 1',df,subset=sub==T)
        parts = strsplit(obj$formula,'~')[[1]]
        #steps = step(lm(paste(parts[1],'~1'),df,subset=sub==T),scope=paste('~',parts[2]),direction='forward',trace=1)
        steps = step(lmr,scope=list(lower='kwh ~ 1',upper=paste('kwh ~ ',parts[2])),direction='forward',trace=1)

        #print(summary(steps))
        print(anova(steps))
        print(names(anova(steps)))
        print(class(anova(steps)))
      }
      summaries = rbind(summaries, summarizeModel(lm.result,
                                                  df,
                                                  modelDescriptor = obj,
                                                  nm              = obj$name,
                                                  id              = resData$id,
                                                  zip             = resData$zip,
                                                  subnm           = snm,
                                                  cvReps          = obj$cvReps,
                                                  keepResiduals   = obj$keepResiduals,
                                                  formula         = obj$formula,
                                                  doOccModel      = doOccModel,
                                                  subset          = obj$subset[[snm]]) )
    }
    return(list(summaries=summaries)) # using list so other functions can return additional data
  }

  class(obj) = "ModelDescriptor"
  return(obj)
}

# Custom function that runs when print(modelDescriptor) is called.
#' @export
print.ModelDescriptor = function(md) {
  print(paste('ModelDescriptor',md$name))
  print(paste('  Formula:',md$formula))
  print(paste('  Subset:',md$subset,collapse=','))
}

# DescriptorGenerators generate model descriptors and additional regressor columns
# for custom model configurations. For example, the hourly change point model
# returns a separate set of piecewise temperature regressors for each hour.
#' @export
DescriptorGenerator = function(genImpl,name='',formula=NULL,subset=NULL,cvReps=0,terms=NULL,datesToIgnore=NULL,...){
  obj = list (
    name       = name,
    formula    = formula,
    subset     = subset,
    terms      = terms,
    genImpl    = genImpl,
    cvReps     = cvReps,
    datesToIgnore = datesToIgnore
  )

  obj$run = function(r,df=NULL,doOccModel=F) {
    summaries = c()
    other     = c()
    ####
    #### todo: add code to look up and remove "outliers" as determined using the occupancy model
    ####
    if(is.null(df)) { df = regressorDF(r,norm=FALSE) }
    updates = obj$genImpl(r,df,name,formula,subset=subset,cvReps=cvReps,terms=terms,...)
    if(! plyr::empty(updates$regressors)) {
      df = cbind(df,updates$regressors)
    }
    for(modelDescriptor in updates$descriptors) {
      runOut    = modelDescriptor$run(r,df,doOccModel,obj$datesToIgnore) # returns a list, with a named entry 'summaries'
      summaries = rbind(summaries,runOut$summaries)
    }
    # the rest....
    updates['regressors']  <- NULL
    updates['descriptors'] <- NULL
    other = rbind(other,data.frame(updates))
    return(list(summaries=summaries,other=other))
  }
  class(obj) = "DescriptorGenerator"
  return(obj)
}

# Custom print function that runs when print(descriptorGenerator) is called.
#' @export
print.DescriptorGenerator = function(dg) {
  print(paste('DescriptorGenerator',dg$name))
  print(paste('  Subset:',dg$subset,collapse=','))
}

# Custom Model Generators -------------------------------------------------
#
# these *Generator functions define custon data frame columns and ModelDescriptors for
# special case models.
# Generators returns a list with members:
# regressors: a data.frame (or compatible) containing additional required regressor columns
# descriptors: a list of ModelDescriptor objects that specify a model formula and other
#              parameters required to run the custom model assuming data is drawn from
#              the cbind() of the original data.frame df and the new regressors.
# other named values: an arbitrary set of additional named fileds containing information
#              related to the custom process of outputs of the pre-processing.
#              For example - the hourly change point model will return the change points
#                            it is using.

# lagGenerator calculates a single separate temperature coefficient for the range of lags specified.
# It would need change point detection to fit data reasonably well.
#' @export
lagGenerator = function(r,df,namePrefix,formula,subset=NULL,cvReps=0,terms=NULL,hrs=0:18) {
  lagRegressors = apply(t(hrs),2,function(x) return(lag(df$tout,x))) # construct a regressor matrix with lagged columns
  colnames(lagRegressors) <- paste('tout_L',hrs,sep='')
  lagStr = paste('tout_L',hrs,sep='',collapse='+')
  hourlyLags = paste('kw ~',lagStr,'+ HOD')
  out = list(
    regressors   = lagRegressors,
    descriptors  = list( hourlyLags=ModelDescriptor(
      name=paste(namePrefix,'hourlyLags', sep=''),
      formula=hourlyLags,
      subset=subset,cvReps=cvReps)
    )
  )
  return(out)
}

#' @export
partsGenerator = function(r,df,namePrefix,formula,subset=NULL,cvReps=0,terms=NULL,hrs=0:18) {

  print(names(df))
  cold = subset(df,subset=tout < 60)

  print(cfg$outDir)
  path = file.path(getwd(),cfg$outDir,paste(r$zip,r$id,'parts.png',sep='_'))
  print(path)
  #png(path)
  hist(cold$kw,breaks=30)
  plot(r$tout,r$kw)
  points(cold$tout,cold$kw,col='blue')
  mn = mean(cold$kw,na.rm=T)
  tRange = floor(min(r$tout,na.rm=T)):floor(max(r$tout,na.rm=T))
  points(tRange,rep(mn,length(tRange)),col='green',type='l')
  points(tRange,rep(mn+ 2*dev,length(tRange)),col='gray',type='l')
  points(tRange,rep(mn- 2*dev,length(tRange)),col='gray',type='l')
  upper = subset(df,kw > mn+2*dev & tout > 60 )
  points(upper$tout,upper$kw,col='red')
  ulm = lm('kw ~ tout',upper)
  points(upper$tout,ulm$fitted.values,col='black',type='l')


  # next:
  # fit upper points.
  # histogram of upper points; select the middle of the highest peak on the histogram
  # refit these
  # intersect lower and upper lines at the CP
  #dev.off()

  lagRegressors = apply(t(hrs),2,function(x) return(lag(df$tout,x))) # construct a regressor matrix with lagged columns
  colnames(lagRegressors) <- paste('tout_L',hrs,sep='')
  lagStr = paste('tout_L',hrs,sep='',collapse='+')
  hourlyLags = paste('kw ~',lagStr,'+ HOD')
  out = list()
  #  regressors   = lagRegressors,
  #  descriptors  = list( hourlyLags=ModelDescriptor(
  #    name=paste(namePrefix,'hourlyLags', sep=''),
  #    formula=hourlyLags,
  #    subset=subset,cvReps=cvReps)
  #  )
  #)
  return(out)
}

# geometricLagGenerator finds the best fit for a single parameter 'a' that determines geometric
# decay for hourly lag terms in the form a^k for the kth lag. This form is consistent
# with a simple physical model of heat transfer through a wall with resistance and
# capacitance.
#' @export
geometricLagGenerator = function(r,df,namePrefix,formula,subset=NULL,cvReps=0,terms=NULL,hrs=18) {
  # grid search for a geometric decay term beest fit such that the coeficient for the kth lag hour is a^k
  # this can be implemented as a single moving average outpit with weights a^k.
  # wma(t) = Sum_{k=0}^hrs [ Tout(t-k) * a^k ]
  # returns the r.squared value for a mode run with a geometric decay for lagged weights
  modR2 = function(guess,df,hrs) {
    weights = guess ** c(1:hrs)
    weights = weights / sum(weights) # normalize
    wma = ma(df$tout,hrs,weights)
    pieces = regressor.piecewise(wma,c(55,65,75))
    dfwma = cbind(df,wma,pieces)
    #model = lm('kw ~ wma',dfwma) # very different results based on the model used
    # it may be that the lagged Tout calc is incompatible with HOD intereactions (one assumes IID, the other does not)
    model = lm(paste('kw ~',paste(colnames(pieces),collapse=" + ",sep=''),'+ HOD - 1'),dfwma)
    return(summary(model)$r.squared)
  }
  # initialize a search over a = (0,1]
  step = 0.05
  guesses = seq(step,1,step) # a coefficients can't be zero

  fits = apply(t(guesses),2,modR2,df,hrs) # run modR2 for all guess values
  bestGuess = guesses[which.max(fits)]    # choose the best one
  #plot(guesses,fits) # see what the fit(a) looks like
  print(paste('best guess for a:',bestGuess))
  weights = bestGuess ** c(1:hrs)
  weights = rep(1,hrs)
  weights = weights / sum(weights) # normalize
  wmaRegressor = as.matrix(ma(df$tout,hrs,weights))
  colnames(wmaRegressor) <- c('ToutWeightedMA')
  pieces = regressor.piecewise(wmaRegressor,c(55,65,75))
  out = list(
    regressors   = cbind(wmaRegressor,pieces),
    fits         = fits,
    descriptors  = list(
      simple=ModelDescriptor( name=paste(namePrefix,'Simple', sep=''),
                              formula=paste('kw ~ ToutWeightedMA'),
                              subset=subset,cvReps=cvReps),
      pieces=ModelDescriptor( name=paste(namePrefix,'Pieces24', sep=''),
                              formula=paste('kw ~',paste(colnames(pieces),collapse=" + ",sep=''),'+ HOD - 1'),
                              subset=subset,cvReps=cvReps)
    )
  )
  return(out)
}

# cp24Generator calculates a separate temperature change point for every hour of the day.
# It also includes terms that calculate sun sky position (aka solar geometry)
#' @export
cp24Generator = function(r,df,namePrefix,formula,subset=NULL,cvReps=0,terms=NULL,diverge=F,...) {
  #tout65     = pmax(0,tout-65) # TODO: should this be here?
  hourlyFits=hourlyChangePoint(df,as.list(1:24),trange=c(50:(max(df$tout,na.rm=T)-5)))
  cps = hourlyFits['cp',]
  cps[hourlyFits['nullModelTest',] > 0.05] <- NA # if the cp model failed the f-test afgainst the no cp model, don't include it
  splitToutList = plyr::alply(cps,.fun=piecewise.regressor,.margin=1,r$tout,diverge=diverge) # get a list of hourly piecewise splits for tout
  # combine the list into a sensible set of regressors
  hourlyCP = c() # hourly change point pieces
  for (hr in 1:length(cps)) { # for every hour, get the piecewise regressors, select just the right hours, and combine with cbind
    splitTout = splitToutList[[hr]]
    if (dim(splitTout)[2] == 1) colnames(splitTout) <- c(paste('tout_',hr,sep='')) # no split made
    if (dim(splitTout)[2] == 2) colnames(splitTout) <- paste(c('tout_lower_','tout_upper_'),hr,sep='') # 2 cols: above and below cp
    hrFilter = df$HOD %in% paste('H',sprintf('%02i',(hr-1)),sep='')
    splitTout[!hrFilter,] <- 0 # zero out values not matching the hour the change point comes from
    hourlyCP = cbind(hourlyCP,splitTout)
  }
  #tic()
  #sg = solarGeom(r$dates,r$zip)
  #toc()
  #newCols = cbind(sg$zone,((sg$zone != 'night') * 1))
  #colnames(newCols) <- c('solarZone','dayTime')
  hourlyCPf  = paste('kw ~',paste(colnames(hourlyCP),collapse=" + ",sep=''),'+ HOD - 1')
  hourlyCPfZ = paste('kw ~',paste(colnames(hourlyCP),collapse=" + ",sep=''),'+ dayTime + DOW - 1')
  #hourlyCPfZ = paste('kw ~',paste(colnames(hourlyCP),collapse=" + ",sep=''),'+ dayTime:solarZone + HOD - 1')
  changePoints = list(id=r$id,changePoints=hourlyFits)
  if(class(subset) == 'character' & subset == 'idxSteps') {
    step = 30*24 # 30 days
    width = 3 # 3 x 30 days = 90 days
    steps = (1-width):(length(df$idx) %/% (step) -1)
    #if(steps < 0) steps = 0
    subset = list()
    for(i in steps) {
      subset[[paste('sub',i,sep='')]] = paste('idx > ',i*step, '& idx <= ',i*step + step*width)
    }
    print('Generating subsets spanning multiple steps')
    #print(subset)
  }
  out = list(
    regressors   = hourlyCP, # had to wait to add the newCols until after the formula generation above
    #regressors   = cbind(hourlyCP,newCols), # had to wait to add the newCols until after the formula generation above
    changePoints = changePoints,
    descriptors  = list ( #hourlyCP  = ModelDescriptor(
      #  name=paste(namePrefix,'hourlyCP', sep=''),
      #  formula=hourlyCPf,
      #  subset=subset,cvReps=cvReps  ),
      hourlyCPZ = ModelDescriptor(
        name=paste(namePrefix,'hourlyCP',sep=''),
        formula=hourlyCPf,
        subset=subset,cvReps=cvReps,... )
    )
  )
  return(out)
}

# toutPieces24Generator breaks temperature into piecewise segments, broken at 55,65,75F
# and regresses these with different coefficients for every hour of the day.
#' @export
toutPieces24Generator = function(r,df,namePrefix,formula,subset=NULL,cvReps=0,terms='+ HOW - 1',basics=NULL,breaks=c(55,65,75),diverge=F,datesToIgnore=NULL) {
  toutPIECES = regressor.piecewise(r$tout,breaks,diverge=diverge)
  if(class(subset) == 'character' & subset == 'idxSteps') {
    step = 30*24
    width = 3
    steps = (1-width):(length(df$idx) %/% (step) -1)
    #if(steps < 0) steps = 0
    subset = list()
    for(i in steps) {
      subset[[paste('sub',i,sep='')]] = paste('idx > ',i*step, '& idx <= ',i*step + step*width)
    }
    print('Generating subsets spanning multiple steps')
    #print(subset)
  }
  out = list(
    regressors   = cbind(toutPIECES),
    descriptors  = list(
      Pieces24=ModelDescriptor( # regression with the best fit lag
        name=paste(namePrefix,'24', sep=''),
        formula=paste('kw ~',paste(colnames(toutPIECES),':HOD',collapse='+',sep=''),terms),
        subset=subset,cvReps=cvReps,datesToIgnore=datesToIgnore)
    )
  )
  return(out)
}

# toutPieces24LagGenerator finds a single lag k such that it maximizes corr(kw,lag(tout,k))
# Physical reality suggests that much of the heat energy entering a home comes via lags in
# the thermal mass of the walls, roof, and ceilings.
# This T* modified (lagged) temperature is then broken into piecewise segments for every
# hour of the day.
#' @export
toutPieces24LagGenerator = function(r,df,namePrefix,formula,subset=NULL,cvReps=0,terms=NULL,basics=NULL) {
  # basic lagged correlations are calculated as a part of basicFeatures. Here we pull out the
  # single lag with the max correlation with kW demand
  if(length(basics) == 0) { basics = basicFeatures(r) }
  lagCorrs = basics[grep('^lag[0-9]+',names(basics))]
  lagVal   = max(lagCorrs)
  lagHrs   = which.max(lagCorrs) - 1 # fisrt cl is 0 lag
  if(lagVal < 0.2) lagHrs = 0
  print(paste('lag by',lagHrs,lagVal))
  toutL = lag(r$tout,lagHrs)
  lagDiff = toutL - r$tout
  toutPIECESL = regressor.piecewise(toutL,c(55,65,75))

  # define regression formula that uses the piecewise pieces
  piecesf24L     = paste('kw ~',paste(colnames(toutPIECESL),':HOD',collapse=" + ",sep=''),'+ HOD - 1')
  piecesf24Ldiff = paste('kw ~',paste(colnames(toutPIECESL),':HOD',collapse=" + ",sep=''),'+ lagDiff + HOD - 1')
  out = list(
    regressors   = cbind(toutPIECESL,lagDiff),
    lagCorrs      = lagCorrs,
    descriptors  = list(
      Pieces24L=ModelDescriptor( # regression with the bet fit lag
        name=paste(namePrefix,'24L', sep=''),
        formula=piecesf24L,
        subset=subset,cvReps=cvReps),
      Pieces24Ldiff=ModelDescriptor( # regression using the diff between the best fit lag and current temp
        name=paste(namePrefix,'24Ldiff', sep=''),
        formula=piecesf24Ldiff,
        subset=subset,cvReps=cvReps)

    )
  )
  return(out)
}

# toutPieces24MAGenerator finds a single averaging width k such that it maximizes corr(kw,ma(tout,k))
# where ma calculates a moving average using a window of width k. Physical reality suggests that
# much of the heat energy entering a home comes via lags in the thermal mass of the walls, roof,
# and ceilings. It is sensible to presume that the thermal storage allows for averaging over some
# characteristic time period.
# This T* modified (averaged) temperature is then broken into piecewise segments for every
# hour of the day.
#' @export
toutPieces24MAGenerator = function(r,df,namePrefix,formula,subset=NULL,cvReps=0,terms=NULL,basics=NULL) {
  # basic moving average correlations are calculated as a part of basicFeatures. Here we pull out the
  # single averaging window with the max correlation with kW demand
  if(length(basics) == 0) { basics = basicFeatures(r) }
  maCorrs = basics[grep('^ma[0-9]+',names(basics))]
  maVal = max(maCorrs)
  maHrs = which.max(maCorrs)
  if(maVal < 0.2) {
    print('Not much correlation for ma(tout) so now what?')
    maVal = 0 # if there isn't much correlation
  }
  print(paste('moving average width',maHrs,maVal))
  toutMA = ma(r$tout,maHrs)
  toutPIECESMA = regressor.piecewise(toutMA,c(55,65,75))
  # define regression formula that uses the piecewise pieces
  piecesf24MA  = paste('kw ~',paste(colnames(toutPIECESMA),collapse=" + ",sep=''),'+ HOD - 1')
  out = list(
    regressors   = toutPIECESMA,
    maCorrs      = maCorrs,
    descriptors  = list(
      Pieces24MA=ModelDescriptor( # regression with the bet fit lag
        name=paste(namePrefix,'24MA', sep=''),
        formula=piecesf24MA,
        subset=subset,cvReps=cvReps)
    )
  )
  return(out)
}

#' @export
toutDailyFixedCPGenerator = function(r,df,namePrefix,formula,subset=NULL,cvReps=0,terms=NULL,basics=NULL,diverge=F) {
  return(toutDailyCPGenerator(r,df,namePrefix,formula,subset=subset,cvReps=cvReps,basics=basics,terms=terms,diverge=diverge,forceCP=65))
}

#' @export
toutDailyNPCPGenerator = function(r,df,namePrefix,formula,subset=NULL,cvReps=0,terms=NULL,basics=NULL,diverge=F) {
  return(toutDailyCPGenerator(r,df,namePrefix,formula,subset=subset,cvReps=cvReps,basics=basics,terms=terms,diverge=diverge,forceCP=c(55,65,75)))
}

#' @export
toutDailyDivergeCPGenerator = function(...) {
  return(toutDailyCPGenerator(...,diverge=T))
}

# toutDailyCP finds a single change point for a model of daily kWh usage or takes any number of forced
# change points as arguments
#' @export
toutDailyCPGenerator = function(r,df,namePrefix,formula,subset=NULL,cvReps=0,terms='+ DOW - 1',basics=NULL,forceCP=NULL,diverge=F) {
  if(is.null(terms)) { terms = ''}
  changeModel = NULL
  if(is.null(forceCP)) {
    changeModel = toutChangePointFast(df=df,reweight=F)
    #print(changeModel)
    modelCP = changeModel[grep('^cp',names(changeModel))]
  }
  else { modelCP = forceCP }
  pieces = regressor.piecewise(df$tout.mean,modelCP,diverge=diverge) # get a list of daily piecewise splits for tout.mean
  nSegs = dim(pieces)[2]
  if (nSegs == 1) colnames(pieces) <- c('tout.mean') # no split made
  if (nSegs == 2) colnames(pieces) <- c('tout.mean_lower','tout.mean_upper') # 2 cols: above and below cp
  if(nSegs > 2) {
    middle = paste('tout.mean_middle_',1:(nSegs-2),sep='')
    colnames(pieces) <- c('tout.mean_lower',middle,'tout.mean_upper')
  }
  # define regression formula that uses the piecewise pieces
  dailyCPf  = paste('kwh ~',paste(colnames(pieces),collapse=" + ",sep=''),terms)
  out = list(
    regressors   = pieces,
    changeModel  = changeModel,
    descriptors  = list(
      DailyCP=ModelDescriptor( # regression with the best fit daily change point
        name=paste(namePrefix,'DailyCP', sep=''),
        formula=dailyCPf,
        subset=subset,cvReps=cvReps)
    )
  )
  return(out)
}

#' @export
toutDailyFlexCPGenerator = function(r,df,namePrefix,formula,subset=NULL,cvReps=0,terms='+ DOW - 1',basics=NULL,forceCP=NULL,diverge=F) {
  # todo: test 1,2,and 3 segment change point models.
  #coolCP = 70
  bestFit = toutDoubleChangePoint(df)
  cp = bestFit[grep('^cp',names(bestFit))]
  pieces = regressor.piecewise(df$tout.mean,cp,diverge=diverge) # get a list of daily piecewise splits for tout.mean
  middle = c()
  nSegs = dim(pieces)[2]
  if(nSegs > 2) middle = paste('tou.mean_middle_',1:(nSegs-2),sep='')
  colnames(pieces) <- c('tout.mean_lower',middle,'tout.mean_upper')
  # define regression formula that uses the piecewise pieces
  dailyCPf  = paste('kwh ~',paste(colnames(pieces),collapse=" + ",sep=''),terms)
  out = list(
    regressors   = pieces,
    changeModel  = bestFit,
    descriptors  = list(
      DailyFlexCP=ModelDescriptor( # regression with the best fit daily change point
        name=paste(namePrefix,'DailyFlexCP', sep=''),
        formula=dailyCPf,
        subset=subset,cvReps=cvReps)
    )
  )
  return(out)
}

# Helper functions --------------------------------------------------------

# this looks really complicated, but it is just converting an arbitrary
# id (numerical, text, or otherwise) into a number suitable for seeding
# random numbers to make cvfold repeatable per id.
idHash = function(obj) {
  md5hash = digest::digest(obj,algo='md5')
  intOfHash = strtoi(substr(md5hash,27,32),base=16)
  return(intOfHash)
}

# summarize regression model for future use
# designed to be friendly to storing a lot of results in sequence
# so it separates out the simple scalar metrics from the more complicated
# coefficients
#' @export
summarizeModel = function(m,df,modelDescriptor,nm,id,zip,subnm=NULL,cv=F,cvReps=1,doNewey=F,doOccModel=F,keepResiduals=F,formula='',subset='') {
  #lm(m,subset=m$y > 1)
  basics = list()
  basics$id          <- id
  basics$zip         <- zip
  basics$model.name  <- nm        # name of model run
  basics$subset.name <- subnm     # name of sample subset criteria (i.e. "summer" or "afternoon" or "weekdays")
  basics$formula     <- formula   # string of lm model call
  basics$subset      <- subset    # string of lm subset argument
  basics$logLik      <- logLik(m) # log liklihood for the model
  basics$AIC         <- AIC(m)    # Akaike information criterion

  #print(names(m))
  #print(summary(m, correlation=T))
  if(doNewey) {
    basics$BG <- unclass(lmtest::bgtest(m,order=1)) # Breusch-Godfrey test for first-order serial correlation
    DW             <- unclass(lmtest::dwtest(m)) # Durban-Watson test for serial corr (range is 0-4. 2 means no corr, DW near 1 is cause for concern)
    basics$DW      <- DW$statistic
    basics$DW.pval <- DW$p.value
    basics$coefficientsNW <- unclass(lmtest::coeftest(m,vcov.=sandwich::NeweyWest))
    basics$pacf = unclass(pacf(m$residuals,plot=F))$acf
    basics$pacf.1 <- basics$pacf[1]
    basics$pacf.significance <- qnorm((1 + 0.95)/2)/sqrt(sum(!is.na(m$residuals)))

  }

  s <- c(basics,as.list(summary(m, correlation=FALSE))) # generic list is more friendly for adding to a data.frame
  class(s) <- 'list'         # make sure the class is no longer summary.lm
  s$hist          <- hist(s$residuals,breaks=100,plot=F)
  s$kurtosis      <- timeDate::kurtosis(s$residuals)

  s$total         <- sum(predict(m),na.rm=T)
  s$dates         <- df$dates[eval(parse(text=subset),envir=df)]
  s$residuals     <- residuals(s) # include NAs in the resudials

  if(doOccModel) {
    # note that we can only assume that the length of residuals and dates
    # are the same if the regression uses na.action=na.exclude and we call
    # residuals(s), not s$residuals. If the lengths differ, we will report
    # incorrect dates
    #print('running occ models')
    dens = quantileDensities(s$residuals,df$dates)
    s$occ.hr   = dens$hr
    s$occ.wday = dens$wday
    s$occ.wknd = dens$wknd
    s$occ.wkdy = dens$wkdy
    s$mon      = dens$mon
    mdens = quantileDensities(s$residuals,df$dates,monthly=T)
    s$occ.hr.m   = mdens$hr
    s$occ.wday.m = mdens$wday
    s$occ.wknd.m = mdens$wknd
    s$occ.wkdy.m = mdens$wkdy
    s$mon.m      = mdens$mon
  }


  s$call          <- c()     # lm model call (depends on variable scope and can be junk)
  s$terms         <- c()     # lm model terms (depends on variable scope and can be junk)
  if(keepResiduals) {
    s$prediction    <- predict(m)
  } else {
    s$residuals     <- c()     # residuals scaled by weights assigned to the model
  }
  s$cov.unscaled  <- c()     # p x p matrix of (unscaled) covariances
  #s$aliased      <- c()     # named logical vector showing if the original coefficients are aliased
  s$na.action     <- c()     # get rid of extra meta info from the model
  #s$sigma                   # the square root of the estimated variance of the random error
  #s$adj.r.squared           # penalizing for higher p
  #s$r.squared               # 'fraction of variance explained by the model'
  #s$fstatistic              # 3-vector with the value of the F-statistic with its numerator and denominator degrees of freedom
  #s$df                      # degs of freedom, vector (p,n-p,p*), the last being the number of non-aliased coefficients.
  #s$coefficients            # a p x 4 matrix: cols = coefficient, standard error, t-stat, (two-sided) p-value

  # net contribution of each coefficient
  # todo: there are a lot of negative numbers in this, so the contributions aren't directly interpretable
  if("x" %in% names(m)) {
    s$contribution <- colSums(t(m$coefficients * t(m$x)))
    #s$contributionCP1 <- colSums(t(m$coefficients * t(changeCP(m$x,1))))
    #s$contributionCP2 <- colSums(t(m$coefficients * t(changeCP(m$x,2))))
    #s$contributionCP5 <- colSums(t(m$coefficients * t(changeCP(m$x,5))))
  }

  # k-fold prediction error
  if (cvReps > 0) {
    #s$fold.rmse <- kFold(df,modelDescriptor,K=5)  # hand rolled cross validation function
    # this use of the id as the random number seed might be a bit strange, but it ensures that
    # all cvFits across different model specifications use the same subsets of data
    # this ensures consistency for the purposes of comparison
    s$cv.rmse   <- cvFold(df,modelDescriptor,K=5,R=cvReps,seed=idHash(s$id)) # pre-rolled function from cvTools
    s$cv.mape   <- cvFold(df,modelDescriptor,K=5,R=cvReps,costfn=cvTools::mape,seed=idHash(s$id) ) # pre-rolled function from cvTools

  }
  PLOT_RESUDIALS = F
  if(PLOT_RESUDIALS) {
    tryCatch( {
      kwh = df$kwh
      lowPts = kwh < 1
      png(file.path(getwd(),'residuals',paste(zip,id,'residuals.png',sep='_')))
      op <- par(no.readonly = TRUE)
      par( mfrow=c(2,2), oma=c(2,2,3,0),mar=c(2,2,2,2))# Room for the title
      plot(m$y,col='black',main=paste('R2=',sprintf("%0.2f",s$r.squared),'cv.RMSE=',sprintf("%0.2f",s$cv.rmse)))
      points(m$fitted.values,col='grey')
      plot(s$hist)
      plot(s$residuals,col='blue',main=paste('Residuals; kurtosis=',s$kurtosis))
      pacf(s$residuals,main='PACF')
    },
    error = function(e) { print(e) },
    finally = {
      par(op)
      dev.off()
    } )
  }
  return(s)
}

#' @export
changeCP = function(data,dt=2) {
  # find and alter the temperature components
  # to simulate a change in the setpoint
  # currently only works for toutCP models.
  # WARNING: doest work with negative changes!!!
  upChanged = data[,'tout.mean_upper'] - dt
  data[,'tout.mean_lower'] = data[,'tout.mean_lower'] - pmin(0,upChanged)
  data[,'tout.mean_upper'] = pmax(0,upChanged)
  return(data)
}

# returns a matrix of length(sort(unique(membership))) columns, that is all
# zeros except for the regressor values that match the membership values
# corresponding to the column. This supports regression with separate
# coefficints for each group defined by the membership
# for example, splitRegressor(Tout,dates$hour) will return a matrix with 24
# columns, where the only non-zero entry per row contains the Tout value in
# the column corresponding to the hour of day it was recorded
#' @export
regressor.split = function(regressor,membership=NULL) {
  mat <- c()
  nm  <- c()
  if(is.null(membership)) {
    if(class(regressor) == 'factor') {
      membership = regressor
      regressor=rep(1,length(regressor))
    }
  }
  for (i in sort(unique(membership))) {
    mat <- cbind(mat,ifelse(membership==i,1,0)) # add a colunm of 1's and 0's
    nm <- c(nm,i)
  }
  colnames(mat) <- nm
  return(mat * regressor)
}

# break a vector out for continuous piecewise regression (i.e. the fitted
# segments will join eachother at each end) into a matrix whose row
# totals are the original values, but whose columns divide the value across
# a set of bins, so 82 across bins with boundaries c(50,60,65,70,80,90)
# becomes the row 50,10,5,5,10,2,0, which sums to 82...
# This is very useful for finding rough change points in thermal response
# as is expected for buildings with clear setpoints

# TODO: This can create a column of zeros, which should break the regression
# so we might need to prune the columns when we're done and keep track of
# which bins are in play when comparing across regressions
#' @export
regressor.piecewise = function(regressor,bins,diverge=F) {
  if(any(is.na(bins))) return(as.matrix(regressor)) # if bins itself is NA or any of its values are NA, return the original data
  binLower = 0
  mat <- c()
  nm = c()
  for(binUpper in c(bins,Inf)) {
    col = regressor - binLower
    # this is to avoid columns of zeros when the bins are outside the data
    # values are scaled by the first bin value to make sure they aren't too
    # big or too small for the data
    smalls = bins[1] * runif(length(col),0.00001,0.00009)
    negs = col < 0
    negs[is.na(negs)] = F # force NA values to become False boolean values
    col[negs] = 0 #smalls[negs]
    col[(col > binUpper-binLower)] = binUpper-binLower
    mat = cbind(mat,col)
    nm = c(nm,paste('tout',binLower,'_',binUpper,sep=''))
    binLower = binUpper
  }
  if(diverge) {
    mat[,1] = pmax(bins[1] - regressor,0) # the first column contains the distance of
    # the value from the bottom change point (rather than from 0)
  }
  colnames(mat) <- nm
  return(mat)
}

# convienience function putting the bins first for apply style calls...
#' @export
piecewise.regressor = function(bins,regressor,...) return(regressor.piecewise(regressor, bins,...))

# pre-compute holiday to save time below - the dates must be a superset of all potential dates.
# holidaysNYSE is a function from the timeDate package
library(timeDate)
hdays = as.Date(holidayNYSE(2008:2011))

# Given a MeterDataClass instance (meterData), regressorDF returns a data.frame consisting of a standard set of
# regressor columns suitable for passing into a call to lm.
#' @export
regressorDF = function(meterData,norm=FALSE,rm.na=FALSE) {
  wday       = meterData$dates$wday
  WKND       = c('WK','ND')[(wday == 0 | wday == 6) * 1 + 1] # weekend indicator
  dateDays   = as.Date(meterData$dates)
  vac        = factor(dateDays %in% hdays)
  hStr       = paste('H',sprintf('%02i',meterData$dates$hour),sep='')
  hwkndStr   = paste(WKND,hStr,sep='')
  dStr       = paste('D',wday,sep='')
  mStr       = paste('M',meterData$dates$mon,sep='')
  howStrs    = paste(dStr,hStr,sep='')
  idx        = 1:length(meterData$kw)
  MOY        = factor(mStr,levels=sort(unique(mStr)))         # month of year
  DOW        = factor(dStr,levels=sort(unique(dStr)))         # day of week
  HOD        = factor(hStr,levels=sort(unique(hStr)))         # hour of day
  HODWK      = factor(hwkndStr,levels=sort(unique(hwkndStr))) # hour of day for weekdays and weekends
  HOW        = factor(howStrs,levels=sort(unique(howStrs))) # hour of week
  tout       = meterData$tout
  pout       = meterData$pout
  rain       = meterData$rain
  dp         = meterData$dp
  rh         = meterData$rh
  kw         = meterData$kw

  tout65     = pmax(0,tout-65) # todo: we know that 65 or any other fixed change point is a bad assumpiton
  # but it is commonly made. Maybe we should eliminate this, but maybe we
  # should keep it to allow for comparisons with other models...

  if(norm) kw = meterData$norm(kw)

  #kw_min = kw - quantile(kw,na.rm=TRUE,c(0.02)) # remove the min for regression w/o const term
  print(paste(length(pout),length(rain),length(rh),length(meterData$dates),length(vac),length(MOY)))
  df = data.frame(
    #tout65_l1 = lag(tout65,1),
    #tout65_l3 = lag(tout65,3),
    #tout_d1 = diff2(tout,1),
    #tout65_d1 = diff2(tout,1)*(tout65 > 0),
    #tout_d3 = diff2(tout,3),
    #kw_min=kw_min,
    idx=idx,
    kw=kw,
    tout=tout,
    tout65=tout65,
    pout=pout,
    rain=rain,
    rh=rh,
    dates=meterData$dates,
    vac=vac,
    MOY,DOW,HOD,HODWK,HOW,WKND   )
  if(rm.na) { df = df[!rowSums(is.na(df)),] }
  #df = cbind(df,toutPIECES) # add the columns with names from the matrix to the df
  return(df)
}

#' @export
rDFG = function(meterData,norm=F,bp=65,rm.na=FALSE) {
  df = data.frame(therms=meterData$therms,tout=meterData$gasTout)
  df$tout.65 = pmax(0,65 - df$tout)
  return(df)
}

#' @export
rDFA = function( meterData, norm=F, bp=65, rm.na=FALSE ) {
  return( as.daily.df( meterData, norm, bp, rm.na )  )
}

#' @export
# TODO: this is the main function in use for converting a MeterDataClass into a data frame. Rename it to something more descriptive
as.daily.df = function(meterData,norm=F,bp=65,rm.na=FALSE) {
  numDays = dim(meterData$kwMat)[1]
  dts  = meterData$dates
  days = as.POSIXlt(paste(meterData$days,'00:00'),format='%Y-%m-%d %H:%M' )
  #days = dts[(0:(numDays-1)*24)+1]
  df = data.frame(day = format(days,'%Y-%m-%d'))
  df$mon   = days$mon
  df$wday  = days$wday
  df$DOW   = paste('D',days$wday,sep='')  # Su=0 ... Sa=6
  df$DOW   = factor(df$DOW, levels=sort(unique(df$DOW)))
  df$MOY   = format(days,'%y-%m')   # as.POSIXlt(df$dates)$mon
  df$MOY   = factor(df$MOY, levels=sort(unique(df$MOY)))
  df$WKND  = (df$wday == 0 | df$wday == 6) * 1 # weekend indicator
  df$kwh   = meterData$daily('kw',sum)
  df$kw.mean = meterData$daily('kw',mean)
  df$kw.max = meterData$daily('kw',max)
  df$CDH = meterData$daily('tout',function(tout,bp=65,na.rm=T) sum(pmax(0,tout-bp),na.rm=na.rm))
  df$HDH = meterData$daily('tout',function(tout,bp=65,na.rm=T) sum(pmax(0,bp-tout),na.rm=na.rm))
  w = meterData$weather
  dayMatch = match(as.Date(df$day),as.Date(w$dayMeans$day))

  # todo: add NAs for days that are in df$day, but not dayMeans (caused by > 24 hrs of data missing)
  df$tout.mean  = w$dayMeans[dayMatch,'tout']
  df$tout.min   = w$dayMins[dayMatch,'tout']
  df$tout.max   = w$dayMaxs[dayMatch,'tout']
  df$tout.mean.65 = pmax(0,df$tout.mean-65)
  tPieces = regressor.piecewise(df[['tout.mean']],c(65))
  df$tout.mean.65lower = tPieces[,1] # lower data for fixed 65 CP
  df$tout.mean.65upper = tPieces[,2] # upper data for fixed 65 CP
  df$tout.mean.65.l1 = lag(pmax(0,(df$tout.mean-65)),1) # 1 day lagged tout.mean over 65F
  dl            = w$dayLengths[dayMatch,'dayMeans']
  df$day.length = NULL # day length is optional because it takes a long time to compute
  if(length(dl) > 0) { df$day.length = dl - min(dl) }
  #   df$tout.mean = meterData$daily('tout',mean)
  #   df$tout.max  = meterData$daily('tout',max)
  #   df$tout.min  = meterData$daily('tout',min)
  df$pout.mean = w$dayMeans[dayMatch,'pout']
  df$pout.min  = w$dayMins[dayMatch,'pout']
  df$pout.max  = w$dayMaxs[dayMatch,'pout']
  #   df$pout.mean = meterData$daily('pout',mean)
  #   df$pout.max  = meterData$daily('pout',max)
  #   df$pout.min  = meterData$daily('pout',min)
  #   df$rh.mean = w$rh(df$tout.mean,w$dayMeans[dayMatch,'dp'])
  df$rh.mean = meterData$daily('rh',mean)
  #df$rh.max  = meterData$daily('rh',max)
  #df$rh.min  = meterData$daily('rh',min)
  # add vacation days flags
  # holidaysNYSE is a function from the dateTime package
  #hdays      = as.Date(holidayNYSE((days[1]$year+1900):(days[length(days)]$year+1900)))
  df$vac  = factor(as.Date(days,,tz="PST8PDT") %in% hdays)

  return(df)
}
# Given a MeterDataClass instance, returns a list of data.frames with rows aggregated to 1 per day,
# typically via averaging. These can be used as input into daily or monthly regression models.
regressorDFAggregated = function(meterData,norm=F,bp=65,rm.na=FALSE) {
  # uses melt and cast to reshape and aggregate data
  df = meterData$df() # kw, tout, dates
  if(norm) df$kw_norm = meterData$norm(df$kw)
  df$kw_min = df$kw - quantile(df$kw,na.rm=TRUE,c(0.02)) # remove the min for regression w/o const term

  df$pout  = meterData$pout
  dp       = meterData$dp
  df$rh    = meterData$rh

  df$day   = format(df$dates,'%Y-%m-%d') # melt has a problem with dates
  df$wday  = as.POSIXlt(df$dates)$wday   # raw for subsetting Su=0 ... Sa=6
  df$DOW   = paste('D',df$wday,sep='')  # Su=0 ... Sa=6
  df$WKND  = (df$wday == 0 | df$wday == 6) * 1 # weekend indicator
  df$DOW   = factor(df$DOW, levels=sort(unique(df$DOW)))
  month    = format(df$dates,'%y-%m')   # as.POSIXlt(df$dates)$mon
  df$mon   = as.POSIXlt(df$dates)$mon   # raw month data for subset functions Jan=0 ... Dec=11
  df$MOY   = factor(month, levels=sort(unique(month)))
  df <- subset(df, select = -c(dates) )  # melt has a problem with dates but we don't need anymore
  # melt and cast to reshape data into monthly and daily time averages
  dfm = melt(df,id.vars=c("day",'DOW','MOY','mon','wday','WKND'),measure.vars=c('kw','tout','pout','rh'),na.rm=TRUE)

  monthly = cast(dfm,MOY + mon ~ variable,fun.aggregate=c(sum,mean,function(ar1,bp=65) sum(ar1 > bp),function(ar2,bp=65) sum(ar2 < bp)),subset= variable %in% c('kw','tout'))
  colnames(monthly) <- c('MOY','mon','kwh','kw.mean','junk1','junk2','junk3','tout.mean','CDH','HDH')
  monthly <- subset(monthly, select = -c(junk1, junk2, junk3) )
  #monthly = c()
  daily = cast(dfm, MOY + day + DOW + mon + wday + WKND ~ variable,fun.aggregate=c(sum,mean,max,function(ar1,bp=65) sum(ar1 > bp),function(ar2,bp=65) sum(ar2 < bp)),subset= variable %in% c('kw','tout','pout','rh'))
  colnames(daily) <- c('MOY','day','DOW','mon','wday','WKND','kwh','kw.mean','kw.max','junk1','junk2','junk3','tout.mean','tout.max','CDH','HDD','junk4','pout.mean','pout.max','junk5','junk6','junk7','rh.mean','rh.max','junk8','junk9')
  daily <- subset(daily, select = grep("^junk", colnames(daily), invert=TRUE) )

  # add vacation days flags
  dateDays   = as.Date(daily$day,,tz="PST8PDT")
  # holidaysNYSE is a function from the dateTime package
  hdays      = as.Date(holidayNYSE(min(as.POSIXlt(dateDays)$year+1900):max(as.POSIXlt(dateDays)$year+1900)),,tz="PST8PDT")
  daily$vac  = factor(dateDays %in% hdays)

  if(FALSE) {
    M <- rbind(c(1, 2), c(3, 4), c(5, 6))
    layout(M)
    pacf(daily$kwh)
    acf(daily$kwh)
    plot(daily$pout.mean, daily$kwh)
    plot(daily$rh.mean, daily$kwh)
    plot(daily$tout.max, daily$kwh)
    plot(daily$kwh,type='l',main=paste('',meterData$id))
    Sys.sleep(1)
  }
  if(rm.na) {
    daily   = daily[!rowSums(is.na(daily)),]
    monthly = monthly[!rowSums(is.na(monthly)),]
  }
  return(list(daily=daily,monthly=monthly))
}

# Change point helper functions -------------------------------------------
#' @export
hourlyChangePoint = function(df,hourBins=list(1:24),trange=NULL,fast=T,reweight=F) {
  # hourBins should be a list of n numeric arrays such that each member of the list
  # is 1 or more hours of the day to use with the subset command to get
  # n change point estimates. So the default list(1:24) corresponds to using
  # all the data. Whereas as.list(1:24) would fit 24 subsets...
  if(class(hourBins) != 'list') hourBins = as.list(hourBins)
  if(fast) {
    cps = sapply(hourBins,toutChangePointFast,subset(df),trange,reweight)
  }
  else {
    cps = sapply(hourBins,toutChangePoint,subset(df),trange,reweight)
  }
  return(cps)
}

# this runs all the models and chooss the minimum one.
# likely waste of CPU on models past the min. See faster impl below
#' @export
toutChangePoint = function(hrs=NULL,df,trange=NULL,reweight=F) {
  toutStr = 'tout'
  if(! is.null(hrs)) {
    sub = df$HOD %in% paste('H',sprintf('%02i',(hrs-1)),sep='') # pull out hrs subset (#0-23 in the df)
    df = df[sub,]
    df = df[!is.na(df$kw),]
  }
  else { toutStr = 'tout.mean' }
  if(is.null(trange)) {
    rng = floor(quantile(df[[toutStr]],c(0.1,0.90),na.rm=T))
    trange = c( rng[1]:rng[2]  )
  }
  steps = sapply(trange,FUN=evalCP,df,reweight)  # run all the models in the range
  #print(steps['SSR',])
  bestFit = steps[,which.min(steps['SSR',])] # find and return the min SSR in the range
  return(bestFit)
}

# find all minima by scanning all values
allMins = function(trange,fn,valCol='SSR',tCol='cp', ...) {
  cur.val = fn(trange[1],...)
  old.delta = 1 # as long as it starts positive, we are fine
  mins = c()
  for(cur.t in trange[-1]) {
    #print(cur.t)
    old.val = cur.val
    cur.val = fn(cur.t,...)
    cur.delta = cur.val[valCol] - old.val[valCol]
    #print(c(old.delta,cur.delta))
    if(old.delta < 0 & cur.delta >= 0) { # if it was descending, but isn't anymore
      mins = rbind(mins,old.val) # record the min
    }
    old.t     = cur.t
    old.delta = cur.delta
  }
  rownames(mins) <- NULL
  return(mins)
}

#' @export
descend = function(trange,fn,valCol='SSR',tCol='cp',default.t=60, ...) {
  results = data.frame( trange )
  names(results) = tCol
  if ( default.t %in% trange[-1] ) { # see if the default is at least one in from the lower bound
    start = default.t
  } else {
    start = trange[floor(length(trange)/2)] # use the mid point
  }
  cur.t        = start - 1
  start.val    = fn(start,...)
  neighbor.val = fn(cur.t,...)
  # to initialize, we need to figure out which way is down
  # the rest of the code assumes that cur.t is down slope, so we need
  # to switch labels at the start to ensure that condition
  if(start.val[valCol] < neighbor.val[valCol]) { # we are descending from neighbor towards start
    old.t   = cur.t
    old.val = neighbor.val
    cur.t   = start
    cur.val = start.val

  } else {
    old.t   = start
    old.val = start.val
    #cur.t   = cur.t # unchanged
    cur.val = neighbor.val

  }
  while(cur.t %in% trange) { # note %in% is checking for membership, not looping through trange values
    if ( cur.val[valCol] < old.val[valCol] ) { # keep going
      dt      = cur.t - old.t
      old.t   = cur.t
      old.val = cur.val
      cur.t   = cur.t + dt
      cur.val = fn(cur.t, ...)
    } else { # if they are equal or greater, we found the (a?) min
      return(old.val)
    }
  }
  cur.val[tCol] = NA
  return(cur.val) # return the last value computed, which is by definition the lowest
}

#' @export
toutChangePointFast2 = function(hrs=NULL,df,trange=NULL,reweight=F,method='descend',warn=T) {
  toutStr = 'tout'
  if(! is.null(hrs)) {
    sub = df$HOD %in% paste('H',sprintf('%02i',(hrs-1)),sep='')  # define hrs subset (#0-23 in the df)
    df = df[sub,]                                                # pull out the subset from the df
    df = df[!is.na(df$kw),]                                      # no NA's. Breaks the algorithm
  }
  else { toutStr = 'tout.mean' }
  if(is.null(trange)) {
    rng = floor(quantile(df[[toutStr]],c(0.1,0.90),na.rm=T))
    trange = c( rng[1]:rng[2]  )
  }
  if(method=='descend') {
    return( descend(trange,evalCP,valCol='SSR',tCol='cp',default.t=60,df,reweight) )
  } else if (method == 'allMins') {
    return( allMins(trange,evalCP,valCol='SSR',tCol='cp',df,reweight) )
  } else {
    print(paste('toutChangePointFast2: Unrecognized method',method))
    return(NULL)
  }

}

# this takes advantage of the fact that for one change point,
# the SSR will be convex so it stops when the change in SSR is positive
# note that each cp in trange could be a list c(40,50,60,70) or just a number
toutChangePointFast = function(hrs=NULL,df,trange=NULL,reweight=F,warn=T) {
  toutStr = 'tout'
  if(! is.null(hrs)) {
    sub = df$HOD %in% paste('H',sprintf('%02i',(hrs-1)),sep='')  # define hrs subset (#0-23 in the df)
    df = df[sub,]                                                # pull out the subset from the df
    df = df[!is.na(df$kw),]                                      # no NA's. Breaks the algorithm
  }
  else { toutStr = 'tout.mean' }
  if(is.null(trange)) {
    rng = floor(quantile(df[[toutStr]],c(0.1,0.90),na.rm=T))
    trange = c( rng[1]:rng[2]  )
  }

  prev = c(cp=-1,SSR=Inf)                                      # init the compare options
  warnMulti = F
  for(cp in trange) {
    #if(length(cp)>1) warnMulti = T # there is no guarantee against global minima
    out = evalCP(cp,df,reweight)                               # run piecewise regression
    if(out['SSR'] > prev['SSR']) { # the previous value was the min
      #plot(df$tout,df$kw,main=paste('Hr',paste(hrs,collapse=',')))
      #points(quickEst(cp,prev['(Intercept)'],prev['lower'],prev['upper']),type='l')
      if(warnMulti) {
        print(paste('warning: toutChangePointFast returning multiple change points: ',
                    paste(prev[grep('^cp[0-9]+',names(prev))],collapse=','),'. ',
                    'May be a local minima. Consider toutChangePoint instead.',sep=''))
      }
      return(prev)
    }
    prev = out
  }
  # failed search
  if (warn) {
    print(paste('Warning. SSR min not found for hr ',paste(hrs,collapse=','),
                '. Increase your temperature range? Returning higest value: ',
                paste(prev[grep('^cp',names(prev),value=T)],collapse=','),sep=''))
  }
  return(prev)
}


toutDoubleChangePoint = function(df) {
  tMin = floor(min(df$tout.mean,na.rm=T)+5)
  tMax = floor(max(df$tout.mean,na.rm=T)-5)
  changeModels = lapply(tMin:tMax,
                        function(coolCP) {
                          trange = lapply((coolCP-1):tMin-1, function(x) c(x,coolCP))
                          toutChangePointFast(df=df,trange=trange,reweight=F,warn=F)
                        })
  cm = do.call(rbind,changeModels)
  bestFit = cm[which.min(cm[,'SSR']),]
  return(bestFit)
}

#' @export
evalCP = function(cp,df,reweight=F) {
  out = list()
  lhs = 'kw'
  toutStr = 'tout'
  if(! is.null(df$kwh)) {
    lhs = 'kwh'
    toutStr = 'tout.mean'
  }
  tPieces = regressor.piecewise(df[[toutStr]],c(cp))
  middle = c()
  nSegs = dim(tPieces)[2]
  if(nSegs > 2) middle = paste('middle_',1:(nSegs-2),sep='')
  colnames(tPieces) <- c('lower',middle,'upper')
  # calculate weights such that each partition of Tout data makes the same
  # potential contribution to the SSR. So if, for example the data is partitioned
  # into 1/4 and 3/4 of obs in each, the minority data would be weighted at 3x
  n = dim(tPieces)[1]
  m = dim(tPieces)[2]
  df$w = rep(1,n) # default weights
  if(reweight) {
    # find the index of the last column with a non-zero value
    # for a piecewise regressor, this happens to be the number of non-zero
    # values per row
    highestCol = rowSums(tPieces != 0,na.rm=T)
    colCounts  = table(highestCol)
    # only alter the weights when all columns are participating
    if(length(colCounts) == m) {
      names(colCounts) <- colnames(tPieces)
      nobs       = sum(colCounts)
      ncols      = length(colCounts)
      colWeights = (nobs/ncols) / colCounts # spread equal weight across segments
      # even if one has fewer obs than the other
      #print(paste(sum(colWeights[highestCol]),nobs))
      #print(colWeights)
      if(colWeights[1] < 1) { # only re-weight if it improves cooling estimate
        df$w = colWeights[highestCol]
      }
    }
  }
  colSums(tPieces != 0)
  df = cbind(df,tPieces) # add the columns from the matrix to the df
  # define regression formula that uses the piecewise pieces
  #    f_0  = paste(lhs,'~',toutStr)
  f_cp = paste(lhs,'~',paste(colnames(tPieces),collapse=" + ",sep=''))
  #    fit_0  = lm(f_0, df) #,weights=w)
  fit_cp = lm(f_cp,df) #,weights=w)
  s_cp = summary(fit_cp)
  #    s_0  = summary(fit_0)
  coefficients = s_cp$coefficients[,'Estimate'] # regression model coefficients
  pvals        = s_cp$coefficients[,'Pr(>|t|)'] # coefficient pvalues
  # if one of our partitions has no data, we need to fill in the missing columns
  # in the returned data to ensure that sapply comes back as a matrix, not a list
  if(any(colSums(tPieces != 0,na.rm=T)==0)) {
    missingCols = setdiff(colnames(tPieces),names(coefficients))
    coefficients[missingCols] = NA
    pvals[missingCols]        = NA
  }
  names(pvals) = paste('pval',names(pvals))

  # weights to enforce a bayesian idea that higher temps should matter more
  # t > 75 gets a double weighting in the SSR
  #w = (df$tout > 75)*3 + 1
  #print(length(df$w))
  #print(length(fit_cp$residuals))
  SSR_cp = (df$w * fit_cp$residuals) %*% (df$w * fit_cp$residuals)
  #    SSR_0  = (df$w * fit_0$residuals ) %*% (df$w * fit_0$residuals )
  k = 1 # we have one regressor, so k = 1
  n = length(fit_cp$residuals)
  out$SSR = SSR_cp
  out$RMSE = sqrt(out$SSR/(n - 1))
  #    out$AIC_cp <- AIC(fit_cp)
  #    out$AIC_0  <- AIC(fit_0)
  k_cp = s_cp$df[1] #- 1 # degs of freedom, assuming intercept doesn't count
  #    k_0  = s_0$df[1]  #- 1
  # for comparison of models, see also f-test http://en.wikipedia.org/wiki/F-test#Regression_problems
  #    fstat = ( (SSR_0 - SSR_cp) / (k_cp - k_0) ) / ( SSR_cp / (n - k_cp) )
  #print(paste((SSR_0 - SSR_cp),(k_cp - k_0),SSR_cp,(n - k_cp)))
  #    plower = pf(fstat,k_cp - k_0, n - k_cp) # single sided
  #    nullModelTest = 2 * min(plower, 1 - plower)      # double sided p test for whether the cp model improves on a non-cp model
  return(c(cp=cp,SSR=out$SSR,
           #    AIC_cp=out$AIC_cp,
           #    AIC_0=out$AIC_0,nullModelTest=nullModelTest,
           coefficients,pvals))
}

# Model evaluation --------------------------------------------------------
#' @export
kFold = function(df,modelDescriptor,K=5) {
  folds <- sample(1:K, dim(df)[1], replace=T)
  residuals = c()
  for (i in 1:K) {
    fld = folds == i
    df$fold = fld
    fmla = modelDescriptor$formula
    subm = lm(fmla, data=df, subset=(!fold),na.action=na.omit)
    yhat = predict(subm,newdata=df[fld,])
    #print(length(yhat))
    ynm = as.character(formula(fmla))[[2]]
    residuals = c(residuals,df[,ynm][fld] - yhat) # accumulate the errors from all predictions
  }
  #plot(residuals)
  rmspe = sqrt(mean(residuals^2,na.rm=TRUE)) # root mean squared prediciton error RMSE
  return(rmspe)
}

#' @export
cvFold = function(df,modelDescriptor,K=5,R=1,costfn=cvTools::rmspe,seed=NULL) {
  fmla = formula(modelDescriptor$formula)
  cvOut = cvTools::cvFit(lm,formula=fmla,data=df,K=K,R=R,foldType='random',cost=costfn,seed=seed) #root mean squared prediction error
  #cvMape = cvTools::cvFit(lm,formula=fmla,data=df,K=K,R=R,foldType='random',cost=cvTools::mape,seed=seed) #root mean squared prediction error

  #print(cvOut)
  #print(names(cvOut))
  #print(cvOut$reps)
  return(mean(cvOut$cv))
}

smartCP = function(r) {
  df = regressorDF(r)
  simpleModel = lm('kw ~ tout + HOD -1',df)

  plot(cumsum(simpleModel$residuals))
}

#smartCP(rCO)
