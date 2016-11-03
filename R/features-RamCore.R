# Copyright 2016 The Board of Trustees of the Leland Stanford Junior University.
# Direct inquiries to Sam Borgeson (sborgeson@stanford.edu)
# or professor Ram Rajagopal (ramr@stanford.edu)

#daily_usage = Load
#bud = Base load
#pud = Peak load
#pud/bud = Peak-to-base ratio
#morning_peak_hour = morning Peak hour (most common)
#afternoon_peak_hour = afternoon Peak hour (most common)
#daily_peak_hour = Peak hour (most common)
#Max load [Actual maximum in period]
#Min load  [Actual minimum in period]
#Load duration (@ 99%?)
#ldc_sum, ldc_win = Load duration curve on common basis
#Load quantiles  (1%, 97%, 10%, 20%, ?, 90%)
#Total load during peak hours
#Cooling sensitivity [using Jungsuk?s improved model!]
#Heating sensitivity [using Jungsuk?s improved model!]
#Lighting sensitivity
#Heating/Cooling change point
#Cooling energy
#Heating energy
#Baseload energy
#Lighting energy
#Load shape frequency in the 7 classes
#Entropy
#Gas

#' @export
coreFeaturesfn = function(meterData, ctx, ...){

  #load('dicionary200.RData') ## for dic200 use
  dic200 = ctx$dic200

  ## get meter data
  #meterData = getMeterDataClass(custID) ## assume this is meter data, but not exactly sure the format yet
  ## for the time being, I assume the number of rows are the number of days
  dlen = nrow(meterData$kwMat)
  ## need to decide whether to return any possible features when the meter doesn't have some data for given period
  ## using ctx$start.date and ctx$end.date, we can handle this

  ## declare the features
  len = 1 ## as this function is for each meter
  #   cool_coef = matrix(0,len,48) ## temp coef below a breakpoint, and standard deviation of the coef
  #   heat_coef = matrix(0,len,48) ## temp coef over a breakpoint, and standard deviation of the coef

  daily_usage = matrix(0,len,dlen) ## daily consumption
  bud = matrix(0,len,dlen) ## base usage per day
  pud = matrix(0,len,dlen) ## peak usage per day
  morning_peak_hour = matrix(0,len,dlen) ## morning peak hour per day
  afternoon_peak_hour = matrix(0,len,dlen) ## afternoon peak hour per day
  daily_peak_hour = matrix(0,len,dlen) ## daily peak hour per day
  encode200 = matrix(0,len,dlen) ## encoding by the dictionary of size 200
  entropy200 = matrix(0,len,3) ## load shape code entropy using the dictionary of size 200, 1st column is for summer, 2nd for winter, 3rd for a year
  cv = matrix(0,len,dlen) ## consumption variability
  ldc.sum = matrix(0,len,24) ## load duration curve
  ldc.win = matrix(0,len,24) ## load duration curve
  ldc.all = matrix(0,len,24) ## load duration curve

  delta = 0.2 ## parameter to be used in generating 4 features below
  ram_sum = matrix(0,len,1) ## ram's idea
  ram_win = matrix(0,len,1) ## ram's idea
  ram_sum_vec = matrix(0,len,24) ## ram's idea
  ram_win_vec = matrix(0,len,24) ## ram's idea

  ## features from ram's definition
  RS = matrix(0,len,1)
  VS=matrix(0,len,1)
  Pu=matrix(0,len,1)
  Pu.mean=matrix(0,len,1)
  R1=matrix(0,len,1)
  R2=matrix(0,len,1)
  R3=matrix(0,len,1)
  R4=matrix(0,len,1)

  ## to calculate temperature sensitivity, we need to know zip code
  ## it can be provided via "meterData" or "ctx".
  ## Anyway, I decided to use temperature sensitivity from Sam's features which can be extracted in more general condition

  odata = kjs.impute(as.matrix(meterData$kwMat),1:24) ## depending on meter data format, adjust here
  odata[which(is.na(odata))] <- 0 # HACK!! What to do with missing data!?
  i=1
  encode200 = class::knn(dic200,odata,1:200)

  ## assume shannon.entropy2 in utility.r is included in the same package. if not, include manually.
  ## sumidx and -sumidx should be populated based on ctx$start.date and ctx$end.date

  midx = as.POSIXlt(meterData$days)$mon + 1 ## month index
  sumidx = which(midx%in%c(5:10)) ## consider May to Oct as summer
  #print(dim(odata))
  #print(length(midx))
  #print(encode200[sumidx])
  entropy200[i,1] = shannon.entropy2(encode200[sumidx])
  entropy200[i,2] = shannon.entropy2(encode200[-sumidx])
  entropy200[i,3] = shannon.entropy2(encode200)

  morning_peak_hour[i,] = apply(odata[,1:12],1,which.max)
  afternoon_peak_hour[i,] = apply(odata[,13:24],1,which.max)+12
  daily_peak_hour[i,] = apply(odata,1,which.max)
  peak.hours = 13:18 # 1 to 5 pm
  peak.hr.tot.load = mean(apply(odata[,peak.hours],1,sum))

  daily_usage[i,] = apply(odata,1,sum)
  bud[i,] = apply(odata,1,min)
  pud[i,] = apply(odata,1,max)

  bu = sum(odata[,10:22])
  tu = sum(odata)
  RS[i] = bu/tu ## ratio between peak time usage(9AM-10PM) and total usage
  #     VS[cnt] = 0.8/(1+0.2*(tu-bu)/bu)
  VS[i] = 0.2/(1+0.8*(tu-bu)/bu)
  tmp = apply(odata[,10:22],1,sum)
  Pu[i] = max(tmp) ## max of peak time usage
  Pu.mean[i] = mean(tmp) ## mean of peak time usage

  ## monthly 1~5th daily peak selection
  # note pud is daily peak
  top5PerMonth = matrix(0,5,12) # rows are top 5 ranked daily peaks, cols are months
  for (j in unique(midx)){
    ## assume the data length is at most 1year
    top5PerMonth[,j] = sort(pud[which(midx==j)],decreasing=T)[1:5]
  }

  peakSums = apply(top5PerMonth,1,sum)
  print(peakSums)
  R1[i] = 1-peakSums[2]/peakSums[1] # ratio of 2nd highest peaks to highest peaks
  R2[i] = 1-peakSums[3]/peakSums[1]
  R3[i] = 1-peakSums[4]/peakSums[1]
  R4[i] = 1-peakSums[5]/peakSums[1]

  cv[i,] = apply(odata,1,function(j){
    sum(sqrt((1/24)^2+diff(j)^2))
  })
  #print(length(sumidx))
  #print(dim(odata))
  #print(odata[sumidx,])
  ld = sapply((1:9)*.1,FUN =function(x) { sum(odata>(max(odata)*x)) } )
  ldc.all[i,] = apply(apply(odata,1,sort,decreasing=T),1,mean,na.rm=T)
  ldc.sum[i,] = apply(apply(odata[sumidx,],1,sort,decreasing=T),1,mean,na.rm=T)
  ldc.win[i,] = apply(apply(odata[-sumidx,],1,sort,decreasing=T),1,mean,na.rm=T)

  fmat = apply(odata,1,function(j){
    maxbin = floor(max(j/delta))
    ftable = matrix(0,1,25)
    if (maxbin<1) ftable
    tl = c()
    for (k in 1:maxbin){
      t3 = diff(c(0,j>k*delta,0))
      tl = c(tl, which(t3==-1)-which(t3==1))
    }
    tmp = table(tl)
    ftable[as.numeric(names(tmp))] = tmp
    ftable[25] = mean(tl)
    ftable
  })
  ram_sum_vec[i,] = apply(fmat[1:24,sumidx],1,mean)
  ram_win_vec[i,] = apply(fmat[1:24,-sumidx],1,mean)

  ram_sum[i,] = mean(fmat[25,sumidx])
  ram_win[i,] = mean(fmat[25,-sumidx])

  return(list(custID = meterData$id, daily_usage = daily_usage, bud = bud, pud = pud,
              morning_peak_hour = morning_peak_hour, afternoon_peak_hour = afternoon_peak_hour, daily_peak_hour = daily_peak_hour,
              peak.hr.tot.load = peak.hr.tot.load,
              encode200 = encode200, entropy200 = entropy200, cv = cv, ld = ld, ldc.all = ldc.all, ldc.sum = ldc.sum, ldc.win = ldc.win,
              ram_sum = ram_sum, ram_win = ram_win, ram_sum_vec = ram_sum_vec, ram_win_vec = ram_win_vec,
              RS = RS, VS = VS, Pu = Pu, Pu.mean = Pu.mean, R1 = R1, R2 = R2, R3 = R3, R4 = R4))

}
