VISDOM_R_PATH = 'C:/dev/VISDOM-R/eneRgy/R'

DATASOURCE_PATH = '../../../SSSL_datasources/'

setwd( file.path(VISDOM_R_PATH) )

source('util-timer.R')               # adds tic() and toc() functions
source('iterator.R')                 # functions to iterate through collections of meters
source('classes-customer.R')         # loads MeterDataClass and WeatherDataClass


source(file.path(DATASOURCE_PATH,'/pgeResDataAccess.R'))    # provides data source implementation for pge_res data

QUERY_CACHE = 'e:/dev/pge_collab/EnergyAnalytics/batch/QUERY_CACHE_STANFORD/'
DATA_SOURCE = PgeResData(dbConfig=file.path(DATASOURCE_PATH,'pge_res_DB.cfg'), queryCache=QUERY_CACHE) # Use PGE res data for analysis
#DATA_SOURCE = PgeSmbData(dbConfig=file.path(DATASOURCE_PATH,'pge_smb_DB.cfg'), queryCache=QUERY_CACHE) # Use PGE res data for analysis


source('baseline.R') # baselineing functions

id = 34543252435432
zip = 93304
startTime = as.POSIXct('2013-01-01 20:00')
endTime   = as.POSIXct('2013-01-01 23:00')
daysBefore = 100
daysAfter  = 10

# 1. Load a MeterDataClass instance
meterData = MeterDataClass(820735863,94610,useCache=T,doSG=F);  plot(meterData,type='hourly',estimates=hourlyChangePoint(regressorDF(r),as.list(1:24),reweight=F))  # heat, no cooling
meterData = MeterDataClass(553991005,93304,useCache=T,doSG=F);  plot(meterData,type='hourly',estimates=hourlyChangePoint(regressorDF(r),as.list(1:24),reweight=F))  # very clear cooling 24x7

# 2. Pass meter data into baseline function with linear regression
result.regression = baseline.regression(meterData,startTime,endTime,daysBefore,daysAfter)

# 3. Pass meter data into baseline function with averaging over past 10 days
result.average = baseline.average(meterData,startTime,endTime,daysBefore,daysAfter)

# 4. Pass meter data into baseline function with gaussian process
result.gp = baseline.gp(meterData,startTime,endTime,daysBefore,daysAfter)

# 5. compare and plot the results....




