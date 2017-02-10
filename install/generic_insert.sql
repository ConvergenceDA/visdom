
# under some configurations, this file has to be in the main MySQL data directory
# ou can try to absolutely path to it, but if it doesn't work, see the MySQL docs on LOAD DATA
LOAD DATA LOCAL INFILE 'intervention_data.csv' 
IGNORE # ignore duplicates of the primary key
INTO TABLE intervention
FIELDS TERMINATED BY ','
lines terminated by '\r\n'
 IGNORE 1 LINES
 (  ACCOUNT_UUID,
    METER_UUID,
    @INSTALL_DATE_v, # load date as a string variable so it can be parsed with a date format into a date
    MEASURE_TYPE,
    MEASURE_DESC,
    TECHNOLOGY_TYPE,
    EE_PROGRAM_NAME,
    @EST_ANNUAL_KW_SAVINGS_v, # load numbers as strings to handle nulls.
    @EST_ANNUAL_KWH_SAVINGS_v,
    @EST_ANNUAL_THM_SAVINGS_v,
    @INCENTIVE_PAYMENT_v,
    @TOTAL_PROJECT_COSTS_v,
   )
 SET INSTALL_DATE   = STR_TO_DATE( SUBSTRING_INDEX(@INSTALL_DATE_v, ' ',  1), '%m/%e/%Y'), # convert dates
     EST_ANNUAL_KW_SAVINGS = nullif(@EST_ANNUAL_KW_SAVINGS_v,''), # handle blanks as nulls
     EST_ANNUAL_KWH_SAVINGS = nullif(@EST_ANNUAL_KWH_SAVINGS_v,''),
     EST_ANNUAL_THM_SAVINGS = nullif(@EST_ANNUAL_THM_SAVINGS_v,''),
     INCENTIVE_PAYMENT = nullif(@INCENTIVE_PAYMENT_v,''),
     TOTAL_PROJECT_COSTS = nullif(@TOTAL_PROJECT_COSTS_v,''),
     ID = NULL # auto-increment
 ;


LOAD DATA LOCAL INFILE 'account_data.csv' # this file has to be in the main data directory
IGNORE # ignore duplicates of the primary key
INTO TABLE account
FIELDS TERMINATED BY ','
LINES TERMINATED BY '\r\n'
 IGNORE 1 LINES
 (  account_uuid, 
    meter_uuid,
    @account_start_date_v,
    @account_end_date_v,
    zip5, 
    rate_plan,
    climate_zone, 
    @pv_indicator_v
   )
 SET pv_indicator = NULLIF(@pv_indicator_v,''),
     account_start_date = STR_TO_DATE( SUBSTRING_INDEX(@account_start_date_v, ' ',  1), '%m/%e/%Y'), # convert dates
     account_end_date   = STR_TO_DATE( SUBSTRING_INDEX(@account_end_date_v, ' ',  1), '%m/%e/%Y'), # convert dates
     ID = NULL
 ;


/* assuming fields are meter_id, date, and 24 x interval readings per row */
load data local infile 'interval_data.txt'
into TABLE meter_data
FIELDS TERMINATED BY ','
 IGNORE 1 LINES # ignore headers
 ( meter_uuid, 
   # note here we assume the account id was not included with the meter data, but can be added below
   @usage_date, # one row per meter day
   @h1, @h2, @h3, @h4, @h5, @h6,
   @h7, @h8, @h9, @h10, @h11, @h12,
   @h13, @h14, @h15, @h16, @h17, @h18,
   @h19, @h20, @h21, @h22, @h23, @h24
   )
 set date = STR_TO_DATE( SUBSTRING_INDEX(@usage_date, ' ', 1), '%m/%e/%Y'),
     h1 = round(@h1 * 1000), # add as ints in units of Wh/period to save disk space
     h2 = round(@h2 * 1000), # assuming raw data is kW/hour, we simply multiply by 1000
     h3 = round(@h3 * 1000), # and round into ints.
     h4 = round(@h4 * 1000), 
     h5 = round(@h5 * 1000), 
     h6 = round(@h6 * 1000), 
     h7 = round(@h7 * 1000), 
     h8 = round(@h8 * 1000), 
     h9 = round(@h9 * 1000), 
     h10 = round(@h10 * 1000), 
     h11 = round(@h11 * 1000), 
     h12 = round(@h12 * 1000), 
     h13 = round(@h13 * 1000), 
     h14 = round(@h14 * 1000), 
     h15 = round(@h15 * 1000), 
     h16 = round(@h16 * 1000), 
     h17 = round(@h17 * 1000), 
     h18 = round(@h18 * 1000), 
     h19 = round(@h19 * 1000), 
     h20 = round(@h20 * 1000), 
     h21 = round(@h21 * 1000), 
     h22 = round(@h22 * 1000), 
     h23 = round(@h23 * 1000), 
     h24 = round(@h24 * 1000),
     zip5 = NULL
 ;


# pull zip code data from the account table
UPDATE meter_data 
LEFT JOIN account ON meter_data.meter_uuid = account.meter_uuid
SET meter_data.zip5 = account.zip5;

# assuming meter_data doesnt have account ids, but the account table does, you can add them like this:
UPDATE meter_data AS md
LEFT JOIN account AS a ON md.meter_uuid = a.meter_uuid AND 
         md.date >= a.ACCOUNT_START_DATE AND 
         md.date <= a.ACCOUNT_END_DATE
SET md.account_uuid = a.account_uuid;

# get ready for weather data
# use the output of this as the input into weatherDump.py from http://github.com/sborgeson/local-weather
SELECT zip5, MIN(DATE), MAX(DATE) FROM meter_data GROUP BY zip5;
# save results as zip_dates.csv
# run with 3 stations averaged per zip and a preferred radius of less than 20 km
# python weatherDump.py -i path/to/zip_dates.csv -o path/to/out/dir/weather_data.csv -n 3 -d 20


/* load weather data in the format produced by local_weather's dumpWeather.py utility */
LOAD DATA LOCAL INFILE 'weather_data.csv' 
  INTO TABLE local_weather 
  FIELDS TERMINATED BY ','  
  LINES TERMINATED BY '\n'
  IGNORE 1 LINES
  (@dateStr, TemperatureF, DewpointF, Pressure, WindSpeed, Humidity, HourlyPrecip, zip5)
  SET `date` = STR_TO_DATE(@dateStr, '%Y-%m-%d %H:%i:%s');

