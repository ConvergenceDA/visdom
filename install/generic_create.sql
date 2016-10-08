/*
SQLyog Community v12.2.4 (64 bit)
MySQL - 10.1.13-MariaDB : Database - pgeres
*********************************************************************
*/

/* NOTE: this is obviously a create script for MYSQL/MariaDB, but any other SQL database can work just as well.
   The important thing is to ensure that R can connect to your preferred DB.
   See also the import script for more information on how the databse can be populated and how indices can be
   added to exsure adequate performance.
*/

/*!40101 SET NAMES utf8 */;

/*!40101 SET SQL_MODE=''*/;

/*!40014 SET @OLD_UNIQUE_CHECKS=@@UNIQUE_CHECKS, UNIQUE_CHECKS=0 */;
/*!40014 SET @OLD_FOREIGN_KEY_CHECKS=@@FOREIGN_KEY_CHECKS, FOREIGN_KEY_CHECKS=0 */;
/*!40101 SET @OLD_SQL_MODE=@@SQL_MODE, SQL_MODE='NO_AUTO_VALUE_ON_ZERO' */;
/*!40111 SET @OLD_SQL_NOTES=@@SQL_NOTES, SQL_NOTES=0 */;
CREATE DATABASE /*!32312 IF NOT EXISTS*/`visdom_data` /*!40100 DEFAULT CHARACTER SET latin1 */;

USE `visdom_data`;

/*Table structure for table `account` */
/* all customer acocunt associated data should go here
   add or subtract columns to reflect what you know about customers
   the main idea is that account data can be added directly to customer features based on meter or account id
   so if you want a feature that reflects account information you already have, put it in this table.
*/

DROP TABLE IF EXISTS `account`;

CREATE TABLE `account` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `ACCOUNT_UUID` varchar(11) DEFAULT NULL,
  `METER_UUID` varchar(11) DEFAULT NULL,
  `ACCOUNT_START_DATE` datetime NOT NULL, # start of association of account_id with meter_id
  `ACCOUNT_END_DATE` datetime NOT NULL,   # end of association of account_id with meter_id
  `zip5` varchar(5) DEFAULT NULL,
  `RATE_PLAN` varchar(6) DEFAULT NULL,
  `CLIMATE_ZONE` varchar(4) DEFAULT NULL,
  `PV_INDICATOR` char(1) DEFAULT NULL,
  # you might also have demographics, census data, site physical characteristics, income, etc. here
  PRIMARY KEY (`id`),
  KEY `zip5_meter_uuid_idx` (`METER_UUID`,`zip5`),
  KEY `account_uuid_idx` (`ACCOUNT_UUID`),
  KEY `meter_uuid_idx` (`METER_UUID`)
) ENGINE=InnoDB AUTO_INCREMENT=327676 DEFAULT CHARSET=latin1;


/*Table structure for table `intervention` */
/* intervention data not required to run visdom, but useful for before/after studies
   add or subtract intervention data columns to match available intervention data */
DROP TABLE IF EXISTS `intervention`;

CREATE TABLE `intervention` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `account_uuid` varchar(11) DEFAULT NULL,
  `meter_uuid` varchar(11) DEFAULT NULL,
  `INSTALL_DATE` date DEFAULT NULL,
  `MEASURE_TYPE` varchar(10) DEFAULT NULL,
  `MEASURE_DESC` varchar(80) DEFAULT NULL,
  `TECHNOLOGY_TYPE` varchar(30) DEFAULT NULL,
  `EE_PROGRAM_NAME` varchar(10) DEFAULT NULL,
  `EST_ANNUAL_KW_SAVINGS` float DEFAULT NULL,
  `EST_ANNUAL_KWH_SAVINGS` float DEFAULT NULL,
  `EST_ANNUAL_THM_SAVINGS` float DEFAULT NULL,
  `INCENTIVE_PAYMENT` float DEFAULT NULL,
  `TOTAL_PROJECT_COSTS` float DEFAULT NULL,
  PRIMARY KEY (`id`),
  KEY `account_uuid_idx` (`account_uuid`),
  KEY `meter_uuid_idx` (`meter_uuid`),
  KEY `install_idx` (`INSTALL_DATE`)
) ENGINE=MyISAM AUTO_INCREMENT=13477 DEFAULT CHARSET=latin1;



/*Table structure for table `local_weather` */
/* this table matches the format of weather data provided by the NOAA data scraping GitHub project local-weather
   which keys off of zip code: http://github.com/sborgeson/local-weather 
*/

DROP TABLE IF EXISTS `local_weather`;

CREATE TABLE `local_weather` (
  `id` int(6) unsigned NOT NULL AUTO_INCREMENT,
  `zip5` int(11) NOT NULL,
  `date` datetime NOT NULL,
  `TemperatureF` float(5,2) DEFAULT NULL,
  `DewpointF` float(5,2) DEFAULT NULL,
  `Pressure` float(5,2) DEFAULT NULL,
  `WindSpeed` float(5,2) DEFAULT NULL,
  `Humidity` float(5,2) DEFAULT NULL,
  `Clouds` varchar(10) DEFAULT NULL,
  `HourlyPrecip` float(5,2) DEFAULT NULL,
  `SolarRadiation` float(6,2) DEFAULT NULL,
  PRIMARY KEY (`id`),
  UNIQUE KEY `zip_date_idx` (`zip5`,`date`),
  KEY `zip_idx` (`zip5`),
  KEY `date_idx` (`date`)
) ENGINE=MyISAM AUTO_INCREMENT=1637609 DEFAULT CHARSET=latin1;

/*Table structure for table `meter_data` */
/* hourly meter data uses 24 columns per row 15 minute interval data can use 96 cols in the same manner
   you can build a data source to pull data for each meter (i.e. all data from a given meter) or account
   i.e. for the period of time an individual account holder was behind a given meter. If you get the account_uuid
   correct over time, it is very simple for a data source to key by account rather than meter ids.
*/

DROP TABLE IF EXISTS `meter_data`;

CREATE TABLE `meter_data` (
  `meter_uuid` varchar(11) NOT NULL,
  `account_uuid` varchar(11) NOT NULL,
  `date` date NOT NULL,
  `zip5` varchar(5) DEFAULT NULL,
  `h1` int(11) DEFAULT NULL, # note that these are ints because they are cheaper to store than floats
  `h2` int(11) DEFAULT NULL, # and the assimption is that you can store Wh per interval period
  `h3` int(11) DEFAULT NULL, # converting to kWh/period on the way out of the db
  `h4` int(11) DEFAULT NULL, # thsi table can become HUGE, so optimizations like this are a good idea.
  `h5` int(11) DEFAULT NULL,
  `h6` int(11) DEFAULT NULL,
  `h7` int(11) DEFAULT NULL,
  `h8` int(11) DEFAULT NULL,
  `h9` int(11) DEFAULT NULL,
  `h10` int(11) DEFAULT NULL,
  `h11` int(11) DEFAULT NULL,
  `h12` int(11) DEFAULT NULL,
  `h13` int(11) DEFAULT NULL,
  `h14` int(11) DEFAULT NULL,
  `h15` int(11) DEFAULT NULL,
  `h16` int(11) DEFAULT NULL,
  `h17` int(11) DEFAULT NULL,
  `h18` int(11) DEFAULT NULL,
  `h19` int(11) DEFAULT NULL,
  `h20` int(11) DEFAULT NULL,
  `h21` int(11) DEFAULT NULL,
  `h22` int(11) DEFAULT NULL,
  `h23` int(11) DEFAULT NULL,
  `h24` int(11) DEFAULT NULL,
  PRIMARY KEY (`meter_uuid`,`date`),
  KEY `meter_uuid_idx` (`meter_uuid`),
  KEY `account_uuid_idx` (`account_uuid`),
  KEY `zip_Date_idx` (`date`,`zip5`),
  KEY `zip_idx` (`zip5`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1;


