# Copyright 2016 Convergence Data Analytics
# Direct inquiries to Sam Borgeson (sam@convergenceda.com) 

#' 
#' MOY - list of months of the year to include, using 1 for Jan through 12 for Dec
#' DOW - days of week to include, using 1 for Sun and 7 for Sat
#' start.date - the first day of data to include: all dates before this date are excluded
#' end.date - the last day of data to include: all dates after this date are excluded
#' @export
applyDateFilters = function(df, filterRules=NULL, dateCol="dates") {
  # if no rules, don't filter
  if( is.null(filterRules) ) { return(df) }
  
  # sanity check inputs - test for one or more valid filters
  validFilters = c('start.date', 'end.date', 'DOW', 'MOY')
  if( ! any(validFilters %in% names(filterRules) ) ) {
    stop('No valid filter rules.')
  }
  
  # sanity check inputs - check for proper date format
  dts = df[, dateCol]
  if( any( c('character','factor', 'Date', 'POSIXct') %in% class(dts) )) {
    if('Date' %in% class(dts)) {
      warning('Conversion from Date class to POSIXlt assumes UTC time zone, which can shift your dates around')
    }
    dts = as.POSIXlt(dts)
  }
  if( ! 'POSIXlt' %in% class(dts)) { 
    stop(sprintf('Dates from column %s not a valid date class type: %s',dateCol, class(dts))) 
  }
  
  DOW   = filterRules$DOW
  MOY   = filterRules$MOY
  start = filterRules$start.date
  end   = filterRules$end.date
  
  filter = rep(T, length(dts))
  if( ! is.null(DOW) )  { filter = filter & (dts$wday + 1) %in% DOW }
  if( ! is.null(MOY) )  { filter = filter & (dts$mon  + 1) %in% MOY }
  if( ! is.null(start)) { filter = filter & dts >= as.POSIXct(start) }
  if( ! is.null(end))   { filter = filter & dts <= as.POSIXct(end)   }
  return( df[which(filter),])
}



