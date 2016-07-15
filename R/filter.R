# Copyright 2016 The Board of Trustees of the Leland Stanford Junior University.
# Direct inquiries to Sam Borgeson (sborgeson@stanford.edu) 
# or professor Ram Rajagopal (ramr@stanford.edu)

rangeFilter = function(cust,ctx,...) {
  dates    = cust$dates
  filter = dates >= ctx$start.date & dates < ctx$end.date
  
}

DOWFilter = function(cust,ctx,...) {
  dates    = cust$dates
  days     = dates$day
  filter   = days %in% ctx$DOW.filter
  
}

monFilter = function(cust,ctx,...) {
  dates    = cust$dates
  mons     = dates$mon + 1
  filter   = mons %in% ctx$mon.filter
  
}

