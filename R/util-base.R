# Copyright 2016 The Board of Trustees of the Leland Stanford Junior University.
# Direct inquiries to Sam Borgeson (sborgeson@stanford.edu) 
# or professor Ram Rajagopal (ramr@stanford.edu)

#' @export
Mode <- function(x) {
  ux <- unique(x)
  tab = tabulate(match(x, ux))
  maxCount = max(tab)

  modes = ux[tab == maxCount] # there could be more than one
  out = sample(rep(modes,2),1) # random choice breaks ties; single mode is deterministic
  if(length(out) == 0) out = NA
  return(out)
}


# lag (shift) an array of values by n places, filling in the gap at the beginning with n NAs
#' @export
lag = function(v,n=1) {
  if(n==0) return(v)
  return(c(rep(NA,n),head(v,-n)))
} # prepend NAs and truncate to preserve length

# finite difference between observations
#' @export
diff2 = function(v,n=1) { return(c(rep(NA,n),diff(v, n))) } # prepend NAs to preserve length of standard diff

# calculate moving averages note adds n-1 NAs to beginning
# as.numeric is called because filter returns 'ts' objects which apparently don't play well with cbind and data.frames
#' @export
ma = function(v,n=5,weights=NULL) {
  if(length(weights) == 0) { weights = rep(1/n,n) } # standard moving window average
  as.numeric(stats::filter(v, weights, sides=1))
}




# utility fn to clear all active variables -
#leaves .varName vars behind to eliminate these, use ls(all.names=TRUE)
#' @export
clearAllVars = function() { rm(list=ls(),envir=baseenv()) }
