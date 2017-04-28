# Copyright 2016 The Board of Trustees of the Leland Stanford Junior University.
# Direct inquiries to Sam Borgeson (sborgeson@stanford.edu) 
# or professor Ram Rajagopal (ramr@stanford.edu)


#' @export
tic <- function(name='default', gcFirst = FALSE, type=c("elapsed", "user.self", "sys.self"))
{
  type <- match.arg(type)
  assign(".type", type, envir=baseenv())
  if(gcFirst) gc(FALSE)
  tic = list()
  tryCatch({tic <- get(".tic", envir=baseenv())},
           error=function(e){  })
  tic[name] <- proc.time()[type]
  assign(".tic", tic, envir=baseenv())
  invisible(tic)
}

#' @export
toc <- function(name='default',prefixStr=NA)
{
  type <- get(".type", envir=baseenv())
  tic  <- get(".tic", envir=baseenv())
  dt   <- proc.time()[type] - as.numeric(tic[name])

  # must be ints...
  d <- floor(dt / 3600 / 24)
  h <- floor(dt / 3600) %% 24
  m <- floor(dt / 60)   %% 60
  s <- dt %% 60
  #f <- s - floor(s)
  #s <- floor(s)
  if(is.na(prefixStr)) prefixStr <- name
  print(paste(prefixStr,': ',sprintf('%02i:%02i:%02i:%05.2f',d,h,m,s),sep=''))

}

#' @export
mem.usage = function() {
  mem.use = sapply(ls(), function(x) { object.size(get(x))}, simplify = FALSE)
  print(sapply(mem.use[order(as.integer(mem.use))], function(var) { format(var, unit = 'auto') } ))
}

# print out crude profiling stats fort he padded in function with arguments ...
#' @export
run.profile = function(fn, ...) {
  Rprof("profile1.out", line.profiling=TRUE)
  test = fn(...)
  Rprof(NULL)
  summaryRprof("profile1.out", lines = "show")
}
