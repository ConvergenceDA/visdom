# Copyright 2016 The Board of Trustees of the Leland Stanford Junior University.
# Direct inquiries to Sam Borgeson (sborgeson@stanford.edu) 
# or professor Ram Rajagopal (ramr@stanford.edu)

#library(party)

#' @title 
#' bootstrap several runs of the ctree algorithm
#' 
#' @description
#' This function repeatedly runs the ctree algorithm on random sub-samples of the underlying data
#' to provide a measure of robustness for the individual ctree results.
#' 
#' @param df data.frame with response variable to be explained and all explanatory variables for the ctree to run with
#' @param nRun number of bootstrapping runs to perform
#' @param nSample number of samples (with replacement) for each run 
#' @param ctl ctree control instance to be passed to ctree algorithm to control stopping criteria, etc.
#' @param responseVar the name of the variable that the ctree is trying to explain
#' @param ignoreCols an optional list of columns in df, but that should be excluded from the ctree modeling
# 
#' @export
ctree.boot = function(df, nRun=100, nSample=NULL, ctl=NULL, responseVar, ignoreCols) {
  if(is.null(nSample)) { nSample = nrow(df) / 10 }
  out = adply( 1:nRun, 
               .margins=1, 
               .fun = ctree.subscore, ctl=ctl,
               df=df, nSample=nSample, responseVar=responseVar, ignoreCols=ignoreCols,
               .id='run'
               #.parallel=T,
               #.progress = 'time'
  )
  return(out)
}

#' @title 
#' bootstrap spread scores across several runs of the ctree algorithm
#' 
#' @description
#' This function repeatedly randomly sub-samples the passed data.frame to provide points for classificaiton 
#' by the passed ctree model and calculates the resulting spread score for classifications of each subset 
#' and returns a vector of the resulting scores. It is useful for determining the robustness of the spread score 
#' for a particular data set.
#' 
#' @param df data.frame with response variable to be explained and all explanatory variables to be used in ctree classificaiton
#' @param df.ct ctree already trained on data, that is used to classify the data points in each sub sample.
#' @param nRun number of bootstrapping runs to perform
#' @param nSample number of samples (with replacement) for each run
#' @param responseVar the name of the variable that the ctree is trying to explain
#' @param ignoreCols an optional list of columns in df, but that should be excluded from the ctree modeling
#' @export
spread.boot = function(df, df.ct, nRun=100, nSample=NULL, responseVar, ignoreCols) {
  if(is.null(nSample)) { nSample = nrow(df) / 10 }
  out = plyr::adply( 1:nRun, 
               .margins=1, 
               .fun = spreadScore, 
               df=df,
               nSample=nSample,
               df.ct=df.ct, 
               responseVar=responseVar, #ignoreCols=ignoreCols, # spreadScore arguments
               .id='run'
               #.parallel=T,
               #.progress = 'time'
  )
  names(out)[2] = 'score'
  return(out)
}

#' @title 
#' fit a ctree with passed data and compute and return its spread score
#' 
#' @description
#' This function trains a ctree using data from a passed data.frame and returns the resulting spread score.
#' 
#' @param df data.frame with response variable to be explained and all explanatory variables to be used in ctree classificaiton
#' @param nSample number of samples if hte spread score should be run on a subset of the data
#' @param ctl ctree control to specify ctree model parameters
#' @param responseVar the name of the variable that the ctree is trying to explain
#' @param ignoreCols an optional list of columns in df, but that should be excluded from the ctree modeling
#' @export
ctree.subscore = function(i, df, nSample=NULL, ctl=NULL, responseVar, ignoreCols) {
  sub = sample(1:nrow(df),nSample)
  df.ct = ctree.run(df, fmla=formula(paste(responseVar,'~ .')), sub, ctl=ctl, responseVar, ignoreCols )
  score = spreadScore(df[sub, ], df.ct, nSample=nSample, responseVar=responseVar, ignoreCols=ignoreCols)
  return( data.frame(score=score) )
}

#' @title 
#' run a ctree model
#' 
#' @description
#' This function trains a partykit::ctree using data from a passed data.frame.
#' 
#' @param df data.frame with response variable to be explained and all explanatory variables to be used in ctree classificaiton
#' @param fmla formula, referring to values in the df, specifying the variables to be used to train the ctree model
#' @param ctl partykit::ctree_control instance to specify ctree model parameters
#' @param responseVar the name of the variable that the ctree is trying to explain
#' @param ignoreCols an optional list of columns in df, but that should be excluded from the ctree modeling
#' @param ... additional arguments to be passed to partykit::ctree
#' 
#' @export
ctree.run = function(df, fmla=NULL, sub=T, ctl=NULL, responseVar, ignoreCols, ... ) {
  if(is.null(fmla)) { 
    fmla = formula(paste(responseVar,'~ .'))
  }
  if(is.null(sub)) { sub=T }
  if(is.null(ctl)) {
    ctl = partykit::ctree_control(mincriterion = 0.95)
  }
  df.ct  <- partykit::ctree(  fmla,
                              data = rm.col( df[sub,], setdiff(ignoreCols,responseVar)), 
                              control = ctl, ... )
  return(df.ct)
}

#' @title 
#' given a ctree model, compute its spread score
#' 
#' @description
#' This function computes the "spread score" for a sample of data, given a ctree explaining a binary variable.
#' New data samples are "predicted" into their ctree nodes and the probability of a 'True' response value for 
#' their members is fed into a weighted average of the absolute distance from the sample mean across all dat asamples.
#' The higher the score, the better the model has done at classifying people with divergent behaviors (higher or loewr than average).
#' 
#' @param df data.frame with response variable to be explained and all explanatory variables to be used in ctree classificaiton
#' @param df.ct ctree already trained on data, that is used to classify the data points in each sub sample.
#' @param nSample number of samples (with replacement) to use for the calculation
#' @param responseVar the name of the variable that the ctree is trying to explain
#' @param ignoreCols an optional list of columns in df, but that should be excluded from the ctree modeling
#' 
#' @export
spreadScore = function(df, df.ct, nSample=NULL, responseVar, ignoreCols=c()) {
  sub=T
  if( ! is.null(nSample) ) {
    sub = sample(1:nrow(df),nSample)
  }
  newData = rm.col(df[sub,], c(responseVar,ignoreCols))
  responsePct = mean( as.logical(df[,responseVar]) )
  responseProb = NULL
  if("constparty" %in% class(df.ct)) { # if the tree is from party
    responseprob = predict(df.ct, newdata = newData, type='prob')[,2]
  } else { # the tree must be from partykit
    responseprob = sapply(partykit::treeresponse(df.ct, newdata = newData), FUN = function(x) x[2])
  }
  relResponse = responsePct - responseprob
  #print(head(relResponse))
  return(mean(abs(relResponse)))
}


#' @title 
#' ctree classificaiton results plot for a binary response variable
#' 
#' @description
#' plot a histogram of customer segments and enrollment percentages, based on the membership of ctree leaf nodes.
#' The plot is a "histogram" whose x-axis is the average "True" response for each ctree leaf node and whose 
#' height is the number of customers in each corresponding group. Strong results have nodes with high membership
#' far from the sample mean.
#' 
#' @param df data.frame with response variable to be explained and all explanatory variables to be used in ctree classificaiton
#' @param df.ct ctree already trained on data, that is used to classify the data points in each sub sample.
#' @param responseCol the name of the variable that the ctree is trying to explain
#' @param title figure title of the plot
#' @param idCol column with data row identifiers in it, which is removed from the df
#' @param xlab label for x-axis of the figure
#' @param normalize boolean for whether the x-axis should be percentages compared to the sample mean or absolute percentages
#' @param compareData optional data.fram of one additional set of bar values to be plotted as well.
#' @param colors optional named list specifying the figure colors.
#' 
#' @export
groupHist = function(df, df.ct, responseCol, title='Group probabilities', 
                     idCol='id', xlab='Enrollment probability (%)', 
                     normalize=FALSE, compareData=NULL, colors=NULL) {
  if(is.null(colors)) {
    colors = list()
    colors$fill1st = 'grey24'
    colors$line1st = 'black'
    colors$fill2nd = 'lightblue'
    colors$line2nd = 'blue'
    colors$linemean = 'red'
  }
  response = as.logical(df[,responseCol])
  newData = rm.col(df, c(idCol,responseCol))
  responseProb = NULL
  if("constparty" %in% class(df.ct)) {
    df$responseprob = predict(df.ct, newdata = newData, type='prob')[,2]
    df$group = predict(df.ct, newdata = newData, type='node')
  } else {
    df$responseprob = sapply(treeresponse(df.ct, newData = newData), FUN = function(x) x[2])
    df$group = where(df.ct, newdata = newData)
  }
  meanLine = data.frame(x=mean(response)) * 100
  barStats = as.data.frame(as.list(aggregate(df$responseprob, by=list(df$group), FUN=function(x) { c(count=sum(x>0),enroll_prob=mean(x)*100) } )))
  names(barStats) = c('nodeId','counts','enroll_prob')
  barStats$label = toupper(letters[rank(-barStats$enroll_prob)])
  barStats$group = barStats$label
  barStats$pct = barStats$count / sum(barStats$count) * 100
  meanLine$meanCount = mean(barStats$counts)
  meanLine$maxCount = max(barStats$counts)
  meanLine$maxPct = max(barStats$pct)
  #print(barStats)
  p = ggplot(barStats, aes(x=enroll_prob, y=pct, label=label)) +
    geom_bar(stat='identity', width=0.2, color=colors$fill1st, fill=colors$fill1st) + 
    labs(title=title,x=xlab) +
    geom_text(vjust=-1) + 
    scale_x_continuous(breaks = seq(12,24,1)) +
    ylim(0, max(barStats$pct) * 1.1)
  if( ! is.null(compareData) ) {
    p = p + geom_bar(data=compareData, mapping=aes(x=enroll_prob, y=pct, label=psycographic), 
                     stat='identity', color=colors$fill2nd, fill=colors$fill2nd, width=0.05)
  }
  #p = p + scale_y_continuous(labels = percent)
  p = p + geom_vline(data=meanLine, aes(xintercept=x), color=colors$linemean, size=1) + 
    geom_text(data=meanLine, aes(x=x, y=maxPct*1.1, hjust=-0.05), 
              label="full sample mean", 
              vjust = 0, colour=colors$linemean, angle=0, size=4.5) +
    theme( text = element_text(size=15) ) + theme_bw()
  
  return(p)
}