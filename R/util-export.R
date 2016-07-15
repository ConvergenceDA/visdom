# Copyright 2016 The Board of Trustees of the Leland Stanford Junior University.
# Direct inquiries to Sam Borgeson (sborgeson@stanford.edu)
# or professor Ram Rajagopal (ramr@stanford.edu)


#' @export
fixNames = function(df,prefix='') {
  if(class(df) == 'data.frame') {
    nms = names(df)
  } else {
    nms = df
  }
  fixed = gsub('[[[:punct:] ]','_',nms) # change ! " # $ % & ' ( ) * + , - . / : ; < = > ? @ [ \ ] ^ _ ` { | } ~ to underscores
  fixed = gsub('__+','_',fixed) # remove double or more underscores
  fixed = gsub('_$','',fixed)   # remove trailing underscores
  fixed = gsub('^_','',fixed)   # remove leading underscores
  fixed = paste(prefix,fixed,sep='')
  return(fixed)
}

#' @export
mergeShapeFeatures = function(features,shape.results) {
  newFeatures = merge(features,shape.results$shape.features[,c('id','entropy')],by.x='id',by.y='id',all.x=T)
  catCounts = shape.results$shape.stats$category.counts
  catTotals = rowSums(catCounts[,-1])
  catCounts[,-1] = catCounts[,-1] / catTotals
  names(catCounts) = gsub(pattern='count',replacement='pct',names(catCounts))
  newFeatures = merge(newFeatures,catCounts,by.x='id',by.y='id',all.x=T)
  return(newFeatures)
}

#' @export
cleanFeatureDF = function(features) {
  names(features) <- fixNames(features)
  # convert any factors to regular characters (otherwise the values are the factor indices)
  i <- sapply(features, is.factor)
  features[i] <- lapply(features[i], as.character)
  if( ! c('id') %in% names(features)) {
    stop('id column required for exported data')
  }
  if( ! c('zip5') %in% names(features)) {
    print('WARNING: VISDOM-web requires a zip5 geography column to produce maps')
  }
  return(features)
}

#' @export
writeH5Data = function(data,fName,label) {
  #source("https://bioconductor.org/biocLite.R")
  #biocLite("rhdf5")
  require(rhdf5)
  if(! file.exists(fName)) { h5createFile(fName) }
  h5write(data,fName,label)
}

#' @export
writeSqliteData = function(data,fName,label) {
  print(paste(fName,label))
  print('No SQLite support yet!')
}

#' @export
writeCSVData = function(data,fName,label=None) {
  write.csv(data, file=fName, row.names=F)
}

#' @export
exportShapes = function(shape.results,prefix='',format='hdf5') {
  name = paste(prefix,'LoadShape',sep='')
  exportData(shape.results$shape.stats$cluster.counts,  name, 'counts',          format)
  exportData(shape.results$shape.stats$cluster.energy,  name, 'sums',            format)
  exportData(shape.results$encoding.dict,               name, 'centers',         format)
  exportData(shape.results$encoding.dict.category.info, name, 'categoryMapping', format)
}

#' @export
exportData = function(df,name,label=None,format='hdf5') {
  ext = list(hdf5='h5',sqlite='',csv='csv')
  fn = list(hdf5=writeH5Data,sqlite=writeSqliteData,csv=writeCSVData)
  fName = paste(name,'.',ext[[format]],sep='')
  df = cleanFeatureDF(df)
  fn[[format]](df,fName,label) # call the format appropriate export function
}

#' @export
exportFeatureAndShapeResults = function(feature.rdata, shape.results.rdata=NULL, format='hdf5', prefix='') {

  load(feature.rdata)       # should provide var named featureDF
  if(! is.null(shape.results.rdata)) {

    print(paste('Using shape data',shape.results.rdata))
    load(shape.results.rdata) # should provide var named shape.results

    print('Merging shape features into basic features')
    featureDF = mergeShapeFeatures(featureDF,shape.results)

    print(paste('Writing load shape data to',format))
    exportShapes(shape.results,prefix,format)
  }
  featureDF = cleanFeatureDF(featureDF)

  print(paste('Writing feature data frame to',format))
  exportData(featureDF,paste(prefix,'Basics',sep=''),'basics',format)
}

