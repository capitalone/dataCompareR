# SPDX-Copyright: Copyright (c) Capital One Services, LLC 
# SPDX-License-Identifier: Apache-2.0 
# Copyright 2017 Capital One Services, LLC 
#
# Licensed under the Apache License, Version 2.0 (the "License"); 
# you may not use this file except in compliance with the License. 
#
# You may obtain a copy of the License at http://www.apache.org/licenses/LICENSE-2.0 
#
# Unless required by applicable law or agreed to in writing, software distributed 
# under the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS
# OF ANY KIND, either express or implied.

#'Takes raw info for meta and adds it to the compare object
#'
#'@param x List of class 'meta' with data related to meta
#'@param compObj dataCompareRobject to be appended
#'@return \code{compObj} dataCompareRobject updated with meta block
updateCompareObject.meta <- function(x, compObj){
  
  compObj$meta <- list()
  
  #print(x$args)
  
  # One-for-one x to compObj additions
  compObj$meta$args <- x$args
  compObj$meta$runTimestamp <- x$runTimestamp
  
  # Info about the data
  #     If the arguments don't have these labels, 
  #     these will be 'character(0)'
  compObj$meta$A <- metaDataInfo(as.character(x$args$dfA), x$DFA)
  compObj$meta$B <- metaDataInfo(as.character(x$args$dfB), x$DFB)
  
  # Object version for future compatibility handling
  compObj$meta$objVersion <- currentObjVersion()
  
  compObj$meta$roundDigits <- x$roundDigits
  
  return(compObj)
}

#' Creates a list of info about the dataframe.
#' 
#' @param name The variable name of the df from the dataCompareR function call
#' @param df A data frame
#' @return \code{dfInfo} A list of info about the data frame
metaDataInfo <- function(name, df){
  
  dfInfo <- list()
  dfInfo$name <- name
  # Removed summary as it was quite slow for large datasets
  #dfInfo$summary <- summary(df)
  dfInfo$rows <- nrow(df)
  dfInfo$cols <- ncol(df)
  return(dfInfo)
}