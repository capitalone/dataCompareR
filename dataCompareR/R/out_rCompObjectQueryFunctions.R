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

#' rcompObjItemLength: return length of an item, returning 0 if null, and handling the fact that
#' we might have a data frames or a vector
#' 
#' @param x an object
#' @return length, numeric
rcompObjItemLength <- function(x){
  if (isNotNull(x)) {
    if(is.data.frame(x)) {
      len <- nrow(x)
    } else {
      len <- length(x)
    }
  } else {
    len <- 0
  }
  return(len)
}


#' colsWithUnequalValues: a dataframe summarising a column with unequal values
#' 
#' @param x the column to be considered
#' @param mismatches - a mismatches object from an dataCompareR object
#' @return data frame with a summary of the mismatching column
colsWithUnequalValues <- function(x, mismatches){
  if (isNotNull(x)) {
    
    colName <- names(mismatches[x])
    
    colTypeA <- unlist(unique(mismatches[[x]]['typeA']))
    colTypeB <- unlist(unique(mismatches[[x]]['typeB']))
    
    nDif <- unlist(nrow(mismatches[[x]]))
    
    maxDif <- as.character(suppressWarnings(unlist(
      apply(mismatches[[x]]['diffAB'], 2, function(x) { 
        if (is.numeric(x) & length(x) > 0) { 
          max(abs(x), na.rm = T) 
          } else {
            NA
            }
        }))))
    
    maxDif[maxDif == "-Inf"] <- ""
    
    missDif <- unlist(sum(is.na(mismatches[[x]]['diffAB'])))
    
    data.frame(colName, colTypeA, colTypeB, nDif, maxDif ,missDif, 
               row.names = NULL)
  }
}