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

#' function for updating a compare object with  
#' information passed to it from the match rows function
#'  
#' @param x Object of information with classes related to the relevant section of the dataCompareRobject 
#' @param compObj dataCompareRobject to be updated 
#' @param matchKey the list of keys based on which the row matching was performed 
#' @return compObj Updated dataCompareRobject 
createRowMatching <- function(compObj, x, matchKey)
{
  #the input x is a list of lists. In x, the first
  #list is dfa which is a list of all elements in 
  #dataset A which are common between A and B. The
  #second list dfb is essentially the same as it contains 
  #elements in B which are common between A and B.
  #The third and fourth lists in x contain elements 
  #dropped from A and B respectively as they only
  #exist in A and B (respectively). These lists are
  #tranlsated into relevant elemnts of the object
  #rowMatching in this function. The object is 
  #then attached to the compObj.
  #if(matchKey == '' | length(matchKey)==0)
  #  stop('createRowMatching: no keys provided (length of matchKey is 0).')
  rowMatching <- list()
  matchKey <- c(matchKey)
  rowMatching$matchKey <- matchKey

  
  if (length(matchKey)==1 && is.na(matchKey)[[1]]) {
    
    rowMatching$inboth <- seq(1:nrow(x[[1]]))
    
  } else {
    rowMatching$inboth <- x[[1]][matchKey]
  }
  
  
  inA <- x[[3]][[1]]
  inB <- x[[3]][[2]]
  
  row.names(inA) <- NULL
  row.names(inB) <- NULL
  colnames(inA) <- NULL
  colnames(inB) <- NULL
  

  if(length(matchKey) >= 1)
  {
    rowMatching$inA <- as.list(x[[3]][[1]])
    rowMatching$inB <- as.list(x[[3]][[2]])
    
  }
  else
  {
    
    if(nrow(inA) > 0) {
      rowMatching$inA <- lapply (1:nrow(inA), function (i) unlist(t(inA)[,i]))
    } 
    else {
      rowMatching$inA <- NA
    }
    if(nrow(inB) > 0) {
      rowMatching$inB <- lapply (1:nrow(inB), function (i) unlist(t(inB)[,i]))
    }
    else {
      rowMatching$inB <- NA
    }
    
  }
  class(rowMatching) <- c("rowmatching")
  updateCompareObject(rowMatching, compObj)
  return (updateCompareObject(rowMatching, compObj))
}

