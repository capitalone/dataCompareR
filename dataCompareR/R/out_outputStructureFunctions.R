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

#' listObsNotVerbose 
#' 
#' Return a summary of mismatching data
#' 
#' @param i The position of the element we want to compare
#' @param x An dataCompareR object
#' @param uniquevarlist A list of the variables in the compare
#' @param nObs How many obervations to return
#' @return A list of mismatching observations from start/end of mismatches
listObsNotVerbose <- function(i, x, uniquevarlist, nObs) {
  mismatchesHead <- head(x$mismatches[[uniquevarlist[i]]], nObs)
  mismatchesTail <- tail(x$mismatches[[uniquevarlist[i]]], nObs)
  
  mismatchesHead$rowNo <- as.numeric(rownames(mismatchesHead))
  mismatchesTail$rowNo <- as.numeric(rownames(mismatchesTail))
  
  obslist <- unique(rbind(mismatchesHead, mismatchesTail))
  
  # Value column must be a character to avoid missing factor level badness
  obslist[,1] <- as.character(obslist[,1])
  obslist[,2] <- as.character(obslist[,2])
  
  rownames(obslist) <- NULL
  
  obslist <- unique(obslist[c(length(obslist), 1:length(obslist) - 1)])
  obslist <- obslist[order(obslist[, 1]) ,]
  
  
}

#' listObsVerbose 
#' 
#' Return all mismatching data
#' 
#' @param i The position of the element we want to compare
#' @param x An dataCompareR object
#' @return A list of mismatching observations
listObsVerbose <- function(i, x) {
  mismatches <- x$mismatches[[i]]
  mismatches$rowNo <- as.numeric(rownames(mismatches))
  rownames(mismatches) <-NULL
  
  obslist <- unique(mismatches[c(length(mismatches), 1:length(mismatches) - 1)])
  obslist <- obslist[order(obslist[, 1]), ]
  
}

#' allVarMatchMessage 
#' 
#' Returns data about matching
#' 
#' @param x An dataCompareR object
#' @return A string containing the required message
allVarMatchMessage <- function(x){
  newLine <-"\n"
  cat("All compared variables match", newLine,
      "Number of rows compared:", 
      rcompObjItemLength(x$rowMatching$inboth), newLine,
      "Number of columns compared:", 
      rcompObjItemLength(x$colMatching$inboth))            
}
