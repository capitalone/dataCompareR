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

#' collapseClasses. Collapse the classes of an object to a single string
#'
#' @param x any object
#' @return a string listing the classes of x, seperated by commas
#'
#' @examples 
#'\dontrun{collapseClasses(iris)}
#'\dontrun{collapseClasses("hello")}
collapseClasses <- function(x) {
  return(paste(class(x),collapse = ","))
}

#' mismatchHighStop Checks if we've exceeded threshold of mismatches
#'
#' @param trueFalseMatrix a matrix of true/false
#' @param maxMismatches number of mismatches at which the routine stopes
#' @return Nothing. Stops if threshold exceeded

mismatchHighStop <- function(trueFalseMatrix, maxMismatches) {
  # If we already have too many mismatches, stop
  if(!is.na(maxMismatches)) {
    mismatchCount <- (ncol(trueFalseMatrix)*nrow(trueFalseMatrix)) - sum(trueFalseMatrix, na.rm = TRUE)
    if(mismatchCount > maxMismatches) {
      stop(paste0("Detected at least ", mismatchCount, " mismatches. This exceeds the maximum mismatches",
                                                  " value of ", maxMismatches, " so dataCompareR has stopped." ))
    }
  }
}

#' Checks whether elements in two input data frames are equal.
#' 
#' @param DFA input data frame
#' @param DFB input data frame  
#' @param maxMismatches Integer. The max number of mismatches to assess, after which dataCompareR will stop 
#' (without producing a dataCompareR object). Designed to improve performance for large datasets.
#' @param keys character vector of index variables
#' 
#' @importFrom dplyr mutate_all
#' 
#' @return data frame containing keys and boolean logic of match/no match for each element
#'         If data types are not equal returns FALSE. Treats NA and NaN as unequal.
locateMismatches <- function(DFA, DFB, keys=NULL, maxMismatches=NA){

  # Short cut  - if there are no matching rows, just send back an empty DF
  if(nrow(DFA)==0) {
    return(data.frame())
  }
  
    # col names
  colNames <- names(DFA)
  
  # drop keys
  colCompare <- setdiff(colNames,keys)
  
  #print(dim(DFA))
  #print(dim(DFB))
  
  # find vars where type different excluding keys
  colTypeDiff <- sapply(select_(DFA,.dots=colCompare), collapseClasses) == sapply(select_(DFB,.dots=colCompare), collapseClasses)
  cols2Compare <- names(colTypeDiff[colTypeDiff==T])
  
  # select columns to compare
  if(length(cols2Compare)>0) {
    
    # First find matching cols with identical
    matchingCols <- vector(mode = 'logical', length = length(cols2Compare))
    for(i in 0:length(cols2Compare)) {
      matchingCols[i] <- identical(DFA[,cols2Compare[i]], DFB[,cols2Compare[i]])
    }
    
    # Get names of full matches
    colsFullMatch <- cols2Compare[matchingCols]
    
    # Create a list of cols with diffs
    cols2Diff <- setdiff(cols2Compare, colsFullMatch)
    
    if(length(cols2Diff) > 0) {
      
      # Now handle the cases where we're not equal 
      
      # Get these cols once
      subsetA <- select_(DFA,.dots = cols2Diff)
      subsetB <- select_(DFB,.dots = cols2Diff)
      
      # Look for NA's
      isNA_A <- mutate_all(subsetA, list(~is.na))
      isNA_B <- mutate_all(subsetB, list(~is.na))
      
      # Find any cells impacted by NA's
      anyNA <- isNA_A | isNA_B
      
      # and repeat the above for NAN's
      isNaN_A <- mutate_all(subsetA, list(~is.nan))
      isNaN_B <- mutate_all(subsetB, list(~is.nan))
      anyNaN <- isNaN_A | isNaN_B
      
      # find matching NA or NaNs
      matchNA <- isNA_A == isNA_B
      matchNaN <- isNaN_A == isNaN_B
      
      # Create a naive summary of matches first
      compareTF <- subsetA == subsetB
      
      # Check for mismatch count, stop if exceeded
      mismatchHighStop(compareTF,maxMismatches)
      
      # And then a somewhat confusing hierarchy...
      # If we get a true or a false from matchAnyway, this is correct
      # Otherwise, if they are both NA, we need to look at NA and NaN
      
      # Cols that have a NaN - overwrite with NaN matching status
      compareTF[anyNA] <- matchNA[anyNA]
      
      # Check for mismatch count, stop if exceeded
      mismatchHighStop(compareTF,maxMismatches)
      
      # and For NA's
      compareTF[anyNaN] <- matchNaN[anyNaN]
      
      # Check for mismatch count, stop if exceeded
      mismatchHighStop(compareTF,maxMismatches)
      
      # Make a DF
      compareTF <- as.data.frame(compareTF)
      
      # Add in cols that full match as all T
      compareTF[,colsFullMatch] <- TRUE
    }
    else {
      # Special case if no matching rows - need an empty DF
      if(nrow(DFA) > 0 ) {
        # If we get here, all is equal, so need a full DF of TRUES
        compareTF <- as.data.frame(matrix(T, nrow = nrow(DFA), ncol = length(cols2Compare)))
        names(compareTF) <- cols2Compare
      } else {
        compareTF <- data.frame()
      }
    }
    
    
    
  }
  else {
    compareTF <- data.frame()
  }
  
  # not compared
  colsNot2Compare <- names(colTypeDiff[colTypeDiff==F])
  
  # ID only
  mismatchOut <- data.frame(DFA[,keys], stringsAsFactors = FALSE)
  names(mismatchOut) <- keys
  
  if(nrow(compareTF) > 0) {
    # We have some matching rows, proceed as normal
    mismatchOut <- cbind(mismatchOut,compareTF)
    # where data types mismatch, return false
    mismatchOut[colsNot2Compare] <- FALSE
  } else {
    # There are no columns matching, so we must construct the object we need
    for(i in colsNot2Compare) {
      mismatchOut[i] <- FALSE
    }
    
    
  }
  
  
  
  # output columns in same order as input data frames
  # if statement handles cases where we have no overlap
  if(nrow(mismatchOut) == 0) {
    mismatchOut <- data.frame()
  } else {
    mismatchOut <- mismatchOut %>% select_(.dots=colNames)
  }
  
  return(mismatchOut)
  
}


