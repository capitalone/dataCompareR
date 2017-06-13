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

#' Extract data from a dataCompareR comparison
#' 
#' @description Produces a list of two data frames, containing the mismatched rows from the two input tables
#' 
#' Note that this function requires the user to pass in the two data frames used in the inital comparison. If this data
#' does not match that used for the generation of the dataCompareR object the results produced will not be accurate.
#' @family dataCompareR.functions
#' @param x A dataCompareRobject.
#' @param dfA Data frame (or object coercable to a data frame). One of the two data frames used in the initial rCompare call.
#' @param dfB Data frame (or object coercable to a data frame). One of the two data frames used in the initial rCompare call.
#' @param ... Unused currently, may be used in future
#' @export
#' @return \code{mismatchData} A list containing two objects: mismatched rows in first data object and mismatched rows in 
#' second data object
#' 
generateMismatchData <- function(x, dfA, dfB, ...){
  
  
  if(missing(dfA) | missing(dfB)) {
    stop("Please specify datasets in the function call")
  }
  
  argsIn <- match.call()
  
  
  
  if (!is.dataCompareRobject(x)) {
    stop("Input is not of class: dataCompareRobject")
  } else if(length(names(x$mismatches)) == 0) {
      message("No mismatches")
  } else {
    
    
    #Extract names of compared objects:
    DFAname <- x$meta$A[[1]]
    DFBname <- x$meta$B[[1]]
    
    # Check names match up between passed arguments and dataCompareR object
    if(as.character(argsIn$dfA) %in% c(DFAname,DFBname)) {
      # All is good
      } else {
        stop(paste0("Data frame named ", argsIn$dfA, " passed as argument, but not found in original comparison"))
      }
    
    if(as.character(argsIn$dfB) %in% c(DFAname,DFBname)) {
      # All is good
    } else {
      stop(paste0("Data frame named ", argsIn$dfB, "  passed as argument, but not found in original comparison"))
    }
    
    
    # Coerce data - is there a way to do this other than eval?
    coercedData <- coerceData(dfA, dfB)
    DFA <- coercedData[[1]]
    DFB <- coercedData[[2]]
    
    
    # If there are no keys, we can use the following approach based on rowname
    if(is.null(x$meta$args$keys)) {
      #Extract list of unique row numbers where mismatches occur:
      rowNoMismatches <- sort(unique(as.numeric(unlist(lapply(x$mismatches,row.names),use.names=F))))

      mismatchData <- list(DFA_mm = DFA[rowNoMismatches,],
                           DFB_mm = DFB[rowNoMismatches,])
      
      names(mismatchData) <- c(paste0(DFAname,'_mm'), paste0(DFBname,'_mm'))

      return(mismatchData)
      
    } else {
      
      # There are keys. Potentially multiple. 

      # Sort out columns names
      names(DFA) <- toupper(trimws(names(DFA)))
      names(DFB) <- toupper(trimws(names(DFB)))
      
      # Get keys - make upper
      keys <- toupper(eval(x$meta$args$keys))
      
      # Get all mismatches from the mismatch list
      allMism <- do.call(rbind,x$mismatches)
      distRows <- select_(allMism, keys) %>% distinct()
      
      # Get diffs by joining
      aMism <- suppressMessages(inner_join(DFA, distRows))
      bMism <- suppressMessages(inner_join(DFB, distRows))
      
      # Create mismatch object
      mismatchData <- list(DFA_mm = aMism, DFB_mm = bMism)
      
      names(mismatchData) <- c(paste0(DFAname,'_mm'), paste0(DFBname,'_mm'))
      
      return(mismatchData)
      
    }
  }
  
}
    
    

