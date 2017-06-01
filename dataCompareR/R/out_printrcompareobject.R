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


#' Printing RCompare Output
#'
#' Prints a brief report of an dataCompareR object to the screen.
#'
#' @family dataCompareR.functions
#' @param x an object of class "dataCompareR", usually a result of a call to \code{\link{rCompare}}.
#' @param nVars the number of mismatched columns to print and extract rows for
#' @param nObs the number of rows to print from the top and bottom of the mismatched list for each selected column
#' @param verbose logical; if TRUE will print out the full list of columns and rows that do not match
#' @param ... Passes additional arguments to print
#' @export
#' @examples
#' rc1 <- rCompare(iris,iris)
#' print(rc1)  

print.dataCompareRobject <- function(x, nVars=5, nObs=5, verbose=F, ...) {
  # Arg validation
  if(!is.numeric(nVars) | nVars < 1) {
    stop("nVars must be a positive number")
  }
  
  if(!is.numeric(nObs) | nObs < 1) {
    stop("nObs must be a positive number")
  }
  
  if(!is.logical(verbose)) {
    stop("verbose must be boolean") 
  }
  
  obslist <- NULL
  
  # Determine if we had a match key 
  
  if(length(x$rowMatching$matchKeys) == 1 && is.na(x$rowMatching$matchKeys)) {
    matchKeyUsed <- FALSE
  } else {
    matchKeyUsed <- TRUE
  }
  
  # Some basic warnings about the nature of the comparison - it should be clear if rows and columns
  # were dropped from the comparison
  # Columns
  if(length(x$colMatching$inA) == 0 && length(x$colMatching$inB) == 0) {
    # All columns compared
    cat("All columns were compared, ")
  } else {
    # Not all columns were compared
    cat(paste0(length(x$colMatching$inA) + length(x$colMatching$inB), " column(s) were dropped, " ))
    
  }
  
  
  if(length(x$rowMatching$inA[[1]]) == 0 && length(x$rowMatching$inB[[1]]) == 0) {
    # All rows
    cat("all rows were compared \n")
  } else {
    # Not all columns were compared
    cat(paste0(length(x$rowMatching$inA[[1]]) + length(x$rowMatching$inB[[1]]), " row(s) were dropped from comparison\n" ))
  }
  

  if (verbose != T) {
    varlist <- names(x$mismatches)
    
    uniquevarlist <- unique(append(head(varlist, nVars), tail(varlist, nVars)))
    if (length(uniquevarlist) == 0) {
      if (nchar(x$matches[1]) != 0) {
        allVarMatchMessage(x)
      }
      else {
        cat("No variables match")
      }
    }
    if (length(uniquevarlist) > 0) {
      noMismatchVars <- length(varlist)
      
      cat('There are ', noMismatchVars, "mismatched variables:\n")
      
      if (noMismatchVars <= nVars) {
        cat(
          'First and last',
          nObs,
          'observations for the ',
          noMismatchVars,
          'mismatched variables\n'
        )
        
      }
      else
      {
        cat(
          'First and last',
          nObs,
          'observations for first and last',
          nVars,
          'mismatched variables\n'
        )
      }
      
      obslist <- do.call(rbind, lapply(1:length(uniquevarlist),
                                       FUN = listObsNotVerbose, x
                                       , uniquevarlist, nObs))
      
      if(matchKeyUsed) obslist <- select(obslist, -rowNo)
      
      rownames(obslist) <- 1:nrow(obslist)
      
      print(obslist, ...)
    }
  }
  
  if (verbose == T) {
    uniquevarlist <- names(x$mismatches)
    
    if (length(uniquevarlist) == 0) {
      allVarMatchMessage(x)
    }
    
    if (length(uniquevarlist) > 0) {
      obslist <- do.call(rbind, lapply(1:length(uniquevarlist),
                                       FUN = listObsVerbose, x))
      
      if(matchKeyUsed) obslist <- select(obslist, -rowNo)
      
      print(obslist, ...)
    }
  }
  
  invisible(obslist)
}

