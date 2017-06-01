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

#' createTextSummary: create a text based summary of an dataCompareR object
#' 
#' @param x an dataCompareR object
#' @param ... Arguments passed on to other functions
#' @return cat's lines to the screen (or to be captured)
createTextSummary <- function(x,...) {
  
  dataCompHeader <- 'Data Comparison'
  metaSummHeader <- 'Meta Summary'
  varSummHeader <- 'Variable Summary'
  dataTypeMisHeader <- 'Listing of Common Columns with Different Data Types'
  rowSummHeader <- 'Row Summary'
  dataSummHeader <- 'Data Values Comparison Summary'
  missRowHeader <- 'Dropped Rows Details'
  
  newLine <- "\n"
  colon <- ": "
  space <- "  "
  
  #############################################################################
  
  cat(outputSectionHeader(dataCompHeader))
  cat(newLine)
  
  cat(paste0("Date comparison run: ",x$runtime, space))
  cat(newLine)
  cat(paste0("Comparison run on ", x$rversion, space))
  cat(newLine)
  cat(paste0("With dataCompareR version ",x$version, space))
  cat(newLine)
  cat(newLine)
  
  cat(outputSectionHeader(metaSummHeader))

  print(kable(x$datasetSummary, row.names = FALSE))
  cat(newLine)
  if(x$rounding) {
    cat(paste0('Numeric values were rounded to ', x$roundDigits, ' decimal place(s).'))
    cat(newLine)
  }
  
  cat(outputSectionHeader(varSummHeader))
  cat(newLine)
  
  cat(paste0("Number of columns in common: ", x$ncolCommon, space))
  cat(newLine)
  cat(paste0("Number of columns only in ", x$datanameA, colon, x$ncolInAOnly, space))
  cat(newLine)
  cat(paste0("Number of columns only in ", x$datanameB, colon, x$ncolInBOnly, space))
  cat(newLine)
  cat(paste0("Number of columns with a type mismatch: ", x$typeMismatchN, space))
  if(x$ncolID > 0) {
    cat(newLine)
    cat(paste0("Match keys : ", x$ncolID, space, " - ", paste0(x$matchKey,collapse = ", ")))
    cat(newLine)
  } else {
    cat(newLine)
    cat("No match key used, comparison is by row")
    cat(newLine)
  }
  
  cat(newLine)
  if(x$ncolInAOnly >0) {
    cat(newLine)
    cat(paste0("Columns only in ", x$datanameA, colon, paste(x$colsInAOnly,collapse = ', '), space))
  }
  
  if(x$ncolInBOnly >0) {
    cat(newLine)
    cat(paste0("Columns only in ", x$datanameB, colon, paste(x$colsInBOnly,collapse = ', '), space))
  }
  
  if(x$ncolInAOnly >0 | x$ncolInBOnly >0) {
    cat(newLine)
    cat(paste0("Columns in both ", colon, paste(x$colsInBoth,collapse = ', '), space))
  }
  
  # A massive if/else here - if there are no matching columns, there's not much point in continuing...
  # The "Listing of Common Columns with Different Data Types" chunk
  # Only needed if anything to print...
  
  if(x$ncolCommon == 0) {
    cat(newLine)
    cat(newLine)
    cat("No columns match, so no comparison could take place")
  } else {
    if (x$typeMismatchN > 0) {
      cat(outputSectionHeader(dataTypeMisHeader))
      cat(newLine)
      print(kable(x$typeMismatch,row.names = FALSE))
      cat(newLine)
    }
    cat(newLine)
    
    cat(outputSectionHeader(rowSummHeader))
    cat(newLine)
    
    cat(paste0("Total number of rows read from ", x$datanameA, colon, x$nrowA, space))
    cat(newLine)
    cat(paste0("Total number of rows read from ", x$datanameB, colon, x$nrowB, space, space))
    cat(newLine)
    cat(paste0("Number of rows in common: ", x$nrowCommon,space))
    cat(newLine)
    cat(paste0("Number of rows dropped from ", x$datanameA, colon, x$nrowInAOnly, space))
    cat(newLine)
    cat(paste0("Number of rows dropped from  ", x$datanameB, colon, x$nrowInBOnly, space))
    cat(newLine)
    #if(isNotNull(x$ncolID) & showRowMatching) {
    #'  cat(newLine)
    #  cat(paste0("Number of rows with some compared columns unequal: ", x$nrowSomeUnequal, space))
    #  cat(newLine)
    #  cat(paste0("Number of rows with all compared columns equal: " ,x$nrowAllEqual, space))
    #  cat(newLine)
    #}
    cat(newLine)
    
    cat(outputSectionHeader(dataSummHeader))
    cat(newLine)
    
    # Some if else trickery..
    
    if(x$nrowCommon == 0) {
      cat(paste0("No rows were compared, so no summary can be provided"))
    } else {
      cat(paste0("Number of columns compared with ALL rows equal: ", x$ncolsAllEqual, space))
      cat(newLine)
      cat(paste0("Number of columns compared with SOME rows unequal: ", x$ncolsSomeUnequal, space))
      cat(newLine)
      cat(paste0("Number of columns with missing value differences: ", x$nrowNAmismatch, space))
      cat(newLine)
      cat(newLine)
      
      # If some columns are equal, list them
      if(x$ncolsAllEqual > 0) {
        cat("Columns with all rows equal : ")
        cat(paste(x$ColsMatching, collapse = ', '))
      }
      
      cat(newLine)
      cat(newLine)
      
      # If some columns are uneuqal, list them
      if(x$ncolsSomeUnequal>0) {
        cat("Summary of columns with some rows unequal: ")
        cat(newLine)
        cat(newLine)
        print(kable(x$colsWithUnequalValues, row.names = FALSE))
        cat(newLine)
        cat(newLine)
        cat(outputSectionHeader("Unequal column details"))
        cat(newLine)
        cat(newLine)
        cat(newLine)
        for(z in 1:length(x$colMismDetls)) {
          cat("#### Column - ", names(x$colMismDetls[z]) )  
          cat(newLine)
          if(nrow(x$colMismDetls[[z]]) >= x$mismatchCount) {
            cat(paste0("Showing sample of size ", x$mismatchCount))
            cat(newLine)
          }
          cat(newLine)
          # If no match key, print the row names, otherwise don't
          if(is.null(x$matchKey) || (length(x$matchKey) == 1 && is.na(x$matchKey))) {
            print(kable(x$colMismDetls[[z]], row.names = TRUE ))
          } else {
            print(kable(x$colMismDetls[[z]], row.names = FALSE))
          }
          cat(newLine)
          cat(newLine)
        }
      }
      
  
      # Optional, if required, block about which rows were dropped
      # Only shown if match keys were used and some rows didn't match
      if(x$ncolID > 0 & (nrow(x$rowsInAOnly) >0 | nrow(x$rowsInBOnly)  > 0))
      {
        cat(outputSectionHeader(missRowHeader))
        cat(newLine)
        cat(newLine)
        if(nrow(x$rowsInAOnly) >0) {
          cat('The following rows were dropped from ', x$datanameA)
          cat(newLine)
          if(nrow(x$rowsInAOnly) >= x$mismatchCount) {
            cat(paste0("Showing sample of size ", x$mismatchCount))
            cat(newLine)
          }
          print(kable(x$rowsInAOnly))
        } else {
          cat('No rows were dropped from ', x$datanameA)
          cat(newLine)
        }
        
        cat(newLine)
        cat(newLine)
        if(nrow(x$rowsInBOnly) >0) {
          cat('The following rows were dropped from ', x$datanameB)
          cat(newLine)
          if(nrow(x$rowsInBOnly) >= x$mismatchCount) {
            cat(paste0("Showing sample of size ", x$mismatchCount))
            cat(newLine)
          }
          print(kable(x$rowsInBOnly))
        } else {
          cat('No rows were dropped from ', x$datanameB)
          cat(newLine)
        }
        
      }
      
    }
  }
}
