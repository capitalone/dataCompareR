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

#' Compare two data frames
#' @description Compare two data frames (or objects coercible to data frames) and produce a dataCompareR object containing 
#' details of the matching and mismatching elements of the data. See \code{vignette("dataCompareR")} for more details.
#' @family dataCompareR.functions
#' @param dfA data frame. The first data object. dataCompareR will attempt to coerce all data objects to data frames.
#' @param dfB data frame. The second data object. dataCompareR will attempt to coerce all data objects to data frames.
#' @param roundDigits Integer. If NA, numerics are not rounded before comparison. If specified, numerics are 
#'                    rounded to the specified number of decimal places using \link[base]{round}.
#' @param keys String. Name of identifier column(s) used to compare dfA and dfB. NA if no identifier (row order will be used instead), 
#'             a character for a single column name, or a vector of column names to match of multiple columns
#' @param mismatches Integer. The max number of mismatches to assess, after which dataCompareR will stop (without producing an dataCompareR 
#'                   object). Designed to improve performance for large data sets.
#' @param trimChars Boolean. If true, strings and factors have whitespace trimmed before comparison.
#' @return An dataCompareR object. 
#'          An S3 object containing details of the comparison between the two data objects. Can be used with \link{summary}, 
#'          \link{print}, \link{saveReport} and \link{generateMismatchData}
#' @importFrom dplyr filter
#' @importFrom dplyr select
#' @importFrom dplyr summarise
#' @importFrom dplyr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr select_
#' @importFrom dplyr arrange
#' @importFrom dplyr arrange_
#' @importFrom dplyr sample_n
#' @importFrom dplyr inner_join
#' @importFrom dplyr distinct
#' @importFrom utils head
#' @importFrom utils tail
#' @importFrom utils capture.output
#' @importFrom utils packageVersion
#' @export
#' @examples
#' iris2 <- iris
#' iris2 <- iris2[1:130,]
#' iris2[1,1] <- 5.2
#' iris2[2,1] <- 5.2
#' rCompare(iris,iris2,key=NA)
#' compDetails <- rCompare(iris,iris2,key=NA, trimChars = TRUE)
#' print(compDetails)
#' summary(compDetails)
#' 
#' pressure2 <- pressure
#' pressure2[2,2] <- pressure2[2,2] + 0.01
#' rCompare(pressure2,pressure2,key='temperature')
#' rCompare(pressure2,pressure2,key='temperature', mismatches = 10)

rCompare <- function(dfA,dfB,keys=NA, roundDigits = NA, mismatches = NA,trimChars = FALSE) {
  
  message('Running rCompare...')

  # Get args
  argsIn <- match.call()

  # Validate arguments
  validateArguments(matchKey = keys, roundDigits = roundDigits, maxMismatch = mismatches, coerceCols =  trimChars)
  checkNA(dfA)
  checkNA(dfB)
  
  # Make syntactically valid names & keys
  dfA <- makeValidNames(dfA)
  dfB <- makeValidNames(dfB)
  keys <- makeValidKeys(keys)
  
  # Coerce data
  coercedData <- coerceData(doa = dfA, dob = dfB)
  
  # Warn if data is large
  warnLargeData(as.double(nrow(coercedData[[1]])),
                as.double(ncol(coercedData[[1]])),
                as.double(nrow(coercedData[[2]])), 
                as.double(ncol(coercedData[[2]])))
  
  # Round data if needed 
  if(!is.na(roundDigits)) {
    coercedData[[1]] <- rounddf(coercedData[[1]], roundDigits)
    coercedData[[2]] <- rounddf(coercedData[[2]], roundDigits)
  }

  # Call process flow
  outObj <- processFlow(coercedData[[1]],coercedData[[2]],roundDigits,keys,mismatches,trimChars,argsIn)
  
  return(outObj)

}

#' Warn users if the calculation is likely to be slow
#' 
#' @description Checks if there are more than 20E6 elements for comparison. If there are, spits out a warning 
#' message that the calculation may run slowly
#' 
#' @param nrow_dfa number of rows in first data frame
#' @param ncol_dfa number of columns in first data frame
#' @param nrow_dfb number of rows in second data frame
#' @param ncol_dfb number of columns in second data frame
#' 
#' @return Nothing
warnLargeData <- function(nrow_dfa, ncol_dfa, nrow_dfb, ncol_dfb) {
  
  # Check for total number of cells
  totalSize <- as.double(nrow_dfa)*as.double(ncol_dfa) + as.double(nrow_dfb)*as.double(ncol_dfb)
  
  # If this is too large, warn the user...
  if(totalSize > 20E6) {
    message(paste0("CAUTION - There are ", totalSize, " elements across both data frames.",
                   "dataCompareR may take a little longer than usual for large data sizes."))
  }
  
}


