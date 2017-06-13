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

#' processFlow Handles the process flow for the whole package
#' 
#' @param dfa Dataframe. One of the two data frames to be compared
#' @param dfb Dataframe. One of the two data frames to be compared
#' @param argsIn The arguments that were passed to the main dataCompareR function
#' @param keys The keys used to match rows between \code{dfa} and \code{dfb}
#' @param trimChars Boolean. Do we trim characers before comparing?
#' @param mismatches Integer. The max number of mismatches to assess, after which dataCompareR will stop 
#' (without produceing an dataCompareR object). Designed to improve performance for large datasets.
#' @param roundDigits Integer. If NA, numerics are not rounded before comparison. If /code{roundDigits} is specified, numerics are 
#'                    rounded to /code{roundDigits} decimal places using \link[base]{round}.
#' @return \code{dataCompareRObject} containing details of the comparison

processFlow <- function(dfa, dfb, roundDigits, keys,mismatches, trimChars,argsIn) {
  
  
  # 1. Create an r compare object
  rCompObj <- createCompareObject()

  # 2. Validate data
  validateData(dfa,dfb,keys)
  
  # 3. Create meta and update
  rCompObj <- createMeta(rCompObj,dfa,dfb,argsIn ,timestamp = Sys.time(),roundDigits )
  
  # 4. Prepare data
  preparedData <- prepareData(dfa, dfb, keys, trimChars)
  
  # Keys are returned upper cased, so overwrite
  keys <- preparedData$keys
  
  # 5. createMatching
  rCompObj <- createColMatching(rCompObj,colMatchInfo = preparedData$colMatching)
  rCompObj <- createRowMatching(rCompObj, preparedData$rowMatching, keys)
  
  # 6. createCleaning
  rCompObj <- createCleaningInfo(rCompObj,cleaningInfo = preparedData$coerceData)
  
  # 7. compareData
  # This expects NULL isntead of NA
  compareDataKey <- keys
  if(length(compareDataKey) == 1 && is.na(compareDataKey)) compareDataKey <- NULL
  matchingData <- compareData(DFA = preparedData$rowMatching[[1]], DFB = preparedData$rowMatching[[2]], keys = compareDataKey, maxMismatches =  mismatches )
  
  # 8. createMismatches
  rCompObj <- createMismatches(rCompObj, misObj = matchingData)

  return(rCompObj)
  
}
  


