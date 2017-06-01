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

#' prepareData Prepares data for comparison in 3 stages.
#'             1. Match columns - filter dataframes to those columns that match and summarise differences
#'             2. Match rows - filter dataframes to those rows that match and summarise differences
#'             3. Coerce data 
#' 
#' @inheritParams rCompare
#' @return \code{dataCompareRObject} containing details of the comparison
#' @examples 
#' \dontrun{dfA <- iris}
#' \dontrun{dfB <- iris}
#' \dontrun{keys <- NA}
#' \dontrun{prepareData(dfA,dfB,keys, trimChars = T)}

prepareData <- function(dfA, dfB, keys = NA, trimChars = T) {
  
  # ----- 1. Match columns ----- 
  colMatching <- matchColumns(dfA, dfB)
  
 
  # -----  2. Coercions ----- 
  
  coerceData <- executeCoercions(colMatching[[1]],colMatching[[2]],trimChars)
 
  # -----  3. Match rows -----
  rowMatching <- matchRows(coerceData$DFA, coerceData$DFB, toupper(keys) )
  
   
  toReturn <- list()
  toReturn$colMatching <- colMatching
  toReturn$coerceData <- coerceData
  toReturn$rowMatching <- rowMatching
  toReturn$keys <- toupper(keys)
  
  return(toReturn)
  
  }