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
# 
# See the License for the specific language governing permissions and limitations under the License. 


#' Compare data. Wrapper for comparison functionality.
#'
#' @param DFA dataframe as returned from prepareData
#' @param DFB dataframe as returend from prepareData
#' @param keys vector of chars - names of index variables
#' @param maxMismatches Integer. The max number of mismatches to assess, after which dataCompareR will stop 
#' (without produceing an dataCompareR object). Designed to improve performance for large datasets.
#' @return mismatchObject containing mismatch data for each of the variables in
#'         the dataframes
#'
#' @examples 
#'\dontrun{compareData(iris, iris)}
#'
#'\dontrun{iris2 <- iris}
#'\dontrun{iris2[1,1] <- 5.2}
#'\dontrun{iris2[2,1] <- 5.2}
#'\dontrun{compareData(iris, iris2)}
#'
#'\dontrun{compareData(pressure, pressure, keys = 'temperature')}
compareData <- function(DFA, DFB, keys = NULL, maxMismatches) {

  trueFalseTable <- locateMismatches(DFA, DFB, keys, maxMismatches)
  mismatchObject <- createMismatchObject(DFA, DFB, trueFalseTable, keys)
  return(mismatchObject)
}
