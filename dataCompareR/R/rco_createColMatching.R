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

#' Converts the output of the column matching logic to something consumable by updateCompareObject.
#' 
#' @param compObj dataCompareRobject instance to be updated
#' @param colMatchInfo List output from the column matching logic
#' @return \code{compObj} updated with colMatching block
#' 
createColMatching <- function(compObj, colMatchInfo){
  # Keeping the explicit references to the implementation of col matching logic here
  # This hard-coding is technical debt. It would be better to manage these kinds of
  # references in a more centralized way
  colNameCol <- "colName"
  matchFlagColA <- "inB"
  matchFlagColB <- "inA"
  colMatching <- list()
  colMatching$matchingColNames <- names(colMatchInfo$subsetDFA)
  colMatching$inA <- getMismatchColNames(colMatchInfo$colInfoA, colNameCol, matchFlagColA)
  colMatching$inB <- getMismatchColNames(colMatchInfo$colInfoB, colNameCol, matchFlagColB)
  class(colMatching) <- c("colmatching")
  return(updateCompareObject(colMatching, compObj))
}


#' Extracts the column names only in one data frame from a table of 
#' match information 
#' 
#' @param colMatchInfo Dataframe with column names, match flag
#' @param colNameCol Name of the column with the column names
#' @param matchFlagCol Name of the column with the match flag
#' @return Vector of column names that do not match 
#' 
getMismatchColNames <- function(colMatchInfo, colNameCol, matchFlagCol){
  mismatchColNames <- dplyr::filter(colMatchInfo, colMatchInfo[,matchFlagCol]==FALSE)[[colNameCol]]
  return(mismatchColNames)
}
