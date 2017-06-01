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

#' Converts cleaning info into a format consumable by updateCompareObject.
#' 
#' @param compObj dataCompareRobject to be updated
#' @param cleaningInfo list of cleaning information
#' @return \code{compObj} updated dataCompareRobject
#' 
createCleaningInfo <- function(compObj, cleaningInfo){
  cleanInfoObj <- as.list(getCoercions(cleaningInfo$DataTypes))
  class(cleanInfoObj) <- c("cleaninginfo")
  return(updateCompareObject(cleanInfoObj, compObj))
}

#' Subsets on the variables that have a coercion. 
#' 
#' @param typesDf Dataframe of type information from the executeCoercion function
#' @return \code{coercedT} Subset version of typesDf where a coercion occurred
getCoercions <- function(typesDf){
  typesDfT <- as.data.frame(t(typesDf), row.names=names(typesDf))
  typesDfT <- cbind(names(typesDf),typesDfT, stringsAsFactors = FALSE)
  names(typesDfT) <- c("colName","origA","origB","newA","newB")
  coercedCols <- dplyr::filter(typesDfT, as.character(typesDfT[,"origA"]) != 
                          as.character(typesDfT[,"newA"])
                        | as.character(typesDfT[,"origB"])
                        != as.character(typesDfT[,"newB"]))
  coercedT <- as.data.frame(t(coercedCols[,-1]))
  names(coercedT) <- as.vector(coercedCols[,1])
  return(coercedT)
}
