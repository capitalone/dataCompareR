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

#' Takes the raw info for the meta block of the output
#' and puts it in a format usable by the updateCompareObject
#' function
#' 
#' @param dataCompareRobject Object of class dataCompareRobject
#' @param DFA First data set passed in to the dataCompareR function 
#' @param DFB Second data set passed in to the dataCompareR function
#' @param arguments Collection of arguments passed to compare object with labels that match the dataCompareR arg definitions
#' @param timestamp Timestamp 
#' @param roundDigits The number of digits to round to, using \link[base]{round}
#' @return \code{dataCompareRobject}
#' 
createMeta <- function(dataCompareRobject, DFA, DFB, arguments, timestamp, roundDigits){
  
  # Add meta info to a list
  metaObject <- list()
  metaObject$DFA <- DFA
  metaObject$DFB <- DFB
  metaObject$args <- arguments
  metaObject$runTimestamp <- timestamp
  metaObject$roundDigits <- roundDigits
  
  # Change the class of the list to 'meta'
  class(metaObject) <- c("meta", "list")
  
  # Update the compare object and return it
  dataCompareRobject <- updateCompareObject(metaObject, dataCompareRobject)
  return(dataCompareRobject)
}