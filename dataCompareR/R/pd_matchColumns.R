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

#' matchColumns : create subset of DFA and DFB to contain matching column names for both data frames
#' @param DFA input data frame
#' @param DFB input data frame
#' @return matchColOut named list of data frames. subsetA,subsetB contain only columns common to both data frames. colInfoA,colInfoB contain mapping of column names from original to treated and boolean indicator of common columns.
#' 
matchColumns <- function(DFA, DFB){

  colInfoA <- cleanColNames(DFA)
  colInfoA <- orderColumns(colInfoA)
  
  colInfoB <- cleanColNames(DFB)
  colInfoB <- orderColumns(colInfoB)
  
  colInfoList <- compareNames(colInfoA,colInfoB)
  
  matchColOut <- subsetDataColumns(DFA, DFB, colInfoList)
  return(matchColOut)
  
}





#' cleanColNames : get colnames, remove leading and trailing whitespace and push to upper case
#' @param DF Input dataframe
#' @return colInfo dataframe containing original and treated column names of DF

cleanColNames <- function(DF) {

  colName <- names(DF)
  mapping <- toupper(trimws(colName))
  
  colInfo <- data.frame(colName, mapping)
  
  return(colInfo)
  
}

#' orderColumns: order columns by treated column names
#' @param colInfo dataframe containing original and treated column names of DF
#' @return ordered colInfo dataframe containing original and treated column names of DF
#'
orderColumns <- function(colInfo) {

  colInfo <- colInfo %>% arrange(mapping)
  return(colInfo)
  
}

#' compareNames : compare the intersect of colInfoA and colInfoB and return boolean of matched columns for each data frame
#' @param colInfoA input data frames with original and treated column names
#' @param colInfoB input data frames with original and treated column names

compareNames <- function(colInfoA, colInfoB){

  commonCols <- intersect(colInfoA[,'mapping'], colInfoB[,'mapping'])
  
  colInfoA <- colInfoA %>% mutate(inB = (mapping %in% commonCols))
  colInfoB <- colInfoB %>% mutate(inA = (mapping %in% commonCols))
  
  colInfoList <- list(colInfoA,colInfoB,commonCols)
  names(colInfoList) <- c("colInfoA","colInfoB","commonCols")
  
  return(colInfoList)
  
}

#' subsetDataColumns : create subset of DFA and DFB to contain matching column names for both data frames
#' @param DFA input data frame
#' @param DFB input data frame
#' @param colInfoList named list containing the column mapping data frames and the list of common column names
#' @return matchColOut named list of data frames. subsetA,subsetB contain only columns common to both data frames. colInfoA,colInfoB contain mapping of column names from original to treated and boolean indicator of common columns.

subsetDataColumns <- function(DFA, DFB, colInfoList){

  names(DFA) <- c(trimws(toupper(names(DFA))))
  subsetDFA <- DFA %>% select_(.dots=colInfoList[["commonCols"]])
  
  names(DFB) <- c(trimws(toupper(names(DFB))))
  subsetDFB <- DFB %>% select_(.dots=colInfoList[["commonCols"]])
  
  matchColOut <- list(subsetDFA,subsetDFB,colInfoList[["colInfoA"]],colInfoList[["colInfoB"]])
  names(matchColOut) <- c("subsetDFA", "subsetDFB", "colInfoA", "colInfoB")
  
  return(matchColOut)
  
}

