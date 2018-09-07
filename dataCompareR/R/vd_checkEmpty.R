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

#' checkEmpty
#'
#' Checks if a df is actually a single NA, or has no columns
#'
#' @param df a data frame
#' @return None. Stops if empty.
#' @examples
#' \dontrun{checkEmpty(iris)}

checkEmpty <- function(df) {
  
  # This was rewritten for speed. We do not want to do is.na() on a massive data frame
  # So this logic ensures we proceed for large data frames without running the is.na
  # step
  
  if(is.null(ncol(df)) || is.na(ncol(df)) || ncol(df)==0) {
    stop("ERROR : One oe more dataframes have no coumns")
  }
}



#' CheckNA 
#' 
#' Checks a data frame is NA - if so, stops
#' 
#' @param df A (probable) dataframe
#' 
#' @return Nothing. Errors is df is NA

checkNA <- function(df) {
  if(isSingleNA(df)) {
    stop('ERROR : One or more dataframes are empty')
  }
}


#' isSingleNA
#' 
#' Boolean function - T if x is a single NA. False otherwise.
#' 
#' @param x literally anything
#' 
#' @return boolean
isSingleNA <- function(x) {
  if(is.vector(x) && length(x) == 1 && is.na(x)) {
    TRUE
  } else {
    FALSE
  }
}
