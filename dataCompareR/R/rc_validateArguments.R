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

#' validateArguments
#'
#' @param matchKey A character or character vector of column names to match on
#' @param coerceCols Boolean - do we coerce columns names?
#' @param maxMismatch Cap for number of mismatches 
#' @inheritParams rCompare
#' @return Nothing. Errors if any parameters are invalid.
#' @examples
#' \dontrun{validateArguments('plantName',1E-8,T,1000)}
#' \dontrun{validateArguments('colorName',1E-9,F,10)}
validateArguments <- function(matchKey = NA, roundDigits = NA, coerceCols = TRUE , maxMismatch = NA)
{

  # Check that match key is empty, or is a character, or is a vector of characters
  if(length(matchKey) == 1) {
    if (!is.na(matchKey) & !is.character(matchKey))
    {
      stop("ERROR: Key must be character")
    }
  }
  else if (!is.character(matchKey))
  {
    stop("ERROR: Key must be character")
  }
  

  # Check that roundDigits is numeric or NA
  if (is.numeric(roundDigits) | is.na(roundDigits)) {
  } else {
    stop("ERROR: roundDigits must be an integer or NA ")
  }
  
  # Check that coerceCols is boolean
  if (!is.logical(coerceCols))  {
    stop("ERROR: Coerce flag must be Boolean")
  }
  
  # Check that maxMismatch is numeric, > 0 
  if (is.numeric(maxMismatch) | is.na(maxMismatch)) {
  }
  else {
    stop("ERROR: Mismatch must be numeric or NA")
  }
  
  if (is.numeric(maxMismatch) & maxMismatch <= 0)  {
    stop("ERROR: Mismatch must be greater than 0")
  }
  
  if (is.numeric(maxMismatch) & maxMismatch%%1!=0)  {
    stop("ERROR: Mismatch must be an integer")
  }
  
}

#' makeValidNames
#' 
#' Correct syntactically invalid names in a data frame
#' @param df A data frame
#' @return A data frame with syntactically valid names 
#' @examples
#' \dontrun{makeValidNames(iris)}
makeValidNames <- function(df) {
  
  # Get make names version of names
  nm <- make.names(names(df), unique = TRUE, allow_ = TRUE)
  
  if(!all(nm == names(df))) {
    message('Fixing syntactically invalid names')
     
    message(paste('    Names changed from - ', paste(names(df)[!(nm == names(df))], collapse = ", ")))
    message(paste('                    to - ', paste(nm[!(nm == names(df))], collapse = ", ")))
    
    names(df) <- nm
  }
  
  return(df)

}


#' makeValidKeys
#' 
#' Correct syntactically invalid Keys
#' @param keys A character vector
#' @return A character vector with syntactically valid names 
#' @examples
#' \dontrun{makeValidKeys(c(" hello", "__BAD NAME___")}
makeValidKeys <- function(keys) {
  
  # check for empty keys first
  if(length(keys) ==1 && is.na(keys)) {
    return(keys)
  }
  
  # Get make names version of keys
  keysClean <- make.names(keys, unique = TRUE, allow_ = TRUE)
  
  # Check if changes are needed
  if(!all(keysClean == keys)) {
    message('Fixing syntactically incorrect keys')
    
    message(paste('    Keys changed from - ', paste(keys[!(keys == keysClean)], collapse = ", ")))
    message(paste('                   to - ', paste(keysClean[!(keys == keysClean)], collapse = ", ")))
    
    keys <- keysClean
  }
  
  return(keys)
  
}
