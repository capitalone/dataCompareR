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

#' checkKeysExist
#'
#' @param df a data frame
#' @param keys a list of expected columns
#' @return None. Stops if keys are not present as column names in df.
#' @examples
#' \dontrun{checkKeysExist(iris, 'columnName')}
checkKeysExist <- function(df,keys) {

  '%!in%' <- function(x,y)!('%in%'(x,y))
  
  for (i in 1:length(keys)) {
    
    if (keys[i] %!in% names(df)) {
      stop(paste0('ERROR : Key ',keys[i],' not found'))
    }

  }
  
}

