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
#' @param df a data frame
#' @return None. Stops if empty.
#' @examples
#' \dontrun{checkEmpty(iris)}

checkEmpty <- function(df) {
  
  # This was rewritten for speed. We do not want to do is.na() on a massive data frame
  # So this logic ensures we proceed for large data frames without running the is.na
  # step
  
  
  if(nrow(df)>1) {
    return()
  } else if (nrow(df) ==0 || is.na(df)) {
    stop('ERROR : One or more dataframes are empty')
  }
}


