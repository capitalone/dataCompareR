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


#' validateData : routine to validate the input data
#'
#' @param df1 a data frame
#' @param df2 a data frame
#' @param keys Keys used
#' @return None. Stops if error.
#' @examples
#' \dontrun{validateData(iris,iris)}
validateData <- function(df1,df2,keys=NA) {
  
  checkEmpty(df1)
  checkEmpty(df2)  
  
  if(length(keys) > 1 || !is.na(keys)) {
    checkKeysExist(df1,keys)
    checkKeysExist(df2,keys)
  }
  
  checkForRCompareCol(df1)
  checkForRCompareCol(df2)

}

