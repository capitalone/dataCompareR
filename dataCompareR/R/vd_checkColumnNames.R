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

#' checkForRcompareCol
#'
#' @param df1 a data frame
#' @return None. Stops if error.
#' @examples
#' \dontrun{checkForRcompareCol(iris)}
checkForRCompareCol <- function(df1) {
  
  if ('dataCompareR_merged_indices' %in% names(df1)) {
    stop('ERROR : You cannot have a column in your dataframes called dataCompareR_merged_indices, as this is used internally')
  }
  
  
}