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

#' Round all numeric fields in a data frame
#' 
#' @param df A data frame to round
#' @param roundDigits Number of digits to round to
#' @return A rounded data frame

rounddf <- function(df, roundDigits) {
  
  df[sapply(df, is.numeric)] <- lapply(df[sapply(df, is.numeric)], round, roundDigits)
  return(df)
}
