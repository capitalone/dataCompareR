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

#' Create mismatch object
#'
#' @param compObj RCompareObject, output from processFlow
#' @param misObj MismatchObject, output from compareData (processFlow)
#' @param keys Character vector, the keys matched on, to allow removal of any extra columns
#'              introudced by the compare process
#'
#' @return The mismatch object
#'
createMismatches <- function(compObj, misObj, keys) {
  mismatches <- misObj[!is.na(misObj)]
  matches <- names(misObj[is.na(misObj)])
  class(mismatches) <- c("mismatches")
  if (is.null(matches)) matches <- ""
  class(matches) <- c("matches")
  tempObj <- updateCompareObject(mismatches, compObj)
  return(updateCompareObject(matches, tempObj))
}
