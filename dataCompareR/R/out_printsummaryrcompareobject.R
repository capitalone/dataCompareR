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

#' Printing summaryRCompare Output
#'
#' @param x an object of class "summary.dataCompareRobject", usually a result of a call to 
#' \code{\link{summary.dataCompareRobject}}.
#' @param ... Additional arguments passsed on to \code{\link{createTextSummary}}
#' @export
#' @examples
#' rc1 <- rCompare(iris,iris)
#' summary(rc1)  
print.summary.dataCompareRobject <- function(x,   ...) {
  y <- createTextSummary(x, ...)
  invisible(y)
}