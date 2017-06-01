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

#' createReportText: prepares text which is used in the summary report
#' Saves R markdown and HTML reports in the area specified by the user. Reports are called RcompareReport.Rmd (.html)
#' Uses knitr package to create tables in the markdown (createReportText function) and HTML report. 
#'  
#' @param x input object which summary comparison information
#' @return text in R markdown format
#' @examples  
#'\dontrun{createReportText(x=MysummaryCompareObject)}
createReportText <- function(x){
  y <- createTextSummary(x)
  invisible(y)
}
