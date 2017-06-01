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

#' trimCharVars: trim white spaces in character variables from an input dataframe
#'
#' @param DF Input dataframe
#' @return \code{DF} with preceding and trailing white spaces removed from character fields
#' @examples
#' \dontrun{trimCharVars(iris)}

trimCharVars<-function(DF){
  
  DF<-data.frame(sapply(DF, function(x) if (collapseClasses(x)=="character") {trimws(x, which = "both")} else {x}, simplify=F), stringsAsFactors=FALSE)
  
  return(DF)
}
