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

#' isNotNull: is object not null
#' 
#' @param x an object
#' @return \code{boolean} is object null T/F 
#' @examples 
#'\dontrun{isNotNull(NULL)}
#'\dontrun{isNotNull(5)}
isNotNull <- function(x)  !is.null(x)

#' outputSectionHeader: creates an outputSectionHeader
#' 
#' @param headerName a header name
#' @return \code{character} a character based section headers
outputSectionHeader <- function(headerName) {
  newLine <- "\n"
  decoration <- paste(rep("=", nchar(headerName)), collapse="")
  header <- paste(newLine, headerName, newLine, decoration, newLine, sep="")
  return(header)
}



