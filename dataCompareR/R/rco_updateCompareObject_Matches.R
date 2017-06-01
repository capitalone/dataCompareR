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

#' Adds a colMatching block to the output
#' 
#' @param x List of class 'matches' with column matching info
#' @param compObj dataCompareRobject instance to be updated
#' @return \code{compObj} Updated dataCompareRobject
updateCompareObject.matches <- function(x, compObj){
  compObj$matches <- x
  return(compObj)
} 