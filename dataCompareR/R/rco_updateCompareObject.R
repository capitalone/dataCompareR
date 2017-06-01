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

#' Generic function for updating a compare object with 
#' information passed to it, that has methods based on the class
#' of the info argument.
#' 
#' @param x Object of information with classes related to the relevant section of the dataCompareRobject
#' @param compObj dataCompareRobject to be updated
#' @return compObj Updated dataCompareRobject
updateCompareObject <- function(x, compObj){
  UseMethod("updateCompareObject", x)
}