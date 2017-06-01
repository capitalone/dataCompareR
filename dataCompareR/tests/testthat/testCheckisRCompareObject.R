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

#
# UNIT TEST: is.dataCompareRobject
#
# * Assumes that dataCompareR output is accurate
#
# Checks that is.dataCompareRobject detects dataCompareRobjects and 
# what isn't an dataCompareRobject
#

library(testthat)

if(require(titanic)) {
  #The aim of this test is to ensure that is.dataCompareRobject correctly returns
  source('createTitanicDatasets.R')
  
  test_that("isdataCompareRobject", {
    
    # The object has already pre-determined structure
    
    compareObject <- rCompare(titanic2,titanic2)
    
    i1 <- is.dataCompareRobject(compareObject)
    i2 <- is.dataCompareRobject(titanic)
    
    expect_equal(i1, TRUE)
    expect_equal(i2, FALSE)
    
  })
} else {
  print('isdataCompareRobject not run, due to lack of titanic data')
}