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
# See the License for the specific language governing permissions and limitations under the License. 

#
# UNIT TEST: summary.dataCompareRobject
#
# summary.dataCompareRobject returns a summary.recompareobject object that
# has a certain structure depending on the content of the dataCompareR object
#
# * Assumes that the dataCompareR function output is correct
#
# This is not a complete test
#

if(require(titanic)) {

  source('createTitanicDatasets.R')
  
  # Create a series of data we can use for testing with a single index
  
  context("OutputComparisons : Summary")
  
  test_that("compareSummaryEqual", {
    
    
    # Test 1 - no rows differing
    
    # The object has already pre-determined structure
    compareObject <- rCompare(titanic2,titanic2)
    
    s0 <- summary(compareObject)
    
    
    expect_is(s0, "summary.dataCompareRobject")
    expect_output(str(s0), "List of 32")
    expect_equal(s0$ncolCommon, 13)
    expect_equal(s0$ncolInAOnly, 0)
    expect_equal(s0$ncolInBOnly, 0)
    expect_equal(s0$typeMismatch, 0)
    expect_equal(s0$typeMismatchN, 0)
     expect_equal(s0$nrowInAOnly, 0)
    expect_equal(s0$nrowInBOnly, 0)
    
    expect_equal(s0$ncolsAllEqual, 13)
    expect_equal(s0$colsWithUnequalValues, 0)
    expect_equal(s0$nrowNAmismatch, 0)
    
    expect_equal(length(s0$colsInAOnly),0)
    expect_equal(length(s0$colsInBOnly),0)
    expect_equal(length(s0$colsInBoth),13)
  
  })
  
  test_that("compareSummaryUnEqual", {
    
    
    # The object has already pre-determined structure
    
    b1 <- rCompare(titanic,titanic2, trimChars = F, keys = 'PassengerId')
    
    #Generate print output objects
    
    s0 <- summary(b1)
    
    
    #Test output is as expected:
    
    expect_is(s0, "summary.dataCompareRobject")
    expect_output(str(s0), "List of 33")
    expect_equal(s0$ncolCommon, 13)
    expect_equal(s0$ncolInAOnly, 0)
    expect_equal(s0$ncolInBOnly, 0)
    
    expect_is(s0$typeMismatch, "data.frame")
    expect_output(str(s0$typeMismatch),"0 obs")
    expect_output(str(s0$typeMismatch), "3 variables")
    
    expect_equal(s0$typeMismatchN, 0)
    expect_equal(s0$nrowCommon, 891)
    expect_equal(s0$nrowInAOnly, 0)
    expect_equal(s0$nrowInBOnly, 0)
    expect_equal(s0$ncolsAllEqual, 9)
    expect_equal(s0$ncolsSomeUnequal, 3)
    expect_output(str(s0$colsWithUnequalValues), '3 obs')
    expect_output(str(s0$colsWithUnequalValues), '6 variables')
    expect_equal(s0$nrowNAmismatch, 0)
    expect_equal(length(s0$colsInAOnly),0)
    expect_equal(length(s0$colsInBOnly),0)
    expect_equal(length(s0$colsInBoth),13)
    
    # Test 2 - some rows do match
    titanic3 <- titanic2
    titanic3[2,2] <- 2
    titanic3[10,1] <- 999
    
    compareObject2 <- rCompare(titanic2,titanic3,keys = 'PassengerId')
    s1 <- summary(compareObject2)
    
    expect_is(s1, "summary.dataCompareRobject")
    expect_output(str(s1), "List of 33")
    expect_equal(s1$ncolCommon, 13)
    expect_equal(s1$ncolInAOnly, 0)
    expect_equal(s1$ncolInBOnly, 0)
    expect_equal(length(s1$typeMismatch), 3)
    expect_equal(s1$typeMismatchN, 1)
    expect_equal(s1$nrowInAOnly, 1)
    expect_equal(s1$nrowInBOnly, 1)
    expect_equal(s1$ncolsAllEqual, 11)
    expect_equal(nrow(s1$colsWithUnequalValues), 1)
    expect_equal(s1$nrowNAmismatch, 0)
    expect_equal(length(s1$colsInAOnly),0)
    expect_equal(length(s1$colsInBOnly),0)
    expect_equal(length(s1$colsInBoth),13)
    
    # Test 2 - some rows do match
    titanic3 <- titanic2
    titanic3[2,2] <- 2
    titanic3[10,1] <- 999
    titanic3$NEWCOL <- 1
    
    compareObject2 <- rCompare(titanic2,titanic3,keys = 'PassengerId')
    s1 <- summary(compareObject2)
    
    expect_is(s1, "summary.dataCompareRobject")
    expect_output(str(s1), "List of 33")
    expect_equal(s1$ncolCommon, 13)
    expect_equal(s1$ncolInAOnly, 0)
    expect_equal(s1$ncolInBOnly, 1)
    expect_equal(length(s1$typeMismatch), 3)
    expect_equal(s1$typeMismatchN, 1)
    expect_equal(s1$nrowInAOnly, 1)
    expect_equal(s1$nrowInBOnly, 1)
    expect_equal(s1$ncolsAllEqual, 11)
    expect_equal(nrow(s1$colsWithUnequalValues), 1)
    expect_equal(s1$nrowNAmismatch, 0)
    expect_equal(length(s1$colsInAOnly),0)
    expect_equal(length(s1$colsInBOnly),1)
    expect_equal(length(s1$colsInBoth),13)
    
  })
  
  test_that("summary errors if incorrect args passed", {
    
    # The object has already pre-determined structure
    b1 <- rCompare(titanic,titanic2, trimChars = F, keys = 'PassengerId')
    
    #Generate print output objects
    
    expect_error(summary(b1, mismatchCount = -1))
    expect_error(summary(b1, mismatchCount = "A"))
    
  })
  
} else {
  # Titanic package is not available, so skip test
  print('testCheckSummaryObject.R not run as Titanic data not available')
}