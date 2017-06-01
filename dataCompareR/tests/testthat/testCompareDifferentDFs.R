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
# SYSTEM TEST: DF with no similarities
#
# End-to-end test to check how the dataCompareR function works
# with two data frames that have absolutely nothing in common
#

context('testCompareBizzarelyDifferentItems')

test_that("check two dfs with no overlap", {
  
  
  expect_message(rCompare(iris,women), "Running rCompare")
  
  a <- rCompare(iris,women)
  
  expect_true(length(a$colMatching$inboth) == 0 )
  expect_true(length(a$colMatching$inA) == 5 )
  expect_true(length(a$colMatching$inB) == 2 )
  
  expect_true(length(a$mismatches) == 0 )
  expect_true(a$matches == "" )
  
})
