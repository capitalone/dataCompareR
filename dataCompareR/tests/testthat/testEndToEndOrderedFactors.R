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
# SYSTEM TEST: dataCompareR : Ordered factors
#
# End-to-end system tests of the dataCompareR function that look at 
# scenarios involving data frames with different ordered factors
#

library(testthat)

source('createTwoKeyData.R')

context("OrderedFactorComparisons")

test_that("ComparisonOfOrderedFactor", {
  
  dts <- createOrderedFactor()
  
  dfTableA <- dts[[1]]
  dfTableB <- dts[[2]]
  
  ABcomparison <- rCompare(dfTableA, dfTableB, keys = c("color"))
  
  expect_that(ABcomparison$matches[1] == "COLOR2", is_true())
  
  expect_that(names(ABcomparison$mismatches)[1] == "COLOR3", is_true())
  expect_that(nrow(ABcomparison$mismatches[[1]]) == 4, is_true())
  expect_that(all(ABcomparison$mismatches$COLOR3$diffAB == ""), is_true())
  
})

