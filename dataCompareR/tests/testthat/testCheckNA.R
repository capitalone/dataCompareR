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

library(testthat)

context('checkNA and isSingleNA')

test_that("checks checkNA works as intended", {
 
  # This function should return TRUE If, and only If, a single NA is passed 
  # It appears to catch NaN and a list of 1 Na. I can live with this!

  expect_error(checkNA(NA))
  expect_error(checkNA(NaN))
  expect_error(checkNA(list(a = NA)))
  
  # No other errors should ever be raised
  expect_silent(checkNA(c(NA,NA)))
  expect_silent(checkNA(c(1,NA)))
  expect_silent(checkNA(c("A",NA)))
  expect_silent(checkNA(iris))
  expect_silent(checkNA(NULL))
  expect_silent(checkNA(Inf))
  
  

})

test_that("checks checkNA works as intended", {
  
  # This function should return TRUE If, and only If, a single NA is passed 
  # It appears to catch NaN too- I can live with this!
  
  expect_true(isSingleNA(NA))
  expect_true(isSingleNA(NaN))
  expect_true(isSingleNA(list(a = NA)))
  
  # No other errors should ever be raised
  expect_false(isSingleNA(c(NA,NA)))
  expect_false(isSingleNA(c(1,NA)))
  expect_false(isSingleNA(c("A",NA)))
  expect_false(isSingleNA(iris))
  expect_false(isSingleNA(NULL))
  expect_false(isSingleNA(Inf))
  
})



