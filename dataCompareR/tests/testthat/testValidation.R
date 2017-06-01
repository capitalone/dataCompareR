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
# INTEGRATION TESTS
#
# These tests check that providing dataCompareR with invalid arguments,
# such as empty data frames or missing keys, results in errors.
#
# They are integration tests as they use the user-facing dataCompareR function
# call to test these errors rather than calls to the validation functions
# directly.
# 

library(testthat)

context('validateData')

test_that("checks for dataCompareR_merged_indices column", {
 
  iris2 <- iris
  
  # Adding an dataCompareR_merged_indices column
  iris2$dataCompareR_merged_indices <- 1

  expect_error(rCompare(iris,iris2))
   
})

test_that("checks for keys works", {
  
  iris2 <- iris
  iris3 <- iris
  
  # Passing a key that doesn't exist
  expect_error(rCompare(iris2, iris3, keys = 'ANEWKEY'))
  
  # Key only exists in one
  iris2$ANEWKEY <- 1
  expect_error(rCompare(iris2, iris3, keys = 'ANEWKEY'))
  
  iris2$ANEWKEY <- NULL
  iris3$ANEWKEY <- 1
  
  expect_error(rCompare(iris2, iris3, keys = 'ANEWKEY'))
  
  
})
  

test_that("checks for empty works", {
  
  iris2 <- iris
  iris3 <- iris
  
  # NA input
  expect_error(rCompare(iris, NA))
  expect_error(rCompare(NA, iris2))
  
  iris3 <- NA
  
  expect_error(rCompare(iris2, iris3))
  expect_error(rCompare(iris3, iris2))
  
  # Empty data frame
  iris3 <- data.frame()
  
  expect_error(rCompare(iris2, iris3))
  expect_error(rCompare(iris3, iris2))
  
  
})