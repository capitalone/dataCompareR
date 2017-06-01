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
# UNIT TEST: rounddf
#
# rounddf should round numeric fields within a data frame 
# to a specified number of digits
#
library(testthat)

context("Check that rounddf function is working")

test_that("rounddf", {
  
  # Create some data
  expect_silent(irisRounded <- rounddf(iris,0))
  expect_silent(pressureRounded <- rounddf(pressure,1))
  expect_silent(pressureRounded2 <- rounddf(pressure,-2))
  
  
  # Check a few randomly
  expect_true(irisRounded[2,2] == 3)
  expect_true(irisRounded[29,2] == 3)
  expect_true(irisRounded[29,3] == 1)
  expect_true(irisRounded[29,4] == 0)
  expect_true(irisRounded[29,5] == "setosa")
  
  expect_true(pressureRounded[1,2] == 0)
  expect_true(pressureRounded[5,1] == 80)
  expect_true(pressureRounded[5,2] == 0.1)
  expect_true(pressureRounded[10,2] == 8.8)
  expect_true(pressureRounded[15,2] == 157)
  
  expect_true(pressureRounded2[1,2] == 0)
  expect_true(pressureRounded2[5,1] == 100)
  expect_true(pressureRounded2[5,2] == 0)
  expect_true(pressureRounded2[10,2] == 0)
  expect_true(pressureRounded2[15,2] == 200)
  
})
