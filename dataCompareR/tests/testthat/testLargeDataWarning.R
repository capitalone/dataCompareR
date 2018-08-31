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
# UNIT TEST*: checks large data warning

context("checks large data warning in rCompare")

test_that("Silent for small data", {
  
  expect_silent(warnLargeData(nrow(iris),ncol(iris),nrow(iris),ncol(iris)))
  expect_silent(warnLargeData(nrow(iris),ncol(iris),nrow(pressure),ncol(pressure)))
  expect_silent(warnLargeData(nrow(pressure),ncol(pressure), nrow(pressure),ncol(pressure)))
  
  # Large (~E6) test case
  expect_silent(warnLargeData(2E2,1E3,1E3,5E3))
  
})
  

test_that("Warns for large data", {
  
  expect_message(warnLargeData(1E9,1E9,1E9,1E9))
  expect_message(warnLargeData(5E6,5E6,5E6,6E6))
  
})



