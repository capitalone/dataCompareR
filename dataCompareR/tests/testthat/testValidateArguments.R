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
# These tests check that the calls to the validateArguments function
# behaves appropriately given certain kinds of input.
#
# Because there are underlying functions to validateArguments, these
# are integration tests.
#

context('validateArguments')

test_that("validateArguments executes silently for valid input arguments", {
  
  # No arguments
  
  expect_silent(validateArguments())
  
  # Simple scalars
  
  key <- "real_name"
  roundTo <- NA
  coerceFlag <- F
  maxMismatch <- 10
  
  expect_silent(validateArguments(key, roundTo, coerceFlag, maxMismatch))
  
  # Combined key
  
  key <- c("real_name", "date")
  roundTo <- 5
  coerceFlag <- F
  maxMismatch <- 10
  
  expect_silent(validateArguments(key, roundTo, coerceFlag, maxMismatch))
  
  # Negative threshold is fine - round handles this
  
  key <- "real_name"
  threshold <- -0.001
  coerceFlag <- F
  maxMismatch <- 10
  
  expect_silent(validateArguments(key, threshold, coerceFlag, maxMismatch))
})

test_that("validateArguments errors correctly for invalid input arguments", {
  
  # Numeric key
  
  key <- 0.7
  threshold <- 0.001
  coerceFlag <- F
  maxMismatch <- 10
  
  expect_error(validateArguments(key, threshold, coerceFlag, maxMismatch), "ERROR.*")

  # String threshold
  
  key <- "real_name"
  threshold <- "really close"
  coerceFlag <- F
  maxMismatch <- 10
  
  expect_error(validateArguments(key, threshold, coerceFlag, maxMismatch), "ERROR.*")
  
  # String coerceFlag
  
  key <- "real_name"
  threshold <- 0.001
  coerceFlag <- "FALSE"
  maxMismatch <- 10
  
  expect_error(validateArguments(key, threshold, coerceFlag, maxMismatch), "ERROR.*")
  
  # String maxMismatch  

  key <- "real_name"
  threshold <- 0.001
  coerceFlag <- F
  maxMismatch <- "ten"
  
  expect_error(validateArguments(key, threshold, coerceFlag, maxMismatch), "ERROR.*")
  
  # Multiple wrong argument types - first one should flag

  key <- 0.7
  threshold <- "really close"
  coerceFlag <- "false"
  maxMismatch <- "ten"
  
  expect_error(validateArguments(key, threshold, coerceFlag, maxMismatch), "ERROR.*")
  
  # Key is aggregate - so first one could be right, second one wrong
  
  key <- list("real_name", 0.7)
  threshold <- 0.001
  coerceFlag <- F
  maxMismatch <- 10
  
  expect_error(validateArguments(key, threshold, coerceFlag, maxMismatch), "ERROR.*")
  
  
  
  # Non-integer maxMismatch
  
  key <- "real_name"
  threshold <- 0.001
  coerceFlag <- F
  maxMismatch <- 10.2
  
  expect_error(validateArguments(key, threshold, coerceFlag, maxMismatch), "ERROR.*")
  
  # MaxMismatch > 0
  
  key <- "real_name"
  threshold <- 0.001
  coerceFlag <- F
  maxMismatch <- 0
  
  expect_error(validateArguments(key, threshold, coerceFlag, maxMismatch), "ERROR.*")
})



