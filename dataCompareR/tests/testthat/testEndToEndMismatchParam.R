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
# SYSTEM TEST: dataCompareR : Mismatch Argument
#
# End-to-end system tests of the dataCompareR function that look at 
# different scenarios that involve passing in a value for the
# mismatch parameter, which stops the run when a certain number
# of mismatches have been discovered.
#

# loading testing library
library(testthat)

context('Test mismatches parameters stops run')


# Testing that the mismatch parameter works
test_that("Mismatch stops when exceeded", {
  
  # Create a copy of iris with 20 mismatches
  iris2 <- iris
  iris2[1:10,1] <- iris2[1:10,1] + 1
  iris2[1:10,2] <- iris2[1:10,2] - 1
  
  # In this case, expect error (stop) when mismatches < 20
  expect_error(rCompare(iris,iris2,mismatches = 1))
  expect_error(rCompare(iris,iris2,mismatches = 19))
  
  # But not for 20 plus
  expect_error(rCompare(iris,iris2,mismatches = 20),NA)
  expect_error(rCompare(iris,iris2,mismatches = 21),NA)
  
})

