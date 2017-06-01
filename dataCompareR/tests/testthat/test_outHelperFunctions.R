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
# UNIT TESTS: Output Helper Functions
#
# Unit tests that look at the various helper functions in the output
# such as checking that something is not null and generating section headers.
#


library(testthat)

context('out_helperFunctions.R')


test_that("isNotNull", {

  expect_that(isNotNull(NULL), is_false())
  expect_that(isNotNull(1), is_true())
  
})


test_that("outputSectionHeader", {

  # Little to do here - just check the header is what we expect  
  expect_equal(outputSectionHeader("Foo") , "\nFoo\n===\n")
  

})