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
# UNIT TEST: checkUniqueness
#
# checkUniqueness returns a boolean indicating whether or not the
# keys provided are unique. 
# 

context("checkUniqueness")

test_that("checkUniqueness correctly detects whether keys are unique or not", {
  
  # Keys are unique in this case
  
  ky <- seq(1,99)

  expect_silent(checkUniqueness(ky))     # Keys are unique, so say nothing
  expect_equal(checkUniqueness(ky), T)   # Keys are unique, so expect a "TRUE"
  
  # Keys are not in this case
  
  ky <- c(seq(1,50), seq(1,50))

  expect_silent(checkUniqueness(ky))     # Keys are not unique, still say nothing
  expect_equal(checkUniqueness(ky), F)   # Keys are unique, so expect a "FALSE"
})