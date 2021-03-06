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
# UNIT TEST: rcompObjItemLength
#
# rcompObjItemLength takes in an object and returns the length, switching betwee
# len() and nrows() as needed and returning 0 in place of NA

library(testthat)

context("checkrcompObjItemLength")

test_that("rcompObjItemLength works",{
  expect_equal(rcompObjItemLength(iris), 150)
  expect_equal(rcompObjItemLength(c(1,2,3)), 3)
  expect_equal(rcompObjItemLength(NA), 1)
  expect_equal(rcompObjItemLength(NULL), 0)
  
  })