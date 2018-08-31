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
# UNIT TEST: checkEmpty
#
# checkEmpty checks whether or not the input data frame is empty 
# and stops with an error if it is
# Note that by empty, we mean no columns. Data frames with no columns
# fail, but data frames with columns but no rows pass

# loading testing library
library(testthat)

context('checkEmpty')

# Testing that Empty DFs give errors
test_that("Empty DFs give errors", {
  
  # create empty dataframe
  emptydf <- data.frame(Car = character(),
                        Date = as.Date(character()),
                        Model = character(),
                        stringsAsFactors = FALSE)
  
  veryEmptydf <- data.frame()
  
  # create populated dataframe
  fulldf <- iris
  
  # no error with cols
  expect_silent( checkEmpty(emptydf))
  
  # get no error with no empty df
  expect_silent(checkEmpty(fulldf))
  
  # get error with empty df
  expect_error(checkEmpty(veryEmptydf))
  
  })
