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
# UNIT TEST: checkKeysExist
#
# checkKeysExist takes in a data frame and a vector of one or more keys
# and then checks if the keys exist in the data frame, stopping with an error
# if it does not
#

library(testthat)

context("checkKeysExists")

test_that("checkKeysExist_Works",{

  # Data frame A
  id <- c('1','2','3','4','5','6','7','8','9','10')
  col1 <- c('1','2','3','4','5','6','7','8','9','10')
  col2 <- c('a','b','c','d','e','f','g','h','i','j')
  col3 <- c('1','2c','d','xc','asa','21','36','7','11','1')
  COL4 <- c('1','2c','d','xc','asa','21','36','7','11','1')
  DFA <- data.frame(id,col1,col2,col3,COL4)
  
  # Data frame B
  id <- c('1','2','3','4','5')
  col2 <- c('a','b','c','d','e')
  col3 <- c('1','2c','d','xc','asa')
  col4 <- c('1','2c','d','xc','asa','21','36','7','11','1')
  DFB <- data.frame(id,col2,col3,col4)
  
  # Returns nothing if they exist
  expect_null(checkKeysExist(DFA,c('id')))
  expect_null(checkKeysExist(DFB,c('id')))
  
  expect_null(checkKeysExist(DFA,c('id','col2')))
  expect_null(checkKeysExist(DFB,c('id','col2')))
  
  # Generates an error if they don't exist
  expect_error(checkKeysExist(DFA,c('id','coL4')))
  expect_error(checkKeysExist(DFB,c('id','CoL4')))
  
  expect_error(checkKeysExist(DFA,c('id2')))
  expect_error(checkKeysExist(DFB,c('id2')))
  
  expect_error(checkKeysExist(DFA,c('id2')))
  expect_error(checkKeysExist(DFB,c('id2')))
  
  expect_error(checkKeysExist(DFB,c('id','col1')))
  
  expect_error(checkKeysExist(DFA,c('')))
  expect_error(checkKeysExist(DFB,c('')))
  
  expect_error(checkKeysExist(DFA,c('id','')),)
  expect_error(checkKeysExist(DFB,c('id','')),)
  
  
})