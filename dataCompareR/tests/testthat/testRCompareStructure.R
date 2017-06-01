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
# These tests check that the dataCompareR object has the structure we expect.
# 

# loading testing library
library(testthat)

context('check dataCompareR Structural requirements')

test_that("dataCompareR object meets requirements in row matching", {
  
  
  # To create the output, we make some very specific assumptions about the structure of the dataCompareR object
  # and how rows in the row matching section are labelled. This is fine, but its not very intuitive
  # To guard against unintended changes, this tests checks that the rowmatching section is working as 
  # expected for all 3 cases of no key, 1 key and > 1 key
  
  
  # Create datasets for no key, 1 key, >1 key matching
  
  pressure2 <- pressure[5:15,]
  aaa <- rCompare(pressure2,pressure, keys = 'temperature')
  
  iris2 <- iris[1:140,]
  bbb <- rCompare(iris,iris2)
  
  pressure3 <- pressure
  pressure4 <- pressure2
  pressure3$NEWKEY <- '1'
  pressure4$NEWKEY <- '1'
  ccc <- rCompare(pressure4,pressure3, keys = c('temperature','NEWKEY'))
  
  # 1 Key
  expect_equal(sum(sapply(aaa$rowMatching$inB,length)) / length(aaa$rowMatching$inB), 8)
  expect_equal(sum(sapply(aaa$rowMatching$inA,length)) / length(aaa$rowMatching$inA), 0)
  
  # No Key
  expect_equal(sum(sapply(bbb$rowMatching$inB,length)) / length(bbb$rowMatching$inB),0)
  expect_equal(sum(sapply(bbb$rowMatching$inA,length)) / length(bbb$rowMatching$inA), 10)
  
  # >1 Key
  expect_equal(sum(sapply(ccc$rowMatching$inB,length)) / length(ccc$rowMatching$inB),8)
  expect_equal(sum(sapply(ccc$rowMatching$inA,length)) / length(ccc$rowMatching$inA),0)
  
})

