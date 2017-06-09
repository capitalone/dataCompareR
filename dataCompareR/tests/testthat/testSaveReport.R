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
# INTEGRATION TEST
#
# Test that saveReport will behave as expected. This involves writing
# to disk, so is technically an integration test.
#


library(testthat)

context('saveReport')

test_that("checks save report works", {
  
  skip_on_cran()
  
  # There's very little we can do here - just check it runs with no errors
  iris2 <- iris
  iris2 <- iris2[1:130,]
  iris2[1,1] <- 5.2
  iris2[2,1] <- 5.2
  
  aaa <- rCompare(iris,iris2)
  
  # Check no errors
  expect_error(saveReport(aaa, 'testing',reportLocation = '.', HTMLReport = T), NA)
  expect_error(saveReport(aaa, 'testing', reportLocation = '.',HTMLReport = F), NA)
  expect_error(saveReport(aaa,  'testing',reportLocation = '.', HTMLReport = T, showInViewer = F), NA)
  expect_error(saveReport(aaa,  'testing',reportLocation = '.', HTMLReport = F, showInViewer = F), NA)
  expect_error(saveReport(aaa, 'testing',reportLocation = '.', HTMLReport = T, showInViewer = T), NA)
  expect_error(saveReport(aaa, 'testing',reportLocation = '.', HTMLReport = F, showInViewer = T), NA)
   
})



test_that("checks save report argument validation works", {
  
  skip_on_cran()
  
  # There's very little we can do here - just check it runs with no errors
  iris2 <- iris
  iris2 <- iris2[1:130,]
  iris2[1,1] <- 5.2
  iris2[2,1] <- 5.2
  
  aaa <- rCompare(iris,iris2)
  
  # Check each incorrect call produces an error
  expect_error(saveReport(iris, reportName = 'random'))
  expect_error(saveReport(aaa, reportName = 'random', reportLocation = 'NONEXISTANTPATH'))
  expect_error(saveReport(aaa, reportName = 'random',HTMLReport = 'YesPlease'))
  expect_error(saveReport(aaa, reportName = 'random',showInViewer = 'YesPlease'))
  expect_error(saveReport(aaa, reportName = 'random',stylesheet = 'Please fetch it from my favourite website'))
  
})