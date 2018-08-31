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
  
  tmpDir <- tempdir()
  
  # Test some basic arguments
  expect_message(saveReport(compareObject = aaa, reportName = 'testing',reportLocation = tmpDir, HTMLReport = TRUE), regexp = "Using default stylesheet")
  expect_message(saveReport(aaa, 'testing2',reportLocation = tmpDir, HTMLReport = FALSE), regexp = "Using default stylesheet")
  expect_message(saveReport(aaa, 'testing3',reportLocation = tmpDir, HTMLReport = FALSE, showInViewer = FALSE), regexp = "Using default stylesheet")
  
  # Check the files exist
  expect_true(file.exists(file.path(tmpDir, "testing.Rmd")))
  expect_true(file.exists(file.path(tmpDir, "testing.md")))
  expect_true(file.exists(file.path(tmpDir, "testing.html")))
  expect_true(file.exists(file.path(tmpDir, "testing2.Rmd")))
  expect_true(file.exists(file.path(tmpDir, "testing3.Rmd")))
  expect_true(!file.exists(file.path(tmpDir, "testing2.md")))
  expect_true(!file.exists(file.path(tmpDir, "testing3.md")))
  expect_true(!file.exists(file.path(tmpDir, "testing2.html")))
  expect_true(!file.exists(file.path(tmpDir, "testing3.html")))
  
  # Check no errors
  expect_error(saveReport(aaa, 'testing',reportLocation = '.', HTMLReport = TRUE), NA)
  expect_error(saveReport(aaa, 'testing', reportLocation = '.',HTMLReport = FALSE), NA)
  expect_error(saveReport(aaa,  'testing',reportLocation = '.', HTMLReport = TRUE, showInViewer = FALSE), NA)
  expect_error(saveReport(aaa,  'testing',reportLocation = '.', HTMLReport = FALSE, showInViewer = FALSE), NA)
  expect_error(saveReport(aaa, 'testing',reportLocation = '.', HTMLReport = TRUE, showInViewer = TRUE), NA)
  expect_error(saveReport(aaa, 'testing',reportLocation = '.', HTMLReport = FALSE, showInViewer = TRUE), NA)
   
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
  expect_error(saveReport(aaa, reportName = 'random',printAll = iris))
  
})



test_that("checks save report passing extra arguments works", {
  
  skip_on_cran()
  
  # There's very little we can do here - just check it runs with no errors
  iris2 <- iris
  iris2 <- iris2[1:130,]
  iris2$Sepal.Length <- iris2$Sepal.Length + 1
  
  
  aaa <- rCompare(iris,iris2)
  
  tmpDir <- tempdir()
  
  # Test some basic arguments
  expect_message(saveReport(aaa, 'testing',reportLocation = tmpDir, HTMLReport = TRUE), regexp = "Using default stylesheet")
  
  expect_message(saveReport(aaa, 'testing-big',reportLocation = tmpDir, 
                            HTMLReport = TRUE, mismatchCount = 10), 
                 regexp = "Using default stylesheet")
  
  expect_message(saveReport(aaa, 'testing-all',reportLocation = tmpDir, 
                            HTMLReport = TRUE, printAll = TRUE),
                 regexp = "Using default stylesheet")
                 
  expect_true(file.exists(file.path(tmpDir, "testing.Rmd")))
  expect_true(file.exists(file.path(tmpDir, "testing.md")))
  expect_true(file.exists(file.path(tmpDir, "testing.html")))
  
  expect_true(file.exists(file.path(tmpDir, "testing-big.Rmd")))
  expect_true(file.exists(file.path(tmpDir, "testing-big.md")))
  expect_true(file.exists(file.path(tmpDir, "testing-big.html")))
  
  expect_true(file.exists(file.path(tmpDir, "testing-all.Rmd")))
  expect_true(file.exists(file.path(tmpDir, "testing-all.md")))
  expect_true(file.exists(file.path(tmpDir, "testing-all.html")))
  
  # Read the files so we can check they're correct
  file1 <- readLines(file.path(tmpDir, "testing.md"))
  file2 <- readLines(file.path(tmpDir, "testing-big.md"))
  file3 <- readLines(file.path(tmpDir, "testing-all.md"))
  
  expect_equal(file1[66],"Showing sample of size 5")
  expect_equal(file2[66],"Showing sample of size 10")
  expect_equal(file3[66],"Showing sample of size 130")
  
  # check lengths differ by 5 betwee 1 and 2 
  expect_equal(length(file1)+5, length(file2))
  # and 125 from 1 to 3
  expect_equal(length(file1)+125, length(file3))
  
})