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
# UNIT TEST: print.summary.dataCompareRobject
#
# * Assumes that the output of dataCompareR is accurate
#
# The print output of the summary object of the dataCompareR object 
# should have a certain structure depending on the content of the object.
#

library(testthat)

context('test printsummaryrcompobj')


test_that("test printsummaryrcompobj", {
  
  
  # Create a couple of R compare objects
  testSame <- rCompare(iris,iris)
  
  iris2 <- iris[1:140,]
  testDiff <- rCompare(iris,iris2)
  
  # Capture the outpts of createReportText as text
  textSame <- capture.output(print(summary(testSame)))
  textDiff <- capture.output(print(summary(testDiff)))
  
  # For now we won't hard code each - instead, we will just check a few points...
  
  # We should look for the names of the data
  expect_that(any(grepl("iris",textSame)),is_true())
  expect_that(any(grepl("iris",textDiff)),is_true())
  expect_that(any(grepl("iris2",textSame)),is_false())
  expect_that(any(grepl("iris2",textDiff)),is_true())
  
  # Check that the column equal report is working
  expect_that(any(grepl("Columns with all rows equal : PETAL.LENGTH, PETAL.WIDTH, SEPAL.LENGTH, SEPAL.WIDTH, SPECIES",textSame)),is_true())
  expect_that(any(grepl("Columns with all rows equal : PETAL.LENGTH, PETAL.WIDTH, SEPAL.LENGTH, SEPAL.WIDTH, SPECIES",textDiff)),is_true())
  
  # Expect the diff report to contain 140 (10 rows are missing) and 10
  expect_that(any(grepl("140",textDiff)),is_true())
  expect_that(any(grepl("10",textDiff)),is_true())
  
  # And both contain 150
  expect_that(any(grepl("150",textSame)),is_true())
  expect_that(any(grepl("150",textDiff)),is_true())
  
  # Both contain some data, say more than 40 lines
  expect_that(length(textSame) > 40,is_true())
  expect_that(length(textDiff) > 40,is_true())
  
  # Expect a bunch of words will always be there
  expect_that(any(grepl("columns",textDiff)),is_true())
  expect_that(any(grepl("columns",textSame)),is_true())
  
  expect_that(any(grepl("rows",textDiff)),is_true())
  expect_that(any(grepl("rows",textSame)),is_true())
  
  expect_that(any(grepl("Variable",textDiff)),is_true())
  expect_that(any(grepl("Variable",textSame)),is_true())
  
  expect_that(any(grepl("equal",textDiff)),is_true())
  expect_that(any(grepl("equal",textSame)),is_true())
  
  expect_that(any(grepl("unequal",textDiff)),is_true())
  expect_that(any(grepl("unequal",textSame)),is_true())
  
  # Expect they differ
  expect_that(all(textDiff==textSame),is_false())
  
})

test_that("rounding note appears", {
  
  
  # Create a couple of R compare objects
  
  # With rounding
  testRound <- rCompare(iris,iris, roundDigits = 0)
  
  # Capture the outputs of createReportText as text
  textRound <- capture.output(print(summary(testRound)))
  
  expect_true(any(grepl("Numeric values were rounded to 0 decimal.", textRound)))
  
  # without rounding
  testNoRound <- rCompare(iris,iris)
  # Capture the outpts of createReportText as text
  textNoRound <- capture.output(print(summary(testNoRound)))
  
  expect_false(any(grepl("Numeric values were rounded", textNoRound)))
  
  
  
  
})


test_that("complete mismatching data output is truncated", {
  
  
  # Create a couple of R compare objects
  
  # With rounding
  testVeryDifferent <- rCompare(iris,pressure)
  testSame <- rCompare(iris,iris)
  
  # Capture the outputs of createReportText as text
  textVeryDifferent <- capture.output(print(summary(testVeryDifferent)))
  textSame <- capture.output(print(summary(testSame)))
  
  expect_true(any(grepl("No columns match, so no comparison could take place", textVeryDifferent)))
  expect_false(any(grepl("No columns match, so no comparison could take place", textSame)))

  
})


test_that("row matching details appear only when matching on keys and row matching is incomplete", {
  
  
  # Create a couple of R compare objects
  
  # With rounding
  testExpectNoRowsSummary <- rCompare(iris,iris)
  testExpectNoRowsSummary2 <- rCompare(pressure,pressure,key='temperature')
  
  pressure2 <- pressure[1:10,]
  testExpectRowsSummary <- rCompare(pressure,pressure2,key='temperature')
  
  
  # Capture the outputs of createReportText as text
  texttestExpectNoRowsSummary <- capture.output(print(summary(testExpectNoRowsSummary)))
  testtestExpectNoRowsSummary2 <- capture.output(print(summary(testExpectNoRowsSummary2)))
  testtestExpectRowsSummary <- capture.output(print(summary(testExpectRowsSummary)))
  
  
  expect_false(any(grepl("The following rows were dropped from  pressure", texttestExpectNoRowsSummary)))
  expect_false(any(grepl("The following rows were dropped from  pressure", testtestExpectNoRowsSummary2)))
  expect_true(any(grepl("The following rows were dropped from  pressure", testtestExpectRowsSummary)))
  
  
  
  
})


test_that("the number of rows and columns returned are correct", {
  
  pressure2 <- pressure
  pressure2$ID2 <- 1
  pressure2$field2 <- 10
  pressure3 <- pressure2
  pressure3$pressure <- pressure3$pressure * 2
  pressure3$field2 <- 9
  
  withkeys <- capture.output(print(rCompare(pressure2, pressure2, keys = c('temperature'))) )
  withoutkeys <- capture.output(print(rCompare(pressure2, pressure2))) 
  
  expect_equal(withkeys[1], "All columns were compared, all rows were compared ")
  expect_equal(withkeys[2], "All compared variables match ")
  expect_equal(withkeys[3], " Number of rows compared: 19 ")
  expect_equal(withkeys[4], " Number of columns compared: 4")
  expect_equal(withoutkeys[1], "All columns were compared, all rows were compared ")
  expect_equal(withoutkeys[2], "All compared variables match ")
  expect_equal(withoutkeys[3], " Number of rows compared: 19 ")
  expect_equal(withoutkeys[4], " Number of columns compared: 4")
  
}) 


