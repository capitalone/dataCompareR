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
# UNIT TEST: createReportText
#
# * Assumes that the output of the dataCompareR function is accurate
#
# createReportText should have a certain structure depending on the
# contents of the dataCompareR object
#

library(testthat)

context('test createReportText')

test_that("check a non comprehensive set of properties about createReportText", {

  # Create a couple of R compare objects
  testSame <- rCompare(iris,iris)

  iris2 <- iris[1:140,]
  testDiff <- rCompare(iris,iris2)

  # Capture the outpts of createReportText as text
  textSame <- capture.output(createReportText(summary.dataCompareRobject(testSame)))
  textDiff <- capture.output(createReportText(summary.dataCompareRobject(testDiff)))

  # For now we won't hard code each - instead, we will just check a few points...

  # We should look for the names of the data
  expect_true(any(grepl("iris",textSame)))
  expect_true(any(grepl("iris",textDiff)))
  expect_false(any(grepl("iris2",textSame)))
  expect_true(any(grepl("iris2",textDiff)))

  # Check that the column equal report is working
  expect_true(any(grepl("Columns with all rows equal : PETAL.LENGTH, PETAL.WIDTH, SEPAL.LENGTH, SEPAL.WIDTH, SPECIES",textSame)))
  expect_true(any(grepl("Columns with all rows equal : PETAL.LENGTH, PETAL.WIDTH, SEPAL.LENGTH, SEPAL.WIDTH, SPECIES",textDiff)))

  # Expect the diff report to contain 140 (10 rows are missing) and 10
  expect_true(any(grepl("140",textDiff)))
  expect_true(any(grepl("10",textDiff)))

  # And both contain 150
  expect_true(any(grepl("150",textSame)))
  expect_true(any(grepl("150",textDiff)))

  # Both contain some data, say more than 40 lines
  expect_true(length(textSame) > 40)
  expect_true(length(textDiff) > 40)

  # Expect a bunch of words will always be there
  expect_true(any(grepl("columns",textDiff)))
  expect_true(any(grepl("columns",textSame)))

  expect_true(any(grepl("rows",textDiff)))
  expect_true(any(grepl("rows",textSame)))

  expect_true(any(grepl("Variable",textDiff)))
  expect_true(any(grepl("Variable",textSame)))

  expect_true(any(grepl("equal",textDiff)))
  expect_true(any(grepl("equal",textSame)))

  expect_true(any(grepl("unequal",textDiff)))
  expect_true(any(grepl("unequal",textSame)))

  # Expect they differ
  expect_false(all(textDiff==textSame))

  # Expect that both contain the line "No match key used, comparison is by row"
  expect_true(any(grepl("No match key used, comparison is by row",textDiff)))
  expect_true(any(grepl("No match key used, comparison is by row",textSame)))

})


test_that("check a key based match with 1 matching keys", {

  pressure2 <- pressure
  pressure2$temperature <- pressure2$temperature + 1

  testNoKeys <- rCompare(pressure,pressure2)
  testKeys <- rCompare(pressure,pressure2, keys = 'temperature')

  # Capture the outpts of createReportText as text
  textNoKeys <- capture.output(createReportText(summary.dataCompareRobject(testNoKeys)))
  textKeys<- capture.output(createReportText(summary.dataCompareRobject(testKeys)))


  expect_true(length(textKeys) != length(textNoKeys))
  expect_true(any(grepl("No rows were compared, so no summary can be provided",textKeys)))

  # Expect that the match keys are present in the output
  expect_true(any(grepl("No match key used, comparison is by row",textNoKeys)))
  expect_true(any(grepl("Match keys : 1   - TEMPERATURE",textKeys, fixed = TRUE)))

})

# This is by its nature pretty hardcoded - not sure what else to do?
test_that("check that the max difference works with negative values", {

  pressure2 <- pressure
  pressure2[2,2] <- 10
  pressure2[3,2] <- -110

  testLargeNegativeDelta <- rCompare(pressure,pressure2)
  testLargeNegativeDeltaInv <- rCompare(pressure2,pressure)

  textTestLargeNegativeDelta <- capture.output(summary(testLargeNegativeDelta))
  testTestLargeNegativeDeltaInv <- capture.output(summary(testLargeNegativeDeltaInv))

  # The max mismatch row should be identical, even though they're the opposite way around
  expect_equal(gsub(" ", "", testTestLargeNegativeDeltaInv[57]),gsub(" ", "", textTestLargeNegativeDelta[57]))



})

# This is by its nature pretty hardcoded - not sure what else to do?
test_that("checkthe column headers in tables contain the right data", {

  pressure2 <- pressure
  pressure2[2,2] <- 10
  pressure2[3,2] <- -110

  testLargeNegativeDelta <- rCompare(pressure,pressure2)
  testLargeNegativeDeltaInv <- rCompare(pressure2,pressure)

  textTestLargeNegativeDelta <- capture.output(summary(testLargeNegativeDelta))
  testTestLargeNegativeDeltaInv <- capture.output(summary(testLargeNegativeDeltaInv))

  # The max mismatch row should be identical, even though they're the opposite way around
  expect_equal(testTestLargeNegativeDeltaInv[69],"|   | PRESSURE (pressure2)| PRESSURE (pressure)|Type (pressure2) |Type (pressure) | Difference|")
  expect_equal(testTestLargeNegativeDeltaInv[54],"|Column   |Type (in pressure2) |Type (in pressure) | # differences|Max difference | # NAs|")


})

test_that("test that keys vs no keys has the correct output in the mismatch table", {

  # Expect keys to have no row numbers, no keys to have row numbers
  pressure2 <- pressure
  pressure2[2,2] <- 10
  pressure2[3,2] <- -110

  testNoKey <- rCompare(pressure,pressure2)
  testKey <- rCompare(pressure2,pressure, key = 'temperature')

  texttestNoKey <- capture.output(summary(testNoKey))
  testKey <- capture.output(summary(testKey))

  # The max mismatch row should be identical, even though they're the opposite way around
  expect_equal(texttestNoKey[69],"|   | PRESSURE (pressure)| PRESSURE (pressure2)|Type (pressure) |Type (pressure2) | Difference|")
  expect_equal(texttestNoKey[70],"|:--|-------------------:|--------------------:|:---------------|:----------------|----------:|")
  expect_equal(testKey[69],"| TEMPERATURE| PRESSURE (pressure2)| PRESSURE (pressure)|Type (pressure2) |Type (pressure) | Difference|")
  expect_equal(testKey[70],"|-----------:|--------------------:|-------------------:|:----------------|:---------------|----------:|")



})
