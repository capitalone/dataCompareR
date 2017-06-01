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
# SYSTEM TEST: dataCompareR : Four Keys
#
# End-to-end system tests of the dataCompareR function that look at 
# different scenarios involving data frames with four keys
#

library(testthat)

source('createTwoKeyData.R')

context("Four Match Key Comparisons")

test_that("ComparisonOfEquals", {
  
  # Create and compare two matching datasets
  dts <- createMatchingFourIndiceData(5)
  
  dfTableA <- dts[[1]]
  dfTableB <- dts[[2]]
  
  ABcomparison <- rCompare(dfTableA, dfTableB, keys = c("color", "number", "color2", "number2"))
  
  # Expect col summary to match the following
  expect_equal(length(ABcomparison$colMatching$inboth),5)
  expect_equal(ABcomparison$colMatching$inboth, c("COLOR" ,"COLOR2", "NUMBER", "NUMBER2", "VALUEA"))
  expect_equal(ABcomparison$colMatching$inA, character(0))
  expect_equal(ABcomparison$colMatching$inB, character(0))
  
  # Expect row summary to match the following
  expect_equal(length(ABcomparison$rowMatching),4)
  expect_equal(dim(ABcomparison$rowMatching$inboth), c(5,4))
  expect_equal(ABcomparison$rowMatching$inA$COLOR, logical(0))
  expect_equal(ABcomparison$rowMatching$inA$NUMBER, logical(0))
  expect_equal(ABcomparison$rowMatching$inB$COLOR, logical(0))
  expect_equal(ABcomparison$rowMatching$inB$NUMBER, logical(0))
  expect_equal(ABcomparison$rowMatching$inA$COLOR2, logical(0))
  expect_equal(ABcomparison$rowMatching$inA$NUMBER2, logical(0))
  expect_equal(ABcomparison$rowMatching$inB$COLOR2, logical(0))
  expect_equal(ABcomparison$rowMatching$inB$NUMBER2, logical(0))
  expect_equal(ABcomparison$rowMatching$matchKeys, toupper(c("color", "number", "color2", "number2")))
  
  # Matches should just be 1 field
  expect_that(ABcomparison$matches == "VALUEA", is_true())
  
  # Mismatches should be empty
  expect_equal(length(ABcomparison$mismatches), 0)
  
  # Cleaning - expect char to factor
  expect_equal(length(ABcomparison$cleaninginfo$COLOR),4)
  
})

test_that("ComparisonOfUnEquals", {
  
  # Create and compare two matching datasets
  dts <- createMatchingFourIndiceData(5)
  
  dfTableA <- dts[[1]]
  dfTableB <- dts[[2]]
  
  dfTableB[3,5] <- dfTableB[3,5] - 0.1
  
  ABcomparison <- rCompare(dfTableA, dfTableB, keys = c("color", "number", "color2", "number2"))
  
  # Expect col summary to match the following
  expect_equal(length(ABcomparison$colMatching$inboth),5)
  expect_equal(ABcomparison$colMatching$inboth, c("COLOR" ,"COLOR2", "NUMBER", "NUMBER2", "VALUEA"))
  expect_equal(ABcomparison$colMatching$inA, character(0))
  expect_equal(ABcomparison$colMatching$inB, character(0))
  
  # Expect row summary to match the following
  expect_equal(length(ABcomparison$rowMatching),4)
  expect_equal(dim(ABcomparison$rowMatching$inboth), c(5,4))
  expect_equal(ABcomparison$rowMatching$inA$COLOR, logical(0))
  expect_equal(ABcomparison$rowMatching$inA$NUMBER, logical(0))
  expect_equal(ABcomparison$rowMatching$inB$COLOR, logical(0))
  expect_equal(ABcomparison$rowMatching$inB$NUMBER, logical(0))
  expect_equal(ABcomparison$rowMatching$inA$COLOR2, logical(0))
  expect_equal(ABcomparison$rowMatching$inA$NUMBER2, logical(0))
  expect_equal(ABcomparison$rowMatching$inB$COLOR2, logical(0))
  expect_equal(ABcomparison$rowMatching$inB$NUMBER2, logical(0))
  expect_equal(ABcomparison$rowMatching$matchKeys, toupper(c("color", "number", "color2", "number2")))
  
  # Matches should just be No fields
  expect_equal(length(ABcomparison$matches), 0)
  
  # Mismatches should have 1 entry
  expect_equal(length(ABcomparison$mismatches), 1)
  expect_equal(names(ABcomparison$mismatches), "VALUEA")
  expect_equal(ABcomparison$mismatches$VALUEA$diffAB, 0.1)
  
  # Cleaning - expect char to factor
  expect_equal(length(ABcomparison$cleaninginfo$COLOR),4)
  
})

test_that("ComparisonOfMissRows", {
  
  # Create and compare two matching datasets
  dts <- createMatchingFourIndiceData(5)
  
  dfTableA <- dts[[1]]
  dfTableB <- dts[[2]][1:4,]
  
  ABcomparison <- rCompare(dfTableA, dfTableB, keys = c("color", "number", "color2", "number2"))
  
  # Expect col summary to match the following
  expect_equal(length(ABcomparison$colMatching$inboth),5)
  expect_equal(ABcomparison$colMatching$inboth, c("COLOR" ,"COLOR2", "NUMBER", "NUMBER2", "VALUEA"))
  expect_equal(ABcomparison$colMatching$inA, character(0))
  expect_equal(ABcomparison$colMatching$inB, character(0))
  
  # Expect row summary to match the following
  expect_equal(length(ABcomparison$rowMatching),4)
  expect_equal(dim(ABcomparison$rowMatching$inboth), c(4,4))
  expect_equal(length(ABcomparison$rowMatching$inA$COLOR), 1)
  expect_equal(length(ABcomparison$rowMatching$inA$NUMBER), 1)
  expect_equal(ABcomparison$rowMatching$inB$COLOR, logical(0))
  expect_equal(ABcomparison$rowMatching$inB$NUMBER, logical(0))
  expect_equal(length(ABcomparison$rowMatching$inA$COLOR2), 1)
  expect_equal(length(ABcomparison$rowMatching$inA$NUMBER2), 1)
  expect_equal(ABcomparison$rowMatching$inB$COLOR2, logical(0))
  expect_equal(ABcomparison$rowMatching$inB$NUMBER2, logical(0))
  expect_equal(ABcomparison$rowMatching$matchKeys, toupper(c("color", "number", "color2", "number2")))
  
  # Matches should just be 1 field
  expect_that(ABcomparison$matches == "VALUEA", is_true())
  
  # Mismatches should be empty
  expect_equal(length(ABcomparison$mismatches), 0)
  
  # Cleaning - expect char to factor
  expect_equal(length(ABcomparison$cleaninginfo$COLOR),4)
  
})

test_that("ComparisonOfMissCols", {
  
  # Create and compare two matching datasets
  dts <- createMatchingFourIndiceData(5)
  
  dfTableA <- dts[[1]]
  dfTableB <- dts[[2]]
  dfTableB$NEWCOL <- seq(1:5)
  
  ABcomparison <- rCompare(dfTableA, dfTableB, keys = c("color", "number", "color2", "number2"))
  
  # Expect col summary to match the following
  expect_equal(length(ABcomparison$colMatching$inboth),5)
  expect_equal(ABcomparison$colMatching$inboth, c("COLOR" ,"COLOR2", "NUMBER", "NUMBER2", "VALUEA"))
  expect_equal(ABcomparison$colMatching$inA, character(0))
  expect_equal(ABcomparison$colMatching$inB, "NEWCOL")
  
  # Expect row summary to match the following
  expect_equal(length(ABcomparison$rowMatching),4)
  expect_equal(dim(ABcomparison$rowMatching$inboth), c(5,4))
  expect_equal(ABcomparison$rowMatching$inA$COLOR, logical(0))
  expect_equal(ABcomparison$rowMatching$inA$NUMBER, logical(0))
  expect_equal(ABcomparison$rowMatching$inB$COLOR, logical(0))
  expect_equal(ABcomparison$rowMatching$inB$NUMBER, logical(0))
  expect_equal(ABcomparison$rowMatching$inA$COLOR2, logical(0))
  expect_equal(ABcomparison$rowMatching$inA$NUMBER2, logical(0))
  expect_equal(ABcomparison$rowMatching$inB$COLOR2, logical(0))
  expect_equal(ABcomparison$rowMatching$inB$NUMBER2, logical(0))
  expect_equal(ABcomparison$rowMatching$matchKeys, toupper(c("color", "number", "color2", "number2")))
  
  # Matches should just be 1 field
  expect_that(ABcomparison$matches == "VALUEA", is_true())
  
  # Mismatches should be empty
  expect_equal(length(ABcomparison$mismatches), 0)
  
  # Cleaning - expect char to factor
  expect_equal(length(ABcomparison$cleaninginfo$COLOR),4)
  
})

