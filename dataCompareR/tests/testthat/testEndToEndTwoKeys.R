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
# SYSTEM TEST: dataCompareR : Two Match Keys
#
# End-to-end system tests of the dataCompareR function that look at 
# different scenarios involving two-key data frame comparisons. 
#

library(testthat)

source('createTwoKeyData.R')

context("Two Match Key Comparisons")

test_that("ComparisonOfEquals", {

  # Create and compare two matching datasets
  dts <- createMatchoingTwoIndiceData(5)
  
  dfTableA <- dts[[1]]
  dfTableB <- dts[[2]]
  
  ABcomparison <- rCompare(dfTableA, dfTableB, keys = c("color", "number"))
  
  # Expect col summary to match the following
  expect_equal(length(ABcomparison$colMatching$inboth),3)
  expect_equal(ABcomparison$colMatching$inboth, c("COLOR" , "NUMBER", "VALUEA"))
  expect_equal(ABcomparison$colMatching$inA, character(0))
  expect_equal(ABcomparison$colMatching$inB, character(0))
  
  # Expect row summary to match the following
  expect_equal(length(ABcomparison$rowMatching),4)
  expect_equal(dim(ABcomparison$rowMatching$inboth), c(5,2))
  expect_equal(ABcomparison$rowMatching$inA$COLOR, logical(0))
  expect_equal(ABcomparison$rowMatching$inA$NUMBER, logical(0))
  expect_equal(ABcomparison$rowMatching$inB$COLOR, logical(0))
  expect_equal(ABcomparison$rowMatching$inB$NUMBER, logical(0))
  expect_equal(ABcomparison$rowMatching$matchKeys, c("COLOR","NUMBER"))
  
  # Matches should just be 1 field
  expect_that(ABcomparison$matches == "VALUEA", is_true())
  
  # Mismatches should be empty
  expect_equal(length(ABcomparison$mismatches), 0)
  
  # Cleaning - expect char to factor
  expect_equal(length(ABcomparison$cleaninginfo$COLOR),4)
    
})

test_that("ComparisonOfUnEquals", {
  
  # Create and compare two matching datasets
  dts <- createMatchoingTwoIndiceData(5)
  
  dfTableA <- dts[[1]]
  dfTableB <- dts[[2]]
  
  dfTableB[3,3] <- dfTableB[3,3] - 0.1
  
  ABcomparison <- rCompare(dfTableA, dfTableB, keys = c("color", "number"))
  
  # Expect col summary to match the following
  expect_equal(length(ABcomparison$colMatching$inboth),3)
  expect_equal(ABcomparison$colMatching$inboth, c("COLOR" , "NUMBER", "VALUEA"))
  expect_equal(ABcomparison$colMatching$inA, character(0))
  expect_equal(ABcomparison$colMatching$inB, character(0))
  
  # Expect row summary to match the following
  expect_equal(length(ABcomparison$rowMatching),4)
  expect_equal(dim(ABcomparison$rowMatching$inboth), c(5,2))
  expect_equal(ABcomparison$rowMatching$inA$COLOR, logical(0))
  expect_equal(ABcomparison$rowMatching$inA$NUMBER, logical(0))
  expect_equal(ABcomparison$rowMatching$inB$COLOR, logical(0))
  expect_equal(ABcomparison$rowMatching$inB$NUMBER, logical(0))
  expect_equal(ABcomparison$rowMatching$matchKeys, c("COLOR","NUMBER"))
  
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
  dts <- createMatchoingTwoIndiceData(5)
  
  dfTableA <- dts[[1]]
  dfTableB <- dts[[2]][1:4,]
  
  ABcomparison <- rCompare(dfTableA, dfTableB, keys = c("color", "number"))
  
  # Expect col summary to match the following
  expect_equal(length(ABcomparison$colMatching$inboth),3)
  expect_equal(ABcomparison$colMatching$inboth, c("COLOR" , "NUMBER", "VALUEA"))
  expect_equal(ABcomparison$colMatching$inA, character(0))
  expect_equal(ABcomparison$colMatching$inB, character(0))
  expect_equal(ABcomparison$rowMatching$matchKeys, c("COLOR","NUMBER"))
  
  # Expect row summary to match the following
  expect_equal(length(ABcomparison$rowMatching),4)
  expect_equal(dim(ABcomparison$rowMatching$inboth), c(4,2))
  expect_equal(length(ABcomparison$rowMatching$inA$COLOR), 1)
  expect_equal(length(ABcomparison$rowMatching$inA$NUMBER), 1)
  expect_equal(ABcomparison$rowMatching$inB$COLOR, logical(0))
  expect_equal(ABcomparison$rowMatching$inB$NUMBER, logical(0))
  
  # Matches should just be 1 field
  expect_that(ABcomparison$matches == "VALUEA", is_true())
  
  # Mismatches should be empty
  expect_equal(length(ABcomparison$mismatches), 0)
  
  # Cleaning - expect char to factor
  expect_equal(length(ABcomparison$cleaninginfo$COLOR),4)
  
})

test_that("ComparisonOfMissCols", {
  
  # Create and compare two matching datasets
  dts <- createMatchoingTwoIndiceData(5)
  
  dfTableA <- dts[[1]]
  dfTableB <- dts[[2]]
  dfTableB$NEWCOL <- seq(1:5)
  
  ABcomparison <- rCompare(dfTableA, dfTableB, keys = c("color", "number"))
  
  # Expect col summary to match the following
  expect_equal(length(ABcomparison$colMatching$inboth),3)
  expect_equal(ABcomparison$colMatching$inboth, c("COLOR" , "NUMBER", "VALUEA"))
  expect_equal(ABcomparison$colMatching$inA, character(0))
  expect_equal(ABcomparison$colMatching$inB, "NEWCOL")
  
  # Expect row summary to match the following
  expect_equal(length(ABcomparison$rowMatching),4)
  expect_equal(dim(ABcomparison$rowMatching$inboth), c(5,2))
  expect_equal(ABcomparison$rowMatching$inA$COLOR, logical(0))
  expect_equal(ABcomparison$rowMatching$inA$NUMBER, logical(0))
  expect_equal(ABcomparison$rowMatching$inB$COLOR, logical(0))
  expect_equal(ABcomparison$rowMatching$inB$NUMBER, logical(0))
  expect_equal(ABcomparison$rowMatching$matchKeys, c("COLOR","NUMBER"))
  
  # Matches should just be 1 field
  expect_that(ABcomparison$matches == "VALUEA", is_true())
  
  # Mismatches should be empty
  expect_equal(length(ABcomparison$mismatches), 0)
  
  # Cleaning - expect char to factor
  expect_equal(length(ABcomparison$cleaninginfo$COLOR),4)
  
})