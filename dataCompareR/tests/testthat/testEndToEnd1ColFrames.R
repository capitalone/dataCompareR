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
# SYSTEM TEST: dataCompareR : Single column datasets
#
# End-to-end system tests of the dataCompareR function that look at 
# different scenarios involving data frames with a single columns. Added
# becase there was a bug with single column data frames
#

library(testthat)

context("Tests dataframes with a single column")

test_that("Comparison with missing row", {
  
  df1 <- tibble(col1 = c("cat", "dog", "rat"))
  df2 <- tibble(col1 = c("cat", "dog", "mouse", "fly"))
  
  # test it both ways as we have some logic where nrows a > b, etc
  comparison1 <- rCompare(df1, df2)
  comparison2 <- rCompare(df2, df1)
  
  # Expect col summary to match the following
  expect_equal(length(comparison1$colMatching$inboth),1)
  expect_equal(comparison1$colMatching$inboth, c("COL1"))
  expect_equal(comparison1$colMatching$inA, character(0))
  expect_equal(comparison1$colMatching$inB, character(0))
  
  expect_equal(length(comparison2$colMatching$inboth),1)
  expect_equal(comparison2$colMatching$inboth, c("COL1"))
  expect_equal(comparison2$colMatching$inA, character(0))
  expect_equal(comparison2$colMatching$inB, character(0))
  
  # Expect row summary to match the following
  expect_equal(length(comparison1$rowMatching),4)
  expect_equal(comparison1$rowMatching$inboth, c(1,2,3))
  expect_equal(comparison1$rowMatching$inA$indices_removed, integer(0))
  expect_equal(comparison1$rowMatching$inB$indices_removed, c(4))
  
  expect_equal(length(comparison2$rowMatching),4)
  expect_equal(comparison2$rowMatching$inboth, c(1,2,3))
  expect_equal(comparison2$rowMatching$inB$indices_removed, integer(0))
  expect_equal(comparison2$rowMatching$inA$indices_removed, c(4))
  
  # Matches should be empty
  expect_equal(length(comparison1$matches), 0)
  expect_equal(length(comparison2$matches), 0)
  
  # Mismatches should list with 1 entry
  expect_equal(length(comparison1$mismatches), 1)
  expect_equal(names(comparison1$mismatches), c("COL1"))
  
  expect_equal(length(comparison2$mismatches), 1)
  expect_equal(names(comparison2$mismatches), c("COL1"))
  
})


test_that("Comparison with equal datasets", {
  
  df2 <- tibble(col1 = c("cat", "dog", "mouse", "fly"))
  df1 <- tibble(col1 = c("cat", "dog", "mouse", "fly"))
  
  comparison <- rCompare(df1, df2)
  
  # Expect col summary to match the following
  expect_equal(length(comparison$colMatching$inboth),1)
  expect_equal(comparison$colMatching$inboth, c("COL1"))
  expect_equal(comparison$colMatching$inA, character(0))
  expect_equal(comparison$colMatching$inB, character(0))
  
  # Expect row summary to match the following
  expect_equal(length(comparison$rowMatching),4)
  expect_equal(comparison$rowMatching$inboth, c(1,2,3, 4))
  expect_equal(comparison$rowMatching$inA$indices_removed, integer(0))
  expect_equal(comparison$rowMatching$inB$indices_removed, integer(0))
  
  # Matches should be empty
  expect_equal(length(comparison$matches), 1)
  
  # Mismatches should list with 1 entry
  expect_equal(length(comparison$mismatches), 0)
  
  
})

