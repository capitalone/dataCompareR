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
# UNIT TESTS: createMismatchObject and variableMismatches
#
# createMismatchObject creates a list of mismatch information.
# variableMismatches, which is called by createMismatchObject 
# generates information about the mismatches for a given variable.
# 

library(testthat)
library(dplyr)

# Test are example data specific, but written in such a way that adding a new column should not break the tests
context('createMismatchObject')

test_that("Mismatches work", {
  
  ### Set up
  dat_a <- data.frame(
    id = 1001:1010,
    x = 1L:10L, # integer
    y = seq(0.1, 1, length.out = 10),
    z = letters[1:10],
    stringsAsFactors = F
  )
  
  dat_b <- data.frame(
    id = 1001:1010,
    x = c(10,1:9), # numeric
    y = c(1, seq(0.2, 1, length.out = 9)),
    z = c(letters[1:9], "z"),
    stringsAsFactors = F
  )
  
  dat_eq <- data.frame(
    id = 1001:1010,
    x = rep(F, 10), 
    y = c(F, rep(T, 9)),
    z = c(rep(T, 9), F),
    stringsAsFactors = FALSE 
  )
  
  str_index = c("id")
  
  # createMismatchObject output
  mismatchObject <- createMismatchObject(dat_a, dat_b, dat_eq, str_index)
  
  anyNegative <- function(x) {
    any(!x)
  }
  
  numberOfColumnsWithMismatch <- sum(unlist(
    dat_eq %>%
      dplyr::select(which(!(colnames(dat_eq) %in% str_index))) %>%
      summarize_each(list(~anyNegative))
  )) 
  
  # Same sub-functions as used by createMismatchObject
  xMismatches <- variableMismatches("x", dat_a[, c(str_index, "x")], dat_b[, c(str_index, "x")], dat_eq$x)
  yMismatches <- variableMismatches("y", dat_a[, c(str_index, "y")], dat_b[, c(str_index, "y")], dat_eq$y)
  zMismatches <- variableMismatches("z", dat_a[, c(str_index, "z")], dat_b[, c(str_index, "z")], dat_eq$z)
  
  # xDetails <- variableDetails(xMismatches)
  # yDetails <- variableDetails(yMismatches)
  # zDetails <- variableDetails(zMismatches)
  
  
  ### Tests
  expect_equal( length(mismatchObject), numberOfColumnsWithMismatch )
  
  expect_equal( dim(xMismatches), c(sum(!(dat_eq$x)), length(str_index) + 3))
  expect_equal( dim(yMismatches), c(sum(!(dat_eq$y)), length(str_index) + 3))
  expect_equal( dim(zMismatches), c(sum(!(dat_eq$z)), length(str_index) + 3))
  
  expect_equal( dim(mismatchObject$x), c(sum(!(dat_eq$x)), length(str_index) + 6))
  expect_equal( dim(mismatchObject$y), c(sum(!(dat_eq$y)), length(str_index) + 6))
  expect_equal( dim(mismatchObject$y), c(sum(!(dat_eq$z)), length(str_index) + 6))
  
  expect_equal( mismatchObject$x$typeA[1], "integer")
  expect_equal( mismatchObject$x$typeB[1], "double")
  expect_equal( mismatchObject$y$typeA[1], "double")
  expect_equal( mismatchObject$y$typeB[1], "double")
  expect_equal( mismatchObject$z$typeA[1], "character")
  expect_equal( mismatchObject$z$typeB[1], "character")
  
  expect_equal( mismatchObject$y$id, 1001)
  expect_equal( mismatchObject$z$id, 1010)
  
  expect_equal( mismatchObject$y$diffAB, -0.9)

})
