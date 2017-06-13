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
# UNIT TEST*: matchColumns
#
# * matchColumns mostly just calls a bunch of other functions we've
#   written, so it might be called an integration test
#
# matchColumns should return subsets of the two input data frames based
# on their matching columns, as well as information about the columns that
# did not match and were excluded from the subsets
#

# loading testing library
library(testthat)

context("Match Columns")

test_that("function passes overall functionality", {

  # create sample dataframes
  # sample input dataframes
  inDfa <- data.frame(daTe = as.Date(character()),
                      stringsAsFactors = FALSE)
  inDfa$' car' = character()
  inDfa$'model ' = character()
  
  inDfb <- data.frame(model = character(),
                      daTe = as.Date(character()),
                      brand = character(),
                      stringsAsFactors = FALSE)
  
  # expected output dataframes
  outDfa <- data.frame(DATE = as.Date(character()),
                       MODEL = character(),
                       stringsAsFactors = FALSE)
  outDfb <- data.frame(DATE = as.Date(character()),
                       MODEL = character(),
                       stringsAsFactors = FALSE)
  

  
  # check output
  colName <- c(' car', 'daTe', 'model ')
  mapping <- c('CAR', 'DATE', 'MODEL')
  inB <- c(FALSE, TRUE, TRUE)
  resultA <- data.frame(colName, mapping, inB)
  colName <- c('brand', 'daTe', 'model')
  mapping <- c('BRAND', 'DATE', 'MODEL')
  inA <- c(FALSE, TRUE, TRUE)
  resultB <- data.frame(colName, mapping, inA)
  result <- list(outDfa, outDfb, resultA, resultB)
  names(result) <- c("subsetDFA", "subsetDFB", "colInfoA", "colInfoB")
  expect_equal(matchColumns(inDfa, inDfb), result)
})


test_that("function handles possible duplicate column names", {
  
  # create sample dataframes
  # sample input dataframes
  inDfa <- data.frame(daTe = as.Date(character()),
                      stringsAsFactors = FALSE)
  inDfa$' car' = character()
  inDfa$'model ' = character()
  inDfa$' MoDel ' = character()
  
  inDfb <- data.frame(model = character(),
                      daTe = as.Date(character()),
                      brand = character(),
                      stringsAsFactors = FALSE)
  # Add some data
  inDfb[1,] <- c("A",NA, "B")
  inDfa[1,] <- c(NA, "A", "B", "C")
  
 
  
 expect_error(matchColumns(inDfa, inDfb))
 
 
})