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
# UNIT TEST: coerceData
#
# Test of coerceData function.
# Check that they can be coerced to data frame
# Input: DOA, DOB
# Output: DFA, DFB
#

context('coerceData')


test_that("coerceData fails for items that can't be coerced to df", {
  
  # Things to check
  #   - weird lists fail
  #   - dates fail
  #   - things from weird classes (i.e. models) fail
  #   - Lists error
  
  df <- data.frame(col1 = c(1, 2, 3), col2 = c(3, 4, 5))
  lst <- list(c(2, 3, 5), c(4, 3))
  x <- c(1, 2, 4)
  y <- c(4, 5, 6)
  mod <- lm(y ~ x)
  
  expect_error(coerceData(df, lst))
  expect_error(coerceData(mod, df))
})

test_that("coerceData works correctly for items that can be coerced to df", {
  
  # Things to check
  #   - Two Dfs works
  #   - Matrices work
  #   - Lists error
  
  df <- data.frame(col1 = c(1, 2, 3), col2 = c(3, 4, 5))
  mat <- matrix(data = c(1, 2, 3, 4), nrow = 2)
  lst <- list(1, 2, 3, 4)
  
  expect_equal(coerceData(df, mat), list(df, as.data.frame(mat)))
  expect_equal(coerceData(df, lst), list(df, as.data.frame(lst)))
  # Should be messaged about one list, one data frame beibg passsed, as col names are uncertain
  expect_message(coerceData(df, lst))
  expect_equal(coerceData(df, df), list(df, df))
})

test_that("coerceData correctly renames vectors that are passed", {
  

  vect1 <- c(1,2,3,4,5,6,7,8,9)
  vect2 <- c(1,2,3,4,5,6,7,8,18)
  
  vect3 <- c("A","B","C")
  vect4 <- c("A","B","C")
  
  expect_message(cd1 <- coerceData(vect1, vect2), "Detected vector input - renaming columns")
  expect_message(cd2 <- coerceData(vect3, vect4), "Detected vector input - renaming columns")
  
  expect_equal(names(cd1[[1]]),"VectorCol")
  expect_equal(names(cd1[[2]]),"VectorCol")
  expect_equal(names(cd2[[1]]),"VectorCol")
  expect_equal(names(cd2[[2]]),"VectorCol")
  
})