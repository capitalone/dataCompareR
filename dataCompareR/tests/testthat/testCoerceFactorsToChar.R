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
# UNIT TEST: coerceFactorsToChar
#
# Input: Data Frame
# Output: Data Frame with factors replaced with characters.
#
# Tests
# - DF with character data as factor
# - DF with numeric data as factor
# - DF with mix as factor
# - DF with multiple column types all as factor
#

context('coerceFactorsToChar')


test_that("coerceFactorsToChar: DF with numeric factors", {
  df <- data.frame(test_col = c(1,3,3,4,3,1))
  df$test_col <- as.factor(df$test_col)
  shouldbe <- data.frame(test_col = c("1","3","3","4","3","1"), stringsAsFactors = F)
  
  expect_equal(coerceFactorsToChar(df), shouldbe)
})

test_that("coerceFactorsToChar: DF with characters as factors", {
  base <- data.frame(test_col = c("a", "b", "d"))
  check <- data.frame(test_col = c("a", "b", "d"), stringsAsFactors=FALSE)
  
  expect_equal(coerceFactorsToChar(base), check)
})

test_that("coerceFactorsToChar: DF with mix of num/char as factors",{
  base <- data.frame(test_col = as.factor(c(1, "cat", 4, 5, "dog", "Clarkson")))
  check <- data.frame(test_col = c(1, "cat", 4, 5, "dog", "Clarkson"), stringsAsFactors = F)
  
  expect_equal(coerceFactorsToChar(base), check)
})

test_that("coerceFactorsToChar: DF with multiple columns to be coerced", {
  base <- data.frame(col1 = c("a", "b", "brexit", "golf"), 
                     col2 = c("1", "2", ".", "999"), 
                     col3 = c("0", "1", "0", "1"))
  check <- data.frame(col1 = c("a", "b", "brexit", "golf"), 
                     col2 = c("1", "2", ".", "999"), 
                     col3 = c("0", "1", "0", "1"), stringsAsFactors = F)
  
  expect_equal(coerceFactorsToChar(base), check)
})

