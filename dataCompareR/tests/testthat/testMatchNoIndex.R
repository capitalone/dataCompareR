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
# UNIT TEST: matchNoIndex
# 
# matchNoIndex returns matches two data frames on rows without a key
# and returns the subset of the data frames based on shared rows,
# as well as a list of the rows that were not included in the subset
# 
context("matchNoIndex")
test_that("matchNoIndex correctly finds matching rows", {

  # Simple subset of both data frames A > B
  
  ky <- seq(1,20)
  dta <- as.character(paste("data", ky))
  df1 <- data.frame(ky, dta, stringsAsFactors = F)
  
  ky <- seq(1,15)
  dta <- as.character(paste("data", ky))
  df2 <- data.frame(ky, dta, stringsAsFactors = F)
  
  # Matching elements
  ky <- seq(1,15)
  dta <- as.character(paste("data", ky))
  dfMtch <- data.frame(ky, dta, stringsAsFactors = F)

  mtch <- matchNoIndex(df1, df2) # Expected matched subset
  msgA <- seq(16,20)   # Expected missing indices from A
  msgB <- integer()   # Expected missing indices from B
  
  expect_equal(mtch[[1]][,1], dfMtch[,1]) # Keys match subset of A
  expect_equal(mtch[[1]][,2], dfMtch[,2]) # Data matches subset of A
  expect_equal(mtch[[2]][,1], dfMtch[,1]) # Keys match subset of B
  expect_equal(mtch[[2]][,2], dfMtch[,2]) # Data matches subset of B
  expect_equal(mtch[[3]][[1]][[1]], msgA) # Missing indices from A
  expect_equal(mtch[[3]][[2]][[1]], msgB) # Missing indices from A

  # Simple subset of both data frames A < B
  
  ky <- seq(1,15)
  dta <- as.character(paste("data", ky))
  df1 <- data.frame(ky, dta, stringsAsFactors = F)
  
  ky <- seq(1,20)
  dta <- as.character(paste("data", ky))
  df2 <- data.frame(ky, dta, stringsAsFactors = F)
  
  # Matching elements
  ky <- seq(1,15)
  dta <- as.character(paste("data", ky))
  dfMtch <- data.frame(ky, dta, stringsAsFactors = F)
  
  mtch <- matchNoIndex(df1, df2) # Expected matched subset
  msgA <- integer()              # Expected missing indices from A
  msgB <- seq(16,20)             # Expected missing indices from B
  
  expect_equal(mtch[[1]][,1], dfMtch[,1]) # Keys match subset of A
  expect_equal(mtch[[1]][,2], dfMtch[,2]) # Data matches subset of A
  expect_equal(mtch[[2]][,1], dfMtch[,1]) # Keys match subset of B
  expect_equal(mtch[[2]][,2], dfMtch[,2]) # Data matches subset of B
  expect_equal(mtch[[3]][[1]][[1]], msgA) # Missing indices from A
  expect_equal(mtch[[3]][[2]][[1]], msgB) # Missing indices from A
  
})