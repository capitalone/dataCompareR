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
# UNIT TEST*: matchRows
#
# * matchRows mostly just does a switch on input to call
#   the appropriate subfunctions, so it might be more
#   appropriate to call it an integration test
#
# matchRows generates subsets of two input dataframes that match
# on rows with zero, one or more shared keys, as well as information
# about which rows were dropped from the subsets
#

context("matchRows")

test_that("matchRows correctly finds matching rows", {

  #
  # No index example - Simple subset of both data frames A > B
  # 
  
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
  
  mtch <- matchRows(df1, df2, NA) # Expected matched subset
  msgA <- seq(16,20)   # Expected missing indices from A
  msgB <- integer()   # Expected missing indices from B
  
  expect_equal(mtch[[1]][,1], dfMtch[,1]) # Keys match subset of A
  expect_equal(mtch[[1]][,2], dfMtch[,2]) # Data matches subset of A
  expect_equal(mtch[[2]][,1], dfMtch[,1]) # Keys match subset of B
  expect_equal(mtch[[2]][,2], dfMtch[,2]) # Data matches subset of B
  expect_equal(mtch[[3]][[1]][[1]], msgA) # Missing indices from A
  expect_equal(mtch[[3]][[2]][[1]], msgB) # Missing indices from A
  
  #
  # No index example - Simple subset of both data frames A < B
  #
  
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
  
  mtch <- matchRows(df1, df2, NA) # Expected matched subset
  msgA <- integer()              # Expected missing indices from A
  msgB <- seq(16,20)             # Expected missing indices from B
  
  expect_equal(mtch[[1]][,1], dfMtch[,1]) # Keys match subset of A
  expect_equal(mtch[[1]][,2], dfMtch[,2]) # Data matches subset of A
  expect_equal(mtch[[2]][,1], dfMtch[,1]) # Keys match subset of B
  expect_equal(mtch[[2]][,2], dfMtch[,2]) # Data matches subset of B
  expect_equal(mtch[[3]][[1]][[1]], msgA) # Missing indices from A
  expect_equal(mtch[[3]][[2]][[1]], msgB) # Missing indices from A
  
  #
  # Single index example
  #
  
  ky <- seq(1,10)
  dta <- as.character(paste("data", ky))
  df1 <- data.frame(ky, dta, stringsAsFactors = F)
  
  ky <- seq(7,15)
  dta <- as.character(paste("data", ky))
  df2 <- data.frame(ky, dta, stringsAsFactors = F)
  
  # Matching elements
  
  ky <- seq(7,10)
  dta <- as.character(paste("data", ky))
  dfMtch <- data.frame(ky, dta, stringsAsFactors = F)
  
  mtch <- matchRows(df1, df2, "ky") # Expected matched subset
  msgA <- seq(1,6)   # Expected missing indices from A
  msgB <- seq(11,15) # Expected missing indices from B
  
  expect_equal(mtch[[1]][,1], dfMtch[,1]) # Keys match subset of A
  expect_equal(mtch[[1]][,2], dfMtch[,2]) # Data matches subset of A
  expect_equal(mtch[[2]][,1], dfMtch[,1]) # Keys match subset of B
  expect_equal(mtch[[2]][,2], dfMtch[,2]) # Data matches subset of B
  expect_equal(mtch[[3]][[1]][[1]], msgA) # Missing indices from A
  expect_equal(mtch[[3]][[2]][[1]], msgB) # Missing indices from A
  
  #
  # Multiple index example
  #
  
  ky <- seq(1,20)
  dta <- as.character(paste("data", ky))
  ky1 <- ky %% 10
  ky2 <- ky - ky1
  df1 <- data.frame(ky1, ky2, ky, dta, stringsAsFactors = F)
  
  ky <- seq(7,25)
  ky1 <- ky %% 10
  ky2 <- ky - ky1
  dta <- as.character(paste("data", ky))
  df2 <- data.frame(ky1, ky2, ky, dta, stringsAsFactors = F)
  
  # Matching elements
  
  ky <- seq(7,20)
  ky1 <- ky %% 10
  ky2 <- ky - ky1
  dta <- as.character(paste("data", ky))
  dfMtch <- data.frame(ky1, ky2, ky, dta, stringsAsFactors = F)
  
  mtch <- matchRows(df1, df2, c("ky1", "ky2")) # Expected matched subset
  msgA <- seq(1,6)   # Expected missing indices from A
  msgA1 <- msgA %% 10
  msgA2 <- msgA - msgA1
  
  msgB <- seq(21,25) # Expected missing indices from B
  msgB1 <- msgB %% 10
  msgB2 <- msgB - msgB1
  
  mtchSorted <- arrange(mtch[[1]], ky1, ky2)
  dfMtchSorted <- arrange(dfMtch, ky1, ky2)
  
  expect_equal(mtchSorted[,1], dfMtchSorted[,1]) # First key matches subset of A
  expect_equal(mtchSorted[,2], dfMtchSorted[,2]) # Second key matches subset of A
  expect_equal(mtchSorted[,4], dfMtchSorted[,4]) # Data matches subset of A
  
  mtch2Sorted <- arrange(mtch[[2]], ky1, ky2)
  
  expect_equal(mtch2Sorted[,1], dfMtchSorted[,1]) # First key matches subset of B
  expect_equal(mtch2Sorted[,2], dfMtchSorted[,2]) # Second key matches subset of B
  expect_equal(mtch2Sorted[,4], dfMtchSorted[,4]) # Data matches subset of B
  
  mtchSorted31 <- arrange(mtch[[3]][[1]], ky1, ky2)
  mtchSorted32 <- arrange(mtch[[3]][[2]], ky1, ky2)
  
  expect_equal(mtchSorted31[[1]], msgA1) # Missing indices from A
  expect_equal(mtchSorted31[[2]], msgA2) # Missing indices from A
  expect_equal(mtchSorted32[[1]], msgB1) # Missing indices from A
  expect_equal(mtchSorted32[[2]], msgB2) # Missing indices from A  
})
