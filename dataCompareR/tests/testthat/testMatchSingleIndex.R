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
# UNIT TEST: matchSingleIndex
#
# matchSingleIndex should generate two dataframes that contain 
# the same rows based on a single index, as well as a list
# of information about the rows that were dropped from the
# data frames, if applicable
# 

context("matchSingleIndex")

test_that("matchSingleIndex correctly finds matching rows", {

  # Simple subset of both data frames
  
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

  mtch <- matchSingleIndex(df1, df2, "ky") # Expected matched subset
  msgA <- seq(1,6)   # Expected missing indices from A
  msgB <- seq(11,15) # Expected missing indices from B
  
  expect_equal(mtch[[1]][,1], dfMtch[,1]) # Keys match subset of A
  expect_equal(mtch[[1]][,2], dfMtch[,2]) # Data matches subset of A
  expect_equal(mtch[[2]][,1], dfMtch[,1]) # Keys match subset of B
  expect_equal(mtch[[2]][,2], dfMtch[,2]) # Data matches subset of B
  expect_equal(mtch[[3]][[1]][[1]], msgA) # Missing indices from A
  expect_equal(mtch[[3]][[2]][[1]], msgB) # Missing indices from A
})
