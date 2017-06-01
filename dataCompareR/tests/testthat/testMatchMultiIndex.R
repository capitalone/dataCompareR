
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
# UNIT TEST: matchMultiIndex
#
# matchMultiIndex should return subsets of two data frames 
# that share rows using a multi-key index, as well as information
# about any rows that were excluded from the subsets
#

context('matchMultiIndex')

test_that("matchMultiIndex correctly finds matching rows", {

  # Simple subset of both data frames
  
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

  mtch <- matchMultiIndex(df1, df2, c("ky1", "ky2")) # Expected matched subset
  msgA <- seq(1,6)   # Expected missing indices from A
  msgA1 <- msgA %% 10
  msgA2 <- msgA - msgA1
  
  msgB <- seq(21,25) # Expected missing indices from B
  msgB1 <- msgB %% 10
  msgB2 <- msgB - msgB1
  
  dfMtchSorted <- arrange(dfMtch, ky1, ky2)
  mtchSorted <- arrange(mtch[[1]], ky1, ky2)
  
  expect_equal(mtchSorted[,1], dfMtchSorted[,1]) # First key matches subset of A
  expect_equal(mtchSorted[,2], dfMtchSorted[,2]) # Second key matches subset of A
  expect_equal(mtchSorted[,4], dfMtchSorted[,4]) # Data matches subset of A
  
  mtchSorted2 <- arrange(mtch[[2]], ky1, ky2)
  
  expect_equal(mtchSorted2[,1], dfMtchSorted[,1]) # First key matches subset of B
  expect_equal(mtchSorted2[,2], dfMtchSorted[,2]) # Second key matches subset of B
  expect_equal(mtchSorted2[,4], dfMtchSorted[,4]) # Data matches subset of B
  
  mtchSorted31 <- arrange(mtch[[3]][[1]], ky1, ky2)
  mtchSorted32 <- arrange(mtch[[3]][[2]], ky1, ky2)
  
  expect_equal(mtchSorted31[[1]], msgA1) # Missing indices from A
  expect_equal(mtchSorted31[[2]], msgA2) # Missing indices from A
  expect_equal(mtchSorted32[[1]], msgB1) # Missing indices from A
  expect_equal(mtchSorted32[[2]], msgB2) # Missing indices from A
})

test_that("matchMultiIndex doesn't produce any warnings when creating output" , {
  pressure2 <- pressure
  
  pressure2$ID1 <- 1
  pressure2$ID2 <- 1
  
  pressure3 <- pressure2
  
  pressure3$NEWCOL <- 1
  pressure3$temperature[5] <- 81
  pressure3$pressure[4] <- 81
  
  a <- rCompare(pressure2, pressure3,keys = c('temperature', 'ID1', 'ID2') )
  
  expect_message(summary(a),"dataCompareR is generating the summary...")
  
})
