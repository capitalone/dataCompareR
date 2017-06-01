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

#' @suggests bit64

#
# SYSTEM TEST: dataCompareR : Bit64
#
# End-to-end system tests of the dataCompareR function that look at 
# different scenarios involving data frames that have bit64 values
#

library(testthat)

context("Bit64Comparisons")

test_that("ComparisonOfBit64Fields", {

  if(require(bit64)) {
    source('createTwoKeyData.R')
    
    dts <- createBit64Dataset()
    
    dfTableA <- dts[[1]]
    dfTableB <- dts[[2]]
    dfTableC <- dts[[2]]
    dfTableC$bigNumber <- dfTableC$bigNumber -1
    
    ABcomparison <- rCompare(dfTableA, dfTableB, keys = c("color"))
    ACcomparison <- rCompare(dfTableA, dfTableC, keys = c("color"))
    
    expect_that(ABcomparison$matches[1] == "BIGNUMBER", is_true() )
    expect_that(is.na(ACcomparison$matches[1]), is_true() )
    
    expect_that(length(ABcomparison$mismatches) == 0, is_true() )
    expect_that(nrow(ACcomparison$mismatches$BIGNUMBER) == 5, is_true() )
    expect_that(all(ACcomparison$mismatches$BIGNUMBER$diffAB == 1), is_true() )
  }
  
 })
  
  
  