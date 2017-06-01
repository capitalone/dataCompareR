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
# UNIT TEST*: executeCoercions
#
# Input DFA, DFB, WhitespaceTrim (Y/N)
# Output: CDFA, CDFB, COERCION info?
#
# * Could be considered integration
#
# The function executeCoercions calls other functions for trimming whitespace, 
# coercing factors to characters. In the future, it may change date formats to 
# be the same and also coerce numeric types (not currently within scope).
# 
# Things to test
# - Two identical dfs with no coercion needed returns the same
# - One "perfect" df and one with factors returns the df and the coerced one
# - Vice Versa
# - Two that need coercion both work
#

context("Coercion wrapper function")

test_that("Coercion wrapper function", {
  
  DF <- data.frame(numeric = c(1, 2, 3, 4), character = c("a", "b", "c", "d"), stringsAsFactors = F)
  Fac <- data.frame(numeric = c("1", "2", "3", "4"), character = c("a", "b", "c", "d"))
  WSF <- data.frame(numeric = c(1.0, 2, 3.0, 4), character = c(" a ", "  b", "c ", "d"))
  WS <- data.frame(numeric = c(1.0, 2, 3.0, 4), character = c(" a ", "  b", "c ", "d"), stringsAsFactors = F)
  
  FacAfter <- data.frame(numeric = c("1", "2", "3", "4"), character = c("a", "b", "c", "d"), stringsAsFactors = F)
  WSFAfter <- data.frame(numeric = c(1.0, 2, 3.0, 4), character = c("a", "b", "c", "d"), stringsAsFactors = F)
  WSFAfterWS <- data.frame(numeric = c(1.0, 2, 3.0, 4), character = c(" a ", "  b", "c ", "d"), stringsAsFactors = F)
  WSAfter <- data.frame(numeric = c(1.0, 2, 3.0, 4), character = c("a", "b", "c", "d"), stringsAsFactors = F)
  
  T1 <- data.frame(numeric = c("numeric", "numeric", "numeric", "numeric"), 
                   character = c("character", "character", "character", "character"), 
                   stringsAsFactors = FALSE, 
                   row.names = c("DFATypesOrig", "DFBTypesOrig", "DFATypesNew", "DFBTypesNew"))
  
  T2 <- data.frame(numeric = c("numeric", "numeric", "numeric", "numeric"), 
                   character = c("character", "character", "character", "character"), 
                   stringsAsFactors = FALSE, 
                   row.names = c("DFATypesOrig", "DFBTypesOrig", "DFATypesNew", "DFBTypesNew"))
  
  T3 <- data.frame(numeric = c("factor", "numeric", "character", "numeric"), 
                   character = c("factor", "factor", "character", "character"), 
                   stringsAsFactors = FALSE, 
                   row.names = c("DFATypesOrig", "DFBTypesOrig", "DFATypesNew", "DFBTypesNew"))
  
  T4 <- data.frame(numeric = c("factor", "numeric", "character", "numeric"), 
                   character = c("factor", "factor", "character", "character"), 
                   stringsAsFactors = FALSE, 
                   row.names = c("DFATypesOrig", "DFBTypesOrig", "DFATypesNew", "DFBTypesNew"))
  
  T5 <- data.frame(numeric = c("numeric", "factor", "numeric", "character"), 
                   character = c("factor", "factor", "character", "character"), 
                   stringsAsFactors = FALSE, 
                   row.names = c("DFATypesOrig", "DFBTypesOrig", "DFATypesNew", "DFBTypesNew"))
  
  T6 <- data.frame(numeric = c("numeric", "numeric", "numeric", "numeric"), 
                   character = c("character", "factor", "character", "character"), 
                   stringsAsFactors = FALSE, 
                   row.names = c("DFATypesOrig", "DFBTypesOrig", "DFATypesNew", "DFBTypesNew"))
  
  Ret1 <- list(DF, DF, T1)
  names(Ret1) <- c("DFA", "DFB", "DataTypes")
  Ret2 <- list(DF, DF, T2)
  names(Ret2) <- c("DFA", "DFB", "DataTypes")
  Ret3 <- list(FacAfter, WSFAfter, T3)
  names(Ret3) <- c("DFA", "DFB", "DataTypes")
  Ret4 <- list(FacAfter, WSFAfterWS, T4)
  names(Ret4) <- c("DFA", "DFB", "DataTypes")
  Ret5 <- list(WSFAfterWS, FacAfter, T5)
  names(Ret5) <- c("DFA", "DFB", "DataTypes")
  Ret6 <- list(WSAfter, WSFAfter, T6)
  names(Ret6) <- c("DFA", "DFB", "DataTypes")

  expect_equal(executeCoercions(DF, DF, T), Ret1)
  expect_equal(executeCoercions(DF, DF, F), Ret2)
  expect_equal(executeCoercions(Fac, WSF, T), Ret3)
  expect_equal(executeCoercions(Fac, WSF, F), Ret4)
  expect_equal(executeCoercions(WSF, Fac, F), Ret5)
  expect_equal(executeCoercions(WS, WSF, T), Ret6)
  
})