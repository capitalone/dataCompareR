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
# UNIT TEST: locateMismatches
#
# Input: Two dfs of identical size (with potentially different values)
# Output: Table with ID, then TRUE/FALSE for non-key variables.
#
# To Test:
# - Identical DFs
# - Different DFs
# - Numbers as characters and actual numbers 
#

context("LocateMismatches")

test_that("locateMismatches with a single key", {
  
  # Test DFs (random using pets)
  DFA <- data.frame(k = c(1, 2, 3), pet = c("dog", "cat", "axolotl"), colour = c("brown", "white", "white"), count = c(1, 2, 1), stringsAsFactors = F)
  DFB <- data.frame(k = c(1, 2, 3), pet = c("dog", "cat", "axolotl"), colour = c("brown", "brown", "white"), count = c(1, 2, 1), stringsAsFactors = F)
  DFC <- data.frame(k = c(1, 2, 3), pet = c("dog", "cat", "wolf"), colour = c("brown", "white", "white"), count = c(1, 1, 2), stringsAsFactors = F)
  DFD <- data.frame(k = c(1, 2, 3), pet = c("dog", "cat", "axolotl"), colour = c("brown", "white", "white"), count = c("1", "2", "1"), stringsAsFactors = F)
  
  # Expected Results
  ResAA <- data.frame(k = c(1, 2, 3), pet = c(T, T, T), colour = c(T, T, T), count = c(T, T, T), stringsAsFactors = F)
  ResAB <- data.frame(k = c(1, 2, 3), pet = c(T, T, T), colour = c(T, F, T), count = c(T, T, T), stringsAsFactors = F)
  ResAC <- data.frame(k = c(1, 2, 3), pet = c(T, T, F), colour = c(T, T, T), count = c(T, F, F), stringsAsFactors = F)
  ResAD <- data.frame(k = c(1, 2, 3), pet = c(T, T, T), colour = c(T, T, T), count = c(F, F, F), stringsAsFactors = F)
  
  # Test
  expect_equal(locateMismatches(DFA, DFA, keys = "k"), ResAA)
  expect_equal(locateMismatches(DFA, DFB, keys = "k"), ResAB)
  expect_equal(locateMismatches(DFA, DFC, keys = "k"), ResAC)
  expect_equal(locateMismatches(DFA, DFD, keys = "k"), ResAD)

})

test_that("locateMismatches with multiple keys", {
  
  # Test DFs
  DFA <- data.frame(k1 = c(1, 1, 2), k2 = c(1, 2, 1), char = c("dog", "cat", "monkey"), num = c(10, 8, 6), stringsAsFactors = F)
  DFB <- data.frame(k1 = c(1, 1, 2), k2 = c(1, 2, 1), char = c("dog", "dog", "monkey"), num = c(10, 8, 6), stringsAsFactors = F)
  DFC <- data.frame(k1 = c(1, 1, 2), k2 = c(1, 2, 1), char = c("dog", "cat", "monkey"), num = c(10, 8, 0.1), stringsAsFactors = F)
  DFD <- data.frame(k1 = c(1, 1, 2), k2 = c(1, 2, 1), char = c("dog", "dog", "dog"), num = c(1, 4, 7), stringsAsFactors = F)
  
  # Expected Results
  ResAA <- data.frame(k1 = c(1, 1, 2), k2 = c(1, 2, 1), char = c(T, T, T), num = c(T, T, T), stringsAsFactors = F)
  ResAB <- data.frame(k1 = c(1, 1, 2), k2 = c(1, 2, 1), char = c(T, F, T), num = c(T, T, T), stringsAsFactors = F)
  ResAC <- data.frame(k1 = c(1, 1, 2), k2 = c(1, 2, 1), char = c(T, T, T), num = c(T, T, F), stringsAsFactors = F)
  ResAD <- data.frame(k1 = c(1, 1, 2), k2 = c(1, 2, 1), char = c(T, F, F), num = c(F, F, F), stringsAsFactors = F)
  ResBD <- data.frame(k1 = c(1, 1, 2), k2 = c(1, 2, 1), char = c(T, T, F), num = c(F, F, F), stringsAsFactors = F)
  
  # Test
  expect_equal(locateMismatches(DFA, DFA, keys = c("k1", "k2")), ResAA)
  expect_equal(locateMismatches(DFA, DFB, keys = c("k1", "k2")), ResAB)
  expect_equal(locateMismatches(DFA, DFC, keys = c("k1", "k2")), ResAC)
  expect_equal(locateMismatches(DFA, DFD, keys = c("k1", "k2")), ResAD)
  expect_equal(locateMismatches(DFB, DFD, keys = c("k1", "k2")), ResBD)
  
})

test_that("locateMismatches handling NAs", {
  
  # Test DFs
  DFA <- data.frame(k = c(1, 2, 3), pet = c("dog", "cat", "axolotl"), colour = c("brown", "white", "white"), count = c(1, 2, 1), stringsAsFactors = FALSE)
  DFB <- data.frame(k = c(1, 2, 3), pet = c("dog", "cat", "axolotl"), colour = c("brown", NA, "white"), count = c(NA, NA, NA), stringsAsFactors = FALSE)
  DFC <- data.frame(k = c(1, 2, 3), pet = c("dog", "cat", "axolotl"), colour = c("brown", "white", "red"), count = c(NaN, NaN, NaN), stringsAsFactors = FALSE)
  
  # Expected Results
  ResAB <- data.frame(k = c(1, 2, 3), pet = c(T, T, T), colour = c(T, F, T), count = c(F, F, F), stringsAsFactors = F)
  ResBB <- data.frame(k = c(1, 2, 3), pet = c(T, T, T), colour = c(T, T, T), count = c(T, T, T), stringsAsFactors = F)
  ResBC <- data.frame(k = c(1, 2, 3), pet = c(T, T, T), colour = c(T, F, F), count = c(F, F, F), stringsAsFactors = F)
  ResAC <- data.frame(k = c(1, 2, 3), pet = c(T, T, T), colour = c(T, T, F), count = c(F, F, F), stringsAsFactors = F)
  
  # Impossible to create DF with NULL entry
  
  # Test
  expect_equal(locateMismatches(DFA, DFB, keys = "k"), ResAB)
  expect_equal(locateMismatches(DFB, DFB, keys = "k"), ResBB)
  expect_equal(locateMismatches(DFB, DFC, keys = "k"), ResBC)
  expect_equal(locateMismatches(DFA, DFC, keys = "k"), ResAC)
})

test_that("locateMismatches with no key information", {
  
  # Test DFs
  DFA <- data.frame(pet = c("dog", "cat", "axolotl"), colour = c("brown", "white", "white"), count = c(1, 2, 1), stringsAsFactors = FALSE)
  DFB <- data.frame(pet = c("dog", "rabbit", "axolotl"), colour = c("brown", "NA", "white"), count = c(NA, NA, NA), stringsAsFactors = FALSE)
  
  # Expected Results
  ResAA <- data.frame(pet = c(T, T, T), colour = c(T, T, T), count = c(T, T, T))
  ResAB <- data.frame(pet = c(T, F, T), colour = c(T, F, T), count = c(F, F, F))
  
  # Test
  expect_equal(locateMismatches(DFA, DFA, keys = NULL), ResAA)
  expect_equal(locateMismatches(DFB, DFA, keys = NULL), ResAB)
  
})