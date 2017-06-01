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


library(testthat)

# The aim of this test is to check the generation of the mismatch data.
# It is not a complete test

# Create pressure 2 with a slight difference
# and randomly ordered
pressure2 <- pressure
pressure2[3,2] <-  pressure2[3,2] + 1
pressure2[5,2] <-  pressure2[5,2] + 1
pressure2 <- pressure2[sample(nrow(pressure2)),]

# Create pressure 3/4 with a bigger difference and a new ID column
# and randomly ordered

pressure3 <- pressure2
pressure3$ID2 <- 0

pressure4 <- pressure3
pressure4 <- pressure4[sample(nrow(pressure4)),]
pressure4[1,2] <- pressure4[1,2] + 1
pressure4[4,2] <- pressure4[4,2] + 1
pressure4[6,2] <- pressure4[6,2] + 1


context("OutputComparisons : mismatch data")

if(require(titanic)) {
  source('createTitanicDatasets.R')
  
  # Tests the case where the dataCompareR object has no mismatches
  test_that("noMismatches", {
    
    # The object has already pre-determined structure
    compareObject <- rCompare(titanic2,titanic2)
    
    m0 <- generateMismatchData(compareObject,titanic2, titanic2)
    
    expect_message(generateMismatchData(compareObject,titanic2, titanic2),"No mismatches")
    expect_null(m0)
  })
  
  # When there are mismatches, ensure we get the right output
  test_that("Mismatches", {
  
    # The object has already pre-determined structure
    compareObject <- rCompare(titanic,titanic2)
    m0 <- generateMismatchData(compareObject, titanic,titanic2)
  
    expect_is(m0,"list")
    expect_output(str(m0), "List of 2")
    expect_output(str(m0[[1]]),"178 obs")
    expect_output(str(m0[[1]]),"13 variables")
    expect_output(str(m0[[2]]),"178 obs")
    expect_output(str(m0[[2]]),"13 variables")
    
    expect_is(m0[[1]],"data.frame")
    expect_is(m0[[2]],"data.frame")
    
    expect_equal(names(m0)[1],"titanic_mm")
    expect_equal(names(m0)[2],"titanic2_mm")
    
    
  })
  
  # Check if we try to send the wrong class
  test_that("WrongClass", {
    
    
    
    expect_error(generateMismatchData(dfA = titanic, dfB = titanic2, iris),"Input is not of class: dataCompareRobject")
    
    
  })
  
  test_that("No data", {
    
    compareObject <- rCompare(titanic,titanic2)
    expect_error(generateMismatchData(dfA = NA, dfB = titanic2, compareObject))
    
    
  })
  
} else {
  print('Part of testCreateMismatch.R not run due to missing Titanic library')
}

# Run tests using data with a single match key
test_that("With match key", {

  # Generate mismatches
  bbb <- rCompare(pressure, pressure2, keys = 'temperature')
  ccc <- generateMismatchData(bbb, pressure, pressure2)
    
  # Check object has 2 items and is a list
  expect_equal(length(ccc),2)
  expect_true(is.list(ccc))

  # Both tables have two rows  
  expect_equal(nrow(ccc[[1]]),2)
  expect_equal(nrow(ccc[[2]]),2)
  expect_equal(ncol(ccc[[1]]),2)
  expect_equal(ncol(ccc[[2]]),2)
  
  # The temp col matches in sum
  expect_equal(sum(ccc[[1]][,1]), sum(ccc[[2]][,1]))
  
  # Pressure col does not
  expect_false(any(ccc[[1]][,2] == ccc[[2]][,2]))
  
})

# And finally, with two match keys
test_that("With two match keys", {

  # Generate mismatches
  bbb <- rCompare(pressure3, pressure4, keys = c('temperature','ID2'))
  
  ccc <- generateMismatchData(bbb, pressure4, pressure3)
  
  # Check object has 2 items and is a list
  expect_equal(length(ccc),2)
  expect_true(is.list(ccc))
  
  # Both tables have three rows, columns
  expect_equal(nrow(ccc[[1]]),3)
  expect_equal(nrow(ccc[[2]]),3)
  expect_equal(ncol(ccc[[1]]),3)
  expect_equal(ncol(ccc[[2]]),3)
  
  # The temp col sum matches 
  expect_equal(sum(ccc[[1]][,1]), sum(ccc[[2]][,1]))
  
  # Pressure col does not
  expect_false(any(ccc[[1]][,2] == ccc[[2]][,2]))
  
  # ID2 column does match
  expect_true(all(ccc[[1]][,3] == ccc[[2]][,3]))
  
  
})

# And a quck check if our objects are not dataframes
test_that("With lists and matrices", {
  
  mat <- matrix(data = c(1, 2, 3, 4), nrow = 2)
  mat2 <- matrix(data = c(1, 2, 4, 5), nrow = 2)
  
  lst <- list(1, 2, 3, 4)
  lst2 <- list(1, 2, 4, 4)

  matComp <- rCompare(mat, mat2)
  lstComp <- rCompare(lst, lst2)
  
  # Generate objects
  expect_error(matMism <-  generateMismatchData(x = matComp, dfA = mat, dfB = mat2), NA)
  expect_error(lstMism <- generateMismatchData(x = lstComp, dfA = lst, dfB = lst2), NA)
  
  # lstMism was empty beucase they match
  expect_equal(lstMism, NULL)
  
  # matMism should have two tables
  expect_equal(matMism[[2]][1,2],4)
  expect_equal(matMism[[2]][2,2],5)
  expect_equal(matMism[[2]][1,1],1)
  expect_equal(matMism[[2]][2,1],2)
  
  expect_equal(matMism[[1]][1,2],3)
  expect_equal(matMism[[1]][2,2],4)
  expect_equal(matMism[[1]][1,1],1)
  expect_equal(matMism[[1]][2,1],2)
  

    
  
})
