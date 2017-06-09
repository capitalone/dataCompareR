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

#
# UNIT TEST: print.dataCompareRobject
# 
# The aim of this test is to check the print output object,
# which has a certain structure based on the content of the dataCompareR output.
# It is not a complete test.
#

# Create a series of data we can use for testing with a single index

context("OutputComparisons : Print")

if(require(titanic)) {
  
  source('createTitanicDatasets.R')

  test_that("comparePrintEqual", {
    
    
    # The object has already pre-determined structure
    
    compareObject <- rCompare(titanic2,titanic2)
    
    # Check the print output with different parameters
    
    p0 <- print(compareObject)
    p1 <- print(compareObject,nObs = 1)
    p2 <- print(compareObject,nObs = 2, nVars = 1)
    p3 <- print(compareObject,verbose = T)
    p4 <- print(compareObject,nObs = 2, nVars = 1,verbose = T)
    
    
    expect_null(p0, info = "Expect a NULL object to be created and a message sent to the console 'All variables match'")
    expect_null(p1, info = "Expect a NULL object to be created and a message sent to the console 'All variables match'")
    expect_null(p2, info = "Expect a NULL object to be created and a message sent to the console 'All variables match'")
    expect_null(p3, info = "Expect a NULL object to be created and a message sent to the console 'All variables match'")
    expect_null(p4, info = "Expect a NULL object to be created and a message sent to the console 'All variables match'")
    
  })
  
  test_that("comparePrintUnEqual", {
    
    
    # The object has already pre-determined structure
    
    b1 <- rCompare(titanic,titanic2, trimChars = F, keys = 'PassengerId')
   
    #Generate print output objects
    
    p0 <- print(b1)
    p1 <- print(b1,nObs = 1)
    p2 <- print(b1,nObs = 2, nVars = 1)
    p3 <- print(b1,verbose = T)
    p4 <- print(b1,nObs = 2, nVars = 1,verbose = T)
    
    #Test output is as expected:
    
    expect_is(p0, "data.frame")
    expect_output(str(p0), "20 obs")
    expect_output(str(p0), "7 variables")
    
    expect_is(p0$PASSENGERID,"character")
    expect_is(p0$valueA, "character")
    expect_is(p0$valueA, "character")
    expect_is(p0$variable, "character")
    expect_is(p0$typeA, "character")
    expect_is(p0$typeB, "character")
    expect_is(p0$diffAB, "character")
    
    expect_equal(p0$PASSENGERID, as.character(c(2,10,20,27,31,867,875,876,880,890,5,1,2,3,4,5,6,7,8,9)))
    expect_equal(p0$variable, c(rep("EMBARKED",10),"HASSURVIVED",rep("NAME",9)))
    expect_equal(p0$typeA, c(rep("character",20)))
    expect_equal(p0$typeB, c(rep("character",20)))
    expect_equal(p0$diffAB, c(rep("",20)))
    
    expect_output(str(p1),"5 obs")
    expect_output(str(p1), "7 variables")
    
    expect_equal(p1$PASSENGERID, as.character(c(2,890,5,1,9)))
    expect_equal(p1$variable, c("EMBARKED", "EMBARKED","HASSURVIVED","NAME", "NAME"))
    
    expect_output(str(p2), "8 obs")
    expect_output(str(p2), "7 variables")
    
    expect_equal(p2$PASSENGERID, as.character(c(2,10,880,890, 1, 2, 8, 9)))
    expect_equal(p2$variable, c(rep("EMBARKED",4),rep("NAME",4)))
    
    expect_output(str(p3),"180 obs")
    expect_output(str(p3), "7 variables")
    
    expect_output(str(p4),"180 obs")
    expect_output(str(p4),"7 variables")
    
    
  })

} else {
  print("Part of OutputComparisons : Print test context not run, due to lack of titanic data")
}

test_that("print rcomp object", {
  
  
  # Create a couple of R compare objects
  testSame <- rCompare(iris,iris)
  
  iris2 <- iris[1:140,]
  iris2[3,3] <- 1.5
  testDiff <- rCompare(iris,iris2)
  
  # Capture the outputs of createReportText as text
  textSame <- capture.output(print(testSame))
  textDiff <- capture.output(print(testDiff))
  
  # For now we won't hard code each - instead, we will just check a few points...
  
  # We should look for the all match label
  expect_that(any(textSame == "All compared variables match "),is_true())
  expect_that(any(textSame == " Number of rows compared: 150 "),is_true())
  expect_that(any(textSame == " Number of columns compared: 5"),is_true())
  
  # Expect they differ
  expect_that(textSame[[1]] == textDiff[[1]], is_false())
  expect_that(textSame[[2]] == textDiff[[2]], is_false())
  expect_that(textSame[[3]] == textDiff[[3]], is_false())
  expect_that(textSame[[4]] == textDiff[[4]], is_false())
  
  # Check that the textDiff has more cols
  expect_that(length(textDiff) > 2,is_true())
  
  
})


test_that("test print rcompobj rows columns dropped messages", {
  
  # We'll use the pressure dataset for comparison
  
  # Make a copy of pressure with missing rows
  pressure2 <- pressure[1:15,]
  
  # Make a copy of pressure with extra cols
  pressure3 <- pressure
  pressure3$randomCol <- 1
  
  # Make a copy with missing rows AND extra cols
  pressure4 <- pressure[1:15,]
  pressure4$randomCol <- 1
  pressure4$randomCol2 <- 3
  
  comp1 <- rCompare(pressure, pressure)
  comp2 <- rCompare(pressure, pressure2)
  comp3 <- rCompare(pressure, pressure3)
  comp4 <- rCompare(pressure, pressure4)
  
  text1 <- capture.output(print(comp1))
  text2 <- capture.output(print(comp2))
  text3 <- capture.output(print(comp3))
  text4 <- capture.output(print(comp4))
  
  # Check we see what we  expect
  expect_that(any(grepl("All columns were compared, all rows were compared", text1, fixed = T)), is_true())
  expect_that(any(grepl("All columns were compared, 4 row(s) were dropped from comparison", text2, fixed = T)), is_true())
  expect_that(any(grepl("1 column(s) were dropped, all rows were compared", text3, fixed = T)), is_true())
  expect_that(any(grepl("2 column(s) were dropped, 4 row(s) were dropped from comparison", text4, fixed = T)), is_true())
  
})



test_that("test print argument validation", {
  aaa <- rCompare(iris,iris)
  

  expect_error(print(aaa,nVars="A"))
  expect_error(print(aaa,nObs="A"))
  expect_error(print(aaa,verbose="A"))
  
  expect_error(print(aaa,nVars=-1))
  expect_error(print(aaa,nObs=-1))
  
})