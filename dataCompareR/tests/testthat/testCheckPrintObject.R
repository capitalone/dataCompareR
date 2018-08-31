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
    p3 <- print(compareObject,verbose = TRUE)
    p4 <- print(compareObject,nObs = 2, nVars = 1,verbose = TRUE)
    
    
    expect_null(p0, info = "Expect a NULL object to be created and a message sent to the console 'All variables match'")
    expect_null(p1, info = "Expect a NULL object to be created and a message sent to the console 'All variables match'")
    expect_null(p2, info = "Expect a NULL object to be created and a message sent to the console 'All variables match'")
    expect_null(p3, info = "Expect a NULL object to be created and a message sent to the console 'All variables match'")
    expect_null(p4, info = "Expect a NULL object to be created and a message sent to the console 'All variables match'")
    
  })
  
  test_that("comparePrintUnEqual", {
    
    
    # The object has already pre-determined structure
    
    b1 <- rCompare(titanic,titanic2, trimChars = FALSE, keys = 'PassengerId')
   
    #Generate print output objects
    
    p0 <- print(b1)
    p1 <- print(b1,nObs = 1)
    p2 <- print(b1,nObs = 2, nVars = 1)
    p3 <- print(b1,verbose = TRUE)
    p4 <- print(b1,nObs = 2, nVars = 1,verbose = TRUE)
    
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
  expect_that(any(grepl("All columns were compared, all rows were compared", text1, fixed = TRUE)), is_true())
  expect_that(any(grepl("All columns were compared, 4 row(s) were dropped from comparison", text2, fixed = TRUE)), is_true())
  expect_that(any(grepl("1 column(s) were dropped, all rows were compared", text3, fixed = TRUE)), is_true())
  expect_that(any(grepl("2 column(s) were dropped, 4 row(s) were dropped from comparison", text4, fixed = TRUE)), is_true())
  
})



test_that("test print argument validation", {
  aaa <- rCompare(iris,iris)
  

  expect_error(print(aaa,nVars="A"))
  expect_error(print(aaa,nObs="A"))
  expect_error(print(aaa,verbose="A"))
  
  expect_error(print(aaa,nVars=-1))
  expect_error(print(aaa,nObs=-1))
  
})


test_that("test print with two empty data frames", {
  
  # We'll use the pressure dataset for comparison
  
  # Make a copy of pressure with missing rows
  df_empty <-  data.frame(ColA = character(),
                                    ColB = as.Date(character()),
                                    ColC = character(),
                                    stringsAsFactors = FALSE)
  
  
  comp1 <- rCompare(df_empty, df_empty)
  comp2 <- rCompare(df_empty, df_empty,  keys = "ColA")
  comp3 <- rCompare(df_empty, df_empty, keys = c("ColA","ColB"))
  comp4 <- rCompare(df_empty, df_empty, keys = c("ColA","ColB","ColC"))
  
  
  text1 <- capture.output(print(comp1))
  text2 <- capture.output(print(comp2))
  text3 <- capture.output(print(comp3))
  text4 <- capture.output(print(comp4))
  
  expect_true(length(text1)==2)
  expect_that(any(grepl("All columns were compared,  no rows compared because at least one table has no rows", text1, fixed = TRUE)), is_true())
  expect_that(any(grepl("No variables match", text1, fixed = TRUE)), is_true())
  
  expect_true(length(text2)==2)
  expect_that(any(grepl("All columns were compared,  no rows compared because at least one table has no rows", text2, fixed = TRUE)), is_true())
  expect_that(any(grepl("No variables match", text2, fixed = TRUE)), is_true())
  
  expect_true(length(text3)==2)
  expect_that(any(grepl("All columns were compared,  no rows compared because at least one table has no rows", text3, fixed = TRUE)), is_true())
  expect_that(any(grepl("No variables match", text3, fixed = TRUE)), is_true())
  
  expect_true(length(text4)==2)
  expect_that(any(grepl("All columns were compared,  no rows compared because at least one table has no rows", text4, fixed = TRUE)), is_true())
  expect_that(any(grepl("No variables match", text4, fixed = TRUE)), is_true())
  
})




test_that("test print with one empty data frames", {
  
  # We'll make two test data frames - one with empty rows, one populated
  
  df_empty <-  data.frame(ColA = character(),
                          ColB = as.Date(character()),
                          ColC = character(),
                          stringsAsFactors = FALSE)
  
  
  df_not_empty <- data.frame(ColA = c("A","B"),
                             ColB = c(Sys.Date(), Sys.Date()),
                             ColC = c(1,1),
                         stringsAsFactors = FALSE)
  
  
  # Run a set of comparisons on them
  comp1 <- rCompare(df_empty, df_not_empty)
  comp2 <- rCompare(df_empty, df_not_empty,  keys = "ColA")
  comp3 <- rCompare(df_empty, df_not_empty, keys = c("ColA","ColB"))
  comp4 <- rCompare(df_empty, df_not_empty, keys = c("ColA","ColB","ColC"))
  
  
  comp5 <- rCompare(df_not_empty, df_empty)
  comp6 <- rCompare(df_not_empty, df_empty,  keys = "ColA")
  comp7 <- rCompare(df_not_empty, df_empty, keys = c("ColA","ColB"))
  comp8 <- rCompare(df_not_empty, df_empty, keys = c("ColA","ColB","ColC"))
  
  text1 <- capture.output(print(comp1))
  text2 <- capture.output(print(comp2))
  text3 <- capture.output(print(comp3))
  text4 <- capture.output(print(comp4))
  
  text5 <- capture.output(print(comp5))
  text6 <- capture.output(print(comp6))
  text7 <- capture.output(print(comp7))
  text8 <- capture.output(print(comp8))
  
  expect_true(length(text1)==2)
  expect_that(any(grepl("All columns were compared,  no rows compared because at least one table has no rows", text1, fixed = TRUE)), is_true())
  expect_that(any(grepl("No variables match", text1, fixed = TRUE)), is_true())
  
  expect_true(length(text2)==2)
  expect_that(any(grepl("All columns were compared,  no rows compared because at least one table has no rows", text2, fixed = TRUE)), is_true())
  expect_that(any(grepl("No variables match", text2, fixed = TRUE)), is_true())
  
  expect_true(length(text3)==2)
  expect_that(any(grepl("All columns were compared,  no rows compared because at least one table has no rows", text3, fixed = TRUE)), is_true())
  expect_that(any(grepl("No variables match", text3, fixed = TRUE)), is_true())
  
  expect_true(length(text4)==2)
  expect_that(any(grepl("All columns were compared,  no rows compared because at least one table has no rows", text4, fixed = TRUE)), is_true())
  expect_that(any(grepl("No variables match", text4, fixed = TRUE)), is_true())
  
  expect_true(length(text5)==2)
  expect_that(any(grepl("All columns were compared,  no rows compared because at least one table has no rows", text5, fixed = TRUE)), is_true())
  expect_that(any(grepl("No variables match", text5, fixed = TRUE)), is_true())
  
  expect_true(length(text6)==2)
  expect_that(any(grepl("All columns were compared,  no rows compared because at least one table has no rows", text6, fixed = TRUE)), is_true())
  expect_that(any(grepl("No variables match", text6, fixed = TRUE)), is_true())
  
  expect_true(length(text7)==2)
  expect_that(any(grepl("All columns were compared,  no rows compared because at least one table has no rows", text7, fixed = TRUE)), is_true())
  expect_that(any(grepl("No variables match", text7, fixed = TRUE)), is_true())
  
  expect_true(length(text8)==2)
  expect_that(any(grepl("All columns were compared,  no rows compared because at least one table has no rows", text8, fixed = TRUE)), is_true())
  expect_that(any(grepl("No variables match", text8, fixed = TRUE)), is_true())
  
  })
