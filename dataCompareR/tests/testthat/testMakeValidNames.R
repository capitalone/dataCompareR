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

# Unit test makeValidNames
# Fixes bad names in data frames and messages the user

context("test makeValidNames")



test_that("makeValidNames function", {

  # Test the function in 3 scenarios
  # 1. No changes
  # 2. invalid name changes
  # 3. repeat name changes
  
  iris2 <- iris
  iris3 <- iris
  
  names(iris2) <- c("`__a horrible name`", "`and   another`", "`___so so bad`", "and", "a_good_one")
  names(iris3) <- c("A", "A", "A", "A", "A")

  expect_silent(a <- makeValidNames(iris))
  expect_message(b <- makeValidNames(iris2), "Fixing syntactically invalid names")
  expect_message(c <- makeValidNames(iris3), "Fixing syntactically invalid names")


  expect_equal(names(a), names(iris))
  expect_equal(names(b), c("X.__a.horrible.name.","X.and...another.","X.___so.so.bad.","and","a_good_one"))
  expect_equal(names(c), c("A" ,  "A.1" ,"A.2" ,"A.3" ,"A.4"))
})

test_that("makeValidKeys function", {
  
  # Test the function in a few scenarios
  
  # normal keys
  a <- "hello"
  expect_equal(makeValidKeys(a), a)
  
  b <- c("hello", "world")
  expect_equal(makeValidKeys(b), b)
  
  c <- c("hello", "world", "wide")
  expect_equal(makeValidKeys(c), c)
  
  # An NA
  d <- NA
  expect_equal(makeValidKeys(d), d)
  
  # Some cleaning
  e <- " hello "
  expect_equal(makeValidKeys(e), "X.hello.")
  
  f <- c(" hello ", " hello ")
  expect_equal(makeValidKeys(f), c("X.hello.", "X.hello..1"))
  
  g <- "_hello "
  expect_equal(makeValidKeys(g), "X_hello.")
  
  h <- c("_hello ", "_hello ")
  expect_equal(makeValidKeys(h), c("X_hello.", "X_hello..1"))
  
  # Should not need this test - keys are checked for validity
  #i <- c("A" , NA)
  #makeValidKeys(i)
  
  # Messages
  # No  message if no cleaning
  expect_silent(makeValidKeys(a))
  # Message if cleaning
  expect_message(makeValidKeys(e))
  
  
})


test_that("makeValidNames function in end to end context", {

  # Not comprehensive - just testing that the name changes work
  
  iris2 <- iris
  iris3 <- iris
  
  
  names(iris2) <- c("`__a horrible name`", "`and   another`", "`___so so bad`", "and111", "a_good_one")
  names(iris3) <- c("A", "A", "A", "A", "A")
  
  print(summary(iris))
  print(summary(iris2))
  
  expect_message(a <- rCompare(iris, iris2), "Fixing syntactically invalid names")
  expect_message(b <- rCompare(iris2, iris2), "Fixing syntactically invalid names")
  
  aa <- capture.output(summary(a))
  bb <- capture.output(summary(b))
  
  expect_equal(aa[34], "No columns match, so no comparison could take place")
  expect_equal(aa[30], "Columns only in iris: Petal.Length, Petal.Width, Sepal.Length, Sepal.Width, Species  ")
  
  # There appear to be reordering in this column in the devtools::check() run vs testing normally
  # so we'll just look for the titles
  expect_true(grepl(pattern =  "Columns only in iris2", x = aa[31], fixed = TRUE))
  expect_true(grepl(pattern =  "a_good_one", x = aa[31], fixed = TRUE))
  expect_true(grepl(pattern =  "and111", x = aa[31], fixed = TRUE))
  expect_true(grepl(pattern =  "X.___so.so.bad.", x = aa[31], fixed = TRUE))
  expect_true(grepl(pattern =  "X.__a.horrible.name.", x = aa[31], fixed = TRUE))
  expect_true(grepl(pattern =  "X.and...another.", x = aa[31], fixed = TRUE))
  
  expect_true(grepl(pattern =  "Columns with all rows equal", x = bb[48], fixed = TRUE))
  expect_true(grepl(pattern =  "A_GOOD_ONE", x = bb[48], fixed = TRUE))
  expect_true(grepl(pattern =  "AND111", x = bb[48], fixed = TRUE))
  expect_true(grepl(pattern =  "X.___SO.SO.BAD.", x = bb[48], fixed = TRUE))
  expect_true(grepl(pattern =  "X.__A.HORRIBLE.NAME.", x = bb[48], fixed = TRUE))
  expect_true(grepl(pattern =  "X.AND...ANOTHER.", x = bb[48], fixed = TRUE))

  
  
})
  
test_that("makeValidKeys function in end to end context", {

  # set up data
  
  aa <- pressure
  bb <- pressure

  names(aa) <- c("  temp" ,"  pressure")
  names(bb) <- c("  temp" ,"  pressure")

  
  # What do we expect..
  
  # Keyless compare should just work
  expect_message(rCompare(aa, bb))
  test1  <- capture.output(rCompare(aa, bb))
  
  expect_equal("All columns were compared, all rows were compared ", test1[1])
  expect_equal("All compared variables match ", test1[2])                     
  expect_equal(" Number of rows compared: 19 ", test1[3])
  expect_equal(" Number of columns compared: 2" , test1[4])
  
  
  # Compare with the original name should work
  expect_message(rCompare(aa, bb, keys = "  temp"))
  test2  <- capture.output(rCompare(aa, bb, keys = "  temp"))
  expect_equal("All columns were compared, all rows were compared ", test2[1])
  expect_equal("All compared variables match ", test2[2])                     
  expect_equal(" Number of rows compared: 19 ", test2[3])
  expect_equal(" Number of columns compared: 2" , test2[4])
  
  # And with the new name that will be set
  expect_message(rCompare(aa, bb, keys = "X..temp"))
  test3  <- capture.output(rCompare(aa, bb, keys = "X..temp"))
  expect_equal("All columns were compared, all rows were compared ", test3[1])
  expect_equal("All compared variables match ", test3[2])                     
  expect_equal(" Number of rows compared: 19 ", test3[3])
  expect_equal(" Number of columns compared: 2" , test3[4])
  
  # But not temp  
  expect_error(rCompare(aa, bb, keys = "temp"))

  # Or with lists inc NA
  # THis error could be improved..
  expect_error(rCompare(aa, bb, keys = c(NA,"temp")))
  
  
})
