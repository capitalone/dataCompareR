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


test_that("makeValidNames function in end to end context", {

  # Not comprehensive - just testing that the name changes work
  
  iris2 <- iris
  iris3 <- iris
  
  
  names(iris2) <- c("`__a horrible name`", "`and   another`", "`___so so bad`", "and111", "a_good_one")
  names(iris3) <- c("A", "A", "A", "A", "A")
  
  print(summary(iris))
  print(summary(iris2))
  
  writeLines(capture.output(print(iris2)), 'C:/temp/a1.txt')
  
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
  