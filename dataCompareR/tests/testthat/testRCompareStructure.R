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
# INTEGRATION TESTS
#
# These tests check that the dataCompareR object has the structure we expect.
# 

# loading testing library
library(testthat)

context('check dataCompareR Structural requirements')

test_that("dataCompareR object meets requirements in row matching", {
  
  
  # To create the output, we make some very specific assumptions about the structure of the dataCompareR object
  # and how rows in the row matching section are labelled. This is fine, but its not very intuitive
  # To guard against unintended changes, this tests checks that the rowmatching section is working as 
  # expected for all 3 cases of no key, 1 key and > 1 key
  
  
  # Create datasets for no key, 1 key, >1 key matching
  
  pressure2 <- pressure[5:15,]
  aaa <- rCompare(pressure2,pressure, keys = 'temperature')
  
  iris2 <- iris[1:140,]
  bbb <- rCompare(iris,iris2)
  
  pressure3 <- pressure
  pressure4 <- pressure2
  pressure3$NEWKEY <- '1'
  pressure4$NEWKEY <- '1'
  ccc <- rCompare(pressure4,pressure3, keys = c('temperature','NEWKEY'))
  
  # 1 Key
  expect_equal(sum(sapply(aaa$rowMatching$inB,length)) / length(aaa$rowMatching$inB), 8)
  expect_equal(sum(sapply(aaa$rowMatching$inA,length)) / length(aaa$rowMatching$inA), 0)
  
  # No Key
  expect_equal(sum(sapply(bbb$rowMatching$inB,length)) / length(bbb$rowMatching$inB),0)
  expect_equal(sum(sapply(bbb$rowMatching$inA,length)) / length(bbb$rowMatching$inA), 10)
  
  # >1 Key
  expect_equal(sum(sapply(ccc$rowMatching$inB,length)) / length(ccc$rowMatching$inB),8)
  expect_equal(sum(sapply(ccc$rowMatching$inA,length)) / length(ccc$rowMatching$inA),0)
  
})


test_that("dataCompareR object when both datasets have no rows", {
  
  df_empty <-  data.frame(ColA = character(),
                          ColB = as.Date(character()),
                          ColC = character(),
                          stringsAsFactors = FALSE)
  
  
  comp1 <- rCompare(df_empty, df_empty)
  comp2 <- rCompare(df_empty, df_empty,  keys = "ColA")
  comp3 <- rCompare(df_empty, df_empty, keys = c("ColA","ColB"))
  comp4 <- rCompare(df_empty, df_empty, keys = c("ColA","ColB","ColC"))

  # In all cases, cols should match  
  expect_true(length(comp1$colMatching$inboth)==3)
  expect_true(length(comp2$colMatching$inboth)==3)
  expect_true(length(comp3$colMatching$inboth)==3)
  expect_true(length(comp4$colMatching$inboth)==3)
  
  # No matching row - have to live with the fact this is int or data.frame
  expect_true(comp1$rowMatching$inboth==0)
  expect_true(nrow(comp2$rowMatching$inboth)==0)
  expect_true(nrow(comp3$rowMatching$inboth)==0)
  expect_true(nrow(comp4$rowMatching$inboth)==0)
  
  # no entires in the in either inA or inB entries
  expect_true(length(comp1$rowMatching$inA$indices_removed)==0)
  expect_true(length(comp2$rowMatching$inA$COLA)==0)
  expect_true(length(comp3$rowMatching$inA$COLA)==0)
  expect_true(length(comp3$rowMatching$inA$COLB)==0)
  expect_true(length(comp4$rowMatching$inA$COLA)==0)
  expect_true(length(comp4$rowMatching$inA$COLB)==0)
  expect_true(length(comp4$rowMatching$inA$COLC)==0)
  
  expect_true(length(comp1$rowMatching$inB$indices_removed)==0)
  expect_true(length(comp2$rowMatching$inB$COLA)==0)
  expect_true(length(comp3$rowMatching$inB$COLA)==0)
  expect_true(length(comp3$rowMatching$inB$COLB)==0)
  expect_true(length(comp4$rowMatching$inB$COLA)==0)
  expect_true(length(comp4$rowMatching$inB$COLB)==0)
  expect_true(length(comp4$rowMatching$inB$COLC)==0)
  
  
})



test_that("dataCompareR object when one datasets has no rows", {
  
  df_empty <-  data.frame(ColA = character(),
                          ColB = as.Date(character()),
                          ColC = character(),
                          stringsAsFactors = FALSE)
  
  df_not_empty <- data.frame(ColA = c("A","B"),
                             ColB = c(Sys.Date(), Sys.Date()),
                             ColC = c(1,1),
                             stringsAsFactors = FALSE)
  
  
  comp1 <- rCompare(df_empty, df_not_empty)
  comp2 <- rCompare(df_empty, df_not_empty,  keys = "ColA")
  comp3 <- rCompare(df_empty, df_not_empty, keys = c("ColA","ColB"))
  comp4 <- rCompare(df_empty, df_not_empty, keys = c("ColA","ColB","ColC"))
  
  
  comp5 <- rCompare(df_not_empty, df_empty)
  comp6 <- rCompare(df_not_empty, df_empty,  keys = "ColA")
  comp7 <- rCompare(df_not_empty, df_empty, keys = c("ColA","ColB"))
  comp8 <- rCompare(df_not_empty, df_empty, keys = c("ColA","ColB","ColC"))
  
  # In all cases, cols should match  
  expect_true(length(comp1$colMatching$inboth)==3)
  expect_true(length(comp2$colMatching$inboth)==3)
  expect_true(length(comp3$colMatching$inboth)==3)
  expect_true(length(comp4$colMatching$inboth)==3)
  expect_true(length(comp5$colMatching$inboth)==3)
  expect_true(length(comp6$colMatching$inboth)==3)
  expect_true(length(comp7$colMatching$inboth)==3)
  expect_true(length(comp8$colMatching$inboth)==3)
  
  # No matching row - have to live with the fact this is int or data.frame
  expect_true(comp1$rowMatching$inboth==0)
  expect_true(nrow(comp2$rowMatching$inboth)==0)
  expect_true(nrow(comp3$rowMatching$inboth)==0)
  expect_true(nrow(comp4$rowMatching$inboth)==0)
  expect_true(comp5$rowMatching$inboth==0)
  expect_true(nrow(comp6$rowMatching$inboth)==0)
  expect_true(nrow(comp7$rowMatching$inboth)==0)
  expect_true(nrow(comp8$rowMatching$inboth)==0)

  # in either inA or inB entries are as expected
  expect_true(length(comp1$rowMatching$inA$indices_removed)==0)
  expect_true(length(comp1$rowMatching$inB$indices_removed)==2)
  
  expect_true(length(comp2$rowMatching$inA$COLA)==0)
  expect_true(length(comp2$rowMatching$inB$COLA)==2)
  
  expect_true(length(comp3$rowMatching$inA$COLA)==0)
  expect_true(length(comp3$rowMatching$inA$COLB)==0)
  expect_true(length(comp3$rowMatching$inB$COLA)==2)
  expect_true(length(comp3$rowMatching$inB$COLB)==2)
  
  expect_true(length(comp4$rowMatching$inA$COLA)==0)
  expect_true(length(comp4$rowMatching$inA$COLB)==0)
  expect_true(length(comp4$rowMatching$inA$COLC)==0)
  expect_true(length(comp4$rowMatching$inB$COLA)==2)
  expect_true(length(comp4$rowMatching$inB$COLB)==2)
  expect_true(length(comp4$rowMatching$inB$COLC)==2)
  
  
  expect_true(length(comp5$rowMatching$inB$indices_removed)==0)
  expect_true(length(comp5$rowMatching$inA$indices_removed)==2)
  
  expect_true(length(comp6$rowMatching$inB$COLA)==0)
  expect_true(length(comp6$rowMatching$inA$COLA)==2)
  
  expect_true(length(comp7$rowMatching$inB$COLA)==0)
  expect_true(length(comp7$rowMatching$inB$COLB)==0)
  expect_true(length(comp7$rowMatching$inA$COLA)==2)
  expect_true(length(comp7$rowMatching$inA$COLB)==2)
  
  expect_true(length(comp8$rowMatching$inB$COLA)==0)
  expect_true(length(comp8$rowMatching$inB$COLB)==0)
  expect_true(length(comp8$rowMatching$inB$COLC)==0)
  expect_true(length(comp8$rowMatching$inA$COLA)==2)
  expect_true(length(comp8$rowMatching$inA$COLB)==2)
  expect_true(length(comp8$rowMatching$inA$COLC)==2)
  
})