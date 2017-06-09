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
# SYSTEM TEST: dataCompareR Object
#
# The aim of this test is to check the structure of the output object.
# It is not a complete test.
#

library(testthat)

test_that("compareObject", {

  
  # The object has already pre-determined structure

  compareObject <- rCompare(iris,iris)

  # Test the overall object
  expect_equal( length(compareObject), 6)
  expect_type( compareObject, "list")
  expect_equal( names(compareObject), c("meta", "colMatching", "rowMatching", "cleaninginfo","mismatches","matches") )

   # Test list 'meta'
  expect_type( compareObject[["meta"]], "list")
  expect_equal( names(compareObject[["meta"]]), c("args", "runTimestamp",
                                                "A", "B", "objVersion", "roundDigits"))
  expect_s3_class( compareObject[["meta"]][["runTimestamp"]], "POSIXct")

  # Test list 'colMatching'
  expect_type( compareObject[["colMatching"]], "list")
  expect_equal( names(compareObject[["colMatching"]]), c("inboth", "inA", "inB"))

  # Test list 'rowMatching'
  expect_type( compareObject[["rowMatching"]], "list")
  expect_equal( names(compareObject[["rowMatching"]]), c("matchKeys","inboth", "inA", "inB"))

  # Testothers
  expect_type( compareObject[["cleaninginfo"]], "list")
  expect_type( compareObject[["mismatches"]], "list")
  expect_type( compareObject[["matches"]], "character")

})



