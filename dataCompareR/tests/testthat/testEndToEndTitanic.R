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

#' @suggests tibble data.frame

#
# SYSTEM TEST: dataCompareR : Single Match Keys
#
# End-to-end system tests of the dataCompareR function that look at 
# different scenarios involving single-key data frame comparisons. 
#

context("Single Match Key Comparisons")

test_that("ComparisonOfEquals", {
  
  if(require(tibble) & require(data.table) & require(titanic)) {
    # Create a series of data we can use for testing with a single index
    source('createTitanicDatasets.R')
    
    # Part 1 - comparing a dataframe to itself should result in a very simple output
    #           and it shouldn't matter what the formats are 
    
    # compare to itself , with or without keys should be the same
    a1 <- rCompare(titanic2, titanic2, trimChars = TRUE) 
    a2 <- rCompare(titanic2, titanic2, keys = 'PassengerId', trimChars = TRUE)
    # or with a different order, and keys
    a3 <- rCompare(titanic2, titanic2shuffle, keys = 'PassengerId', trimChars = TRUE)
    # And the source data format should not matter
    a4 <- rCompare(titanic2, titanic2DataTable, keys = 'PassengerId', trimChars = TRUE)
    a5 <- rCompare(titanic2, titanic2Matrix, keys = 'PassengerId', trimChars = TRUE)
    a6 <- rCompare(titanic2, titanic2Tibble, keys = 'PassengerId', trimChars = TRUE)
  
    #  a1 is direct A:A comparison, so check that the results are expected
    
    expect_that(length(a1$colMatching$inboth), equals(ncol(titanic2)) )
    expect_that(length(a1$colMatching$inA), equals(0) )
    expect_that(length(a1$colMatching$inB), equals(0) )
    
    expect_that(length(a1$rowMatching$inboth), equals(nrow(titanic2)) )
    expect_that(a1$rowMatching$inA[[1]], equals(integer(0)) )
    expect_that(a1$rowMatching$inB[[1]], equals(integer(0)) )
    
    
    expect_that(length(a1$mismatches), equals(0) )
    expect_that(length(a1$matches), equals(ncol(titanic2) ))
    
    expect_that(all.equal(a1$meta$A,a1$meta$B), equals(TRUE))
    
    # a2 should match on matching details, but the object will differ a little due
    # to a2 using a match key
    
    expect_that(all.equal(a1$colMatching,a2$colMatching),equals(TRUE))
    # Cannot compare row matching, details are different
    expect_that(all.equal(a1$cleaninginfo,a2$cleaninginfo), equals(TRUE))
    expect_that(all.equal(a1$mismatches,a2$mismatches), equals(TRUE))
    # Matches will exclude the key column, so should be out by 1
    expect_that(length(a1$matches) - length(a2$matches), equals(1))
    
    # a3 should match a2 on important details
    expect_that(a2$meta$A, equals(a3$meta$A))
    #expect_that(a2$meta$B, equals(a3$meta$B)) NO! Names differ
    expect_that(a2$colMatching, equals(a3$colMatching))
    expect_that(a2$rowMatching, equals(a3$rowMatching))
    expect_that(a2$cleaninginfo, equals(a3$cleaninginfo))
    expect_that(a2$matches, equals(a3$matches))
    expect_that(a2$mismatches, equals(a3$mismatches))
   
    # a4 should all match a2 too
    expect_that(a2$meta$A, equals(a4$meta$A))
    expect_that(a2$colMatching, equals(a4$colMatching))
    expect_that(a2$rowMatching, equals(a4$rowMatching))
    expect_that(a2$cleaninginfo, equals(a4$cleaninginfo))
    expect_that(a2$matches, equals(a4$matches))
    expect_that(a2$mismatches, equals(a4$mismatches))
    
    # a5 should match a2 for row/col matching, but values are different due to 
    # factor > char conversion
    expect_that(a2$meta$A, equals(a5$meta$A))
    expect_that(a2$colMatching, equals(a5$colMatching))
    expect_that(a2$rowMatching, equals(a5$rowMatching))
    
    # a6 should match a2 too
    expect_that(a2$meta$A, equals(a6$meta$A))
    expect_that(a2$colMatching, equals(a6$colMatching))
    expect_that(a2$rowMatching, equals(a6$rowMatching))
    expect_that(a2$cleaninginfo, equals(a6$cleaninginfo))
    expect_that(a2$matches, equals(a6$matches))
    expect_that(a2$mismatches, equals(a6$mismatches))
  }
  
})


test_that("ComparisonOfUnequals", {

  if(require(tibble) & require(data.table) & require(titanic)) {
    # Create a series of data we can use for testing with a single index
    source('createTitanicDatasets.R')
    
    b1 <- rCompare(titanic,titanic2, trimChars = FALSE, keys = 'PassengerId')
    b1a <- rCompare(titanic,titanic2,trimChars = TRUE, keys = 'PassengerId')
    b1b <- rCompare(titanic,titanic2,trimChars = TRUE)
    
    b2 <- rCompare(titanic,titanic2, keys = 'PassengerId',trimChars = FALSE)
    b3 <- rCompare(titanic, titanic2shuffle, keys = 'PassengerId',trimChars = FALSE)
    b4 <- rCompare(titanic,titanic2DataTable, keys = 'PassengerId',trimChars = FALSE)
    b5 <- rCompare(titanic,titanic2Matrix, keys = 'PassengerId',trimChars = FALSE)
    b6 <- rCompare(titanic,titanic2Tibble, keys = 'PassengerId',trimChars = FALSE)
  
    # Part 1 - determine that b1 looks correct
    # Based on
    # 
    #* `Embarked` has different order of levels, this results in different rows.
    #* `Cabin` has additional levels, this however does not result in different rows (they are the same via `as.character`)
    #* `HasSurvived` has additional level: NaN. Line 5 has HasSurvived as NaN, but in the basel dataset it is NA.
    #
    #The rest of the differences are demonstrated on the `Name` column:
    #  
    # * Row 1: very long string, difference at the end 
    #* Row 2: additional spaces at the beginning
    #* Row 3: additional spaces at the end
    #* Row 4: additional spaces at the beginning and at the end
    #* Row 5: Na vs NaN
    #* Row 6: Inserted new line instead of spaces
    #* Row 7: name in lowercase
    #* Row 8: name in uppercase
    #* Row 9: special characters included
    
    # Check b1 matches all columns
    expect_that(length(b1$colMatching$inboth), equals(ncol(titanic2)) )
    expect_that(length(b1$colMatching$inA), equals(0) )
    expect_that(length(b1$colMatching$inB), equals(0) )
    
    # Check b1 matches all columns 
    expect_that(length(b1$rowMatching$inboth$PASSENGERID), equals(nrow(titanic2)) )
    expect_that(b1$rowMatching$inA[[1]], equals(logical(0)) )
    expect_that(b1$rowMatching$inB[[1]], equals(logical(0)) )
    
    # Check that b1 has mismatches - not trimming chars so expect 6 issues
    expect_true(length(b1$mismatches) >0)
    # 3 fields mismatch
    expect_true(length(b1$mismatches) == 3)
    expect_that(names(b1$mismatches)[1], equals("EMBARKED"))
    expect_that(names(b1$mismatches)[2], equals("HASSURVIVED"))
    expect_that(names(b1$mismatches)[3], equals("NAME"))
    
    # EMBARKED should mismatch
    # Should have 6 columns
    expect_true(length(b1$mismatches$EMBARKED) == 7)
    # I am not working this out - assume that 170 is correct and make sure it holds later
    expect_true(length(b1$mismatches$EMBARKED[[1]]) == 170)
    
    # CABIN should not have mismatches
    expect_that(which(names(b1$mismatches) == 'CABIN'),equals(integer(0)) )
    
    # HasSurvived should have 1 mismatch
    # but first, it should have 6 columns
    expect_true(length(b1$mismatches$HASSURVIVED) == 7)
    # and 1 row
    expect_true(length(b1$mismatches$HASSURVIVED[[1]]) == 1)
    # And they're NAN and NA
    expect_true(is.na(b1$mismatches$HASSURVIVED$valueA[1]))
    expect_true(as.character(b1$mismatches$HASSURVIVED$valueB[1]) == "NaN")
    
    # NAME should have 9 rows of mismatches
    # And of course, 6 cols
    expect_true(length(b1$mismatches$NAME) == 7)
    expect_true(length(b1$mismatches$NAME[[1]]) == 9)
    
    # b1a should be the same, apart from the mismatches, which should only reveal 6 in total
    expect_true(all.equal(b1$matches,b1a$matches))
    expect_true(all.equal(b1$rowMatching,b1a$rowMatching))
    expect_true(all.equal(b1$colMatching,b1a$colMatching))
    expect_true(all.equal(b1$cleaninginfo,b1a$cleaninginfo))
    
    expect_true(all.equal(b1$mismatches$EMBARKED,b1a$mismatches$EMBARKED))
    expect_true(all.equal(b1$mismatches$HASSURVIVED,b1a$mismatches$HASSURVIVED))
    expect_true(all.equal(b1$mismatches$CABIN,b1a$mismatches$CABIN))
    expect_false(length(b1$mismatches$NAME[[1]]) == length(b1a$mismatches$NAME[[1]]))
    expect_true(length(b1a$mismatches$NAME[[1]]) == 6)
    
    
    # b1b should match b1a in terms of content, but differs in format, so check it finds same number of name changes
    expect_true(all.equal(b1b$colMatching,b1a$colMatching))
    expect_true(all.equal(b1b$cleaninginfo,b1a$cleaninginfo))
    expect_true(length(b1b$mismatches$NAME[[1]]) == 6)
    
    
    # 1 diff, due to lack of a match key
    expect_true(length(b1b$matches)- length(b1a$matches) == 1)
    expect_true(length(b1b$rowMatching$inboth) == length(b1a$rowMatching$inboth$PASSENGERID))
    expect_true(length(b1b$mismatches) == length(b1a$mismatches))
    
      # Part 2 - determine that b2-b6 match b1 as far as expected
  
    # b2 should match b1
    expect_that(all.equal(b1$colMatching,b2$colMatching),equals(T))
    expect_that(all.equal(b1$rowMatching,b2$rowMatching),equals(T))
    expect_that(all.equal(b1$mismatches,b2$mismatches),equals(T))
    expect_that(all.equal(b1$matches,b2$matches),equals(T))
    expect_that(all.equal(b1$cleaninginfo,b2$cleaninginfo),equals(T))
    
    # b2 should match b1
    expect_that(all.equal(b1$colMatching,b3$colMatching),equals(T))
    expect_that(all.equal(b1$rowMatching,b3$rowMatching),equals(T))
    expect_that(all.equal(b1$mismatches,b3$mismatches),equals(T))
    expect_that(all.equal(b1$matches,b3$matches),equals(T))
    expect_that(all.equal(b1$cleaninginfo,b3$cleaninginfo),equals(T))
    
    
    # b4 should all match b1 too
    expect_that(all.equal(b1$colMatching,b4$colMatching),equals(T))
    expect_that(all.equal(b1$rowMatching,b4$rowMatching),equals(T))
    expect_that(all.equal(b1$mismatches,b4$mismatches),equals(T))
    expect_that(all.equal(b1$matches,b4$matches),equals(T))
    expect_that(all.equal(b1$cleaninginfo,b4$cleaninginfo),equals(T))
    
    # b5 should match b1 for row/col matching, but values are different due to 
    # factor > char conversion
    expect_that(all.equal(b1$colMatching,b5$colMatching),equals(T))
    # The rest is hard to work on, don't think its worth coding for
    
    # b6 should match b1 too
    expect_that(all.equal(b1$colMatching,b6$colMatching),equals(T))
    expect_that(all.equal(b1$rowMatching,b6$rowMatching),equals(T))
    expect_that(all.equal(b1$mismatches,b6$mismatches),equals(T))
    expect_that(all.equal(b1$matches,b6$matches),equals(T))
    expect_that(all.equal(b1$cleaninginfo,b6$cleaninginfo),equals(T))
  }
  
})


test_that("ComparisonWithMissingRows", {
  if(require(tibble) & require(data.table) & require(titanic)) {
    # Create a series of data we can use for testing with a single index
    source('createTitanicDatasets.R')
    
    # Create a subset of the dataset to test for missing rows
    titanic3 <- titanic2[1:800,]
    
    c1 <- rCompare(titanic,titanic3, trimChars = TRUE, keys = 'PassengerId')
    c2 <- rCompare(titanic2,titanic3, trimChars = TRUE, keys = 'PassengerId')
    c3 <- rCompare(titanic3,titanic2, trimChars = TRUE, keys = 'PassengerId')
    
    
    # Both should match on all columns
    expect_that(length(c1$colMatching$inboth), equals(ncol(titanic2)) )
    expect_that(length(c1$colMatching$inA), equals(0) )
    expect_that(length(c1$colMatching$inB), equals(0) )
    
    expect_that(length(c2$colMatching$inboth), equals(ncol(titanic2)) )
    expect_that(length(c2$colMatching$inA), equals(0) )
    expect_that(length(c2$colMatching$inB), equals(0) )
    
    expect_that(length(c3$colMatching$inboth), equals(ncol(titanic2)) )
    expect_that(length(c3$colMatching$inA), equals(0) )
    expect_that(length(c3$colMatching$inB), equals(0) )
    
    
    # Row matching - expect to miss out on 91 rows
    expect_true(length(c1$rowMatching$inboth$PASSENGERID) == 800)
    expect_true(length(c1$rowMatching$inA$PASSENGERID) == 91)
    expect_true(length(c1$rowMatching$inB$PASSENGERID) == 0)
    
    expect_true(length(c2$rowMatching$inboth$PASSENGERID) == 800)
    expect_true(length(c2$rowMatching$inA$PASSENGERID) == 91)
    expect_true(length(c2$rowMatching$inB$PASSENGERID) == 0)
    
    expect_true(length(c3$rowMatching$inboth$PASSENGERID) == 800)
    expect_true(length(c3$rowMatching$inA$PASSENGERID) == 0)
    expect_true(length(c3$rowMatching$inB$PASSENGERID) == 91)
    
    # Can't do much about if the mismatches work, but, we can check equality where we can
    # Col matching should be the same
    expect_that(all.equal(c1$colMatching,c2$colMatching),equals(TRUE))
    expect_that(all.equal(c3$colMatching,c2$colMatching),equals(TRUE))
  
    # Same # mismatches in 2/3 (which is to say, 0)
    expect_that(length(c3$mismatches) == length(c2$mismatches),equals(TRUE))
    
    # Could do more here, but won't
  }
})


test_that("ComparisonWithMissingCols", {
  
  if(require(tibble) & require(data.table) & require(titanic)) {
    # Create a series of data we can use for testing with a single index
    source('createTitanicDatasets.R')
    
    # Create a subset of the dataset to test for missing rows
    titanic3 <- titanic2[,1:10]
    
    d1 <- rCompare(titanic3,titanic2, trimChars = TRUE, keys = 'PassengerId')
    d2 <- rCompare(titanic2,titanic3, trimChars = TRUE, keys = 'PassengerId')
    
    # Again, we'll only check a few of the easier aspects
    
    # Check we match 10 cols, mismatch 3 in each case
    expect_that(length(d1$colMatching$inboth), equals(10) )
    expect_that(length(d1$colMatching$inA), equals(0) )
    expect_that(length(d1$colMatching$inB), equals(3) )
    
    expect_that(length(d2$colMatching$inboth), equals(10) )
    expect_that(length(d2$colMatching$inA), equals(3) )
    expect_that(length(d2$colMatching$inB), equals(0) )
    
    expect_that(d1$rowMatching, equals(d2$rowMatching) )
    expect_that(length(d1$rowMatching$inboth$PASSENGERID), equals(891) )
    expect_that(length(d1$rowMatching$inA$PASSENGERID), equals(0) )
    expect_that(length(d1$rowMatching$inB$PASSENGERID), equals(0) )
    
    expect_that(d1$matches, equals(d2$matches) )
  }
  })
  
  test_that("TestRoundingEndToEnd", {
    
    if(require(titanic)) {
      # Create a series of data we can use for testing with a single index
      source('createTitanicDatasets.R')
      
      titanicShort <- select(titanic, PassengerId, Age, Fare)
      
      # Expect no error
      expect_error(roundedCompare <- rCompare(titanicShort, titanicShort, keys = 'PassengerId', roundDigits = 0), NA)
      
      titanicShort2 <- titanicShort
      titanicShort2$Age <- titanicShort2$Age + 0.00001
      
      # Run a test where we get rounding errors
      
      # Expect no error
      expect_error(roundedCompare2 <- rCompare(titanicShort, titanicShort2, keys = 'PassengerId', roundDigits = NA), NA)
      
      # Expect all rows to error
      expect_true(nrow(roundedCompare2$mismatches$AGE) == nrow(filter(titanicShort, !is.na(Age))))
      # Expect only noise to remain
      expect_true(all(abs(roundedCompare2$mismatches$AGE$diffAB+1e-05) < 1E-14))
      
      
      # And another where we don't
      
      # Expect no error
      expect_error(roundedCompare3 <- rCompare(titanicShort, titanicShort2, keys = 'PassengerId', roundDigits = 1), NA)
      
      # Expect no mismatches
      expect_true(length(roundedCompare3$mismatches) == 0)
      expect_true(length(roundedCompare3$matches)==2)
    }
  })
