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
# UNIT TEST: trimCharVars
#
# trimCharVars should remove white space from character columns in data frames
#

context('trimCharVars')

test_that("trimCharVars correctly trims whitespace", {
  
  a <- as.character(c("The fat", "cat sat ", "   on a very   comfy", " mat"))
  b <- as.character(c("   gabba   ", "  boo ", "   gabba ", " boo "))
  c <- c(1,2,3,4)
  df1 <- data.frame(a, b, c, stringsAsFactors = F)
  
  a <- as.character(c("The fat", "cat sat", "on a very   comfy", "mat"))
  b <- as.character(c("gabba", "boo", "gabba", "boo"))
  c <- c(1,2,3,4)
  df2 <- data.frame(a, b, c, stringsAsFactors = F)
  
  
  expect_equal( trimCharVars(df1), df2 )
})