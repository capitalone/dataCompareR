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
# Functions that generate sample data frames for tests
#


createMatchoingTwoIndiceData <- function(sampleSize) {

  # Create Datasets to be joined
  colorSample <- c('red', 'green', 'blue', 'yellow', 'orange', 'purple')
  set.seed(sampleSize)
  color <- sample(colorSample, sampleSize, replace = TRUE)
  number <- sample(1:ceiling(`^`(sampleSize, 0.8)), sampleSize, replace = TRUE)
  valueA <- runif(sampleSize, 0, 1)
  dfA = data.frame(color, number, valueA)
  colnames(dfA) = c('color', 'number', 'valueA')
  
  # dfB is just a shuffled version of dfA
  dfB <- dfA[sample(sampleSize),]
  
  return(list(dfA,dfB))
}

createMatchingFourIndiceData <- function(sampleSize) {
  
  # Create Datasets to be joined
  colorSample <- c('red', 'green', 'blue', 'yellow', 'orange', 'purple')
  set.seed(sampleSize)
  color <- sample(colorSample, sampleSize, replace = TRUE)
  color2 <- sample(colorSample, sampleSize, replace = TRUE)
  number <- sample(1:ceiling(`^`(sampleSize, 0.8)), sampleSize, replace = TRUE)
  number2 <- sample(1:ceiling(`^`(sampleSize, 0.8)), sampleSize, replace = TRUE)
  valueA <- runif(sampleSize, 0, 1)
  dfA = data.frame(color, number,color2,number2, valueA)
  colnames(dfA) = c('color', 'number','color2','number2', 'valueA')
  
  # dfB is just a shuffled version of dfA
  dfB <- dfA[sample(sampleSize),]
  
  return(list(dfA,dfB))
}


createDateDataset <- function() {
  
  # Create Datasets to be joined
  colorSample <- c('red', 'green', 'blue', 'yellow', 'orange', 'purple')
  set.seed(5)
  color <- sample(colorSample, 5, replace = FALSE)
  dateA <- Sys.Date() + sort(sample(1:10, 5))
  dateB <- Sys.Date() + sort(sample(1:10, 5))
  
  dfA = data.frame(color, dateA,dateB)
  colnames(dfA) = c('color', 'dateA','dateB')
  
  # dfB is just a shuffled version of dfA
  dfB <- dfA[sample(5),]
  
  # Resample dateB
  dfB$dateB <- Sys.Date() + sort(sample(11:20, 5))
  
  return(list(dfA,dfB))
}


createTimestampDataset <- function() {
  
  # Create Datasets to be joined
  colorSample <- c('red', 'green', 'blue', 'yellow', 'orange', 'purple')
  set.seed(5)
  color <- sample(colorSample, 5, replace = FALSE)
  dateA <- Sys.time() + sort(sample(1:10, 5))
  dateB <- Sys.time() + sort(sample(1:10, 5))
  
  dfA = data.frame(color, dateA,dateB)
  colnames(dfA) = c('color', 'dateA','dateB')
  
  # dfB is just a shuffled version of dfA
  dfB <- dfA[sample(5),]
  
  # Resample dateB
  dfB$dateB <- Sys.time() + sort(sample(11:20, 5))
  
  return(list(dfA,dfB))
}

createBit64Dataset <- function() {
  library(bit64)
  
  # Create Datasets to be joined
  colorSample <- c('red', 'green', 'blue', 'yellow', 'orange', 'purple')
  set.seed(5)
  color <- sample(colorSample, 5, replace = FALSE)
  bigNumber <- as.integer64(sample(1E3:1E4,5))
  
  dfA = data.frame(color, bigNumber)
  colnames(dfA) = c('color', 'bigNumber')
  
  # dfB is just a shuffled version of dfA
  dfB <- dfA[sample(5),]
  
  return(list(dfA,dfB))
}

createOrderedFactor <- function() {
  
  # Create Datasets to be joined
  colorSample <- c('red', 'green', 'blue', 'yellow', 'orange', 'purple')
  set.seed(5)
  color <- sample(colorSample, 5, replace = FALSE)
  color2 <- sample(colorSample, 5, replace = FALSE)
  
  dfA = data.frame(color, color2)
  colnames(dfA) = c('color', 'color2')
  dfA$color2 <- factor(dfA$color2, levels=c('red', 'green', 'blue', 'yellow', 'orange', 'purple'), ordered=TRUE)
  dfA$color3 <- dfA$color2
  
  
  # dfB is just a shuffled version of dfA
  dfB <- dfA[sample(5),]
  
  dfA$color3[1] <- 'red'
  dfA$color3[2] <- 'red'
  dfA$color3[3] <- 'red'
  dfA$color3[4] <- 'red'
  dfA$color3[5] <- 'red'

  
  
  return(list(dfA,dfB))
}