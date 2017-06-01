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


library(ggplot2)

doubleMyWidth <- function(x,add) {
  xCopy <- x
  names(xCopy) <- paste0(add,names(x))
  x2 <- cbind(x,xCopy)
  return(x2)
}

baseColData <- createLargeDataSets()
baseColData2 <- doubleMyWidth(baseColData,"A")
baseColData4 <- doubleMyWidth(baseColData2,"B")
baseColData32 <- doubleMyWidth(doubleMyWidth(doubleMyWidth(baseColData4,"C"),"D"),"E")
baseColData128 <- doubleMyWidth(doubleMyWidth(baseColData32,"F"),"G")

baseColDataDiff <- modifyDatasetMinimally(baseColData)
baseColData2Diff <- doubleMyWidth(baseColDataDiff,"A")
baseColData4Diff <-doubleMyWidth(baseColData2Diff,"B")
baseColData32Diff <- doubleMyWidth(doubleMyWidth(doubleMyWidth(baseColData4Diff,"C"),"D"),"E")
baseColData128Diff <-doubleMyWidth(doubleMyWidth(baseColData32Diff,"F"),"G")

compareEqual <- list()
compareEqual[[1]] <- system.time(rCompare(baseColData,baseColData))
compareEqual[[2]] <- system.time(rCompare(baseColData2,baseColData2))
compareEqual[[3]] <- system.time(rCompare(baseColData4,baseColData4))
compareEqual[[4]] <- system.time(rCompare(baseColData32,baseColData32))
compareEqual[[5]] <- system.time(rCompare(baseColData128,baseColData128))

compareDiff <- list()
compareDiff[[1]] <- system.time(rCompare(baseColData,baseColDataDiff))
compareDiff[[2]] <- system.time(rCompare(baseColData2,baseColData2Diff))
compareDiff[[3]] <- system.time(rCompare(baseColData4,baseColData4Diff))
compareDiff[[4]] <- system.time(rCompare(baseColData32,baseColData32Diff))
compareDiff[[5]] <- system.time(rCompare(baseColData128,baseColData128Diff))


# ----- the equal data
compareEqualSummary <- as.data.frame(rbind(c(1,compareEqual[[1]][3],"equal"),
                                        c(2,compareEqual[[2]][3],"equal"),
                                        c(4,compareEqual[[3]][3],"equal"),
                                        c(32,compareEqual[[4]][3],"equal"),
                                        c(128,compareEqual[[5]][3],"equal")))

names(compareEqualSummary) <- c('DataSize','Time','Test')
compareEqualSummary$DataSize <- as.numeric(as.character(compareEqualSummary$DataSize))
compareEqualSummary$Time <- as.numeric(as.character(compareEqualSummary$Time))

compEqualPlot <- ggplot(compareEqualSummary, aes(x = DataSize, y = Time, group = 1)) 
compEqualPlot <- compEqualPlot + geom_line() + geom_point()
compEqualPlot <- compEqualPlot + ggtitle("Comparison time for equal datasets") 
compEqualPlot

# ----- the equal data
compareDiffSummary <- as.data.frame(rbind(c(1,compareDiff[[1]][3],"unequal"),
                                           c(2,compareDiff[[2]][3],"unequal"),
                                           c(4,compareDiff[[3]][3],"unequal"),
                                           c(32,compareDiff[[4]][3],"unequal")))

names(compareDiffSummary) <- c('DataSize','Time','Test')
compareDiffSummary$DataSize <- as.numeric(as.character(compareDiffSummary$DataSize))
compareDiffSummary$Time <- as.numeric(as.character(compareDiffSummary$Time))

compunEqualPlot <- ggplot(compareDiffSummary, aes(x = DataSize, y = Time, group = 1)) 
compunEqualPlot <- compunEqualPlot + geom_line() + geom_point()
compunEqualPlot <- compunEqualPlot + ggtitle("Comparison time for unequal datasets") 
compunEqualPlot



# Put it together
allData <- rbind(compareEqualSummary,compareDiffSummary)

allPlot <- ggplot(allData, aes(x = DataSize, y = Time, group = Test, color= Test)) 
allPlot <- allPlot + geom_line() + geom_point()
allPlot <- allPlot + ggtitle("Comparison across tests") 
allPlot

library(dplyr)
library(knitr)
kable(allData)


# Lets profile this

library(profvis)

prof <- profvis(rCompare(baseColData4,baseColData4Diff))

