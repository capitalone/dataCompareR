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

baseData <- createLargeDataSets()
baseDataChangedMinimal <- modifyDatasetMinimally(baseData)
baseDataChangedLarge <- modifyDatasetMaximally(baseData)
baseDataSliced <- modifyDatasetRowsCols(baseData)

baseData5 <- makeMeBigger(baseData,5)
baseDataChangedMinimal5 <- makeMeBigger(baseDataChangedMinimal,5)
baseDataChangedLarge5 <- makeMeBigger(baseDataChangedLarge,5)
baseDataSliced5 <- makeMeBigger(baseDataSliced,5)

baseData10 <- makeMeBigger(baseData5,2)
baseDataChangedMinimal10 <- makeMeBigger(baseDataChangedMinimal5,2)
baseDataChangedLarge10 <- makeMeBigger(baseDataChangedLarge5,2)
baseDataSliced10 <- makeMeBigger(baseDataSliced5,2)

baseData50 <- makeMeBigger(baseData10,5)
baseDataChangedMinimal50 <- makeMeBigger(baseDataChangedMinimal10,5)
baseDataChangedLarge50 <- makeMeBigger(baseDataChangedLarge10,5)
baseDataSliced50 <- makeMeBigger(baseDataSliced10,5)

compareEqual <- list()
compareEqual[[1]] <- system.time(rCompare(baseData,baseData))
compareEqual[[2]] <- system.time(rCompare(baseData5,baseData5))
compareEqual[[3]] <- system.time(rCompare(baseData10,baseData10))
compareEqual[[4]] <- system.time(rCompare(baseData50,baseData50))

compareMinUnequal <- list()
compareMinUnequal[[1]] <- system.time(rCompare(baseData,baseDataChangedMinimal))
compareMinUnequal[[2]] <- system.time(rCompare(baseData5,baseDataChangedMinimal5))
compareMinUnequal[[3]] <- system.time(rCompare(baseData10,baseDataChangedMinimal10))
compareMinUnequal[[4]] <- system.time(rCompare(baseData50,baseDataChangedMinimal50))

compareMaxUnequal <- list()
compareMaxUnequal[[1]] <- system.time(rCompare(baseData,baseDataChangedLarge))
compareMaxUnequal[[2]] <- system.time(rCompare(baseData5,baseDataChangedLarge5))
compareMaxUnequal[[3]] <- system.time(rCompare(baseData10,baseDataChangedLarge10))
compareMaxUnequal[[4]] <- system.time(rCompare(baseData50,baseDataChangedLarge50))

compareSlice <- list()
compareSlice[[1]] <- system.time(rCompare(baseData,baseDataSliced))
compareSlice[[2]] <- system.time(rCompare(baseData5,baseDataSliced5))
compareSlice[[3]] <- system.time(rCompare(baseData10,baseDataSliced10))
compareSlice[[4]] <- system.time(rCompare(baseData50,baseDataSliced50))

# ----- the equal data
compEqualSummary <- as.data.frame(rbind(c(1,compareEqual[[1]][3],"equal"),
                                        c(5,compareEqual[[2]][3],"equal"),
                                        c(10,compareEqual[[3]][3],"equal"),
                                        c(50,compareEqual[[4]][3],"equal")))

names(compEqualSummary) <- c('DataSize','Time','Test')
compEqualSummary$DataSize <- as.numeric(as.character(compEqualSummary$DataSize))
compEqualSummary$Time <- as.numeric(as.character(compEqualSummary$Time))

compEqualPlot <- ggplot(compEqualSummary, aes(x = DataSize, y = Time, group = 1)) 
compEqualPlot <- compEqualPlot + geom_line() 
compEqualPlot <- compEqualPlot + ggtitle("Comparison time for equal datasets") 
compEqualPlot

# ----- first unequal data
compMinEqualSummary <- as.data.frame(rbind(c(1,compareMinUnequal[[1]][3],"minunequal"),
                                        c(5,compareMinUnequal[[2]][3],"minunequal"),
                                        c(10,compareMinUnequal[[3]][3],"minunequal"),
                                        c(50,compareMinUnequal[[4]][3],"minunequal")))

names(compMinEqualSummary) <- c('DataSize','Time','Test')
compMinEqualSummary$DataSize <- as.numeric(as.character(compMinEqualSummary$DataSize))
compMinEqualSummary$Time <- as.numeric(as.character(compMinEqualSummary$Time))

compMinEqualPlot <- ggplot(compMinEqualSummary, aes(x = DataSize, y = Time, group = 1)) 
compMinEqualPlot <- compMinEqualPlot + geom_line() 
compMinEqualPlot <- compMinEqualPlot + ggtitle("Comparison time for somewhat unequal datasets") 
compMinEqualPlot

# ----- 2nd unequal data
compMaxEqualSummary <- as.data.frame(rbind(c(1,compareMaxUnequal[[1]][3],"Maxunequal"),
                                           c(5,compareMaxUnequal[[2]][3],"Maxunequal"),
                                           c(10,compareMaxUnequal[[3]][3],"Maxunequal"),
                                           c(50,compareMaxUnequal[[4]][3],"Maxunequal")))

names(compMaxEqualSummary) <- c('DataSize','Time','Test')
compMaxEqualSummary$DataSize <- as.numeric(as.character(compMaxEqualSummary$DataSize))
compMaxEqualSummary$Time <- as.numeric(as.character(compMaxEqualSummary$Time))

compMaxEqualPlot <- ggplot(compMaxEqualSummary, aes(x = DataSize, y = Time, group = 1)) 
compMaxEqualPlot <- compMaxEqualPlot + geom_line() 
compMaxEqualPlot <- compMaxEqualPlot + ggtitle("Comparison time for very unequal datasets") 
compMaxEqualPlot

# ----- slice data 
compsliceSummary <- as.data.frame(rbind(c(1,compareSlice[[1]][3],"slice"),
                                           c(5,compareSlice[[2]][3],"slice"),
                                           c(10,compareSlice[[3]][3],"slice"),
                                           c(50,compareSlice[[4]][3],"slice")))

names(compsliceSummary) <- c('DataSize','Time','Test')
compsliceSummary$DataSize <- as.numeric(as.character(compsliceSummary$DataSize))
compsliceSummary$Time <- as.numeric(as.character(compsliceSummary$Time))

compslicePlot <- ggplot(compsliceSummary, aes(x = DataSize, y = Time, group = 1)) 
compslicePlot <- compslicePlot + geom_line() 
compslicePlot <- compslicePlot + ggtitle("Comparison time for col/rows diff") 
compslicePlot


# Put it together
allData <- rbind(compsliceSummary,compMaxEqualSummary,compMinEqualSummary,compEqualSummary)

allPlot <- ggplot(allData, aes(x = DataSize, y = Time, group = Test, color= Test)) 
allPlot <- allPlot + geom_line() + geom_point()
allPlot <- allPlot + ggtitle("Comparison across tests") 
allPlot



