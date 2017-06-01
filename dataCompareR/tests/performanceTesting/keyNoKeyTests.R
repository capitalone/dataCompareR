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


baseColDataDiff <- modifyDatasetMinimally(baseColData)
baseColData2Diff <- doubleMyWidth(baseColDataDiff,"A")
baseColData4Diff <-doubleMyWidth(baseColData2Diff,"B")


compareDiff <- list()
compareDiff[[1]] <- system.time(rCompare(baseColData,baseColDataDiff))
compareDiff[[2]] <- system.time(rCompare(baseColData2,baseColData2Diff))
compareDiff[[3]] <- system.time(rCompare(baseColData4,baseColData4Diff))

names(baseColData2)
names(baseColDataDiff)

compareDiffKey <- list()
compareDiffKey[[1]] <- system.time(rCompare(baseColData,baseColDataDiff, key = "a"))
compareDiffKey[[2]] <- system.time(rCompare(baseColData2,baseColData2Diff, key = "a"))
compareDiffKey[[3]] <- system.time(rCompare(baseColData4,baseColData4Diff, key = "a"))


