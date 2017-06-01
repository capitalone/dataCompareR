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


# Changes to Titanic dataset
# The following should be mentioned as the output of dataCompareR function.
# To load all Titanic dataset run load("titanic_datasets.RData").
# The base dataset (titanic) is a slightly altered of the original titanic dataset.
# All differences mentioned below compare an altered version of data to the base dataset.
# The differences are identical for all altered versions 
# (titanic2, titanic2DataTable, titanic2Tibble, titanic2Matrix)
#
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


#titanicKK <- read.csv('./tests/testthat/titanic_train.csv')
titanic <- titanic_train

titanic$Name <- as.factor(titanic$Name)
titanic$Sex <- as.factor(titanic$Sex)
titanic$Ticket <- as.factor(titanic$Ticket)
titanic$Cabin <- as.factor(titanic$Cabin)
titanic$Embarked <- as.factor(titanic$Embarked)


#head(titanic)
#str(titanic)

titanic2 <- titanic
titanic2$Sex <- as.character(titanic2$Sex)
levels(titanic2$Embarked) <- levels(titanic2$Embarked)[c(2,1,3,4)]

# Add additional level
levels(titanic2$Cabin) <- c(levels(titanic2$Cabin), "Jack's secret cabin")

# 
#table(titanic$Survived)
titanic$HasSurvived <- ifelse(titanic$Survived == 1, "survived", "died")
titanic$HasSurvived <- as.factor(titanic$HasSurvived)

titanic2$HasSurvived <- ifelse(titanic2$Survived == 1, "survived", "died")
titanic2$HasSurvived <- as.factor(titanic2$HasSurvived)

titanic$HasSurvived[5] <- NA
titanic2$HasSurvived <- factor(titanic2$HasSurvived, levels = c("died", "survived", NaN))
titanic2$HasSurvived[5] <- NaN

titanic$Name <- as.character(titanic$Name)
titanic$Name[1] <- paste0(rep("Jack and Rose or Rose and Jack or", 2), collapse = " ")

titanic2$Name <- as.character(titanic2$Name)
titanic2$Name[1] <- paste0(c(rep("Jack and Rose or Rose and Jack or", 3), "?"), collapse = " ")

titanic2$Name[2] <- paste0(" ", titanic2$Name[2])
titanic2$Name[3] <- paste0(titanic2$Name[3], "        ")
titanic2$Name[4] <- paste0(" ", titanic2$Name[4], "        ")
titanic2$Name[5] <- NA
titanic$Name[5] <- NaN
titanic2$Name[6] <-  paste0(paste0(rep("Jack and Rose or Rose and Jack or", 3), collapse = " "), "\n", "Jack and Rose or Rose and Jack or")
titanic$Name[6]  <- paste0(paste0(rep("Jack and Rose or Rose and Jack or", 3), collapse = " "), " ", "Jack and Rose or Rose and Jack or")
titanic2$Name[7] <- tolower(titanic2$Name[7])
titanic2$Name[8] <- toupper(titanic2$Name[8])
titanic$Name[9] <- "a¬!£$%^&*()_+{}:@~<>?"
titanic2$Name[9] <- "a"

if(require(data.table)) {
  titanic2DataTable <- as.data.table(titanic2)
} else {
  titanic2DataTable <- titanic2
}

titanic2Matrix <- as.matrix(titanic2)

if(require(tibble)) {
  titanic2Tibble <- as_tibble(titanic2)
} else {
  titanic2Tibble <- titanic2
}

titanic2shuffle <- titanic2[nrow(titanic2):1, ]


