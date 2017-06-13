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

#' Summarizing RCompare Output
#'
#' @family dataCompareR.functions
#' @param object an dataCompareR object, usually a result of a call to \code{\link{rCompare}}.
#' @param mismatchCount Integer. How many mismatches to include in tables
#' @param ... Passes any additional arguments (not used in current version)
#' @return The function summary.dataCompareR computes and returns a list of summary details from the dataCompareR output given in \code{object} containing
#' \item{datanameA}{name of the first dataframe in the compare call}
#' \item{datanameB}{name of the second dataframe in the compare call}
#' \item{nrowA}{the number of rows in \code{datanameA}}
#' \item{nrowB}{the number of rows in \code{datanameB}}
#' \item{version}{the version of \code{\link{rCompare}} used to generate the dataCompareR object \code{object}}
#' \item{runtime}{the date and time the dataCompareR object \code{object} was created}
#' \item{rversion}{the version of R used}
#' \item{datasetSummary}{a data frame containing the meta data information on \code{datanameA} and \code{datanameB}}
#' \item{ncolCommon}{the number of columns of the same name contained in both \code{datanameA} and \code{datanameB}}
#' \item{ncolInAOnly}{the number of columns only in \code{datanameA}}
#' \item{ncolInBOnly}{the number of columns only in \code{datanameB}}
#' \item{ncolID}{the number of columns used to match rows in \code{datanameA} and \code{datanameB} }
#' \item{typeMismatch}{a data frame detailing which columns in both \code{datanameA} and \code{datanameB} have different class types}
#' \item{typeMismatchN}{the number of columns with different variable types}
#' \item{nrowCommon}{the number of rows with matching ID columns in both \code{datanameA} and \code{datanameB}}
#' \item{nrowInAOnly}{the number of rows with non matching ID columns in \code{datanameA}}
#' \item{nrowInBOnly}{the number of rows with non matching ID columns in \code{datanameB}}
#' \item{nrowSomeUnequal}{the number of matched rows where at least one value is unequal}
#' \item{nrowAllEqual}{the number of matched rows where all values are equal}
#' \item{ncolsAllEqual}{the number of matched columns where all values are equal}
#' \item{ncolsSomeUnequal}{the number of matched columns where at least one value is unequal}
#' \item{colsWithUnequalValues}{a data frame detailing the mismatches for each matched column}
#' \item{nrowNAmisMatch}{the number of matched numeric rows that contain a NA}
#' \item{maxDifference}{the maximum difference between numeric columns from all matched columns}
#' @export
#' @examples
#'
#' rc1 <- rCompare(iris,iris) 
#' summary(rc1)

summary.dataCompareRobject <- function(object, mismatchCount = 5, ...){
  
  if(!is.numeric(mismatchCount) | mismatchCount < 0) {
    stop("mismatchCount must be a positive number")
  }
  
  # For Column mismatch details...
  sampleIfPossible <- function(X, size) {
    sampSize = min(size, nrow(X))
    sample_n(X, sampSize)
  }
  
  
  # This can be slow  for large files - let the user know its running
  message('dataCompareR is generating the summary...')
  
  ans <- list()
  
  #############################################################################
  
  
  ans$datanameA <- object$meta$A$name
  ans$datanameB <- object$meta$B$name
  ans$nrowA <- object$meta$A$rows
  ans$nrowB <- object$meta$B$rows
  
  # Was any rounding conducted?
  ans$rounding <- FALSE
  ans$roundDigits <- 0
  
  if (is.na(object$meta$roundDigits)) {
    # No rounding was performed
    # Leave as FALSE
  } else {
    # Rounding was performed
    ans$rounding <- TRUE
    ans$roundDigits <- as.numeric(object$meta$roundDigits)
  }
  
  
  # Allocate version number for the dataCompareR package to ans$version
  ans$version <- packageVersion("dataCompareR")
  
  # Allocate time run to ans$runtime
  ans$runtime <- object$meta$runTimestamp
  
  # Allocate R version to ans$Rversion
  ans$rversion <- R.version.string
  
  ans$datasetSummary <- as.data.frame(matrix(c(object$meta$A$name, object$meta$A$rows, 
                        object$meta$A$cols, object$meta$B$name, object$meta$B$rows, object$meta$B$cols), 
                        ncol=3, nrow=2, byrow=T))
  names(ans$datasetSummary) <- c("Dataset Name", "Number of Rows",
                                 "Number of Columns")
  
  #############################################################################
  
  # Number of columns in common
  if (isNotNull(object$colMatching$inboth)) {
    ans$ncolCommon <- length(object$colMatching$inboth)
  }
  
  # Number of columns only in A
  if (isNotNull(object$colMatching$inA)) {
    ans$ncolInAOnly <- length(object$colMatching$inA)
  }
  
  # Number of columns only in B
  if (isNotNull(object$colMatching$inB)) {
    ans$ncolInBOnly <- length(object$colMatching$inB)
  }
  
  # columns only in A
  if (isNotNull(object$colMatching$inA)) {
    ans$colsInAOnly <- object$colMatching$inA
  }
  
  
  # columns only in B
  if (isNotNull(object$colMatching$inB)) {
    ans$colsInBOnly <- object$colMatching$inB
  }
  
  # columns in both
  if (isNotNull(object$colMatching$inboth)) {
    ans$colsInBoth <- object$colMatching$inboth
  }
  
  # Number of ID columns used for matching
  if ((length(object$rowMatching$matchKey)) == 1 && is.na(object$rowMatching$matchKey)) {
    ans$ncolID <- 0
    ans$matchKey <- NA
  } else {
    ans$ncolID <- length(object$rowMatching$matchKey)
    ans$matchKey <- object$rowMatching$matchKey
  }
  
  
  # Number of variables with differing types/classes
  
  ans$typeMismatch <- 0
  ans$typeMismatchN <- 0
  
  if (length(object$mismatches) > 0){
  
    # For each variable take the type info from the first row (same for all 
    # rows) and combine into a data frame
    typeComp<-do.call(rbind.data.frame, lapply(object$mismatches, function (x) x[1, 
                                             c("variable", "typeA", "typeB")]))
    ans$typeMismatch <-subset(typeComp, subset=as.character(typeA) != 
                                                          as.character(typeB))
  
    # Number of columns with different variable types
    ans$typeMismatchN <- nrow(ans$typeMismatch)
    colnames(ans$typeMismatch) <- c('Column Name', 
                                    paste0('Column Type (in ', object$meta$A$name, ')'),
                                    paste0('Column Type (in ', object$meta$B$name, ')'))
  }
  #############################################################################
  
  noMatchKeys <- length(object$rowMatching$matchKey) + 1
  
  #Number of Rows in Common:
  if (isNotNull(object$rowMatching$inboth) & is.integer(object$rowMatching$inboth)){
    ans$nrowCommon  <- length(object$rowMatching$inboth)
  }
  if (isNotNull(object$rowMatching$inboth) & is.data.frame(object$rowMatching$inboth)){
    ans$nrowCommon  <- nrow(object$rowMatching$inboth)
  }
  
  # Number of rows only in A
  if (isNotNull(object$rowMatching$inA)) {
    ans$nrowInAOnly <- sum(sapply(object$rowMatching$inA,length)) / length(object$rowMatching$inA)
  }
  
  # Number of rows only in B
  if (isNotNull(object$rowMatching$inB)) {
    ans$nrowInBOnly <- sum(sapply(object$rowMatching$inB,length)) / length(object$rowMatching$inB)
  }
  
  # Actual rows labels that are not in A or B
  ans$rowsInAOnly <- sampleIfPossible(as.data.frame(object$rowMatching$inA),mismatchCount)
  ans$rowsInBOnly <- sampleIfPossible(as.data.frame(object$rowMatching$inB),mismatchCount)
  
  # Total # rows with SOME unequal columns:
  # No longer used, also very slow
  #ans$nrowSomeUnequal <- 0
  
  #if(showRowMatching) {
  #  if (isNotNull(mismatches) & length(mismatches) > 0) {
  #    ans$nrowSomeUnequal <- nrow(na.omit(unique(do.call(rbind,(
  #                                lapply(1:length(mismatches),
  #                                 function(x){
  #                                   if (isNotNull(x)){
  #                                     mismatches[[x]][c(2:noMatchKeys)]
  #                                     }
  #                                   }
  #                                 ))))))
  #  }
  #}
  
  # Total # of rows with ALL equal columns:
  #if (isNotNull(rowMatching$inboth)) {
  #  ans$nrowAllEqual <- ans$nrowCommon - ans$nrowSomeUnequal
  #}
  
  #############################################################################
  # RowComparisonSummary
  
  # Number of columns compared with ALL rows equal:
  if(isNotNull(object$matches)) {
    ans$ncolsAllEqual <- length(object$matches)
  }
  
  # Number of columns compared with some rows Unequal:
  if(isNotNull(object$mismatches)) {
    ans$ncolsSomeUnequal <- length(object$mismatches)
  }
  
  ans$colsWithUnequalValues <- 0
  ans$nrowNAmismatch <- 0
  
  #Columns with unequal values:
  if (length(object$mismatches) != 0) {
    ans$colsWithUnequalValues <- do.call(rbind,(lapply(1:length(object$mismatches), 
                                                FUN = colsWithUnequalValues,
                                                object$mismatches)))
  
  # Number of rows with a missing value difference
    ans$nrowNAmismatch <-nrow(subset(ans$colsWithUnequalValues, missDif > 0))
  
    colnames(ans$colsWithUnequalValues) <- c('Column',
                                             paste0('Type (in ', object$meta$A$name, ')'),
                                             paste0('Type (in ', object$meta$B$name,')'),
                                             '# differences', 'Max difference',
                                             '# NAs')
  }
  
  ans$ColsMatching <- object$matches
    
  # Maximum difference in comparison
  ans$maxDifference <- NA
  
 
  
  ans$colMismDetls <- NULL
  
  if (length(object$mismatches) > 0){
  
    ans$colMismDetls <- lapply(object$mismatches, size =mismatchCount, FUN = sampleIfPossible)
    
    # We want to rename the column names to make the output tables easier to understand
    # this is a litle painful at this stage
    
    # Function to rename our cols
    colRenamer <- function(mismDetls, nms) {
      #print(nms)
      names(mismDetls)[names(mismDetls) == 'valueA'] <-  paste0(nms, ' (', object$meta$A$name, ')')
      names(mismDetls)[names(mismDetls) == 'valueB'] <-  paste0(nms,' (' , object$meta$B$name, ')')
      names(mismDetls)[names(mismDetls) == 'typeA'] <-  paste0('Type (', object$meta$A$name, ')')
      names(mismDetls)[names(mismDetls) == 'typeB'] <-  paste0('Type (', object$meta$B$name, ')')
      names(mismDetls)[names(mismDetls) == 'diffAB'] <-  'Difference'
      
      return(mismDetls)
    }
    
    # Rename if the format is as expected
    # Loop, as we need access to the names of the outer list too
    for(ii in  1:length(ans$colMismDetls)) {
      ans$colMismDetls[[ii]] <- colRenamer(ans$colMismDetls[[ii]], names(ans$colMismDetls[ii]))
      }
    

    # This isn't great,  but it'll do for now, as a way to hide what we don't want
    ans$colMismDetls <- lapply(ans$colMismDetls, function(x) 
                                  x[ , !(names(x) %in% "variable")] )
  }
  
  ans$mismatchCount <- mismatchCount
  
  class(ans) <- "summary.dataCompareRobject"
  
  ans
  
}