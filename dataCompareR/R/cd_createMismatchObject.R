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

#' Create mismatch object
#'
#' @param dat_a dataframe, output from prepareData
#' @param dat_b dataframe, output from prepareDate
#' @param dat_eq dataframe, output from locateMismatches
#' @param str_index, vector of index variables (could have length 1)
#'
#' @return An dataCompareR mismatch object
#'
#' @examples
#'\dontrun{createMismatchObject(dataA, dataB, mism, idx)}
createMismatchObject <- function(dat_a, dat_b, dat_eq, str_index) {
  
  
  # Initialise output object
  out <- list()

  # Short cut
  # If we have no rows, because no rows match, just return the empty list
  if(nrow(dat_a)==0) {
    return(out)
  }
  
  
  # Loop over variables to create output, ignoring index variable
  varnames <- names(dat_a)[!(names(dat_a) %in% str_index)]
  for (v in varnames) {
    # For each variable name create that variable's mismatch object
    if (sum(!dat_eq[, v]) > 0) {
      if (!is.null(str_index)) {
        d <- variableMismatches(v, dat_a[, c(str_index, v)], dat_b[, c(str_index, v)], dat_eq[, v])
      } else {
        # Hacky fix for no index
        da <- data.frame(dat_a[, v])
        db <- data.frame(dat_b[, v])
        names(da) <- v
        names(db) <- v
        da[, "__temprowname__"] <- seq(1:nrow(da))
        db[, "__temprowname__"] <- seq(1:nrow(db))
        d <- variableMismatches(v, da, db, dat_eq[, v])
      }
      out[[v]] <- variableDetails(d)
    } else {
      out[[v]] <- NA
    }
  }

  return(out)
}

#' Create variable mismatch table
#'
#' @param varname, variable to create mismatch table for
#' @param vals_a, variables from dfA
#' @param vals_b, variables from dfB
#' @param vector_eq, a list of columns which are equal
#'
#' @return Mismatch table
#'
variableMismatches <- function(varname, vals_a, vals_b, vector_eq) {
  d <- vals_a[!vector_eq, ]
  names(d)[names(d) == varname] <- "valueA"
  d[, "valueB"] <- vals_b[!vector_eq, varname]
  if ("__temprowname__" %in% names(d)) {
    d[, "__temprowname__"] <- NULL
  }
  
  d[,'variable'] <- varname
  return(d)
}

#' Create variable mismatch details
#'
#' @param dat The mismatch data
#'
#' @return mismatch details
variableDetails <- function(dat) {
  

  class_a <- collapseClasses(dat[, "valueA"])
  class_b <- collapseClasses(dat[, "valueB"])
  dat[, "typeA"] <- class_a
  dat[, "typeB"] <- class_b
  if (class_a == "numeric") {
    dat[, "typeA"] <- typeof(dat[, "valueA"])
  }
  if (class_b == "numeric") {
    dat[, "typeB"] <- typeof(dat[, "valueB"])
  }
  if ((class_a == class_b) & (class_a != "character") & (!grepl("factor",class_a ))) {
    dat[, "diffAB"] <- dat[, "valueA"] - dat[, "valueB"]
  } else {
    dat[, "diffAB"] <- ""
  }
  
  return(dat)
}
