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

#' Save a report based on a dataCompareR object
#' @description Saves R markdown and HTML reports in the area specified by the user. 
#' 
#' Uses knitr and markdown to create reports. Reports have the extensions .Rmd or .html. 
#' By default the \code{table.css} style sheet is used for format the html output.
#' 
#' @family dataCompareR.functions
#' @param compareObject a dataCompareR object.
#' @param reportLocation String. Location to save reports specified by the user. The R markdown and (optionally) HTML reports will 
#' be saved in this area
#' @param reportName String. The name of the report. Reports will be saved as reportName.Rmd and (optionally) reportName.html in 
#' \code{reportLocation}
#' @param HTMLReport Boolean. Option to output html report.
#' @param showInViewer Boolean. Does the html report open automatically in the viewer?
#' @param stylesheet String. Optional link to customised css stylesheet
#' @param printAll Boolean. If TRUE, all mis-matches in the data are printed to the file. This acts as a shortcut
#' to get all mismatches in the report, compared to passing the number in \code{mismatchCount}. When TRUE, overrides the 
#' \code{mismatchCount} field passed via ellipses 
#' @param ... Optional arguments which will be passed to \code{summary}, for example \code{mismatchCount}
#' @import knitr
#' @import markdown
#' @export
#' @examples  
#' \dontrun{saveReport(rcObj, reportName = 'testReport')}

saveReport <- function(compareObject, reportName, reportLocation = '.', HTMLReport= TRUE, 
                       showInViewer = TRUE, stylesheet = NA, printAll = FALSE, ...) {
  
  # Argument checkin
  if(!is.dataCompareRobject(compareObject)) {
    stop("Invalid comparison object")
  }
  
  if(!file.exists(reportLocation)) {
    stop("Invalid reportLocation")
  }
  
  if(!is.logical(HTMLReport)) {
    stop("HTMLReport must be T/F")
  }
  
  if(!is.logical(showInViewer)) {
    stop("showInViewer must be T/F")
  }
  
  if(!is.character(reportName)) {
    stop("Report name must be a character")
  }
  
  if(length(reportName)!=1) {
    stop("Report name must be a single character")
  }
  
  if(!is.logical(printAll)) {
    stop("printAll must be T/F")
  }
  
  # Determine where the stylesheet is coming from
  # and if custom stylesheet exists
  if(is.na(stylesheet)) {
    message('Using default stylesheet')
    stylesheetToUse <- system.file("css", "table.css", package = "dataCompareR")
  } else {
    if(file.exists(stylesheet)) {
      message(paste0('Using custom stylesheet at ',stylesheet ))
      stylesheetToUse <- stylesheet
    } else {
      stop(paste0("Cannot find stylesheet at ", stylesheet))
    }
  }
  
  # Create file locations for saving
  RmdLocn <- file.path(reportLocation, paste0(reportName, '.Rmd'))
  HTMLLocn <- file.path(reportLocation, paste0(reportName, '.html'))
  MdLocn <- file.path(reportLocation, paste0(reportName, '.md'))
  
  # If printAll is TRUE, set the number of rows to pass to compareObject
  if(printAll) {
    noRowsToPrint <- nrow(compareObject$mismatches[[1]])
    summaryCompareObject <- summary(compareObject, mismatchCount = noRowsToPrint)
  } else {
    summaryCompareObject <- summary(compareObject, ...)
  }
  
  # Create R markdown file from summary.dataCompareRobject function
  
  capture.output(createReportText(x=summaryCompareObject), file=RmdLocn)
  
  # Compile a report based on the R markdown file
  if (HTMLReport==TRUE){
    message('Producing HTML output')
    # Force a 2 step process to avoid the .md file being in the working directory
    knit(input = RmdLocn, output = MdLocn)
    markdown::markdownToHTML(file = MdLocn, output = HTMLLocn, stylesheet= stylesheetToUse )
  }
  
  if (HTMLReport == FALSE & showInViewer == TRUE) {
    message('Cannot display in viewer if HTML report is not enabled')
  }
  else {
    # Display this to user
    if(showInViewer) {
      
      viewer <- getOption("viewer")
      # RSTUDIO
      if (!is.null(viewer)) {
        viewer(HTMLLocn)
      }
      # OTHER
      else {
        utils::browseURL(HTMLLocn)
      }
    }
  }
  

}
