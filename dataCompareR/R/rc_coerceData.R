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

#' coerceData
#'
#' @param doa Data object A (any object that can be coerced to a data frame)
#' @param dob Data object B (any object that can be coerced to a data frame)
#' @return A list of 2 data frames, which is DOA and DOB coerced as data.frames
#' @examples
#' \dontrun{irisMatrix <- as.matrix(iris)}
#' \dontrun{coerceData(irisMatrix,iris)}
coerceData <- function(doa, dob)
{

  # Simple case when we have a data frame  
  if (is.data.frame(doa) & is.data.frame(dob) & length(class(doa)) == 1 & length(class(dob)) == 1) {
    return(list(doa,dob))
  }
  
  # Otherwise we have to coerce doa
  message('Coercing input data to data.frame')
  tryCatch({
    dfa <- as.data.frame(doa)
    }, warning = function(w) {
        print(paste0('WARNING: ', w))
      }, error = function(e) {
        stop(paste0('ERROR:', e))
      }
  )
  
  # and dob
  
  tryCatch({
    dfb <- as.data.frame(dob)
  }, warning = function(w) {
    print(paste0('WARNING: ', w))
  }, error = function(e) {
    stop(paste0('ERROR:', e))
  }
  )
  
  # If the inputs were two vectors, the coercion works, but results in dataframes with column names
  # matching the name of the vector, and so no comparison can occur
  # The user intent is clearly to compare the vectors, so hard coding a fix here
  

  # if both inputs are vectors and resulant tables have 1 column
  if(is.vector(doa) && is.vector(dob) && ncol(dfa) == 1  && ncol(dfb) == 1){
    message('Detected vector input - renaming columns')
  
    names(dfa) <- c('VectorCol')
    names(dfb) <- c('VectorCol')
  }
  
  # In the case that only 1 input is a vector, this will fail, so warn
  
  if(sum(is.vector(doa), is.vector(dob)) == 1) {
    message(paste0("One input was a vector. The vector was coerced to a data frame, but this may not result", 
                   " in the correct column names. Please check details of matching using summary on the resultant object."))
  }
  
  return(list(dfa,dfb))

  
}