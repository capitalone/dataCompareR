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


#' Generate two dataframes and returns subsets of these dataframes that have shared rows.
#' 
#' @param df_a A dataframe
#' @param df_b A dataframe
#' @param indices The indices to match rows between \code{df_a} and \code{df_b}. Can be NA,
#' single character, or a vector of characters
#' @import stringi
#' @return A list containing the two dataframes, subsetted by shared indices, and a list which itself 
#'        contains dataframes for the dropped rows
matchRows <- function(df_a, df_b, indices = NA)
{
  
  if (length(indices ==1) && is.na(indices)) {
    return(matchNoIndex(df_a, df_b))
  }
  
  if(length(indices) == 1){
    return(matchSingleIndex(df_a, df_b, indices))
  }

  if(length(indices) > 1){
    return(matchMultiIndex(df_a, df_b, indices))
  }
}


#' Checks that a list of indexes areunique
#' 
#' @param df_indices A vector of values
#' @return Boolean - true if all values in vector are unique, false if not
#' @examples
#' \dontrun{checkUniqueness(c('car','van','van'))}
#' \dontrun{checkUniqueness(c('car','van','bus'))}
checkUniqueness <- function(df_indices)
{
  if(length(unique(df_indices)) == length(df_indices)){
    return(TRUE)
  }
  return(FALSE)
}

#' Create a dataframe of the rows that don't match 
#' 
#' @param index_antisubset Vector of mismatching indices
#' @param original_keys A character array
#' @param index_key A character array
#' @param df A data frame
#' @return A dataframe containing the dropped rows
createAntiSubset <- function(index_antisubset,original_keys,index_key, df)
{
  if(length(index_antisubset)==0){
    #df_antisubset <- data.frame(integer())
    df_antisubset <- data.frame(matrix(data = NA, ncol = length(original_keys), nrow = 0))
    colnames(df_antisubset) <- original_keys
  } else if(length(original_keys)==1){
    df_antisubset <- data.frame(df[df[,index_key] %in% index_antisubset,original_keys])
    colnames(df_antisubset) <- original_keys
  } else {
    df_antisubset <- df[df[,index_key] %in% index_antisubset,original_keys]
  }
  return(df_antisubset)
}



#' Generate two dataframes that contain the same rows based on a single index
#' 
#' @param df_a A dataframe
#' @param df_b A dataframe
#' @param index_key A character vector
#' @param original_keys A character vector
#' @return A list containing the two dataframes, subsetted by shared indices, and a list which itself 
#'        contains the vectors for the dropped rows
#'        
matchSingleIndex <- function(df_a, df_b, index_key, original_keys)
{
  indexIsUnique <- checkUniqueness(df_a[,index_key]) & checkUniqueness(df_b[,index_key])
  
  if(!indexIsUnique){
    stop("The indices are not unique in the submitted dataframes. Please resubmit with unique indices.")
  }
  
  if(missing(original_keys)){
    original_keys <- c(index_key)
  }
  
  indices_a <- c(df_a[,index_key])
  indices_b <- c(df_b[,index_key])
  
  index_subset <- indices_a[indices_a %in% indices_b]
  index_a_antisubset <- setdiff(indices_a,index_subset)
  index_b_antisubset <- setdiff(indices_b,index_subset)

  # This is better if we have a character key
  
  
  if(is.character(df_a[,index_key]) & is.character(df_b[,index_key])) {
    index_subset_df <- data.frame(index_subset, stringsAsFactors = F)
    names(index_subset_df) <- index_key
  
    df_a2 <- suppressMessages(inner_join(df_a, index_subset_df))
    df_b2 <- suppressMessages(inner_join(df_b, index_subset_df))
    
    df_a_subset <- df_a2[stri_order(df_a2[,index_key]),]
    df_b_subset <- df_b2[stri_order(df_b2[,index_key]),]  
  } else {
    # And this is better for numbers (and works in general)
    
    df_a_subset <- df_a[df_a[,index_key] %in% index_subset,] %>% arrange_(index_key)
    df_b_subset <- df_b[df_b[,index_key] %in% index_subset,] %>% arrange_(index_key)
  }
  

  df_a_antisubset <- createAntiSubset(index_a_antisubset, original_keys, index_key,df_a)  
  df_b_antisubset <- createAntiSubset(index_b_antisubset, original_keys, index_key,df_b)

  return(list(df_a_subset,df_b_subset,list(df_a_antisubset,df_b_antisubset)))
}


#' Generate two dataframes that contain the same rows based on a two-column index
#' 
#' @param df_a A dataframe
#' @param df_b A dataframe
#' @param indices A char vector
#' @return A list containing the two dataframes, subsetted by shared indices, and a list which itself 
#'        contains the vectors for the dropped rows
matchMultiIndex <- function(df_a, df_b, indices)
{
  df_a$dataCompareR_merged_indices <- do.call(paste,c(df_a[indices],sep = ""))
  df_b$dataCompareR_merged_indices <- do.call(paste,c(df_b[indices],sep = ""))
  
  mtchedData <- matchSingleIndex(df_a, df_b, 'dataCompareR_merged_indices', indices)
  
  # Lose these columns
  mtchedData[[1]]$dataCompareR_merged_indices <- NULL
  mtchedData[[2]]$dataCompareR_merged_indices <- NULL
  
  return(mtchedData)
}

#' Generate two dataframes that contain the same rows based on a two-column index
#' 
#' @param df_a A dataframe
#' @param df_b A dataframe
#' @return A list containing the two dataframes, subsetted to the size of the smaller one, and
#'          a list containing vectors of the rows dropped.
matchNoIndex <- function(df_a, df_b)
{
  if(nrow(df_a)>nrow(df_b)) {
    df_a_subset <-df_a[1:nrow(df_b),]
    df_b_subset <- df_b
    rows_dropped_from_a <- data.frame(indices_removed=(nrow(df_a_subset)+1):nrow(df_a))
    rows_dropped_from_b <- data.frame(indices_removed=integer())
  }
  else if (nrow(df_b)>nrow(df_a)) {
    df_a_subset <- df_a
    df_b_subset <-df_b[1:nrow(df_a),]
    rows_dropped_from_a <- data.frame(indices_removed=integer())
    rows_dropped_from_b <- data.frame(indices_removed=(nrow(df_b_subset)+1):nrow(df_b))
  }
  else {
    df_a_subset <- df_a
    df_b_subset <- df_b
    rows_dropped_from_a <- data.frame(indices_removed=integer())
    rows_dropped_from_b <- data.frame(indices_removed=integer())
  }
  

  return(list(df_a_subset, df_b_subset, list(rows_dropped_from_a,rows_dropped_from_b)))
}
