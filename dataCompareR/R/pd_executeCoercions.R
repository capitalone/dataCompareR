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


#' executeCoercions: 
#' 
#' @param DFA Input dataframe A
#' @param DFB Input dataframe B
#' @param WhitespaceTrim User defined boolean for whether leading/trailing white space is trimmed in strings (TRUE / FALSE)
#' @return \code{out} list containing 3 data frames DFA, DFB and DataTypes
#' @return \code{DFA} Dataframe with factor fields converted to character type and white space trimming (if option is selected by the user)
#' @return \code{DFB} Dataframe with factor fields converted to character type and white space trimming (if option is selected by the user)
#' @return \code{DataTypes} Dataframe with field types before and after cleaning for both DFA and DFB
#' @examples 
#' \dontrun{executeCoercions(DFA=iris,DFB=iris,WhitespaceTrim=T)}

executeCoercions<-function(DFA,DFB,WhitespaceTrim=T){

# Store original data types 
  DFATypesOrig <-sapply(DFA, function(x) class(x))
  DFBTypesOrig <-sapply(DFB, function(x) class(x))
  
# Execute Factor to Character string coercion
  DFA<-coerceFactorsToChar(DF=DFA)
  DFB<-coerceFactorsToChar(DF=DFB)

# If WhitespaceTrim parameter is TRUE, execute white space trimming
if (WhitespaceTrim==T){
  DFA<-trimCharVars(DF=DFA)
  DFB<-trimCharVars(DF=DFB)
}
  

# Store new data types 
  DFATypesNew <-sapply(DFA, function(x) class(x))
  DFBTypesNew <-sapply(DFB, function(x) class(x))

# Save data type information
  DataTypes<-data.frame(rbind(DFATypesOrig,DFBTypesOrig,DFATypesNew,DFBTypesNew), stringsAsFactors=FALSE)

# Output list containing dataframes DFA, DFB, DataTypes
  out<-list()
  out$DFA <- DFA
  out$DFB <- DFB
  out$DataTypes <- DataTypes
  return(out)
  
}
