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


createLargeDataSets <- function() {
  
  # create a large dataset. We can make larger datasets by 
  # progressively rbinding to itself
  
  set.seed(1234)
  
  # an ID column
  a <- seq(1:1000000)
  # Some doubles
  b <- as.character(sample(letters[1:5], 1000000, TRUE))
  c <- as.character(sample(letters[1:5], 1000000, TRUE))
  d <- as.character(sample(letters[1:5], 1000000, TRUE))
  # Some chars
  e <- runif(1000000,0,1)
  f <- runif(1000000,0,1)
  g <- runif(1000000,0,1)
  # Some dates
  h <- Sys.Date() + runif(1000000,0,100)
  i <- Sys.Date() + runif(1000000,0,100)
  # Some times
  j <- Sys.time() + runif(1000000,0,100)
  k <- Sys.time() + runif(1000000,0,100)
  # Some factors
  l <- as.factor(sample(letters[1:5], 1000000, TRUE))
  m <- as.factor(sample(letters[1:10], 1000000, TRUE))
  n <- as.factor(sample(letters[1:20], 1000000, TRUE))
  # Some ints
  o <- sample(seq(1:100),1000000, TRUE)
  p <- sample(seq(-100:100),1000000, TRUE)
  q <- sample(seq(-1000000:1000000),1000000, TRUE)
  
  
  # combine to form a data frame
  df <- data.frame(a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q)
  df$b <- as.character(df$b)
  df$c <- as.character(df$c)
  df$d <- as.character(df$d)
  #object.size(df)/1024/1024
  
  return(df)
  
}

modifyDatasetMinimally <- function(df) {
  
  df$b[1:50] <- 'A'
  df$c[1:50] <- 'A'
  df$d[1:50] <- 'A'
  
  df$e[50:100] <- -1.1
  df$f[50:100] <- -1.1
  df$g[50:100] <- -1.1
  
  df$h[100:150] <- Sys.Date() + 1
  df$i[100:150] <- Sys.Date() - 1
  
  df$j[150:200] <- Sys.time() + 10
  df$k[150:200] <- Sys.time() - 10
  
  # Some factors
  df$l[200:250] <- 'b'
  df$m[200:250] <- 'b'
  df$n[200:250] <- 'b'
  
  # Some ints
  df$o[250:300] <- 1
  df$p[250:300] <- 1
  df$q[250:300] <- 1
  
  return(df)
}

modifyDatasetMaximally <- function(df) {
  
  df$b[1:50000] <- 'A'
  df$c[1:50000] <- 'A'
  df$d[1:50000] <- 'A'
  
  df$e[1:50000] <- -1.1
  df$f[1:50000] <- -1.1
  df$g[1:50000] <- -1.1
  
  df$h[1:50000] <- Sys.Date() + 1
  df$i[1:50000] <- Sys.Date() - 1
  
  df$j[1:50000] <- Sys.time() + 10
  df$k[1:50000] <- Sys.time() - 10
  
  # Some factors
  df$l[1:50000] <- 'b'
  df$m[1:50000] <- 'b'
  df$n[1:50000] <- 'b'
  
  # Some ints
  df$o[1:50000] <- 1
  df$p[1:50000] <- 1
  df$q[1:50000] <- 1
  
  return(df)
}

modifyDatasetRowsCols <- function(df) {

  df$B <- NULL
  df$E <- NULL
  df$G <- NULL
  df$P <- NULL
  
  df <- df[1:800000,]
  
  return(df)
}

makeMeBigger <- function(df, n) {
  do.call("rbind", replicate(n, df, simplify = FALSE))
}

makeMeWider <- function(df, n) {
  do.call("cbind", replicate(n, df, simplify = FALSE), deparse.level = 0)
}