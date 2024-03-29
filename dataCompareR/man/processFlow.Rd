% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pf_processFlow.R
\name{processFlow}
\alias{processFlow}
\title{processFlow Handles the process flow for the whole package}
\usage{
processFlow(dfa, dfb, roundDigits, keys, mismatches, trimChars, argsIn)
}
\arguments{
\item{dfa}{Dataframe. One of the two data frames to be compared}

\item{dfb}{Dataframe. One of the two data frames to be compared}

\item{roundDigits}{Integer. If NA, numerics are not rounded before comparison. If /code{roundDigits} is specified, numerics are 
rounded to /code{roundDigits} decimal places using \link[base]{round}.}

\item{keys}{The keys used to match rows between \code{dfa} and \code{dfb}}

\item{mismatches}{Integer. The max number of mismatches to assess, after which dataCompareR will stop 
(without producing a dataCompareR object). Designed to improve performance for large datasets.}

\item{trimChars}{Boolean. Do we trim characters before comparing?}

\item{argsIn}{The arguments that were passed to the main dataCompareR function}
}
\value{
\code{dataCompareRObject} containing details of the comparison
}
\description{
processFlow Handles the process flow for the whole package
}
