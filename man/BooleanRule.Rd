% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ConditionalFormat.R
\name{BooleanRule}
\alias{BooleanRule}
\alias{is.BooleanRule}
\title{Boolean Rule}
\usage{
BooleanRule(condition, format)

is.BooleanRule(x)
}
\arguments{
\item{condition}{object of class \link{BooleanCondition}. If the condition
is \code{TRUE}, then the \code{format} is applied}

\item{format}{object of class \link{CellFormat}. Conditional formatting can only
apply a subset of \code{bold}, \code{italic}, \code{strikethrough}, \code{foregroundColor} and
\code{backgroundColor}}

\item{x}{any R object}
}
\description{
A rule for conditional formatting that may match or may not,
depending on condition provided. Used in \code{\link[=ConditionalFormatRule]{ConditionalFormatRule()}}
}
