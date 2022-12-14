% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ConditionalFormat.R
\name{ConditionalFormatRule}
\alias{ConditionalFormatRule}
\alias{is.ConditionalFormatRule}
\title{Conditional Format Rule}
\usage{
ConditionalFormatRule(ranges, booleanRule = NULL, gradientRule = NULL)

is.ConditionalFormatRule(x)
}
\arguments{
\item{ranges}{object of class \link{GridRange} or list of such objects. The ranges
that are formatted according to the condition. All ranges must be on the same
\code{GRID} sheet}

\item{booleanRule}{object of class \link{BooleanRule}. A rule that is either applied
or not depending to the condition}

\item{gradientRule}{object of class \link{GradientRule}. Formatting will be varied
based on the gradients in the rule.}

\item{x}{any R object}
}
\description{
Rule describing the usage of conditional formatting. Only one
of \code{booleanRule} or \code{gradientRule} can be specified
}
