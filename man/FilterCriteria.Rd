% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Filters.R
\name{FilterCriteria}
\alias{FilterCriteria}
\alias{is.FilterCriteria}
\title{FilterCriteria}
\usage{
FilterCriteria(
  hiddenValues = NULL,
  condition = NULL,
  visibleBackgroundColorStyle = NULL,
  visibleForegroundColorStyle = NULL,
  ...
)

is.FilterCriteria(x)
}
\arguments{
\item{hiddenValues}{character vector specifying values that should be hidden}

\item{condition}{object of class \link{BooleanCondition}. A condition that must be
\code{TRUE} for values to be shown. Doesn't override values specified in \code{hiddenValues}:
even if it returns \code{TRUE} but is specified there, it will still be hidden.}

\item{visibleBackgroundColorStyle, visibleForegroundColorStyle}{objects of class \link{ColorStyle}. The background or foreground
fill color to filter by; only cells with this color are shown. Must be
specified with explicit \strong{RGB} specification}

\item{...}{for deprecated fields returned from Sheets API}

\item{x}{any R object}
}
\description{
Criteria for showing/hiding rows in a filter or filter view.
}
