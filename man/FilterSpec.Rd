% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Filters.R
\name{FilterSpec}
\alias{FilterSpec}
\alias{is.FilterSpec}
\title{FilterSpec}
\usage{
FilterSpec(
  filterCriteria,
  columnIndex = NULL,
  dataSourceColumnReference = NULL
)

is.FilterSpec(x)
}
\arguments{
\item{filterCriteria}{object of class \link{FilterCriteria}. The criteria for the column.}

\item{columnIndex}{Index of column this sort should be applied to, if
sort is related to column on \strong{GRID} sheet}

\item{dataSourceColumnReference}{Unique name of the column, if sort is related
to column on \strong{DATA_SOURCE} sheet}

\item{x}{any R object}
}
\description{
The \link{FilterCriteria} associated with a specific column. Exactly
one of \code{columnIndex} or \code{dataSourceColumnReference} needs to be specified.
}
