% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Cells.R
\name{GridData}
\alias{GridData}
\alias{is.GridData}
\title{Specification of data in a grid}
\usage{
GridData(
  startRow = NULL,
  startColumn = NULL,
  rowData = NULL,
  rowMetadata = NULL,
  columnMetadata = NULL
)

is.GridData(x)
}
\arguments{
\item{startRow, startColumn}{zero-based indices of first row and column
this \code{GridData} refers to}

\item{rowData}{object of class \link{RowData} or list of such objects. Describes
data of the cells, one entry per row}

\item{rowMetadata, columnMetadata}{objects of class \link{DimensionProperties} or
lists of such objects. Properties of requested rows and columns in the grid
(starting with ones in \code{startRow} and \code{startColumn})}

\item{x}{any R object}
}
\description{
Data in a grid, as well as its dimensions metadata. This function
is a low-level constructor for \code{GridData}, requiring other \code{deepgsheets4}
objects. For higher-level wrapper, use \code{\link[=to_GridData_from_df]{to_GridData_from_df()}}.
}
