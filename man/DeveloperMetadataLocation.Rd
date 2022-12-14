% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/DeveloperMetadata.R
\name{DeveloperMetadataLocation}
\alias{DeveloperMetadataLocation}
\alias{is.DeveloperMetadataLocation}
\title{Developer Metadata Location}
\usage{
DeveloperMetadataLocation(
  dimensionRange = NULL,
  sheetId = NULL,
  spreadsheet = NULL,
  locationType = NULL
)

is.DeveloperMetadataLocation(x)
}
\arguments{
\item{dimensionRange}{Object of class \link{DimensionRange}. Represents the row or
column when metadata is associated with a dimension. The specified
range must represent a single row or column. It cannot be unbounded or
span multiple rows or columns.}

\item{sheetId}{ID of the sheet. Specified if the metadata represents entire sheet.}

\item{spreadsheet}{if TRUE then metadata represents entire spreadsheet}

\item{locationType}{\strong{READ ONLY} The type of location this object represents.
Can be one of: \code{"ROW"}, \code{"COLUMN"}, \code{"SHEET"} or \code{"SPREADSHEET"}}

\item{x}{any R object}
}
\description{
Object that represents the location of given \link{DeveloperMetadata}
on the spreadsheet. Only one of \code{dimensionRange}, \code{sheetId} or \code{spreadsheet}
arguments can be specified.
}
