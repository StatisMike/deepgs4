% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/request_spreadsheets.R
\name{request_ss_get}
\alias{request_ss_get}
\alias{request_ss_getByDataFilter}
\title{Get spreadsheet object from Sheets API}
\usage{
request_ss_get(
  spreadsheetId,
  fields = NULL,
  ranges = NULL,
  includeGridData = NULL,
  add_params = list()
)

request_ss_getByDataFilter(
  spreadsheetId,
  dataFilters,
  fields = NULL,
  includeGridData = NULL,
  add_params = list()
)
}
\arguments{
\item{spreadsheetId}{ID of the spreadsheet}

\item{fields}{Fields to get in \emph{FieldMask} notation.}

\item{ranges}{Specified ranges in \emph{A1} notation. See \link{get_A1_not} for more info.}

\item{includeGridData}{Should the \link{GridData} will also be received. By default
they will not be returned. Ignored if \code{fields} are specified.}

\item{add_params}{Named list of additional parameters to include in the request}

\item{dataFilters}{object of class \link{DataFilter} or list of such objects.}
}
\value{
\link{Spreadsheet}
}
\description{
Sends request to the \href{https://developers.google.com/sheets/api/reference/rest/v4/spreadsheets/get}{spreadsheets.get}
method. Returns an instance of \link{Spreadsheet}, holding data inside specified
Google Sheet spreadsheet. Other arguments can narrow down the data to get.

Both functions allow specifying additional range to get only specific portions
of the spreadsheet. With \code{request_ss_get()} you can specify \code{ranges} in \emph{A1}
notation to get only, while \code{request_ss_get_byDataFilter()} allows specifying
\link{DataFilter} objects.
}
\seealso{
Other Sheets Spreadsheets requests: 
\code{\link{request_ss_batchUpdate}()},
\code{\link{request_ss_create}()}
}
\concept{Sheets Spreadsheets requests}
