% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/request_spreadsheets.R
\name{request_ss_create}
\alias{request_ss_create}
\title{Create new spreadsheet through Sheets API}
\usage{
request_ss_create(spreadsheet, add_params = list())
}
\arguments{
\item{spreadsheet}{object of class \link{Spreadsheet}}

\item{add_params}{Named list of additional parameters to include in the request}
}
\value{
\link{Spreadsheet}
}
\description{
Sends request to the \href{https://developers.google.com/sheets/api/reference/rest/v4/spreadsheets/create}{spreadsheets.create}
method. Creates a new spreadsheet on basis of provided \link{Spreadsheet}. Returns
newly created spreadsheet as it present on Google Sheets.
}
\seealso{
Other Sheets Spreadsheets requests: 
\code{\link{request_ss_batchUpdate}()},
\code{\link{request_ss_get}()}
}
\concept{Sheets Spreadsheets requests}
