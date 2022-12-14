% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/DataSource.R
\name{DataSourceParameter}
\alias{DataSourceParameter}
\alias{is.DataSourceParameter}
\title{DataSourceParameter}
\usage{
DataSourceParameter(name, namedRangeId = NULL, range = NULL)

is.DataSourceParameter(x)
}
\arguments{
\item{name}{Named parameter. Must be a legitimate identifier for the \link{DataSource}
that supports it. For example, BigQuery identifier.}

\item{namedRangeId}{ID of a \link{NamedRange.} Its size must be 1x1.}

\item{range}{A \link{GridRange} that contains the value of the parameter. Its size must be 1x1.}

\item{x}{any R object}
}
\description{
A parameter in a data source's query. The parameter allows
the user to pass in values from the spreadsheet into a query. Exactly
one of \code{namedRangeId} or \code{range} needs to be specified.
}
