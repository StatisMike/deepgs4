% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/CellsRequests.R
\name{UpdateBordersRequest}
\alias{UpdateBordersRequest}
\title{Update borders of a given range}
\usage{
UpdateBordersRequest(
  range,
  top = NULL,
  bottom = NULL,
  left = NULL,
  right = NULL,
  innerHorizontal = NULL,
  innerVertical = NULL
)
}
\arguments{
\item{range}{object of class \link{GridRange}}

\item{top, bottom, left, right}{objects of class \link{Border}, specifying borders
to put at the outside of the range}

\item{innerHorizontal, innerVertical}{objects of class \link{Border}, specifying
borders to put between the cells inside the range}
}
\value{
deepgsheets4Req object
}
\description{
Generate request to update borders of a given range of cells.
Can specify both cell borders on outside of range and within the range.
If a field is not set in the request, that means the border remains as-is:
to clear a border, explicitly set the style to \code{"NONE"}.
}
\seealso{
Other deepgsheets4Req constructors: 
\code{\link{DimensionGroupRequests}},
\code{\link{MergeRequests}}
}
\concept{deepgsheets4Req constructors}
