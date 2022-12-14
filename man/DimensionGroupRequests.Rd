% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/DimensionRequests.R
\name{DimensionGroupRequests}
\alias{DimensionGroupRequests}
\alias{AddDimensionGroupRequest}
\alias{DeleteDimensionGroupRequest}
\alias{UpdateDimensionGroupRequest}
\title{Requests related to Dimension Groups}
\usage{
AddDimensionGroupRequest(range)

DeleteDimensionGroupRequest(range)

UpdateDimensionGroupRequest(dimensionGroup, fields = c("collapsed"))
}
\arguments{
\item{range}{object of class \link{DimensionRange} declaring the range of the
operation}
}
\value{
deepgsheets4Req object
}
\description{
Create \code{deepgsheets4Req} objects that allow addition, update and deletion of
dimension groups on the sheet. Send created requests with \code{\link[=send_batchUpdate_req]{send_batchUpdate_req()}}
}
\section{Add}{

Creates a group over the specified range.
\subsection{Depth incrementation and groups unification}{
\itemize{
\item If the requested range is a superset of the range of an existing group G,
then the depth of G is incremented and this new group G' has the depth of
that group.

For example: a group \strong{C:D} with depth of 1 + group \strong{B:E} results in groups:
\strong{B:E} of depth 1 and \strong{C:D} of depth 2.
\item If the requested range is a subset of the range of an existing group G,
then the depth of the new group G' becomes one greater than the depth of G.

For example, a group \strong{B:E} of depth 1 + group \strong{C:D} results in groups
\strong{B:E} of depth 1 and \strong{C:D} of depth 2.
\item If the requested range starts before and ends within, or starts within
and ends after, the range of an existing group G, then the range of the
existing group G becomes the union of the ranges, and the new group G'
has depth one greater than the depth of G and range as the intersection
of the ranges.

For example, a group \strong{B:D} of depth 1 + \strong{C:E} results in groups
\strong{B:E} of depth 1 and \strong{C:D} of depth 2.
}
}
}

\section{Delete}{

Deletes a group over the specified range by decrementing the depth of the
dimensions in the range.
\subsection{Depth decrementation}{

For example, assume the sheet has a group \strong{B:E} of depth 1 and group \strong{C:D}
of depth 2. Deleting a group over \code{D:E} range leaves the sheet with a
group \strong{B:D} of depth 1 and group \strong{C:C} of depth 2.
}
}

\section{Update}{

Updates the state of specified group. For now, the only field that
can be updated is \code{collapsed} - so you can collapse and un-collapse
to dimension group identified on basis of other fields in \code{dimensionGroup}
}

\seealso{
Other deepgsheets4Req constructors: 
\code{\link{MergeRequests}},
\code{\link{UpdateBordersRequest}()}
}
\concept{deepgsheets4Req constructors}
