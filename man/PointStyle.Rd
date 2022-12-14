% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Varia.R
\name{PointStyle}
\alias{PointStyle}
\title{Chart point style specification}
\usage{
PointStyle(
  size = NULL,
  shape = c("POINT_SHAPE_UNSPECIFIED", "CIRCLE", "DIAMOND", "HEXAGON", "PENTAGON",
    "SQUARE", "STAR", "TRIANGLE", "X_MARK")
)
}
\arguments{
\item{size}{Point size. If empty, default size is used}

\item{shape}{type of the point. If kept as \code{"POINT_SHAPE_UNSPECIFIED"}, the
default shape is used.}
}
\description{
Object allowing for specifying the chart point style.
}
