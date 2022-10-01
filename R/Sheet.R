#' @title Googlesheets Sheet specification
#' @description Specification of singular Sheet of the google spreadsheeet

Sheet <- function(
    properties = NULL,
    data = NULL,
    merges = NULL,
    conditionalFormats = NULL,
    filterViews = NULL,
    protectedRanges = NULL,
    basicFilter = NULL,
    charts = NULL,
    bandedRanges = NULL,
    developerMetadata = NULL,
    rowGroups = NULL,
    columnGroups = NULL,
    slicers = NULL) {

  out <- list() |>
    append_cond(properties, class = "SheetProperties") |>
    deepgs_class("Sheet")

  return(out)

}

#' @rdname Sheet
#' @param x any R object
is.Sheet <- function(x) {
  inherits(x, "Sheet")
}

# {
#   "properties": {
#     object (SheetProperties)
#   },
#   "data": [
#     {
#       object (GridData)
#     }
#   ],
#   "merges": [
#     {
#       object (GridRange)
#     }
#   ],
#   "conditionalFormats": [
#     {
#       object (ConditionalFormatRule)
#     }
#   ],
#   "filterViews": [
#     {
#       object (FilterView)
#     }
#   ],
#   "protectedRanges": [
#     {
#       object (ProtectedRange)
#     }
#   ],
#   "basicFilter": {
#     object (BasicFilter)
#   },
#   "charts": [
#     {
#       object (EmbeddedChart)
#     }
#   ],
#   "bandedRanges": [
#     {
#       object (BandedRange)
#     }
#   ],
#   "developerMetadata": [
#     {
#       object (DeveloperMetadata)
#     }
#   ],
#   "rowGroups": [
#     {
#       object (DimensionGroup)
#     }
#   ],
#   "columnGroups": [
#     {
#       object (DimensionGroup)
#     }
#   ],
#   "slicers": [
#     {
#       object (Slicer)
#     }
#   ]
# }
