#' @title Request update of dimension properties
#' @description Send a request to update properties of dimension (column/row) in
#' a given range. Range can be specified with either `range` (for `GRID` sheets)
#' or `dataSourceSheetRange` (for `DATA_SOURCE` sheets) arguments
#' @param properties object of class [DimensionProperties] providing data to
#' update
#' @param fields which fields of current properties need to be updated
#' @param range object of class [DimensionRange] specifying the range of columns
#' or rows to update on `GRID` sheets
#' @param dataSourceSheetRange object of class `DataSourceDimensionRange` specifying
#' the range of columns in `DATA_SOURCE` sheet
#' @export
#'

UpdateDimensionPropertiesRequest <- function(
    properties,
    fields = c("pixelSize", "developerMetadata", "hiddenByUser"),
    range = NULL,
    dataSourceSheetRange = NULL) {

  ranges_provided <- vapply(list(range, dataSourceSheetRange),
                            is.null,
                            logical(1))

  if (sum(ranged_provided) != 1)
    deepgs_error("Exactly one of {.arg range} or {.arg dataSourceSheetRange} need to be provided")

  fields <- rlang::arg_match(fields, multiple = TRUE)
  fields <- paste(fields, collapse = ",")

  req <- list() |>
    append_cond(properties, class = "DimensionProperties") |>
    append_cond(fields) |>
    append_cond(range, class = "DimensionRange") |>
    append_cond(dataSourceSheetRange, class = "DataSourceSheetDimensionRange")

  out <- list(updateDimensionProperties = req) |>
    deepgs_class(object_type = "Req")

  return(out)

}
