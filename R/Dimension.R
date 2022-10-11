#' @title Properties about dimension
#' @description Describe a properties of row or column in a sheet
#' @param pixelSize Height (if row) or width (if column) of the dimension in
#' pixels
#' @param developerMetadata object of class [DeveloperMetadata] or list of such
#' objects. Developer metadata associated with single row or column
#' @param hiddenByUser `TRUE` if this dimension is explicitly hidden
#' @param hiddenByFilter **READ ONLY** `TRUE` if this dimension is being filtered
#' @param dataSourceColumnReference **READ ONLY** if this is a column in a
#' data source sheet, the name of the column.
#' @export
DimensionProperties <- function(
    pixelSize = NULL,
    developerMetadata = NULL,
    hiddenByUser = NULL,
    hiddenByFilter = NULL,
    dataSourceColumnReference = NULL) {

  developerMetadata <- nest_if_class(developerMetadata, "DeveloperMetadata") |>
    check_if_all_class("DeveloperMetadata")

  out <- list() |>
    append_cond(pixelSize, type = "integer") |>
    append_cond(developerMetadata) |>
    append_cond(hiddenByUser, type = "logical") |>
    append_cond(hiddenByFilter, type = "logical") |>
    append_cond(dataSourceColumnReference, type = "character") |>
    deepgs_class("DimensionProperties")

  return(out)

}

#' @rdname DimensionProperties
#' @param x any R object
is.DimensionProperties <- function(x) {
  inherits(x, "DimensionProperties")
}

#' @title Dimension Range
#' @description Object representing range alongside one of the dimensions.
#' @param sheetId ID of the sheet this range is on
#' @param dimension type of the dimension
#' @param startIndex,endIndex zero-based indices bounding the range. `startIndex`
#' is inclusive, `endIndex` is exclusive. Missing indices indicate the range is
#' unbounded on that side.
#' @export
DimensionRange <- function(
    sheetId,
    dimension = c("ROWS", "COLUMNS"),
    startIndex = NULL,
    endIndex = NULL) {

  if (is.null(startIndex))
    startIndex <- 0

  dimension <- rlang::arg_match(dimension)

  out <- list() |>
    append_cond(sheetId, type = "integer", skip_null = FALSE) |>
    append_cond(dimension) |>
    append_cond(startIndex, type = "integer") |>
    append_cond(startIndex, type = "integer") |>
    deepgs_class("DimensionRange")

  return(out)

}

#' @rdname DimensionRange
#' @param x any R object
#' @export
is.DimensionRange <- function(x) {
  inherits(x, "DimensionRange")
}
