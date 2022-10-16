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
    dgs4_class("DimensionProperties")

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

  dimension <- rlang::arg_match(dimension)

  out <- list() |>
    append_cond(sheetId, type = "integer", skip_null = FALSE) |>
    append_cond(dimension) |>
    append_cond(startIndex, type = "integer") |>
    append_cond(endIndex, type = "integer") |>
    dgs4_class("DimensionRange")

  return(out)

}

#' @rdname DimensionRange
#' @param x any R object
#' @export
is.DimensionRange <- function(x) {
  inherits(x, "DimensionRange")
}

#' @title Dimension Group
#' @description A group over an interval of rows or columns on a sheet, which
#' can contain or be contained within other groups. A group can be collapsed
#' or expanded as a unit on the sheet.
#' @param range object of class [DimensionRange] that declares the range over
#' which this group exists.
#' @param depth the depth of the group - integer stating how many groups have a
#' range that wholly contains the range of this group.
#' @param collapsed This field is true if this group is collapsed. A collapsed
#' group remains collapsed if an overlapping group at a shallower depth is expanded.
#'
#' A `TRUE` value does not imply that all dimensions within the group are hidden,
#' since a dimension's visibility can change independently from this group
#' property. However, when this property is updated, all dimensions within it
#' are set to hidden if this field is `TRUE`, or set to visible if this field is `FALSE`.
#' @export
DimensionGroup <- function(range, depth = NULL, collapsed = NULL) {

  out <- list() |>
    append_cond(range, class = "DimensionRange", skip_null = FALSE) |>
    append_cond(depth, type = "integer") |>
    append_cond(collapsed, type = "logical") |>
    dgs4_class("DimensionGroup")

  return(out)

}

#' @rdname DimensionGroup
#' @param x any R object
#' @export
is.DimensionGroup <- function(x) {
  inherits(x, "DimensionGroup")
}
