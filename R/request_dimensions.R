#' @title Requests related to dimensions (rows/columns) in a sheet
#' @description
#' Create `deepgsheets4Req` objects that allows inserting, appending, deletion
#' or updating a dimension in a sheet. Send created requests with
#' [send_batchUpdate_req()]
#' @param range object of class [DimensionRange] declaring range of dimension
#' in `GRID` sheet
#' @param inheritFromBefore boolean indicating if the dimension properties
#' should be inherited from before the `range` (if TRUE) or from after the range
#' (FALSE)
#' @param sheetId ID of the sheet to append rows/columns
#' @param length number of rows/columns to append
#' @param dimension either `ROWS` or `COLUMNS` to check
#' @param properties object of class [DimensionProperties] providing data to
#' update
#' @param fields which fields of current properties need to be updated
#' @param dataSourceSheetRange object of class [DataSourceDimensionRange] specifying
#' the range of columns in `DATA_SOURCE` sheet
#' @param dimensions,DataSourceSheetDimensionRange objects of class, sequentially:
#' [DimensionRange] or [DataSourceDimensionRange], declaring the range in `GRID`
#' or `DATA_SOURCE` sheet to resize automatically. Exactly one of these arguments
#' needs to be provided.
#' @param source object of class [DimensionRange] declaring the source range
#' for move
#' @param destinationIndex index of the first column or row to where the dimensions
#' should be moved. Provide the index as in **before** the move.
#'
#' @name DimensionRequests
#' @rdname DimensionRequests
#' @aliases InsertDimensionRequest AppendDimensionRequest UpdateDimensionPropertiesRequest
#' AutoResizeDimensionsRequest DeleteDimensionRequest
#' @family deepgsheets4Req constructors
#' @return deepgsheets4Req object
NULL

#' @rdname DimensionRequests
#' @section Insert:
#' Inserts rows or columns in a sheet at a particular range Both `startIndex`
#' and `endIndex` must be bounded. Existing cells will be shifted to make place
#' for inserted dimension.
#' @export
InsertDimensionRequest <- function(range, inheritFromBefore = NULL) {

  req <- list() |>
    append_cond(range, class = "DimensionRange", skip_null = FALSE) |>
    append_cond(inheritFromBefore, type = "logical")

  obj <- list(
    insertDimension = req
  ) |>
    dgs4_class(object_type = "Req")

  return(obj)

}

#' @rdname DimensionRequests
#' @section Append:
#' Appends given number of rows or columns to the end of a sheet.
#' @export
AppendDimensionRequest <- function(
    sheetId,
    length,
    dimension = c("ROWS", "DIMENSIONS")) {

  dimension <- rlang::arg_match(dimension)

  req <- list() |>
    append_cond(sheetId, type = "integer", skip_null = FALSE) |>
    append_cond(length, type = "integer", skip_null = FALSE) |>
    append_cond(dimension)

  obj <- list(appendDimension = req) |>
    dgs4_class(object_type = "Req")

  return(obj)

}

#' @rdname DimensionRequests
#' @section UpdateProperties:
#' Send a request to update properties of dimension (column/row) in
#' a given range. Range can be specified with either `range` (for `GRID` sheets)
#' or `dataSourceSheetRange` (for `DATA_SOURCE` sheets) arguments
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

  if (sum(ranges_provided) != 1)
    dgs4_error("Exactly one of {.arg range} or {.arg dataSourceSheetRange} need to be provided")

  fields <- rlang::arg_match(fields, multiple = TRUE)
  fields <- paste(fields, collapse = ",")

  req <- list() |>
    append_cond(properties, class = "DimensionProperties", skip_null = FALSE) |>
    append_cond(fields) |>
    append_cond(range, class = "DimensionRange") |>
    append_cond(dataSourceSheetRange, class = "DataSourceSheetDimensionRange")

  out <- list(updateDimensionProperties = req) |>
    dgs4_class(object_type = "Req")

  return(out)

}

#' @rdname DimensionRequests
#' @section AutoResize:
#' Automatically resizes dimensions based on the contents of the cells in that
#' dimension.
#' @export
AutoResizeDimensionsRequest <- function(dimensions = NULL,
                                        dataSourceSheetDimensions = NULL) {

  ranges_provided <- vapply(list(dimensions, dataSourceSheetDimensions),
                            is.null,
                            logical(1))

  if (sum(ranges_provided) != 1)
    dgs4_error("Exactly one of {.arg dimensions} or {.arg dataSourceSheetDimensions} need to be provided")

  req <- list() |>
    append_cond(dimensions, class = "DimensionRange") |>
    append_cond(dataSourceSheetDimensions, "DataSourceDimensionRange")

  obj <- list(autoResizeDimensions = req) |>
    dgs4_class(object_type = "Req")

  return(obj)

}

#' @rdname DimensionRequests
#' @section Move:
#' Moves one or more rows or columns declared by `source` into destination
#' marked by `destinationIndex` (index of first row or column)
#' @export
MoveDimensionRequest <- function(source, destinationIndex) {

  req <- list() |>
    append_cond(source, class = "DimensionRange", skip_null = FALSE) |>
    append_cond(destinationIndex, type = "integer", skip_null = FALSE)

  obj <- list(moveDimension = req) |>
    dgs4_class(object_type = "Req")

  return(obj)

}

#' @rdname DimensionRequests
#' @section Delete:
#' Deletes the dimensions from the sheet in the given `range`. Other cells
#' will be shifted in their place.
#' @export
DeleteDimensionRequest <- function(range) {

  req <- list() |>
    append_cond(range, class = "DimensionRange", skip_null = FALSE)

  obj <- list(
    deleteDimension = req
  ) |>
    dgs4_class(object_type = "Req")

  return(obj)

}

#' @title Requests related to Dimension Groups
#' @description
#' Create `deepgsheets4Req` objects that allow addition, update and deletion of
#' dimension groups on the sheet. Send created requests with [send_batchUpdate_req()]
#' @param range object of class [DimensionRange] declaring the range of the
#' operation
#'
#' @name DimensionGroupRequests
#' @rdname DimensionGroupRequests
#' @aliases AddDimensionGroupRequest DeleteDimensionGroupRequest
#' UpdateDimensionGroupRequest
#' @family deepgsheets4Req constructors
#' @return deepgsheets4Req object
NULL

#' @rdname DimensionGroupRequests
#' @section Add:
#' Creates a group over the specified range.
#'
#' ## Depth incrementation and groups unification
#'
#' - If the requested range is a superset of the range of an existing group G,
#'   then the depth of G is incremented and this new group G' has the depth of
#'   that group.
#'
#'   For example: a group **C:D** with depth of 1 + group **B:E** results in groups:
#'   **B:E** of depth 1 and **C:D** of depth 2.
#'
#' - If the requested range is a subset of the range of an existing group G,
#'   then the depth of the new group G' becomes one greater than the depth of G.
#'
#'   For example, a group **B:E** of depth 1 + group **C:D** results in groups
#'   **B:E** of depth 1 and **C:D** of depth 2.
#'
#' - If the requested range starts before and ends within, or starts within
#'   and ends after, the range of an existing group G, then the range of the
#'   existing group G becomes the union of the ranges, and the new group G'
#'   has depth one greater than the depth of G and range as the intersection
#'   of the ranges.
#'
#'   For example, a group **B:D** of depth 1 + **C:E** results in groups
#'   **B:E** of depth 1 and **C:D** of depth 2.
#' @export
AddDimensionGroupRequest <- function(range) {

  req <- list() |>
    append_cond(range, class = "DimensionRange", skip_null = F)

  obj <- list(addDimensionGroup = req) |>
    dgs4_class(object_type = "Req")

  return(obj)

}

#' @rdname DimensionGroupRequests
#' @section Delete:
#' Deletes a group over the specified range by decrementing the depth of the
#' dimensions in the range.
#'
#' ## Depth decrementation
#' For example, assume the sheet has a group **B:E** of depth 1 and group **C:D**
#' of depth 2. Deleting a group over `D:E` range leaves the sheet with a
#' group **B:D** of depth 1 and group **C:C** of depth 2.
#' @export
DeleteDimensionGroupRequest <- function(range) {

  req <- list() |>
    append_cond(range, class = "DimensionRange", skip_null = F)

  obj <- list(deleteDimensionGroup = req) |>
    dgs4_class(object_type = "Req")

  return(obj)

}

#' @rdname DimensionGroupRequests
#' @section Update:
#' Updates the state of specified group. For now, the only field that
#' can be updated is `collapsed` - so you can collapse and un-collapse
#' to dimension group identified on basis of other fields in `dimensionGroup`
#' @export
UpdateDimensionGroupRequest <- function(dimensionGroup,
                                        fields = c("collapsed")) {

  fields <- rlang::arg_match(fields)

  req <- list() |>
    append_cond(dimensionGroup, class = "DimensionGroup", skip_null = FALSE) |>
    append_cond(fields)

  obj <- list(updateDimensionGroup = req) |>
    dgs4_class(object_type = "Req")

  return(obj)

}
