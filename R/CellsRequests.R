#' @title Requests to update or append cells in the grid
#' @description
#' Create `dgs4Req` objects that allow for update or append
#' rows of cell data into sheet. Send created requests with [request_ss_batchUpdate()]
#' @param rows object of class [RowData] or list of multiple such objects
#' @param fields name of [CellData] fields to update. To get a list of valid fields,
#' check [valid_update_fields()]
#' @param start object of class [GridCoordinate]. The coordinate to start writing
#' data at. Any number of rows and columns (including a different number of
#' columns per row) may be written. Can't be specified alongside `range.`
#' @param range object of class [GridRange] or list of such objects.
#' The range to write data to. If the data in rows does not cover the entire
#' requested range, the fields matching those set in fields will be cleared.
#' Can't be specified alongside `start`.
#' @param sheetId The sheet ID to append the data to.
#' @name CellRequests
#' @rdname CellRequests
#' @aliases UpdateCellsRequest AppendCellsRequest
#' @family dgs4Req constructors
#' @return dgs4Req object
NULL

#' @rdname CellRequests
#' @section UpdateCells:
#' Update cells beginning at `start` (upper-left coordinate) or in `range`.
#' Only properties specified in `fields` will be updated. If a property
#' is specified in `fields` but is missing from `CellData`, the property
#' will be erased
#' @export
UpdateCellsRequest <- function(
    rows,
    fields = NULL,
    start = NULL,
    range = NULL) {

  arg_check <- vapply(list(start, range), is.null, logical(1))

  if (sum(arg_check) != 1)
    dgs4_error("Exactly one of {.arg start} and {.arg range} need to be provided for {.emph UpdateCellsRequest}.")

  fields <- check_valid_update_fields(fields, "UpdateCells")

  rows <- nest_if_class(rows, "RowData") |>
    check_if_all_class("RowData")

  out <- list() |>
    append_cond(fields, skip_null = FALSE) |>
    append_cond(rows, skip_null = FALSE) |>
    append_cond(start, class = "GridCoordinate") |>
    append_cond(range, class = "GridRange") |>
    dgs4_class("UpdateCells", "Req")

  return(out)

}

#' @rdname CellRequests
#' @section AppendCells:
#' Adds new cells after the last row with data in a sheet, inserting new rows
#' into the sheet if necessary. Properties of `CellData` specified in
#' `fields` will be inserted.
#' @export
AppendCellsRequest <- function(
    sheetId,
    rows,
    fields = NULL) {

  fields <- check_valid_update_fields(fields, "UpdateCells")
  fields <- paste(fields, collapse = ",")

  rows <- nest_if_class(rows, "RowData") |>
    check_if_all_class("RowData")

  out <- list() |>
    append_cond(sheetId, type = "integer", skip_null = F) |>
    append_cond(rows) |>
    append_cond(fields) |>
    dgs4_class("AppendCells", "Req")

  return(out)

}

#' @title Requests handling cell merges
#' @description
#' Create `deepgsheets4Req` objects to merge or unmerge cells in given range.
#' Send created requests with [send_batchUpdate_req()]
#' @param range object of class [GridRange] declaring the range in which
#' cells need to be merged or unmerged
#' @param mergeType type of the merge to create
#' @name MergeRequests
#' @rdname MergeRequests
#' @aliases MergeCellsRequest UnmergeCellsRequest
#' @family deepgsheets4Req constructors
#' @return deepgsheets4Req object
NULL

#' @rdname MergeRequests
#' @section MergeCells:
#' Merges all cells in the `range` in a way described by `mergeType`:
#' - **MERGE_COLUMNS** Create a merge for each column in the range
#' - **MERGE_ROWS** Create a merge for each row in the range
#' - **MERGE_ALL** Create a single merge from the range
#' @export
MergeCellsRequest <- function(
    range,
    mergeType = c("MERGE_COLUMNS", "MERGE_ROWS", "MERGE_ALL")) {

  mergeType <- rlang::arg_match(mergeType)

  out <- list() |>
    append_cond(range, class = "GridRange", skip_null = F) |>
    append_cond(mergeType) |>
    dgs4_class("MergeCells", "Req")

  return(out)

}

#' @rdname MergeRequests
#' @section UnmergeCells:
#' Unmerges cells in the given range. If the `range` spans multiple merges,
#' all will be unmerged. It must not span any merge partially
#' @export
UnmergeCellsRequest <- function(
    range) {

  out <- list() |>
    append_cond(range, class = "GridRange", skip_null = F) |>
    dgs4_class("UnmergeCells", "Req")

  return(out)

}

#' @title Update borders of a given range
#' @description Generate request to update borders of a given range of cells.
#' Can specify both cell borders on outside of range and within the range.
#' If a field is not set in the request, that means the border remains as-is:
#' to clear a border, explicitly set the style to `"NONE"`.
#' @param range object of class [GridRange]
#' @param top,bottom,left,right objects of class [Border], specifying borders
#' to put at the outside of the range
#' @param innerHorizontal,innerVertical objects of class [Border], specifying
#' borders to put between the cells inside the range
#'
#' @family deepgsheets4Req constructors
#' @return deepgsheets4Req object
#' @export
UpdateBordersRequest <- function(range,
                                 top = NULL,
                                 bottom = NULL,
                                 left = NULL,
                                 right = NULL,
                                 innerHorizontal = NULL,
                                 innerVertical = NULL) {

  obj <- list() |>
    append_cond(range, class = "GridRange", skip_null = FALSE) |>
    append_cond(top, class = "Border") |>
    append_cond(bottom, class = "Border") |>
    append_cond(left, class = "Border") |>
    append_cond(right, class = "Border") |>
    append_cond(innerHorizontal, class = "Border") |>
    append_cond(innerVertical, class = "Border") |>
    dgs4_class("UpdateBorders", "Req")

  return(obj)

}
