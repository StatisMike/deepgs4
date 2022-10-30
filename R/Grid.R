#' @title GridProperties
#' @description Properties of a chart grid
#' @param rowCount,columnCount number of rows and columns in the grid
#' @param frozenRowCount,frozenColumnCount number of frozen rows and columns in the grid
#' @param hideGridLines boolean indicating if gridlines should be shown
#' @param rowGroupControlAfter,columnGroupControlAfter boolean indicating if
#' row/column toggle is shown after the group
#' @export
GridProperties <- function(
    rowCount = NULL,
    columnCount = NULL,
    frozenRowCount = NULL,
    frozenColumnCount = NULL,
    hideGridlines = NULL,
    rowGroupControlAfter = NULL,
    columnGroupControlAfter = NULL) {

  out <- list() |>
    append_cond(rowCount, type = "integer") |>
    append_cond(columnCount, type = "integer") |>
    append_cond(frozenRowCount, type = "integer") |>
    append_cond(frozenColumnCount, type = "integer") |>
    append_cond(hideGridlines, type = "logical") |>
    append_cond(rowGroupControlAfter, type = "logical") |>
    append_cond(columnGroupControlAfter, type = "logical") |>
    dgs4_class("GridProperties")

  return(out)

}

#' @rdname GridProperties
#' @param x any R object
#' @export
is.GridProperties <- function(x) {
  inherits(x, "GridProperties")
}

#' @rdname GridProperties
#' @param gp GridProperties object
#' @export
print.GridProperties <- function(gp) {

  cli_text("{.cls {class(gp)}}")
  cli_bullets(bulletize_fields(gp))
  return(invisible(gp))

}

#' @title GridCoordinate
#' @description Specification of cell in a grid
#' @param sheetId Integer - sheet ID
#' @param rowIndex,columnIndex position of the cell in a grid
#' @section Warning:
#' Objects representing `GridCoordinate` received from GoogleSheets API v4
#' don't include `sheetId`. It is filled during response processing with `sheetId`
#' of the sheet in which the object was located in.
#' @export
GridCoordinate <- function(
    sheetId,
    rowIndex,
    columnIndex
) {

  out <- list() |>
    append_cond(sheetId, type = "integer", skip_null = F) |>
    append_cond(rowIndex, type = "integer", skip_null = F) |>
    append_cond(columnIndex, type = "integer", skip_null = F) |>
    dgs4_class("GridCoordinate")

  return(out)

}

#' @rdname GridCoordinate
#' @param x any R object
#' @export
is.GridCoordinate <- function(x) {
  inherits(x, "GridCoordinate")
}

#' @rdname GridCoordinate
#' @param gc GridCoordinate object
#' @param A1 if `TRUE` then include *A1* notation. Can be set globally with
#' `options("deepgs4.A1" = TRUE)`
#' @param ... optional arguments to `print` methods.
#' @importFrom cli cli_text cli_bullets
#' @export
print.GridCoordinate <- function(gc, A1 = getOption("deepgs4.A1", FALSE), ...) {

  cli_text("{.cls {class(gc)}}")

  props <- c("*" = "{.field sheetId}: {gc$sheetId}",
             "*" = "{.field rowIndex}: {gc$rowIndex}",
             "*" = "{.field columnIndex}: {gc$columnIndex}",
             if (isTRUE(A1)) c("i" = "{.field A1}: {.emph {get_A1_not(gc, strict = F)}}"))

  cli_bullets(props)
  return(invisible(gc))

}

#' @title GridRange
#' @description Specification of grid range in spreadsheet
#' @section Warning:
#' Objects representing `GridRange` received from GoogleSheets API v4
#' don't include `sheetId`. It is filled during response processing with `sheetId`
#' of the sheet in which the object was located in.
#' @param sheetId Integer - sheet ID
#' @param startRowIndex,startColumnIndex,endRowIndex,endColumnIndex Zero-based
#' indices declaring the range on the grid. `start` indices are inclusive, `end`
#' indices are exclusive. Missing index means that the range is unbounded on that
#' side.
#' @export
GridRange <- function(
    sheetId,
    startRowIndex = NULL,
    endRowIndex = NULL,
    startColumnIndex = NULL,
    endColumnIndex = NULL) {

  if (isTRUE(endRowIndex <= startRowIndex))
    dgs4_error("{.arg endRowIndex} needs to be greater than {.arg startRowIndex}",
               class = "WrongIndexError")

  if (isTRUE(endColumnIndex <= startColumnIndex))
    dgs4_error("{.arg endColumnIndex} needs to be greater than {.arg startColumnIndex}",
               class = "WrongIndexError")

  out <- list() |>
    append_cond(sheetId, type = "integer", skip_null = F) |>
    append_cond(startRowIndex, type = "integer") |>
    append_cond(endRowIndex, type = "integer") |>
    append_cond(startColumnIndex, type = "integer") |>
    append_cond(endColumnIndex, type = "integer") |>
    dgs4_class("GridRange")

  null_indices <- vapply(list(startRowIndex, startColumnIndex, endRowIndex, endColumnIndex),
                         is.null, logical(1))

  attr(out, "unbounded") <-
    c("startRow", "startColumn", "endRow", "endColumn")[null_indices]

  return(out)

}

#' @rdname GridRange
#' @param x any R object
#' @export
is.GridRange <- function(x) {
  inherits(x, "GridRange")
}

#' @rdname GridRange
#' @param gr GridRange object
#' @param A1 if `TRUE` then include *A1* notation. Can be set globally with
#' `options("deepgs4.A1" = TRUE)`
#' @param ... optional arguments to `print` methods.
#' @importFrom cli cli_text cli_bullets
#' @export
print.GridRange <- function(gr, A1 = getOption("deepgs4.A1", FALSE), ...) {

  cli_text("{.cls {class(gr)}}")

  props <- bulletize_fields(gr)
  if (isTRUE(A1))
    props <- c(props, c("i" = "{.field A1}: {.emph {get_A1_not(gr, strict = F)}}"))

  if (length(attr(gr, "unbounded")) > 0)
    props <- c(props, c(">" = "{.field unbounded}: {.val {attr(gr, 'unbounded')}}"))

  cli_bullets(props)
  return(invisible(gr))

}

#' @title Check if the vector is coercible to date or datetime
#' @param x vector to check
#' @return bool
#' @noRd
is.coercible.dt <- function(x) {

  tryCatch({
    lubridate::as_datetime(x)
    TRUE },
    error = function(e) FALSE,
    warning = function(w) FALSE)

}

#' @title Manipulate GridRange object
#' @name manipulate_GridRange
#' @rdname manipulate_GridRange
#' @aliases split_GridRange extract_GridCoordinates
#' @description Functions to manipulate [GridRange] objects.
#'
#' - `split_GridRange` splits `gr` into collections of one-row or one-column
#' [GridRange] objects, depending on the `split` argument. Cannot split grid on
#' unbounded dimension.
#' - `extract_GridCoordinates` extracts [GridCoordinate] objects specifying all
#' cells cantained within the provided `gr`. Cannot extract if object specifies
#' unbounded range.
NULL

#' @rdname manipulate_GridRange
#' @param gr [GridRange] object
#' @param split how the split be made. With `split = cell` it return nested list of
#' [GridCoordinate] objects: outer list containing rows, inner list: cells in a range.
#' With other `split` the function returns list of [GridRange] objects splitted
#' accordingly
#' @export
split_GridRange <- function(gr, split = c("row", "col")) {

  split <- rlang::arg_match(split)

  if (is.null(gr$endRowIndex) && split == "row")
    dgs4_error("Cannot split by {.val row}: {.cls GridRange} spans unbinded rows")

  if (gr$endRowIndex - gr$startRowIndex == 1 && split == "row")
    dgs4_error("Cannot split by {.val row}: {.cls GridRange} specifies an one-row range")

  if (is.null(gr$endColumnIndex) && split == "col")
    dgs4_error("Cannot split by {.val col}: {.cls GridRange} spans unbinded columns")

  if (gr$endColumnIndex - gr$startColumnIndex == 1 && split == "col")
    dgs4_error("Cannot split by {.val col}: {.cls GridRange} specifies an one-column range")

  switch(split,
         row = lapply(seq(gr$startRowIndex,
                          gr$endRowIndex - 1, by = 1),
                      \(x) GridRange(gr$sheetId, x,
                                     x+1, gr$startColumnIndex,
                                     gr$endColumnIndex)),
         col = lapply(seq(gr$startColumnIndex,
                          gr$endColumnIndex - 1, by = 1),
                      \(x) GridRange(gr$sheetId, gr$startRowIndex,
                                     gr$endRowIndex,x,
                                     x+1)))

}

#' @rdname manipulate_GridRange
#' @param gr [GridRange] object
#' @export
extract_GridCoordinates <- function(gr) {

  if (is.null(gr$endRowIndex) || is.null(gr$endColumnIndex))
    dgs4_error("Cannot extract from unbounded {.cls GridRange)")

  row_indices <- seq(gr$startRowIndex, gr$endRowIndex - 1, by = 1)
  col_indices <- seq(gr$startColumnIndex, gr$endColumnIndex - 1, by = 1)
  grid_indices <- expand.grid(row = row_indices, col = col_indices)

  mapply(GridCoordinate, SIMPLIFY = FALSE, sheetId = gr$sheetId,
         rowIndex = grid_indices$row, columnIndex = grid_indices$col)

}
