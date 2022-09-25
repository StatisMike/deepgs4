#' @title GridProperties
#' @description Properties of a chart grid
#' @param rowCount,columnCount number of rows and columns in the grid
#' @param frozenRowCount,frozenColumnCount number of frozen rows and columns in the grid
#' @param hideGridlines boolean indicating if gridlines should be shown
#' @param rowGroupControlAfter,columnGroupControlAfter boolean indicating if
#' row/column toggle is shown after the group
#' @export
GridProperties <- function(
    rowCount,
    columnCount,
    frozenRowCount = NULL,
    frozenColumnCount = NULL,
    hideGridLines = NULL,
    rowGroupControlAfter = NULL,
    columnGroupControlAfter = NULL) {

  out <- list(rowCount = rowCount,
              columnCount = columnCount) |>
    append_cond(frozenRowCount, type = "integer") |>
    append_cond(frozenColumnCount, type = "integer") |>
    append_cond(hideGridLines, type = "logical") |>
    append_cond(rowGroupControlAfter, type = "logical") |>
    append_cond(columnGroupControlAfter, type = "logical") |>
    deepgs_class("GridProperties")

  return(out)

}

#' @title Generate GridProperties
#' @noRd
gen_GridProperties <- function(obj, sheetProperties = NULL)
  do.call(GridProperties, args = obj)

#' @title GridCoordinate
#' @description Specification of cell in a grid
#' @param sheetId Integer - sheet ID
#' @param rowIndex,columnIndex position of the cell in a grid
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
    deepgs_class("GridCoordinate")

  return(out)

}

#' @rdname GridCoordinate
#' @param x any R object
#' @export
is.GridCoordinate <- function(x)
  inherits(x, "GridCoordinate")

#' @title Generate GridRange
#' @description Function used internally by [SpreadSheetsData] object
#' @noRd
gen_GridCoordinate <- function(obj,
                               sheetProperties = NULL) {

  args <- obj

  if (is.null(obj$sheetId))
    args[["sheetId"]] <- sheetProperties$sheetId

  do.call(GridCoordinate, args = args)
}


#' @title GridRange
#' @description Specification of grid range in spreadsheet
#' @details Object representing `GridRange` received from GoogleSheets API v4
#' don't include `sheetId`. It is filled during response processing with `Id`
#' of the sheet which the higher-level object was located in.
#' @param sheetId Integer - sheet ID
#' @param startRowIndex Integer. Starts from 0, inclusive
#' @param endRowIndex Integer. Starts from 0, exclusive
#' @param startColumnIndex Integer. Starts from 0, inclusive
#' @param endColumnIndex Integer. Starts from 0, exclusive
#' @export
GridRange <- function(
    sheetId,
    startRowIndex,
    endRowIndex,
    startColumnIndex,
    endColumnIndex) {

  if (endRowIndex <= startRowIndex)
    deepgs_error("{.arg endRowIndex} needs to be greater than {.arg startRowIndex}",
                 class = "WrongIndexError")

  if (endColumnIndex <= startColumnIndex)
    deepgs_error("{.arg endColumnIndex} needs to be greater than {.arg startColumnIndex}",
                 class = "WrongIndexError")

  out <- list() |>
    append_cond(sheetId, type = "integer", skip_null = F) |>
    append_cond(startRowIndex, type = "integer", skip_null = F) |>
    append_cond(endRowIndex, type = "integer", skip_null = F) |>
    append_cond(startColumnIndex, type = "integer", skip_null = F) |>
    append_cond(endColumnIndex, type = "integer", skip_null = F) |>
    deepgs_class("GridRange")

  return(out)

}

#' @rdname GridRange
#' @param x any R object
#' @export
is.GridRange <- function(x) {
  inherits(x, "GridRange")
}

#' @title Generate GridRange
#' @description Function used internally by [SpreadSheetsData] object
#' @noRd
gen_GridRange <- function(obj,
                          sheetProperties = NULL) {

  args <- obj

  if (is.null(obj$sheetId))
    args[["sheetId"]] <- sheetProperties$sheetId

  do.call(GridRange, args = args)
}

