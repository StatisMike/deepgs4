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

  out <- list(sheetId = sheetId,
              rowIndex = rowIndex,
              columnIndex = columnIndex)

  class(out) <- "GridCoordinate"

  return(out)

}

#' @rdname GridCoordinate
#' @param x any R object
#' @export
is.GridCoordinate <- function(x)
  inherits(x, "GridCoordinate")

#' @title GridRange
#' @description Specification of grid range in spreadsheet
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

  if (any(sapply(list(sheetId, startRowIndex, endRowIndex,
                      startColumnIndex, endColumnIndex),
                 \(x) !is.numeric(x) || x %% 1 != 0)))
    stop("sheetId and cell indices need to be integers")

  if (endRowIndex <= startRowIndex)
    stop("endRowIndex needs to be greater than startRowIndex")

  if (endColumnIndex <= startColumnIndex)
    stop("endColumnIndex needs to be greater than startRowIndex")

  out <- list(
    sheetId = sheetId,
    startRowIndex = startRowIndex,
    endRowIndex = endRowIndex,
    startColumnIndex = startColumnIndex,
    endColumnIndex = endColumnIndex
  )

  class(out) <- "GridRange"

  return(out)

}

#' @rdname GridRange
#' @param x any R object
#' @export
is.GridRange <- function(x)
  inherits(x, "GridRange")

#' @title Generate GridRange
#' @description Function used internally by [SpreadSheetsData] object
#' @noRd
gen_GridRange <- function(sheetProperties,
                          obj)
  do.call(GridRange,
          args = c(
            list(sheetId = sheetProperties$sheetId),
            obj
          ))
