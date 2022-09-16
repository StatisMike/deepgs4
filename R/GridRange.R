#' @title GridRange
#' @description Specification of grid range in spreadsheet
#' @param sheetId Integer. Starts from 0. Spreadsheet Id
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

  class(out) <- "CellRange"

  return(out)

}

#' @rdname GridRange
#' @param x any R object
#' @export
is.GridRange <- function(x)
  inherits(x, "GridRange")
