enc_sing_q <- function(x) {
  if (grepl(x, pattern = "\\s"))
    return(x)

  return(paste0("'", x, "'"))
}

check_unbounded_grid <- function(x, call = rlang::caller_call(),
                                 split_rows = FALSE, split_cols = FALSE) {

  # for now, until figured out
  if (is.null(x$endRowIndex) || is.null(x$endColumnIndex))
    deepgs_error("Currently cannot split on unbounded GridRange",
                 call = call)

  if ((is.null(x$endRowIndex) && isTRUE(split_rows)) ||
      (is.null(x$endColumnIndex) && isTRUE(split_cols)))
    deepgs_error("Cannot split on unbounded dimension.",
                 call = call)

  return(x)

}

#' @title Get sheet name from sheetVec or provided
#' @param sheetVec sheetVec
#' @param sheetName name of the sheet
#' @param sheetId sheet ID
#' @noRd
get_sheetName <- function(sheetVec = NULL, sheetName = NULL, sheetId = NULL) {

  if (is.null(sheetName) && is.null(sheetVec))
    return(NULL)

  if (!is.null(sheetVec) && !is.null(sheetName))
    deepgs_error("Only one of {.arg sheetName} or {.arg sheetVec} can be provided.",
                 call = rlang::caller_call(2))

  if (!is.null(sheetName))
    return(sheetName)

  if (is.null(sheetId))
    return(NULL)

  sheetVec <- check_if_class(sheetVec ,"sheetVec", call = rlang::caller_call(2))

  n <- which(sheetVec == sheetId)

  if (length(n) == 0)
    deepgs_error("No sheetId found. Try to refresh the data using `SpreadSheetData$get_data('sheets', refresh = TRUE)`}",
                 call = rlang::caller_call(1))

  return(names(sheetVec)[n])

}

#' @title Get A1 notation from `deepgsheets4` objects
#' @description All sheets API-viable objects hold the notation for in-sheet
#' location by presenting rows and cols in form of 0-based indices. For function
#' construction the legacy spreadsheet notation *A1* is required, though. This
#' generic allows for retrieval of such notation from objects holding this
#' information.
#' @param x object holding the R1C1 notation
#' @param strict should strict address (eg. `$A$1`) be retrieved
#' @param sheetVec object of class `sheetVec` describing the sheets.
#' Produced by [SpreadSheetData] object's `sheets` field
#' @param sheetName character with sheetName to append
#' @param ... further arguments passed to or from other methods
#' @importFrom cellranger ra_ref to_string
#' @return character vector or list (row-wise) containing all cell adresses in *A!* notation
#' @export
get_A1_not <- function(x, strict = TRUE, sheetName = NULL, sheetVec = NULL, ...) {
  UseMethod("get_A1_not", x)
}

#' @rdname get_A1_not
#' @export
get_A1_not.GridCoordinate <- function(x, strict = TRUE, sheetName = NULL,
                                      sheetVec = NULL, ...) {

  sheetName <- get_sheetName(sheetVec = sheetVec, sheetName = sheetName,
                             sheetId = x$sheetId)

  ra_ref(row_ref = x$rowIndex + 1,
         col_ref = x$columnIndex,
         sheet = if (is.null(sheetName)) NA_character_ else enc_sing_q(sheetName)) |>
    to_string(fo = "A1", sheet = !is.null(sheetName))

  return(out)

}

#' @rdname get_A1_not
#' @param split_rows,split_cols booleans - should the rows and columns be splitted,
#' before returning their *A1* notation?
#' @importFrom cellranger cell_limits as.range
#' @export
get_A1_not.GridRange <- function(x, strict = TRUE, sheetName = NULL,
                                 sheetVec = NULL, split_rows = FALSE,
                                 split_cols = FALSE, ...) {

  if (any(isTRUE(split_rows), isTRUE(split_cols))) {

    x <- sanitize_unbounded_grid(x = x,
                                 split_rows = split_rows,
                                 split_cols = split_cols)

    n_rows <- x$endRowIndex - x$startRowIndex
    n_cols <- x$endColumnIndex - x$startColumnIndex

    if (isTRUE(split_rows) && n_rows == 1)
      deepgs_error("Cannot split by rows - only one row available")

    if (isTRUE(split_cols) && n_cols == 1)
      deepgs_error("Cannot split by columns, only one column available")
  }

  sheetName <- get_sheetName(sheetVec = sheetVec, sheetName = sheetName,
                             sheetId = x$sheetId)

  if (isTRUE(split_rows) && isTRUE(split_cols)) {

    rows <- split_GridRange(gr = x, split = "row")
    out <- vapply(rows, \(row) {
      extract_GridCoordinates(row) |>
        vapply(get_A1_not, strict = strict, sheetName = sheetName, ... = ...,
               FUN.VALUE = character(1))
        }, FUN.VALUE = character(n_cols))

    return(out)

  }

  if (isTRUE(split_rows) || isTRUE(split_cols)) {

    split <- if (isTRUE(split_rows)) "row" else "col"

    splitted <- split_GridRange(x, split)
    out <- vapply(splitted, get_A1_not, strict = strict, sheetName = sheetName,
                  ... = ..., FUN.VALUE = character(1))

    return(out)

  }

  ul <- c(x$startRowIndex + 1,
          x$startColumnIndex + 1)

  lr <- c(if (is.null(x$endRowIndex)) NA_integer_ else x$endRowIndex,
          if (is.null(x$endColumnIndex)) NA_integer_ else x$endColumnIndex)

  limits <- cell_limits(
    ul = ul,
    lr = lr,
    sheet = if (is.null(sheetName)) NA_character_ else enc_sing_q(sheetName)
  )

  out <- as.range(limits, fo = "A1", strict = strict, sheet = !is.null(sheetName))

  return(out)

}
