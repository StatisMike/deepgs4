#' @title GridProperties
#' @description Properties of a chart grid
#' @param rowCount,columnCount number of rows and columns in the grid
#' @param frozenRowCount,frozenColumnCount number of frozen rows and columns in the grid
#' @param hideGridLines boolean indicating if gridlines should be shown
#' @param rowGroupControlAfter,columnGroupControlAfter boolean indicating if
#' row/column toggle is shown after the group
#' @export
GridProperties <- function(
    rowCount,
    columnCount,
    frozenRowCount = NULL,
    frozenColumnCount = NULL,
    hideGridlines = NULL,
    rowGroupControlAfter = NULL,
    columnGroupControlAfter = NULL) {

  out <- list(rowCount = rowCount,
              columnCount = columnCount) |>
    append_cond(frozenRowCount, type = "integer") |>
    append_cond(frozenColumnCount, type = "integer") |>
    append_cond(hideGridlines, type = "logical") |>
    append_cond(rowGroupControlAfter, type = "logical") |>
    append_cond(columnGroupControlAfter, type = "logical") |>
    deepgs_class("GridProperties")

  return(out)

}

#' @rdname GridProperties
#' @param x any R object
#' @export
is.GridProperties <- function(x) {
  inherits(x, "GridProperties")
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
    deepgs_class("GridCoordinate")

  return(out)

}

#' @rdname GridCoordinate
#' @param x any R object
#' @export
is.GridCoordinate <- function(x) {
  inherits(x, "GridCoordinate")
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

  if (is.null(startRowIndex))
    startRowIndex <- 0

  if (is.null(startColumnIndex))
    startColumnIndex <- 0

  if (isTRUE(endRowIndex <= startRowIndex))
    deepgs_error("{.arg endRowIndex} needs to be greater than {.arg startRowIndex}",
                 class = "WrongIndexError")

  if (isTRUE(endColumnIndex <= startColumnIndex))
    deepgs_error("{.arg endColumnIndex} needs to be greater than {.arg startColumnIndex}",
                 class = "WrongIndexError")

  out <- list() |>
    append_cond(sheetId, type = "integer", skip_null = F) |>
    append_cond(startRowIndex, type = "integer") |>
    append_cond(endRowIndex, type = "integer") |>
    append_cond(startColumnIndex, type = "integer") |>
    append_cond(endColumnIndex, type = "integer") |>
    deepgs_class("GridRange")

  return(out)

}

#' @rdname GridRange
#' @param x any R object
#' @export
is.GridRange <- function(x) {
  inherits(x, "GridRange")
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


#' @title Guess column types from `data.frame`
#' @param x data.frame
#' @return vector of types indicators
#' @noRd
guess_data_types <- function(x) {

  sapply(x, \(col) {

    if (is.logical(col))
      return("b")
    if (is.numeric(col))
      return("n")
    if (is.factor(col))
      return("c")
    if (is.coercible.dt(col))
      return("dt")

    return("c")
  })
}

#' @title Handle value types
#' @description Object to handle column and name types
#' @param x vector of types
#' @param df data.frame
#' @param which either names or values
#' @return list containing character vector of types
#' @noRd
handle_val_types <- function(x, df, which = c("names", "values")) {

  which <- rlang::arg_match(which)

  if (is.null(x)) {
    x <- switch(which,
                names = "c",
                values = guess_data_types(df))
  }

  if (length(x) == 1) {
    x <- rep(x, times = ncol(df))
  }

  if (length(x) != ncol(df))
    deepgs_error("Either {.val NULL}, {.val 1} or as many types as columns or names need to provided to {.arg value_types} or {.arg names_types} arguments.",
                 call = rlang::caller_env(2))

  return(x)

}

#' @title Construct CellData from value and type
#' @param value,type,format args provided in lapply in [to_GridData_from_df()]
#' @noRd
construct_CellData_from_types <- function(
    value,
    type,
    format) {

  switch(
    type,
    n = CellData(userEnteredValue = ExtendedValue(numberValue = as.numeric(value)),
                 userEnteredFormat = if (!is.null(format)) format),
    c = CellData(userEnteredValue = ExtendedValue(stringValue = as.character(value)),
                 userEnteredFormat = if (!is.null(format)) format),
    b = CellData(userEnteredValue = ExtendedValue(boolValue = as.logical(value)),
                 userEnteredFormat = if (!is.null(format)) format),
    f = CellData(userEnteredValue = ExtendedValue(formulaValue = as.character(value)),
                 userEnteredFormat = if (!is.null(format)) format),
    dt = {

      value <- deepgs_serial_number(unlist(value))

      to_date <- value %% 1 == 0

      if (is.null(format))
        format <- CellFormat(numberFormat = NumberFormat(if (to_date) "DATE" else "DATE_TIME"))
      else if (is.null(format$numberFormat))
        format$numberFormat <- NumberFormat(if (to_date) "DATE" else "DATE_TIME")

      CellData(userEnteredValue = ExtendedValue(numberValue = value),
               userEnteredFormat = format)

    })
}

#' @title Create RowData or GridData objects out of `data.frame`
#' @description Higher-level wrapper for mass [RowData] creation, taking as input
#' data in form of `data.frame`.
#' @param df `data.frame` object to use the data from
#' @inheritParams GridData
#' @param names_format,values_format object of class [CellFormat] that provides
#' format for column names and values
#' @param names_types,values_types vector of types to use for column names and
#' values. If nothing is provided, types are guessed for `value_types` (`names_types`)
#' are always presumed to be of `c` type. For more info read *details* section
#' @details
#' `names_types` and `values_types` could be provided as singular value, in which
#' case ALL names and ALL columns will be saved as the same class, or as a vector
#' of the same length as number of columns, in which case the every column will
#' have its own class provided.
#'
#' Possible values are:
#' - `n` for values formatted as numeric
#' - `c` for values formatted as character
#' - `b` for values formatted as boolean (logical)
#' - `dt` for values to be coerced by [deepgs_serial_number()]
#' - `f` if values are characters that need to be interpreted as googlesheets
#' formulas. This type is never guessed.
#'
#' If `values_types` argument is kept as `NULL`, the types will be interpreted
#' from data.frame column type.
#' @name rd_gd_from_df
#' @rdname rd_gd_from_df
#' @aliases to_RowData_from_df to_GridData_from_df
NULL

#' @rdname rd_gd_from_df
#' @export
to_RowData_from_df <- function(
    df,
    names_format = NULL,
    values_format = NULL,
    names_types = NULL,
    values_types = NULL) {

  if (!is.data.frame(df))
    deepgsheets4:::deepgs_error("Object provided to {.arg df} needs to be a {.cls data.frame}.")

  df <- as.data.frame(df)

  names_types <- handle_val_types(names_types, df, "names")
  values_types <- handle_val_types(values_types, df, "values")

  names_format <- check_if_class(names_format, "CellFormat")
  values_format <- check_if_class(values_format, "CellFormat")

  # handle names
  names_cells <- lapply(
    seq_along(names(df)), \(i)
    construct_CellData_from_types(
      value = names(df)[i],
      type = names_types[i],
      format = names_format
    ))

  names_row <- RowData(names_cells)

  values_rows <- lapply(seq_len(nrow(df)), \(i_r) {

    values_cells <- lapply(seq_len(ncol(df)), \(i_c) {

      construct_CellData_from_types(
        value = df[i_r, i_c],
        type = values_types[i_c],
        format = values_format
      )
    })

    RowData(values_cells)

  })

  out <- c(list(names_row),
           values_rows)

  return(out)

}

#' @rdname rd_gd_from_df
#' @export
to_GridData_from_df <- function(
    df,
    startRow,
    startColumn,
    names_format = NULL,
    values_format = NULL,
    names_types = NULL,
    values_types = NULL) {

  startRow <- check_if_type(startRow, "integer")
  startColumn <- check_if_type(startColumn, "integer")

  rows <- to_RowData_from_df(
    df = df,
    names_format = names_format,
    values_format = values_format,
    names_types = names_types,
    values_types = values_types
  )

  GridData(startRow = startRow,
           startColumn = startColumn,
           rowData = rows)

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
    deepgs_error("Cannot split by {.val row}: {.cls GridRange} spans unbinded rows")

  if (gr$endRowIndex - gr$startRowIndex == 1 && split == "row")
    deepgs_error("Cannot split by {.val row}: {.cls GridRange} specifies an one-row range")

  if (is.null(gr$endColumnIndex) && split == "col")
    deepgs_error("Cannot split by {.val col}: {.cls GridRange} spans unbinded columns")

  if (gr$endColumnIndex - gr$startColumnIndex == 1 && split == "col")
    deepgs_error("Cannot split by {.val col}: {.cls GridRange} specifies an one-column range")

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
    deepgs_error("Cannot extract from unbounded {.cls GridRange)")

  row_indices <- seq(gr$startRowIndex, gr$endRowIndex - 1, by = 1)
  col_indices <- seq(gr$startColumnIndex, gr$endColumnIndex - 1, by = 1)
  grid_indices <- expand.grid(row = row_indices, col = col_indices)

  mapply(GridCoordinate, SIMPLIFY = FALSE, sheetId = gr$sheetId,
         rowIndex = grid_indices$row, columnIndex = grid_indices$col)

}
