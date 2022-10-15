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
    if (is.coercible.dt(col) || is.dgs4_serial_number(col)) {
      if (is.dgs4_serial_number(col))
        as_sn <- head(col, 20)
      else
        as_sn <- dgs4_serial_number(head(col, 20))
      if (all(vapply(as_sn, \(x) is.na(x) || x %% 1 == 0, logical(1))))
        return("d")
      else
        return("dt")
    }
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
    dgs4_error("Either {.val NULL}, {.val 1} or as many types as columns or names need to provided to {.arg value_types} or {.arg names_types} arguments.",
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
    n = CellData(userEnteredValue = if (!is.na(value)) ExtendedValue(numberValue = as.numeric(value)),
                 userEnteredFormat = if (!is.null(format)) format),
    c = CellData(userEnteredValue = if (!is.na(value)) ExtendedValue(stringValue = as.character(value)),
                 userEnteredFormat = if (!is.null(format)) format),
    b = CellData(userEnteredValue = if (!is.na(value)) ExtendedValue(boolValue = as.logical(value)),
                 userEnteredFormat = if (!is.null(format)) format),
    f = CellData(userEnteredValue = if (!is.na(value)) ExtendedValue(formulaValue = as.character(value)),
                 userEnteredFormat = if (!is.null(format)) format),
    d = {

      if (!is.dgs4_serial_number(value) && !is.na(value))
        value <- dgs4_serial_number(value)

      if (is.null(format))
        format <- CellFormat(numberFormat = NumberFormat("DATE"))
      else if (is.null(format$numberFormat))
        format$numberFormat <- NumberFormat("DATE")

      if (is.na(value)) {
        return(CellData(userEnteredFormat = format))
      }

      CellData(userEnteredValue = ExtendedValue(numberValue = value),
               userEnteredFormat = format)

    },

    dt = {

      if (!is.dgs4_serial_number(value) && !is.na(value))
        value <- dgs4_serial_number(value)

      if (is.null(format))
        format <- CellFormat(numberFormat = NumberFormat("DATE_TIME"))
      else if (is.null(format$numberFormat))
        format$numberFormat <- NumberFormat("DATE_TIME")

      if (is.na(value)) {
        return(CellData(userEnteredFormat = format))
      }

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
#' @param transpose if `TRUE`, then the data.frame will be transposed, so the first
#' column in Sheets will be the names, and every next column: one row from the `df`
#' @details
#' ## Set types
#' `names_types` and `values_types` could be provided as singular value, in which
#' case ALL names and ALL columns will be saved as the same class, or as a vector
#' of the same length as number of columns, in which case the every column will
#' have its own class provided.
#'
#' Possible types are:
#' - `n` for values formatted as numeric
#' - `c` for values formatted as character
#' - `b` for values formatted as boolean (logical)
#' - `f` if values are characters that need to be interpreted as googlesheets
#' formulas. This type is never guessed
#' - `d` for values to be formatted as `DATE` numeric type. If they aren't
#' `deepgsh_serial_number`, they will be coerced first
#' - `dt` for values to be formatted as `DATE_TIME` numeric type. If they aren't
#' `deepgsh_serial_number`, they will be coerced first
#'
#' If `values_types` argument is kept as `NULL`, the types will be interpreted
#' from data.frame column type.
#'
#' ## Type guess
#' If `values_types` aren't provided, they will be guessed
#' automatically. Guessing is done on basis of vector type in following order:
#' 1. `logical` vectors will be given `b` type (entered as *boolValue*)
#' 2. `numeric` vectors will be given `n` type (entered as *numberValue*)
#' 3. `factor` vectors will be given `c` type (entered as *stringValue*)
#' 4. vectors will be tested if they can be processed with `lubridate::as_date_time()`
#' or if they consists of [dgs4_serial_number].
#'   4a. First 20 values will be tested if they are dates or NAs. If so,
#'   they will be given `d` type (entered as *numberValue* with `DATE` [NumberFormat])
#'   4b. If not, they will be given `dt` type (entered as *numberValue* with
#'   `DATE_TIME` [NumberFormat])
#' 5. If none of above applies, they will be given `c` type.
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
    values_types = NULL,
    transpose = FALSE) {

  if (!is.data.frame(df))
    dgs4_error("Object provided to {.arg df} needs to be a {.cls data.frame}.")

  df <- as.data.frame(df)

  names_types <- handle_val_types(names_types, df, "names")
  values_types <- handle_val_types(values_types, df, "values")

  names_format <- check_if_class(names_format, "CellFormat")
  values_format <- check_if_class(values_format, "CellFormat")

  # transposed strategy ####
  if (isTRUE(transpose)) {

    rows <- lapply(seq_len(ncol(df)), \(i_c) {

      rows_values <- c(
        list(
          construct_CellData_from_types(
            value = names(df)[i_c],
            type = names_types[i_c],
            format = names_format
          )),
        lapply(seq_len(nrow(df)), \(i_r) {
          construct_CellData_from_types(
            value = df[i_r, i_c],
            type = values_types[i_c],
            format = values_format
          )
        })
      )
      RowData(rows_values)
    })

    return(rows)

  }

  # regular data.frame

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
    values_types = NULL,
    transpose = FALSE) {

  startRow <- check_if_type(startRow, "integer")
  startColumn <- check_if_type(startColumn, "integer")

  rows <- to_RowData_from_df(
    df = df,
    names_format = names_format,
    values_format = values_format,
    names_types = names_types,
    values_types = values_types,
    transpose = transpose
  )

  GridData(startRow = startRow,
           startColumn = startColumn,
           rowData = rows)

}
