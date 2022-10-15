#' @title Value Range
#' @description Object containing values within range of a spreadsheet. Mostly
#' received and send through `spreadsheets.values` methods.
#' @param range The range the values cover, in *A1 notation*. For output, this
#' range indicates the entire requested range, even though the values will
#' exclude trailing rows and columns. When appending values, this field
#' represents the range to search for a table, after which values will be appended.
#' @param majorDimension The major dimension of the values.
#' @param values nested list containing values. Each nested list is one
#' row or column of the data (depending on `majorDimension` specification).
#' Values can be of different types, but only atomic types
#' @export
ValueRange <- function(
    range,
    values,
    majorDimension = c("ROWS", "COLUMNS")) {

  out <- list(
    range = range,
    majorDimension = majorDimension,
    values = values
  ) |>
    dgs4_class("ValueRange")

  return(out)

}

#' @title Update Values Response
#' @description Response when updating a range of values in a spreadsheet
#' @param spreadsheetId ID of the spreadsheet the updates were applied to
#' @param updatedRange range in *A1 notation* where the updates were applied to
#' @param updatedRows number of rows where at least one cell in the row was updated
#' @param updatedColumns number of columns where at least one cell in the column
#' was updated
#' @param updatedCells number of cells updated
#' @param updatedData object of class [ValueRange] containing the values of the
#' cells after updates were applied. Only if the `includeValuesInResponse` of
#' [send_update_values_req()] were `TRUE`
#' @export
#' @return UpdateValuesResponse object
UpdateValuesResponse <- function(
    spreadsheetId,
    updatedRange,
    updatedRows,
    updatedColumns,
    updatedCells,
    updatedData = NULL) {

  out <- list() |>
    append_cond(spreadsheetId, type = "character", skip_null = FALSE) |>
    append_cond(updatedRange, type = "character", skip_null = FALSE) |>
    append_cond(updatedRows, type = "integer", skip_null = FALSE) |>
    append_cond(updatedColumns, type = "integer", skip_null = FALSE) |>
    append_cond(updatedCells, type = "integer", skip_null = FALSE) |>
    append_cond(updatedData, class = "ValueRange") |>
    dgs4_class("UpdateValuesResponse")

  return(out)

}

