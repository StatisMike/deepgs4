#' @title Values within range of a spreadsheet
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
    deepgs_class("ValueRange")

  return(out)

}
