#' @title Update data in cells
#' @description Construct request to update data in specified range
#' @param rows object of class [RowData] or list of multiple such objects
#' @param fields name of [CellData] fields to update
#' @param start object of class [GridCoordinate]. The coordinate to start writing
#' data at. Any number of rows and columns (including a different number of
#' columns per row) may be written. Can't be specified alongside `range.`
#' @param range object of class [GridRange] or list of such objects.
#' The range to write data to. If the data in rows does not cover the entire
#' requested range, the fields matching those set in fields will be cleared.
#' Can't be specified alongside `start`.
#' @export
UpdateCellsRequest <- function(
    rows,
    fields= c("userEnteredValue", "userEnteredFormat", "note", "textFormatRuns",
              "dataValidation", "pivotTable", "dataSourceTable", "dataSourceFormula"),
    start = NULL,
    range = NULL) {

  arg_check <- vapply(list(start, range), is.null, logical(1))

  if (sum(arg_check) != 1)
    deepgs_error("Exactly one of {.arg start} and {.arg range} need to be provided for {.emph UpdateCellsRequest}.")

  fields <- rlang::arg_match(fields, multiple = TRUE)

  if (is.RowData(rows))
    rows <- list(rows)

  rows <- check_if_all_class("RowData")

  out <- list() |>
    append_cond(fields, skip_null = FALSE) |>
    append_cond(rows, skip_null = FALSE) |>
    append_cond(start, class = "GridCoordinate", skip_null = FALSE) |>
    append_cond(range, class = "GridRange", skip_null = FALSE) |>
    deepgs_class(object_type = "Req")

  return(out)

}
