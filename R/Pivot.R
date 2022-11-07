#' @title PivotTable
#' @param rows,columns object of class [PivotGroup] or list of such. Each row/column
#' grouping in the pivot table.
#' @param filterSpecs object of class [PivotFilterSpec] or list of such. The
#' filters applied to the source columns before aggregating data for the pivot table.
#' @param values object of class [PivotValue] or list of such. A list of values
#' to include in the pivot table.
#' @param source Object of class [GridRange]. Range of values the pivot table
#' is reading data from, if based on **GRID** sheet
#' @param dataSourceId The ID of the data source the pivot table is reading data
#' from, if based on [DataSource].
#' @param valueLayout Whether values should be listed horizontally (as columns)
#' or vertically (as rows).
#' @param dataExecutionStatus **READ ONLY** object of class [DataExecutionStatus]
#' if pivot table is based on [DataSource]
#' @param ... additional args received from Googlesheets API
#' @export
PivotTable <- function(
    rows = NULL,
    columns = NULL,
    filterSpecs = NULL,
    values = NULL,
    source = NULL,
    dataSourceId = NULL,
    valueLayout = c("HORIZONTAL", "VERTICAL"),
    dataExecutionStatus = NULL) {

  valueLayout <- rlang::arg_match(valueLayout)

  source_args <- vapply(list(source, dataSourceId), is.null, logical(1))

  if (sum(source_args) != 1)
    dgs4_error("Exactly one of {.arg source} or {.arg dataSourceId} needs to be specified.")

  rows <- nest_if_class(rows, "PivotGroup") |>
    check_if_all_class("PivotGroup")
  columns <- nest_if_class(columns, "PivotGroup") |>
    check_if_all_class("PivotGroup")
  filterSpecs <- nest_if_class(filterSpecs, "PivotFilterSpec") |>
    check_if_all_class("PivotFilterSpec")
  values <- nest_if_class(values, "PivotValue") |>
    check_if_all_class("PivotValue")

  obj <- list() |>
    append_cond(rows) |>
    append_cond(columns) |>
    append_cond(filterSpecs) |>
    append_cond(values) |>
    append_cond(source, class = "GridRange") |>
    append_cond(dataSourceId, type = "character") |>
    append_cond(valueLayout) |>
    append_cond(dataExecutionStatus, class = "DataExecutionStatus") |>
    dgs4_class("PivotTable")

  return(obj)

}
