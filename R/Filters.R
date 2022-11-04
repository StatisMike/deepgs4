#' @title SortSpec
#' @description A sort order associated with a specific column or row. Exactly
#' one of `dimensionIndex` or `dataSourceColumnReference` needs to be specified.
#' @param sortOrder the order in which data should be sorted.
#' @param foregroundColorStyle object of class [ColorStyle]. The foreground color
#' to sort by; cells with this foreground color are sorted to the top. Must be
#' an RGB-type color.
#' @param backgroundColorStyle object of class [ColorStyle]. The background color
#' to sort by; cells with this background color are sorted to the top. Must be
#' an RGB-type color.
#' @param dimensionIndex Index of dimension this sort should be applied to, if
#' sort is related to dimension on **GRID** sheet
#' @param dataSourceColumnReference Unique name of the column, if sort is related
#' to column on **DATA_SOURCE** sheet
#' @param ... for deprecated fields returned from Sheets API
#' @export
SortSpec <- function(
    sortOrder = c("ASCENDING", "DESCENDING"),
    foregroundColorStyle = NULL,
    backgroundColorStyle = NULL,
    dimensionIndex = NULL,
    dataSourceColumnReference = NULL,
    ...) {

  null_indices <- vapply(list(dimensionIndex, dataSourceColumnReference),
                         is.null, logical(1))

  if (sum(null_indices) != 1)
    dgs4_error("Exactly one of {.arg dimensionIndex} or {.arg dataSourceColumnReference} needs to be specified.")

  sortOrder <- rlang::arg_match(sortOrder)

  obj <- list() |>
    append_cond(sortOrder) |>
    append_cond(foregroundColorStyle, class = "ColorStyle") |>
    append_cond(backgroundColorStyle, class = "ColorStyle") |>
    append_cond(dimensionIndex, type = "integer") |>
    append_cond(dataSourceColumnReference, type = "character") |>
    dgs4_class("SortSpec")

  return(obj)

}

#' @rdname SortSpec
#' @param x any R object
#' @export
is.SortSpec <- function(x) {
  is.dgs4_class(x, "SortSpec")
}

#' @title FilterCriteria
#' @description Criteria for showing/hiding rows in a filter or filter view.
#' @param hiddenValues character vector specifying values that should be hidden
#' @param condition object of class [BooleanCondition]. A condition that must be
#' `TRUE` for values to be shown. Doesn't override values specified in `hiddenValues`:
#' even if it returns `TRUE` but is specified there, it will still be hidden.
#' @param visibleBackgroundColorStyle,visibleForegroundColorStyle
#' objects of class [ColorStyle]. The background or foreground
#' fill color to filter by; only cells with this color are shown. Must be
#' specified with explicit **RGB** specification
#' @param ... for deprecated fields returned from Sheets API
#' @export
FilterCriteria <- function(
    hiddenValues = NULL,
    condition = NULL,
    visibleBackgroundColorStyle = NULL,
    visibleForegroundColorStyle = NULL,
    ...) {

  if (!is.null(hiddenValues) && !is.character(hiddenValues))
    dgs4_error("{.arg hiddenValues} needs to be a {.cls character} vector.")

  obj <- list() |>
    append_cond(hiddenValues) |>
    append_cond(condition, class = "BooleanCondition") |>
    append_cond(visibleBackgroundColorStyle, class = "ColorStyle") |>
    append_cond(visibleForegroundColorStyle, class = "ColorStyle") |>
    dgs4_class("FilterCriteria")

  return(obj)

}

#' @rdname FilterCriteria
#' @param x any R object
#' @export
is.FilterCriteria <- function(x) {
  is.dgs4_class(x, "FilterCriteria")
}

#' @title FilterSpec
#' @description The [FilterCriteria] associated with a specific column. Exactly
#' one of `columnIndex` or `dataSourceColumnReference` needs to be specified.
#' @param filterCriteria object of class [FilterCriteria]. The criteria for the column.
#' @param columnIndex Index of column this sort should be applied to, if
#' sort is related to column on **GRID** sheet
#' @param dataSourceColumnReference Unique name of the column, if sort is related
#' to column on **DATA_SOURCE** sheet
FilterSpec <- function(
    filterCriteria,
    columnIndex = NULL,
    dataSourceColumnReference = NULL) {

  null_indices <- vapply(list(columnIndex, dataSourceColumnReference),
                         is.null, logical(1))

  if (sum(null_indices) != 1)
    dgs4_error("Exactly one of {.arg columnIndex} or {.arg dataSourceColumnReference} needs to be specified.")

  obj <- list() |>
    append_cond(filterCriteria, class = "FilterCriteria", skip_null = FALSE) |>
    append_cond(columnIndex, type = "integer") |>
    append_cond(dataSourceColumnReference, type = "character") |>
    dgs4_class("FilterSpec")

  return(obj)

}

#' @rdname FilterSpec
#' @param x any R object
#' @export
is.FilterSpec <- function(x) {
  is.dgs4_class(x, "FilterSpec")
}

#' @title BasicFilter
#' @description The default filter associated with a sheet.
#' @param range object of class [GridRange]. The range the filter covers.
#' @param sortSpecs object of class [SortSpec] or list of such objects.
#' The sort order per column. Later specifications are used when values are
#' equal in the earlier specifications.
#' @param filterSpecs object of class [FilterSpec] or list of such objects.
#' The filter criteria per column.

BasicFilter <- function(
    range,
    sortSpecs = NULL,
    filterSpecs = NULL) {

  sortSpecs <- nest_if_class(sortSpecs, "SortSpec") |>
    check_if_all_class("SortSpec")
  filterSpecs <- nest_if_class(filterSpecs, "FilterSpec") |>
    check_if_all_class("FilterSpec")

  obj <- list() |>
    append_cond(range, class = "GridRange") |>
    append_cond(sortSpecs) |>
    append_cond(filterSpecs) |>
    dgs4_class("BasicFilter")

  return(obj)

}

#' @rdname BasicFilter
#' @param x any R object
#' @export
is.BasicFilter <- function(x) {
  is.dgs4_class(x, "BasicFilter")
}

#' @title FilterView
#' @description A filter view.
#' @param filterViewId The ID of the filter view.
#' @param title The name of the filter view.
#' @param range The range this filter view covers.
#' @param namedRangeId The [NamedRange] this filter view is backed by, if any.
#' @param sortSpecs The sort order per column. Later specifications are used
#' when values are equal in the earlier specifications.
#' @param filterSpecs The filter criteria for showing/hiding values per column.
#' @param ... for deprecated fields returned from Google Sheets API
#' @export
FilterView <- function(
    filterViewId = NULL,
    title = NULL,
    range = NULL,
    namedRangeId = NULL,
    sortSpecs = NULL,
    filterSpecs = NULL,
    ...) {

  range_null <- vapply(list(querySpec, tableSpec), is.null, logical(1))

  if (sum(range_null) != 1)
    dgs4_error("Exactly one of {.arg namedRangeId} or {.arg range} needs to be specified.")

  sortSpecs <- nest_if_class(sortSpecs, "SortSpec") |>
    check_if_all_class("SortSpec")
  filterSpecs <- nest_if_class(filterSpecs, "FilterSpec") |>
    check_if_all_class("FilterSpec")

  obj <- list() |>
    append_cond(filterViewId, type = "integer") |>
    append_cond(title, type = "character") |>
    append_cond(range, class = "GridRange") |>
    append_cond(namedRangeId, type = "integer") |>
    append_cond(sortSpecs) |>
    append_cond(filterSpecs) |>
    dgs4_class("FilterView")

  return(obj)

}

#' @rdname FilterView
#' @param x any R object
#' @export
is.FilterView <- function(x) {
  is.dgs4_class(x, "FilterView")
}
