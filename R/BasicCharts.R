#' @title BasicChartAxis
#' @description Specification for googlesheets Basic Chart
#' @param position Position of the axis
#' @param title Title for the axis. If set, overrides title infered from data.
#' @param format object of class [TextFormat()]. Specifies format of text.
#' @param viewWindowOptions object of class [ChartAxisViewWindowOptions()].
#' Specifies view on this axis
#' @param titleTextPosition Alignment of the title text on the axis. Valid positions
#' are: `LEFT`, `CENTER` and `RIGHT`
#' @export
#' @return object of class BasicChartAxis
BasicChartAxis <- function(
    position = c("BOTTOM_AXIS", "LEFT_AXIS", "RIGHT_AXIS"),
    title = NULL,
    format = NULL,
    viewWindowOptions = NULL,
    titleTextPosition = NULL) {

  position <- rlang::arg_match(position)

  out <- list(position = position) |>
    append_cond(title) |>
    append_cond(format, class = "TextFormat") |>
    append_cond(viewWindowOptions, class = "ChartAxisViewWindowOptions") |>
    append_cond(titleTextPosition) |>
    dgs4_class("BasicChartAxis")

  return(out)

}

#' @rdname BasicChartAxis
#' @param x any R object
#' @export
is.BasicChartAxis <- function(x) {
  is.dgs4_class(x, "BasicChartAxis")
}

#' @title BasicChartDomain
#' @description object of class BasicChartDomain. Specifies the context
#' of series.
#' @param domains object of class [ChartData] specifying the data scope.
#' @param reversed boolean indicating if values should be reversed
#' @export
BasicChartDomain <- function(
    domain,
    reversed = NULL) {

  out <- list() |>
    append_cond(domain, class = "ChartData") |>
    append_cond(reversed, type = "logical") |>
    dgs4_class("BasicChartDomain")

  return(out)

}

#' @rdname BasicChartDomain
#' @param x any R object
#' @export
is.BasicChartDomain <- function(x) {
  is.dgs4_class(x, "BasicChartDomain")
}

#' @title Style override for BasicChartSeries
#' @description Style override settings for a single series data point.
#' @param index zero-based index for the series data point
#' @param colorStyle object of [ColorStyle] class
#' @param pointStyle object of [PointStyle] class, valid if series (or whole chart)
#' type is on of: `"AREA"`, `"LINE"` or `"SCATTER"`
#' @param ... for deprecated GoogleSheets API fields
#' @export
BasicSeriesDataPointStyleOverride <- function(
    index,
    colorStyle = NULL,
    pointStyle = NULL,
    ...) {

  out <- list() |>
    append_cond(index, type = "integer", skip_null = FALSE) |>
    append_cond(colorStyle, class = "ColorStyle") |>
    append_cond(pointStyle, class = "PointStyle") |>
    dgs4_class("BasicSeriesDataPointStyleOverride")

}

#' @rdname BasicSeriesDataPointStyleOverride
#' @param x any R object
#' @export
is.BasicSeriesDataPointStyleOverride <- function(x) {
  is.dgs4_class(x, "BasicSeriesDataPointStyleOverride")
}

#' @title BasicChartSeries
#' @description object of class BasicChartDomain. Specifies the data
#' that will be visualized in the chart
#' @param series object of class [ChartData] specifying the data scope.
#' @param dataLabel object of class [DataLabel] specifying labels for the series
#' @param type specifies the type of the series (relevant only if chart type is COMBO).
#' Possible values are `"LINE"`, `"AREA"` and `"COLUMN"`
#' @param targetAxis specifies on which axis the values will be visualised
#' @param lineStyle object of class [LineStyle], which specifies style of the line.
#' Relevant only if chart type is one of: AREA, LINE, SCATTER or COMBO with correct
#' series `type`
#' @param colorStyle object of class [ColorStyle], specifying color for
#' elements associated with the series.
#' @param pointStyle object of class [PointStyle], specifying style of the data
#' points associated with the series. Valid only if chart type is one of: AREA,
#' LINE, SCATTER or COMBO with correct series `type`
#' @param styleOverrides currently unimplemented. Present for correct data
#' reading
#' @export
BasicChartSeries <- function(
    series,
    targetAxis = c("BOTTOM_AXIS", "LEFT_AXIS", "RIGHT_AXIS"),
    dataLabel = NULL,
    type = NULL,
    lineStyle = NULL,
    colorStyle = NULL,
    pointStyle = NULL,
    styleOverrides = NULL
) {

  targetAxis <- match.arg(targetAxis)
  type <- check_if_options(type, "LINE", "AREA", "COLUMN")

  if (!is.null(styleOverrides)) {

    if (is.BasicSeriesDataPointStyleOverride(styleOverrides))
      styleOverrides <- list(styleOverrides)

    styleOverrides <- check_if_all_class(styleOverrides,
                                         "BasicSeriesDataPointStyleOverride")

  }

  out <- list() |>
    append_cond(series, skip_null = FALSE) |>
    append_cond(targetAxis) |>
    append_cond(type) |>
    append_cond(dataLabel, class = "DataLabel") |>
    append_cond(lineStyle, class = "LineStyle") |>
    append_cond(colorStyle, class = "ColorStyle") |>
    append_cond(pointStyle, class = "PointStyle") |>
    append_cond(styleOverrides) |>
    dgs4_class("BasicChartSeries")

  return(out)

}

#' @rdname BasicChartSeries
#' @param x any R object
#' @export
is.BasicChartSeries <- function(x) {
  is.dgs4_class(x, "BasicChartSeries")
}

#' @title BasicChartSpec
#' @description Specification for googlesheets Basic Chart object
#' @param axis object of class [BasicChartAxis] or list of such objects
#' @param domains object of class [BasicChartDomain] or list containing one of such object
#' @param series object of class [BasicChartSeries] or list of such series
#' @param chartType type of the chart
#' @param legendPosition position of the legend
#' @param headerCount The number of rows or columns in the data that are "headers".
#' @param threeDimensional boolean indicating if chart need to be made 3D. Only
#' for charts of types: `"BAR"` or `"COLUMN"`
#' @param interpolateNulls boolean indicating if missing values in series should
#' be interpolated
#' @param stackedType stack type for charts supporting vertical stacking. Applies
#' if `chartType` is one of `"AREA"`, `"BAR"`, `"COLUMN"`, `"COMBO"` or `"STEPPED_AREA"`.
#' See **details** for info about possible values.
#' @param lineSmoothing boolean indicating if lines should be rendered smooth. Applies
#' if `chartType = "LINE"`
#' @param compareMode behavior of tooltips and data highlighting on hover. See
#' **details** for info about possible values
#' @param totalDataLabel controls if additional data labels should be displayed
#' on stacked charts with total sum. Only for stacked charts.
#' @details
#' # Meaning of values for arguments with finite options:
#' - stackedType:
#'    - NOT_STACKED: Series are not stacked.
#'    - STACKED: Series values are stacked, each value is rendered vertically
#'    beginning from the top of the value below it.
#'    - PERCENT_STACKED: Vertical stacks are stretched to reach the top of the
#'    chart, with values laid out as percentages of each other.
#'  - compareMode:
#'    - DATUM: Only the focused data element is highlighted and shown in the tooltip.
#'    - CATEGORY: All data elements with the same category (e.g., domain value)
#'    are highlighted and shown in the tooltip.
#' @export

BasicChartSpec <- function(
    axis,
    domains,
    series,
    chartType = c("BAR", "LINE", "AREA", "COLUMN", "SCATTER", "COMBO", "STEPPED_AREA"),
    legendPosition = c("BOTTOM_LEGEND", "LEFT_LEGEND", "RIGHT_LEGEND", "TOP_LEGEND", "NO_LEGEND"),
    headerCount = NULL,
    threeDimensional = NULL,
    interpolateNulls = NULL,
    stackedType = NULL,
    lineSmoothing = NULL,
    compareMode = NULL,
    totalDataLabel = NULL) {

  chartType <- match.arg(chartType)
  legendPosition <- match.arg(legendPosition)

  if (is.BasicChartAxis(axis))
    axis <- list(axis)
  if (is.BasicChartDomain(domains))
    domains <- list(domains)
  if (is.BasicChartSeries(series))
    series <- list(series)

  axis <- check_if_all_class(axis, "BasicChartAxis")
  domains <- check_if_all_class(domains, "BasicChartDomain")
  series <- check_if_all_class(series, "BasicChartSeries")

  check_domains_series(domains = domains, series = series)

  out <- list(
    chartType = chartType,
    legendPosition = legendPosition,
    axis = axis,
    domains = domains,
    series = series
  ) |>
    append_cond(headerCount, type = "integer") |>
    append_cond(threeDimensional, type = "logical") |>
    append_cond(check_if_options(compareMode, "DATUM", "CATEGORY"), "compareMode") |>
    dgs4_class("BasicChartSpec")

  if (chartType == "LINE")
    out <- append_cond(out, lineSmoothing, type = "logical")

  if (chartType %in% c("AREA", "BAR", "COLUMN", "COMBO", "STEPPED_AREA"))
    out <- append_cond(out, check_if_options(stackedType, "NOT_STACKED", "STACKED", "PERCENT_STACKED"), "stackedType")

  if (!is.null(totalDataLabel) && isTRUE(stackedType %in% c("STACKED", "PERCENT_STACKED"))) {
    totalDataLabel <- check_if_class(totalDataLabel, "DataLabel")
    if (totalDataLabel$type == "CUSTOM" || !is.null(totalDataLabel$customDataLabel))
      dgs4_error("{.cls DataLabel} provided to {.arg totalDataLabel} cannot be of {.val CUSTOM} type nor contain {.val customDataLabel}.",
                 class = "totalDataLabelError")
    out[["totalDataLabel"]] <- totalDataLabel
  }

  return(out)

}

#' @rdname BasicChartSpec
#' @param x any R object
#' @export
is.BasicChartSpec <- function(x) {
  is.dgs4_class(x, "BasicChartSpec")
}
