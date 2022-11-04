#### Valid Chart Specs ####
# During addition of new type of chart spec, name of its constructor (and class)
# needs to be added below
pkg_env$valid_chart_specs <- c(
  "BasicChartSpec"
)

#' @title Extract and convert TypeChartSpec classname to field name
#' @param object TypeChartSpec object
#' @noRd
extract_chart_name <- function(field_names) {

  chart_name <- field_names[grepl(pattern = "Chart$", field_names)]

  return(chart_name)

}

#' @title Extract and convert TypeChartSpec classname to field name
#' @param object TypeChartSpec object
#' @noRd
extract_chart_field <- function(object) {

  chart_class <- class(object)
  chart_class <- chart_class[grepl(chart_class, pattern = "ChartSpec$")]
  chart_class <- unlist(gsub(chart_class, pattern = "Spec$", replacement = ""))
  chart_class <- unlist(strsplit(chart_class, split = ""))
  chart_class[1] <- tolower(chart_class[1])
  chart_class <- paste(chart_class, collapse = "")

  return(chart_class)
}

#' @title Valid Chart Specs
#' @description List of valid types of charts that can be provided to `chart`
#' argument of [ChartSpec()] and, consequently created by sending [AddChartRequest()]
#' @export
TypeChartSpecs <- function() {
  pkg_env$valid_chart_specs
}

#' @title Options of view window in Chart Axis
#' @description Object with specification on how the view on the chart should
#' be placed
#' @param viewWindowMode mode for view. `"PRETTY"` chooses min and max values
#' automatically
#' @param viewWindowMin,viewWindowMax minimum and maximum values shown on axis.
#' Used only when `viewWindowMode = "EXPLICIT"`
#' @family Chart objects constructors
#' @return dgs4Obj of class `ChartAxisViewWindowOptions`
#' @export

ChartAxisViewWindowOptions <- function(
    viewWindowMode = c("PRETTY", "EXPLICIT"),
    viewWindowMin = NULL,
    viewWindowMax = NULL) {

  viewWindowMode <- rlang::arg_match(viewWindowMode)

  out <- list() |>
    append_cond(viewWindowMode, "viewWindowMode") |>
    append_cond(viewWindowMin, "viewWindowMin") |>
    append_cond(viewWindowMax, "viewWindowMax") |>
    dgs4_class("ChartAxisViewWindowOptions")

  return(out)

}

#' @rdname ChartAxisViewWindowOptions
#' @param x any R object
#' @export
is.ChartAxisViewWindowOptions <- function(x)
  inherits(x, "ChartAxisViewWindowOptions")

#' @title ChartData
#' @description Specification for data to be used for creation of *domain*,
#' *series* or other specific types
#' @param sourceRange object of class [GridRange] or list of such objects.
#' All of them need to have one of the dimension of length one (one column or one row).
#' need to be provided for non-data source charts.
#' @param columnReference Display name of the data source column that the data
#' reads from. Need to be provided for data source charts.
#' @param aggregateType aggregation type for the *series* of a data source chart.
#' Only for data-source charts
#' @param groupRule object of class `ChartGroupRule`. Only for data source charts.
#' @family Chart objects constructors
#' @return dgs4Obj of class `ChartData`
#' @export
ChartData <- function(
    sourceRange = NULL,
    columnReference = NULL,
    aggregateType = NULL,
    groupRule = NULL) {

  if (is.null(sourceRange) && is.null(columnReference))
    dgs4_error("One of the {.arg sourceRange} or {.arg columnReference}
                 needs to be specified")

  out <- list()

  if (!is.null(sourceRange)) {

    if (is.GridRange(sourceRange)) {
      out[["sourceRange"]] <- list(sourceRange)
    } else {
      sources <- check_if_all_class(sourceRange, "GridRange")
      names(sources) <- NULL
      out[["sourceRange"]] <- sources
    }

    out$sourceRange <- lapply(out$sourceRange,
                              check_chartGridRange,
                              arg = "sourceRange",
                              call = rlang::current_call())
    out <- dgs4_class(out, "ChartData")

    return(out)
  }

  aggregateType <- check_if_options(
    aggregateType, "AVERAGE", "COUNT", "MAX", "MEDIAN", "MIN", "SUM")

  out <- out |>
    append_cond(columnReference, class = "character", nests = "name") |>
    append_cond(aggregateType) |>
    append_cond(groupRule) |>
    dgs4_class(out, "ChartData")

  return(out)

}

#' @title ChartSpec
#' @description Specification for googlesheets chart
#' @param chart Specification of the chart to render. One of [TypeChartSpec].
#' @param title,subtitle Title and subtitle of the chart.
#' @param titleTextPosition,subtitleTextPosition Horizntal aligmnent of the title and
#' subtitle.
#' @param titleTextFormat,subtitleTextFormat Objects of [TextFormat] class.
#' Strikethrough, underline and link aren't supported.
#' @param fontName name of the font to use.
#' @param altText alternative text to describe a chart.
#' @param maximized boolean. If `TRUE`, the chart will take maximum amount of
#' available space with minimal padding.
#' @param backgroundColorStyle object of [ColorStyle] class, describing the
#' background color for entire chart.
#' @param dataSourceChartProperties object of [DataSourceChartPoperties],
#' contained only in *data source* charts #NOT DONE YET#
#' @param filterSpecs object or list of objects of [FilterSpec] class, describing
#' filters applied to the source data. Only in *data source* charts. #NOT DONE YET#
#' @param sortSpecs object of [SortSpec] class, describing the order to sort
#' the chart data. Only in *data source* charts. #NOT DONE YET#
#' @param hiddenDimensionStrategy describing how the charts will use hidden
#' rows or columns.
#' @param ... for support of deprecated GoogleSheets API fields during read
#' @family Chart objects constructors
#' @return dgs4Obj of class `ChartSpec`
#' @export
ChartSpec <- function(
    chart,
    title = NULL,
    titleTextPosition = NULL,
    titleTextFormat = NULL,
    subtitle = NULL,
    subtitleTextPosition = NULL,
    subtitleTextFormat = NULL,
    fontName = NULL,
    altText = NULL,
    maximized = NULL,
    backgroundColorStyle = NULL,
    dataSourceChartProperties = NULL,
    filterSpecs = NULL,
    sortSpecs = NULL,
    hiddenDimensionStrategy = NULL,
    ...) {

  chart <- check_if_class(chart, class = pkg_env$valid_chart_specs)

  titleTextPosition <- check_if_options(titleTextPosition, "CENTER", "LEFT", "RIGHT")
  subtitleTextPosition <- check_if_options(subtitleTextPosition, "CENTER", "LEFT", "RIGHT")
  hiddenDimensionStrategy <- check_if_options(hiddenDimensionStrategy,
                                              "SKIP_HIDDEN_ROWS_AND_COLUMNS",
                                              "SKIP_HIDDEN_ROWS",
                                              "SKIP_HIDDEN_COLUMNS",
                                              "SHOW_ALL")

  out <- list() |>
    append_cond(chart,
                name = extract_chart_field(chart),
                skip_null = FALSE) |>
    append_cond(title, type = "character") |>
    append_cond(titleTextPosition) |>
    append_cond(titleTextFormat, class = "TextFormat") |>
    append_cond(subtitle, type = "character") |>
    append_cond(subtitleTextPosition) |>
    append_cond(subtitleTextFormat, class = "TextFormat") |>
    append_cond(fontName, type = "character") |>
    append_cond(altText, type = "character") |>
    append_cond(maximized, type = "logical") |>
    append_cond(backgroundColorStyle, class = "ColorStyle") |>
    append_cond(dataSourceChartProperties, class = "DataSourceChartProperties") |>
    append_cond(filterSpecs, class = "FilterSpec") |>
    append_cond(sortSpecs, class = "SortSpec") |>
    append_cond(hiddenDimensionStrategy) |>
    dgs4_class("ChartSpec")

  return(out)

}

#' @rdname ChartSpec
#' @param x any R object
#' @export
is.ChartSpec <- function(x) {
  inherits(x, "ChartSpec")
}

#' @title DataLabel
#' @description Settings for chart data labels - annotations that appear on
#' the chart next to a series
#' @param textFormat object of class [TextFormat] specifying formats of data
#' labels
#' @param placement placement of the data label relative to the series
#' @param type Type of the data label
#' @param customLabelData object of class [CellRange] specifying the data source
#' for data labels with `type = "CUSTOM"`
#' @details
#' - **Type** options:
#'    - DATA_LABEL_TYPE_UNSPECIFIED:  The data label type is not specified and
#'    will be interpreted depending on the context of the data label within the chart.
#'    - DATA: The data label is displayed using values from the series data.
#'    - CUSTOM: The data label is displayed using values from a custom data source
#'    indicated by `customLabelData`
#'    - NONE: The data label is not displayed
#' - **Placement** options:
#'    - DATA_LABEL_PLACEMENT_UNSPECIFIED: The positioning is determined
#'    automatically by the renderer.
#'    - CENTER: Center within a bar or column, both horizontally and vertically.
#'    - LEFT: To the left of a data point.
#'    - RIGHT: To the right of a data point.
#'    - ABOVE: Above a data point.
#'    - BELOW: Below a data point.
#'    - INSIDE_END: Inside a bar or column at the end (top if positive, bottom if negative).
#'    - INSIDE_BASE: Inside a bar or column at the base.
#'    - OUTSIDE_END: Outside a bar or column at the end.
#' @family Chart objects constructors
#' @return dgs4Obj of class `DataLabel`
#' @export
DataLabel <- function(
    textFormat = NULL,
    placement = c("DATA_LABEL_PLACEMENT_UNSPECIFIED", "CENTER", "LEFT",
                  "RIGHT", "ABOVE", "BELOW", "INSIDE_END", "INSIDE_BASE",
                  "OUTSIDE_END"),
    type = c("DATA_LABEL_TYPE_UNSPECIFIED", "DATA", "CUSTOM", "NONE"),
    customLabelData = NULL) {

  type <- rlang::arg_match(type)
  placement <- rlang::arg_match(placement)

  if (!is.null(customLabelData))
    type <- "CUSTOM"

  out <- list(type = type, placement = placement) |>
    append_cond(customLabelData, class = "ChartData") |>
    append_cond(textFormat, class = "TextFormat") |>
    append_cond(type) |>
    dgs4_class("DataLabel")

  return(out)

}

#' @rdname DataLabel
#' @param x any R object
#' @export
is.DataLabel <- function(x) {
  inherits(x, "DataLabel")
}

#' @title EmbeddedChart
#' @description Specification of a chart embedded in a sheet
#' @param spec object of [ChartSpec] class, declaring chart specification
#' @param position object of [EmbeddedObjectPosition] class, declaring the
#' position of chart
#' @param borderColor object of [ColorStyle] class, declaring the color of
#' borders of the chart
#' @param chartId ID of the chart unique in a spreadsheet. If not set during
#' write, the one is chosen for you.
#' @family Chart objects constructors
#' @return dgs4Obj of class `EmbeddedChart`
#' @export
EmbeddedChart <- function(
    spec,
    position,
    borderColor = NULL,
    chartId = NULL) {

  out <- list() |>
    append_cond(spec, class = "ChartSpec", skip_null = FALSE) |>
    append_cond(position, class = "EmbeddedObjectPosition", skip_null = FALSE) |>
    append_cond(borderColor, class = "ColorStyle") |>
    append_cond(chartId, type = "integer") |>
    dgs4_class("EmbeddedChart")

  return(out)

}

#' @rdname EmbeddedChart
#' @param x any R object
#' @export
is.EmbeddedChart <- function(x) {
  inherits(x, "EmbeddedChart")
}

#' @title DataSourceChartProperties
#' @description Properties of a chart based on data located in [DataSource].
#' @param dataSourceId ID of the data source that the chart is associated with.
#' @param dataExecutionStatus **READ ONLY ** object of class [DataExecutionStatus].
#' @family Chart objects constructors
#' @return dgs4Obj of class `DataSourceChartProperties`
#' @export
DataSourceChartProperties <- function(dataSourceId,
                                      dataExecutionStatus = NULL) {

  obj <- list() |>
    append_cond(dataSourceId, type = "character", skip_null = FALSE) |>
    append_cond(dataExecutionStatus, class = "DataExecutionStatus") |>
    dgs4_class("DataSourceChartProperties")

  return(obj)

}

#' @rdname DataSourceChartProperties
#' @param x any R object
#' @export
is.DataSourceChartProperties <- function(x) {
  is.dgs4_class(x, "DataSourceChartProperties")
}



