#' @title Options of view window in Chart Axis
#' @description Object with specification on how the view on the chart should
#' be placed
#' @param viewWindowMode mode for view. `"PRETTY"` chooses min and max values
#' automatically
#' @param viewWindowMin,viewWindowMax minimum and maximum values shown on axis.
#' Used only when `viewWindowMode = "EXPLICIT"`
#' @export

ChartAxisViewWindowOptions <- function(
    viewWindowMode = c("PRETTY", "EXPLICIT"),
    viewWindowMin = NULL,
    viewWindowMax = NULL) {

  viewWindowMode <- match.arg(viewWindowMode)

  out <- list() |>
    append_cond(viewWindowMode, "viewWindowMode") |>
    append_cond(viewWindowMin, "viewWindowMin") |>
    append_cond(viewWindowMax, "viewWindowMax") |>
    deepgs_class(out, "ChartAxisViewWindowOptions")

  return(out)

}

#' @rdname ChartAxisViewWindowOptions
#' @param x any R object
#' @export
is.ChartAxisViewWindowOptions <- function(x)
  inherits(x, "ChartAxisViewWindowOptions")

#' @title Generate ChartAxisViewWindowOptions
#' @description Function used internally by [SpreadSheetsData] object
#' @noRd
gen_ChartAxisViewWindowOptions <- function(obj) {

  args <- list()

  if (args$viewWindowMode != "VIEW_WINDOW_MODE_UNSUPPORTED")
    args$viewWindowMode <- obj$viewWindowMode

  if (!is.null(obj$viewWindowMin))
    args$viewWindowMin <- obj$viewWindowMin

  if (!is.null(obj$viewWindowMax))
    args$viewWindowMax <- obj$viewWindowMax

  do.call(ChartAxisViewWindowOptions,
          args = args)
}

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
#' @export
ChartData <- function(
    sourceRange = NULL,
    columnReference = NULL,
    aggregateType = c("AVERAGE", "COUNT", "MAX", "MEDIAN", "MIN", "SUM"),
    groupRule = NULL) {

  if (is.null(sourceRange) && is.null(columnReference))
    deepgs_error("One of the {.arg sourceRange} or {.arg columnReference}
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
    out <- deepgs_class(out, "ChartData")

    return(out)
  }

  out <- out |>
    append_cond(columnReference, class = "character", nests = "name") |>
    append_cond(aggregateType) |>
    append_cond(groupRule)

  out <- deepgs_class(out, "ChartData")
  return(out)

}

#' @title Generate ChartData
#' @description Function used internally by [SpreadSheetsData] object
#' @noRd
gen_ChartData <- function(obj,
                          sheetProperties) {

  if (!is.null(obj$sourceRange)) {

    args <- list(
      sourceRange = lapply(obj$sourceRange$sources, try_to_gen,
                           class = "GridRange",
                           sheetProperties = sheetProperties))

    return(do.call(ChartData, args = args))

  }

  args <- list() |>
    append_cond(obj$columnReference, "columnReference") |>
    append_cond(obj$aggregateType, "aggregateType") |>
    append_cond(obj$groupRule, "groupRule")

  do.call(ChartData,
          args = args)
}

#' @title BasicChartAxis
#' @description Specification for googlesheets Basic Chart
#' @param position Position of the axis
#' @param title Title for the axis. If set, overrides title infered from data.
#' @param format object of class [TextFormat]. Specifies format of text.
#' @param viewWindowOptions object of class [ChartAxisViewWindowOptions].
#' Specifies view on this axis
#' @param horizontalAlignment Alignment of the title text on the axis. Valid positions
#' are: `LEFT`, `CENTER` and `RIGHT`
#' @export
#' @return object of class BasicChartAxis
BasicChartAxis <- function(
    position = c("BOTTOM_AXIS", "LEFT_AXIS", "RIGHT_AXIS"),
    title = NULL,
    format = NULL,
    viewWindowOptions = NULL,
    horizontalAlignment = NULL) {

  position <- match.arg(position)

  out <- list(position = position) |>
    append_cond(title) |>
    append_cond(format, class = "TextFormat") |>
    append_cond(viewWindowOptions, class = "ChartAxisViewWindowOptions") |>
    append_cond(horizontalAlignment) |>
    deepgs_class("BasicChartAxis")

  return(out)

}

#' @rdname BasicChartAxis
#' @param x any R object
#' @export
is.BasicChartAxis <- function(x)
  inherits(x, "BasicChartAxis")

#' @title Generate BasicChartAxis
#' @description Function used internally by [SpreadSheetsData] object
#' @noRd
gen_BasicChartAxis <- function(obj,
                               sheetProperties = NULL) {

  args <- list(position = obj$position) |>
    append_cond(obj$title, "title") |>
    append_cond(try_to_gen(obj$format, "TextFormat"), "format") |>
    append_cond(obj$titleTextPosition$horizontalAlignment, "horizontalAlignment") |>
    append_cond(try_to_gen(obj$viewWindowOptions, "ChartAxisViewWindowOptions"),
                       "viewWindowOptions")

  do.call(BasicChartAxis,
          args = args)
}


#' @title BasicChartDomain
#' @description object of class BasicChartDomain. Specifies the context
#' of series.
#' @param chartData object of class [ChartData] specifying the data scope.
#' @param reversed boolean indicating if values should be reversed
#' @export
BasicChartDomain <- function(
    chartData,
    reversed = NULL) {

  out <- list() |>
    append_cond(chartData, class = "ChartData") |>
    append_cond(reversed, type = "logical") |>
    deepgs_class("BasicChartDomain")

  return(out)

}

#' @rdname BasicChartDomain
#' @param x any R object
#' @export
is.BasicChartDomain <- function(x)
  inherits(x, "BasicChartDomain")

#' @title Generate BasicChartDomain
#' @description Function used internally by [SpreadSheetsData] object
#' @noRd
gen_BasicChartDomain <- function(obj,
                                 sheetProperties) {

  args <- list(
    chartData = gen_ChartData(
      sheetProperties = sheetProperties,
      obj = obj$domain)
  ) |>
    append_cond(obj$reversed, "reversed")

  do.call(BasicChartDomain,
          args = args)
}

#' @title BasicChartSeries
#' @description object of class BasicChartDomain. Specifies the data
#' that will be visualized in the chart
#' @param chartData object of class [ChartData] specifying the data scope.
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
    chartData,
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

  out <- list() |>
    append_cond(chartData, skip_null = FALSE) |>
    append_cond(targetAxis) |>
    append_cond(type) |>
    append_cond(dataLabel, class = "DataLabel") |>
    append_cond(lineStyle, class = "LineStyle") |>
    append_cond(colorStyle, class = "ColorStyle") |>
    append_cond(pointStyle, class = "PointStyle") |>
    append_cond(styleOverrides) |>
    deepgs_class("BasicChartSeries")

  return(out)

}

#' @rdname BasicChartSeries
#' @param x any R object
#' @export
is.BasicChartSeries <- function(x)
  inherits(x, "BasicChartSeries")

#' @title Generate BasicChartSeries
#' @description Function used internally by [SpreadSheetsData] object
#' @noRd
gen_BasicChartSeries <- function(obj,
                                 sheetProperties) {

  obj$chartData <- gen_ChartData(
    sheetProperties = sheetProperties,
    obj = obj$series
  )

  obj <- obj[!grepl(names(obj), pattern = "^series$")]

  do.call(BasicChartSeries,
          args = obj)
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
    append_cond(headerCount, class = c("integer", "numeric")) |>
    append_cond(threeDimensional, class = "logical") |>
    append_cond(check_if_options(compareMode, "DATUM", "CATEGORY"), "compareMode") |>
    deepgs_class(c("BasicChartSpec", "GSChart"))

  if (chartType == "LINE")
    out <- append_cond(out, lineSmoothing, class = "logical")

  if (chartType %in% c("AREA", "BAR", "COLUMN", "COMBO", "STEPPED_AREA"))
    out <- append_cond(out, check_if_options(stackedType, "NOT_STACKED", "STACKED", "PERCENT_STACKED"))

  if (!is.null(totalDataLabel) && isTRUE(stackedType %in% c("STACKED", "PERCENT_STACKED"))) {
    totalDataLabel <- check_if_class(totalDataLabel, "DataLabel")
    if (totalDataLabel$type == "CUSTOM" || !is.null(totalDataLabel$customDataLabel))
      deepgs_error("{.cls DataLabel} provided to {.arg totalDataLabel} cannot be of {.val CUSTOM} type nor contain {.val customDataLabel}.",
                   class = "totalDataLabelError")
    out[["totalDataLabel"]] <- totalDataLabel
  }

  return(out)

}

#' @rdname BasicChartSpec
#' @param x any R object
#' @export
is.BasicChartSpec <- function(x)
  inherits(x, "BasicChartSpec")

#' @title Generate BasicChartSeries
#' @description Function used internally by [SpreadSheetsData] object
#' @noRd
gen_BasicChartSpec <- function(obj,
                               sheetProperties) {

  args <- list(
    axis = lapply(obj$axis, gen_BasicChartAxis, sheetProperties = sheetProperties),
    domains = lapply(obj$domains, gen_BasicChartDomain, sheetProperties = sheetProperties),
    series = lapply(obj$series, gen_BasicChartSeries, sheetProperties = sheetProperties)
  ) |>
    append_cond(obj$legendPosition, "legendPosition") |>
    append_cond(obj$chartType, "chartType") |>
    append_cond(obj$headerCount, "headerCount") |>
    append_cond(obj$threeDimensional, "threeDimensional") |>
    append_cond(obj$interpolateNulls, "interpolateNulls") |>
    append_cond(obj$stackedType, "stackedType") |>
    append_cond(obj$lineSmoothing, "lineSmoothing") |>
    append_cond(obj$compareMode, "compareMode") |>
    append_cond(obj$totalDataLabel, "totalDataLabel")

  do.call(BasicChartSpec,
          args = args)
}

#' @title ChartSpec
#' @description Specification for googlesheets chart
#' @param chart Specification of the chart to render. Currently supports only
#' `BasicChartSpec` objects
#' @param title Title of the chart. Optional.
#' @param titlePosition Horizntal aligmnent of the title. Defaults to `CENTER`
#' @param subtitle Subtitle of the chart. Optional.
#' @param subtitlePosition Horizonstal aligment of the subtitle. Defaults to
#' `CENTER`
#' @param fontName name of the font to use. Optional.
#' @param maximized boolean. If `TRUE`, the chart will take maximum amount of
#' available space with minimal padding. Optional.
#' @export
ChartSpec <- function(
    chart,
    title = NULL,
    titlePosition = c("CENTER", "LEFT", "RIGHT"),
    subtitle = NULL,
    subtitlePosition = c("CENTER", "LEFT", "RIGHT"),
    fontName = NULL,
    maximized = NULL) {

  if (!is.TypeChartSpec(chart))
    stop("Object of class 'BasicChartSpec' needs to be provided to 'chart'
         argument.")

  out <- list(
    basicChart = chart
  )

  if (!is.null(title)) {
    out$title <- title
    out$titleTextPosition$horizontalAlignment <- match.arg(titlePosition)
  }

  if (!is.null(subtitle)) {
    out$subtitle <- subtitle
    out$subtitleTextPosition$horizontalAlignment <- match.arg(subtitlePosition)
  }

  if (!is.null(fontName))
    out$fontName <- fontName

  if (!is.null(maximized))
    out$maximized <- maximized

  class(out) <- c("ChartSpec", "deepgseet4Obj")

  return(out)

}

#' @rdname ChartSpec
#' @param x any R object
#' @export
is.ChartSpec <- function(x)
  inherits(x, "ChartSpec")

#' @rdname ChartSpec
#' @param x any R object
#' @export
is.TypeChartSpec <- function(x) {
  TypeChartSpecs <- c("BasicChartSpec")
  inherits(x, TypeChartSpecs)
}

#' @title EmbeddedObjectPosition
#' @description Specification of position for embedded objects (eg. charts)
#' @param type Should it be embedded in some position on existing sheet, or on
#' new, separate sheet
#' @param gridCoordinate integers declaring the anchor cell for the chart.
#' Indices began with 0. Valid only if `type = "anchor"`
#' @param offsetXPixels,ofsetYPixels integers declaring amount of pixels to
#' offset the position from the anchor cell. Valid only if `type = "anchor"`
#' @param widthPixels,heightPixels integers declaring chart size. Valid only
#' if `type = "anchor"`
#' @param sheetId ID of new sheet to embed the spreadsheet to. Valid only if
#' `type = "new"`. Can be unspecified - then random ID is chosen.
#' @export
#' @return EmbeddedObjectPosition
EmbeddedObjectPosition <- function(
    type = c("anchor", "new"),
    gridCoordinate,
    offsetXPixels = 0,
    offsetYPixels = 0,
    widthPixels = 600,
    heightPixels = 371,
    sheetId = NULL) {

  type <- match.arg(type)

  if (type == "new") {

    out <- list(
      newSheet = T
    )

    if (!is.null(sheetId))
      out$sheetId <- sheetId

    class(out) <- c("EmbeddedObjectPosition", "deepgseet4Obj")

    return(out)

  }

  if (!is.GridCoordinate(gridCoordinate))
    stop("Object of class 'GridCoordinate' need to be provided to the 'gridCoordinate argument")

  out <- list(
    overlayPosition = list(
      anchorCell = gridCoordinate,
      offsetXPixels = offsetXPixels,
      offsetYPixels = offsetYPixels,
      widthPixels = widthPixels,
      heightPixels = heightPixels
    )
  )

  class(out) <- c("EmbeddedObjectPosition", "deepgseet4Obj")

  return(out)

}

#' @rdname ChartSpec
#' @param x any R object
#' @export
is.EmbeddedObjectPosition <- function(x)
  inherits(x, "EmbeddedObjectPosition")

#' @title Chart Data Labels
#' @description Settings for chart data labels - annotations that appear on
#' the chart next to a series
#' @param textFormat object of class [TextFormat] specifying formats of data
#' labels
#' @param placement placement of the data label relative to the series
#' @param type Type of the data label
#' @param customLabelData object of class [GridRange] specifying the data source
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
#'
#' @export
DataLabel <- function(
    textFormat = NULL,
    placement = c("DATA_LABEL_PLACEMENT_UNSPECIFIED", "CENTER", "LEFT",
                  "RIGHT", "ABOVE", "BELOW", "INSIDE_END", "INSIDE_BASE",
                  "OUTSIDE_END"),
    type = c("DATA_LABEL_TYPE_UNSPECIFIED", "DATA", "CUSTOM", "NONE"),
    customLabelData = NULL) {

  type <- match.arg(type)
  placement <- match.arg(placement)

  out <- list(type = type, placement = placement) |>
    append_cond(customLabelData, class = "ChartData") |>
    append_cond(textFormat, class = "TextFormat") |>
    deepgs_class("DataLabel")

  return(out)

}

#' @rdname DataLabel
#' @param x any R object
#' @export
is.DataLabel <- function(x)
  inherits(x, "DataLabel")

#' @title Generate DataLabel
#' @description Function used internally by [SpreadSheetsData] object
#' @noRd
gen_DataLabel <- function(obj, sheetProperties) {

  args <- list() |>
    append_cond(obj$type, "type") |>
    append_cond(obj$placement, "placement") |>
    append_cond(try_to_gen(obj$textFormat,
                           class = "TextFormat"),
                "textFormat") |>
    append_cond(try_to_gen(obj$customLabelData,
                           class = "ChartData",
                           sheetProperties = sheetProperties),
                "customLabelData")

  do.call(ChartDataLabels,
          args = args)
}

#' @title AddChartRequest
#' @description Constructs request for creation of googlesheets chart
#' @param chartSpec a [ChartSpec()] object containing specification for a chart
#' @param embeddedObjectPosition an [EmbeddedObjectPosition()] object containing
#' specification for chart position
#' @param chartId integer. optional ID for the chart
#' @export
#' @return gsheetRequest
AddChartRequest <- function(
    chartSpec,
    embeddedObjectPosition,
    chartId = NULL) {

  if (!is.ChartSpec(chartSpec))
    stop("Object provided to 'chartSpec' argument needs to be of class 'ChartSpec'.")

  if (!is.EmbeddedObjectPosition(embeddedObjectPosition))
    stop("Object provided to 'embeddedObjectPosition' argument needs to be of class 'EmbeddedObjectPosition'.")

  out <- list(
    addChart = list(
      chart = list(
        spec = chartSpec,
        position = embeddedObjectPosition
      )
    )
  )

  if (!is.null(chartId))
    out$chart$chartId <- chartId

  class(out) <- "gsheetRequest"

  return(out)

}
