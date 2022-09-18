#' @title BasicChartAxis
#' @description Specification for googlesheets Basic Chart
#' @param position Position of the axis
#' @param title Title for the axis
#' @export
#' @return object of class BasicChartAxis
BasicChartAxis <- function(
    title,
    position = c("BOTTOM_AXIS", "LEFT_AXIS", "RIGHT_AXIS")) {

  position <- match.arg(position)

  out <- list(
    position = position,
    title = title
  )

  class(out) <- "BasicChartAxis"

  return(out)

}

#' @rdname BasicChartAxis
#' @param x any R object
#' @export
is.BasicChartAxis <- function(x)
  inherits(x, "BasicChartAxis")

#' @title BasicChartDomain
#' @description object of class BasicChartDomain. Specifies the context
#' of scopes (eg. date for numbers)
#' @param gridRange specification for the cells. Only accepts one-row or
#' one-column cell ranges
#' @param reversed boolean indicating if values should be reversed
#' @export
BasicChartDomain <- function(
    gridRange,
    reversed = FALSE) {

  gridRange <- check_chartGridRange(gridRange)

  out <- list(
    domain = list(
      sourceRange = list(
        sources = list(
          gridRange
        )
      )
    ),
    reversed = isTRUE(reversed)
  )

  class(out) <- "BasicChartDomain"

  return(out)

}

#' @rdname BasicChartDomain
#' @param x any R object
#' @export
is.BasicChartDomain <- function(x)
  inherits(x, "BasicChartDomain")



#' @title BasicChartSeries
#' @description object of class BasicChartDomain. Specifies the data
#' that will be visualized in the chart
#' @param gridRange specification for the cells. Only accepts one-row or
#' one-column cell ranges
#' @param targetAxis specifies on which axis the values will be visualised
#' @param type specifies the type of the series (relevant only if chart type is COMBO)
#' @param lineStyle specifies style of the line (for LINE type series)
#' @export
BasicChartSeries <- function(
    gridRange,
    targetAxis = c("BOTTOM_AXIS", "LEFT_AXIS", "RIGHT_AXIS"),
    type = c("LINE", "AREA", "COLUMN"),
    lineStyle = NULL) {

  gridRange <- check_chartGridRange(gridRange)

  targetAxis <- match.arg(targetAxis)
  type <- match.arg(type)

  out <- list(
    series = list(
      sourceRange = list(
        sources = list(
          gridRange
        )
      )
    ),
    targetAxis = targetAxis,
    type = type
  )

  if (type == "LINE" && !is.null(lineStyle)) {
    if (!is.LineStyle(lineStyle))
      stop("Object provided to 'lineStyle' argument needs to be of class 'LineStyle'")
    out$lineStyle <- lineStyle
  }

  class(out) <- "BasicChartSeries"

  return(out)

}

#' @rdname BasicChartSeries
#' @param x any R object
#' @export
is.BasicChartSeries <- function(x)
  inherits(x, "BasicChartSeries")

#' @title BasicChartSpec
#' @description Specification for googlesheets Basic Chart object
#' @param chartType type of the chart
#' @param legendPosition position of the legend
#' @param axis list of 'BasicChartAxis' objects
#' @param domains list containing one 'BasicChartDomain' object
#' @param series list of 'BasicChartSeries' objects
#' @param headerCount The number of rows or columns in the data that are "headers".
#' @param threeDimensional boolean indicating if chart need to be made 3D. Only
#' for charts of types: `"BAR"` or `"COLUMN"`
#' @export

BasicChartSpec <- function(
    chartType = c("BAR", "LINE", "AREA", "COLUMN", "SCATTER", "COMBO", "STEPPED_AREA"),
    legendPosition = c("BOTTOM_LEGEND", "LEFT_LEGEND", "RIGHT_LEGEND", "TOP_LEGEND", "NO_LEGEND"),
    axis,
    domains,
    series,
    headerCount = 0,
    threeDimensional = FALSE) {

  chartType <- match.arg(chartType)
  legendPosition <- match.arg(legendPosition)

  axis <- check_list_of_class(axis, "axis")
  domains <- check_list_of_class(domains, "domains")
  series <- check_list_of_class(series, "series")

  if (!is.numeric(headerCount) || length(headerCount) > 1)
    stop("Value provided to 'headerCount' need to be an integer of length one.")

  out <- list(
    chartType = chartType,
    legendPosition = legendPosition,
    axis = axis,
    domains = domains,
    series = series,
    headerCount = headerCount,
    threeDimensional = threeDimensional
  )

  class(out) <- "BasicChartSpec"

  return(out)

}

#' @rdname BasicChartSpec
#' @param x any R object
#' @export
is.BasicChartSpec <- function(x)
  inherits(x, "BasicChartSpec")

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

  if (!is.BasicChartSpec(chart))
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

  class(out) <- "ChartSpec"

  return(out)

}

#' @rdname ChartSpec
#' @param x any R object
#' @export
is.ChartSpec <- function(x)
  inherits(x, "ChartSpec")

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

    class(out) <- "EmbeddedObjectPosition"

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

  class(out) <- "EmbeddedObjectPosition"

  return(out)

}

#' @rdname ChartSpec
#' @param x any R object
#' @export
is.EmbeddedObjectPosition <- function(x)
  inherits(x, "EmbeddedObjectPosition")

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
