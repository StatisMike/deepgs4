#' @name ChartRequests
#' @rdname ChartRequests
#' @title Requests to add and update Chart
#' @description
#' Create `dgs4Req` objects that allow for addition or modification of chart
#' embedded in a spreadsheet. Send created requests with [request_ss_batchUpdate()]
#' @family dgs4Req constructors
#' @aliases AddChartRequest UpdateChartSpecRequest
#' @return dgs4Req object
NULL

#' @rdname ChartRequests
#' @section AddChart:
#' Adds a chart to spreadsheet.
#' @param chart an [EmbeddedChart] object containing specification for a chart
#' @export
AddChartRequest <- function(
    chart) {

  out <- list() |>
    append_cond(chart, class = "EmbeddedChart", skip_null = FALSE) |>
    dgs4_class("AddChart", "Req")

  return(out)

}

#' @rdname ChartRequests
#' @section UpdateChartSpec:
#' Constructs request for updating chart specification of existing chart
#' @param chartId id of the chart to update
#' @param spec object of class [ChartSpec], containing new chart specifications
#' @export
UpdateChartSpecRequest <- function(
    chartId,
    spec) {

  out <- list() |>
    append_cond(chartId, type = "integer", skip_null = FALSE) |>
    append_cond(spec, class = "ChartSpec", skip_null = FALSE) |>
    dgs4_class("UpdateChartSpec", "Req")

  return(out)

}
