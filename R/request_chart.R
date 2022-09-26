#' @title AddChartRequest
#' @description Constructs request for creation of googlesheets chart
#' @param chart an [EmbeddedChart] object containing specification for a chart
#' @export
#' @return deepgsheets4Req
AddChartRequest <- function(
    chart) {

  chart <- check_if_class(chart, "EmbeddedChart")

  out <- list(
    addChart = list(chart = chart)
  ) |>
    deepgs_class(object_type = "Req")

  return(out)

}

#' @title AddChartRequest
#' @description Constructs request for creation of googlesheets chart
#' @param chartId id of the chart to update
#' @param spec object of class [ChartSpec], containing new chart specifications
#' @export
#' @return deepgsheets4Req
UpdateChartRequest <- function(
    chartId,
    spec) {

  spec <- check_if_class(chart, "ChartSpec")

  out <- list(
    updateChartSpec = list(chart = chart) |>
      append_cond(chartId, type = "integer")
  ) |>
    deepgs_class(object_type = "Req")

  return(out)

}
