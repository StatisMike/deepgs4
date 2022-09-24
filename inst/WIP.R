ss_data <- SpreadSheetData$new("1yTJKa-EsVSEk6oNvdw7X7uxZ8vXySuCzEI0gWDbW1Po")

ss_data$sheets

merges <- ss_data$get_data("merges", "charts")


resp <- getSpreadsheetData(
  "1yTJKa-EsVSEk6oNvdw7X7uxZ8vXySuCzEI0gWDbW1Po",
  "sheets.properties,sheets.merges,sheets.charts"
)


get_els_recursive <- function(l, class = "GridRange") {

  out <- list()

  for (i in seq_along(l)) {

    el <- l[[i]]

    if (inherits(el, "deepgsheet4Obj"))
      out[[i]] <- get_els_recursive(el, class = class)
    else
      out[[i]] <- inherits(el, class)

  }

  return(out)

}

BasicChartAxis <- function(
    position = c("BOTTOM_AXIS", "LEFT_AXIS", "RIGHT_AXIS"),
    title = NULL,
    format = NULL,
    viewWindowOptions = NULL,
    horizontalAlignment = NULL) {

  position <- match.arg(position)

  out <- list(
    position = position
  )

  if (!is.null(title)) {
    out$title <- title
  }

  if (!is.null(format)) {
    if(!is.TextFormat(format))
      stop("Object of class TextFormat need to be provided to format argument")
    out$format <- format
  }

  if (!is.null(viewWindowOptions)) {
    if (!is.ChartAxisViewWindowOptions(viewWindowOptions))
      stop("Object of class ChartAxisViewWindowOptions needs to be provided to viewWindowOptions argument")
    out$viewWindowOptions <- viewWindowOptions
  }

  if (!is.null(horizontalAlignment)) {
    out$titleTextPosition <- horizontalAlignment
  }

  class(out) <- c("BasicChartAxis", "deepgsheet4Obj")

  return(out)

}

as.list.BasicChartAxis <- function(x, ...) {

}
