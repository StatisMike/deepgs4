#' @title Unclass
#' @param x object to unclass
#' @noRd
unclass_obj <- function(x, ...) {

  if (is.list(x))
    x <- lapply(x, unclass_obj)

  class(x) <- NULL
  return(x)
}

#' @title Is an object
#' @param x object
#' @export
is.deepgsheets4Obj <- function(x)
  inherits(x, "deepgsheets4Obj")

#' @name deepgs_listinize
#' @rdname deepgs_listinize
#' @title Transform `deepgsheets4` object to list
#' @param x object to coerce
#' @param ... further arguments passed to or from other methods.
#' @return `list`
#' @description Coerce objects to lists readable by googlesheets API
#' @details
#' GoogleSheets API v4 requires every object to be represented by list. These
#' lists are deeply nested, and as such are not very `R` friendly - to skim
#' through them, modify them etc.
#'
#' Methods to [deepgs_listinize()] provided within the package are called just before
#' sending request, allowing more user-friendly representation in `R` and
#' readibility on GoogleSheets end.
#'
#' Listinized objects can then be parsed back into its source object using
#' correct `gen_ObjectClass()` constructor, which are used mainly for
#' interpreting reads from GoogleSheets API, but are also exported.
#' @export
deepgs_listinize <- function(x, ...)
  UseMethod("deepgs_listinize", x)

#' @rdname deepgs_listinize
#' @export
deepgs_listinize.default <- function(x, ...) {

  if (!is.list(x))
    return(x)
  else
    lapply(x, deepgs_listinize)

}

#' @rdname deepgs_listinize
#' @export
deepgs_listinize.deepgsheets4Req <- function(x, ...) {

  x <- lapply(x, deepgs_listinize)
  return(x)

}

#' @rdname deepgs_listinize
#' @export
deepgs_listinize.deepgsheets4Obj <- function(x, ...) {

  x <- lapply(x, deepgs_listinize)
  x <- lapply(x, unclass_obj)

  return(x)

}

#' @rdname deepgs_listinize
#' @export
deepgs_listinize.GridCoordinate <- function(x, ...) {

  x <- NextMethod()
  unclass_obj(x, ...)

}

#' @rdname deepgs_listinize
#' @export
deepgs_listinize.GridRange <- function(x, ...) {

  x <- NextMethod()
  unclass_obj(x, ...)

}

#' @rdname deepgs_listinize
#' @export
deepgs_listinize.TextFormat <- function(x, ...) {

  x <- nest_cond(x, "link", "uri") |>
    unclass_obj()

  return(x)

}

#' @rdname deepgs_listinize
#' @export
deepgs_listinize.ColorStyle <- function(x, ...) {

  if (!any(vapply(list(x$red, x$blue, x$green), is.null, logical(1)))) {
    out <- list(rgbColor = list(red = x$red,
                                green = x$green,
                                blue = x$blue) |>
                  append_cond(x$alpha, name = "alpha"))
    return(out)
  }

  out <- list(themeColor = x$themeColor)

  out <- NextMethod(object = obj)

  return(out)

}

#' @rdname deepgs_listinize
#' @aliases deepgs_listinize
#' @export
deepgs_listinize.ChartAxisViewWindowOptions <- function(x, ...) {

  x <- NextMethod()
  unclass_obj(x, ...)

}

#' @rdname deepgs_listinize
#' @aliases deepgs_listinize
#' @export
deepgs_listinize.ChartData <- function(x, ...) {

  x <- x |>
    nest_cond(name = "sourceRange", nests = "sources") |>
    unclass_obj()

  return(x)

}

#' @rdname deepgs_listinize
#' @export
deepgs_listinize.BasicChartAxis <- function(x, ...) {

  if (!is.null(x$titleTextPosition))
    x$titleTextPosition <- list(horizontalAlignment = x$titleTextPosition)

  x <- unclass_obj(x)

  return(x)

}

#' @rdname deepgs_listinize
#' @export
deepgs_listinize.BasicChartDomain <- function(x, ...) {

  x <- NextMethod()
  unclass_obj(x, ...)

}

#' @rdname deepgs_listinize
#' @export
deepgs_listinize.BasicChartSeries <- function(x, ...) {

  x <- NextMethod()
  unclass_obj(x, ...)

}

#' @rdname deepgs_listinize
#' @export
deepgs_listinize.Borders <- function(x, ...) {

  obj <- list(
    top = list(style = x$top_style) |>
      append_cond(x$top_colorStyle, "colorStyle"),
    bottom = list(style = x$bottom_style) |>
      append_cond(x$bottom_colorStyle, "colorStyle"),
    left = list(style = x$left_style) |>
      append_cond(x$left_colorStyle, "colorStyle"),
    right = list(style = x$right_style) |>
      append_cond(x$right_colorStyle, "colorStyle")
  )

  return(obj)

}

#' @rdname deepgs_listinize
#' @export
deepgs_listinize.CellFormat <- function(x, ...) {

  if (is.null(x$textRotation))
    return(unclass_obj(x))

  if (x$textRotation == "v") {
    x$textRotation <- list(vertical = TRUE)
    return(x)
  }

  x$textRotation <- list(angle = x$textRotation)
  return(x)
}

#' @rdname deepgs_listinize
#' @export
deepgs_listinize.Padding <- function(x, ...) {

  x <- NextMethod()
  unclass_obj(x, ...)

}

#' @rdname deepgs_listinize
#' @export
deepgs_listinize.NumberFormat <- function(x, ...) {

  x <- NextMethod()
  unclass_obj(x, ...)

}

#' @rdname deepgs_listinize
#' @export
deepgs_listinize.DataLabel <- function(x, ...) {

  x <- NextMethod()
  unclass_obj(x, ...)

}

#' @rdname deepgs_listinize
#' @export
deepgs_listinize.LineStyle <- function(x, ...) {

  x <- NextMethod()
  unclass_obj(x, ...)

}

#' @rdname deepgs_listinize
#' @export
deepgs_listinize.PointStyle <- function(x, ...) {

  x <- NextMethod()
  unclass_obj(x, ...)

}

#' @rdname deepgs_listinize
#' @export
deepgs_listinize.BasicSeriesDataPointStyleOverride <- function(x, ...) {

  x <- NextMethod()
  unclass_obj(x, ...)

}

#' @rdname deepgs_listinize
#' @export
deepgs_listinize.BasicChartSpec <- function(x, ...) {

  x <- NextMethod()
  unclass_obj(x, ...)

}

#' @rdname deepgs_listinize
#' @export
deepgs_listinize.OverlayPosition <- function(x, ...) {

  x <- NextMethod()
  unclass_obj(x, ...)

}

#' @rdname deepgs_listinize
#' @export
deepgs_listinize.EmbeddedObjectPosition <- function(x, ...) {

  x <- NextMethod()
  unclass_obj(x, ...)

}

#' @rdname deepgs_listinize
#' @export
deepgs_listinize.EmbeddedChart <- function(x, ...) {

  obj <- list(
    spec = x$spec,
    position = x$position
  ) |>
    append_cond(x$chartId, "chartId")

  if (!is.null(x$borderColor))
    obj$border <- list(colorStyle = x$borderColor)

  return(obj)

}

#' @rdname deepgs_listinize
#' @export
deepgs_listinize.ChartSpec <- function(x, ...) {

  x <- NextMethod()
  unclass_obj(x, ...)

}

