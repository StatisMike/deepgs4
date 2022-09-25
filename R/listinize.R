#' @title Unclass
#' @param x object to unclass
#' @noRd
unclass_obj <- function(x, ...) {

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
#' @title Transform [deepgseets4] object to list
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
#' All of these objects, beside their specific class inherits also from
#' `deepgseets4Obj` S3 class.
#' @export
#' @aliases deepgs_listinize.default deepgs_listinize.GridRange deepgs_listinize.GridCoordinate
#' deepgs_listinize.TextFormat deepgs_listinize.ColorStyle deepgs_listinize.ChartData
#' deepgs_listinize.ChartAxisViewWindowOptions deepgs_listinize.BasicChartAxis deepgs_listinize.BasicChartDomain
#' deepgs_listinize.Borders
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
  NextMethod()

}

#' @rdname deepgs_listinize
#' @export
deepgs_listinize.GridCoordinate <- unclass_obj

#' @rdname deepgs_listinize
#' @export
deepgs_listinize.GridRange <- unclass_obj

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
    out <- list(Color = list(red = x$red, green = x$green, blue = x$blue)) |>
      append_cond(x$alpha, name = "alpha")
    return(out)
  }

  out <- list(themeColor = x$themeColor)

  return(out)

}

#' @rdname deepgs_listinize
#' @aliases deepgs_listinize
#' @export
deepgs_listinize.ChartAxisViewWindowOptions <- unclass_obj

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
deepgs_listinize.BasicChartAxis <- unclass_obj

#' @rdname deepgs_listinize
#' @export
deepgs_listinize.BasicChartDomain <- unclass_obj

#' @rdname deepgs_listinize
#' @export
deepgs_listinize.BasicChartSeries <- unclass_obj

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
deepgs_listinize.CellFormat <- unclass_obj

#' @rdname deepgs_listinize
#' @export
deepgs_listinize.Padding <- unclass_obj

#' @rdname deepgs_listinize
#' @export
deepgs_listinize.NumberFormat <- unclass_obj
