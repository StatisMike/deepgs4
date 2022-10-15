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
is.deepgsheets4Obj <- function(x) {
  inherits(x, "deepgsheets4Obj")
}

#### default listinizers ####

#' @name dgs4_listinize
#' @rdname dgs4_listinize
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
#' Methods to `dgs4_listinize` provided within the package are called just before
#' sending request, allowing more user-friendly representation in `R` and
#' readibility on GoogleSheets end.
#'
#' Listinized objects can then be parsed back into its source object using
#' [gen_deepgsheets4Obj()] constructor, which are used mainly for
#' interpreting reads from GoogleSheets API, but are also exported.
#' @export
dgs4_listinize <- function(x, ...)
  UseMethod("dgs4_listinize", x)

#' @rdname dgs4_listinize
#' @export
dgs4_listinize.default <- function(x, ...) {

  if (!is.list(x))
    return(x)
  else
    lapply(x, dgs4_listinize, ... = ...) |>
    lapply(unclass_obj)

}

#' @rdname dgs4_listinize
#' @export
dgs4_listinize.deepgsheets4Req <- function(x, ...) {

  x <- lapply(x, dgs4_listinize, ... = ...)
  return(x)

}

#' @rdname dgs4_listinize
#' @export
dgs4_listinize.deepgsheets4Obj <- function(x, ...) {

  x <- lapply(x, dgs4_listinize, ... = ...)
  return(x)

}

#### specific listinizers ####

#' @rdname dgs4_listinize
#' @export
dgs4_listinize.TextFormat <- function(x, ...) {

  x <- lapply(x, dgs4_listinize, ... = ...)

  x <- nest_cond(x, "link", "uri") |>
    unclass_obj()

  return(x)

}

#' @rdname dgs4_listinize
#' @export
dgs4_listinize.ColorStyle <- function(x, ...) {

  x <- lapply(x, dgs4_listinize, ... = ...)

  if (!any(vapply(list(x$red, x$blue, x$green), is.null, logical(1)))) {
    out <- list(rgbColor = list(red = x$red,
                                green = x$green,
                                blue = x$blue) |>
                  append_cond(x$alpha, name = "alpha"))
    return(out)
  }

  out <- list(themeColor = x$themeColor)

  return(out)

}

#' @rdname dgs4_listinize
#' @aliases dgs4_listinize
#' @export
dgs4_listinize.ChartData <- function(x, ...) {

  x <- lapply(x, dgs4_listinize, ... = ...)

  x <- x |>
    nest_cond(name = "sourceRange", nests = "sources") |>
    unclass_obj()

  return(x)

}

#' @rdname dgs4_listinize
#' @export
dgs4_listinize.BasicChartAxis <- function(x, ...) {

  x <- lapply(x, dgs4_listinize, ... = ...)

  if (!is.null(x$titleTextPosition))
    x$titleTextPosition <- list(horizontalAlignment = x$titleTextPosition)

  x <- unclass_obj(x)

  return(x)

}

#' @rdname dgs4_listinize
#' @export
dgs4_listinize.ChartSpec <- function(x, ...) {

  x <- lapply(x, dgs4_listinize, ... = ...)

  x <- x |>
    nest_cond("titleTextPosition", "horizontalAlignment") |>
    nest_cond("subtitleTextPosition", "horizontalAlignment") |>
    unclass_obj()

  return(x)

}

#' @rdname dgs4_listinize
#' @export
dgs4_listinize.CellFormat <- function(x, ...) {

  x <- lapply(x, dgs4_listinize, ... = ...)

  if (is.null(x$textRotation))
    return(unclass_obj(x))

  if (x$textRotation == "v") {
    x$textRotation <- list(vertical = TRUE)
    return(x)
  }

  x$textRotation <- list(angle = x$textRotation)
  return(x)
}

#' @rdname dgs4_listinize
#' @export
dgs4_listinize.ExtendedValue <- function(x, ...) {

  value <- switch(
    attr(x, "type"),
    string = list(stringValue = as.character(x)),
    number = list(numberValue = as.numeric(x)),
    bool = list(boolValue = as.logical(x)),
    formula = list(formulaValue = as.character(x)),
    error = list(errorValue = list(type = x$type,
                                   message = x$message))
  )

  return(value)


}

#' @rdname dgs4_listinize
#' @export
dgs4_listinize.EmbeddedChart <- function(x, ...) {

  x <- lapply(x, dgs4_listinize, ... = ...)

  obj <- list(
    spec = x$spec,
    position = x$position
  ) |>
    append_cond(x$chartId, "chartId")

  if (!is.null(x$borderColor))
    obj$border <- list(colorStyle = x$borderColor)

  return(obj)

}

#' @rdname dgs4_listinize
#' @export
dgs4_listinize.DimensionProperties <- function(x, ...) {

  x <- lapply(x, dgs4_listinize, ... = ...) |>
    nest_cond("dataSourceColumnReference", nests = "name")

  return(x)

}

#' @rdname dgs4_listinize
#' @export
dgs4_listinize.RowData <- function(x, ...) {

  x <- lapply(x, dgs4_listinize, ... = ...)
  out <- list(values = x)
  return(out)

}

#### Conditions.R listinizers ####

#' @rdname dgs4_listinize
#' @export
dgs4_listinize.ConditionValue <- function(x, ...) {

  if (isTRUE(attr(x, "relativeDate")))
    out <- list(relativeDate = as.character(x))
  else
    out <- list(userEnteredValue = unclass_obj(x))

  return(out)

}
