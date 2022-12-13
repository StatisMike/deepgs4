#' @title Interpolation Point
#' @description A single interpolation point on a gradient conditional format.
#' Pins a gradient color scale in accordance to the color, type and value chosen.
#' Used in [GradientRule()]
#' @param colorStyle object of class [ColorStyle] that this point will use.
#' @param type How the value should be interpreted
#' @param value A value that this interpolation point uses, which may also be
#' a formula. Unused for `"MIN"` and `"MAX"` `type`
#' @param ... provided for compatibility with deprecated Google Sheets API
#' objects
#' @export
InterpolationPoint <- function(
    colorStyle,
    type = c("NUMBER", "PERCENT", "PERCENTILE", "MIN", "MAX"),
    value = NULL,
    ...) {

  type <- rlang::arg_match(type)
  skip_value <- type %in% c("MIN", "MAX")
  if (!skip_value)
    value <- as.character(value)[1]

  out <- list() |>
    append_cond(colorStyle, class = "ColorStyle", skip_null = FALSE) |>
    append_cond(type) |>
    append_cond(value, skip_null = skip_value) |>
    dgs4_class("InterpolationPoint")

  return(out)

}

#' @rdname InterpolationPoint
#' @param x any R object
#' @export
is.InterpolationPoint <- function(x) {
  inherits(x, "InterpolationPoint")
}

#' @title Boolean Rule
#' @description A rule for conditional formatting that may match or may not,
#' depending on condition provided. Used in [ConditionalFormatRule()]
#' @param condition object of class [BooleanCondition]. If the condition
#' is `TRUE`, then the `format` is applied
#' @param format object of class [CellFormat]. Conditional formatting can only
#' apply a subset of `bold`, `italic`, `strikethrough`, `foregroundColor` and
#' `backgroundColor`
#' @export
BooleanRule <- function(
    condition,
    format) {

  out <- list() |>
    append_cond(condition, class = "BooleanCondition", skip_null = FALSE) |>
    append_cond(format, class = "CellFormat", skip_null = FALSE) |>
    dgs4_class("BooleanRule")

  return(out)

}

#' @rdname BooleanRule
#' @param x any R object
#' @export
is.BooleanRule <- function(x) {
  inherits(x, "BooleanRule")
}

#' @title Gradient Rule
#' @description Rule that applies a gradient color scale format, based on provided
#' interpolation points. Used in [ConditionalFormatRule()]
#' @param minpoint,maxpoint objects of class [InterpolationPoint] specifying the
#' starting and final interpolation point.
#' @param midpoint object of class [InterpolationPoint]. Optional midway interpolation
#' point.
#' @export
GradientRule <- function(
    minpoint,
    maxpoint,
    midpoint = NULL){

  out <- list() |>
    append_cond(minpoint, class = "InterpolationPoint", skip_null = FALSE) |>
    append_cond(maxpoint, class = "InterpolationPoint", skip_null = FALSE) |>
    append_cond(midpoint, class = "InterpolationPoint") |>
    dgs4_class("GradientRule")

  return(out)

}

#' @rdname GradientRule
#' @param x any R object
#' @export
is.GradientRule <- function(x) {
  inherits(x, "GradientRule")
}

#' @title Conditional Format Rule
#' @description Rule describing the usage of conditional formatting. Only one
#' of `booleanRule` or `gradientRule` can be specified
#' @param ranges object of class [GridRange] or list of such objects. The ranges
#' that are formatted according to the condition. All ranges must be on the same
#' `GRID` sheet
#' @param booleanRule object of class [BooleanRule]. A rule that is either applied
#' or not depending to the condition
#' @param gradientRule object of class [GradientRule]. Formatting will be varied
#' based on the gradients in the rule.
#' @export
ConditionalFormatRule <- function(
    ranges,
    booleanRule = NULL,
    gradientRule = NULL) {

  ranges <- nest_if_class(ranges, "GridRange")
  ranges <- check_if_all_class(ranges, "GridRange", skip_null = FALSE)

  null_rules <- vapply(list(booleanRule, gradientRule), is.null, logical(1))
  if (sum(null_rules) != 1)
    dgs4_error("Exactly one of {.arg booleanRule} or {.arg gradientRule} needs to be provided.")

  out <- list() |>
    append_cond(ranges) |>
    append_cond(booleanRule, class = "BooleanRule") |>
    append_cond(gradientRule, class = "GradientRule") |>
    dgs4_class("ConditionalFormatRule")

  return(out)
}

#' @rdname ConditionalFormatRule
#' @param x any R object
#' @export
is.ConditionalFormatRule <- function(x) {
  inherits(x, "ConditionalFormatRule")
}
