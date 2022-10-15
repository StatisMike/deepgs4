#' @title TextFormat
#' @description Class holding specification for text format. All arguments
#' are optional, but at least one must be present for object creation.
#' @param foregroundColorStyle object of class `ColorStyle`
#' @param fontFamily name of the font family
#' @param fontSize size of the font
#' @param bold boolean: should the text be bolded?
#' @param italic boolean: should the text be italic?
#' @param strikethrough boolean: should the text be strikethrough?
#' @param underline boolean: should the text be underlined?
#' @param link URL if text should link somewhere
#' @param ... additional deprecated fields returned from GoogleSheet API
#' @export
TextFormat <- function(
    foregroundColorStyle = NULL,
    fontFamily = NULL,
    fontSize = NULL,
    bold = NULL,
    italic = NULL,
    strikethrough = NULL,
    underline = NULL,
    link = NULL,
    ...) {

  out <- list() |>
    append_cond(foregroundColorStyle, class = "ColorStyle") |>
    append_cond(fontFamily, type = "character") |>
    append_cond(fontSize, type = "integer") |>
    append_cond(bold, type = "logical") |>
    append_cond(italic, type = "logical") |>
    append_cond(strikethrough, type = "logical") |>
    append_cond(underline, type = "logical") |>
    append_cond(link, type = "character") |>
    dgs4_class("TextFormat")

  if (length(out) == 0)
    dgs4_error("No arguments specified",
               class = "NoArgsError")

  return(out)

}

#' @rdname TextFormat
#' @param x any R file
#' @export
is.TextFormat <- function(x) {
  inherits(x, "TextFormat")
}

#' @title Validate RGBA
#' @param x RGBA value
#' @noRd
is_valid_rgba <- function(x) {

  is.numeric(x) &&
    x >= 0 &&
    x <= 1

}

#' @title ColorStyle
#' @param red,green,blue Floats between `0` and `1`, describing amount of given
#' tone in the color. If all are given, they will take precedence before `themeColorType`
#' @param alpha Float between `0` and `1`. Not universally valid through the
#' Sheets API
#' @param themeColorType One of the colors set in the theme of the spreadsheet.
#' See **details** for more info.
#' @details
#' ColorStyle can be represented in two ways: manually, by providing
#' `red`, `green`, `blue` (and, optionally `alpha`) values describing
#' the color, or dynamically, by assigning one of `themeColorType`s, which are
#' set in the [SpreadsheetProperties]. Valid styles:
#' - *TEXT*: Represents the primary text color
#' - *BACKGROUND*: Represents the primary background color
#' - *ACCENT1* to *ACCENT6*: Represents accents color
#' - *LINK*: Represents the color to use for hyperlinks
#' @return ColorStyle object
#' @export

ColorStyle <- function(
    red,
    green,
    blue,
    alpha = NULL,
    themeColorType = c("TEXT", "BACKGROUND", "ACCENT1", "ACCENT2", "ACCENT3",
                       "ACCENT4", "ACCENT5", "ACCENT6", "LINK")) {

  if (!any(missing(red), missing(green), missing(blue))) {

    if (!all(is_valid_rgba(red), is_valid_rgba(green), is_valid_rgba(blue)))
      dgs4_error("Values provided to {.arg red}, {.arg green} and {.arg blue} need to be {.emph numeric} between (inclusive) {.val 0} and {.val 1}",
                 class = "InvalidRGBAError")

    out <- list(
      red = red,
      green = green,
      blue = blue) |>
      dgs4_class("ColorStyle")


    if (!is.null(alpha)) {
      if (!is_valid_rgba(alpha))
        dgs4_error("Value provided to {.arg alpha} needs to be {.cls numeric} between (inclusive) {.val 0} and {.val 1}",
                   class = "InvalidRGBAError")
      out$alpha <- alpha

    }

    return(out)

  }

  themeColorType <- rlang::arg_match(themeColorType)

  out <- list(
    themeColor = themeColorType
  ) |>
    dgs4_class("ColorStyle")

  return(out)
}

#' @rdname ColorStyle
#' @param x Any R object
#' @export
is.ColorStyle <- function(x) {
  inherits(x, "ColorStyle")
}

#' @title Chart line style specification
#' @description Object allowing for specifying the chart line style.
#' @param width width of the line in pixels
#' @param type type of the line. For description of each type, check details section.
#' @details
#' Valid line types and their description:
#' - *SOLID*: A solid line.
#' - *DOTTED*: A dotted line.
#' - *MEDIUM_DASHED*: A dashed line where the dashes have "medium" length.
#' - *MEDIUM_DASHED_DOTTED*: A line that alternates between a "medium" dash and a dot.
#' - *LONG_DASHED*: A dashed line where the dashes have "long" length.
#' - *LONG_DASHED_DOTTED*: A line that alternates between a "long" dash and a dot.
#' - *INVISIBLE*: No dash type, which is equivalent to a non-visible line.
#' - *CUSTOM*: A custom dash for a line. Modifying the exact custom dash style is currently unsupported.
#' @export

LineStyle <- function(
    width,
    type = c("SOLID", "DOTTED", "MEDIUM_DASHED", "MEDIUM_DASHED_DOTTED", "LONG_DASHED", "LONG_DASHED_DOTTED", "INVISIBLE", "CUSTOM")) {

  type <- rlang::arg_match(type)

  out <- list() |>
    append_cond(width, type = "integer", skip_null = FALSE) |>
    append_cond(type) |>
    dgs4_class("LineStyle")

  return(out)

}

#' @title Chart point style specification
#' @description Object allowing for specifying the chart point style.
#' @param size Point size. If empty, default size is used
#' @param shape type of the point. If kept as `"POINT_SHAPE_UNSPECIFIED"`, the
#' default shape is used.
#' @export
PointStyle <- function(
    size = NULL,
    shape = c("POINT_SHAPE_UNSPECIFIED", "CIRCLE", "DIAMOND", "HEXAGON", "PENTAGON",
              "SQUARE", "STAR", "TRIANGLE", "X_MARK")) {

  shape <- rlang::arg_match(shape)

  obj <- list() |>
    append_cond(size, type = "numeric") |>
    append_cond(shape) |>
    dgs4_class("PointStyle")

  return(obj)

}

#' @title TextFormat for part of the cell value
#' @description Object holding specification for usage with [CellData()] to
#' keep information about only part of a cell. The format of this run continues
#' until the start index of the next run.
#' @param startIndex index (zero-based) of character on which this format run.
#' Can be kept `NULL` for beginning of the value.
#' @param textFormat object of class [TextFormat] with format specification to
#' apply
#' @export
TextFormatRun <- function(
    startIndex = NULL,
    textFormat) {

  if (is.null(startIndex))
    startIndex <- 0

  out <- list() |>
    append_cond(startIndex, type = "integer") |>
    append_cond(textFormat, class = "TextFormat", skip_null = FALSE) |>
    dgs4_class("TextFormatRun")

  return(out)

}

