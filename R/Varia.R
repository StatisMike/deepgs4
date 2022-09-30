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
    deepgs_class("TextFormat")

  if (length(out) == 0)
    deepgs_error("No arguments specified",
                 class = "NoArgsError")

  return(out)

}

#' @rdname TextFormat
#' @param x any R file
#' @export
is.TextFormat <- function(x) {
  inherits(x, "TextFormat")
}


#' @rdname TextFormat
#' @param obj list produced by `deepgs_listinize()`
#' @export
gen_TextFormat <- function(obj) {

  obj[["foregroundColorStyle"]] <- try_to_gen(obj$foregroundColorStyle, "ColorStyle")
  obj[["link"]] <- obj$link$uri

  do.call(TextFormat,
          args = obj)

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
      deepgs_error("Values provided to {.arg red}, {.arg green} and {.arg blue} need to be {.emph numeric} between (inclusive) {.val 0} and {.val 1}",
                   class = "InvalidRGBAError")

    out <- list(
      red = red,
      green = green,
      blue = blue) |>
      deepgs_class("ColorStyle")


    if (!is.null(alpha)) {
      if (!is_valid_rgba(alpha))
        deepgs_error("Value provided to {.arg alpha} needs to be {.cls numeric} between (inclusive) {.val 0} and {.val 1}",
                     class = "InvalidRGBAError")
      out$alpha <- alpha

    }

    return(out)

  }

  themeColorType <- rlang::arg_match(themeColorType)

  out <- list(
    themeColor = themeColorType
  ) |>
    deepgs_class("ColorStyle")

  return(out)
}

#' @rdname ColorStyle
#' @param x Any R object
#' @export
is.ColorStyle <- function(x) {
  inherits(x, "ColorStyle")
}


#' @rdname ColorStyle
#' @param obj list produced by `deepgs_listinize()`
#' @export
gen_ColorStyle <- function(obj) {

  if (!is.null(obj$rgbColor)) {
    args <- obj$rgbColor

    null_i <- which(vapply(c("red", "green", "blue"),
                           \(col) is.null(args[[col]]),
                           logical(1)))

    if (length(null_i) > 0)
      args[c("red", "green", "blue")[null_i]] <- 0

  } else
    args <- list(themeColorType = obj$themeColor)

  do.call(ColorStyle,
          args = args)

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
    deepgs_class("LineStyle")

  return(out)

}

#' @rdname LineStyle
#' @param obj list produced by `deepgs_listinize()`
#' @export
gen_LineStyle <- function(obj) {
  do.call(LineStyle, args = obj)
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
    deepgs_class("PointStyle")

  return(obj)

}

#' @rdname PointStyle
#' @param obj list produced by `deepgs_listinize()`
#' @export
gen_PointStyle <- function(obj) {
  do.call(PointStyle, args = obj)
}

#' @title Coerce Dates to googlesheets Serial Number
#' @param date value to convert. It needs to be a value recognized by
#' [lubridate::as_datetime()]
#' @details
#' "Serial number" format, as popularized by Lotus 1-2-3. The whole number
#' portion of the value (left of the decimal) counts the days since December
#' 30th 1899. The fractional portion (right of the decimal) counts the time
#' as a fraction of the day. For example, January 1st 1900 at noon would be
#' `2.5`, `2` because it's 2 days after December 30th 1899, and `.5` because
#' noon is half a day. February 1st 1900 at 3pm would be `33.625`.
#' @export
deepgs_serial_number <- function(date) {

  diff <- lubridate::as_datetime(date) - lubridate::as_datetime("1899-12-30")
  diff_days <- as.numeric(diff, unit = "days")
  class(diff_days) <- "deepgs_serial_number"

  return(diff_days)

}

#' @rdname deepgs_serial_number
#' @param x object of class `deepgs_serial_number` to coerce
#' @inheritParams base::strptime
#' @export
as.character.deepgs_serial_number <- function(
    x,
    format = "",
    ...) {

  whole_days <- lubridate::as_datetime("1899-12-30") + lubridate::days(floor(x))
  fraction_day <- as.numeric(x - floor(x)) * 86400
  datetime <- whole_days + lubridate::seconds(fraction_day)

  format(datetime, format = format)

}

#' @rdname deepgs_serial_number
#' @export
as.POSIXct.deepgs_serial_number <- function(
    x,
    tz = "",
    ...) {

  as.character(x) |>
    as.POSIXct(tz = tz, ...)

}

#' @rdname deepgs_serial_number
#' @export
as.POSIXlt.deepgs_serial_number <- function(
    x,
    tz = "",
    ...) {

  as.character(x) |>
    as.POSIXlt(tz = tz, ...)

}

#' @rdname deepgs_serial_number
#' @export
as.Date.deepgs_serial_number <- function(
    x,
    ...) {

  as.character(x) |>
    as.Date(...)

}

#' @rdname deepgs_serial_number
#' @export
is.deepgs_serial_number <- function(x) {
  inherits(x, "deepgs_serial_number")
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
    deepgs_class("TextFormatRun")

  return(out)

}

#' @rdname TextFormatRun
#' @param obj list produced by `deepgs_listinize()`
#' @export
gen_TextFormatRun <- function(obj) {
  do.call(TextFormatRun, args = obj)
}

