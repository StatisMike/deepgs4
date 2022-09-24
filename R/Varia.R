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
    deepsh_error("No arguments specified",
                 class = "NoArgsError")

  return(out)

}

#' @rdname TextFormat
#' @param x any R file
#' @export

is.TextFormat <- function(x)
  inherits(x, "TextFormat")

#' @title Generate TextFormat
#' @description Function used internally to construct objects on read
#' @noRd
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
#' set in the [SpreadSheetProperties]. Valid styles:
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
      deepsh_error("Values provided to {.arg red}, {.arg green} and {.arg blue} need to be {.emph numeric} between (inclusive) {.val 0} and {.val 1}",
                   class = "InvalidRGBAError")

    out <- list(
      red = red,
      green = green,
      blue = blue) |>
      deepgs_class("ColorStyle")


    if (!is.null(alpha)) {
      if (!is_valid_rgba(alpha))
        deepsh_error("Value provided to {.arg alpha} needs to be {.cls numeric} between (inclusive) {.val 0} and {.val 1}",
                     class = "InvalidRGBAError")
      out$alpha <- alpha

    }

    return(out)

  }

  themeColorType <- match.arg(themeColorType)

  out <- list(
    themeColor = themeColorType
  ) |>
    deepgs_class("ColorStyle")

  return(out)
}

#' @rdname ColorStyle
#' @param x Any R object
#' @export
is.ColorStyle <- function(x)
  inherits(x, "ColorStyle")

#' @title Generate ColorStyle
#' @description Function used internally to construct objects on read
#' @noRd
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
