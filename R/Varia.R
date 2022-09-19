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
#' @export
TextFormat <- function(
    foregroundColorStyle = NULL,
    fontFamily = NULL,
    fontSize = NULL,
    bold = NULL,
    italic = NULL,
    strikethrough = NULL,
    underline = NULL,
    link = NULL) {

  out <- list()

  if (!is.null(foregroundColorStyle)) {
    if (!is.ColorStyle(foregroundColorStyle))
      stop("'foregroundColorStyle' needs to be of 'ColorStyle' class")
    out$foregroundColorStyle <- foregroundColorStyle
  }

  if (!is.null(fontFamily))
    out$fontFamily <- fontFamily

  if (!is.null(fontSize))
    out$fontSize <- fontSize

  if (!is.null(bold))
    out$bold <- bold

  if (!is.null(italic))
    out$italic <- italic

  if (!is.null(strikethrough))
    out$strikethrough <- strikethrough

  if (!is.null(underline))
    out$underline <- underline

  if (!is.null(link))
    out$link <- list(uri = link)

  if (length(out) == 0)
    stop("No arguments specified.")

  class(out) <- "TextFormat"

  return(out)

}

#' @rdname TextFormat
#' @param x any R file
#' @export

is.TextFormat <- function(x)
  inherits("TextFormat")

#' @title Generate TextFormat
#' @description Function used internally to construct objects on read
#' @noRd
gen_TextFormat <- function(obj) {

  args <- obj[c("fontFamily", "fontSize", "bold", "italic", "strikethrough",
                "underline")]

  if (!is.null(obj$foregroundColorStyle))
    args$foregroundColorStyle <- gen_ColorStyle(obj$foregroundColorStyle)

  if (!is.null(obj$link))
    args$link <- obj$link$uri

  do.call(TextFormat,
          args = args)

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
#' @param themeColorType One of the colors set in the theme of the spreadsheet.
#' See **details** for more info.
#' @param red,green,blue Floats between `0` and `1`, describing amount of given
#' tone in the color. If all are given, they will take precedence before `themeColorType`
#' @param alpha Float between `0` and `1`. Not universally valid through the
#' Sheets API
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
    themeColorType = c("TEXT", "BACKGROUND", "ACCENT1", "ACCENT2", "ACCENT3",
                       "ACCENT4", "ACCENT5", "ACCENT6", "LINK"),
    red,
    green,
    blue,
    alpha = NULL) {

  if (!any(missing(red), missing(green), missing(blue))) {

    if (!all(is_valid_rgba(red), is_valid_rgba(green), is_valid_rgba(blue)))
      stop("Values provided to 'red', 'green' and 'blue' need to be numeric between (inclusive) 0 and 1")

    out <- list(
      Color = list(
        red = red,
        green = green,
        blue = blue
      )
    )

    if (!is.null(alpha)) {
      if (is_valid_rgba(alpha))
        stop("Value provided to 'alpha' need to be numeric between (inclusive) 0 and 1")
      out$Color$alpha <- alpha

    }

    class(out) <- "ColorStyle"
    return(out)

  }

  themeColorType <- match.arg(themeColorType)

  out <- list(
    themeColor = themeColorType
  )

  class(out) <- "ColorStyle"
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

  if (!is.null(obj$Color))
    args <- list(red = obj$Color$red,
                 green = obj$Color$green,
                 blue = obj$Color$blue,
                 alpha = obj$Color$alpha)

  else
    args <- list(themeColorType = obj$themeColor)

  do.call(ColorStyle,
          args = args)

}
