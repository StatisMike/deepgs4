#' @title TextFormat
#' @description Class holding specification for text format. All arguments
#' are optional, but at least one must be present for object creation.
#' @param foregroundColorStyle object of class `foregroundColorStyle`
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

  if (!is.null(foregroundColorStyle))
    out$foregroundColorStyle <- foregroundColorStyle

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




