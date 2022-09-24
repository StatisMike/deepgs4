#' @title Cell Format
#' @description Specification for format of the cells
#' @param numberFormat object of [NumberFormat] class. Describes how the
#' numbers are to be formatted within the cells
#' @param backgrundColorStyle object of [ColorStyle] class, describing the color
#' of cell background
#' @param borders object of [Borders] class, describing the borders of the cell
#' @param padding object of [Padding] class, describing the padding around the cell
#' @param horizontalAlignment,verticalAlignment horizontal and vertical alignments
#' of values in the cells
#' @param wrapStrategy strategy for wrapping values in the cell
#' @param textDirection direction of the text in the cell
#' @param textFormat object of [TextFormat] class, describing format of the text
#' in cells
#' @param hyperlinkDisplayType how hyperlinks should be displayed in cells
#' @param textRotation rotation applied to text in the cell. Either `v` for vertical
#' or integer between `-90` and `90` for angle of rotation
#' @param ... additional deprecated fields returned from GoogleSheet API
#' @details
#' Valid values for enumerative fields:
#' - **verticalAlignment**:
#'   - `TOP`,
#'   - `MIDDLE`,
#'   - `BOTTOM`
#' - **horizontalAlignment**
#'   - `LEFT`
#'   - `CENTER`
#'   - `RIGHT`
#' - **wrapStrategy**
#'   - `OVERFLOW_CELL`: lines that are longer than the cell width will be written
#'   in the next cell over, unless it isn't emppty - then the same behaviour as
#'   in `CLIP` is used
#'   - `LEGACY_WRAP`: old GoogleSheets strategy, not supported on all platforms.
#'   Words wider than cell width are clipped, not broken. Its usage is discouraged.
#'   - `CLIP`: Lines longer than cell width will be clipped.
#'   - `WRAP`: Words longer than cell width are wrapped into new line.
#' - **textDirection**:
#'   - `LEFT_TO_RIGHT`
#'   - `RIGHT_TO_LEFT`
#' - **hyperlingDisplayType**:
#'   - `LINKED`: hyperlink is explicitly rendered
#'   - `PLAIN_TEXT`: hyperlink is not rendered
#'
#' @export
CellFormat <- function(
  numberFormat = NULL,
  backgroundColorStyle = NULL,
  borders = NULL,
  padding = NULL,
  horizontalAlignment = NULL,
  verticalAlignment = NULL,
  wrapStrategy = NULL,
  textDirection = NULL,
  textFormat = NULL,
  hyperlinkDisplayType = NULL,
  textRotation = NULL,
  ...
) {

  horizontalAlignment <- check_if_options(horizontalAlignment, "LEFT", "CENTER", "RIGHT")
  verticalAlignment <- check_if_options(verticalAlignment, "TOP", "MIDDLE", "BOTTOM")
  wrapStrategy <- check_if_options(wrapStrategy, "OVERFLOW_CELL", "LEGACY_WRAP",
                                   "CLIP", "WRAP")
  textDirection <- check_if_options(textDirection, "LEFT_TO_RIGHT", "RIGTH_TO_LEFT")
  hyperlinkDisplayType <- check_if_options(hyperlinkDisplayType, "LINKED", "PLAIN_TEXT")
  textRotation <- check_if_options(
    textRotation, -90:90, "v",
    custom_message = "{.arg arg} needs to be either integer between {.val -90} and {.val 90} for rotation angle {.emph OR} {.val v} for vertical text."
    )


  out <- list() |>
    append_cond(numberFormat, class = "NumberFormat") |>
    append_cond(backgroundColorStyle, class = "ColorStyle") |>
    append_cond(borders, class = "Borders") |>
    append_cond(padding, class = "Padding") |>
    append_cond(horizontalAlignment) |>
    append_cond(verticalAlignment) |>
    append_cond(wrapStrategy) |>
    append_cond(textDirection) |>
    append_cond(textFormat, class = "TextFormat") |>
    append_cond(hyperlinkDisplayType) |>
    append_cond(textRotation) |>
    deepgs_class("CellFormat")

  return(out)

}

#' @title Generate CellFormat
#' @noRd
gen_CellFormat <- function(obj) {

  numberFormat <- try_to_gen(obj$numberFormat, "NumberFormat")
  backgroundColorStyle <- try_to_gen(obj$backgroundColorStyle, "ColorStyle")
  borders <- try_to_gen(obj$borders, "Borders")
  padding <- try_to_gen(obj$padding, "Padding")
  textFormat <- try_to_gen(obj$textFormat, "TextFormat")

  args <- list() |>
    append_cond(numberFormat) |>
    append_cond(backgroundColorStyle) |>
    append_cond(borders) |>
    append_cond(padding) |>
    append_cond(textFormat) |>
    append_cond(obj$textDirection, "textDirection") |>
    append_cond(obj$horizontalAlignment, "horizontalAlignment") |>
    append_cond(obj$verticalAlignment, "verticalAlignment") |>
    append_cond(obj$wrapStrategy, "wrapStrategy") |>
    append_cond(obj$hyperlinkDisplayType, "hyperlinkDisplayType") |>
    append_cond(obj$textRotation$angle, "textRotation")

  if (isTRUE(obj$textRotation$vertical))
    args[["textRotation"]] <- TRUE

  do.call(CellFormat, args = args)

}


#' @title Number Format
#' @description Specification for numeric values display
#' @param type Type of number format. See details for meaning of every field.
#' Required during updates.
#' @param pattern pattern used for formatting - if unset, default
#' patter of user's locale will be used if valid for given type
#' @details
#' - **type**:
#'   - `TEXT`: Text formatting, eg. `1000.12`
#'   - `NUMBER`: Number formatting, eg. `1,000.12`
#'   - `PERCENT`: Percent formatting, eg. `10.12%`
#'   - `CURRENCY`: Currency formatting, eg. `$1,000.12`
#'   - `DATE`: Date formatting, eg. `9/26/2008`
#'   - `TIME`: Time formatting, eg. `3:59:00 PM`
#'   - `DATE_TIME`: Date+Time formatting, eg. `9/26/08 15:59:00`
#'   - `SCIENTIFIC`: Scientific number formatting, eg. `1.01E+03`
#' - **pattern**:
#'   Detailed information is too verbose for inclusion here. For details
#'   refer to \href{https://developers.google.com/sheets/api/guides/formats}(GoogleSheets Date and Number Formats guide)
#' @export
NumberFormat <- function(
    type = c("TEXT", "NUMBER", "PERCENT", "CURRENCY", "DATE", "TIME",
             "DATE_TIME", "SCIENTIFIC"),
    pattern = NULL) {

  type <- rlang::arg_match(type)

  out <- list(type) |>
    append_cond(pattern, type = "character") |>
    deepgs_class("NumberFormat")

  return(out)

}

#' @title Generate NumberFormat
#' @noRd
gen_NumberFormat <- function(obj) {
  do.call(NumberFormat, args = obj)
}


#' @title Cell border specification
#' @param top_style,bottom_style,left_style,right_style Type of the top, bottom,
#' left and right borders.
#' @param top_colorStyle,bottom_colorStyle,left_colorStyle,right_colorStyle Object
#' of class [ColorStyle()] describing color for top, bottom, left and right
#' border
#' @details
#' **Border styles**:
#' - `DOTTED`: The border is dotted.
#' - `DASHED`: The border is dashed.
#' - `SOLID`: The border is a thin solid line.
#' - `SOLID_MEDIUM`: The border is a medium solid line.
#' - `SOLID_THICK`: The border is a thick solid line.
#' - `NONE`: No border. Used only when updating a border in order to erase it.
#' - `DOUBLE`: The border is two solid lines.
#' @export
Borders <- function(
    top_style = c("DOTTED", "DASHED", "SOLID", "SOLID_MEDIUM", "SOLID_THICK", "NONE", "DOUBLE"),
    bottom_style = c("DOTTED", "DASHED", "SOLID", "SOLID_MEDIUM", "SOLID_THICK", "NONE", "DOUBLE"),
    left_style = c("DOTTED", "DASHED", "SOLID", "SOLID_MEDIUM", "SOLID_THICK", "NONE", "DOUBLE"),
    right_style = c("DOTTED", "DASHED", "SOLID", "SOLID_MEDIUM", "SOLID_THICK", "NONE", "DOUBLE"),
    top_colorStyle = NULL,
    bottom_colorStyle = NULL,
    left_colorStyle = NULL,
    right_colorStyle = NULL) {

  top_style <- rlang::arg_match(top_style)
  bottom_style <- rlang::arg_match(bottom_style)
  left_style <- rlang::arg_match(left_style)
  right_style <- rlang::arg_match(right_style)

  out <- list(
    top_style = top_style,
    bottom_style = bottom_style,
    left_style = left_style,
    right_style = right_style
  ) |>
    append_cond(top_colorStyle, class = "ColorStyle") |>
    append_cond(bottom_colorStyle, class = "ColorStyle") |>
    append_cond(left_colorStyle, class = "ColorStyle") |>
    append_cond(right_colorStyle, class = "ColorStyle") |>
    deepgs_class("Borders")

  return(out)

}

#' @title Generate Borders
#' @noRd
gen_Borders <- function(obj) {

  top_colorStyle <- try_to_gen(obj$top$colorStyle, "ColorStyle")
  bottom_colorStyle <- try_to_gen(obj$bottom$colorStyle, "ColorStyle")
  left_colorStyle <- try_to_gen(obj$left$colorStyle, "ColorStyle")
  right_colorStyle <- try_to_gen(obj$right$colorStyle, "ColorStyle")

  args <- list(
    top_style = obj$top$style,
    bottom_style = obj$bottom$style,
    left_style = obj$left$style,
    right_style = obj$right$style
  ) |>
    append_cond(top_colorStyle) |>
    append_cond(bottom_colorStyle) |>
    append_cond(left_colorStyle) |>
    append_cond(right_colorStyle)

  do.call(Borders, args = args)
}

#' @title Cell padding
#' @param top,bottom,left,right Padding in pixels for each side. During update,
#' all fields need to be set
#' @export

Padding <- function(
    top,
    bottom,
    left,
    right){

  out <- list() |>
    append_cond(top, type = "integer", skip_null = FALSE) |>
    append_cond(bottom, type = "integer", skip_null = FALSE) |>
    append_cond(left, type = "integer", skip_null = FALSE) |>
    append_cond(right, type = "integer", skip_null = FALSE) |>
    deepgs_class("Padding")

  return(out)

}

#' @title Generate Padding
#' @noRd
gen_Padding <- function(obj) {
  do.call(Padding, args = obj)
}
