#' @title Cell Format
#' @description Specification for format of the cells
#' @param numberFormat object of [NumberFormat] class. Describes how the
#' numbers are to be formatted within the cells
#' @param backgroundColorStyle object of [ColorStyle] class, describing the color
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

  if (length(out) == 0)
    deepgs_error("No arguments specified",
                 class = "NoArgsError")

  return(out)

}

#' @rdname CellFormat
#' @param x any R object
#' @export
is.CellFormat <- function(x) {
  inherits(x, "CellFormat")
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
#'   refer to [GoogleSheets Date and Number Formats guide](https://developers.google.com/sheets/api/guides/formats)
#' @export
NumberFormat <- function(
    type = c("TEXT", "NUMBER", "PERCENT", "CURRENCY", "DATE", "TIME",
             "DATE_TIME", "SCIENTIFIC"),
    pattern = NULL) {

  type <- rlang::arg_match(type)

  out <- list(type = type) |>
    append_cond(pattern, type = "character") |>
    deepgs_class("NumberFormat")

  return(out)

}

#' @rdname NumberFormat
#' @param x any R object
#' @export
is.NumberFormat <- function(x) {
  inherits(x, "NumberFormat")
}

#' @title Border specification
#' @param style Type of the border.
#' @param colorStyle object of class [ColorStyle] describing color of the border
#' @param ... additional deprecated fields returned from GoogleSheet API
#' @details
#' **Border styles**:
#' - `SOLID`: The border is a thin solid line.
#' - `SOLID_MEDIUM`: The border is a medium solid line.
#' - `SOLID_THICK`: The border is a thick solid line.
#' - `DOTTED`: The border is dotted.
#' - `DASHED`: The border is dashed.
#' - `NONE`: No border. Used only when updating a border in order to erase it.
#' - `DOUBLE`: The border is two solid lines.
#' @export
Border <- function(style = c("SOLID", "SOLID_MEDIUM", "SOLID_THICK", "DOTTED", "DASHED", "NONE", "DOUBLE"),
                   colorStyle = NULL) {

  style <- rlang::arg_match(style)

  out <- list() |>
    append_cond(style) |>
    append_cond(colorStyle, class = "ColorStyle") |>
    deepgs_class("Border")


  return(out)

}

#' @rdname Border
#' @param top,bottom,left,right Objects of class [Border] declaring style
#' of specified border in a cell
#' @export
Borders <- function(
    top = NULL,
    bottom = NULL,
    left = NULL,
    right = NULL) {

  out <- list() |>
    append_cond(top, class = "Border") |>
    append_cond(bottom, class = "Border") |>
    append_cond(left, class = "Border") |>
    append_cond(right, class = "Border") |>
    deepgs_class("Borders")

  return(out)

}

#' @rdname Border
#' @param x any R object
#' @export
is.Border <- function(x) {
  inherits(x, "Border")
}

#' @rdname Border
#' @param x any R object
#' @export
is.Borders <- function(x) {
  inherits(x, "Borders")
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

#' @rdname Padding
#' @param x any R object
#' @export
is.Padding <- function(x) {
  inherits(x, "Padding")
}

#' @title Error Value
#' @description Error value of [ExtendedValue] - signalizing error of the
#' cell value.
#' @param type Error type
#' @param message A message with more information about the error (in the
#' spreadsheet's locale).
#' @details
#' Error values and their meaning:
#' - `ERROR_TYPE_UNSPECIFIED`: The default error type, do not use this.
#' - `ERROR`: Corresponds to the #ERROR! error.
#' - `NULL_VALUE`: Corresponds to the #NULL! error.
#' - `DIVIDE_BY_ZERO`: Corresponds to the #DIV/0 error.
#' - `VALUE`: Corresponds to the #VALUE! error.
#' - `REF`: Corresponds to the #REF! error.
#' - `NAME`: Corresponds to the #NAME? error.
#' - `NUM`: Corresponds to the #NUM! error.
#' - `N_A`: Corresponds to the #N/A error.
#' - `LOADING`: Corresponds to the Loading... state.
#' @export
ErrorValue <- function(
    type = c("ERROR", "NULL_VALUE", "DIVIDE_BY_ZERO", "VALUE", "REF",
             "NAME", "NUM", "N_A", "LOADING"),
    message) {

  type <- rlang::arg_match(type)

  out <- list() |>
    append_cond(type) |>
    append_cond(message, skip_null = FALSE, type = "character") |>
    deepgs_class("ErrorValue")

  return(out)

}

#' @rdname ErrorValue
#' @param x any R object
#' @export
is.ErrorValue <- function(x) {
  inherits(x, "ErrorValue")
}

#' @title Value of the cell
#' @description Object holding specification of cell value in [CellData]. Only
#' one of the parameters can be specified.
#' @param numberValue Represents a double value. Date, Times and DateTimes
#' need to be coerced first to [deepgs_serial_number()]
#' @param stringValue String value. Leading single quotes with numbers aren't
#' included
#' @param boolValue Logical value.
#' @param formulaValue Represents a formula to be calculated
#' @param errorValue object of class [ErrorValue] signaling error in the cell
#' @export
ExtendedValue <- function(
    numberValue = NULL,
    stringValue = NULL,
    boolValue = NULL,
    formulaValue = NULL,
    errorValue = NULL) {

  null_args <- vapply(
    list(numberValue, stringValue, boolValue, formulaValue, errorValue),
    is.null,
    logical(1))

  if (sum(null_args) != 4)
    deepgs_error("Only one argument can be specified.")

  if (is.deepgs_serial_number(numberValue))
    numberValue <- as.numeric(numberValue)

  out <- list() |>
    append_cond(numberValue, type = "numeric") |>
    append_cond(stringValue, type = "character") |>
    append_cond(boolValue, type = "logical") |>
    append_cond(formulaValue, type = "character") |>
    append_cond(errorValue, class = "ErrorValue")

  value_type <- gsub(names(out), pattern = "Value", replacement = "")

  out <- out[[1]] |>
    deepgs_class("ExtendedValue")

  attr(out, "type") <- value_type

  return(out)

}

#' @rdname ExtendedValue
#' @param x any R object
is.ExtendedValue <- function(x) {
  inherits(x, "ExtendedValue")
}

#' @title Cell Data
#' @description Specification of data contained in singular sheet cell
#' @param userEnteredValue object of class [ExtendedValue]. Value that user
#' entered in the cell.
#' @param userEnteredFormat object of class [CellFormat]. Format that user
#' provided for the cell. During write, new format will be merged with
#' existing format.
#' @param note Note entered on the cell.
#' @param textFormatRuns List of objects of class [TextFormatRun]. Sequence
#' of formats if parts of the value needs to have different formatting
#' @param dataValidation object of class [DataValidationRule]. Specification
#' for data validation for a cell. During write, new data validation rule will
#' overwrite prior rule
#' @param pivotTable object of class [PivotTable]. Pivot table anchored at this
#' cell. Size of pivot table is computed dynamically, only the top-left cell
#' contains definition of pivot table: all other cells will contain calculated
#' values of the results in their `effectiveValue` fields
#' @param effectiveValue **ReadOnly** Object of class [ExtendedValue].
#' For cells with formulas: calculated value. For cells with literals,
#' the same as `userEnteredValue`.
#' @param formattedValue **ReadOnly** Formatted value of the cell: Value as
#' it's shown to the user. **ReadOnly**
#' @param effectiveFormat **ReadOnly** Object of class [CellFormat]. Effective
#' format of the cell: results of conditional formatting, computed number
#' format if cell contains formula. If it would be the same as `userEnteredFormat`,
#' it won't be received
#' @param hyperlink **ReadOnly** Hyperlink this cell points to, if any. For
#' multiple hyperlinks: it would be empty. To set hyperlink, you can use
#' `"=HYPERLINK"` formula for [ExtendedValue] provided to `userEnteredValue`
#' or link with [textFormat] provided to `userEnteredFormat` (for one link)
#' or in `textFormatRuns` for multiple links or link spanning part of the value
#' @param ... Other
#' @export
CellData <- function(
    userEnteredValue = NULL,
    userEnteredFormat = NULL,
    note = NULL,
    textFormatRuns = NULL,
    dataValidation = NULL,
    pivotTable = NULL,
    effectiveValue = NULL,
    formattedValue = NULL,
    effectiveFormat = NULL,
    hyperlink = NULL,
    ...) {

  if (!is.null(textFormatRuns))
    textFormatRuns <- check_if_all_class(textFormatRuns, "TextFormatRun")

  out <- list() |>
    append_cond(userEnteredValue, class = "ExtendedValue") |>
    append_cond(userEnteredFormat, class = "CellFormat") |>
    append_cond(note, type = "character") |>
    append_cond(textFormatRuns) |>
    append_cond(dataValidation, class = "DataValidationRule") |>
    append_cond(pivotTable, class = "PivotTable") |>
    append_cond(effectiveValue, class = "ExtendedValue") |>
    append_cond(formattedValue, type = "character") |>
    append_cond(effectiveFormat, class = "CellFormat") |>
    append_cond(hyperlink, type = "class") |>
    deepgs_class("CellData")

  return(out)

}

#' @rdname CellData
#' @param x any R object
#' @export
is.CellData <- function(x) {
  inherits(x, "CellData")
}

#' @title RowData
#' @description Values of one spreadsheet row
#' @param values object of class [CellData] or list of such objects. Each one
#' provides information about one sheet cell
#' @export
RowData <- function(values) {

  if (is.CellData(values))
    values <- list(values)

  out <- check_if_all_class(values, "CellData", skip_null = F) |>
    deepgs_class("RowData")

  return(out)

}

#' @rdname RowData
#' @param x any R object
#' @export
is.RowData <- function(x) {
  inherits(x, "RowData")
}

#' @title Specification of data in a grid
#' @description Data in a grid, as well as its dimensions metadata. This function
#' is a low-level constructor for `GridData`, requiring other `deepgsheets4`
#' objects. For higher-level wrapper, use [to_GridData_from_df()].
#' @param startRow,startColumn zero-based indices of first row and column
#' this `GridData` refers to
#' @param rowData object of class [RowData] or list of such objects. Describes
#' data of the cells, one entry per row
#' @param rowMetadata,columnMetadata objects of class [DimensionProperties] or
#' lists of such objects. Properties of requested rows and columns in the grid
#' (starting with ones in `startRow` and `startColumn`)
#' @export

GridData <- function(
    startRow = NULL,
    startColumn = NULL,
    rowData = NULL,
    rowMetadata = NULL,
    columnMetadata = NULL) {

  rowData <- nest_if_class(rowData, "RowData") |>
    check_if_all_class("RowData")
  rowMetadata <- nest_if_class(rowMetadata, "DimensionProperties") |>
    check_if_all_class("DimensionProperties")
  columnMetadata <- nest_if_class(columnMetadata, "DimensionProperties") |>
    check_if_all_class("DimensionProperties")

  out <- list() |>
    append_cond(startRow, type = "integer") |>
    append_cond(startColumn, type = "integer") |>
    append_cond(rowData) |>
    append_cond(rowMetadata) |>
    append_cond(columnMetadata) |>
    deepgs_class("GridData")

  return(out)

}

#' @rdname GridData
#' @param x any R object
#' @export
is.GridData <- function(x) {
  inherits(x, "GridData")
}
