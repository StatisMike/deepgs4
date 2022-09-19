#' @title SheetProperties
#' @description Properties of a singular sheet in SpreadSheet
#' @param sheetId ID of the sheet
#' @param title Name of the sheet
#' @param index Index of the sheet. If not set during addition, new sheet will
#' be inserted at the end of the sheets
#' @param sheetType Type of the spreadsheet
#' @param gridProperties object of class [GridProperties]. If not set, then I don't know
#' @param hidden boolean indicating if sheet should be hidden in the UI
#' @param tabColorStyle object of class [ColorStyle], describing color of the tab
#' @param rightToLeft boolean indicating if sheet should be RTL
#' @param dataSourceSheetProperties object of [DataSourceSheetProperties]. Field
#' is only accessible during reads from Sheets API.
#' @export
#' @return SheetProperties object

SheetProperties <- function(
    sheetId,
    title,
    index = NULL,
    sheetType = c("GRID", "OBJECT", "DATA_SOURCE"),
    gridProperties = NULL,
    hidden = NULL,
    tabColorStyle = NULL,
    rightToLeft = NULL,
    dataSourceSheetProperties = NULL) {

  sheetType = match.arg(sheetType)

  out <- list(
    sheetId = sheetId,
    title = title,
    sheetType = sheetType
  )

  if (!is.null(index))
    out$index <- index

  if (!is.null(gridProperties)) {
    if (!is.GridProperties(gridProperties))
      stop("'gridProperties' should be a 'GridProperties' object")
    out$gridProperties <- gridProperties
  }

  if (!is.null(hidden)) {
    if (!is.logical(hidden) || length(hidden) != 1)
      stop("'hidden' should be logical of length 1")
    out$hidden <- hidden
  }

  if (!is.null(tabColorStyle)) {
    if (!is.ColorStyle(tabColorStyle))
      stop("'tabColorStyle' should be a 'ColorStyle' object")
    out$tabColorStyle <- tabColorStyle
  }

  if (!is.null(rightToLeft)) {
    if (!is.logical(rightToLeft) || length(rightToLeft) != 1)
      stop("'rigthToLeft' should be logical of length 1")
    out$rightToLeft <- rightToLeft
  }

  if (!is.null(dataSourceSheetProperties)) {
    if (!is.DataSourceSheetProperties(dataSourceSheetProperties))
      stop("'dataSourceSheetProperties' should be a 'DataSourceSheetProperties' object")
    out$dataSourceSheetProperties <- dataSourceSheetProperties
  }

  class(out) <- "SheetProperties"

  return(out)

}

#' @rdname SheetProperties
#' @param x any R object
is.SheetProperties <- function(x)
  inherits(x, "SheetProperties")
