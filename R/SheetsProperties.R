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
#' @param ... additional deprecated fields returned from GoogleSheet API
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
    dataSourceSheetProperties = NULL,
    ...) {

  sheetType = rlang::arg_match(sheetType)

  out <- list(
    sheetId = sheetId,
    title = title,
    sheetType = sheetType
  ) |>
    append_cond(index, class = c("numeric", "integer")) |>
    append_cond(gridProperties, class = "GridProperties") |>
    append_cond(hidden, class = "logical") |>
    append_cond(tabColorStyle, class = "ColorStyle") |>
    append_cond(rightToLeft, class = "logical") |>
    append_cond(dataSourceSheetProperties,
                class = "DataSourceSheetProperties") |>
    deepgs_class("SheetProperties")

  return(out)

}

#' @rdname SheetProperties
#' @param x any R object
is.SheetProperties <- function(x)
  inherits(x, "SheetProperties")

#' @title Generate GridProperties
#' @noRd
gen_SheetProperties <- function(obj) {

  args <- list(
    sheetId = obj$sheetId,
    title = obj$title,
    index = obj$index,
    sheetType = obj$sheetType,
    gridProperties = try_to_gen(obj$gridProperties,
                                "GridProperties"),
    hidden = obj$hidden,
    tabColorStyle = try_to_gen(obj$tabColorStyle,
                               "ColorStyle"),
    rightToLeft = obj$rightToLeft,
    dataSourceSheetProperties = NULL
  )

  do.call(SheetProperties, args = args)

}
