#' @title SpreadsheetProperties
#' @description Properties of whole spreadsheet
#' @param title Title of the spreadsheet
#' @param locale Locale of the spreadsheet
#' @param autoRecalc Amount of time to wait before recalculation of
#' volatile functions
#' @param timeZone Time zone of the spreadsheet, in `CLDR` format
#' @param defaultFormat object of [CellFormat] class: default format of all cells.
#' @param iterativeCalculationSettings object of [IterativeCalculationSettings]
#' declaring how circular references are resolved with iterative calculation
#' @param spreadsheetTheme object of [SpreadsheetTheme] class: theme applied to
#' spreadsheet
#' @param ... additional deprecated fields returned from GoogleSheet API
#' @export
SpreadsheetProperties <- function(
    title,
    locale,
    autoRecalc = c("ON_CHANGE", "MINUTE", "HOUR"),
    timeZone = NULL,
    defaultFormat = NULL,
    iterativeCalucationSettings = NULL,
    spreadsheetTheme = NULL,
    ...) {

  autoRecalc <- rlang::arg_match(autoRecalc)

  out <- list(
    title = title,
    locale = locale,
    autoRecalc = autoRecalc
  ) |>
    append_cond(timeZone, class = "character") |>
    append_cond(defaultFormat, class = "CellFormat") |>
    append_cond(iterativeCalucationSettings,
                class = "IterativeCalucationSettings") |>
    append_cond(spreadsheetTheme,
                class = "SpreadsheetTheme") |>
    deepgs_class("SpreadsheetProperties")

  return(out)

}

#' @title Generate GridProperties
#' @noRd
gen_SpreadsheetProperties <- function(obj) {

  obj$defaultFormat <- try_to_gen(obj$defaultFormat, "CellFormat")
  obj$iterativeCalculationSettings <-
    try_to_gen(obj$iterativeCalculationSettings, "IterativeCalculationSettings")
  obj$spreadsheetTheme <- try_to_gen(obj$spreadsheetTheme,
                                     "SpreadsheetTheme")

  do.call(SpreadsheetProperties, args = obj)
}

#' @title Settings of iterative calculations
#' @description Settings to control how circular dependencies are resolved with
#' iterative calculation.
#' @param maxIterations When iterative calculation is enabled, the maximum
#' number of calculation rounds to perform.
#' @param convergenceThreshold When iterative calculation is enabled and
#' successive results differ by less than this threshold value, the calculation
#' rounds stop.
#' @export
IterativeCalculationSettings <- function(
    maxIterations,
    convergenceThreshold = NULL) {

  out <- list() |>
    append_cond(maxIterations, type = "integer") |>
    append_cond(convergenceThreshold, type = "numeric") |>
    deepgs_class("IterativeCalculationSetting")

  return(out)

}

#' @title Generate IterativeCalculationSetting
#' @noRd
gen_IterativeCalculationSettings <- function(obj) {
  do.call(IterativeCalculationSettings, args = obj)
}

#' @title Spreadsheet Theme
#' @description Specification of spreadsheet theme
#' @param primaryFontFamily Name of the primary font family.
#' @param TEXT,BACKGROUND,ACCENT1,ACCENT2,ACCENT3,ACCENT4,ACCENT5,ACCENT6,LINK
#' Objects of [ColorStyle()] class with explicit *red*, *green*, *blue*
#' specification for each of the *theme colors*. During update all themes need
#' to be specified.
#' @export
SpreadsheetTheme <- function(
    primaryFontFamily = NULL,
    TEXT = NULL,
    BACKGROUND = NULL,
    ACCENT1 = NULL,
    ACCENT2 = NULL,
    ACCENT3 = NULL,
    ACCENT4 = NULL,
    ACCENT5 = NULL,
    ACCENT6 = NULL,
    LINK = NULL) {

  out <- list() |>
    append_cond(primaryFontFamily, type = "character") |>
    append_cond(TEXT, class = "ColorStyle") |>
    append_cond(BACKGROUND, class = "ColorStyle") |>
    append_cond(ACCENT1, class = "ColorStyle") |>
    append_cond(ACCENT2, class = "ColorStyle") |>
    append_cond(ACCENT3, class = "ColorStyle") |>
    append_cond(ACCENT4, class = "ColorStyle") |>
    append_cond(ACCENT5, class = "ColorStyle") |>
    append_cond(ACCENT6, class = "ColorStyle") |>
    append_cond(LINK, class = "ColorStyle") |>
    deepgs_class("SpreadsheetTheme")

  if (length(out) == 0)
    deepsh_error("No arguments specified",
                 class = "NoArgsError")

  themes_present <- vapply(c("TEXT", "BACKGROUND", "LINK", paste0("ACCENT", 1:6)),
                           \(x) is.null(out[[x]]),
                           logical(1))

  if (!sum(themes_present) %in% c(0, 9))
    deepgs_error("To update {.emph Spreadsheet ColorStyles}, all of them need to be provided",
                 class = "NotAllThemesError")

  themes_rgb <- vapply(c("TEXT", "BACKGROUND", "LINK", paste0("ACCENT", 1:6)),
                       \(x) all(c("red", "blue", "green") %in% names(out[[x]])),
                       logical(1))

  if (!all(themes_rgb))
    deepgs_error("To update {.emph Spreadsheet ColorStyles}, all of them need to be provided",
                 class = "NotRGBAThemesError")

  return(out)

}

#' @title Generate SpreadsheetTheme
#' @noRd
gen_SpreadsheetTheme <- function(obj) {

  args <- list() |>
    append_cond(obj$primaryFontFamily, "primaryFontFamily")

  for (theme in c("TEXT", "BACKGROUND", "LINK", paste0("ACCENT", 1:6))) {

    theme_i <- which(vapply(obj$themeColors,
                            \(themeColor) isTRUE(themeColor$colorType == theme),
                            logical(1)))

    args[[theme]] <- gen_ColorStyle(obj$themeColors[[theme_i]]$color)

  }

  do.call(SpreadsheetTheme, args = args)
}


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
    dataSourceSheetProperties = NULL) {

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
