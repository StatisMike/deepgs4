#' @title GoogleSheet Soreadsheet
#' @details Object representing complete spreadsheet
#' @param properties object of class [SpreadsheetProperties]
#' @param sheets object of class [SheetProperties] or list of such objects
#' @param namedRanges object of class [NamedRange] or list of such objects
#' @param developerMetadata object of class [DeveloperMetadata] or list of such
#' objects.
#' @param dataSources object of class [DataSource] or list of such objects.
#' Represents external data sources for the spreadsheet
#' @param dataSourceSchedules **READ ONLY** object of class [DataSourceRefreshSchedule]
#' or list of such objects. Represents schedules for refreshment of data sources.
#' @param spreadsheetId **READ ONLY ** ID of the spreadsheet
#' @param spreadsheetUrl **READ ONLY** spreadsheet URL
#' @export

Spreadsheet <- function(
    properties = NULL,
    sheets = NULL,
    namedRanges = NULL,
    developerMetadata = NULL,
    dataSources = NULL,
    dataSourceSchedules = NULL,
    spreadsheetId = NULL,
    spreadsheetUrl = NULL
) {

  if (is.Sheet(sheets))
    sheets <- list(sheets)

  sheets <- check_if_all_class(sheets, "Sheet")

  out <- list() |>
    append_cond(spreadsheetId, type = "character") |>
    append_cond(spreadsheetUrl, type = "character") |>
    append_cond(properties, class = "SpreadsheetProperties") |>
    append_cond(sheets) |>
    dgs4_class("Spreadsheet")

  return(out)

}

#' @rdname Spreadsheet
#' @param x any R object
#' @export
is.Spreadsheet <- function(x) {
  inherits(x, "Spreadsheet")
}

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
    title = NULL,
    locale = NULL,
    timeZone = NULL,
    defaultFormat = NULL,
    iterativeCalculationSettings = NULL,
    spreadsheetTheme = NULL,
    autoRecalc = c("ON_CHANGE", "MINUTE", "HOUR"),
    ...) {

  autoRecalc <- rlang::arg_match(autoRecalc)

  out <- list() |>
    append_cond(title, type = "character") |>
    append_cond(locale, type = "character") |>
    append_cond(autoRecalc) |>
    append_cond(timeZone, class = "character") |>
    append_cond(defaultFormat, class = "CellFormat") |>
    append_cond(iterativeCalculationSettings,
                class = "iterativeCalculationSettings") |>
    append_cond(spreadsheetTheme,
                class = "SpreadsheetTheme") |>
    dgs4_class("SpreadsheetProperties")

  return(out)

}

#' @rdname SpreadsheetProperties
#' @param x any R object
#' @export
is.SpreadsheetProperties <- function(x) {
  inherits(x, "SpreadsheetProperties")
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
    dgs4_class("IterativeCalculationSetting")

  return(out)

}

#' @rdname IterativeCalculationSettings
#' @param x any R object
#' @export
is.IterativeCalculationSettings <- function(x) {
  inherits(x, "IterativeCalculationSettings")
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
    dgs4_class("SpreadsheetTheme")

  if (length(out) == 0)
    dgs4_error("No arguments specified",
               class = "NoArgsError")

  themes_present <- vapply(c("TEXT", "BACKGROUND", "LINK", paste0("ACCENT", 1:6)),
                           \(x) is.null(out[[x]]),
                           logical(1))

  if (!sum(themes_present) %in% c(0, 9))
    dgs4_error("To update {.emph Spreadsheet ColorStyles}, all of them need to be provided",
               class = "NotAllThemesError")

  themes_rgb <- vapply(c("TEXT", "BACKGROUND", "LINK", paste0("ACCENT", 1:6)),
                       \(x) all(c("red", "blue", "green") %in% names(out[[x]])),
                       logical(1))

  if (!all(themes_rgb))
    dgs4_error("To update {.emph Spreadsheet ColorStyles}, all of them need to be provided explicitly (with {.arg red}, {.arg green} and {.arg blue} values)",
               class = "NotRGBAThemesError")

  return(out)

}

#' @rdname SpreadsheetTheme
#' @param x any R object
#' @export
is.SpreadsheetTheme <- function(x) {
  inherits(x, "SpreadsheetTheme")
}
