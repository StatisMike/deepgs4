is_sheetId_in_args <- function(f) {
  "sheetId" %in% names(formals(f))
}

is_specific_gen <- function(gen_name, type = pkg_env$object_types) {

  type <- rlang::arg_match(type)

  switch(type,
         "Obj" = gen_name %in% pkg_env$obj_generators,
         "Resp" = gen_name %in% pkg_env$resp_generators)

}

### dgs4Obj generators ####

#' @title Generate dgs4sObj
#' @description
#' Data flow of the `deepgs4` requires all of the responses from googlesheets
#' API to be converted back to specific class representing this object.
#'
#' Some less complex object can be generated by simply passing their nested
#' objects as arguments to their constructor function. Others need specific
#' pre-process strategy to pass prepared `dgs4Obj` into their constructor
#' calls.
#'
#' `gen_dgs4Obj` is high-level wrapper that can handle this mechanism.
#' Other functions listed there are more specific generators
#'
#' @param obj list generated by [dgs4_listinize()] or list received from
#' googlesheets API
#' @param class name of the class to generate
#' @param sheetId optional sheetId to paste into the constructor. Necessary for
#' some objects returned from googlesheets API
#' @export
#' @aliases @eval pkg_env$obj_generators
#' @return deepgs4Obj of given class

gen_dgs4Obj <- function(obj, class, sheetId = NULL) {

  specific_gen_name <- paste0("gen_", class)

  if (is_specific_gen(specific_gen_name)) {

    if (is_sheetId_in_args(specific_gen_name))
      args <- list(obj = obj, sheetId = sheetId)
    else
      args <- list(obj = obj)

    genned <- do.call(specific_gen_name, args = args)

    return(genned)

  }

  if (is_sheetId_in_args(class) && is.null(obj$sheetId))
    args <- c(obj, list(sheetId = sheetId))
  else
    args <- obj

  genned <- do.call(class, args = args)

  return(genned)

}

##### Obj generators list IMPORTANT! ####
# During addition of new specific deepgs4Obj generator,
# add it to this character vector. It will be used to check
# if for given class there is a specific generator
pkg_env$obj_generators <- c(
  "gen_CellFormat",
  "gen_Border",
  "gen_Borders",
  "gen_ExtendedValue",
  "gen_CellData",
  "gen_RowData",
  "gen_GridData",
  "gen_Sheet",
  "gen_SheetProperties",
  "gen_DataSourceSheetProperties",
  "gen_Spreadsheet",
  "gen_SpreadsheetProperties",
  "gen_SpreadsheetTheme",
  "gen_OverlayPosition",
  "gen_EmbeddedObjectPosition",
  "gen_ChartAxisViewWindowOptions",
  "gen_ChartData",
  "gen_ChartSpec",
  "gen_DataLabel",
  "gen_EmbeddedChart",
  "gen_DataSourceChartProperties",
  "gen_ChartGroupRule",
  "gen_BasicChartAxis",
  "gen_BasicChartDomain",
  "gen_BasicSeriesDataPointStyleOverride",
  "gen_BasicChartSeries",
  "gen_BasicChartSpec",
  "gen_DimensionProperties",
  "gen_DimensionGroup",
  "gen_BooleanCondition",
  "gen_InterpolationPoint",
  "gen_BooleanRule",
  "gen_GradientRule",
  "gen_ConditionalFormatRule",
  "gen_ProtectedRange",
  "gen_SortSpec",
  "gen_FilterCriteria",
  "gen_FilterSpec",
  "gen_BasicFilter",
  "gen_FilterView",
  "gen_DeveloperMetadata",
  "gen_DeveloperMetadataLocation",
  "gen_DeveloperMetadataLookup",
  "gen_MatchedDeveloperMetadata",
  "gen_DataFilter",
  "gen_TextFormat",
  "gen_ColorStyle",
  "gen_TextFormatRun",
  "gen_UpdateValuesResponse",
  "gen_DataSource",
  "gen_DataSourceSpec",
  "gen_BigQueryDataSourceSpec",
  "gen_DataSourceParameter",
  "gen_DataSourceRefreshSchedule",
  "gen_DataSourceColumn"
)

##### Cells.R Generators ####

#' @rdname gen_dgs4Obj
#' @export
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
    args[["textRotation"]] <- "v"

  do.call(CellFormat, args = args)

}

#' @rdname gen_dgs4Obj
#' @export
gen_Border <- function(obj) {

  obj <- obj |>
    try_to_gen_inplace("colorStyle", "ColorStyle")

  do.call(Border, args = obj)

}

#' @rdname gen_dgs4Obj
#' @export
gen_Borders <- function(obj) {

  obj <- obj |>
    try_to_gen_inplace("top", "Border") |>
    try_to_gen_inplace("bottom", "Border") |>
    try_to_gen_inplace("left", "Border") |>
    try_to_gen_inplace("right", "Border")

  do.call(Borders, args = obj)

}

#' @rdname gen_dgs4Obj
#' @export
gen_ExtendedValue <- function(obj) {

  obj$errorValue <- try_to_gen(obj$errorValue, "ErrorValue")

  do.call(ExtendedValue, args = obj)

}

#' @rdname gen_dgs4Obj
#' @export
gen_CellData <- function(obj) {

  if (!is.null(obj$textFormatRuns))
    obj$textFormatRuns <- lapply(obj$textFormatRuns,
                                 try_to_gen,
                                 class = "TextFormatRun")

  obj$userEnteredValue <- try_to_gen(obj$userEnteredValue, "ExtendedValue")
  obj$userEnteredFormat <- try_to_gen(obj$userEnteredFormat, "CellFormat")
  obj$dataValidation <- try_to_gen(obj$dataValidation, "DataValidationRule")
  obj$pivotTable <- try_to_gen(obj$pivotTable, "PivotTable")
  obj$effectiveValue <- try_to_gen(obj$effectiveValue, "ExtendedValue")
  obj$effectiveFormat <- try_to_gen(obj$effectiveFormat, "CellFormat")

  do.call(CellData, args = obj)

}

#' @rdname gen_dgs4Obj
#' @export
gen_RowData <- function(obj) {

  obj <- try_to_gen_inplace(obj, "values", "CellData", TRUE, FALSE)

  do.call(RowData, args = obj)

}

#' @rdname gen_dgs4Obj
#' @export
gen_GridData <- function(obj) {

  obj <- obj |>
    try_to_gen_inplace("rowData", "RowData", TRUE) |>
    try_to_gen_inplace("rowMetadata", "DimensionProperties", TRUE) |>
    try_to_gen_inplace("columnMetadata", "DimensionProperties", TRUE)

  do.call(GridData, args = obj)

}

#### Sheet.R Generators ####

#' @rdname gen_dgs4Obj
#' @export
gen_Sheet <- function(obj) {

  sheetId <- obj$properties$sheetId

  obj <- obj |>
    try_to_gen_inplace("data", "GridData", TRUE) |>
    try_to_gen_inplace("properties", "SheetProperties") |>
    try_to_gen_inplace("charts", "EmbeddedChart", TRUE, sheetId = sheetId) |>
    try_to_gen_inplace("conditionalFormats", "ConditionalFormatRule", TRUE,
                       sheetId = sheetId) |>
    try_to_gen_inplace("protectedRanges", "ProtectedRange", TRUE,
                       sheetId = sheetId) |>
    try_to_gen_inplace("developerMetadata", "DeveloperMetadata", TRUE,
                       sheetId = sheetId) |>
    try_to_gen_inplace("merges", "GridRange", TRUE, sheetId = sheetId) |>
    try_to_gen_inplace("rowGroups", "DimensionGroup", TRUE, sheetId = sheetId) |>
    try_to_gen_inplace("columnGroups", "DimensionGroup", TRUE, sheetId = sheetId)


  do.call(Sheet, args = obj)

}

#' @rdname gen_dgs4Obj
#' @export
gen_SheetProperties <- function(obj) {

  obj$gridProperties <- try_to_gen(obj$gridProperties,
                                   "GridProperties")

  obj$tabColorStyle <- try_to_gen(obj$tabColorStyle,
                                  "ColorStyle")

  obj$dataSourceSheetProperties <- try_to_gen(obj$dataSourceSheetProperties,
                                              "DataSourceSheetProperties")

  do.call(SheetProperties, args = obj)

}

#' @rdname gen_dgs4Obj
#' @export
gen_DataSourceSheetProperties <- function(obj) {

  obj <- obj |>
    try_to_gen_inplace("columns", "DataSourceColumn", TRUE) |>
    try_to_gen_inplace("dataExecutionStatus", "DataExecutionStatus")

  do.call(DataSourceSheetProperties, args = obj)

}

#### Spreadsheet.R Generators ####

#' @rdname gen_dgs4Obj
#' @export
gen_Spreadsheet <- function(obj) {

  obj <- obj |>
    try_to_gen_inplace("sheets", "Sheet", TRUE) |>
    try_to_gen_inplace("namedRanges", "NamedRange", TRUE) |>
    try_to_gen_inplace("developerMetadata", "DeveloperMetadata", TRUE) |>
    try_to_gen_inplace("dataSources", "DataSource", TRUE) |>
    try_to_gen_inplace("dataSourceSchedules", "DataSourceRefreshSchedule", TRUE) |>
    try_to_gen_inplace("properties", "SpreadsheetProperties")

  do.call(Spreadsheet, args = obj)

}

#' @rdname gen_dgs4Obj
#' @export
gen_SpreadsheetProperties <- function(obj) {

  obj$defaultFormat <- try_to_gen(obj$defaultFormat, "CellFormat")
  obj$iterativeCalculationSettings <-
    try_to_gen(obj$iterativeCalculationSettings, "IterativeCalculationSettings")
  obj$spreadsheetTheme <- try_to_gen(obj$spreadsheetTheme,
                                     "SpreadsheetTheme")

  do.call(SpreadsheetProperties, args = obj)
}

#' @rdname gen_dgs4Obj
#' @export
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


#### EmbeddedObject.R Generators ####

#' @rdname gen_dgs4Obj
#' @export
gen_OverlayPosition <- function(obj, sheetId = NULL) {

  obj <- try_to_gen_inplace(
    obj, "anchorCell", "GridCoordinate", sheetId = sheetId, skip_null = FALSE
  )

  do.call(OverlayPosition,
          args = obj)

}

#' @rdname gen_dgs4Obj
#' @export
gen_EmbeddedObjectPosition <- function(obj, sheetId = NULL) {

  obj <- try_to_gen_inplace(
    obj, "overlayPosition", "OverlayPosition", sheetId = sheetId
  )

  do.call(EmbeddedObjectPosition,
          args = obj)

}

#### Chart.R Generators ####

#' @rdname gen_dgs4Obj
#' @export
gen_ChartAxisViewWindowOptions <- function(obj) {

  args <- list() |>
    append_cond(obj$viewWindowMin, "viewWindowMin") |>
    append_cond(obj$viewWindowMax, "viewWindowMax")

  if (obj$viewWindowMode != "VIEW_WINDOW_MODE_UNSUPPORTED")
    args$viewWindowMode <- obj$viewWindowMode

  do.call(ChartAxisViewWindowOptions,
          args = args)
}

#' @rdname gen_dgs4Obj
#' @export
gen_ChartData <- function(obj,
                          sheetId = NULL) {

  if (!is.null(obj$sourceRange)) {

    args <- list(
      sourceRange = lapply(obj$sourceRange$sources, try_to_gen,
                           class = "GridRange",
                           sheetId = sheetId))

    return(do.call(ChartData, args = args))

  }

  args <- list() |>
    append_cond(obj$columnReference, "columnReference") |>
    append_cond(obj$aggregateType, "aggregateType") |>
    append_cond(obj$groupRule, "groupRule")

  do.call(ChartData,
          args = args)
}

#' @rdname gen_dgs4Obj
#' @export
gen_ChartSpec <- function(obj, sheetId = NULL) {

  chart_name <- extract_chart_name(names(obj))

  obj[["chart"]] <- try_to_gen(obj[[chart_name]],
                               class = paste0(first_to_upper(chart_name), "Spec"),
                               sheetId = sheetId)
  obj[[chart_name]] <- NULL

  if (!is.null(obj$titleTextPosition))
    obj[["titleTextPosition"]] <- obj$titleTextPosition$horizontalAlignment
  if (!is.null(obj$subtitleTextPosition))
    obj[["subtitleTextPosition"]] <- obj$subtitleTextPosition$horizontalAlignment

  obj[["titleTextFormat"]] <- try_to_gen(obj[["titleTextFormat"]], "TextFormat")
  obj[["subtitleTextFormat"]] <- try_to_gen(obj[["subtitleTextFormat"]], "TextFormat")
  obj[["backgroundColorStyle"]] <- try_to_gen(obj[["backgroundColorStyle"]], "ColorStyle")
  # obj[["dataSourceChartProperties"]] <- try_to_gen(obj[["dataSourceChartProperties"]], "DataSourceChartProperties")
  obj[["filterSpecs"]] <- try_to_gen(obj[["filterSpecs"]], "FitlerSpecs")
  obj[["sortSpecs"]] <- try_to_gen(obj[["sortSpecs"]], "SortSpecs")

  do.call(ChartSpec, args = obj)

}

#' @rdname gen_dgs4Obj
#' @export
gen_DataLabel <- function(obj, sheetId = NULL) {

  args <- list() |>
    append_cond(obj$type, "type") |>
    append_cond(obj$placement, "placement") |>
    append_cond(try_to_gen(obj$textFormat,
                           class = "TextFormat"),
                "textFormat") |>
    append_cond(try_to_gen(obj$customLabelData,
                           class = "ChartData",
                           sheetId = sheetId),
                "customLabelData")

  do.call(DataLabel,
          args = args)
}

#' @rdname gen_dgs4Obj
#' @export
gen_EmbeddedChart <- function(obj, sheetId = NULL) {

  spec <- try_to_gen(obj$spec, "ChartSpec", sheetId)
  position <- try_to_gen(obj$position, "EmbeddedObjectPosition", sheetId)
  borderColor <- try_to_gen(obj$border$colorStyle, "ColorStyle")

  args <- list() |>
    append_cond(spec) |>
    append_cond(position) |>
    append_cond(borderColor) |>
    append_cond(obj$chartId, "chartId")

  do.call(EmbeddedChart,
          args = args)

}

#' @rdname gen_dgs4Obj
#' @export
gen_DataSourceChartProperties <- function(obj) {

  obj <- try_to_gen_inplace(obj,
                            "dataSourceExecutionStatus",
                            "DataSourceExecutionStatus")

  do.call(DataSourceChartProperties, args = obj)

}

#' @rdname gen_dgs4Obj
#' @export
gen_ChartGroupRule <- function(obj) {

  if (!is.null(obj$dateTimeRule))
    obj$dateTimeRule$type <- obj$dateTimeRule

  obj <- try_to_gen_inplace(
    "histogramRule", "ChartHistogramRule"
  )

  do.call(ChartGroupRule, args = obj)

}

#### BasicCharts.R Generators ####

#' @rdname gen_dgs4Obj
#' @export
gen_BasicChartAxis <- function(obj) {

  args <- list(position = obj$position) |>
    append_cond(obj$title, "title") |>
    append_cond(try_to_gen(obj$format, "TextFormat"), "format") |>
    append_cond(obj$titleTextPosition$horizontalAlignment, "titleTextPosition") |>
    append_cond(try_to_gen(obj$viewWindowOptions, "ChartAxisViewWindowOptions"),
                "viewWindowOptions")

  do.call(BasicChartAxis,
          args = args)
}

#' @rdname gen_dgs4Obj
#' @export
gen_BasicChartDomain <- function(obj,
                                 sheetId = NULL) {

  args <- list(
    domain = gen_ChartData(
      sheetId = sheetId,
      obj = obj$domain)
  ) |>
    append_cond(obj$reversed, "reversed")

  do.call(BasicChartDomain,
          args = args)
}

#' @rdname gen_dgs4Obj
#' @export
gen_BasicSeriesDataPointStyleOverride <- function(obj){

  obj$colorStyle <- try_to_gen(obj$colorStyle, "ColorStyle")
  obj$pointStyle <- try_to_gen(obj$pointStyle, "PointStyle")

  do.call(BasicSeriesDataPointStyleOverride, args = obj)

}

#' @rdname gen_dgs4Obj
#' @export
gen_BasicChartSeries <- function(obj,
                                 sheetId = NULL) {

  styleOverrides <- NULL

  if (!is.null(obj$styleOverrides))
    styleOverrides <- lapply(obj$styleOverrides,
                             gen_BasicSeriesDataPointStyleOverride)

  dataLabel <- try_to_gen(obj$dataLabel, "DataLabel")
  lineStyle <- try_to_gen(obj$lineStyle, "LineStyle")
  pointStyle <- try_to_gen(obj$pointStyle, "PointStyle")
  colorStyle <- try_to_gen(obj$colorStyle, "ColorStyle")

  args <- list(
    series = gen_ChartData(
      sheetId = sheetId,
      obj = obj$series),
    targetAxis = obj$targetAxis) |>
    append_cond(dataLabel) |>
    append_cond(obj$type, "type") |>
    append_cond(lineStyle) |>
    append_cond(colorStyle) |>
    append_cond(pointStyle) |>
    append_cond(styleOverrides)

  do.call(BasicChartSeries,
          args = args)
}

#' @rdname gen_dgs4Obj
#' @export
gen_BasicChartSpec <- function(obj,
                               sheetId = NULL) {

  totalDataLabel <- try_to_gen(obj$totalDataLabel, "DataLabel")

  args <- list(
    axis = lapply(obj$axis, gen_BasicChartAxis),
    domains = lapply(obj$domains, gen_BasicChartDomain, sheetId = sheetId),
    series = lapply(obj$series, gen_BasicChartSeries, sheetId = sheetId)
  ) |>
    append_cond(obj$legendPosition, "legendPosition") |>
    append_cond(obj$chartType, "chartType") |>
    append_cond(obj$headerCount, "headerCount") |>
    append_cond(obj$threeDimensional, "threeDimensional") |>
    append_cond(obj$interpolateNulls, "interpolateNulls") |>
    append_cond(obj$stackedType, "stackedType") |>
    append_cond(obj$lineSmoothing, "lineSmoothing") |>
    append_cond(obj$compareMode, "compareMode") |>
    append_cond(totalDataLabel)

  do.call(BasicChartSpec,
          args = args)
}

#### Dimension.R Generators ####

#' @rdname gen_dgs4Obj
#' @export
gen_DimensionProperties <- function(obj) {

  if (!is.null(obj$dataSourceColumnReference))
    obj$dataSourceColumnReference <- obj$dataSourceColumnReference$name

  do.call(DimensionProperties, args = obj)

}

#' @rdname gen_dgs4Obj
#' @export
gen_DimensionGroup <- function(obj, sheetId = NULL) {

  obj <- obj |>
    try_to_gen_inplace("range", "DimensionRange", sheetId = sheetId)

  do.call(DimensionGroup, args = obj)

}

#### Conditions.R Generators ####

#' @rdname gen_dgs4Obj
#' @export
gen_BooleanCondition <- function(obj) {

  obj <- try_to_gen_inplace(obj, "values", "ConditionValue", use_lapply = T)

  do.call(BooleanCondition,
          args = obj)

}

#### ConditionalFormat.R Generators ####
#' @rdname gen_dgs4Obj
#' @export
gen_InterpolationPoint <- function(obj) {

  obj <- try_to_gen_inplace(obj, "colorStyle", "ColorStyle")

  do.call(InterpolationPoint,
          args = obj)

}

#' @rdname gen_dgs4Obj
#' @export
gen_BooleanRule <- function(obj) {

  obj <- obj |>
    try_to_gen_inplace("condition", "BooleanCondition") |>
    try_to_gen_inplace("format", "CellFormat")

  do.call(BooleanRule,
          args = obj)

}

#' @rdname gen_dgs4Obj
#' @export
gen_GradientRule <- function(obj) {

  obj <- obj |>
    try_to_gen_inplace("minpoint", "InterpolationPoint", skip_null = F) |>
    try_to_gen_inplace("maxpoint", "InterpolationPoint", skip_null = F) |>
    try_to_gen_inplace("midpoint", "InterpolationPoint")

  do.call(GradientRule,
          args = obj)

}

#' @rdname gen_dgs4Obj
#' @export
gen_ConditionalFormatRule <- function(obj, sheetId = NULL) {

  obj <- obj |>
    try_to_gen_inplace("ranges", class = "GridRange",
                       use_lapply = TRUE, sheetId = sheetId) |>
    try_to_gen_inplace("booleanRule", class = "BooleanRule") |>
    try_to_gen_inplace("gradientRule", class = "GradientRule")

  do.call(ConditionalFormatRule,
          args = obj)

}

#### ProtectedRange.R Generators ####

#' @rdname gen_dgs4Obj
#' @export
gen_ProtectedRange <- function(obj, sheetId = NULL) {

  obj <- obj |>
    try_to_gen_inplace("range", "GridRange", sheetId = sheetId) |>
    try_to_gen_inplace("unprotectedRanges", "GridRange", use_lapply = T,
                       sheetId = sheetId) |>
    try_to_gen_inplace("editors", "Editors")

  do.call(ProtectedRange, args = obj)

}

#### Filters.R Generators ####

#' @rdname gen_dgs4Obj
#' @export
gen_SortSpec <- function(obj) {

  obj <- obj |>
    try_to_gen_inplace("foregroundColorStyle", "ColorStyle") |>
    try_to_gen_inplace("backgroundColorStyle", "ColorStyle")

  if (!is.null(obj$dataSourceColumnReference))
    obj$dataSourceColumnReference <- obj$dataSourceColumnReference$name

  do.call(SortSpec, args = obj)

}

#' @rdname gen_dgs4Obj
#' @export
gen_FilterCriteria <- function(obj) {

  obj <- obj |>
    try_to_gen_inplace("condition", "BooleanCondition") |>
    try_to_gen_inplace("visibleBackgroundColorStyle", "ColorStyle") |>
    try_to_gen_inplace("visibleForegroundColorStyle", "ColorStyle")

  do.call(FilterCriteria, args = obj)

}

#' @rdname gen_dgs4Obj
#' @export
gen_FilterSpec <- function(obj) {

  obj <- obj |>
    try_to_gen_inplace("filterCriteria", "FilterCriteria")

  if (!is.null(obj$dataSourceColumnReference))
    obj$dataSourceColumnReference <- obj$dataSourceColumnReference$name

  do.call(FilterSpec, args = obj)

}

#' @rdname gen_dgs4Obj
#' @export
gen_BasicFilter <- function(obj) {

  obj <- obj |>
    try_to_gen_inplace("range", "GridRange") |>
    try_to_gen_inplace("sortSpecs", "SortSpec", TRUE) |>
    try_to_gen_inplace("filterSpecs", "FilterSpec", TRUE)

  do.call(BasicFilter, args = obj)

}

#' @rdname gen_dgs4Obj
#' @export
gen_FilterView <- function(obj, sheetId = NULL) {

  obj <- obj |>
    try_to_gen_inplace("range", "GridRange") |>
    try_to_gen_inplace("sortSpecs", "SortSpec", TRUE) |>
    try_to_gen_inplace("filterSpecs", "FilterSpec", TRUE)

  do.call(BasicFilter, args = obj)

}

#### DeveloperMetadata.R Generators ####
#' @rdname gen_dgs4Obj
#' @export
gen_DeveloperMetadata <- function(obj, sheetId = NULL) {

  obj <- try_to_gen_inplace(obj, "location",
                            "DeveloperMetadataLocation",
                            sheetId = sheetId)

  do.call(DeveloperMetadata, args = obj)

}

#' @rdname gen_dgs4Obj
#' @export
gen_DeveloperMetadataLocation <- function(obj, sheetId = NULL) {

  obj <- try_to_gen_inplace(obj, "dimensionRange", "DimensionRange",
                            sheetId = sheetId)

  do.call(DeveloperMetadataLocation, args = obj)

}

#' @rdname gen_dgs4Obj
#' @export
gen_DeveloperMetadataLookup <- function(obj, sheetId = NULL) {

  obj <- try_to_gen_inplace(obj,
                            "metadataLocation",
                            "DeveloperMetadataLocation",
                            sheetId = sheetId)

  do.call(DeveloperMetadataLookup, args = obj)

}

#' @rdname gen_dgs4Obj
#' @export
gen_DataFilter <- function(obj, sheetId = NULL) {

  obj <- obj |>
    try_to_gen_inplace("developerMetadataLookup",
                       "DeveloperMetadataLookup",
                       sheetId = sheetId) |>
    try_to_gen_inplace("gridRange",
                       "GridRange",
                       sheetId = sheetId)

  do.call(DataFilter, args = obj)

}

#' @rdname gen_dgs4Obj
#' @export
gen_MatchedDeveloperMetadata <- function(obj, sheetId = NULL) {

  obj <- obj |>
    try_to_gen_inplace("developerMetadata", "DeveloperMetadata",
                       sheetId = sheetId) |>
    try_to_gen_inplace("dataFilters", "DataFilter", use_lapply = TRUE,
                       sheetId = sheetId) |>
    dgs4_class("MatchedDeveloperMetadata",
               object_type = "Resp")

  return(obj)

}

#### DataSource.R Generators ####

#' @rdname gen_dgs4Obj
#' @export
gen_DataSource <- function(obj) {

  obj <- obj |>
    try_to_gen_inplace("spec", class = "DataSourceSpec") |>
    try_to_gen_inplace("calculatedColumns", class = "DataSourceColumn",
                       use_lapply = TRUE)

  do.call(DataSource, args = obj)

}

#' @rdname gen_dgs4Obj
#' @export
gen_DataSourceSpec <- function(obj) {

  obj <- obj |>
    try_to_gen_inplace("parameters", class = "DataSourceParameter",
                       use_lapply = TRUE) |>
    try_to_gen_inplace("bigQuery", class = "BigQueryDataSourceSpec")

  do.call(DataSourceSpec, args = obj)

}

#' @rdname gen_dgs4Obj
#' @export
gen_BigQueryDataSourceSpec <- function(obj) {

  obj <- obj |>
    try_to_gen_inplace("querySpec", "BigQueryQuerySpec") |>
    try_to_gen_inplace("tableSpec", "BigQueryTableSpec")

  do.call(BigQueryDataSourceSpec, args = obj)

}

#' @rdname gen_dgs4Obj
#' @export
gen_DataSourceParameter <- function(obj) {

  obj <- obj |>
    try_to_gen_inplace("range", "GridRange")

  do.call(DataSourceParameter, args = obj)

}

#' @rdname gen_dgs4Obj
#' @export
gen_DataSourceRefreshSchedule <- function(obj) {

  if (!is.null(obj$weeklySchedule)) {
    obj$daysOfWeek <- as.character(obj$weeklySchedule$daysOfWeek)
    obj$startTime <- obj$weeklySchedule$startTime
    obj$weeklySchedule <- NULL
  } else if (!is.null(obj$monthlySchedule)) {
    obj$daysOfMonth <- as.numeric(obj$monthlySchedule$daysOfMonth)
    obj$startTime <- obj$monthlySchedule$startTime
    obj$monthlySchedule <- NULL
  } else {
    obj$startTime <- obj$dailySchedule$startTime
    obj$dailySchedule <- NULL
  }

  obj$refreshScope <- NULL

  obj <- obj |>
    try_to_gen_inplace("startTime", "TimeOfDay") |>
    try_to_gen_inplace("nextRun", "Interval")

  do.call(DataSourceRefreshSchedule, args = obj)

}

#' @rdname gen_dgs4Obj
#' @export
gen_DataSourceColumn <- function(obj) {

  obj$reference <- obj$reference$name

  do.call(DataSourceColumn, args = obj)

}

#### Varia.R Generators ####

#' @rdname gen_dgs4Obj
#' @export
gen_TextFormat <- function(obj) {

  obj[["foregroundColorStyle"]] <- try_to_gen(obj$foregroundColorStyle, "ColorStyle")
  obj[["link"]] <- obj$link$uri

  do.call(TextFormat,
          args = obj)

}

#' @rdname gen_dgs4Obj
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

#' @rdname gen_dgs4Obj
#' @export
gen_TextFormatRun <- function(obj) {

  obj <- try_to_gen_inplace(
    obj, "textFormatRuns", "TextFormatRun", use_lapply = T
  )

  do.call(TextFormatRun,
          args = obj)

}

#### Values.R Generators ####

#' @rdname gen_dgs4Obj
#' @export
gen_UpdateValuesResponse <- function(obj) {

  obj <- obj |>
    try_to_gen_inplace("updatedData", "ValueRange")

  do.call(UpdateValuesResponse,
          args = obj)

}

### dgs4Resp Generators ####

#' @title Generate dgs4Resp
#' @description
#' Responses of [request_ss_batchUpdate] returned from Sheets API are coerced
#' into `dgs4Resp` objects, allowing for dropping unnecessary nesting and
#' transforming returned objects into `dgs4Obj` classes.
#'
#' @param obj list generated by [dgs4_listinize()] or list received from
#' googlesheets API
#' @param type name of the response type
#' @param sheetId optional sheetId to paste into the constructor. Necessary for
#' some objects returned from googlesheets API
#' @export
#' @aliases @eval pkg_env$resp_generators
#' @return dgs4Resp object

gen_dgs4Resp <- function(obj, type, sheetId = NULL) {

  specific_gen_name <- paste0("gen_", first_to_upper(type))

  if (!is_specific_gen(specific_gen_name, type = "Resp")) {

    return(dgs4_class(obj, object_type = "Resp"))

  }

  if (is_sheetId_in_args(specific_gen_name))
    args <- list(obj = obj[[type]], sheetId = sheetId)
  else
    args <- list(obj = obj[[type]])

  genned <- do.call(specific_gen_name, args = args)

  return(genned)

}

##### Resp generators list IMPORTANT! ####
# During addition of new specific deepgs4Resp generator,
# add it to this character vector. It will be used to check
# if for given class there is a specific generator

pkg_env$resp_generators <- c(
  "gen_AddChart",
  "gen_AddSheet",
  # "gen_AddNamedRange",
  # "gen_AddFilterView",
  # "gen_DuplicateFilterView",
  "gen_DuplicateSheet",
  # "gen_FindReplace",
  # "gen_UpdateEmbeddedObjectPosition",
  "gen_UpdateConditionalFormatRule",
  "gen_DeleteConditionalFormatRule"
  # "gen_AddProtectedRange",
  # "gen_AddBanding",
  # "gen_CreateDeveloperMetadata",
  # "gen_UpdateDeveloperMetadata",
  # "gen_DeleteDeveloperMetadata",
  # "gen_AddDimensionGroup",
  # "gen_DeleteDimensionGroup",
  # "gen_TrimWhitespace",
  # "gen_DeleteDuplicates",
  # "gen_AddSlicer",
  # "gen_AddDataSource",
  # "gen_UpdateDataSource",
  # "gen_RefreshDataSource"
)

#### SheetRequests.R Generators ####

#' @rdname gen_dgs4Resp
#' @export
gen_AddSheet <- function(obj) {

  obj <- obj |>
    try_to_gen_inplace("properties", "SheetProperties", skip_null = FALSE) |>
    dgs4_class("AddSheet", "Resp")

  return(obj)

}

#' @rdname gen_dgs4Resp
#' @export
gen_DuplicateSheet <- function(obj) {

  obj <- obj |>
    try_to_gen_inplace("properties", "SheetProperties", skip_null = FALSE) |>
    dgs4_class("DuplicateSheet", "Resp")

  return(obj)

}

#### ChartRequests.R Generators ####

#' @rdname gen_dgs4Resp
#' @export
gen_AddChart <- function(obj, sheetId = NULL) {

  obj <- obj |>
    try_to_gen_inplace("chart", "EmbeddedChart", skip_null = FALSE, sheetId = sheetId) |>
    dgs4_class("AddChart", "Resp")

  return(obj)

}

#### ConditionalFormatRequests.R Generators ####

#' @rdname gen_dgs4Resp
#' @export
gen_UpdateConditionalFormatRule <- function(obj, sheetId = NULL) {

  obj <- obj |>
    try_to_gen_inplace("newRule", "ConditionalFormatRule", skip_null = FALSE,
                       sheetId = sheetId) |>
    try_to_gen_inplace("oldRule", "ConditionalFormatRule", sheetId = sheetId) |>
    dgs4_class("UpdateConditionalFormatRule", "Resp")

  return(obj)

}

#' @rdname gen_dgs4Resp
#' @export
gen_DeleteConditionalFormatRule <- function(obj, sheetId = NULL) {

  obj <- obj |>
    try_to_gen_inplace("rule", "ConditionalFormatRule", skip_null = FALSE,
                       sheetId = sheetId) |>
    dgs4_class("DeleteConditionalFormatRule", "Resp")

  return(obj)

}
