#' @title Googlesheets Sheet specification
#' @description Specification of singular Sheet of the google spreadsheet
#' @param properties object of class [SheetProperties], describing the
#' properties of the sheet
#' @param data object of class [GridData] or list of such. Data present in the sheet.
#' @param merges object of class [GridRange] or list of such. Describe merged
#' cells.
#' @param conditionalFormats object of class [ConditionalFormatRule] or list of such.
#' Describe conditional format rules applied to the cells in the sheet
#' @param protectedRanges object of class [ProtectedRange] or list of such.
#' Describe protected ranges on a sheet
#' @param charts object of class [EmbeddedChart] or list of such. Charts embedded
#' on a sheet
#' @param developerMetadata object of class [DeveloperMetadata] or list of such,
#' describing metadata on a sheet
#' @param rowGroups,columnGroups objects of class [DimensionGroup] or list
#' of such, describing grouped rows and columns
#' @export

Sheet <- function(
    properties = NULL,
    data = NULL,
    merges = NULL,
    conditionalFormats = NULL,
    # filterViews = NULL,
    protectedRanges = NULL,
    # basicFilter = NULL,
    charts = NULL,
    # bandedRanges = NULL,
    developerMetadata = NULL,
    rowGroups = NULL,
    columnGroups = NULL
    # slicers = NULL
    ) {

  data <- nest_if_class(data, "GridData") |>
    check_if_all_class("GridData")
  merges <- nest_if_class(merges, "GridRange") |>
    check_if_all_class("GridRange")
  conditionalFormats <- nest_if_class(conditionalFormats, "ConditionalFormatRule") |>
    check_if_all_class("ConditionalFormatRule")
  protectedRanges <- nest_if_class(protectedRanges, "ProtectedRange") |>
    check_if_all_class("ProtectedRange")
  charts <- nest_if_class(charts, "EmbeddedChart") |>
    check_if_all_class("EmbeddedChart")
  developerMetadata <-nest_if_class(developerMetadata, "DeveloperMetadata") |>
    check_if_all_class("DeveloperMetadata")
  rowGroups <- nest_if_class(rowGroups, "DimensionGroup") |>
    check_if_all_class("DimensionGroup")
  columnGroups <- nest_if_class(columnGroups, "DimensionGroup") |>
    check_if_all_class("DimensionGroup")

  out <- list() |>
    append_cond(properties, class = "SheetProperties") |>
    append_cond(data) |>
    append_cond(merges) |>
    append_cond(conditionalFormats) |>
    append_cond(protectedRanges) |>
    append_cond(charts) |>
    append_cond(developerMetadata) |>
    append_cond(rowGroups) |>
    append_cond(columnGroups) |>
    dgs4_class("Sheet")

  return(out)

}

#' @rdname Sheet
#' @param x any R object
#' @export
is.Sheet <- function(x) {
  inherits(x, "Sheet")
}

# {
#   "properties": {
#     object (SheetProperties)
#   },
#   "data": [
#     {
#       object (GridData)
#     }
#   ],
#   "merges": [
#     {
#       object (GridRange)
#     }
#   ],
#   "conditionalFormats": [
#     {
#       object (ConditionalFormatRule)
#     }
#   ],
#   "filterViews": [
#     {
#       object (FilterView)
#     }
#   ],
#   "protectedRanges": [
#     {
#       object (ProtectedRange)
#     }
#   ],
#   "basicFilter": {
#     object (BasicFilter)
#   },
#   "charts": [
#     {
#       object (EmbeddedChart)
#     }
#   ],
#   "bandedRanges": [
#     {
#       object (BandedRange)
#     }
#   ],
#   "developerMetadata": [
#     {
#       object (DeveloperMetadata)
#     }
#   ],
#   "rowGroups": [
#     {
#       object (DimensionGroup)
#     }
#   ],
#   "columnGroups": [
#     {
#       object (DimensionGroup)
#     }
#   ],
#   "slicers": [
#     {
#       object (Slicer)
#     }
#   ]
# }

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
#' @param dataSourceSheetProperties **READ ONLY ** object of class
#' [DataSourceSheetProperties].
#' @param ... additional deprecated fields returned from GoogleSheet API
#' @export
#' @return SheetProperties object

SheetProperties <- function(
    sheetId = NULL,
    title = NULL,
    index = NULL,
    sheetType = c("GRID", "OBJECT", "DATA_SOURCE"),
    gridProperties = NULL,
    hidden = NULL,
    tabColorStyle = NULL,
    rightToLeft = NULL,
    dataSourceSheetProperties = NULL,
    ...) {

  sheetType = rlang::arg_match(sheetType)

  out <- list() |>
    append_cond(sheetId, type = "integer") |>
    append_cond(title, type = "character") |>
    append_cond(sheetType) |>
    append_cond(index, type = "integer") |>
    append_cond(gridProperties, class = "GridProperties") |>
    append_cond(hidden, type = "logical") |>
    append_cond(tabColorStyle, class = "ColorStyle") |>
    append_cond(rightToLeft, type = "logical") |>
    append_cond(dataSourceSheetProperties,
                class = "DataSourceSheetProperties") |>
    dgs4_class("SheetProperties")

  return(out)

}

#' @rdname SheetProperties
#' @param x any R object
#' @export
is.SheetProperties <- function(x) {
  inherits(x, "SheetProperties")
}

#' @title DataSourceSheetProperties
#' @param dataSourceId ID of the [DataSource] the [Sheet] is connected to
#' @param columns object of class [DataSourceColumn] or list of such. The columns
#' displayed on the sheet, corresponding to the values in RowData
#' @param dataExecutionStatus object of class [DataExecutionStatus]. The data
#' execution status.
#' @export

DataSourceSheetProperties <- function(
    dataSourceId,
    columns = NULL,
    dataExecutionStatus = NULL) {

  columns <- nest_if_class(columns, "DataSourceColumn") |>
    check_if_all_class("DataSourceColumn")

  obj <- list() |>
    append_cond(dataSourceId, type = "character") |>
    append_cond(columns) |>
    append_cond(dataExecutionStatus, class = "DataExecutionStatus") |>
    dgs4_class("DataSourceSheetProperties")

  return(obj)

}

#' @rdname DataSourceSheetProperties
#' @param x any R object
#' @export
is.DataSourceSheetProperties <- function(x) {
  is.dgs4_class(x, "DataSourceSheetProperties")
}
