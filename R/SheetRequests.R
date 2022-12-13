#' @title Requests to add, modify and delete Sheet
#' @description
#' Create `dgs4Req` objects that allow for addition, modification
#' and deletion of singular sheet. Send created requests with [request_ss_batchUpdate()]
#' @param properties object of class [SheetProperties]
#' @param fields which of the properties specified within `properties` should
#' be updated
#' @param sheetId integer representing the sheet to delete
#' @name SheetRequests
#' @rdname SheetRequests
#' @family dgs4Req constructors
#' @aliases AddSheetRequest UpdateSheetPropertiesRequest DeleteSheet
#' @return dgs4Req object
NULL

#' @rdname SheetRequests
#' @section Add:
#' Adds a new sheet. When a sheet is added at a given index,
#' all subsequent sheets' indexes are incremented. All properties are optional.
#' The sheetId field is optional; if one is not set, an ID will be randomly generated.
#' @export

AddSheetRequest <- function(properties) {

  obj <- list() |>
    append_cond(properties, class = "SheetProperties", skip_null = FALSE) |>
    dgs4_class("AddSheet", object_type = "Req")

  return(obj)

}

#' @rdname SheetRequests
#' @section Update:
#' `sheetId` field of the `properties` will be used to identify which sheet
#' properties need to be updated. Only properties specified in `fields` will
#' be updated.
#' @export

UpdateSheetPropertiesRequest <- function(
    properties,
    fields = NULL) {

  fields <- check_valid_update_fields(fields, "UpdateSheetProperties")

  obj <- list()  |>
    append_cond(properties, class = "SheetProperties", skip_null = FALSE) |>
    append_cond(fields) |>
    dgs4_class("UpdateSheetProperties", "Req")

  return(obj)

}

#' @rdname SheetRequests
#' @section Delete:
#' Sheet of the given `sheetId` will be deleted. If the sheet is of `DATA_SOURCE`
#' type, the associated DataSource is also deleted.
#' @export

DeleteSheetRequest <- function(sheetId) {

  obj <- list() |>
    append_cond(sheetId, type = "integer", skip_null = FALSE) |>
    dgs4_class("DeleteSheet", "Req")

  return(obj)

}

#' @rdname SheetRequests
#' @section Duplicate:
#' Duplicates the contents of a sheet.
#' If the source sheet is of DATA_SOURCE type, its backing *DataSource* is also
#' duplicated and associated with the new copy of the sheet. No data execution
#' is triggered, the grid data of this sheet is also copied over but only
#' available after the batch request completes.
#' @param sourceSheetId ID of the sheet to duplicate
#' @param insertSheetIndex The zero-based index where the new sheet should be
#' inserted. The index of all sheets after this are incremented.
#' @param newSheetId If set, the ID of the new sheet. If not set, an ID is chosen.
#' ID must not conflict with any existing sheet ID and it must be non-negative.
#' @param newSheetName The name of the new sheet. It should be unique amongst
#' spreadsheet. If empty, a new name is chosen automatically.
#' @export
DuplicateSheetRequest <- function(
    sourceSheetId,
    insertSheetIndex,
    newSheetId = NULL,
    newSheetName = NULL) {

  obj <- list() |>
    append_cond(sourceSheetId, type = "integer", skip_null = FALSE) |>
    append_cond(insertSheetIndex, type = "integer", skip_null = FALSE) |>
    append_cond(newSheetId, type = "integer") |>
    append_cond(newSheetName, type = "character") |>
    dgs4_class("DuplicateSheet", "Req")

  return(obj)

}

