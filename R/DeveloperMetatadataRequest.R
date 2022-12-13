#' @title Requests to create, update or delete DeveloperMetadata
#' @description
#' Create `dgs4Req` objects that allow creation, update or deletion
#' of [DeveloperMetadata] in a spreadsheet. Send created requests with
#' [request_ss_batchUpdate()]
#' @name DeveloperMetadataRequests
#' @rdname DeveloperMetadataRequests
#' @aliases CreateDeveloperMetadataRequest AppendCellsRequest
#' @family dgs4Req constructors
#' @return dgs4Req object
NULL


#' @rdname DeveloperMetadataRequests
#' @section CreateDeveloperMetadata:
#' A request to create developer metadata.
#' @param developerMetadata object of class [DeveloperMetadata]
#' @export
CreateDeveloperMetadataRequest <- function(
    developerMetadata) {

  obj <- list() |>
    append_cond(developerMetadata, class = "DeveloperMetadata") |>
    dgs4_class("CreateDeveloperMetadata", "Req")

  return(obj)

}

#' @rdname DeveloperMetadataRequests
#' @section UpdateDeveloperMetadata:
#' A request to update properties of developer metadata. Updates the properties
#' of the developer metadata selected by the filters to the values provided
#' in the [DeveloperMetadata] resource. Callers must specify the properties they
#' wish to update in the `fields` parameter, as well as specify at least one
#' [DataFilter] matching the metadata they wish to update.
#' @param dataFilters object of class [DataFilter] or list of such objects,
#' specifying [DeveloperMetadata] to update
#' @param developerMetadata object of class [DeveloperMetadata] containing
#' properties to update
#' @param fields character vector with fields to update in *FieldMask* format.
#' Check vaild fields using [valid_update_fields()]
#' @export
UpdateDeveloperMetadataRequest <- function(
    dataFilters,
    developerMetadata,
    fields = NULL) {

  fields <- check_valid_update_fields(fields, updateRequest = "UpdateDeveloperMetadata")

  dataFilters <- nest_if_class(dataFilters, "DataFilter") |>
    check_if_all_class("DataFilter")

  obj <- list() |>
    append_cond(fields) |>
    append_cond(developerMetadata, class = "DeveloperMetadata", skip_null = FALSE) |>
    append_cond(dataFilters) |>
    dgs4_class("UpdateDeveloperMetadata", "Req")

  return(obj)

}

#' @rdname DeveloperMetadataRequests
#' @section: DeleteDeveloperMetadata:
#' Delete [DeveloperMetadata] that is matched by using provided `dataFilter`
#' @param dataFilter object of class [DataFilter] specifying [DeveloperMetadata]
#' to remove
#' @export
DeleteDeveloperMetadataRequest <- function(
    dataFilter) {

  obj <- list() |>
    append_cond(dataFilter, class = "DataFilter", skip_null = FALSE) |>
    dgs4_class("DeleteDeveloperMetadata", "Req")

  return(obj)

}
