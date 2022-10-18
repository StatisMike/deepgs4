#' @title Check dimensionRange for DeveloperMetadataLocation
#' @noRd
check_dimensionRange_for_DeveloperMetadataLocation <- function(
    dimensionRange,
    call = rlang::caller_call()) {

  if (is.null(dimensionRange))
    return(NULL)

  if (!is.DimensionRange(dimensionRange))
    dgs4_error("{.arg dimensionRange} needs to be a {.cls DimensionRange} object",
               call = call)

  if (any(vapply(c(dimensionRange$startIndex, dimensionRange$endIndex),
                 is.null, logical(1))))
    dgs4_error("Provided {.cls DimensionRange} needs to be bounded on both sides.",
               call = call)

  if (dimensionRange$endIndex - dimensionRange$startIndex != 1)
    dgs4_error("Provided {.cls DimensionRange} needs to span one row or column.")

  return(dimensionRange)

}


#' @title Developer Metadata Location
#' @description Object that represents the location of given [DeveloperMetadata]
#' on the spreadsheet. Only one of `dimensionRange`, `sheetId` or `spreadsheet`
#' arguments can be specified.
#' @param dimensionRange Object of class [DimensionRange]. Represents the row or
#' column when metadata is associated with a dimension. The specified
#' range must represent a single row or column. It cannot be unbounded or
#' span multiple rows or columns.
#' @param sheetId ID of the sheet. Specified if the metadata represents entire sheet.
#' @param spreadsheet if TRUE then metadata represents entire spreadsheet
#' @param locationType **READ ONLY** The type of location this object represents.
#' Can be one of: `"ROW"`, `"COLUMN"`, `"SHEET"` or `"SPREADSHEET"`
#' @export
DeveloperMetadataLocation <- function(
    dimensionRange = NULL,
    sheetId = NULL,
    spreadsheet = NULL,
    locationType = NULL) {

  args_specified <- vapply(list(dimensionRange, sheetId, spreadsheet),
                           is.null,
                           logical(1))

  if (sum(args_specified) != 1)
    dgs4_error("Exactly one of {.arg dimensionRange}, {.arg sheetId} or {.spreadsheet} needs to be specified")

  dimensionRange <- check_dimensionRange_for_DeveloperMetadataLocation(dimensionRange)


  locationType <- check_if_options(locationType, "ROW", "COLUMN", "SHEET", "SPREADSHEET")

  out <- list() |>
    append_cond(dimensionRange, class = "DimensionRange") |>
    append_cond(sheetId, type = "integer") |>
    append_cond(spreadsheet, type = "logical") |>
    append_cond(locationType) |>
    dgs4_class("DeveloperMetadataLocation")

  return(obj)

}

#' @rdname DeveloperMetadataLocation
#' @param x any R object
#' @export
is.DeveloperMetadataLocation <- function(x) {
  inherits(x, "DeveloperMetadataLocation")
}

#' @title Developer Metadata
#' @description
#' Developer metadata associated with a location or object in a spreadsheet.
#' Developer metadata may be used to associate arbitrary data with various parts
#' of a spreadsheet and will remain associated at those locations as they move
#' around and the spreadsheet is edited.
#'
#' If the associated object is deleted its metadata is deleted too.
#' @param metadataId positive, unique integer that identifies this metadata.
#' If not provided during creation, random number will be created
#' @param metadataKey string used for identification. It doesn't have to be unique
#' @param metadataValue Data associated with the metadata's key.
#' @param location object of class [DeveloperMetadataLocation]. Binds a metadata
#' to element in the spreadsheet
#' @param visibility The metadata visibility.
#' @details
#' ## Metadata visibility:
#' - **DOCUMENT**: Document-visible metadata is accessible from any developer
#' project with access to the document.
#' - **PROJECT**: Project-visible metadata is only visible to and accessible
#' by the developer project that created the metadata.
#' @export
DeveloperMetadata <- function(
    location,
    metadataKey,
    metadataId = NULL,
    metadataValue = NULL,
    visibility = c("DOCUMENT", "PROJECT")) {

  visibility <- rlang::arg_match(visibility)

  obj <- list() |>
    append_cond(location, class = "DeveloperMetadataLocation", skip_null = F) |>
    append_cond(metadataKey, type = "character", skip_null = F) |>
    append_cond(metadataId, type = "integer") |>
    append_cond(metadataValue, type = "character") |>
    append_cond(visibility) |>
    dgs4_class("DeveloperMetadata")

  return(obj)

}

#' @rdname DeveloperMetadata
#' @param x any R object
#' @export
is.DeveloperMetadata <- function(x) {
  inherits(x, "DeveloperMetadata")
}

#' @title Developer Metadata Lookup
#' @description
#' Selects DeveloperMetadata that matches all of the specified fields. Used in
#' [request_metadata_search()]. Any field or combination of fields may be specified.
#' @details
#' If only a metadata ID is specified this considers the DeveloperMetadata with
#' that particular unique ID. If a metadata key is specified, this considers
#' all developer metadata with that key. If a key, visibility, and location
#' type are all specified, this considers all developer metadata with that key
#' and visibility that are associated with a location of that type. In general,
#' this selects all [DeveloperMetadata] that matches the intersection of all
#' the specified fields
#' @param locationType The type of location to search for.
#' Can be one of: `"ROW"`, `"COLUMN"`, `"SHEET"` or `"SPREADSHEET"`
#' @param metadataLocation Object of class [DeveloperMetadataLocation]. Search
#' for metadata on specified location.
#' @param locationMatchingStrategy Determines the strategy for metadata search.
#' Can be either `"EXACT_LOCATION"` or `"INTERSECTING_LOCATION"`
#' @param metadataId search for metadata of given unique metadataId
#' @param metadataKey search for metadata with given metadataKey
#' @param metadataValue search for metadata with given metadataValue
#' @param visibility search for metadata with given visibility. May be either
#' `"DOCUMENT"` or `"PROJECT"`
DeveloperMetadataLookup <- function(
    locationType = NULL,
    metadataLocation = NULL,
    locationMatchingStrategy = NULL,
    metadataId = NULL,
    metadataKey = NULL,
    metadataValue = NULL,
    visibility = NULL) {

  locationType <- check_if_options(locationType, "ROW", "COLUMN", "SHEET", "SPREADSHEET")
  locationMatchingStrategy <- check_if_options(locationMatchingStrategy,
                                               "EXACT_LOCATION",
                                               "INTERSECTING_LOCATION")
  visibility <- check_if_options(visibility, "DOCUMENT", "PROJECT")

  obj <- list() |>
    append_cond(locationType) |>
    append_cond(metadataLocation, class = "DeveloperMetadataLocation") |>
    append_cond(locationMatchingStrategy) |>
    append_cond(metadataId, type = "integer") |>
    append_cond(metadataKey, type = "character") |>
    append_cond(metadataValue, type = "character") |>
    append_cond(visibility) |>
    dgs4_class("DeveloperMetadataLookup")

  return(obj)

}

#' @rdname DeveloperMetadataLookup
#' @param x any R object
#' @export
is.DeveloperMetadataLookup <- function(x) {
  inherits(x, "DeveloperMetadataLookup")
}


request_metadata_search <- function() {



}

request_metadata_get <- function() {



}

