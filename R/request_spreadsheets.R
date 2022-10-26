#' @title Send Google Sheets *spreadsheets* requests
#' @description These functions access the main Sheet API endpoint:
#' *spreadsheets*. They allow getting [Spreadsheet] object out of Sheets
#' API, creating new spreadsheet on basis of such object or updating
#' objects existing inside gived spreadsheet.
#' @rdname request_spreadsheet
#' @name request_spreadsheet
#' @aliases request_ss_get request_ss_create request_ss_batchUpdate
#' request_ss_getByDataFilter
#' @family Sheet API requests
NULL

#' @rdname request_spreadsheet
#' @section Get:
#' Get [Spreadsheet] object holding specified data about the GoogleSheet spreadsheet.
#' Use `fields` parameter to specify which fields to get in *FieldMask*
#' format.
#'
#' ### Returns
#' [Spreadsheet] object
#' @param spreadsheetId ID of the spreadsheet to get or update
#' @param fields Fields to get in *FieldMask* notation.
#' @param ranges Specified ranges in *A1* notation. See [get_A1_not] for more info.
#' @param includeGridData Should the [GridData] will also be received. By default
#' they will not be returned. Ignored if `fields` are specified
#' @param add_params Named list of additional parameters to include in the request
#' @export
request_ss_get <- function(spreadsheetId, fields = NULL, ranges = NULL,
                           includeGridData = NULL, add_params = list()) {

  params <- list() |>
    append_cond(spreadsheetId, type = "character", skip_null = FALSE) |>
    append_cond(fields, type = "character") |>
    append_cond(ranges, type = "character") |>
    append_cond(includeGridData, type = "logical")

  req <- request_generate(
    endpoint = "sheets.spreadsheets.get",
    params = params
  )

  resp <- request_make(req)

  gargle::response_process(resp)

}

#' @rdname request_spreadsheet
#' @section Create:
#' Creates a spreadsheet, returning the newly created spreadsheet.
#'
#' ### Returns
#' [Spreadsheet] object
#' @param spreadsheet object of class [Spreadsheet]
#' @export
request_ss_create <- function(spreadsheet) {

  spreadsheet <- check_if_class(spreadsheet, "Spreadsheet", skip_null = FALSE) |>
    dgs4_listinize()

  req <- request_generate(
    endpoint = "sheets.spreadsheets.create",
    params = spreadsheet
  )

  resp <- request_make(req)

  gargle::response_process(resp) |>
    gen_Spreadsheet()

}

#' @rdname request_spreadsheet
#' @section batchUpdate:
#' This function sends *batchUpdate* request to googlesheets API
#' based on one or many `dgs4Req` objects.
#' Applies one or more updates to the spreadsheet.
#'
#' Each request is validated before being applied. If **any** request is not
#' valid then the entire *batchUpdate* request will fail and nothing will be applied.
#"
#' Some requests have replies to give you some information about how they are
#' applied. The replies will mirror the requests. For example, if you applied
#' 4 updates and the 3rd one had a reply, then the returned `replies` will be a
#' list containing:
#'
#' - 2 empty `list()` objects
#' - one `dgs4Resp` object
#' - 1 more empty `list()` object
#' replies, the actual reply, and another empty reply, in that order.
#'
#' ### Warning
#'
#' Due to the collaborative nature of spreadsheets, it is not
#' guaranteed that the spreadsheet will reflect exactly your changes after
#' this completes, however it is guaranteed that the updates in the request
#' will be applied together atomically. Your changes may be altered with
#' respect to collaborator changes. If there are no collaborators, the
#' spreadsheet should reflect your changes.
#'
#' ### Returns
#' Named list containing:
#' - `spreadsheetId`: ID of the spreadsheet that the updates were applied to
#' - `replies`: List of `dgs4Resp` objects or empty lists
#' - `updatedSpreadsheet`: Object of class [Spreadsheet] showing the state
#' of spreadsheet after the updates. Only if `includeSpreadsheetInResponse`
#' was set to `TRUE`.
#' @param ... objects of class `dgs4Req`
#' @param includeSpreadsheetInResponse Determines if the update response should
#' include the `updatedSpreadsheet`
#'
#' @param responseRanges One or multiple character string in *A1* notation. For
#' more information see [get_A1()]. Limits the ranges included in the response spreadsheet.
#' @param responseIncludeGridData True if grid data should be returned.
#' Meaningful only if `includeSpreadsheetInResponse` is `TRUE`. This parameter
#' is ignored if a field mask was set in the request.
#' @param .dots the same as `...`, but applied as a list
#' @export
request_ss_batchUpdate <- function(
    spreadsheetId,
    ...,
    includeSpreadsheetInResponse = NULL,
    fields = NULL,
    responseRanges = NULL,
    responseIncludeGridData = NULL,
    .dots = list()
) {

  if (length(.dots) > 0)
    requests <- .dots
  else
    requests <- list(...)

  if (!all(vapply(requests, is.dgs4Req, logical(1))))
    dgs4_error("All objects provided to {.arg ...} or {.arg .dots} argument need to be of {.cls dgs4Req} class",
               class = "WrongReqClass")

  requests <- lapply(requests, dgs4_listinize)
  names(requests) <- NULL

  params <- list() |>
    append_cond(spreadsheetId, type = "character", skip_null = FALSE) |>
    append_cond(requests) |>
    append_cond(includeSpreadsheetInResponse, type = "logical") |>
    append_cond(fields, type = "character") |>
    append_cond(responseRanges) |>
    append_cond(responseIncludeGridData, type = "logical")

  req <- request_generate(
    endpoint = "sheets.spreadsheets.batchUpdate",
    params = params
  )

  resp <- request_make(req)

  gargle::response_process(resp) |>
    dgs4_batchUpdate_process(requests)

}

#' @rdname request_spreadsheet
#' @section GetByDataFilter:
#' Get [Spreadsheet] object holding specified data about the GoogleSheet spreadsheet.
#' This function differs from `request_ss_get()` in that it allows selecting which
#' subsets of spreadsheet data to return by specifying a `dataFilters` parameter.
#' Multiple [DataFilter] objects can be specified. Specifying one or more
#' of them returns the portions of the spreadsheet that intersect ranges
#' matched by any of them.
#'
#' ### Returns
#' [Spreadsheet] object
#' @param dataFilters object of class [DataFilter] or list of such objects.
#' @export
request_ss_getByDataFilter <- function(
    spreadsheetId, dataFilters, fields = NULL,
    includeGridData = NULL, add_params = list()) {

  dataFilters <- nest_if_class(dataFilters, "DataFilter") |>
    check_if_all_class("DataFilter") |>
    dgs4_listinize()

  spreadsheetId <- check_if_type(spreadsheetId, type = "character")

  params <- list() |>
    append_cond(fields, type = "character")

  body <- list() |>
    append_cond(dataFilters, skip_null = FALSE) |>
    append_cond(includeGridData, type = "logical")

  req <- gargle::request_build(
    method = "POST",
    path = paste0("v4/spreadsheets/", spreadsheetId, ":getByDataFilters"),
    params = params,
    body = body,
    token = dgs4_token(),
    key = dgs4_api_key(),
    base_url = "https://sheets.googleapis.com"
  )

  resp <- request_make(req)

  gargle::response_process(resp)

}
