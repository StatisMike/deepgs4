#' @title Get spreadsheet object from Sheets API
#' @description Sends request to the [spreadsheets.get](https://developers.google.com/sheets/api/reference/rest/v4/spreadsheets/get)
#' method. Returns an instance of [Spreadsheet], holding data inside specified
#' Google Sheet spreadsheet. Other arguments can narrow down the data to get.
#'
#' Both functions allow specifying additional range to get only specific portions
#' of the spreadsheet. With `request_ss_get()` you can specify `ranges` in *A1*
#' notation to get only, while `request_ss_get_byDataFilter()` allows specifying
#' [DataFilter] objects.
#' @param spreadsheetId ID of the spreadsheet
#' @param fields Fields to get in *FieldMask* notation.
#' @param ranges Specified ranges in *A1* notation. See [get_A1_not] for more info.
#' @param includeGridData Should the [GridData] will also be received. By default
#' they will not be returned. Ignored if `fields` are specified.
#' @param add_params Named list of additional parameters to include in the request
#' @return [Spreadsheet]
#' @family Sheets Spreadsheets requests
#' @aliases request_ss_get_byDataFilter
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

  resp <- do.call(request_make, args = c(list(x = req),
                                         add_params))

  gargle::response_process(resp)

}

#' @rdname request_ss_get
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

  resp <- do.call(request_make, args = c(list(x = req),
                                         add_params))

  gargle::response_process(resp)

}

#' @title Create new spreadsheet through Sheets API
#' @description
#' Sends request to the [spreadsheets.create](https://developers.google.com/sheets/api/reference/rest/v4/spreadsheets/create)
#' method. Creates a new spreadsheet on basis of provided [Spreadsheet]. Returns
#' newly created spreadsheet as it present on Google Sheets.
#' @param spreadsheet object of class [Spreadsheet]
#' @param add_params Named list of additional parameters to include in the request
#' @return [Spreadsheet]
#' @family Sheets Spreadsheets requests
#' @export
request_ss_create <- function(spreadsheet, add_params = list()) {

  spreadsheet <- check_if_class(spreadsheet, "Spreadsheet", skip_null = FALSE) |>
    dgs4_listinize()

  req <- request_generate(
    endpoint = "sheets.spreadsheets.create",
    params = spreadsheet
  )

  resp <- do.call(request_make, args = c(list(x = req),
                                         add_params))

  gargle::response_process(resp) |>
    gen_Spreadsheet()

}

#' @title Update spreadsheet through Sheets API
#' @description
#' Sends request to the [spreadsheets.batchUpdate](https://developers.google.com/sheets/api/reference/rest/v4/spreadsheets/batchUpdate)
#' method. Allows modifying existing spreadsheet using one or more `dgs4Req`
#' objects.
#' @param ... objects of class `dgs4Req`
#' @param includeSpreadsheetInResponse Determines if the update response should
#' include the `updatedSpreadsheet`
#' @param responseRanges One or multiple character string in *A1* notation. For
#' more information see [get_A1()]. Limits the ranges included in the response spreadsheet.
#' @param responseIncludeGridData True if grid data should be returned.
#' Meaningful only if `includeSpreadsheetInResponse` is `TRUE`. This parameter
#' is ignored if a field mask was set in the request.
#' @param .dots the same as `...`, but applied as a list
#' @param add_params Named list of additional parameters to include in the request
#' @details
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
#' @family Sheets Spreadsheets requests
#' @return
#' Named list containing:
#' - `spreadsheetId`: ID of the spreadsheet that the updates were applied to
#' - `replies`: List of `dgs4Resp` objects or empty lists
#' - `updatedSpreadsheet`: Object of class [Spreadsheet] showing the state
#' of spreadsheet after the updates. Only if `includeSpreadsheetInResponse`
#' was set to `TRUE`.
#' @export
request_ss_batchUpdate <- function(
    spreadsheetId,
    ...,
    includeSpreadsheetInResponse = NULL,
    fields = NULL,
    responseRanges = NULL,
    responseIncludeGridData = NULL,
    add_params = list(),
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

  resp <- do.call(request_make, args = c(list(x = req),
                                         add_params))

  gargle::response_process(resp) |>
    dgs4_batchUpdate_process(requests)

}

#' @title Additional response processing
#' @details Responses from googlesheets API detailing the positions in grid
#' sheets are omitting `sheetId` field if referred objects are located
#' on the same sheet. To remedy this problem and construct complete
#' `dgs4Obj`, I apply additional post-processing reapplying
#' the sheetId from the request
#' @param resp whole response
#' @param reqs requests
#' @seealso dgs4_matchedmetadata_process
#' @noRd
dgs4_batchUpdate_process <- function(
    resp,
    reqs) {

  resp$replies <- lapply(seq_along(resp$replies), \(n) {

    if (length(resp$replies[[n]]) == 0)
      return(NULL)

    req_sheetIds <- unique(get_field_values(reqs[[n]], "sheetId"))
    resp_sheetIds <- unique(get_field_values(resp$replies[[n]], "sheetId"))

    input_Id <- req_sheetIds[!req_sheetIds %in% resp_sheetIds]
    if (length(input_Id) > 1)
      stop("More IDS than 1!")

    return(dgs4Resp_batchUpdate(resp$replies[[n]], sheetId = input_Id))


  })

  return(resp)

}

#' @title Reply from googlesheets API
#' @description Mostly internal post-processing function, applied after
#' [deepgsheets4 Request sending] to construct complete [deepgsheets4Obj] objects
#' @param reply list containing response from googlesheets4 API
#' @param sheetId optional sheetId to input
#' @noRd
dgs4Resp_batchUpdate <- function(reply, sheetId = NULL) {

  req_type <- names(reply)

  created <- gen_dgs4Resp(reply, req_type, sheetId = sheetId)

  # created <- switch(
  #   req_type,
  #   addChart = list(
  #     addChart = list(
  #       chart = gen_EmbeddedChart(
  #         reply$addChart$chart,
  #         sheetId = sheetId))),
  #   addSheet = list(
  #     addSheet = list(
  #       properties = gen_SheetProperties(
  #         reply$addSheet$properties
  #       ))),
  #   reply
  # ) |>
  #   dgs4_class(object_type = "Resp")

  return(created)

}


