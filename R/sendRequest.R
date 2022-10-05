#' @title Generate deepgsheets4 request
#' @param endpoint request endpoint
#' @param params list of params passed to the request
#' @param token token to be included
#' @export
request_generate <- function(
    endpoint = c("sheets.spreadsheets.get", "sheets.spreadsheets.batchUpdate",
                 "sheets.spreadsheets.create", "sheets.spreadsheets.getByDataFilter"),
    params,
    token = deepgs_token()) {

  endpoint <- rlang::arg_match(endpoint)

  force(params)
  params <- append_cond(params, params$key, "key") |>
    append_cond(deepgs_api_key(), "key") |>
    append_cond(deepgs_default_api_key(), "key")

  req <- gargle::request_develop(
    endpoint = .endpoints[[endpoint]],
    params = params,
    base_url = "https://sheets.googleapis.com"
  )

  gargle::request_build(
    method = req$method,
    path = req$path,
    params = req$params,
    body = req$body,
    token = token,
    base_url = req$base_url
  )
}

#' @title Make request to googlesheets API
#' @param x List holding components for HTTP request
#' @param ... Optional arguments passed to the HTTP method
#' @param encode how the body should be encoded
#' @export
request_make <- function(x, ..., encode = "json") {

  gargle::request_retry(x, ..., encode = "json", user_agent = deepgs_user_agent())

}

#' @title Additional response processing
#' @details Responses from googlesheets API detailing the positions in grid
#' sheets are omitting `sheetId` field if referred objects are located
#' on the same sheet. To remedy this problem and construct complete
#' `deepgsheets4Obj`, I apply additional post-processing reapplying
#' the sheetId from the request
#' @param resp whole response
#' @param reqs requests
#' @noRd

deepgs_batchUpdate_process <- function(
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

    return(deepgsheets4Reply_batchUpdate(resp$replies[[n]], sheetId = input_Id))


  })

  return(resp)

}

#' @title Requests that are to be sent to googlesheets API
#' @details All requests that can be sent to googlesheets API by `deepgsheets4`
#' package need to be parsed into object of `deepgsheets4Req`
#' @family deepgsheets4Req
#' @param x any R object
#' @export
is.deepgsheets4Req <- function(x) {
  inherits(x, "deepgsheets4Req")
}

#' @title Send *batchUpdate* request to googlesheets API
#' @description This function sends *batchUpdate* request to googlesheets API
#' based on multiple [deepsheets4Req] objects
#' @param spreadsheetId ID of the spreadsheet to apply the changes
#' @param ... objects of class [deepgsheets4Req]
#' @param .dots the same as `...`, but applied as a list
#' @export
send_batchUpdate_req <- function(
    spreadsheetId,
    ...,
    .dots = list()
) {

  if (length(.dots) > 0)
    requests <- .dots
  else
    requests <- list(...)

  if (!all(vapply(requests, is.deepgsheets4Req, logical(1))))
    deepgs_error("All objects provided to {.arg ...} or {.arg .dots} argument need to be of {.cls deepgsheets4Req} class",
                 class = "WrongReqClass")

  requests <- lapply(requests, deepgs_listinize)
  names(requests) <- NULL

  req <- request_generate(
    endpoint = "sheets.spreadsheets.batchUpdate",
    params = list(
      spreadsheetId = spreadsheetId,
      requests = requests
    )
  )

  resp <- request_make(req)

  gargle::response_process(resp) |>
    deepgs_batchUpdate_process(requests)

}

#' @title Send *create* request to googlesheets API
#' @description This function sends *create* request to googlesheets API
#' to create specified spreadsheet
#' @param spreadsheet object of class [Spreadsheet]
#' @return response containing created object of class [Spreadsheet]
#' @export
send_create_req <- function(
    spreadsheet
) {

  spreadsheet <- check_if_class(spreadsheet, "Spreadsheet", skip_null = FALSE) |>
    deepgs_listinize()

  req <- request_generate(
    endpoint = "sheets.spreadsheets.create",
    params = spreadsheet
  )

  resp <- request_make(req)

  gargle::response_process(resp) |>
    gen_Spreadsheet()

}

#' @title Reply from googlesheets API
#' @description Mostly internal post-processing function, applied after
#' [deepgsheets4 Request sending] to construct complete [deepgsheets4Obj] objects
#' @param reply list containing response from googlesheets4 API
#' @param sheetId optional sheetId to input
#' @noRd
deepgsheets4Reply_batchUpdate <- function(reply, sheetId = NULL) {

  req_type <- names(reply)

  created <- switch(
    req_type,
    addChart = list(
      addChart = list(
        chart = gen_EmbeddedChart(
          reply$addChart$chart,
          sheetId = sheetId))),
    reply
  )

  class(created) <- "deepgsheets4Reply"

  return(created)

}

#' @rdname deepgsheets4Reply
#' @param x any R object
#' @export
is.deepgsheets4Reply <- function(x) {
  inherits(x, "deepgsheets4Reply")
}

deepgs_user_agent <- function() {

  httr::user_agent(paste0("deepgsheets4/", utils::packageVersion("deepgsheets4"),
                          " ", "gargle/", utils::packageVersion("gargle"),
                          " ", "httr/", utils::packageVersion("httr")))

}
