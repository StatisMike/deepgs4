#' @title Additional response processing
#' @details Responses from googlesheets API detailing the positions in grid
#' sheets are omitting `sheetId` field if referred objects are located
#' on the same sheet. To remedy this problem and construct complete
#' `deepgsheets4Obj`, I apply additional post-processing reapplying
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

    return(dgs4Response(resp$replies[[n]], sheetId = input_Id))


  })

  return(resp)

}

#' @title Send *batchUpdate* request to googlesheets API
#' @description This function sends *batchUpdate* request to googlesheets API
#' based on multiple `deepsheets4Req` objects
#' @param spreadsheetId ID of the spreadsheet to apply the changes
#' @param ... objects of class `deepgsheets4Req`
#' @param .dots the same as `...`, but applied as a list
#' @return list containing `spreadsheetId` and `replies`: list of `deepgsheets4Response`
#' objects
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

  if (!all(vapply(requests, is.dgs4Req, logical(1))))
    dgs4_error("All objects provided to {.arg ...} or {.arg .dots} argument need to be of {.cls deepgsheets4Req} class",
                 class = "WrongReqClass")

  requests <- lapply(requests, dgs4_listinize)
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
    dgs4_batchUpdate_process(requests)

}

#' @title Reply from googlesheets API
#' @description Mostly internal post-processing function, applied after
#' [deepgsheets4 Request sending] to construct complete [deepgsheets4Obj] objects
#' @param reply list containing response from googlesheets4 API
#' @param sheetId optional sheetId to input
#' @noRd
dgs4Response <- function(reply, sheetId = NULL) {

  req_type <- names(reply)

  created <- switch(
    req_type,
    addChart = list(
      addChart = list(
        chart = gen_EmbeddedChart(
          reply$addChart$chart,
          sheetId = sheetId))),
    addSheet = list(
      addSheet = list(
        properties = gen_SheetProperties(
          reply$addSheet$properties
        ))),
    reply
  ) |>
    dgs4_class(object_type = "Response")

  return(created)

}

#' @rdname send_batchUpdate_req
#' @param x any R object
#' @export
is.dgs4Req <- function(x) {
  is.dgs4_class(x, object_type = "Req")
}

#' @rdname send_batchUpdate_req
#' @param x any R object
#' @export
is.dgs4Response <- function(x) {
  is.dgs4_class(x, object_type = "Response")
}

