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
    dgs4_class(object_type = "Resp")

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
is.dgs4Resp <- function(x) {
  is.dgs4_class(x, object_type = "Resp")
}

