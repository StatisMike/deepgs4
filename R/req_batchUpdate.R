#' @title Send *batchUpdate* request to googlesheets API
#' @description This function sends *batchUpdate* request to googlesheets API
#' based on multiple `deepsheets4Req` objects
#' @param spreadsheetId ID of the spreadsheet to apply the changes
#' @param ... objects of class `deepgsheets4Req`
#' @param .dots the same as `...`, but applied as a list
#' @return list containing `spreadsheetId` and `replies`: list of `deepgsheets4Resp`
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
