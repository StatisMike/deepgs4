is.deepgsheets4Req <- function(x)
  inherits(x, "deepgsheets4Req")


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

  req <- googlesheets4::request_generate(
    endpoint = "sheets.spreadsheets.batchUpdate",
    params = list(
      spreadsheetId = spreadsheetId,
      requests = requests
    )
  )

  resp <- googlesheets4::request_make(
    req
  )

  gargle::response_process(resp)

}

