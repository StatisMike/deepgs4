is.deepgsheetsRequest <- function(x)
  inherits(x, "deepgsheetsRequest")


send_batchUpdate_req <- function(
    spreadsheetId,
    ...,
    .dots = list()
) {

  if (length(.dots) > 0)
    requests <- .dots
  else
    requests <- list(...)

  if (!all(vapply(requests, is.deepgsheetsRequest, logical(1))))
    stop("All objects provided to `...` or `.dots` argument need to be of `deepgsheetsRequest` class")

  requests <- remove_class_recursive(requests)
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

