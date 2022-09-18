is.deepgsheetsRequest <- function(x)
  inherits("deepgsheetsRequest")


send_batchUpdate_req <- function(
    spreadsheetId,
    requests
) {

  if (!all(vapply(requests, is.deepgsheetsRequest, logical(1))))
    stop("All objects provided to `requests` argument need to be of `deepgsheetsRequest` class")

  requests <- remove_class_recursive(requests)

  req <- googlesheets4::request_generate(
    endpoint = "sheets.spreadsheets.batchUpdate",
    params = list(
      spreadsheetId = spreadsheetId,
      requests = list(
        requests
      )
    )
  )

  resp <- googlesheets4::request_make(
    gen_request
  )

  return(resp)

}

