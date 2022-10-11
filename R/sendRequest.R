#' @title Generate deepgsheets4 request
#' @param endpoint request endpoint
#' @param params list of params passed to the request
#' @param token token to be included
#' @export
request_generate <- function(
    endpoint,
    params,
    token = deepgs_token()) {

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

deepgs_user_agent <- function() {

  httr::user_agent(paste0("deepgsheets4/", utils::packageVersion("deepgsheets4"),
                          " ", "gargle/", utils::packageVersion("gargle"),
                          " ", "httr", utils::packageVersion("httr")))

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

#' @title Get data about the spreadsheet
#' @param spreadsheetId ID of the spreadsheet
#' @param fields fields to get
#' @param ranges specified ranges in *A1* notation. See [get_A1_not] for more info.
#' @export

send_get_req <- function(spreadsheetId, fields = NULL, ranges = NULL) {

  params <- list() |>
    append_cond(spreadsheetId, type = "character", skip_null = FALSE) |>
    append_cond(fields, type = "character") |>
    append_cond(ranges, type = "character")

  req <- request_generate(
    endpoint = "sheets.spreadsheets.get",
    params = params
  )

  resp <- request_make(req)

  gargle::response_process(resp) |>
    gen_Spreadsheet()

}
