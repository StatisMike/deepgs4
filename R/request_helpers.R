#' @title Generate deepgsheets4 request
#' @param endpoint request endpoint
#' @param params list of params passed to the request
#' @param token token to be included
#' @export
request_generate <- function(
    endpoint,
    params,
    token = dgs4_token()) {

  force(params)
  params <- append_cond(params, params$key, "key") |>
    append_cond(dgs4_api_key(), "key") |>
    append_cond(dgs4_default_api_key(), "key")

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

  gargle::request_retry(x, ..., encode = "json", user_agent = dgs4_user_agent())

}

dgs4_user_agent <- function() {

  httr::user_agent(paste0("deepgs4/", utils::packageVersion("deepgs4"),
                          " ", "gargle/", utils::packageVersion("gargle"),
                          " ", "httr/", utils::packageVersion("httr")))

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

