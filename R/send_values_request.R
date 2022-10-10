#' @title Get values in a given range from spreadsheet
#' @param spreadsheetId ID of the spreadsheet
#' @param range specified range in *A1* or *R1C1* notation.
#' See [get_A1_not] for more info.
#' @param majorDimension The major dimension that results should use.
#' @param valueRendeOption How values should be represented in the output.
#' @param dateTimeRenderOption How values should be represented in the output.
#' @description
#' Request to the [spreadsheets.values.get](https://developers.google.com/sheets/api/reference/rest/v4/spreadsheets.values/get){target="_blank"}
#' method.
#'
#' ## majorDimension
#' If the spreadsheet data on Sheet1 are:
#'
#' |    | A  | B  |
#' | -- | -- | -- |
#' | 1  | 1  | 2  |
#' | 2  | 3  | 4  |
#'
#' Then `majorDimension = "ROWS"` returns values in form of:
#' `list(list(1, 2), list(3, 4))` and `majorDimension = "COLUMNS"`:
#' `list(list(1, 3), list(2, 4))`.
#'
#' ## valueRenderOption
#' - **FORMATTED_VALUE**: Values will be calculated & formatted in the reply
#' according to the cell's formatting. Formatting is based on the
#' spreadsheet's locale, not the requesting user's locale. For example,
#' if A1 is 1.23 and A2 is =A1 and formatted as currency,
#' then A2 would return "$1.23".
#' - **UNFORMATTED_VALUE**: Values will be calculated, but not formatted in the
#' reply. For example, if A1 is 1.23 and A2 is =A1 and formatted as currency,
#' then A2 would return the number 1.23.
#' - **FORMULA**: Values will not be calculated. The reply will include the
#' formulas. For example, if A1 is 1.23 and A2 is =A1 and formatted as
#' currency, then A2 would return "=A1".
#'
#' ## dateTimeRenderOption
#' - **SERIAL_NUMBER**: Instructs date, time, datetime, and duration fields to
#' be output as doubles in "serial number" format. For more info, see [deepgs_serial_number()]
#' - **FORMATTED_STRING**: Instructs date, time, datetime, and duration fields
#' to be output as strings in their given number format (which depends
#' on the spreadsheet locale).
#'
#' @family Sheets Values requests
#' @return ValueRange object
#' @export

send_get_values_req <- function(
    spreadsheetId,
    range,
    majorDimension = c("ROWS", "COLUMNS"),
    valueRenderOption = c("FORMATTED_VALUE", "UNFORMATTED_VALUE", "FORMULA"),
    dateTimeRenderOption = c("SERIAL_NUMBER", "FORMATTED_STRING")) {

  majorDimension <- rlang::arg_match(majorDimension)
  valueRenderOption <- rlang::arg_match(valueRenderOption)
  dateTimeRenderOption <- rlang::arg_match(dateTimeRenderOption)

  params <- list() |>
    append_cond(majorDimension) |>
    append_cond(valueRenderOption) |>
    append_cond(dateTimeRenderOption)


  req <- gargle::request_build(
    path = paste0("v4/spreadsheets/", spreadsheetId, "/values/", range, ":clear"),
    params = params,
    token = deepgs_token(),
    key = deepgs_api_key(),
    base_url = "https://sheets.googleapis.com"
  )

  resp <- request_make(req)

  gargle::response_process(resp) |>
    gen_deepgsheets4Obj("ValueRange")

}

#' @title Clear values in a given range from spreadsheet
#' @description Send a request to clear values from cells at given range
#' @param spreadsheetId ID of the spreadsheet
#' @param range specified range in *A1* or *R1C1* notation.
#' See [get_A1_not] for more info.
#' @family Sheets Values requests
#' @return a list containing `spreadsheetId` and `clearedRange`
#' @export

send_clear_values_req <- function(
    spreadsheetId,
    range) {

  req <- gargle::request_build(
    path = paste0("v4/spreadsheets/", spreadsheetId, "/values/", range),
    params = params,
    token = deepgs_token(),
    key = deepgs_api_key(),
    base_url = "https://sheets.googleapis.com"
  )

  resp <- request_make(req)

  gargle::response_process(resp)

}



