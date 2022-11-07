#' @title Get values in a given range from spreadsheet
#' @param spreadsheetId ID of the spreadsheet
#' @param range specified range in *A1* or *R1C1* notation.
#' See [get_A1_not] for more info.
#' @param majorDimension The major dimension that results should use.
#' @param valueRendeOption How values should be represented in the output.
#' @param dateTimeRenderOption How values should be represented in the output.
#' @param add_params named list of additional params to include in HTML request
#' @description
#' Sends request to the [spreadsheets.values.get](https://developers.google.com/sheets/api/reference/rest/v4/spreadsheets.values/get)
#' method.
#'
#' @details
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
#' be output as doubles in "serial number" format. For more info, see [dgs4_serial_number()]
#' - **FORMATTED_STRING**: Instructs date, time, datetime, and duration fields
#' to be output as strings in their given number format (which depends
#' on the spreadsheet locale).
#'
#' @family Sheets Values requests
#' @return [ValueRange] object
#' @export

request_ss_get_values <- function(
    spreadsheetId,
    range,
    majorDimension = c("ROWS", "COLUMNS"),
    valueRenderOption = c("FORMATTED_VALUE", "UNFORMATTED_VALUE", "FORMULA"),
    dateTimeRenderOption = c("SERIAL_NUMBER", "FORMATTED_STRING"),
    add_params = list()) {

  majorDimension <- rlang::arg_match(majorDimension)
  valueRenderOption <- rlang::arg_match(valueRenderOption)
  dateTimeRenderOption <- rlang::arg_match(dateTimeRenderOption)

  params <- list(
    spreadsheetId = spreadsheetId,
    range = URLencode(range)
  ) |>
    append_cond(majorDimension) |>
    append_cond(valueRenderOption) |>
    append_cond(dateTimeRenderOption)

  req <- gargle::request_build(
    path = paste0("v4/spreadsheets/{spreadsheetId}/values/{range}"),
    params = params,
    token = dgs4_token(),
    key = dgs4_api_key(),
    base_url = "https://sheets.googleapis.com"
  )

  resp <- do.call(request_make, args = c(list(x = req),
                                         add_params))
  gargle::response_process(resp) |>
    gen_dgs4Obj("ValueRange")

}

#' @title Clear values in a given range from sheet
#' @description Send a request to clear values from cells at given range.
#' Sends request to [spreadsheets.values.clear](https://developers.google.com/sheets/api/reference/rest/v4/spreadsheets.values/clear)
#' method.
#' @param spreadsheetId ID of the spreadsheet
#' @param range specified range in *A1* or *R1C1* notation.
#' See [get_A1_not()] for more info.
#' @family Sheets Values requests
#' @return a list containing `spreadsheetId` and `clearedRange`
#' @export

request_ss_clear_values <- function(
    spreadsheetId,
    range) {

  req <- gargle::request_build(
    method = "POST",
    path = paste0("v4/spreadsheets/", spreadsheetId, "/values/", URLencode(range), ":clear"),
    params = params,
    token = dgs4_token(),
    key = dgs4_api_key(),
    base_url = "https://sheets.googleapis.com"
  )

  resp <- request_make(req)

  gargle::response_process(resp)

}

#' @title Update values in a given range in a sheet
#' @description Send a request to update given `range` of cells with values
#' provided within provided `values`. Communicates with
#' [spreadsheets.values.update](https://developers.google.com/sheets/api/reference/rest/v4/spreadsheets.values/update) method.
#' @param spreadsheetId ID of the spreadsheet
#' @param range specified range in *A1* or *R1C1* notation.
#' See [get_A1_not()] for more info.
#' @param values object of class [ValueRange] containing values to update
#' @param valueInputOption how the input data should be interpreted
#' @param includeValuesInResponse if `TRUE`, the updated values will be received
#' in response. If the range to write was larger than the range actually
#' written, the response includes all values in the requested range (
#' excluding trailing empty rows and columns)
#' @param responseValueRenderOption How the values in the response should be
#' rendered
#' @param responseDateTimeRenderOption How dates, times and durations in the
#' response should be rendered
#' @details
#' ## valueInputOption
#' - **RAW**: The values the user has entered will not be parsed and will be
#' stored as-is.
#' - **USER_ENTERED**: The values will be parsed as if the user typed them into
#' the UI. Numbers will stay as numbers, but strings may be converted to
#' numbers, dates, etc. following the same rules that are applied when entering
#' text into a cell via the Google Sheets UI.
#'
#' ## responseValueRenderOption
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
#' ## responseDateTimeRenderOption
#' - **SERIAL_NUMBER**: Instructs date, time, datetime, and duration fields to
#' be output as doubles in "serial number" format. For more info, see [dgs4_serial_number()]
#' - **FORMATTED_STRING**: Instructs date, time, datetime, and duration fields
#' to be output as strings in their given number format (which depends
#' on the spreadsheet locale).
#' @family Sheets Values requests
#' @returns [UpdateValuesResponse] object
#' @export
request_ss_update_values <- function(
    spreadsheetId,
    range,
    values,
    valueInputOption = c("RAW", "USER_ENTERED"),
    includeValuesInResponse = FALSE,
    responseValueRenderOption = c("FORMATTED_VALUE", "UNFORMATTED_VALUE", "FORMULA"),
    dateTimeRenderOption = c("SERIAL_NUMBER", "FORMATTED_STRING")) {

  values <- check_if_class(values, class = "ValueRange")

  valueInputOption <- rlang::arg_match(valueInputOption)
  includeValuesInResponse <- check_if_type(includeValuesInResponse, type = "logical")
  responseValueRenderOption <- rlang::arg_match(responseValueRenderOption)
  dateTimeRenderOption <- rlang::arg_match(dateTimeRenderOption)

  params <- list(valueInputOption = valueInputOption)

  if (isTRUE(includeValuesInResponse)) {
    params <- params |>
      append_cond(includeValuesInResponse) |>
      append_cond(responseValueRenderOption) |>
      append_cond(dateTimeRenderOption)
  }

  req <- gargle::request_build(
    method = "PUT",
    path = paste0("v4/spreadsheets/", spreadsheetId, "/values/", URLencode(range)),
    params = params,
    body = dgs4_listinize(values),
    token = dgs4_token(),
    key = dgs4_api_key(),
    base_url = "https://sheets.googleapis.com"
  )

  resp <- request_make(req)

  gargle::response_process(resp) |>
    gen_UpdateValuesResponse()

}

#' @title Append values in a given range in a sheet
#' @description Send a request to append values to a sheet. The input range is
#' used to search for existing data and find a "table" within that range. Values
#' will be appended to the next row of the table, starting with the first column
#' of the table.
#'
#' Communicates with [spreadsheets.values.append](https://developers.google.com/sheets/api/reference/rest/v4/spreadsheets.values/append)
#' method.
#' @param spreadsheetId ID of the spreadsheet
#' @param range specified range in *A1* or *R1C1* notation.
#' See [get_A1_not()] for more info.
#' @param values object of class [ValueRange] containing values to update
#' @param valueInputOption how the input data should be interpreted
#' @param insertDataOption How the input data should be inserted.
#' @param includeValuesInResponse if `TRUE`, the updated values will be received
#' in response.
#' @param responseValueRenderOption How the values in the response should be
#' rendered
#' @param responseDateTimeRenderOption How dates, times and durations in the
#' response should be rendered
#' @section valueInputOption options:
#' - **RAW**: The values the user has entered will not be parsed and will be
#' stored as-is.
#' - **USER_ENTERED**: The values will be parsed as if the user typed them into
#' the UI. Numbers will stay as numbers, but strings may be converted to
#' numbers, dates, etc. following the same rules that are applied when entering
#' text into a cell via the Google Sheets UI.
#'
#' @section insertDataOption options:
#' - **OVERWRITE**: The new data overwrites existing data in the areas it is written.
#' (Note: adding data to the end of the sheet will still insert new rows or
#' columns so the data can be written.)
#' - **INSERT_ROWS**: Rows are inserted for the new data.
#'
#' @section responseValueRenderOption options:
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
#' @section responseDateTimeRenderOption options:
#' - **SERIAL_NUMBER**: Instructs date, time, datetime, and duration fields to
#' be output as doubles in "serial number" format. For more info, see [dgs4_serial_number()]
#' - **FORMATTED_STRING**: Instructs date, time, datetime, and duration fields
#' to be output as strings in their given number format (which depends
#' on the spreadsheet locale).
#' @family Sheets Values requests
#' @returns [UpdateValuesResponse] object
#' @export
request_ss_append_values <- function(
    spreadsheetId,
    range,
    values,
    valueInputOption = c("RAW", "USER_ENTERED"),
    insertDataOption = c("INSERT_ROWS", "OVERWRITE"),
    includeValuesInResponse = FALSE,
    responseValueRenderOption = c("FORMATTED_VALUE", "UNFORMATTED_VALUE", "FORMULA"),
    responseDateTimeRenderOption = c("SERIAL_NUMBER", "FORMATTED_STRING")) {

  values <- check_if_class(values, class = "ValueRange")

  valueInputOption <- rlang::arg_match(valueInputOption)
  insertDataOption <- rlang::arg_match(insertDataOption)
  includeValuesInResponse <- check_if_type(includeValuesInResponse, type = "logical")
  responseValueRenderOption <- rlang::arg_match(responseValueRenderOption)
  responseDateTimeRenderOption <- rlang::arg_match(responseDateTimeRenderOption)

  params <- list(valueInputOption = valueInputOption,
                 insertDataOption = insertDataOption)

  if (isTRUE(includeValuesInResponse)) {
    params <- params |>
      append_cond(includeValuesInResponse) |>
      append_cond(responseValueRenderOption) |>
      append_cond(responseDateTimeRenderOption)
  }

  req <- gargle::request_build(
    method = "POST",
    path = paste0("v4/spreadsheets/", spreadsheetId, "/values/", URLencode(range), ":append"),
    params = params,
    body = dgs4_listinize(values),
    token = dgs4_token(),
    key = dgs4_api_key(),
    base_url = "https://sheets.googleapis.com"
  )

  resp <- request_make(req)

  gargle::response_process(resp) |>
    try_to_gen_inplace("updates", "UpdateValuesResponse")

}
