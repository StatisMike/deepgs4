#' @title Copy spreadsheet sheet to another spreadsheet
#' @description Sends request to the [spreadsheets.sheets.copyTo](https://developers.google.com/sheets/api/reference/rest/v4/spreadsheets/get)
#' method, allowing copying specified sheet from one spreadsheet to another.
#' @param spreadsheetId ID of the source spreadsheet
#' @param sheetId ID of the source sheet
#' @param destinationSpreadsheetId ID of the destination spreadsheet
#' @param add_params Named list of additional parameters for http method
#' @return [SheetProperties] of copied sheet
#' @export
request_ss_sheet_copy <- function(spreadsheetId,
                                  sheetId,
                                  destinationSpreadsheetId,
                                  add_params = list()) {

  browser()

  params <- list() |>
    append_cond(spreadsheetId, type = "character", skip_null = FALSE) |>
    append_cond(sheetId, type = "integer", skip_null = FALSE)

  body <- list() |>
    append_cond(destinationSpreadsheetId, type = "character", skip_null = FALSE)

  req <- gargle::request_build(
    method = "POST",
    path = "v4/spreadsheets/{spreadsheetId}/sheets/{sheetId}:copyTo",
    params = params,
    base_url = "https://sheets.googleapis.com",
    body = body,
    token = dgs4_token(),
    key = dgs4_api_key()
  )

  resp <- do.call(request_make, args = c(list(x = req),
                                         add_params))

  gargle::response_process(resp) |>
    gen_dgs4Obj("SheetProperties")

}
