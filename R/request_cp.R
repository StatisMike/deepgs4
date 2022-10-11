#' @title Requests to paste data into a sheet
#' @description
#' Create `deepgsheets4Req` objects that allow for pasting data in various way
#' into the sheet. Send created requests with [send_batchUpdate_req()]
#' @param source object of class [GridRange] specifying the location in the grid
#' where the source data is located
#' @param destination object of class [GridCoordinate] (for `CutPaste`)
#' or [GridRange] (for `CopyPaste`) specifying the location to paste data
#' @param pasteType type of the paste operation to use. See *details* for more information
#' @param pasteOrientation how that data should be oriented when pasting. See *details*
#' for more information
#' @param coordinate object of class [GridCoordinate] specifying the top left
#' cell of the `PasteData` destination
#' @param type the same as `pasteType` but for `PasteData` request
#' @param data string containing the data to paste for `PasteData` request
#' @param delimiter delimitier if `data` is a character-delimited text
#' @param html boolean - `"TRUE"` if `data` is an *html*
#' @details
#' # pasteType options
#' - **PASTE_NORMAL**: Paste values, formulas, formats, and merges.
#' - **PASTE_VALUES**: Paste the values ONLY without formats, formulas, or merges.
#' - **PASTE_FORMAT**: Paste the format and data validation only.
#' - **PASTE_NO_BORDERS**: Like PASTE_NORMAL but without borders.
#' - **PASTE_FORMULA**: Paste the formulas only.
#' - **PASTE_DATA_VALIDATION**: Paste the data validation only.
#' - **PASTE_CONDITIONAL_FORMATTING**: Paste the conditional formatting rules only.
#'
#' # pasteOrientation options
#' - **NORMAL**: Paste normally.
#' - **TRANSPOSE**: Paste transposed, where all rows become columns and vice versa.
#' @name PasteRequests
#' @rdname PasteRequests
#' @family deepgsheets4Req constructors
#' @aliases CutPasteRequest CopyPasteRequest PasteDataRequest
#' @return deepgsheets4Req object
NULL

#' @rdname PasteRequests
#' @section CutPaste:
#' Moves data from the source to the destination. `destination` specifies the
#' top-left cell of the grid. Regardless of the `pasteType`, all data will be
#' cut from the `source`
#' @export
CutPasteRequest <- function(
    source,
    destination,
    pasteType = c("PASTE_NORMAL", "PASTE_VALUES", "PASTE_FORMAT",
                  "PASTE_NO_BORDERS", "PASTE_FORMULA", "PASTE_DATA_VALIDATION",
                  "PASTE_CONDITIONAL_FORMATTING")) {

  pasteType <- rlang::arg_match(pasteType)

  req <- list() |>
    append_cond(source, class = "GridRange", skip_null = FALSE) |>
    append_cond(destination, class = "GridCoordinate", skip_null = FALSE) |>
    append_cond(pasteType)

  out <- list(cutPaste = req) |>
    deepgs_class(object_type = "Req")

  return(out)

}

#' @rdname PasteRequests
#' @section CopyPaste:
#' Copies data from the `source` to the `destination.` If the `destination` covers
#' a span that's a multiple of the `source`'s height or width, then the data will
#' be repeated to fill in the `destination` range. If the `destination` is smaller
#' than the `source` the entire source data will still be copied (beyond the
#' end of the destination range).
#' @export
CopyPasteRequest <- function(
    source,
    destination,
    pasteType = c("PASTE_NORMAL", "PASTE_VALUES", "PASTE_FORMAT",
                  "PASTE_NO_BORDERS", "PASTE_FORMULA", "PASTE_DATA_VALIDATION",
                  "PASTE_CONDITIONAL_FORMATTING"),
    pasteOrientation = c("NORMAL", "TRANSPOSE")) {

  pasteType <- rlang::arg_match(pasteType)
  pasteOrientation <- rlang::arg_match(pasteOrientation)

  req <- list() |>
    append_cond(source, class = "GridRange", skip_null = FALSE) |>
    append_cond(destination, class = "GridRange", skip_null = FALSE) |>
    append_cond(pasteType) |>
    append_cond(pasteOrientation)

  out <- list(copyPaste = req) |>
    deepgs_class(object_type = "Req")

  return(out)

}

#' @rdname PasteRequests
#' @section PasteData:
#' Inserts data into the spreadsheet starting at the specified coordinate. `data`
#' can be either a delimited text (in which case `delimiter` needs to be specified)
#' or an html source (in which case `html` needs to be specified and `TRUE`)
#' @export
PasteDataRequest <- function(
    coordinate,
    data,
    type = c("PASTE_NORMAL", "PASTE_VALUES", "PASTE_FORMAT",
             "PASTE_NO_BORDERS", "PASTE_FORMULA", "PASTE_DATA_VALIDATION",
             "PASTE_CONDITIONAL_FORMATTING"),
    delimiter = NULL,
    html = NULL) {

  type <- rlang::arg_match(type)

  null_fields <- vapply(list(delimiter, html), is.null, logical(1))

  if (sum(null_fields) != 1)
    deepgs_error("Exactly one of {.arg delimiter} or {.arg html} needs to be provided.")

  req <- list() |>
    append_cond(coordinate, class = "GridCoordinate", skip_null = F) |>
    append_cond(data, type = "character", skip_null = F) |>
    append_cond(type) |>
    append_cond(delimiter, type = "character") |>
    append_cond(html, type = "logical")

  out <- list(pasteData = req) |>
    deepgs_class(object_type = "Req")

  return(out)

}
