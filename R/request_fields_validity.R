#' @title Check validity of specified fields
#' @param updateRequest which update request to check
#' @param fields character vector in
#' @description Many requests send to Sheets API take
#' a `fields` argument in form of [FieldMask](https://developers.google.com/protocol-buffers/docs/reference/google.protobuf#google.protobuf.FieldMask){target="_blank"}
#'
#' *FieldMask* format is helpful for specifying accurately which fields should
#' be updated by batchUpdate request or retrieved with *get* request
#'
#' This function works as a helper for retrieval of valid update fields per
#' request.
#'
#' @return character vector
#' @export
valid_update_fields <- function(
    updateRequest = c("UpdateCells", "UpdateSheetProperties")) {

  updateRequest <- rlang::arg_match(updateRequest)

  pkg_env$valid_update_fields[[updateRequest]]

}

check_valid_update_fields <- function(
    fields,
    updateRequest = c("UpdateCells", "UpdateSheetProperties"),
    call = rlang::caller_call()) {

  updateRequest <- rlang::arg_match(updateRequest)

  if (is.null(fields))
    return("*")

  non_valid <- fields[!fields %in% c("*", pkg_env$valid_update_fields[[updateRequest]])]

  if (length(non_valid) == 0) {
    return(paste(fields, collapse = ","))
  }

  dgs4_error(class = "NonValidField",
             call = call,
             "Fields: {.val {non_valid}} aren't valid fieds for this request. Check valid fields with `valid_update_fields('{updateRequest}')`.")

}
