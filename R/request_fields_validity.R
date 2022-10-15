#' @title Check validity of specified fields
#' @param updateRequest which update request to check
#' @param fields character vector in
#' @description All update requests to be sent by [send_batchUpdate_req()] take
#' a `fields` argument in form of [FieldMask](https://developers.google.com/protocol-buffers/docs/reference/google.protobuf#google.protobuf.FieldMask){target="_blank"}
#'
#' *FieldMask* format is helpful for specifying accurately which fields should
#' be updated by BatchUpdate request.
#'
#' This function works as a helper for retrieval of valid update fields per
#' request.
#'
#' @return character vector
#' @export
valid_update_fields <- function(
    updateRequest = c("Cells")) {

  updateRequest <- rlang::arg_match(updateRequest)

  pkg_env$valid_update_fields[[updateRequest]]

}

check_valid_update_fields <- function(
    fields,
    updateRequest = c("Cells"),
    call = rlang::caller_call()) {

  updateRequest <- rlang::arg_match(updateRequest)

  non_valid <- fields[!fields %in% c("*", pkg_env$valid_update_fields[[updateRequest]])]

  if (length(non_valid) == 0) {
    return(fields)
  }

  dgs4_error(class = "NonValidField",
             call = call,
             "Fields: {.val {non_valid}} aren't valid fieds for this request. Check valid fields with `valid_update_fields('{updateRequest}')`.")

}
