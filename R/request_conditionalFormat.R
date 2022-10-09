#' @title Requests to add, modify and delete conditional formatting
#' @description
#' Create `deepgsheets4Req` objects that allow for addition, modification
#' and deletion of conditional formatting from given sheet. Send created
#' requests with [send_batchUpdate_req()]
#' @param index zero-based index identifying the rule
#' @param rule object of class [ConditionalFormatRule] describing the rule
#' for addition or update
#' @param newIndex zero-based index to which the rule should be moved
#' @param sheetId ID of the sheet the rule is located in
#' @name ConditionalFormatRequests
#' @rdname ConditionalFormatRequests
#' @aliases AddConditionalFormatRule UpdateConditionalFormatRule
#' DeleteConditionalFormatRule
#' @family deepgsheets4Req constructors
#' @return deepgsheets4Req object
NULL

#' @rdname ConditionalFormatRequests
#' @section Add:
#' New rule is added at a given index. All subsequent rules indices are incremented.
#' @export

AddConditionalFormatRule <- function(index,
                                     rule) {

  req <- list() |>
    append_cond(index, type = "integer") |>
    append_cond(rule, class = "ConditionalFormatRule", skip_null = FALSE)

  obj <- list(addConditionalFormatRule = req) |>
    deepgs_class(object_type = "Req")

  return(obj)

}

#' @rdname ConditionalFormatRequests
#' @section Update:
#' Rule at a given index can be either replaced, providing the `rule` argument
#' or moved, providing the `newIndex` and `sheetId` arguments.
#' @export

UpdateConditionalFormatRule <- function(
    index,
    rule = NULL,
    newIndex = NULL,
    sheetId = NULL) {

  req <- list() |>
    append_cond(index, type = "integer", skip_null = FALSE)

  if (!is.null(rule)) {

    req <- req |>
      append_cond(rule, class = "ConditionalFormatRule")

  } else if (!is.null(newIndex) && !is.null(sheetId)) {

    req <- req |>
      append_cond(newIndex, type = "integer") |>
      append_cond(sheetId, type = "integer")

  } else
    deepgs_error("Either specify {.arg rule} for rule replacement or {.arg newIndex} and {.arg sheetId} to move given rule.")

  obj <- list(updateConditionalFormatRule = req) |>
    deepgs_class(object_type = "Req")

  return(obj)

}

#' @rdname ConditionalFormatRequests
#' @section Delete:
#' Conditional format rule at given `index` from specified `sheetId` is
#' deleted, and all subsequent rules' indices are decremented.
#' @export

DeleteConditionalFormatRule <- function(index,
                                        sheetId) {

  req <- list() |>
    append_cond(sheetId, type = "integer", skip_null = FALSE)

  obj <- list(deleteConditionalFormatRule = req) |>
    deepgs_class(object_type = "Req")

  return(obj)

}

