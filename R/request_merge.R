#' @title Merge cells request
#' @description Single MergeCells request
#' @param gridRange object of class [GridRange] declaring the range in which
#' cells need to be merged
#' @param mergeType one of MERGE_COLUMNS, MERGE_ROWS or MERGE_ALL
#' @export
#' @family deepgsheets4Req
#' @return deepgsheets4Req
MergeCellsRequest <- function(
    gridRange,
    mergeType = c("MERGE_COLUMNS", "MERGE_ROWS", "MERGE_ALL")) {

  gridRange <- check_if_class(gridRange, "GridRange")
  mergeType <- rlang::arg_match(mergeType)

  out <- list(
    mergeCells = list(
      range = gridRange,
      mergeType = mergeType
    )
  )

  out <- deepgs_class(out, object_type = "Req")

  return(out)

}

#' @title Unmerge cells request
#' @description Request unmerging all merged cells in given range
#' @param gridRange object of class [GridRange] declaring the cells to be unmerged
#' @export
#' @family deepgsheets4Req
#' @return deepgsheets4Req
UnmergeCellsRequest <- function(
    gridRange) {

  gridRange <- check_if_class(gridRange, "GridRange")

  out <- list(
    unmergeCells = list(
      range = gridRange
    )
  )

  out <- deepgs_class(out, object_type = "Req")

  return(out)

}
