#' @title MergeCells
#' @description Single MergeCells request
#' @param GridRange `GridRange` object
#' @param mergeType one of MERGE_COLUMNS, MERGE_ROWS or MERGE_ALL
#' @export
#' @return `deepgsheets4Req` object
MergeCellsRequest <- function(
    gridRange,
    mergeType = c("MERGE_COLUMNS", "MERGE_ROWS", "MERGE_ALL")) {

  if (!is.GridRange(gridRange))
    stop("'GridRange' object should be provided to 'gridRange' argument")

  mergeType <- match.arg(mergeType)

  out <- list(
    mergeCells = list(
      range = gridRange,
      mergeType = mergeType
    )
  )

  out <- deepgs_class(out, object_type = "Req")

  return(out)

}
