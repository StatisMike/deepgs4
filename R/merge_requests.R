#' @title MergeCells
#' @description Single MergeCells request
#' @param GridRange `GridRange` object
#' @param mergeType one of MERGE_COLUMNS, MERGE_ROWS or MERGE_ALL
#' @export
#' @return gsheetRequest object
MergeCells <- function(
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

  class(out) <- c("deepgsheetsRequest")

  return(out)

}
