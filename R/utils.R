#' @title Remove classes from nested lists
#' @description Removes classes from nested lists with S3 class
#' @noRd

remove_class_recursive <- function(x) {

  if (is.list(x) && !is.null(class(x))) {
    class(x) <- NULL
    for (i in seq_along(x))
      x[[i]] <- remove_class_recursive(x[[i]])
  }

  return(x)

}

#' @title Check if object is a list with elements of given class
#' @param l list to check
#' @param obj class to check
#' @importFrom glue glue
#' @noRd
check_list_of_class <- function(
    l,
    obj = c("axis", "domains", "series")) {

  obj <- match.arg(obj)

  if (!is.list(l))
    stop(glue(
      "Argument provided to '{obj}' need to be of class 'list'."
    ))

  valid_els <- switch(
    obj,
    axis = all(vapply(l, is.BasicChartAxis, logical(1))),
    series = all(vapply(l, is.BasicChartSeries, logical(1))),
    domains = all(vapply(l, is.BasicChartDomain, logical(1))) && length(l) == 1
  )

  if (!valid_els) {
    el_cls <- switch(obj,
                     axis = "BasicChartAxis",
                     series = "BasicChartSeries",
                     domains = "BasicChartDomain")

    stop(glue("All elements in list provided to '{obj}' need to be of class '{el_cls}'."))
  }

  return(l)

}

#' @title Check ChartCellRange validity for chart series
#' @description Needs to be a gridRange object AND either row or column need
#' to be 1 cell range
#' @noRd
check_chartGridRange <- function(gridRange) {
  if (!is.GridRange(gridRange))
    stop("'gridRange' should be a 'GridRange' object")

  if ((gridRange$endRowIndex - gridRange$startRowIndex > 1) &&
      (gridRange$endColumnIndex - gridRange$startColumnIndex > 1))
    stop("Provided 'GridRange' need to be one-row or one-column")

  return(gridRange)
}
