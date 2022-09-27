#' @title First character to upper case
#' @param x string
#' @noRd
first_to_upper <- function(x) {

  x <- unlist(strsplit(x, split = ""))
  x[1] <- toupper(x[1])
  x <- paste(x, collapse = "")

  return(x)

}

#' @title Add deepgsheets4 class
#' @param x object to add the class
#' @param class name of the child class
#' @param object_type type of the object
#' @noRd
deepgs_class <- function(x,
                          class = NULL,
                          object_type = c("Obj", "Req")) {

  object_type <- rlang::arg_match(object_type)

  class(x) <- c(paste0("deepgsheets4", object_type), class)

  return(x)

}

#' @title Check if x is integer-like (whole number)
#' @param x object to check
#' @noRd
is_integerlike <- function(x)
  x %% 1 == 0

#' @title Check if object if of given class
#' @param x object to check
#' @param class name of the class to check for
#' @noRd
check_if_class <- function(x,
                           class,
                           call = rlang::caller_call(),
                           arg = rlang::caller_arg(x)) {

  if (!inherits(x, what = class))
    deepgs_error(
      message = "Object provided to {.arg {arg}} should be of {.cls {class}} class.",
      .envir = rlang::current_env(),
      call = call,
      class = "WrongClassError"
    )

  return(x)

}

#' @title Check if object if of given class
#' @param x object to check
#' @param type type to check for
#' @noRd
check_if_type <- function(x,
                          type = c("numeric", "integer", "character", "logical"),
                          call = rlang::caller_call(),
                          arg = rlang::caller_arg(x)) {

  valid <- switch(
    type,
    numeric = is.numeric(x),
    integer = is_integerlike(x),
    character = is.character(x),
    logical = is.logical(x))

  if (length(x) != 1 && !valid)
    deepgs_error(
      message = "Object provided to {.arg {arg}} should be singular {.cls {type}} value.",
      .envir = rlang::current_env(),
      call = call,
      class = "WrongTypeError"
    )

  return(x)

}

#' @title Check if all objects in list are of given class
#' @param l list of objects to check
#' @param class name of the class to check for
#' @noRd
check_if_all_class <- function(
    l,
    class,
    call = rlang::caller_call(),
    arg = rlang::caller_arg(l)) {

  if (!all(vapply(l, inherits, what = class, FUN.VALUE = logical(1))))
    deepgs_error(
      message = "All objects in a list provided to {.arg {arg}} should be of {.cls {class}} class.",
      .envir = rlang::current_env(),
      call = call,
      class = "WrongClassError"
    )

  return(l)

}

#' @title Check if value matches one from the options
#' @param x object to check
#' @param ... list of options
#' @param max_length maximum length of x
#' @param custom_message custom error message
#' @noRd
check_if_options <- function(
    x,
    ...,
    max_length = 1,
    skip_null = TRUE,
    call = rlang::caller_call(),
    arg = rlang::caller_arg(x),
    custom_message = NULL) {

  if (skip_null && (is.null(x) || length(x) == 0))
    return(x)

  options <- list(...)

  if (length(x) > max_length)
    deepgs_error(
      "{.arg {arg}} should be of maximum length {.val {max_length}}.",
      class = "WrongArgLength",
      call = call
    )

  x_in_opts <- all(
    vapply(x, \(val)
           any(vapply(options, \(opts) val %in% opts,
                      logical(1))),
           logical(length(x))))

  if (!x_in_opts)
    deepgs_error(
      message = if(is.null(custom_message))
        "Value provided to {.arg {arg}} should be of {.val {options}}."
              else custom_message,
      class = "WrongArgValue",
      call = call
    )

  return(x)

}

#' @title Check series and domains compatibility
#' @param series list of BasicChartSeries
#' @param domains list of one BasicChartDomain
#' @noRd
check_domains_series <- function(
    domains,
    series,
    call = rlang::caller_call()) {

  has_sourceRange <- sapply(c(domains, series),
                            \(x) !is.null(x$chartData$sourceRange))

  if (!(sum(has_sourceRange) == 0 || sum(has_sourceRange) == length(has_sourceRange)))
    deepgs_error("All {.arg domains} and {.arg series} need to be either based on {.emph sourceRange} or {.emph data source}.",
                 call = call,
                 class = "UncompatibleDomainSeries")

  if (sum(has_sourceRange) == 0)
    return(NULL)

  domains_length <- sapply(
    domains,
    \(x) length(x$chartData$sourceRange$sources))

  series_length <- sapply(
    series,
    \(x) length(x$chartData$sourceRange$sources))

  if (!all(domains_length == series_length))
    deepgs_error("All underlying {.cls ChartData} objects need to have the same amount of {.emph SourceRanges}",
                 call = call,
                 class = "UncompatibleDomainSeries")

}

#' @title Append object if not NULL
#' @param l list to append the object
#' @param x object to append
#' @param name name in list `l` to append as. If NULL, then append object
#' to the end of the list `l`.
#' @param class If object needs to be also of class, provide its name
#' @param type If object needs to be of type: `character`, `numeric`, `logical`
#' or `integer-like` (whole number) of length 1.
#' @param nests up to three nests to nest into. Only possible when `name`
#' is not NULL.
#' @param skip_null should NULL `x` be skipped
#' @noRd
append_cond <- function(
    l,
    x,
    name = rlang::caller_arg(x),
    class = NULL,
    type = NULL,
    nests = NULL,
    skip_null = TRUE) {

  if (skip_null && (is.null(x) || length(x) == 0))
    return(l)

  force(name)

  if (!is.null(class))
    x <- check_if_class(x, class, rlang::caller_call() ,rlang::caller_arg(x))

  if (!is.null(type))
    x <- check_if_type(x, type, rlang::caller_call(), rlang::caller_arg(x))

  if (!is.null(name)) {
    l[[name]] <- x
    if (!is.null(nests))
      l <- nest_cond(l, name, nests = nests, skip_null = skip_null)
  }

  else
    l[[length(l) + 1]] <- x

  return(l)

}

#' @title Nests object of list conditionally
#' @param l list to transform
#' @param name name in list
#' @param nests up to three nests to nest into
#' @param skip_null should NULL `x` be skipped
#' @noRd
nest_cond <- function(
    l,
    name,
    nests,
    skip_null = TRUE) {

  if (skip_null && (is.null(l[[name]]) || length(l[[name]]) == 0))
    return(l)

  obj <- l[[name]]
  l[[name]] <- list()

  if (length(nests) == 1) {
    l[[name]][[nests[1]]] <- obj
  }
  if (length(nests) == 2) {
    l[[name]][[nests[1]]][[nests[2]]] <- obj
  }
  if (length(nests) == 3) {
    l[[name]][[nests[1]]][[nests[2]]][nests[3]] <- obj
  }
  return(l)
}

#' @title Try to generate object
#' @param x data source for object
#' @param class character name of the class for which use constructor
#' @noRd
try_to_gen <- function(x, class, sheetProperties = NULL) {

  if (is.null(x) || length(x) == 0)
    return(NULL)

  args <- list(obj = x) |>
    append_cond(sheetProperties)

  do.call(paste("gen", class, sep = "_"),
          args = args)

}

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
check_chartGridRange <- function(gridRange,
                                 arg,
                                 call = rlang::caller_call()) {

  if ((gridRange$endRowIndex - gridRange$startRowIndex > 1) &&
      (gridRange$endColumnIndex - gridRange$startColumnIndex > 1))
    deepgs_error("All {.cls GridRange} objects provided to {.arg arg} need to be one-row or one-column wide.",
                 call = call)

  return(gridRange)
}

#' @title Error message template
#' @inheritParams cli::cli_abort
#' @param class additional class to add beyong `deepgsheets4_error`
#' @noRd
deepgs_error <- function(message,
                         ...,
                         class = NULL,
                         .envir = parent.frame(),
                         call = rlang::caller_call()) {

  cli::cli_abort(message = message,
                 ...,
                 class = c(class, "deepgsheets4_error"),
                 .envir = .envir,
                 call = call)
}
