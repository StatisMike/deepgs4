#' @title Find values in nested list by name
#' @param l list
#' @param name name of the nested object
#' @noRd
get_field_values <- function(l, name) {

  rrapply::rrapply(l,
                   classes = "ANY",
                   condition = function(x, .xname) .xname == name,
                   f = function(x) x,
                   how = "flatten")

}


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
dgs4_class <- function(x,
                       class = NULL,
                       object_type = pkg_env$object_types) {

  object_type <- rlang::arg_match(object_type)

  class(x) <- c(class, paste0(pkg_env$cls_prfx, object_type))

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
                           arg = rlang::caller_arg(x),
                           skip_null = TRUE,
                           dgs4_class = TRUE,
                           object_type = pkg_env$object_types) {

  object_type <- rlang::arg_match(object_type)

  if (skip_null && (is.null(x) || length(x) == 0))
    return(x)

  if (isTRUE(dgs4_class) && !is.dgs4_class(x, class, object_type = object_type))
    dgs4_error(
      message = "Object provided to {.arg {arg}} should be of {.cls {c(class, paste0(pkg_env$cls_prfx, object_type))}} class.",
      .envir = rlang::current_env(),
      call = call,
      class = "WrongClassError"
    )

  if (isFALSE(dgs4_class) && !inherits(x, what = class))
    dgs4_error(
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
    dgs4_error(
      message = "Object provided to {.arg {arg}} should be singular {.cls {type}} value.",
      .envir = rlang::current_env(),
      call = call,
      class = "WrongTypeError"
    )

  return(x)

}

#' @title Nest object if its of given class
#' @param x object to nest
#' @param class name of class to check for
#' @noRd
nest_if_class <- function(x, class, dgs4_class = TRUE, object_type = pkg_env$object_types ) {

  object_type <- rlang::arg_match(object_type)

  if (isTRUE(dgs4_class) && is.dgs4_class(x, class, object_type = object_type))
    return(list(x))

  if (inherits(x, class))
    return(list(x))

  return(x)

}

#' @title Check if all objects in list are of given class
#' @param l list of objects to check
#' @param class name of the class to check for
#' @noRd
check_if_all_class <- function(
    l,
    class = NULL,
    call = rlang::caller_call(),
    arg = rlang::caller_arg(l),
    skip_null = TRUE,
    dgs4_class = TRUE,
    object_type = pkg_env$object_types) {

  if (skip_null && is.null(l))
    return(NULL)

  if (skip_null && all(vapply(l, is.null, logical(1))))
    return(NULL)

  if (isTRUE(dgs4_class) && !all(vapply(l, is.dgs4_class,
                                        class = class,
                                        object_type = object_type,
                                        FUN.VALUE = logical(1))))
  dgs4_error(
    message = "All objects in a list provided to {.arg {arg}} should be of {.cls {c(class, paste0(pkg_env$cls_prfx, object_type))}} class.",
    .envir = rlang::current_env(),
    call = call,
    class = "WrongClassError"
  )

  if (!all(vapply(l, inherits, what = class, FUN.VALUE = logical(1))))
    dgs4_error(
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
    dgs4_error(
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
    dgs4_error(
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
    dgs4_error("All {.arg domains} and {.arg series} need to be either based on {.emph sourceRange} or {.emph data source}.",
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
    dgs4_error("All underlying {.cls ChartData} objects need to have the same amount of {.emph SourceRanges}",
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
    skip_null = TRUE,
    dgs4_class = TRUE,
    object_type = pkg_env$object_types) {

  if (skip_null && (is.null(x) || length(x) == 0))
    return(l)

  force(name)

  if (!is.null(class)) {
    object_type <- rlang::arg_match(object_type)
    x <- check_if_class(x, class, rlang::caller_call() ,rlang::caller_arg(x),
                        skip_null = FALSE, dgs4_class = dgs4_class, object_type = object_type)
  }

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
try_to_gen <- function(x, class, sheetId = NULL) {

  if (is.null(x) || length(x) == 0)
    return(NULL)

  args <- list(obj = x,
               class = class) |>
    append_cond(sheetId)

  do.call(gen_dgs4Obj,
          args = args)

}

#' @title Try to generate objects in list in place
#' @param l list where the object is located
#' @param name name of the field
#' @param class name of the class to try to generate
#' @param use_lapply should the generation occur in `lapply`
#' @param skip_null should it skip NULL field
#' @param sheetId optional sheetId
#' @noRd
try_to_gen_inplace <- function(
    l,
    name,
    class,
    use_lapply = FALSE,
    skip_null = TRUE,
    sheetId = NULL
) {

  if (skip_null && is.null(l[[name]]))
    return(l)

  if (!isTRUE(use_lapply)) {
    l[[name]] <- try_to_gen(l[[name]],
                            class = class,
                            sheetId = sheetId)
    return(l)
  }

  l[[name]] <- lapply(l[[name]],
                      try_to_gen,
                      class = class,
                      sheetId = sheetId)
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
    dgs4_error("All {.cls GridRange} objects provided to {.arg arg} need to be one-row or one-column wide.",
               call = call)

  return(gridRange)
}

#' @title Error message template
#' @inheritParams cli::cli_abort
#' @param class additional class to add beyong `deepgsheets4_error`
#' @noRd
dgs4_error <- function(message,
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

#' @title Check for class
#' @param x R object
#' @param class dgs4 class
#' @param object_type object type
#' @noRd
is.dgs4_class <- function(x, class = NULL, object_type = c("Obj", "Req", "Data", "Response")) {

  object_type <- rlang::arg_match(object_type)

  !any(inherits(x, c(class, paste0(pkg_env$cls_prfx, object_type)), which = TRUE) == 0)

}
