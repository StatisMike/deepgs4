library(glue)

expect_genned_identical <- function(object,
                                    sheetId = NULL,
                                    remove_sheetId = FALSE) {

  act <- quasi_label(rlang::enquo(object), arg = "object")

  constructed_class <- class(act$val)[1]

  listinized <- deepgs_listinize(act$val)

  # mockup object returned from Sheets API (withoud sheetId)
  if (isTRUE(remove_sheetId))
    listinized <- listinized[!grepl(names(listinized), pattern = "^sheetId$")]

  genned <- tryCatch({
    args = list(obj = listinized)
    if (!is.null(sheetId))
      args$sheetId <- sheetId

    args$class <- constructed_class
    do.call(gen_deepgsheets4Obj,
            args = args)
  }, error = function(e) FALSE
  )

  if (isFALSE(genned))
    fail(glue("Listinized {act$lab} couldn't coerce back to {constructed_class}"))

  if (!identical(act$val, genned))
    fail(glue("Genned {act$lab} is not identical to constructed one."))

  succeed()
  return(invisible(act$val))

}
