library(glue)

expect_genned_identical <- function(object,
                                    sheetProperties = NULL,
                                    remove_sheetId = FALSE) {

  act <- quasi_label(rlang::enquo(object), arg = "object")

  constructed_class <- class(act$val)[2]

  listinized <- deepgs_listinize(act$val)

  # mockup object returned from Sheets API (withoud sheetId)
  if (isTRUE(remove_sheetId))
    listinized <- listinized[!grepl(names(listinized), pattern = "^sheetId$")]

  genned <- tryCatch({
    args = list(obj = listinized)
    if (!is.null(sheetProperties))
      args$sheetProperties <- sheetProperties
    do.call(paste("gen", constructed_class, sep = "_"),
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
