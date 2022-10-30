#' cli utility for creating bullet point fields list
bulletize_fields <- function(fields, fields_names = NULL) {

  if (!is.null(fields_names)) {
    fields_i <- which(names(fields) %in% fields_names)
    fields <- fields[fields_i]
  }

  fields <- paste0("{.field ", names(fields), "}: {", fields, "}")
  names(fields) <- rep("*", length(fields))

  return(fields)

}
