compare_objects <- function(obj1, obj2, skip_compare = c(), pos_name = NULL,
                            na.rm = FALSE) {

  if (!is.null(names(obj1)))
    comparisons <- lapply(names(obj1), \(name) {

      if (name %in% skip_compare)
        return(NULL)

      else if (is.null(obj2[[name]]))
        comparison <- as.logical(NA)

      else if (is.list(obj1[[name]])) {
        comparison <- compare_objects(obj1[[name]], obj2[[name]],
                                      skip_compare = skip_compare,
                                      pos_name = if (is.null(pos_name)) name
                                      else paste(pos_name, name, sep = "."))
        return(comparison)
      } else {
        comparison <- obj1[[name]] == obj2[[name]]
      }
      comparison <- setNames(nm = if (is.null(pos_name)) name
                                 else paste(pos_name, name, sep = "."),
                             object = comparison)
    })
  else {
    comparisons <- lapply(seq_along(obj1), \(i) {

      if (i %in% skip_compare)
        return(NULL)

      if (is.list(obj1[[i]])) {
        comparison <- compare_objects(obj1[[i]], obj2[[i]],
                                      skip_compare = skip_compare,
                                      pos_name = if (is.null(pos_name)) i
                                      else paste(pos_name, i, sep = "."))
        return(comparison)
      } else {
          comparison <- obj1[[i]] == obj2[[i]]
      }
      comparison <- setNames(nm = if (is.null(pos_name)) i
                             else paste(pos_name, i, sep = "."),
                             object = comparison)
    })
  }

  comparisons <- unlist(comparisons)
  if (isTRUE(na.rm))
    return(comparisons[!is.na(comparisons)])

  return(comparisons)

}
