#' @title Create metadata request
#' @param developerMetadata object of class [DeveloperMetadata]
#' @export

CreateDeveloperMetadataRequest <- function(
    developerMetadata) {

  req <- list() |>
    append_cond(developerMetadata, class = "DeveloperMetadata")

  obj <- list(createDeveloperMetadata = req) |>
    dgs4_class(object_type = "Req")

  return(obj)

}

# {
#   "developerMetadata": {
#     object (DeveloperMetadata)
#   }
# }
