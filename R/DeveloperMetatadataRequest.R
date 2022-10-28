#' @title Create metadata request
#' @param developerMetadata object of class [DeveloperMetadata]
#' @export

CreateDeveloperMetadataRequest <- function(
    developerMetadata) {

  obj <- list() |>
    append_cond(developerMetadata, class = "DeveloperMetadata") |>
    dgs4_class("CreateDeveloperMetadata", "Req")

  return(obj)

}
