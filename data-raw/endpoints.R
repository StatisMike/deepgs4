library(tidyverse)

source(
  system.file("discovery-doc-ingest", "ingest-functions.R", package = "gargle")
)

existing <- list_discovery_documents("sheets")
if (length(existing) > 1) {
  rlang::warn("MULTIPLE DISCOVERY DOCUMENTS FOUND. FIX THIS!")
}

if (length(existing) < 1) {
  rlang::inform("Downloading Discovery Document")
  x <- download_discovery_document("sheets:v4")
} else {
  msg <- glue::glue("
    Using existing Discovery Document:
      * {existing}
    ")
  rlang::inform(msg)
  x <- here::here("data-raw", existing)
}

dd <- read_discovery_document(x)

methods <- get_raw_methods(dd)

methods <- methods %>% map(groom_properties,  dd)
methods <- methods %>% map(add_schema_params, dd)
methods <- methods %>% map(add_global_params, dd)

.endpoints <- methods
attr(.endpoints, "base_url") <- dd$baseUrl
# View(.endpoints)

usethis::use_data(.endpoints, internal = TRUE, overwrite = TRUE)

