objects_with_sheetId <- c("GridRange")

construct_sheet_object_from_resp <- function(resp, sheetProperties) {

  data <- lapply(seq_along(resp), \(i) {

    browser()

    obj <- resp[[i]]
    obj_name <- names(resp)[i]

    if (is.list(obj) && exists(obj_name, where = "package:deepgsheets4", mode = "function")) {

      if (obj_name %in% objects_with_sheetId)
        obj <- c(obj, sheetProperties$sheetId)

      return(do.call(obj_name, args = args))

    }

    return(obj)

  })

  return(data)
}


#' @title Get data about the spreadsheet
#' @param spreadsheetId ID of the spreadsheet
#' @param fields fields to get
#' @noRd

getSpreadsheetData <- function(spreadsheetId, fields = NULL) {

  req <- googlesheets4::request_generate(
    endpoint = "sheets.spreadsheets.get",
    params = list(
      spreadsheetId = spreadsheetId,
      fields = fields
    )
  )

  resp <- googlesheets4::request_make(
    req
  )

  rjson::fromJSON(rawToChar(resp$content))

}

#' @title SpreadSheetData
#' @docType class
#' @description Object holding and querying for data regarding specific
#' spreadsheet
#' @export

SpreadSheetData <- R6::R6Class(
  classname = "SpreadSheetData",

  public = list(

    #' @field spreadsheetProperties object of class spreadSheetProperties.
    #' Acquired during object initialization
    spreadsheetProperties = NULL,

    #' @field sheetProperties objects of class `sheetProperties`. Acquired during
    #' object initialization
    sheetProperties = NULL,

    #' @field namedRanges objects of class `namedRange`.
    namedRanges = NULL,

    #' @field merges objects of class `GridRange` describing the merged cells
    merges = NULL,

    #' @field filterViews objects filtered views
    filterViews = NULL,

    #' @field protectedRanges objects protected ranges
    protectedRanges = NULL,

    #' @field basicFilter objects basic filters
    basicFilter = NULL,

    #' @field charts objects charts
    charts = NULL,

    #' @field bandedRanges banded Ranges objects
    bandedRanges = NULL,

    #' @field developerMetadata objects
    developerMetadata = NULL,

    #' @field rowGroups rowGroups objects
    rowGroups = NULL,

    #' @field columnGroups columnGroups objects
    columnGroups = NULL,

    #' @field slicers slicers objects
    slicers = NULL,

    #' @description Initialize object
    #' @param spreadSheetId ID ofthe spreadsheet
    initialize = function(spreadSheetId) {
      private$`.spreadSheetId` <- spreadSheetId

      invisible(self$get_data(c("spreadsheetProperties", "sheetProperties")))

    },

    #' @description Get data about specific spreadsheet or sheet properties
    #' @param fields character vector of fields to get. Valid values can be
    #' retrieved with `SpreadSheetData$valid_fields`
    #' @param refresh boolean indicating if data should be refreshed
    get_data = function(
    fields,
    refresh = FALSE) {

      invalid_fields <- which(!fields %in% names(private$queryFields))

      if (length(invalid_fields) > 0)
        stop(paste0("Invalid field(s): ", paste(fields[invalid_fields], collapse = ",")))

      fields_to_acquire <- vapply(
        fields, \(field) {
          is.null(private$data[[field]]) || isTRUE(refresh)
        },
        logical(1))

      if (sum(fields_to_acquire) > 0) {
        fields_to_acquire <- fields[fields_to_acquire]

        fields_loc <- private$queryFields[fields_to_acquire]

        ss_data <- getSpreadsheetData(
          spreadsheetId = private$.spreadSheetId,
          fields = paste(fields_loc, collapse = ",")
        )

        invisible(
          lapply(
            seq_along(fields_to_acquire),
            \(i) {
              if (grepl(x = fields_loc[i], pattern = "sheets\\.")) {
                private$data[[names(fields_loc)[i]]] <-
                  lapply(ss_data$sheets,
                         \(x) x[[gsub(x = fields_loc[i],
                                      pattern = "sheets\\.",
                                      replacement = "")]])
              } else {
                private$data[[names(fields_loc)[i]]] <-
                  ss_data[[fields_loc[i]]]
              }
            }
          )
        )
      }

      out <- private$data[fields]
      out <- out[!vapply(out, is.null, logical(1))]

      return(out)

    }
  ),

  private = list(
    queryFields = c(
      spreadsheetProperties = "properties",
      sheetProperties = "sheets.properties",
      namedRanges = "namedRanges",
      merges = "sheets.merges",
      filterViews = "sheets.filterViews",
      protectedRanges = "sheets.protectedRanges",
      basicFilter = "sheets.basicFilter",
      charts = "sheets.charts",
      bandedRanges = "sheets.bandedRanges",
      developerMetadata = "sheets.developerMetadata",
      rowGroups = "sheets.rowGroups",
      columnGroups = "sheets.columnGroups",
      slicers = "sheets.slicers"
    ),

    .spreadSheetId = NULL,
    .sheetsVec = NULL,

    data = list(
      spreadsheetProperties = NULL,
      sheetProperties = NULL,
      namedRanges = NULL,
      merges = NULL,
      filterViews = NULL,
      protectedRanges = NULL,
      basicFilter = NULL,
      charts = NULL,
      bandedRanges = NULL,
      developerMetadata = NULL,
      rowGroups = NULL,
      columnGroups = NULL,
      slicers = NULL
    )
  ),

  active = list(

    #' @field spreadSheetId ID of the spreadsheet
    spreadSheetId = function(value) {
      if (!missing(value))
        stop("This field is read-only.")
      return(private$`.spreadSheetId`)
    },

    #' @field valid_fields Fields valid to get with `get_data()` method
    valid_fields = function(value) {
      if (!missing(value))
        stop("This field is read-only.")
      return(names(private$queryFields))
    },

    #' @field sheets Named vector of sheets inside this spreadsheet. Names of the
    #' vector are the titles of sheets, and values are the sheets IDs
    sheets = function(value) {
      if (!missing(value))
        stop("This field is read-only.")

      if (is.null(private$data$sheetProperties))

      sheetProperties <-
        self$get_data(fields = "sheetProperties")$sheetProperties

      sheets_vec <- setNames(
        nm = vapply(sheetProperties, \(x) x$title, character(1)),
        object = vapply(sheetProperties, \(x) as.integer(x$sheetId), double(1))
      )

      return(sheets_vec)

    }
  )

)
