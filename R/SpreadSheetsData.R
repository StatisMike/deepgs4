#' @title SpreadSheetData
#' @docType class
#' @description Object holding and querying for data regarding specific
#' spreadsheet
#' @section Warning:
#' WIP: do not use!
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
          is.null(self[[field]]) || isTRUE(refresh)
        },
        logical(1))

      if (sum(fields_to_acquire) > 0) {
        fields_to_acquire <- fields[fields_to_acquire]

        fields_loc <- private$queryFields[fields_to_acquire]

        ss_data <- private$getSpreadsheetData(
          fields = paste(fields_loc, collapse = ",")
        )

        invisible(
          lapply(
            seq_along(fields_to_acquire),
            \(i) {

              fieldNm <- fields_to_acquire[i]

              if (grepl(x = fields_loc[i], pattern = "sheets\\.")) {

                self[[fieldNm]] <- lapply(
                  seq_along(ss_data$sheets),
                  \(i_sh) {

                    fieldData <-
                      ss_data$sheets[[i_sh]][[gsub(x = fields_loc[i],
                                                   pattern = "sheets\\.",
                                                   replacement = "")]]

                    private$process_sheet_field(
                      fieldData = fieldData,
                      fieldNm = fieldNm,
                      sheetId = self$sheetProperties[[i_sh]]$sheetId
                    )
                  }
                )

                names(self[[fieldNm]]) <-
                  vapply(self$sheetProperties,
                         \(x) as.character(x$sheetId),
                         character(1))
              } else {
                self[[names(fields_loc)[i]]] <-
                  private$process_field(
                    fieldData = ss_data[[fields_loc[i]]],
                    fieldNm = fieldNm)
              }
            }
          )
        )
      }

      out <- vapply(fields, \(x) list(self[[x]]), vector("list", 1))

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

    getSpreadsheetData = function(fields) {

      req <- request_generate(
        endpoint = "sheets.spreadsheets.get",
        params = list(
          spreadsheetId = private$.spreadSheetId,
          fields = fields
        )
      )

      resp <- request_make(req)

      gargle::response_process(resp)

    },

    process_field = function(fieldData,
                             fieldNm) {

      switch(fieldNm,

             spreadsheetProperties = gen_dgs4Obj(obj = fieldData,
                                                         class = "SpreadsheetProperties"),

             fieldData)


    },

    process_sheet_field = function(fieldData,
                                   fieldNm,
                                   sheetId) {

      switch(fieldNm,
             sheetProperties = gen_dgs4Obj(obj = fieldData,
                                                   class = "SheetProperties"),
             merges = lapply(fieldData,
                             gen_dgs4Obj,
                             class = "GridRange",
                             sheetId = sheetId),
             fieldData)

    }
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

      self$get_data(fields = "sheetProperties")

      sheets_vec <- setNames(
        nm = vapply(self$sheetProperties, \(x) x$title, character(1)),
        object = vapply(self$sheetProperties, \(x) as.integer(x$sheetId), double(1))
      ) |>
        dgs4_class("sheetVec", "Data")

      return(sheets_vec)

    }
  )

)
