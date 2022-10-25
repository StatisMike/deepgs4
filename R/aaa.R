# Package environment will be used for many convienience things. During development
# there will be a need to add new positions there
pkg_env <- new.env()

#### Valid Update Fields ####
# valid `FieldMask` values for specific `Update*Request` need to be generated
# there
pkg_env$valid_update_fields <- list(
  ##### UpdatCells ####
  "UpdateCells" = c(
    "userEnteredValue",
    "userEnteredFormat",
    paste(sep = ".", "userEnteredFormat",
          c("numberFormat",
          "backgroundColorStyle",
          "borders",
          "padding",
          "horizontalAlignment",
          "verticalAlignment",
          "wrapStrategy",
          "textDirection",
          "textFormat",
          paste(sep = ".", "textFormat",
                c("foregroundColorStyle",
                "fontFamily",
                "fontSize",
                "bold",
                "italic",
                "strikethroug",
                "underline",
                "link")),
          "hyperlinkDisplayType",
          "textRotation")),
    "note",
    "textFormatRuns",
    "dataValidation",
    paste(sep = ".", "dataValidation",
          c("condition",
          "inputMessage",
          "strict",
          "showCustomUi"))),
  ##### UpdateSheetProperties ####
  "UpdateSheetProperties" = c(
    "title",
    "index",
    "gridProperties",
    paste(sep = ".", "gridProperties",
          c("rowCount",
            "columnCount",
            "frozenColumnCount",
            "hideGridlines",
            "rowGroupControlAfter",
            "columnGroupControlAfter")),
    "hidden",
    "tabColorStyle"
  )
)

#### Valid Chart Specs ####
# During addition of new type of chart spec, name of its constructor (and class)
# needs to be added below
pkg_env$valid_chart_specs <- c(
  "BasicChartSpec"
)

#### deeps classes prefic ####
pkg_env$cls_prfx <- "dgs4"
pkg_env$object_types <- c("Obj", "Req", "Response", "Data")
