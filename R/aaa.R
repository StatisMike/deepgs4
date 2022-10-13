pkg_env <- new.env()

pkg_env$valid_chart_specs <- c(
  "BasicChartSpec"
)

pkg_env$valid_update_fields <- list(
  "Cells" = c(
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
          "showCustomUi")))
)
