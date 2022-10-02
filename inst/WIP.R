spreadsheet <- Spreadsheet(
  properties = SpreadsheetProperties(
    title = "Testowy spreadsheet",
    locale = "pl"
  ),
  sheets = Sheet(
    properties = SheetProperties(
      sheetId = 1,
      title = "Pierwszy Sheet",
      gridProperties = GridProperties(
        100,
        100,
        1,
        1,
        TRUE
      ),
      tabColorStyle = ColorStyle(
        themeColorType = "ACCENT3"
      )
    )
  )
)

resp <- send_create_req(spreadsheet = spreadsheet)
