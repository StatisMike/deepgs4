resp <- send_create_req(spreadsheet = spreadsheet)


gd_economics <- to_GridData_from_df(
  df = ggplot2::economics,
  0, 0,
  names_format = CellFormat(backgroundColorStyle = ColorStyle(themeColorType = "ACCENT1")),
  values_format = CellFormat(textFormat = TextFormat(foregroundColorStyle = ColorStyle(themeColorType = "ACCENT3"))))

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
        576,
        7,
        1,
        0,
        TRUE
      ),
      tabColorStyle = ColorStyle(
        themeColorType = "ACCENT3"
      )
    ),
    data = gd_economics
  )
)

deepgs_auth("statismike@gmail.com")

ss <- send_create_req(spreadsheet)


#### for testing charts ####

cars_spreadsheet <- Spreadsheet(
  properties = SpreadsheetProperties(
    title = "Cars Spreadsheet"
  ),
  sheets = list(
    Sheet(
      properties = SheetProperties(
        sheetId = 0,
        title = "Embedded chart test",
        gridProperties = GridProperties(30, 30)
      ),
      data = to_GridData_from_df(cars, 0, 0)),
    Sheet(
      properties = SheetProperties(
        sheetId = 1,
        title = "Cars dataset",
        gridProperties = GridProperties(51, 2)
      ),
      data = to_GridData_from_df(cars, 0, 0))
  )
)
