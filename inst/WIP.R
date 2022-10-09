cars_gd <- to_GridData_from_df(cars, 0, 0)

test_spreadsheet <- Spreadsheet(
  properties = SpreadsheetProperties("gridProperties test"),
  sheets = list(
    Sheet(
      properties = SheetProperties(
        sheetId = 0,
        title = "First cars",
        gridProperties = GridProperties(51, 2)),
      data = cars_gd),
    Sheet(
      properties = SheetProperties(
        sheetId = 1,
        title = "Second cars",
        gridProperties = GridProperties(51, 2)
      ),
      data = cars_gd
    )
  )
)

created <- send_create_req(test_spreadsheet)

newRule <- ConditionalFormatRule(
  ranges = list(GridRange(0, 0, 52, 0, 1),
                GridRange(0, 0, 52, 1, 2)),
  gradientRule = GradientRule(
    InterpolationPoint(colorStyle = ColorStyle(themeColorType = "ACCENT1"), type = "MIN"),
    InterpolationPoint(colorStyle = ColorStyle(themeColorType = "ACCENT6"), type = "MAX")
  )
)

addReq <- AddConditionalFormatRule(
  0, rule
)

resp <- send_batchUpdate_req(
  created$spreadsheetId,
  addReq
)

updateRule <- ConditionalFormatRule(
  ranges = GridRange(0, 0, 52, 1, 2),
  gradientRule = GradientRule(
    InterpolationPoint(colorStyle = ColorStyle(themeColorType = "ACCENT1"), type = "MIN"),
    InterpolationPoint(colorStyle = ColorStyle(themeColorType = "ACCENT3"), type = "MAX")
  )
)

updateReq <- UpdateConditionalFormatRule(
  0, rule = updateRule
)

resp <- send_batchUpdate_req(
  created$spreadsheetId,
  updateReq
)

nextRule <- ConditionalFormatRule(
  ranges = GridRange(0, 0, 52, 0, 1),
  booleanRule = BooleanRule(
    condition = BooleanCondition("NUMBER_GREATER",
                                 values = ConditionValue("10")),
    CellFormat(backgroundColorStyle = ColorStyle(theme = "ACCENT4"))
  )
)

nextAddReq <- AddConditionalFormatRule(
  index = 1,
  rule = nextRule
)

resp <- send_batchUpdate_req(
  created$spreadsheetId,
  nextAddReq
)

moveReq <- UpdateConditionalFormatRule(
  index = 0,
  newIndex = 1,
  sheetId = 0
)

resp <- send_batchUpdate_req(
  created$spreadsheetId,
  moveReq
)

check <- send_get_req(created$spreadsheetId,
             fields = "sheets.conditionalFormats")



# ss_id <- googledrive::as_id(created$spreadsheetId)
# on.exit(googledrive::drive_trash(ss_id))
# googledrive::drive_share(ss_id,
#                          role = "writer",
#                          type = "user",
#                          "emailAddress" = "statismike@gmail.com")
