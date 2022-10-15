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




googledrive::drive_auth(path = Sys.getenv("G_SERVICE_ACCOUNT"),
                        cache = F)
dgs4_auth(path = Sys.getenv("G_SERVICE_ACCOUNT"),
          cache = F)

# create test spreadsheet
test_spreadsheet <- Spreadsheet(
  properties = SpreadsheetProperties()
)

created <- send_create_req(test_spreadsheet)
ss_id <- googledrive::as_id(created$spreadsheetId)


min_req <- AddSheetRequest(
  SheetProperties()
)

max_req <- AddSheetRequest(
  SheetProperties(sheetId = 2137,
                  title = "Test of sheet",
                  index = 1,
                  sheetType = "GRID",
                  gridProperties = GridProperties(10, 10, 1, 2, TRUE, TRUE, TRUE),
                  hidden = TRUE,
                  tabColorStyle = ColorStyle(0.5, 0.4, 0.6),
                  rightToLeft = TRUE)
)

resp_min <- send_batchUpdate_req(created$spreadsheetId,
                                 min_req)

resp_max <- send_batchUpdate_req(created$spreadsheetId,
                                 max_req)

comparison <-
  compare_objects(max_req, resp_max$replies[[1]],
                  skip_compare = c("red", "green", "blue"))

update_req <- UpdateSheetPropertiesRequest(
  SheetProperties(sheetId = 2137,
                  title = "Test of sheetUpdate",
                  index = 0,
                  sheetType = "GRID",
                  gridProperties = GridProperties(15, 20, 3, 1, FALSE, FALSE, FALSE),
                  hidden = FALSE,
                  tabColorStyle = ColorStyle(0.6, 0.3, 0.2),
                  rightToLeft = FALSE)
)

resp_update <- send_batchUpdate_req(created$spreadsheetId,
                                    update_req)

confirm <- send_get_req(created$spreadsheetId,
                        fields = "sheets.properties",
                        range = paste0("'", update_req$updateSheetProperties$properties$title, "'"))

# for now, as get req don't return spreadsheet
confirm <- gen_Spreadsheet(confirm)

comparison_update <- compare_objects(
  update_req$updateSheetProperties$properties,
  confirm$sheets[[1]]$properties,
  skip_compare = c("red", "blue", "green"))

na_compare <- vapply(comparison_update, is.na, logical(1))

expect_true(all(comparison_update[!na_compare]))

expect_equal(
  names(comparison_update[na_compare]),
  c("gridProperties.hideGridlines", "gridProperties.rowGroupControlAfter",
    "gridProperties.columnGroupControlAfter",
    "hidden", "rightToLeft")
)

googledrive::drive_trash(ss_id)
