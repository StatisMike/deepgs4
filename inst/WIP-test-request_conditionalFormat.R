googledrive::drive_auth(email = Sys.getenv("G_SERVICE_MAIL"),
                        path = Sys.getenv("G_SERVICE_ACCOUNT"),
                        cache = F)
deepgs_auth(email = Sys.getenv("G_SERVICE_MAIL"),
            path = Sys.getenv("G_SERVICE_ACCOUNT"),
            cache = F)

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

created <- send_create_req(cars_spreadsheet)
ss_id <- googledrive::as_id(created$spreadsheetId)
on.exit(googledrive::drive_trash(ss_id))
# googledrive::drive_share(ss_id,
#                          role = "writer",
#                          type = "user",
#                          "emailAddress" = "statismike@gmail.com")


Sheet(
  properties = SheetProperties(
    sheetId = 1,
    title = "Cars dataset",
    gridProperties = GridProperties(51, 2)
  ),
  data = )
