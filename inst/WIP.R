googledrive::drive_auth(path = Sys.getenv("G_SERVICE_ACCOUNT"),
                        cache = F)
dgs4_auth(path = Sys.getenv("G_SERVICE_ACCOUNT"),
          cache = F)

# create test spreadsheet
test_spreadsheet <- Spreadsheet(
  properties = SpreadsheetProperties(),
  sheets = Sheet(SheetProperties(sheetId = 0,
                                 gridProperties = GridProperties(10,10)))
)

created <- send_create_req(test_spreadsheet)
ss_id <- googledrive::as_id(created$spreadsheetId)

req_min <- UpdateBordersRequest(
        range = GridRange(0, 1, 10, 2, 9),
        top = Border("DOTTED")
      )

resp_min <- send_batchUpdate_req(
  created$spreadsheetId,
  req_min
)

confirm <- send_get_req(created$spreadsheetId,
                        fields = "sheets.data.rowData.values.userEnteredFormat.borders")

top_borders_row_1 <- sapply(
  confirm$sheets[[1]]$data[[1]]$rowData[[1]]$values,
  \(x) if (!is.null(x$userEnteredFormat$borders))
    x$userEnteredFormat$borders$top$style
  else NA)

top_borders_row_2 <- sapply(
  confirm$sheets[[1]]$data[[1]]$rowData[[2]]$values,
  \(x) if (!is.null(x$userEnteredFormat$borders))
    x$userEnteredFormat$borders$top$style
  else NA)

expect_true(length(top_borders_row_1) == 0)
expect_equal(top_borders_row_2,
             c(rep(NA, 2), rep("DOTTED", 7)))

req_max <- UpdateBordersRequest(
  range = GridRange(0, 1, 10, 2, 9),
  top = Border("NONE"),
  bottom = Border("DOUBLE"),
  left = Border("SOLID_MEDIUM"),
  right = Border("SOLID_THICK"),
  innerHorizontal = Border("DASHED"),
  innerVertical = Border(colorStyle = ColorStyle("ACCENT3"))
)

resp_max <- send_batchUpdate_req(
  created$spreadsheetId,
  req_max
)

confirm <- send_get_req(created$spreadsheetId,
                        fields = "sheets.data.rowData.values.userEnteredFormat.borders")

top_borders_row_2 <- sapply(
  confirm$sheets[[1]]$data[[1]]$rowData[[2]]$values,
  \(x) if (!is.null(x$userEnteredFormat$borders$top$style))
    x$userEnteredFormat$borders$top$style
  else NA)

top_borders_row_3 <- sapply(
  confirm$sheets[[1]]$data[[1]]$rowData[[3]]$values,
  \(x) if (!is.null(x$userEnteredFormat$borders))
    x$userEnteredFormat$borders$top$style
  else NA)

left_borders_row_3 <- sapply(
  confirm$sheets[[1]]$data[[1]]$rowData[[3]]$values,
  \(x) if (!is.null(x$userEnteredFormat$borders))
    x$userEnteredFormat$borders$left$style
  else NA)

bottom_borders_row_10 <- sapply(
  confirm$sheets[[1]]$data[[1]]$rowData[[10]]$values,
  \(x) if (!is.null(x$userEnteredFormat$borders))
    x$userEnteredFormat$borders$bottom$style
  else NA)

expect_true(all(vapply(top_borders_row_2, is.na, logical(1))))
expect_equal(top_borders_row_3,
             c(rep(NA, 2), rep("DASHED", 7)))
expect_equal(left_borders_row_3,
             c(rep(NA, 2), "SOLID_MEDIUM", rep("SOLID", 6)))
expect_equal(bottom_borders_row_10,
             c(rep(NA, 2), rep("DOUBLE", 7)))



googledrive::drive_trash(ss_id)
