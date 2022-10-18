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
on.exit(googledrive::drive_trash(ss_id))

test_that("AddSheetRequest can be created and send", {

  expect_failure(
    expect_error(
      min_req <- AddSheetRequest(
        SheetProperties()
      )
    )
  )

  expect_true(is.dgs4Req(min_req))

  expect_failure(
    expect_error(
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
    )
  )

  expect_true(is.dgs4Req(min_req))

  expect_failure(
    expect_error(
      resp_min <- send_batchUpdate_req(created$spreadsheetId,
                                       min_req)
    )
  )

  expect_failure(
    expect_error(
      resp_max <- send_batchUpdate_req(created$spreadsheetId,
                                       max_req)
    )
  )

  comparison <-
    compare_objects(max_req, resp_max$replies[[1]],
                    skip_compare = c("red", "green", "blue"))

  expect_true(all(comparison[!is.na(comparison)]))

})

test_that("UpdateSheetRequest can be created and send", {

  expect_failure(
    expect_error(
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
    )
  )

  expect_true(is.dgs4Req(update_req))

  expect_failure(
    expect_error(
      resp_update <- send_batchUpdate_req(created$spreadsheetId,
                                       update_req)
    )
  )

  confirm <- send_get_req(created$spreadsheetId,
                          fields = "sheets.properties",
                          range = paste0("'", update_req$updateSheetProperties$properties$title, "'"))

  comparison_update <- compare_objects(
    update_req$updateSheetProperties$properties,
    confirm$sheets[[1]]$properties,
    skip_compare = c("red", "blue", "green"))

  na_compare <- vapply(comparison_update, is.na, logical(1))

  expect_true(all(comparison_update[!na_compare]))

  expect_equal(
    names(comparison_update[na_compare]),
    c("gridProperties.hideGridlines", "gridProperties.rowGroupControlAfter",
      "gridProperties.columnGroupControlAfter", "hidden", "rightToLeft")
  )

  expect_true(all(comparison_update[!is.na(comparison_update)]))

})

test_that("DeleteSheetRequest can be created and send", {

  expect_failure(
    expect_error(
      delete_req <- DeleteSheetRequest(
        sheetId = 2137
      )
    )
  )

  expect_true(is.dgs4Req(delete_req))

  expect_failure(
    expect_error(
      resp_delete <- send_batchUpdate_req(created$spreadsheetId,
                                          delete_req)
    )
  )

  confirm_delete <- send_get_req(spreadsheetId = created$spreadsheetId,
                                 fields = "sheets.properties.sheetId")

  expect_true(!2137 %in% vapply(confirm_delete$sheets, \(x) x$properties$sheetId, numeric(1)))

})
