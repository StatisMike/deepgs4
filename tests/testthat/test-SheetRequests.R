googledrive::drive_auth(path = Sys.getenv("G_SERVICE_ACCOUNT"),
                        cache = F)
dgs4_auth(path = Sys.getenv("G_SERVICE_ACCOUNT"),
          cache = F)

# create test spreadsheet
test_spreadsheet <- Spreadsheet(
  properties = SpreadsheetProperties()
)

created <- request_ss_create(test_spreadsheet)
ss_id <- googledrive::as_id(created$spreadsheetId)
on.exit(googledrive::drive_trash(ss_id))

els <- new.env()

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

  expect_true(is.dgs4Req(max_req))

  expect_failure(
    expect_error(
      els$resp_min <- request_ss_batchUpdate(
        created$spreadsheetId,
        min_req)
    )
  )

  expect_failure(
    expect_error(
      els$resp_max <- request_ss_batchUpdate(
        created$spreadsheetId,
        max_req)
    )
  )

  comparison <-
    compare_objects(max_req, els$resp_max$replies[[1]],
                    skip_compare = c("red", "green", "blue"))

  expect_true(all(comparison[!is.na(comparison)]))

})

test_that("DuplicateSheetRequest can be generated and sent", {

  expect_failure(
    expect_error(
      duplicate_min <- DuplicateSheetRequest(
        sourceSheetId = els$resp_min$replies[[1]]$properties$sheetId,
        insertSheetIndex = 0
      )
    )
  )

  expect_failure(
    expect_error(
      duplicate_max <- DuplicateSheetRequest(
        sourceSheetId = els$resp_max$replies[[1]]$properties$sheetId,
        insertSheetIndex = 1,
        newSheetId = 11323,
        newSheetName = "Duplicated max"
      )
    )
  )

  expect_failure(
    expect_error(
      resps_dup <- request_ss_batchUpdate(
        created$spreadsheetId,
        duplicate_min,
        duplicate_max
      )
    )
  )

  compare_min <- compare_objects(
    obj1 = els$resp_min$replies[[1]]$properties,
    obj2 = resps_dup$replies[[1]]$properties,
    skip_compare = c("sheetId", "title", "index"),
    na.rm = TRUE
  )

  expect_true(all(compare_min))

  compare_max <- compare_objects(
    obj1 = els$resp_max$replies[[1]]$properties,
    obj2 = resps_dup$replies[[2]]$properties,
    skip_compare = c("sheetId", "title", "index"),
    na.rm = TRUE
  )

  expect_true(all(compare_max))
  expect_equal(c(sheetId = resps_dup$replies[[2]]$properties$sheetId,
                 title = resps_dup$replies[[2]]$properties$title,
                 index = resps_dup$replies[[2]]$properties$index),
               c(sheetId = 11323, title = "Duplicated max", index = 1))


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
      resp_update <- request_ss_batchUpdate(created$spreadsheetId,
                                       update_req)
    )
  )

  confirm <- request_ss_get(created$spreadsheetId,
                          fields = "sheets.properties",
                          range = paste0("'", update_req$properties$title, "'"))

  comparison_update <- compare_objects(
    update_req$properties,
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
      resp_delete <- request_ss_batchUpdate(created$spreadsheetId,
                                            delete_req)
    )
  )

  confirm_delete <- request_ss_get(spreadsheetId = created$spreadsheetId,
                                   fields = "sheets.properties.sheetId")

  expect_true(!2137 %in% vapply(confirm_delete$sheets, \(x) x$properties$sheetId, numeric(1)))

})
