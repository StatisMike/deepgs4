googledrive::drive_auth(path = Sys.getenv("G_SERVICE_ACCOUNT"),
                        cache = F)
dgs4_auth(path = Sys.getenv("G_SERVICE_ACCOUNT"),
          cache = F)

test_title <- "Test sheet"
gen_test_A1 <- function(title, lt, rd) {
  paste0("'", title, "'!", lt, ":", rd)
}

# create test spreadsheet
test_spreadsheet <- Spreadsheet(
  properties = SpreadsheetProperties(),
  sheets = Sheet(properties = SheetProperties(title = test_title))
)

created <- request_ss_create(test_spreadsheet)
ss_id <- googledrive::as_id(created$spreadsheetId)
on.exit(googledrive::drive_trash(ss_id))

els <- new.env()

els[["by_rows"]] <- ValueRange(
  range = gen_test_A1(test_title, "A1", "B2"),
  values = list(
    list("Values", "Data"),
    list(1, TRUE)
  ),
  majorDimension = "ROWS"
)

els[["by_cols"]] <- ValueRange(
  range = gen_test_A1(test_title, "A1", "B"),
  values = list(
    cars$speed,
    cars$dist
  ),
  majorDimension = "COLUMNS"
)

els[["cars_update"]] <- ValueRange(
  range = gen_test_A1(test_title, "A3", "B53"),
  values =list(
    cars$speed * 1.5,
    cars$dist * 10
  ),
  majorDimension = "COLUMNS"
)

test_that("request append values works", {
  expect_failure(
    expect_error(
      resp <- request_ss_append_values(
        spreadsheetId = ss_id,
        range = gen_test_A1(test_title, "A1", "B2"),
        values = els$by_rows,
        valueInputOption = "USER_ENTERED",
        insertDataOption = "OVERWRITE",
        includeValuesInResponse = TRUE
      )
    )
  )

  expect_failure(
    expect_error(
      resp <- request_ss_append_values(
        spreadsheetId = ss_id,
        range = gen_test_A1(test_title, "A1", "B"),
        values = els$by_cols,
        valueInputOption = "RAW",
        insertDataOption = "OVERWRITE",
        includeValuesInResponse = FALSE
      )
    )
  )
})

test_that("request update values works", {

  expect_failure(
    expect_error(
      resp <- request_ss_update_values(
        spreadsheetId = ss_id,
        range = els$cars_update$range,
        values = els$cars_update,
        valueInputOption = "USER_ENTERED",
        includeValuesInResponse = TRUE,
        responseValueRenderOption = "UNFORMATTED_VALUE"
      )
    )
  )

})

test_that("request clear values works", {

  expect_failure(
    expect_error(
      resp <- request_ss_clear_values(
        spreadsheetId = ss_id,
        range = els$cars_update$range
      )
    )
  )

  expect_failure(
    expect_error(
      resp <- request_ss_get_values(
        spreadsheetId = ss_id,
        range = paste0("'", test_title, "'")
      )
    )
  )

  expect_equal(length(resp$values), 2)

})


