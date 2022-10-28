dgs4_auth(path = Sys.getenv("G_SERVICE_ACCOUNT"),
            cache = F)
googledrive::drive_auth(path = Sys.getenv("G_SERVICE_ACCOUNT"),
                        cache = F)

created <- request_ss_create(
  spreadsheet = Spreadsheet(
    sheets = Sheet(
      properties = SheetProperties(
        sheetId = 0, title = "Test_Sheet",
        gridProperties = GridProperties(
          10, 10
        )
      )
    )
  )
)

on.exit(googledrive::drive_trash(googledrive::as_id(created$spreadsheetId)))

test_that("UpdateDimensionPropertiesRequest can be created and send", {

  expect_failure(
    expect_error(
      columnReq <- UpdateDimensionPropertiesRequest(
        properties = DimensionProperties(pixelSize = 140,
                                         hiddenByUser = TRUE),
        range = DimensionRange(sheetId = 0,
                               dimension = "COLUMNS",
                               startIndex = 5,
                               endIndex = 7),
        fields = c("hiddenByUser", "pixelSize")
      )
    )
  )

  expect_true(is.dgs4Req(columnReq))

  expect_failure(
    expect_error(
      rowReq <- UpdateDimensionPropertiesRequest(
        properties = DimensionProperties(
          hiddenByUser = TRUE,
          pixelSize = 50
        ),
        range = DimensionRange(sheetId = 0,
                               dimension = "ROWS",
                               startIndex = 7),
        fields = c("hiddenByUser", "pixelSize")
      )
    )
  )

  expect_true(is.dgs4Req(rowReq))

  expect_failure(
    expect_error(
      update_col_resp <- request_ss_batchUpdate(
        created$spreadsheetId,
        columnReq
      )
    )
  )

  expect_failure(
    expect_error(
      update_row_resp <- request_ss_batchUpdate(
        created$spreadsheetId,
        rowReq
      )
    )
  )

  confirm <- request_ss_get(
    created$spreadsheetId,
    fields = c("sheets.data.columnMetadata,sheets.data.rowMetadata"))

  col_properties <- vapply(confirm$sheets[[1]]$data[[1]]$columnMetadata,
                           \(x) list(hiddenByUser = x$hiddenByUser,
                                     pixelSize = x$pixelSize),
                           vector("list", 2))

  row_properties <- vapply(confirm$sheets[[1]]$data[[1]]$rowMetadata,
                           \(x) list(hiddenByUser = x$hiddenByUser,
                                     pixelSize = x$pixelSize),
                           vector("list", 2))

  expect_true(
    all(as.logical(col_properties["hiddenByUser", c(6:7)]))
  )

  expect_true(
    all(vapply(col_properties["hiddenByUser", c(1:5, 8:10)], is.null, logical(1)))
  )

  expect_equal(
    as.numeric(col_properties["pixelSize", ]),
    c(rep(100, 5), rep(140, 2), rep(100, 3))
  )

  expect_true(
    all(as.logical(row_properties["hiddenByUser", c(8:10)]))
  )

  expect_true(
    all(vapply(row_properties["hiddenByUser", c(1:7)], is.null, logical(1)))
  )

  expect_equal(
    as.numeric(row_properties["pixelSize", ]),
    c(rep(21, 7), rep(50, 3))
  )
})

test_that("InserDimensionRequest can be created and send", {

  expect_failure(
    expect_error(
      columnReq <- InsertDimensionRequest(
        range = DimensionRange(sheetId = 0,
                               dimension = "COLUMNS",
                               startIndex = 7,
                               endIndex = 9),
        inheritFromBefore = TRUE
      )
    )
  )

  expect_true(is.dgs4Req(columnReq))

  expect_failure(
    expect_error(
      rowReq <- InsertDimensionRequest(
        range = DimensionRange(sheetId = 0,
                               dimension = "ROWS",
                               startIndex = 7,
                               endIndex = 9)
      )
    )
  )

  expect_true(is.dgs4Req(rowReq))

  expect_failure(
    expect_error(
      insert_col_resp <- request_ss_batchUpdate(
        created$spreadsheetId,
        columnReq
      )
    )
  )

  expect_failure(
    expect_error(
      insert_row_resp <- request_ss_batchUpdate(
        created$spreadsheetId,
        rowReq
      )
    )
  )

  confirm <- request_ss_get(
    created$spreadsheetId,
    fields = c("sheets.data.columnMetadata,sheets.data.rowMetadata"))

  col_properties <- vapply(confirm$sheets[[1]]$data[[1]]$columnMetadata,
                           \(x) list(hiddenByUser = x$hiddenByUser,
                                     pixelSize = x$pixelSize),
                           vector("list", 2))

  row_properties <- vapply(confirm$sheets[[1]]$data[[1]]$rowMetadata,
                           \(x) list(hiddenByUser = x$hiddenByUser,
                                     pixelSize = x$pixelSize),
                           vector("list", 2))

  expect_equal(
    as.numeric(col_properties["pixelSize", 8:9]),
    rep(140, 2)
  )

  expect_equal(
    as.numeric(row_properties["pixelSize", 8:9]),
    rep(50, 2)
  )

})

test_that("AppendDimensionRequest can be created and send", {

  expect_failure(
    expect_error(
      columnReq <- AppendDimensionRequest(
        sheetId = 0,
        length = 10,
        dimension = "COLUMNS"
      )
    )
  )

  expect_true(is.dgs4Req(columnReq))

  expect_failure(
    expect_error(
      rowReq <- AppendDimensionRequest(
        sheetId = 0,
        length = 20,
        dimension = "ROWS"
      )
    )
  )

  expect_true(is.dgs4Req(rowReq))

  expect_failure(
    expect_error(
      append_col_resp <- request_ss_batchUpdate(
        created$spreadsheetId,
        columnReq
      )
    )
  )

  expect_failure(
    expect_error(
      append_row_resp <- request_ss_batchUpdate(
        created$spreadsheetId,
        rowReq
      )
    )
  )

  confirm <- request_ss_get(
    created$spreadsheetId,
    fields = c("sheets.properties.gridProperties"))

  expect_equal(c(rowCount = confirm$sheets[[1]]$properties$gridProperties$rowCount,
                 columnCount = confirm$sheets[[1]]$properties$gridProperties$columnCount),
               c(rowCount = 32,
                 columnCount = 22))

})

test_that("AutoResizeDimensionRequest can be created and send", {

  expect_failure(
    expect_error(
      columnReq <- AutoResizeDimensionsRequest(
        dimensions = DimensionRange(0, 3, 8, "COLUMNS")
      )
    )
  )

  expect_true(is.dgs4Req(columnReq))

  expect_failure(
    expect_error(
      rowReq <- AutoResizeDimensionsRequest(
        dimensions = DimensionRange(0, 17, 20)
      )
    )
  )

  expect_true(is.dgs4Req(rowReq))

  expect_failure(
    expect_error(
      autoresize_col_resp <- request_ss_batchUpdate(
        created$spreadsheetId,
        columnReq
      )
    )
  )

  expect_failure(
    expect_error(
      autoresize_row_resp <- request_ss_batchUpdate(
        created$spreadsheetId,
        rowReq
      )
    )
  )

})
