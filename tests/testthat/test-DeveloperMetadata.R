els <- new.env(parent = baseenv())

test_that("DeveloperMetadataLocation can be created, listinized and generated", {

  expect_failure(
    expect_error(
      els$loc_spreadsheet <- DeveloperMetadataLocation(
        spreadsheet = TRUE
      )
    )
  )

  expect_true(is.DeveloperMetadataLocation(els$loc_spreadsheet))
  expect_genned_identical(els$loc_spreadsheet)

  expect_failure(
    expect_error(
      els$loc_sheet <- DeveloperMetadataLocation(
        sheetId = 0
      )
    )
  )

  expect_true(is.DeveloperMetadataLocation(els$loc_sheet))
  expect_genned_identical(els$loc_sheet)

  expect_failure(
    expect_error(
      els$loc_range <- DeveloperMetadataLocation(
        dimensionRange = DimensionRange(
          sheetId = 0,
          dimension = "ROWS",
          startIndex = 1,
          endIndex = 2
        )
      )
    )
  )

  expect_true(is.DeveloperMetadataLocation(els$loc_range))
  expect_genned_identical(els$loc_range)

})

test_that("DeveloperMetadataLocation can be created, listinized and generated", {

  expect_failure(
    expect_error(
      els$data$spreadsheet <- DeveloperMetadata(
        location = els$loc_spreadsheet,
        metadataKey = "test_meta"
      )
    )
  )

  expect_true(is.DeveloperMetadata(els$data$spreadsheet))
  expect_genned_identical(els$data$spreadsheet)

  expect_failure(
    expect_error(
      els$data$sheet <- DeveloperMetadata(
        location = els$loc_sheet,
        metadataKey = "test_meta",
        metadataValue = "sheet_meta",
        visibility = "DOCUMENT"
      )
    )
  )

  expect_true(is.DeveloperMetadata(els$data$sheet))
  expect_genned_identical(els$data$sheet)

  expect_failure(
    expect_error(
      els$data$range <- DeveloperMetadata(
        location = els$loc_range,
        metadataKey = "test_meta",
        visibility = "PROJECT"
      )
    )
  )

  expect_true(is.DeveloperMetadata(els$data$range))
  expect_genned_identical(els$data$range)

})

test_that("DeveloperMetadataLookup can be created, listinized and generated", {

  expect_failure(
    expect_error(
      els$lookup_location <- DeveloperMetadataLookup(
        metadataLocation = DeveloperMetadataLocation(
          dimensionRange = DimensionRange(0, 2, 3, "COLUMNS")
        ),
        locationMatchingStrategy = "INTERSECTING_LOCATION"
      )
    )
  )

  expect_true(is.DeveloperMetadataLookup(els$lookup_location))
  expect_genned_identical(els$lookup_location)

  expect_failure(
    expect_error(
      els$lookup_visibility <- DeveloperMetadataLookup(visibility = "PROJECT")
    )
  )

  expect_true(is.DeveloperMetadataLookup(els$lookup_visibility))
  expect_genned_identical(els$lookup_visibility)

  expect_failure(
    expect_error(
      els$lookup_key <- DeveloperMetadataLookup(metadataKey = "test_meta")
    )
  )

  expect_true(is.DeveloperMetadataLookup(els$lookup_key))
  expect_genned_identical(els$lookup_key)


})

test_that("DataFilter can be created, listinized and generater", {

  expect_failure(
    expect_error(
      els$filter$loc_range <- DataFilter(
        els$lookup_location
      )
    )
  )

  expect_true(is.DataFilter(els$filter$loc_range))
  expect_genned_identical(els$filter$loc_range)

  expect_failure(
    expect_error(
      els$filter$loc_vis <- DataFilter(
        els$lookup_visibility
      )
    )
  )

  expect_true(is.DataFilter(els$filter$loc_vis))
  expect_genned_identical(els$filter$loc_vis)

  expect_failure(
    expect_error(
      els$filter$loc_key <- DataFilter(
        els$lookup_key
      )
    )
  )

  expect_true(is.DataFilter(els$filter$loc_key))
  expect_genned_identical(els$filter$loc_key)

  expect_failure(
    expect_error(
      els$filter$loc_a1 <- DataFilter(
        a1Range = "Sheet1!2:2"
      )
    )
  )

  expect_true(is.DataFilter(els$filter$loc_a1))
  expect_genned_identical(els$filter$loc_a1)

  expect_failure(
    expect_error(
      els$filter$loc_grid <- DataFilter(
        gridRange = GridRange(0, 2, 3)
      )
    )
  )

  expect_true(is.DataFilter(els$filter$loc_grid))
  expect_genned_identical(els$filter$loc_grid)


})

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

created <- request_ss_create(test_spreadsheet)
ss_id <- googledrive::as_id(created$spreadsheetId)
on.exit(googledrive::drive_trash(ss_id))

resp <- request_ss_batchUpdate(
  created$spreadsheetId,
  .dots = lapply(els$data,
                 CreateDeveloperMetadataRequest)
)

test_that("request_ss_metadata_search can be made", {

  expect_failure(
    expect_error(
      els$resp_search <- request_ss_metadata_search(
        created$spreadsheetId,
        dataFilters = els$filter)
    )
  )

  expect_equal(
    vapply(els$resp_search, is.dgs4Resp, logical(1)),
    rep(TRUE, length(els$resp_search))
  )

  expect_equal(
    length(els$resp_search),
    length(els$data)
  )

})

test_that("request_ss_metadata_get can be made", {

  metadataIds <- vapply(els$resp_search, \(x) x$developerMetadata$metadataId, numeric(1))

  expect_failure(
    expect_error(
      resps_get <- lapply(metadataIds,
                          request_ss_metadata_get,
                          spreadsheetId = created$spreadsheetId)
    )
  )

  expect_equal(
    vapply(resps_get, is.DeveloperMetadata, logical(1)),
    rep(TRUE, length(resps_get))
  )

})
