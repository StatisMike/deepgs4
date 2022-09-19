# initialization

googlesheets4::gs4_auth(email = Sys.getenv("G_SERVICE_MAIL"),
                        path = Sys.getenv("G_SERVICE_ACCOUNT"),
                        cache = F)
googledrive::drive_auth(email = Sys.getenv("G_SERVICE_MAIL"),
                        path = Sys.getenv("G_SERVICE_ACCOUNT"),
                        cache = F)

ss_id <- googlesheets4::gs4_create()
on.exit(googledrive::drive_trash(ss_id))

gridRanges <- list()

test_that("Merge request can be constructed and sent", {

  gridRanges$columns <<- GridRange(
    sheetId = 0,
    startRowIndex = 0,
    endRowIndex = 3,
    startColumnIndex = 0,
    endColumnIndex = 3
  )

  gridRanges$rows <<- GridRange(
    sheetId = 0,
    startRowIndex = 5,
    endRowIndex = 9,
    startColumnIndex = 0,
    endColumnIndex = 3
  )

  gridRanges$all <<- GridRange(
    sheetId = 0,
    startRowIndex = 0,
    endRowIndex = 3,
    startColumnIndex = 5,
    endColumnIndex = 9
  )

  invisible(lapply(gridRanges, expect_s3_class, class =  "GridRange"))

  expect_failure(
    expect_error(
      requests <- list(
        columns = MergeCellsRequest(
          gridRange = gridRanges$columns,
          mergeType = "MERGE_COLUMNS"
        ),
        rows = MergeCellsRequest(
          gridRange = gridRanges$rows,
          mergeType = "MERGE_ROWS"
        ),
        all = MergeCellsRequest(
          gridRange = gridRanges$all,
          mergeType = "MERGE_ALL"
        )
      )
    )
  )

  invisible(lapply(requests, expect_s3_class, class = "deepgsheetsRequest"))

  expect_failure(
    expect_error(
      resp <- send_batchUpdate_req(
        spreadsheetId = ss_id,
        .dots = requests
      )
    )
  )

})

test_that("Created merges can be acquired from spreadsheet", {

  spreadSheetData <- SpreadSheetData$new(ss_id)

  merges <- spreadSheetData$get_data("merges")$merges[['0']]

  expect_true(all(vapply(merges, is.GridRange, logical(1))))

  columns_verification <- sapply(
    merges, \(merge) {

      merge$sheetId == gridRanges$columns$sheetId &&
        merge$startRowIndex == gridRanges$columns$startRowIndex &&
        merge$endRowIndex == gridRanges$columns$endRowIndex &&
        merge$startColumnIndex >= gridRanges$columns$startColumnIndex &&
        merge$endColumnIndex <= gridRanges$columns$endColumnIndex

    }
  )

  expect_equal(sum(columns_verification),
               gridRanges$columns$endColumnIndex - gridRanges$columns$startColumnIndex)

  rows_verification <- sapply(
    merges, \(merge) {

      merge$sheetId == gridRanges$rows$sheetId &&
        merge$startRowIndex >= gridRanges$rows$startRowIndex &&
        merge$endRowIndex <= gridRanges$rows$endRowIndex &&
        merge$startColumnIndex == gridRanges$rows$startColumnIndex &&
        merge$endColumnIndex == gridRanges$rows$endColumnIndex

    }
  )

  expect_equal(sum(rows_verification),
               gridRanges$rows$endRowIndex - gridRanges$rows$startRowIndex)

  all_verification <- sapply(
    merges, \(merge) {

      merge$sheetId == gridRanges$all$sheetId &&
        merge$startRowIndex == gridRanges$all$startRowIndex &&
        merge$endRowIndex == gridRanges$all$endRowIndex &&
        merge$startColumnIndex == gridRanges$all$startColumnIndex &&
        merge$endColumnIndex == gridRanges$all$endColumnIndex

    }
  )

  expect_equal(sum(all_verification), 1)

  expect_equal(
    sum(gridRanges$columns$endColumnIndex - gridRanges$columns$startColumnIndex,
        gridRanges$rows$endRowIndex - gridRanges$rows$startRowIndex,
        1),
    length(merges)
  )
})
