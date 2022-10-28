# initialization

dgs4_auth(path = Sys.getenv("G_SERVICE_ACCOUNT"),
            cache = F)
googledrive::drive_auth(path = Sys.getenv("G_SERVICE_ACCOUNT"),
                        cache = F)

spreadsheet <- request_ss_create(
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

ss_id <- spreadsheet$spreadsheetId

on.exit(googledrive::drive_trash(googledrive::as_id(spreadsheet$spreadsheetUrl)))

gridRanges <- new.env()

test_that("Merge request can be constructed and sent", {

  gridRanges$columns <- GridRange(
    sheetId = 0,
    startRowIndex = 0,
    endRowIndex = 3,
    startColumnIndex = 0,
    endColumnIndex = 3
  )

  gridRanges$rows <- GridRange(
    sheetId = 0,
    startRowIndex = 5,
    endRowIndex = 9,
    startColumnIndex = 0,
    endColumnIndex = 3
  )

  gridRanges$all <- GridRange(
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
          range = gridRanges$columns,
          mergeType = "MERGE_COLUMNS"
        ),
        rows = MergeCellsRequest(
          range = gridRanges$rows,
          mergeType = "MERGE_ROWS"
        ),
        all = MergeCellsRequest(
          range = gridRanges$all,
          mergeType = "MERGE_ALL"
        )
      )
    )
  )

  invisible(lapply(requests, expect_s3_class, class = "dgs4Req"))

  expect_failure(
    expect_error(
      resp <- request_ss_batchUpdate(
        spreadsheetId = ss_id,
        .dots = requests
      )
    )
  )

})

test_that("Created merges can be acquired from spreadsheet", {

  confirm <- request_ss_get(ss_id,
                            fields = "sheets.merges",
                            ranges = "Test_Sheet")

  # spreadSheetData <- SpreadSheetData$new(ss_id)

  merges <- confirm$sheets[[1]]$merges

  columns_verification <- sapply(
    merges, \(merge) {

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

test_that("Created merges can be unmerged with UnmergeCellRequest", {

  row_indices <- vapply(gridRanges,
                        \(x) c(x$startRowIndex,
                               x$endRowIndex),
                        numeric(2)) |> unlist()

  col_indices <- vapply(gridRanges,
                        \(x) c(x$startColumnIndex,
                               x$endColumnIndex),
                        numeric(2)) |> unlist()

  req <- UnmergeCellsRequest(GridRange(sheetId = 0,
                                       startRowIndex = min(row_indices),
                                       endRowIndex = max(row_indices),
                                       startColumnIndex = min(col_indices),
                                       endColumnIndex = max(col_indices)))

  expect_true(is.dgs4Req(req))

  expect_failure(
    expect_error(
      resp <- request_ss_batchUpdate(
        spreadsheetId = ss_id,
        req
      )
    )
  )

  confirm <- request_ss_get(ss_id,
                            fields = "sheets.merges",
                            ranges = "Test_Sheet")

  expect_true(is.null(confirm$sheets[[1]]$merges))

})
