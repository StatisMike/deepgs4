# mockup of sheetProperties
sheetProperties <- list(sheetId = 0)

test_that("GridProperties can be created, listinized and generated from list", {

  expect_failure(
    expect_error(
      gridProperties_max <- GridProperties(
        rowCount = 20,
        columnCount = 10,
        frozenRowCount = 2,
        frozenColumnCount = 1,
        hideGridLines = TRUE,
        rowGroupControlAfter = TRUE,
        columnGroupControlAfter = TRUE)
    )
  )

  expect_failure(
    expect_error(
      gridProperties_min <- GridProperties(
        rowCount = 20,
        columnCount = 10)
    )
  )

  for (gridProperties in list(gridProperties_max, gridProperties_min)) {

    expect_s3_class(gridProperties, "GridProperties")

    expect_failure(
      expect_error(
        listinized <- deepgs_listinize(gridProperties)
      )
    )

    expect_failure(
      expect_error(
        genned <- deepgsheets4:::gen_GridProperties(
          obj = listinized
        )
      )
    )

    expect_true(identical(gridProperties, genned))

  }

})

test_that("GridCoordinate can be created, listinized and generated from list", {

  expect_failure(
    expect_error(
      gridCoordinate <- GridCoordinate(0, 1, 1)
    )
  )

  expect_s3_class(gridCoordinate, "GridCoordinate")

  expect_failure(
    expect_error(
      listinized <- deepgs_listinize(gridCoordinate)
    )
  )

  expect_failure(
    expect_error(
      genned <- deepgsheets4:::gen_GridCoordinate(obj = listinized)
    )
  )

  expect_identical(gridCoordinate, genned)

  # mockup object returned from Sheets API (withoud sheetId)
  listinized <- listinized[!grepl(names(listinized), pattern = "^sheetId$")]

  expect_failure(
    expect_error(
      genned <- deepgsheets4:::gen_GridCoordinate(obj = listinized,
                                                  sheetProperties = sheetProperties)
    )
  )

  expect_identical(gridCoordinate, genned)


})

test_that("GridRange can be created, listinized and generated from list", {

  expect_failure(
    expect_error(
      gridRange <- GridRange(0, 1, 2, 1, 2)
    )
  )

  expect_s3_class(gridRange, "GridRange")

  expect_failure(
    expect_error(
      listinized <- deepgs_listinize(gridRange)
    )
  )

  expect_failure(
    expect_error(
      genned <- deepgsheets4:::gen_GridRange(obj = listinized)
    )
  )

  expect_identical(gridRange, genned)

  # mockup object returned from Sheets API (withoud sheetId)
  listinized <- listinized[!grepl(names(listinized), pattern = "^sheetId$")]

  expect_failure(
    expect_error(
      genned <- deepgsheets4:::gen_GridRange(obj = listinized,
                                             sheetProperties = sheetProperties)
    )
  )

  expect_identical(gridRange, genned)


})
