# mockup of sheetId
sheetId <- 0

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

  expect_genned_identical(gridProperties_max)
  expect_genned_identical(gridProperties_min)

})

test_that("GridCoordinate can be created, listinized and generated from list", {

  expect_failure(
    expect_error(
      gridCoordinate <- GridCoordinate(0, 1, 1)
    )
  )

  expect_s3_class(gridCoordinate, "GridCoordinate")

  expect_genned_identical(gridCoordinate,
                          sheetId)

  expect_genned_identical(gridCoordinate,
                          sheetId,
                          remove_sheetId = TRUE)

})

test_that("GridRange can be created, listinized and generated from list", {

  expect_failure(
    expect_error(
      gridRange <- GridRange(0, 1, 2, 1, 2)
    )
  )

  expect_s3_class(gridRange, "GridRange")

  expect_genned_identical(gridRange,
                          sheetId)

  expect_genned_identical(gridRange,
                          sheetId,
                          remove_sheetId = TRUE)

})
