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
        hideGridlines = TRUE,
        rowGroupControlAfter = TRUE,
        columnGroupControlAfter = TRUE)
    )
  )

  expect_true(is.GridProperties(gridProperties_max))

  expect_failure(
    expect_error(
      gridProperties_min <- GridProperties(
        rowCount = 20,
        columnCount = 10)
    )
  )

  expect_true(is.GridProperties(gridProperties_min))

  expect_genned_identical(gridProperties_max)
  expect_genned_identical(gridProperties_min)

})

test_that("GridCoordinate can be created, listinized and generated from list", {

  expect_failure(
    expect_error(
      gridCoordinate <- GridCoordinate(0, 1, 1)
    )
  )

  expect_true(is.GridCoordinate(gridCoordinate))

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

  expect_true(is.GridRange(gridRange))

  expect_genned_identical(gridRange,
                          sheetId)

  expect_genned_identical(gridRange,
                          sheetId,
                          remove_sheetId = TRUE)

})

test_that("GridRange can be splitted correctly", {

  to_split <- GridRange(0, 0, 5, 0, 6)

  expect_failure(
    expect_error(
      splitted_by_col <- split_GridRange(to_split, split = "col")
    )
  )

  expect_identical(length(splitted_by_col), 6L)

  expect_failure(
    expect_error(
      splitted_by_row <- split_GridRange(to_split, split = "row")
    )
  )

  expect_identical(length(splitted_by_row), 5L)

})
