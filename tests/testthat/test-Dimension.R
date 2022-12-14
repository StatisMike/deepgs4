sheetId <- 2137

test_that("DimensionProperties can be created, listinized and generated from list", {

  expect_failure(
    expect_error(
      dim_min <- DimensionProperties(
        pixelSize = 15,
        hiddenByUser = TRUE
      )
    )
  )

  expect_true(is.DimensionProperties(dim_min))

  expect_failure(
    expect_error(
      dim_max <- DimensionProperties(
        pixelSize = 13,
        hiddenByUser = TRUE,
        hiddenByFilter = TRUE,
        dataSourceColumnReference = "test_col"
      )
    )
  )

  expect_true(is.DimensionProperties(dim_max))

  expect_genned_identical(dim_min)
  expect_genned_identical(dim_max)

})

test_that("DimensionRange can be created, listinized and generated from list", {

  expect_failure(
    expect_error(
      dim_min <- DimensionRange(
        sheetId = sheetId
      )
    )
  )

  expect_true(is.DimensionRange(dim_min))

  expect_failure(
    expect_error(
      dim_max <- DimensionRange(
        sheetId = sheetId,
        2,
        5,
        "COLUMNS"
      )
    )
  )

  expect_true(is.DimensionRange(dim_max))

  expect_genned_identical(dim_min)
  expect_genned_identical(dim_min, sheetId = sheetId, remove_sheetId = TRUE)
  expect_genned_identical(dim_max)
  expect_genned_identical(dim_max, sheetId = sheetId, remove_sheetId = TRUE)

})

test_that("DimensionGroup can be created, listinized and generated from list", {

  expect_failure(
    expect_error(
      dim_min <- DimensionGroup(
        range = DimensionRange(sheetId, 1, 4)
      )
    )
  )

  expect_true(is.DimensionGroup(dim_min))

  expect_failure(
    expect_error(
      dim_max <- DimensionGroup(
        range = DimensionRange(sheetId, 1, 4),
        collapsed = TRUE,
        depth = 2
      )
    )
  )

  expect_true(is.DimensionGroup(dim_max))

  expect_genned_identical(dim_min)
  expect_genned_identical(dim_max)

})


