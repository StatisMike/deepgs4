values <- list(
  list("Test header"),
  list("String", 1),
  list(1, "String"),
  list("This", "is", "wider", "much")
)

test_that("ValueRange can be created, listinized and generated from list", {

  expect_failure(
    expect_error(
      valueRange <- ValueRange("Test!A1:D4",
                               values = values,
                               majorDimension = "ROWS")
    )
  )

  # expect_true(is.ValueRange(valueRange))

  expect_genned_identical(valueRange)

})

test_that("UpdateValuesResponse can be created, listinized and generated from list", {

  expect_failure(
    expect_error(
      response_min <- UpdateValuesResponse(
        1, "Test!A1:D4", 4, 4, 9)
    )
  )

  # expect_true(is.ValueRange(valueRange))

  expect_genned_identical(response_min)

  expect_failure(
    expect_error(
      response_max <- UpdateValuesResponse(
        1, "Test!A1:D4", 4, 4, 9,
        updatedData = ValueRange("Test!A1:D4",
                                 values = values,
                                 majorDimension = "ROWS"))
    )
  )

  # expect_true(is.ValueRange(valueRange))

  expect_genned_identical(response_max)

})
