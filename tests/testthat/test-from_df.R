test_df <- data.frame(dates = seq(lubridate::as_date("2022-01-01"), by = "day", length.out = 10),
                      datetimes = seq(lubridate::as_datetime("2022-05-01 21:37:43"), by = "min", length.out = 10),
                      bools = vapply(seq_len(10), \(i) sample(c(TRUE,FALSE), 1, T), logical(1)),
                      numbers = sample(1:100, 10),
                      chars = vapply(seq_len(10), \(i) paste(sample(LETTERS, 10, replace = T), collapse = ""), character(1)),
                      formulas = "=RAND()")

test_that("RowData can be created from data.frame", {

  # regular with guess ####
  expect_failure(
    expect_error(
      normal_guess <- to_RowData_from_df(test_df)
    )
  )

  expect_true(all(sapply(normal_guess, is.RowData)))

  # first row: names as string
  expect_true(all(sapply(normal_guess[[1]], \(x) attr(x$userEnteredValue, "type") == "string")))

  # every other row:
  for (row in 2:length(normal_guess)) {
    # types identical
    expect_identical(sapply(normal_guess[[row]], \(x) attr(x$userEnteredValue, "type")),
                     c("number", "number", "bool", "number", "string", "string"))
    # correct dates formats
    expect_identical(normal_guess[[row]][[1]]$userEnteredFormat$numberFormat$type, "DATE")
    expect_identical(normal_guess[[row]][[2]]$userEnteredFormat$numberFormat$type, "DATE_TIME")
  }

  # regular with explicit
  expect_failure(
    expect_error(
      normal_explicit <- to_RowData_from_df(test_df,
                                            values_types = c("d", "dt", "b", "n", "c", "f"))
    )
  )

  # every values row:
  for (row in 2:length(normal_explicit)) {
    # types identical
    expect_identical(sapply(normal_explicit[[row]], \(x) attr(x$userEnteredValue, "type")),
                     c("number", "number", "bool", "number", "string", "formula"))
    # correct dates formats
    expect_identical(normal_explicit[[row]][[1]]$userEnteredFormat$numberFormat$type, "DATE")
    expect_identical(normal_explicit[[row]][[2]]$userEnteredFormat$numberFormat$type, "DATE_TIME")
  }

  # transposed with guess
  expect_failure(
    expect_error(
      transposed_guess <- to_RowData_from_df(test_df,
                                             transpose = TRUE)
    )
  )

  mapply(
    grid_rows = transposed_guess,
    val_type = c("number", "number", "bool", "number", "string", "string"),
    FUN = \(grid_rows, val_type) {

      types <- sapply(grid_rows, \(x) attr(x$userEnteredValue, "type"))

      expect_identical(types, c("string", rep(val_type, length(grid_rows) - 1)))

    })

  # transposed with explicit
  expect_failure(
    expect_error(
      transposed_explicit <- to_RowData_from_df(test_df,
                                                values_types = c("dt", "dt", "b", "n", "c", "f"),
                                                transpose = TRUE)
    )
  )

  mapply(
    grid_rows = transposed_explicit,
    val_type = c("number", "number", "bool", "number", "string", "formula"),
    FUN = \(grid_rows, val_type) {

      types <- sapply(grid_rows, \(x) attr(x$userEnteredValue, "type"))

      expect_identical(types, c("string", rep(val_type, length(grid_rows) - 1)))

    })

  # retained formats with normal
  names_format <- CellFormat(borders = Borders(
    Border("SOLID_MEDIUM"), Border("SOLID_MEDIUM"), Border("SOLID"), Border("SOLID")),
                             backgroundColorStyle = ColorStyle(themeColorType = "ACCENT4"))
  values_format <- CellFormat(borders = Borders(
    Border("SOLID"), Border("SOLID"), Border("DASHED"), Border("DASHED")),
                              backgroundColorStyle = ColorStyle(themeColorType = "ACCENT1"))

  expect_failure(
    expect_error(
      normal_formatted <- to_RowData_from_df(test_df,
                                             names_format = names_format,
                                             values_format = values_format)
    )
  )

  expect_identical(unique(lapply(normal_formatted[[1]], \(x) x$userEnteredFormat))[[1]], names_format)
  expect_identical(normal_formatted[[2]][[3]]$userEnteredFormat, values_format)
  expect_identical(normal_formatted[[4]][[1]]$userEnteredFormat$borders, values_format$borders)
  expect_identical(normal_formatted[[7]][[2]]$userEnteredFormat$backgroundColorStyle, values_format$backgroundColorStyle)

})

test_that("GridData can be created from df", {

  # regular

  expect_failure(
    expect_error(
      regular <- to_GridData_from_df(df = test_df, 0, 0)
    )
  )

  expect_true(is.GridData(regular))
  expect_equal(length(regular$rowData), nrow(test_df) + 1)
  expect_equal(length(regular$rowData[[1]]), ncol(test_df))

  # transposed

  expect_failure(
    expect_error(
      transposed <- to_GridData_from_df(df = test_df, 0, 0, transpose = TRUE)
    )
  )

  expect_true(is.GridData(transposed))
  expect_equal(length(transposed$rowData), ncol(test_df))
  expect_equal(length(transposed$rowData[[1]]), nrow(test_df) + 1)


})
