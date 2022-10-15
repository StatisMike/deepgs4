# mockup of sheetProperties
sheetProperties <- list(sheetId = 0)

els <- new.env()

test_that("NumberFormat can be created, listinized and generated from list", {

  expect_failure(
    expect_error(
      els$numberFormat <- numberFormat_max <- NumberFormat(
        type = "NUMBER",
        pattern = "####.#"
      )
    )
  )

  expect_true(is.NumberFormat(numberFormat_max))

  expect_failure(
    expect_error(
      numberFormat_min <- NumberFormat(
        "CURRENCY"
      )
    )
  )

  expect_true(is.NumberFormat(numberFormat_min))

  expect_genned_identical(numberFormat_min)
  expect_genned_identical(numberFormat_max)

})

test_that("Border can be created, listinized and generated from list", {

  expect_failure(
    expect_error(
      els$border1 <- border_min <- Border()
    )
  )

  expect_true(is.Border(border_min))

  expect_failure(
    expect_error(
      els$border2 <- border_max <- Border(
        "DASHED",
        ColorStyle("ACCENT1")
      )
    )
  )

  expect_true(is.Border(border_max))

  expect_genned_identical(border_min)
  expect_genned_identical(border_max)

})

test_that("Borders can be created, listinized and generated from list", {

  expect_failure(
    expect_error(
      els$borders <- borders_max <- Borders(
        top = els$border1,
        bottom = els$border2,
        left = els$border1,
        right = els$border2
      )
    )
  )

  expect_true(is.Borders(borders_max))

  expect_failure(
    expect_error(
      borders_min <- Borders(top = els$border1)
    )
  )

  expect_true(is.Borders(borders_min))

  expect_genned_identical(borders_max)
  expect_genned_identical(borders_min)

})

test_that("Padding can be created, listinized and generated from list", {

  expect_failure(
    expect_error(
      els$padding <- constructed <- Padding(1, 2, 1, 4)
    )
  )

  expect_true(is.Padding(constructed))

  expect_genned_identical(constructed)

})

test_that("CellFormat can be created, listinized and generated from list", {

  expect_failure(
    expect_error(
      cellFormat_max <- CellFormat(
        numberFormat = els$numberFormat,
        backgroundColorStyle = ColorStyle(themeColorType = "BACKGROUND"),
        borders = els$borders,
        padding = els$padding,
        horizontalAlignment = "CENTER",
        verticalAlignment = "MIDDLE",
        wrapStrategy = "WRAP",
        textDirection = "LEFT_TO_RIGHT",
        textRotation = 40,
        hyperlinkDisplayType = "LINKED"
      )
    )
  )

  expect_true(is.CellFormat(cellFormat_max))

  expect_failure(
    expect_error(
      cellFormat_min <- CellFormat(
        textRotation = "v"
      )
    )
  )

  expect_true(is.CellFormat(cellFormat_min))

  expect_error(
    CellFormat(),
    class = "NoArgsError"
  )

  expect_genned_identical(cellFormat_max)
  expect_genned_identical(cellFormat_min)

})

test_that("ErrorValue can be created, listinized and generated from list", {

  expect_failure(
    expect_error(
      errorValue <- ErrorValue("DIVIDE_BY_ZERO",
                               "You cannot divide by zero!")
    )
  )

  expect_true(is.ErrorValue(errorValue))

  expect_genned_identical(errorValue)

})

test_that("ExtendedValue can be created, listinized and generated from list", {

  expect_failure(
    expect_error(
      number <- ExtendedValue(numberValue = 12)
    )
  )
  expect_true(is.ExtendedValue(number))
  expect_true(attr(number, "type") == "number")
  expect_genned_identical(number)

  expect_failure(
    expect_error(
      date_as_num <- ExtendedValue(numberValue = dgs4_serial_number("2022-01-12"))
    )
  )
  expect_true(is.ExtendedValue(date_as_num))
  expect_true(attr(date_as_num, "type") == "number")
  expect_genned_identical(date_as_num)

  expect_failure(
    expect_error(
      number_as_char <- ExtendedValue(stringValue = 12)
    )
  )
  expect_true(is.ExtendedValue(number_as_char))
  expect_true(attr(number_as_char, "type") == "string")
  # expect_genned_identical(number_as_char)

  expect_failure(
    expect_error(
      chars <- ExtendedValue(stringValue = "testing")
    )
  )
  expect_true(is.ExtendedValue(chars))
  expect_true(attr(chars, "type") == "string")
  expect_genned_identical(chars)

  expect_failure(
    expect_error(
      logic <- ExtendedValue(boolValue = TRUE)
    )
  )
  expect_true(is.ExtendedValue(logic))
  expect_true(attr(logic, "type") == "bool")
  expect_genned_identical(logic)

  expect_failure(
    expect_error(
      form <- ExtendedValue(formulaValue = "=A2*15")
    )
  )
  expect_true(is.ExtendedValue(form))
  expect_true(attr(form, "type") == "formula")
  expect_genned_identical(form)

  expect_failure(
    expect_error(
      error <- ExtendedValue(errorValue = ErrorValue("ERROR", message = "test_error"))
    )
  )
  expect_true(is.ExtendedValue(error))
  expect_true(attr(error, "type") == "error")
  expect_genned_identical(error)

})

test_that("CellData can be created, listinized and generated from list", {

  expect_failure(
    expect_error(
      els$first_cellData <- max_viable_cellData <- CellData(
        userEnteredValue = ExtendedValue(stringValue = "Testing First"),
        userEnteredFormat = CellFormat(wrapStrategy = "OVERFLOW_CELL"),
        note = "This is something I am testing",
        effectiveValue = ExtendedValue(stringValue = "Testing Second"),
        formattedValue = "Testing Second",
        effectiveFormat = CellFormat(wrapStrategy = "WRAP"),
        hyperlink = "whatever.com"
      )
    )
  )

  expect_true(is.CellData(max_viable_cellData))
  expect_genned_identical(max_viable_cellData)

  expect_failure(
    expect_error(
      els$second_cellData <- min_cellData <- CellData(
        userEnteredValue = ExtendedValue(boolValue = TRUE)
      )
    )
  )

  expect_true(is.CellData(min_cellData))
  expect_genned_identical(min_cellData)

})

test_that("RowData can be created, listinized and generated from list", {

  expect_failure(
    expect_error(
      els$rowData <- rowData <- RowData(
        values = list(els$first_cellData,
                      els$second_cellData)
      )
    )
  )

  expect_true(is.RowData(rowData))
  expect_genned_identical(rowData)

})

test_that("GridData can be created, listinized and generated from list", {

  expect_failure(
    expect_error(
      gridData_min <- GridData(
        startRow = 1,
        startColumn = 1,
        rowData = list(els$rowData, els$rowData)
      )
    )
  )

  expect_true(is.GridData(gridData_min))
  expect_genned_identical(gridData_min)

})


