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

  expect_s3_class(numberFormat_max, "NumberFormat")

  expect_failure(
    expect_error(
      numberFormat_min <- NumberFormat(
        "CURRENCY"
      )
    )
  )

  expect_s3_class(numberFormat_min, "NumberFormat")

  expect_genned_identical(numberFormat_min)
  expect_genned_identical(numberFormat_max)

})

test_that("Borders can be created, listinized and generated from list", {

  expect_failure(
    expect_error(
      els$borders <- borders_max <- Borders(
        top_style = "SOLID_MEDIUM",
        top_colorStyle = ColorStyle(0.2, 0.5, 1),
        bottom_style = "DASHED",
        bottom_colorStyle = ColorStyle(0.5, 0, 1, 0.7),
        left_style = "DOTTED",
        left_colorStyle = ColorStyle(themeColorType = "ACCENT6"),
        right_style = "DOUBLE",
        right_colorStyle = ColorStyle()
      )
    )
  )

  expect_s3_class(borders_max, "Borders")

  expect_failure(
    expect_error(
      borders_min <- Borders()
    )
  )

  expect_s3_class(borders_min, "Borders")

  expect_genned_identical(borders_max)
  expect_genned_identical(borders_min)

})

test_that("Padding can be created, listinized and generated from list", {

  expect_failure(
    expect_error(
      els$padding <- constructed <- Padding(1, 2, 1, 4)
    )
  )

  expect_s3_class(constructed, "Padding")

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

  expect_s3_class(cellFormat_max, "CellFormat")

  expect_failure(
    expect_error(
      cellFormat_min <- CellFormat(
        textRotation = "v"
      )
    )
  )

  expect_s3_class(cellFormat_min, "CellFormat")

  expect_error(
    CellFormat(),
    class = "NoArgsError"
  )

  expect_genned_identical(cellFormat_max)
  expect_genned_identical(cellFormat_min)

})

