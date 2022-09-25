# mockup of sheetProperties
sheetProperties <- list(sheetId = 0)

els <- list(numberFormat = NULL,
            borders = NULL,
            padding = NULL)

test_that("NumberFormat can be created, listinized and generated from list", {

  expect_failure(
    expect_error(
      els$numberFormat <<- numberFormat_max <- NumberFormat(
        type = "NUMBER",
        pattern = "####.#"
      )
    )
  )

  expect_failure(
    expect_error(
      numberFormat_min <- NumberFormat(
        "CURRENCY"
      )
    )
  )

  for (constructed in list(numberFormat_min, numberFormat_max)) {

    expect_s3_class(constructed, "NumberFormat")

    expect_failure(
      expect_error(
        listinized <- deepgs_listinize(constructed)
      )
    )

    expect_failure(
      expect_error(
        genned <- deepgsheets4:::gen_NumberFormat(
          obj = listinized
        )
      )
    )

    expect_true(identical(constructed, genned))

  }

})

test_that("Borders can be created, listinized and generated from list", {

  expect_failure(
    expect_error(
      els$borders <<- borders_max <- Borders(
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

  expect_failure(
    expect_error(
      borders_min <- Borders()
    )
  )

  for (constructed in list(borders_max, borders_min)) {

    expect_s3_class(constructed, "Borders")

    expect_failure(
      expect_error(
        listinized <- deepgs_listinize(constructed)
      )
    )

    expect_failure(
      expect_error(
        genned <- deepgsheets4:::gen_Borders(
          obj = listinized
        )
      )
    )

    expect_true(identical(constructed, genned))

  }

})

test_that("Padding can be created, listinized and generated from list", {

  expect_failure(
    expect_error(
      els$padding <<- constructed <- Padding(1, 2, 1, 4)
    )
  )

  expect_s3_class(constructed, "Padding")

  expect_failure(
    expect_error(
      listinized <- deepgs_listinize(constructed)
    )
  )

  expect_failure(
    expect_error(
      genned <- deepgsheets4:::gen_Padding(
        obj = listinized
      )
    )
  )

  expect_true(identical(constructed, genned))


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


  expect_failure(
    expect_error(
      cellFormat_min <- CellFormat(
        textRotation = "v"
      )
    )
  )

  expect_error(
    CellFormat(),
    class = "NoArgsError"
  )

  for (constructed in list(cellFormat_min, cellFormat_max)) {

    expect_s3_class(constructed, "CellFormat")

    expect_failure(
      expect_error(
        listinized <- deepgs_listinize(constructed)
      )
    )

    expect_failure(
      expect_error(
        genned <- deepgsheets4:::gen_CellFormat(
          obj = listinized
        )
      )
    )

    expect_true(identical(constructed, genned))

  }

})

