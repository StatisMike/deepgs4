els <- new.env()

test_that("InterpolationPoint can be created, listinized and generated from list", {

  expect_failure(
    expect_error(
      els$point_min <- point_min <- InterpolationPoint(
        colorStyle = ColorStyle(themeColorType = "ACCENT3"),
        type = "MIN"
      )
    )
  )

  expect_true(is.InterpolationPoint(point_min))

  expect_failure(
    expect_error(
      els$point_mid <- point_max <- InterpolationPoint(
        colorStyle = ColorStyle(themeColorType = "ACCENT4"),
        type = "PERCENTILE",
        value = 20
      )
    )
  )

  expect_true(is.InterpolationPoint(point_max))

  expect_genned_identical(point_min)
  expect_genned_identical(point_max)

})

test_that("BooleanRule can be created, listinized and generated from list", {

  expect_failure(
    expect_error(
      els$bool <- rule <- BooleanRule(
        BooleanCondition("NOT_BLANK"),
        format = CellFormat(backgroundColorStyle = ColorStyle(themeColorType = "ACCENT6"))
      )
    )
  )

  expect_true(is.BooleanRule(rule))

  expect_genned_identical(rule)

})

test_that("GradientRule can be created, listinized and generated from list", {

  expect_failure(
    expect_error(
      gradient_min <- GradientRule(
        els$point_min,
        InterpolationPoint(ColorStyle(themeColorType = "ACCENT6"),
                           type = "MAX")
      )
    )
  )

  expect_true(is.GradientRule(gradient_min))

  expect_failure(
    expect_error(
      els$gradient <- gradient_max <- GradientRule(
        minpoint = els$point_min,
        midpoint = els$point_mid,
        maxpoint = InterpolationPoint(ColorStyle(themeColorType = "ACCENT6"),
                                      type = "MAX")
      )
    )
  )

  expect_true(is.GradientRule(gradient_max))

  expect_genned_identical(gradient_min)
  expect_genned_identical(gradient_max)

})

test_that("ConditionalFormatRule can be created, listinized and generated from list", {

  expect_failure(
    expect_error(
      format_gradient <- ConditionalFormatRule(
        ranges = GridRange(1, 0, 12, 0, 12),
        gradientRule = els$gradient)
    )
  )

  expect_true(is.ConditionalFormatRule(format_gradient))

  expect_failure(
    expect_error(
      format_bool <- ConditionalFormatRule(
        ranges = list(GridRange(1, 0, 6, 0, 12),
                      GridRange(1, 7, 9, 5, 10)),
        booleanRule = els$bool)
    )
  )

  expect_true(is.ConditionalFormatRule(format_bool))

  expect_genned_identical(format_gradient)
  expect_genned_identical(format_bool)

})
