test_that("ColorStyle can be created, listinized and generated from list", {

  expect_failure(
    expect_error(
      colorStyle_rgb <- ColorStyle(
        red = 0.3,
        green = 0.4,
        blue = 0.5)
    )
  )

  expect_failure(
    expect_error(
      colorStyle_rgba <- ColorStyle(
        red = 0.1,
        green = 0.2,
        blue = 0.8,
        alpha = 0.8)
    )
  )

  expect_failure(
    expect_error(
      colorStyle_theme <- ColorStyle(
        themeColorType = "ACCENT1"
      )
    )
  )

  for (constructed in list(colorStyle_rgb, colorStyle_rgba, colorStyle_theme)) {

    expect_s3_class(constructed, "ColorStyle")

    expect_failure(
      expect_error(
        listinized <- dgs4_listinize(constructed)
      )
    )

    expect_failure(
      expect_error(
        genned <- gen_ColorStyle(
          obj = listinized
        )
      )
    )

    expect_true(identical(constructed, genned))

  }

})

test_that("TextFormat can be created, listinized and generated from list", {

  expect_failure(
    expect_error(
      textFormat_max <- TextFormat(
        foregroundColorStyle = ColorStyle(0.1, 0.2, 0.3, 0.6),
        fontFamily = "arial",
        fontSize = 15,
        bold = T,
        italic = T,
        strikethrough = T,
        link = "https://github.com/StatisMike/deepgsheets4")
    )
  )

  expect_failure(
    expect_error(
      textFormat_min <- TextFormat(
        foregroundColorStyle = ColorStyle(themeColorType = "ACCENT2")
      )
    )
  )

  expect_error(
    TextFormat(), class = "NoArgsError"
  )

  for (constructed in list(textFormat_max, textFormat_min)) {

    expect_s3_class(constructed, "TextFormat")

    expect_failure(
      expect_error(
        listinized <- dgs4_listinize(constructed)
      )
    )

    expect_failure(
      expect_error(
        genned <- gen_TextFormat(
          obj = listinized
        )
      )
    )

    expect_true(identical(constructed, genned))

  }

})

test_that("LineStyle can be created, listinized and generated from list", {

  expect_failure(
    expect_error(
      lineStyle_max <- LineStyle(5, "LONG_DASHED")
    )
  )

  expect_s3_class(lineStyle_max, "LineStyle")


  expect_failure(
    expect_error(
      lineStyle_min <- LineStyle(3)
    )
  )

  expect_s3_class(lineStyle_min, "LineStyle")

  expect_genned_identical(lineStyle_max)
  expect_genned_identical(lineStyle_min)

})

test_that("PointStyle can be created, listinized and generated from list", {

  expect_failure(
    expect_error(
      pointStyle_max <- PointStyle(2.5, "STAR")
    )
  )

  expect_s3_class(pointStyle_max, "PointStyle")

  expect_failure(
    expect_error(
      pointStyle_min <- PointStyle()
    )
  )

  expect_s3_class(pointStyle_min, "PointStyle")

  expect_genned_identical(pointStyle_max)
  expect_genned_identical(pointStyle_min)

})
