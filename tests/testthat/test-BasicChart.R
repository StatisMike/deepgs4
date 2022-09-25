# mockup of sheetProperties
sheetProperties <- list(sheetId = 0)

test_that("BasicChartAxis can be created, listinized and generated from list", {

  expect_failure(
    expect_error(
      basicChartAxis_max <- BasicChartAxis(
        position = "RIGHT_AXIS",
        title = "Test axis",
        format = TextFormat(fontSize = 14, italic = TRUE),
        viewWindowOptions = ChartAxisViewWindowOptions(
          "EXPLICIT", viewWindowMin = 1.5, viewWindowMax = 2),
        titleTextPosition = "CENTER")
    )
  )

  expect_failure(
    expect_error(
      basicChartAxis_min <- BasicChartAxis()
    )
  )

  for (constructed in list(basicChartAxis_max, basicChartAxis_min)) {

    expect_s3_class(constructed, "BasicChartAxis")

    expect_failure(
      expect_error(
        listinized <- deepgs_listinize(constructed)
      )
    )

    expect_failure(
      expect_error(
        genned <- deepgsheets4:::gen_BasicChartAxis(
          obj = listinized
        )
      )
    )

    expect_true(identical(constructed, genned))

  }

})

test_that("BasicChartSeries can be created, listinized and generated from list", {

  expect_failure(
    expect_error(
      basicChartSeries_max <- BasicChartSeries(
        series = ChartData(sourceRange = GridRange(1, 1, 2, 1, 6)),
        dataLabel = DataLabel(),
        type = "LINE",
        lineStyle = LineStyle(3),
        colorStyle = ColorStyle(),
        pointStyle = PointStyle(),
        styleOverrides = list(
          BasicSeriesDataPointStyleOverride(1, ColorStyle(themeColorType = "ACCENT3")),
          BasicSeriesDataPointStyleOverride(4, ColorStyle(themeColorType = "ACCENT6"))
          )
      )
    )
  )

  expect_failure(
    expect_error(
      basicChartSeries_min <- BasicChartSeries(
        series = ChartData(sourceRange = GridRange(1, 1, 2, 1, 6))
      )
    )
  )

  for (constructed in list(basicChartSeries_max, basicChartSeries_min)) {

    expect_s3_class(constructed, "BasicChartSeries")

    expect_failure(
      expect_error(
        listinized <- deepgs_listinize(constructed)
      )
    )

    expect_failure(
      expect_error(
        genned <- deepgsheets4:::gen_BasicChartSeries(
          obj = listinized
        )
      )
    )

    expect_true(identical(constructed, genned))

  }

})
