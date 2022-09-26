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
          obj = listinized,
          sheetProperties = sheetProperties
        )
      )
    )

    expect_true(identical(constructed, genned))

  }

})

test_that("BasicChartDomain can be created, listinized and generated from list", {

  expect_failure(
    expect_error(
      basicChartDomain_max <- BasicChartDomain(
        domains = ChartData(GridRange(0, 3, 4, 1, 6)),
        reversed = TRUE)
    )
  )

  expect_failure(
    expect_error(
      basicChartDomain_min <- BasicChartDomain(
        ChartData(GridRange(0, 3, 4, 1, 6))
      )
    )
  )

  for (constructed in list(basicChartDomain_max, basicChartDomain_min)) {

    expect_s3_class(constructed, "BasicChartDomain")

    expect_failure(
      expect_error(
        listinized <- deepgs_listinize(constructed)
      )
    )

    expect_failure(
      expect_error(
        genned <- deepgsheets4:::gen_BasicChartDomain(
          obj = listinized,
          sheetProperties = sheetProperties
        )
      )
    )

    expect_true(identical(constructed, genned))

  }

})

test_that("BasicChartSpec can be created, listinized and generated from list", {

  expect_failure(
    expect_error(
      basicChartSpec_max <- BasicChartSpec(
        axis = list(BasicChartAxis("LEFT_AXIS", "Line"),
                    BasicChartAxis("RIGHT_AXIS", "Column")),
        series = list(BasicChartSeries(ChartData(GridRange(0, 1, 2, 1, 6)),
                                       type = "LINE",
                                       targetAxis = "LEFT_AXIS"),
                      BasicChartSeries(ChartData(GridRange(0, 2, 3, 1, 6)),
                                       type = "COLUMN",
                                       targetAxis = "RIGHT_AXIS")),
        domains = list(BasicChartDomain(ChartData(GridRange(0, 0, 1, 1, 6)))),
        chartType = "COMBO",
        legendPosition = "RIGHT_LEGEND",
        headerCount = 0,
        threeDimensional = FALSE,
        interpolateNulls = TRUE,
        stackedType = "STACKED",
        lineSmoothing = TRUE,
        compareMode = "CATEGORY",
        totalDataLabel = DataLabel(
          textFormat = TextFormat(
            foregroundColorStyle = ColorStyle(
              themeColorType = "ACCENT1")))
      )
    )
  )

  expect_failure(
    expect_error(
      basicChartSpec_min <- BasicChartSpec(
        axis = BasicChartAxis("LEFT_AXIS"),
        series = BasicChartSeries(ChartData(GridRange(0, 1, 2, 1, 6))),
        domains = BasicChartDomain(ChartData(GridRange(0, 0, 1, 1, 6)))
      )
    )
  )

  for (constructed in list(basicChartSpec_max, basicChartSpec_min)) {

    expect_s3_class(constructed, "BasicChartSpec")

    expect_failure(
      expect_error(
        listinized <- deepgs_listinize(constructed)
      )
    )

    expect_failure(
      expect_error(
        genned <- deepgsheets4:::gen_BasicChartSpec(
          obj = listinized,
          sheetProperties = sheetProperties
        )
      )
    )

    expect_true(identical(constructed, genned))

  }

})

