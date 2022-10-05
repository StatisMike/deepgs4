# mockup of sheetProperties
sheetId <- 0

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

  expect_true(is.BasicChartAxis(basicChartAxis_max))

  expect_failure(
    expect_error(
      basicChartAxis_min <- BasicChartAxis()
    )
  )

  expect_true(is.BasicChartAxis(basicChartAxis_min))

  expect_genned_identical(basicChartAxis_min)
  expect_genned_identical(basicChartAxis_max)

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
          BasicSeriesDataPointStyleOverride(1,
                                            colorStyle = ColorStyle(themeColorType = "ACCENT3"),
                                            pointStyle = PointStyle(2.5, "X_MARK")),
          BasicSeriesDataPointStyleOverride(4, ColorStyle(themeColorType = "ACCENT6"))
        )
      )
    )
  )

  expect_true(is.BasicChartSeries(basicChartSeries_max))

  expect_failure(
    expect_error(
      basicChartSeries_min <- BasicChartSeries(
        series = ChartData(sourceRange = GridRange(1, 1, 2, 1, 6))
      )
    )
  )

  expect_true(is.BasicChartSeries(basicChartSeries_min))

  expect_genned_identical(basicChartSeries_min, sheetId = sheetId)
  expect_genned_identical(basicChartSeries_max, sheetId = sheetId)

})

test_that("BasicChartDomain can be created, listinized and generated from list", {

  expect_failure(
    expect_error(
      basicChartDomain_max <- BasicChartDomain(
        domain = ChartData(GridRange(0, 3, 4, 1, 6)),
        reversed = TRUE)
    )
  )

  expect_true(is.BasicChartDomain(basicChartDomain_max))

  expect_failure(
    expect_error(
      basicChartDomain_min <- BasicChartDomain(
        ChartData(GridRange(0, 3, 4, 1, 6))
      )
    )
  )

  expect_true(is.BasicChartDomain(basicChartDomain_min))

  expect_genned_identical(basicChartDomain_min, sheetId = sheetId)
  expect_genned_identical(basicChartDomain_max, sheetId = sheetId)

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

  expect_true(is.BasicChartSpec(basicChartSpec_max))

  expect_failure(
    expect_error(
      basicChartSpec_min <- BasicChartSpec(
        axis = BasicChartAxis("LEFT_AXIS"),
        series = BasicChartSeries(ChartData(GridRange(0, 1, 2, 1, 6))),
        domains = BasicChartDomain(ChartData(GridRange(0, 0, 1, 1, 6)))
      )
    )
  )

  expect_true(is.BasicChartSpec(basicChartSpec_min))

  expect_genned_identical(basicChartSpec_min, sheetId = sheetId)
  expect_genned_identical(basicChartSpec_max, sheetId = sheetId)

})

