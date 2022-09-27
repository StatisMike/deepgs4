# mockup of sheetProperties
sheetProperties <- list(sheetId = 0)

test_that("ChartAxisViewWindowOptions can be created, listinized and generated from list", {

  expect_failure(
    expect_error(
      chartAxisViewWindowOptions_max <- ChartAxisViewWindowOptions(
        viewWindowMode = "EXPLICIT",
        viewWindowMin = 1.5,
        viewWindowMax = 21.37)
    )
  )

  expect_s3_class(chartAxisViewWindowOptions_max, "ChartAxisViewWindowOptions")

  expect_failure(
    expect_error(
      chartAxisViewWindowOptions_min <- ChartAxisViewWindowOptions()
    )
  )

  expect_s3_class(chartAxisViewWindowOptions_min, "ChartAxisViewWindowOptions")

  expect_genned_identical(chartAxisViewWindowOptions_max)

  expect_genned_identical(chartAxisViewWindowOptions_min)

})

test_that("ChartData based on GridRange can be created, listinized and generated from list", {

  expect_failure(
    expect_error(
      chartData_max <- ChartData(
        sourceRange = list(GridRange(0, 1, 2, 1, 6),
                           GridRange(0, 2, 3, 1, 6),
                           GridRange(0, 3, 4, 1, 6))
      )
    )
  )

  expect_s3_class(chartData_max, "ChartData")

  expect_failure(
    expect_error(
      chartData_min <- ChartData(
        GridRange(0, 1, 2, 1, 6)
      )
    )
  )

  expect_s3_class(chartData_min, "ChartData")

  expect_genned_identical(chartData_max, sheetProperties)

  expect_genned_identical(chartData_min, sheetProperties)

})

test_that("DataLabel can be created, listinized and generated from list", {

  expect_failure(
    expect_error(
      dataLabel_max <- DataLabel(
        textFormat = TextFormat(ColorStyle(themeColorType = "ACCENT1")),
        placement = "INSIDE_END",
        type = "CUSTOM",
        customLabelData = ChartData(GridRange(0,4,5,1,6))
      )
    )
  )

  expect_failure(
    expect_error(
      dataLabel_min <- DataLabel()
    )
  )

  expect_s3_class(dataLabel_max, "DataLabel")
  expect_s3_class(dataLabel_min, "DataLabel")

  expect_genned_identical(dataLabel_max, sheetProperties)
  expect_genned_identical(dataLabel_min, sheetProperties)





})

test_that("ChartSpec can be created, listinized and generated from list", {

  #TODO: Add dataSouceChartProperties, filterSpecs and sortSpecs

  expect_failure(
    expect_error(
      chartSpec_max <- ChartSpec(
        chart = BasicChartSpec(
          BasicChartAxis(),
          BasicChartDomain(ChartData(GridRange(0,0,1,1,6))),
          BasicChartSeries(ChartData(GridRange(0,1,2,1,6)))
        ),
        title = "Test chart",
        titlePosition = "CENTER",
        titleTextFormat = TextFormat(fontSize = 16),
        subtitle = "Test subtitle",
        subtitlePosition = "RIGHT",
        subtitleTextFormat = TextFormat(fontSize = 13),
        fontName = "Arial",
        altText = "Test chart with all features",
        maximized = T,
        backgroundColorStyle = ColorStyle(themeColorType = "ACCENT5"),
        # dataSourceChartProperties = DataSourceChartProperties(),
        # filterSpecs = FilterSpec(),
        # sortSpecs = SortSpec(),
        hiddenDimensionStrategy = "SKIP_HIDDEN_ROWS"
      )
    )
  )

  expect_s3_class(chartSpec_max, "ChartSpec")

  expect_failure(
    expect_error(
      chartSpec_min <- ChartSpec(
        chart = BasicChartSpec(
          axis = BasicChartAxis(),
          series = BasicChartSeries(ChartData(GridRange(0,0,1,1,6))),
          domains = BasicChartDomain(ChartData(GridRange(0,1,2,1,6)))
        )
      )
    )
  )

  expect_s3_class(chartSpec_min, "ChartSpec")

  expect_genned_identical(chartSpec_max, sheetProperties)
  expect_genned_identical(chartSpec_max, sheetProperties)

  expect_genned_identical(chartSpec_min, sheetProperties)
  expect_genned_identical(chartSpec_max, sheetProperties)

})

test_that("EmbeddedChart can be created, listinized and generated from list", {

  expect_failure(
    expect_error(
      embeddedChart_max <- EmbeddedChart(
        spec = ChartSpec(
          chart = BasicChartSpec(
            axis = BasicChartAxis(),
            series = BasicChartSeries(ChartData(GridRange(0, 1, 2, 1, 6))),
            domains = BasicChartDomain(ChartData(GridRange(0, 0, 1, 1, 6)))
          )
        ),
        position = EmbeddedObjectPosition(
          OverlayPosition(GridCoordinate(0, 10, 0))
        ),
        borderColor = ColorStyle(0.1, 0.5, 1),
        chartId = 2137
      )
    )
  )

  expect_failure(
    expect_error(
      embeddedChart_min <- EmbeddedChart(
        spec = ChartSpec(
          chart = BasicChartSpec(
            axis = BasicChartAxis(),
            series = BasicChartSeries(ChartData(GridRange(0, 1, 2, 1, 6))),
            domains = BasicChartDomain(ChartData(GridRange(0, 0, 1, 1, 6)))
          )
        ),
        position = EmbeddedObjectPosition(newSheet = T)
      )
    )
  )

  expect_s3_class(embeddedChart_max, "EmbeddedChart")
  expect_s3_class(embeddedChart_min, "EmbeddedChart")

  expect_genned_identical(embeddedChart_max, sheetProperties)
  expect_genned_identical(embeddedChart_min, sheetProperties)

})

