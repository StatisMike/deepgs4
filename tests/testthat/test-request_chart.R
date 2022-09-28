googlesheets4::gs4_auth(email = Sys.getenv("G_SERVICE_MAIL"),
                        path = Sys.getenv("G_SERVICE_ACCOUNT"),
                        cache = F)
googledrive::drive_auth(email = Sys.getenv("G_SERVICE_MAIL"),
                        path = Sys.getenv("G_SERVICE_ACCOUNT"),
                        cache = F)

# for now using googlsheets4
ss_id <- googlesheets4::gs4_create()
on.exit(googledrive::drive_trash(ss_id))
googledrive::drive_share(ss_id,
                         role = "writer",
                         type = "user",
                         "emailAddress" = "statismike@gmail.com")

googlesheets4::write_sheet(
  data = cars,
  ss = ss_id
)

ss_data <- SpreadSheetData$new(ss_id)

chartReqs <- list()

chartSpec <- ChartSpec(
  chart = BasicChartSpec(
    chartType = "COLUMN",
    axis = list(BasicChartAxis(title = "Distance",
                               position = "LEFT_AXIS"),
                BasicChartAxis(title = "Speed",
                               position = "BOTTOM_AXIS")),
    domains = BasicChartDomain(domain = ChartData(
      GridRange(sheetId = ss_data$sheets[["cars"]],
                startRowIndex = 1,
                endRowIndex = 52,
                startColumnIndex = 0,
                endColumnIndex = 1))),
    series = BasicChartSeries(series = ChartData(
      GridRange(sheetId = ss_data$sheets[["cars"]],
                startRowIndex = 1,
                endRowIndex = 52,
                startColumnIndex = 1,
                endColumnIndex = 2)),
      targetAxis = "LEFT_AXIS")
  )
)

test_that("Add chart request can be constructed", {

  expect_failure(
    expect_error(
      chartReqs$embedded <<- AddChartRequest(
        chart = EmbeddedChart(
          spec = chartSpec,
          borderColor = ColorStyle(
            red = 0.2,
            blue = 0.3,
            green = 0.2
          ),
          chartId = 2137,
          position = EmbeddedObjectPosition(
            overlayPosition = OverlayPosition(
              anchorCell = GridCoordinate(sheetId = ss_data$sheets["Sheet1"],
                                          rowIndex = 1,
                                          columnIndex = 1)
            )
          )
        )
      )
    )
  )

  expect_failure(
    expect_error(
      chartReqs$specified_sheet <<- AddChartRequest(
        chart = EmbeddedChart(
          spec = chartSpec,
          borderColor = ColorStyle(
            red = 0.2,
            blue = 0.3,
            green = 0.2
          ),
          chartId = 2138,
          position = EmbeddedObjectPosition(
            sheetId = 1337
          )
        )
      )
    )
  )

  expect_failure(
    expect_error(
      chartReqs$new_sheet <<- AddChartRequest(
        chart = EmbeddedChart(
          spec = chartSpec,
          borderColor = ColorStyle(
            red = 0.2,
            blue = 0.3,
            green = 0.2
          ),
          chartId = 2139,
          position = EmbeddedObjectPosition(
            newSheet = TRUE
          )
        )
      )
    )
  )




})
