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
    axis = list(BasicChartAxis(title = "Speed",
                               position = "BOTTOM_AXIS"),
                BasicChartAxis(title = "Distance",
                               position = "LEFT_AXIS")),
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

test_that("AddChart Request can be send, and its response data received", {

  expect_failure(
    expect_error(
      resp <- send_batchUpdate_req(
        ss_id,
        .dots = chartReqs
      )
    )
  )

  embedded <- compare_objects(
    obj1 = chartReqs$embedded$addChart,
    obj2 = resp$replies[[1]]$addChart,
    skip_compare = c("red", "green", "blue", "endRowIndex"),
    na.rm = TRUE
  )

  expect_true(sum(embedded) == length(embedded))

  specified_sheet <- compare_objects(
    obj1 = chartReqs$specified_sheet$addChart,
    obj2 = resp$replies[[2]]$addChart,
    skip_compare = c("red", "green", "blue", "endRowIndex"),
    na.rm = TRUE
  )

  expect_true(sum(specified_sheet) == length(specified_sheet))

  new_sheet <- compare_objects(
    obj1 = chartReqs$new_sheet$addChart,
    obj2 = resp$replies[[3]]$addChart,
    skip_compare = c("red", "green", "blue", "newSheet", "endRowIndex"),
    na.rm = TRUE
  )

  expect_true(sum(new_sheet) == length(new_sheet))

})

test_that("UpdateChartSpecRequest can be constructed, send and reply received", {

  modified_chart_req <- chartReqs$new_sheet$addChart$chart$spec
  modified_chart_req$basicChart$chartType <- "LINE"

  expect_failure(
    expect_error(
      req <- UpdateChartSpecRequest(
        chartId = chartReqs$new_sheet$addChart$chart$chartId,
        spec = modified_chart_req
      )
    )
  )

  expect_failure(
    expect_error(
      resp <- send_batchUpdate_req(
        spreadsheetId = ss_id,
        req
      )
    )
  )


})


