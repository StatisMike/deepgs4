googledrive::drive_auth(email = Sys.getenv("G_SERVICE_MAIL"),
                        path = Sys.getenv("G_SERVICE_ACCOUNT"),
                        cache = F)
deepgs_auth(email = Sys.getenv("G_SERVICE_MAIL"),
            path = Sys.getenv("G_SERVICE_ACCOUNT"),
            cache = F)

# for now using googlsheets4
cars_spreadsheet <- Spreadsheet(
  properties = SpreadsheetProperties(
    title = "Cars Spreadsheet"
  ),
  sheets = list(
    Sheet(
      properties = SheetProperties(
        sheetId = 0,
        title = "Embedded chart test",
        gridProperties = GridProperties(30, 30)
      )),
    Sheet(
      properties = SheetProperties(
        sheetId = 1,
        title = "Cars dataset",
        gridProperties = GridProperties(51, 2)
      ),
      data = to_GridData_from_df(cars, 0, 0))
  )
)
created <- send_create_req(cars_spreadsheet)
ss_id <- googledrive::as_id(created$spreadsheetId)
on.exit(googledrive::drive_trash(ss_id))
# googledrive::drive_share(ss_id,
#                          role = "writer",
#                          type = "user",
#                          "emailAddress" = "statismike@gmail.com")

chartReqs <- list()

chartSpec <- ChartSpec(
  chart = BasicChartSpec(
    chartType = "COLUMN",
    axis = list(BasicChartAxis(title = "Speed",
                               position = "BOTTOM_AXIS"),
                BasicChartAxis(title = "Distance",
                               position = "LEFT_AXIS")),
    domains = BasicChartDomain(domain = ChartData(
      GridRange(sheetId = 1,
                startRowIndex = 1,
                endRowIndex = 52,
                startColumnIndex = 0,
                endColumnIndex = 1))),
    series = BasicChartSeries(series = ChartData(
      GridRange(sheetId = 1,
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
              anchorCell = GridCoordinate(1,
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


