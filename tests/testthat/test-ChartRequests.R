googledrive::drive_auth(path = Sys.getenv("G_SERVICE_ACCOUNT"),
                        cache = F)
dgs4_auth(path = Sys.getenv("G_SERVICE_ACCOUNT"),
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
created <- request_ss_create(cars_spreadsheet)
ss_id <- googledrive::as_id(created$spreadsheetId)
on.exit(googledrive::drive_trash(ss_id))
# googledrive::drive_share(ss_id,
#                          role = "writer",
#                          type = "user",
#                          "emailAddress" = "statismike@gmail.com")

els <- new.env()
els$els$chartReqs <- list()

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
  ),
  title = "Test chart",
  titleTextPosition = "CENTER"
)

test_that("Add chart request can be constructed", {

  expect_failure(
    expect_error(
      els$chartReqs$embedded <- AddChartRequest(
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
              anchorCell = GridCoordinate(0,
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
      els$chartReqs$specified_sheet <- AddChartRequest(
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
      els$chartReqs$new_sheet <- AddChartRequest(
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
      resp <- request_ss_batchUpdate(
        ss_id,
        .dots = els$chartReqs
      )
    )
  )

  embedded <- compare_objects(
    obj1 = els$chartReqs$embedded,
    obj2 = resp$replies[[1]],
    skip_compare = c("red", "green", "blue", "endRowIndex"),
    na.rm = TRUE
  )

  expect_true(sum(embedded) == length(embedded))

  specified_sheet <- compare_objects(
    obj1 = els$chartReqs$specified_sheet,
    obj2 = resp$replies[[2]],
    skip_compare = c("red", "green", "blue", "endRowIndex"),
    na.rm = TRUE
  )

  expect_true(sum(specified_sheet) == length(specified_sheet))

  new_sheet <- compare_objects(
    obj1 = els$chartReqs$new_sheet,
    obj2 = resp$replies[[3]],
    skip_compare = c("red", "green", "blue", "newSheet", "endRowIndex"),
    na.rm = TRUE
  )

  expect_true(sum(new_sheet) == length(new_sheet))

})

test_that("UpdateChartSpecRequest can be constructed, send and reply received", {

  modified_chart_req <- els$chartReqs$new_sheet$chart$spec
  modified_chart_req$basicChart$chartType <- "LINE"
  # can't still add title to right axis :<
  # modified_chart_req$basicChart$axis[[2]]$position <- "RIGHT_AXIS"

  expect_failure(
    expect_error(
      req <- UpdateChartSpecRequest(
        chartId = els$chartReqs$new_sheet$chart$chartId,
        spec = modified_chart_req
      )
    )
  )

  expect_failure(
    expect_error(
      resp <- request_ss_batchUpdate(
        spreadsheetId = ss_id,
        req
      )
    )
  )


})


