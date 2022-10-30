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
        title = "Cars dataset",
        gridProperties = GridProperties(51, 2)
      ),
      data = to_GridData_from_df(cars, 0, 0))
  )
)
created <- request_ss_create(cars_spreadsheet)
ss_id <- googledrive::as_id(created$spreadsheetId)
on.exit(googledrive::drive_trash(ss_id))

els <- new.env()
els$rules <- list()
els$reqs <- list()
els$resps <- list()

test_that("AddConditionalFormatRule can be created and send", {

  expect_failure(
    expect_error(
      els$rules$bool_speed <- ConditionalFormatRule(
        ranges = GridRange(0, startRowIndex = 1, startColumnIndex = 0, endColumnIndex = 1),
        booleanRule = BooleanRule(
          BooleanCondition("NUMBER_LESS",
                           ConditionValue(round(mean(cars$speed), 2))),
          CellFormat(backgroundColorStyle = ColorStyle(0.2, 0.2, 0.8),
                     textFormat = TextFormat(foregroundColorStyle = ColorStyle(0.8, 0.8, 0.2),
                                             bold = TRUE))
        )
      )
    )
  )

  expect_failure(
    expect_error(
      els$rules$grad_dist <- ConditionalFormatRule(
        ranges = GridRange(0, startRowIndex = 1, startColumnIndex = 1, endColumnIndex = 2),
        gradientRule = GradientRule(
          minpoint = InterpolationPoint(ColorStyle(0.1, 0.1, 0.5), type = "MIN"),
          maxpoint = InterpolationPoint(ColorStyle(0.3, 0.5, 0.6), type = "MAX"),
          midpoint = InterpolationPoint(ColorStyle(0.5, 0.1, 0.1), type = "PERCENT", value = 50)
        )
      )
    )
  )

    expect_failure(
      expect_error(
        els$reqs$bool_speed <- AddConditionalFormatRuleRequest(
          0,
          els$rules$bool_speed
        )
      )
    )

    expect_failure(
      expect_error(
        els$reqs$grad_dist <- AddConditionalFormatRuleRequest(
          1, els$rules$grad_dist
        )
      )
    )

    expect_true(is.dgs4Req(els$reqs$bool_speed))
    expect_true(is.dgs4Req(els$reqs$grad_dist))

    expect_failure(
      expect_error(
        els$resps$add <- request_ss_batchUpdate(
          created$spreadsheetId,
          .dots = list(els$reqs$bool_speed,
                       els$reqs$grad_dist),
        )
      )
    )

})

test_that("UpdateConditionalFormatRule can be created and sent", {

  expect_failure(
    expect_error(
      els$rules$bool_dist <- ConditionalFormatRule(
        ranges = GridRange(0, startRowIndex = 1, startColumnIndex = 1, endColumnIndex = 2),
        booleanRule = BooleanRule(
          BooleanCondition("NUMBER_NOT_BETWEEN",
                           values = list(
                             ConditionValue(round(mean(cars$dist) - sd(cars$dist), 2)),
                             ConditionValue(round(mean(cars$dist) + sd(cars$dist), 2)))
          ),
          CellFormat(backgroundColorStyle = ColorStyle(0.5, 0.3, 0.5),
                     textFormat = TextFormat(foregroundColorStyle = ColorStyle(0.2, 0.9, 0.9),
                                             bold = TRUE))
        )
      )
    )
  )

  expect_failure(
    expect_error(
      els$reqs$update_speed <- UpdateConditionalFormatRuleRequest(
        index = 0,
        newIndex = 1,
        sheetId = 0
      )
    )
  )

  expect_failure(
    expect_error(
      els$reqs$update_dist <- UpdateConditionalFormatRuleRequest(
        index = 0,
        rule = els$rules$bool_dist
      )
    )
  )

  expect_failure(
    expect_error(
      els$resps$update <- request_ss_batchUpdate(
        created$spreadsheetId,
        .dots = list(
          els$reqs$update_speed,
          els$reqs$update_dist
        )
      )
    )
  )

  expect_equal(els$resps$update$replies[[1]]$oldIndex, 0)
  expect_equal(els$resps$update$replies[[1]]$newIndex, 1)

  expect_true(all(
    compare_objects(obj1 = els$resps$update$replies[[1]]$newRule,
                    obj2 = els$rules$bool_speed,
                    na.rm = T)
  ))

  expect_true(all(
    compare_objects(els$resps$update$replies[[2]]$oldRule,
                    els$rules$grad_dist,
                    skip_compare = "colorStyle", na.rm = T)
  ))

  expect_true(all(
    compare_objects(els$resps$update$replies[[2]]$newRule,
                    els$rules$bool_dist,
                    skip_compare = c("red", "green", "blue"), na.rm = T)
  ))
})

test_that("DeleteConditionalFormatRuleRequest can be created and sent", {

  expect_failure(
    expect_error(
      els$reqs$delete_dist <- DeleteConditionalFormatRuleRequest(
        index = 0,
        sheetId = 0
      )
    )
  )

  expect_failure(
    expect_error(
      els$reqs$delete_speed <- DeleteConditionalFormatRuleRequest(
        index = 1,
        sheetId = 0
      )
    )
  )

  expect_failure(
    expect_error(
      els$resps$delete <- request_ss_batchUpdate(
        created$spreadsheetId,
        .dots = list(
          els$reqs$delete_dist,
          els$reqs$delete_speed
        )
      )
    )
  )

  expect_true(all(
    compare_objects(obj1 = els$resps$update$replies[[2]]$newRule,
                    obj2 = els$resps$delete$replies[[1]]$rule)
  ))

  expect_true(all(
    compare_objects(obj1 = els$resps$update$replies[[1]]$newRule,
                    obj2 = els$resps$delete$replies[[2]]$rule)
  ))

  confirm <- request_ss_get(
    spreadsheetId = created$spreadsheetId,
    fields = "sheets.conditionalFormats"
  )

  expect_true(
    length(confirm$sheets[[1]]$conditionalFormats) == 0
  )

})
