test_that("ConditionValue can be created, listinized and generated from list", {

  expect_failure(
    expect_error(
      val_user <- ConditionValue("Eustachy")
    )
  )

  expect_true(is.ConditionValue(val_user))

  expect_failure(
    expect_error(
      val_rel <- ConditionValue(relativeDate = "PAST_WEEK")
    )
  )

  expect_true(is.ConditionValue(val_rel))

  expect_genned_identical(val_user)
  expect_genned_identical(val_rel)

})



test_that("BooleanCondition can be created, listinized and generated from list", {

  expect_failure(
    expect_error(
      con_min <- BooleanCondition("BLANK")
    )
  )

  expect_true(is.BooleanCondition(con_min))

  expect_failure(
    expect_error(
      con_max <- BooleanCondition(
        type = "ONE_OF_LIST",
        values = lapply(c("Test", "Values", "To", "Gen"), ConditionValue))
    )
  )

  expect_true(is.BooleanCondition(con_max))

  expect_genned_identical(con_min)
  expect_genned_identical(con_max)

})
