sheetId = 0

test_that("OverlayPosition can be created, listinized and generated from list", {

  expect_failure(
    expect_error(
      overlayPosition_max <- OverlayPosition(
        GridCoordinate(0, 1, 1),
        offsetXPixels = 15,
        offsetYPixels = 22,
        widthPixels = 150,
        heightPixels = 100
      )
    )
  )

  expect_failure(
    expect_error(
      overlayPosition_min <- OverlayPosition(
        GridCoordinate(0, 1, 1)
      )
    )
  )

  expect_s3_class(overlayPosition_max, "OverlayPosition")
  expect_s3_class(overlayPosition_min, "OverlayPosition")

  expect_genned_identical(overlayPosition_max, sheetId)
  expect_genned_identical(overlayPosition_max, sheetId, TRUE)

  expect_genned_identical(overlayPosition_min, sheetId)
  expect_genned_identical(overlayPosition_min, sheetId, TRUE)

})

test_that("EmbeddedObjectPosition can be created, listinized and generated from list", {

  expect_failure(
    expect_error(
      embeddedObjectPosition_overlaid <- EmbeddedObjectPosition(
        OverlayPosition(GridCoordinate(0, 1, 1))
      )
    )
  )

  expect_failure(
    expect_error(
      embeddedObjectPosition_new <- EmbeddedObjectPosition(newSheet = T)
    )
  )

  expect_failure(
    expect_error(
      embeddedObjectPosition_sheetId <- EmbeddedObjectPosition(sheetId = 1)
    )
  )

  expect_s3_class(embeddedObjectPosition_overlaid, "EmbeddedObjectPosition")
  expect_s3_class(embeddedObjectPosition_new, "EmbeddedObjectPosition")
  expect_s3_class(embeddedObjectPosition_sheetId, "EmbeddedObjectPosition")

  expect_genned_identical(embeddedObjectPosition_overlaid, sheetId)
  expect_genned_identical(embeddedObjectPosition_new, sheetId)
  expect_genned_identical(embeddedObjectPosition_sheetId, sheetId)

})
