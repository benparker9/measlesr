test_that("measles_snapshot works correctly", {
  # correct class returned
  expect_s3_class(measles_snapshot(2019), "data.frame")
  # returns 10 rows
  expect_equal(nrow(measles_snapshot(2019)), 10)
  expect_equal(nrow(measles_snapshot(2019, top = FALSE)), 10)
  # errors on NULL year
  expect_error(measles_snapshot())
  # errors on multiple years
  expect_error(measles_snapshot(c(2019, 2020)))
  # errors on out of range year
  expect_error(measles_snapshot(2000))
  expect_error(measles_snapshot(2030))
  # bottom 10 excludes zero case countries
  expect_true(all(measles_snapshot(2019, top = FALSE)$TotalMeaslesCases > 0))
})
