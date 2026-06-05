test_that("lab_coverage works correctly", {
  # correct class returned
  expect_s3_class(lab_coverage(2019), "data.frame")
  # returns 10 rows
  expect_equal(nrow(lab_coverage(2019)), 10)
  expect_equal(nrow(lab_coverage(2019, top = FALSE)), 10)
  # errors on NULL year
  expect_error(lab_coverage())
  # errors on multiple years
  expect_error(lab_coverage(c(2019, 2020)))
  # errors on out of range year
  expect_error(lab_coverage(2000))
  expect_error(lab_coverage(2030))
})
