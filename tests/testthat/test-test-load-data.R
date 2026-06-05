test_that("load_data works correctly", {
  # correct class returned
  expect_s3_class(load_data(), "data.frame")
  # returns 10 rows
  expect_equal(dim(load_data()), c(22780, 15))
  # errors on user inputs 
  expect_error(load_data(12))
})
