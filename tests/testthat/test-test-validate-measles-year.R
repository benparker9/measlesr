test_that("validate_measles_year works", {
  expect_error(validate_measles_year())
  expect_error(validate_measles_year(c(2019, 2020)))
  expect_error(validate_measles_year("2019"))
  expect_error(validate_measles_year(2000))
  expect_error(validate_measles_year(2030))
  expect_silent(validate_measles_year(2019))
})
