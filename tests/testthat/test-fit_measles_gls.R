test_that("fit_measles_gls returns a GLS model", {
  
  example_gls_data <- data.frame(
    country = rep(
      c("Nigeria", "USA", "Yemen", "France", "India", "Japan"),
      each = 5),
    region.x = rep(
      c("AFR", "AMR", "EMR", "EUR", "SEAR", "WPR"),
      each = 5),
    year = rep(2016:2020, times = 6),
    month = 1,
    gdp = runif(30, 100, 1000),
    population = runif(30, 100, 1000),
    measles_total = runif(30, 100, 1000),
    mean_temp = runif(30, 100, 1000),
    urban_population = runif(30, 100, 1000),
    tourists = runif(30, 100, 1000))
  
  model <- fit_measles_gls(example_gls_data)
  
  expect_s3_class(model, "gls")})

test_that("data is not a data frame", {
  expect_error(
    fit_measles_gls("xdxdxd"),
    "data must be a data frame")})