loaded_data <- load_data()

test_year <- max(loaded_data$year, na.rm = TRUE)
missing_year <- max(loaded_data$year, na.rm = TRUE) + 1

bad_data <- loaded_data |>
  dplyr::select(-population)


test_that("loaded data has the columns needed for plot_leaflet_map", {
  expect_true(all(c(
    "iso3",
    "country",
    "year",
    "measles_total",
    "population"
  ) %in% names(loaded_data)))
})


test_that("plot_leaflet_map returns a leaflet map using loaded data", {
  map <- plot_leaflet_map(
    data = loaded_data,
    year = test_year,
    palette = "YlOrRd",
    metric = cases_per_100k
  )
  
  expect_s3_class(map, "leaflet")
  expect_s3_class(map, "htmlwidget")
})


test_that("plot_leaflet_map works with total_cases using loaded data", {
  map <- plot_leaflet_map(
    data = loaded_data,
    year = test_year,
    palette = "YlOrRd",
    metric = total_cases
  )
  
  expect_s3_class(map, "leaflet")
  expect_s3_class(map, "htmlwidget")
})


test_that("plot_leaflet_map works with population using loaded data", {
  map <- plot_leaflet_map(
    data = loaded_data,
    year = test_year,
    palette = "YlOrRd",
    metric = population
  )
  
  expect_s3_class(map, "leaflet")
  expect_s3_class(map, "htmlwidget")
})


test_that("plot_leaflet_map works with custom numeric metric from loaded data", {
  map <- plot_leaflet_map(
    data = loaded_data,
    year = test_year,
    palette = "YlOrRd",
    metric = measles_lab_confirmed
  )
  
  expect_s3_class(map, "leaflet")
  expect_s3_class(map, "htmlwidget")
})


test_that("plot_leaflet_map errors when required columns are missing", {
  expect_error(
    plot_leaflet_map(
      data = bad_data,
      year = test_year,
      palette = "YlOrRd",
      metric = cases_per_100k
    ),
    "`data` must contain"
  )
})


test_that("plot_leaflet_map errors when selected year has no data", {
  expect_error(
    plot_leaflet_map(
      data = loaded_data,
      year = missing_year,
      palette = "YlOrRd",
      metric = cases_per_100k
    ),
    "No data found"
  )
})


test_that("plot_leaflet_map errors when metric does not exist", {
  expect_error(
    plot_leaflet_map(
      data = loaded_data,
      year = test_year,
      palette = "YlOrRd",
      metric = fake_metric
    ),
    "`metric` must be"
  )
})


test_that("plot_leaflet_map errors when metric is not numeric", {
  expect_error(
    plot_leaflet_map(
      data = loaded_data,
      year = test_year,
      palette = "YlOrRd",
      metric = country
    ),
    "`metric` must be numeric"
  )
})