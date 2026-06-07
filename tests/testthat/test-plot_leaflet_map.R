test_data <- data.frame(
  iso3 = c("USA", "CAN"),
  country = c("United States", "Canada"),
  year = c(2023, 2023),
  measles_total = c(100, 25),
  total_population = c(331000000, 38000000),
  vaccine_coverage = c(91.5, 86.2)
)

bad_data <- data.frame(
  iso3 = "USA",
  country = "United States",
  year = 2023
)


test_that("plot_leaflet_map returns a leaflet map", {
  map <- plot_leaflet_map(
    data = test_data,
    year = 2023,
    palette = "YlOrRd",
    metric = cases_per_100k
  )
  
  expect_s3_class(map, "leaflet")
  expect_s3_class(map, "htmlwidget")
})


test_that("plot_leaflet_map works with custom numeric metric", {
  map <- plot_leaflet_map(
    data = test_data,
    year = 2023,
    palette = "YlOrRd",
    metric = vaccine_coverage
  )
  
  expect_s3_class(map, "leaflet")
})


test_that("plot_leaflet_map errors when required columns are missing", {
  expect_error(
    plot_leaflet_map(
      data = bad_data,
      year = 2023,
      palette = "YlOrRd",
      metric = cases_per_100k
    ),
    "`data` must contain"
  )
})


test_that("plot_leaflet_map errors when selected year has no data", {
  expect_error(
    plot_leaflet_map(
      data = test_data,
      year = 1999,
      palette = "YlOrRd",
      metric = cases_per_100k
    ),
    "No data found"
  )
})


test_that("plot_leaflet_map errors when metric does not exist", {
  expect_error(
    plot_leaflet_map(
      data = test_data,
      year = 2023,
      palette = "YlOrRd",
      metric = fake_metric
    ),
    "`metric` must be"
  )
})