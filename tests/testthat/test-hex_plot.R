example_data <- data.frame(
  x_var = c(1, 2, 3, 4, 5),
  y_var = c(10, 20, 15, 30, 25),
  group = c("a", "b", "c", "d", "e"))

plot1 <- create_hex_plot(
  data = example_data,
  x = x_var,
  y = y_var,
  title = "Random Title")

test_that("returns a ggplot object", {
  expect_s3_class(plot1, "ggplot")})

test_that("uses the given title", {
  expect_equal(plot1$labels$title, "Random Title")})

test_that("creates x label from variable name", {
  expect_equal(plot1$labels$x, "X Var")})

test_that("creates y label from variable name", {
  expect_equal(plot1$labels$y, "Y Var")})

test_that("x is not numeric", {
  expect_error(
    create_hex_plot(
      data = example_data,
      x = group,
      y = y_var),
    "x must be a numeric variable")})

test_that("y is not numeric", {
  expect_error(
    create_hex_plot(
      data = example_data,
      x = x_var,
      y = group),
    "y must be a numeric variable")})

test_that("data is not a data frame", {
  expect_error(
    create_hex_plot(
      data = "LUL",
      x = x_var,
      y = y_var),
    "data must be a data frame")})

test_that("plot_theme must be a ggplot theme", {
  expect_error(
    create_hex_plot(
      data = example_data,
      x = x_var,
      y = y_var,
      plot_theme = "not a theme"),
    "plot_theme must be a ggplot theme")})