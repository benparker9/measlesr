#' Create a hexbin plot
#'
#' This function creates a hexbin plot for any two numeric variables in a data
#' frame. 
#' 
#' @param data A data frame containing at least two numeric variables.
#' @param x The numeric variable on x-axis
#' @param y The numeric variable on y-axis
#' @param bins The number of hexagonal bins
#' @param title The plot title.
#' @param x_label The x-axis label
#' @param y_label The y-axis label
#' 
#' @param plot_theme The ggplot theme to use. The default is theme_bw()
#'
#' @return A ggplot object showing a hexbin plot.
#'
#' @importFrom ggplot2 ggplot aes geom_hex labs theme_bw
#' @importFrom dplyr pull
#' @export
#'
#' @examples
#' cases <- load_data()
#'
#' create_hex_plot(
#'   data = cases,
#'   x = year,
#'   y = measles_total)

create_hex_plot <- function(data, x, y, bins = 20,
                     title = NULL,
                     x_label = NULL,
                     y_label = NULL,
                     plot_theme = theme_bw()) {
  if (!is.data.frame(data)) {
    stop("data must be a data frame.")
  }
  
  if (!is.numeric(dplyr::pull(data, {{x}}))) {
    stop("x must be a numeric variable.")
  }
  
  if (!is.numeric(dplyr::pull(data, {{y}}))) {
    stop("y must be a numeric variable.")
  }
  
  data |>
    ggplot(aes(x = {{x}}, y = {{y}})) +
    geom_hex(bins = bins) +
    labs(
      title = title,
      x = x_label,
      y = y_label,
      fill = "Count"
    ) +
    plot_theme
}
