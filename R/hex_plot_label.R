#' Make a nice label for a hex plot
#'
#' This is a helper function that goes along with the hex_plot function.
#' This converts the variable names into nice labels for the plot.
#'
#' @param var A variable name or expression.
#'
#' @return A character string formatted as a plot label.
#'
#' @importFrom rlang as_label
#' @importFrom stringr str_replace_all str_to_title

hex_plot_label <- function(var) {
  var |>
    as_label() |>
    str_replace_all("_", " ") |>
    str_to_title()
}