#' Load monthly cases data
#'
#' Reads the TidyTuesday 2025 monthly cases dataset from GitHub.
#'
#' @return A tibble containing monthly case data.
#' @export
#'
#' @examples
#' load_data()

load_data <- function() {
  readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-06-24/cases_month.csv')
}




