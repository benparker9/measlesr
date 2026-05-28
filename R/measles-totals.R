#' Get top or bottom 10 countries by measles cases
#'
#' Filters the monthly cases dataset to a given year, aggregates total measles
#' cases by country, and returns the top or bottom 10 countries.
#'
#' @param year A single numeric year between 2012 and 2025.
#' @param top Logical. If `TRUE` (default), returns the top 10 countries with
#'   the highest measles cases. If `FALSE`, returns the bottom 10 countries
#'   (excluding countries with 0 cases).
#'
#' @return Prints a summary message and a tibble of 10 countries with their
#'   total measles cases.
#'
#' @importFrom dplyr filter group_by summarize slice_max slice_min
#'
#' @export
#'
#' @examples
#' measles_snapshot(2019)
#' measles_snapshot(2019, top = FALSE)

measles_snapshot <- function(year, top = TRUE) {
  if (is.null(year)) {
    stop("Please define a year for study")
  }
  if (length(year) > 1) {
    stop("Please define a single year")
  }
  if (year < 2012 | year > 2025) {
    stop("Please define a year from 2012 - 2025")
  }
  
  dat <- load_data() |>
    filter(year == year) |>
    group_by(country) |>
    summarize(TotalMeaslesCases = mean(measles_total, na.rm = TRUE))
  
  if (top) {
    dat_sum <- dat |> 
      slice_max(TotalMeaslesCases, n = 10, with_ties = FALSE)
    print(paste("Top 10 Countries with the Highest Total Measles Cases for Year:", year))
    print(dat_sum)
  } else {
    dat_sum <- dat |>
      filter(TotalMeaslesCases > 0) |>
      slice_min(TotalMeaslesCases, n = 10, with_ties = FALSE)
    print(paste("Bottom 10 Countries with the Lowest Total Measles Cases (not including 0 case countries) for Year:", year))
    print(dat_sum)
  } 
}
