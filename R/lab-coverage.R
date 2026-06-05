#' Get top or bottom 10 countries by proportion of lab confirmed to total measles cases
#'
#' Filters the monthly cases dataset to a given year, aggregates lab confirmed, total measles  
#' cases by country, and returns the top or bottom 10 countries proportions.
#'
#' @param year A single numeric year between 2012 and 2025.
#' @param top Logical. If `TRUE` (default), returns the top 10 countries with
#'   the highest lab confirmed:total cases ratio and the top 10 countries by total cases. 
#'   If `FALSE`, returns the bottom 10 countries.
#'
#' @return Prints a summary message and a tibble of 10 countries with their
#'   lab confirmed:total proportion.
#'
#' @importFrom dplyr filter group_by summarize slice_max slice_min arrange
#'
#' @export
#'
#' @examples
#' lab_coverage(2019)
#' lab_coverage(2019, top = FALSE)

lab_coverage <- function(year, top = TRUE) {
  validate_measles_year(year)
  
  dat <- load_data() |>
    filter(year == {{year}}) |>
    group_by(country) |>
    summarize(LabConfirmed = sum(measles_lab_confirmed, na.rm = TRUE),
              TotalCases = sum(measles_total, na.rm = TRUE),
              LabCoverage = (sum(measles_lab_confirmed, na.rm = TRUE) / sum(measles_total, na.rm = TRUE)) * 100) 
  
  
  if (top) {
    dat_sum <- dat |>
      arrange(desc(LabCoverage), desc(TotalCases)) |>
      slice_max(LabCoverage, n = 10, with_ties = FALSE)
    print(paste("Top 10 Countries with the Highest Lab Confirmed to Total Measles Cases for the Year:", year))
    print(dat_sum)
  } else {
    dat_sum <- dat |>
      slice_min(LabCoverage, n = 10, with_ties = FALSE)
    message("Bottom 10 Countries with the Lowest Lab Confirmed to Total Measles Cases for the Year:", year)
    return(dat_sum)
  } 
}




