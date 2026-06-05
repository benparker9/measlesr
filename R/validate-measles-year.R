#' Validate a measles data year
#'
#' Checks that the supplied year is a single numeric value within the valid
#' range of years available in the measles dataset.
#'
#' @param year A single numeric year. Must be between 2012 and 2025.
#'
#' @return Invisibly returns `TRUE` if the year is valid. Otherwise, the function
#'   stops with an error message.
#'
#' @export
#'
#' @examples
#' validate_measles_year(2019)
#'

validate_measles_year <- function(year) {
  if (is.null(year)) {
    stop("Please define a year for study")
  }
  if (length(year) > 1) {
    stop("Please define a single year")
  }
  if (year < 2012 | year > 2025) {
    stop("Please define a year from 2012 - 2025")
  }
  if (!is.numeric(year)) {
    stop("year must be numeric")
  }
  
  invisible(TRUE)
}
