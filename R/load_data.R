#' Load monthly cases data
#'
#' Reads the TidyTuesday 2025 monthly cases dataset from GitHub.
#'
#' @return A tibble containing monthly case data.
#' @export
#'
#' @importFrom arrow read_parquet
#'
#' @examples
#' load_data()

load_data <- function() {
  path <- system.file("measles-meta.parquet", package = "measlesr")
  arrow::read_parquet(path)
}



