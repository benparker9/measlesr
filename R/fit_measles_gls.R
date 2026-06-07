#' Fit the measles GLS model
#'
#' This function fits the generalized least squares model used in the measles
#' analysis. It creates all the variables used in the model and accounts for
#' correlation.
#'
#' @param data The final measles dataset
#'
#' @return A GLS model object
#'
#' @importFrom dplyr distinct select mutate arrange
#' @importFrom nlme gls corAR1
#' @importFrom stats na.omit relevel
#' @export
#'

fit_measles_gls <- function(data){
  
  if (!is.data.frame(data)) {
    stop("data must be a data frame.")
  }
  
  linearreg_df <- data |>
    distinct(country, year, month, .keep_all = TRUE) |>
    select(
      country,
      region.x,
      year,
      month,
      gdp,
      population,
      measles_total,
      mean_temp,
      urban_population,
      tourists) |>
    mutate(
      gdp_per_capita = gdp / population,
      measles_rate_100k = (measles_total / population) * 100000,
      log_gdp_per_capita = log(gdp_per_capita),
      log_measles_rate_100k = log(measles_rate_100k + 1),
      log_tourists = log(tourists + 1),
      year_month = as.integer(year * 12 + month),
      country = as.factor(country),
      region = stats::relevel(as.factor(region.x), ref = "AFR")) |>
    arrange(country, year_month)
  
  gls(
    log_measles_rate_100k ~ log_gdp_per_capita * region + year +
      mean_temp + log_tourists + urban_population,
    data = linearreg_df,
    correlation = corAR1(form = ~ year_month | country),
    method = "ML",
    na.action = stats::na.omit)
}
