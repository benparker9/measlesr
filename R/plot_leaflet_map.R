#' Create a leaflet world map for measles country-level metrics
#'
#' This function creates a leaflet choropleth map for a selected year and metric.
#' It joins country-level measles data to Natural Earth country polygons using ISO3
#' country codes.
#'
#' @param data A data frame containing country-level measles data.
#' @param year A single year to map.
#' @param palette A color palette passed to [leaflet::colorNumeric()].
#'   This can be a palette name such as `"YlOrRd"` or a vector of colors.
#' @param metric The metric to color the map by. Can be one of the computed
#'   metrics: `cases_per_100k`, `total_cases`, or `total_population`.
#'   Can also be another numeric column in `data`, which will be averaged by
#'   country for the selected year.
#'   
#' @return A leaflet map.
#'
#' @examples
#' \dontrun{
#' plot_leaflet_map(
#'   data = cases_year,
#'   year = 2023,
#'   palette = "YlOrRd",
#'   metric = cases_per_100k
#' )
#' }
#'
#' @export
plot_leaflet_map <- function(data,
                             year,
                             palette = "YlOrRd",
                             metric = cases_per_100k) {
  metric_name <- rlang::as_name(rlang::ensym(metric))
  
  required_cols <- c("iso3", "country", "year", "measles_total", "total_population")
  
  if (!all(required_cols %in% names(data))) {
    stop("`data` must contain iso3, country, year, measles_total, and total_population.")
  }
  
  if (length(year) != 1) {
    stop("`year` must be a single value.")
  }
  
  computed_metrics <- c("cases_per_100k", "total_cases", "total_population")
  
  if (!metric_name %in% computed_metrics && !metric_name %in% names(data)) {
    stop("`metric` must be cases_per_100k, total_cases, total_population, or a numeric column in `data`.")
  }
  
  if (metric_name %in% names(data) && !is.numeric(data[[metric_name]])) {
    stop("`metric` must be numeric.")
  }
  
  mean_or_na <- function(x) {
    if (all(is.na(x))) {
      NA_real_
    } else {
      mean(x, na.rm = TRUE)
    }
  }
  
  measles_country <- data |>
    dplyr::filter(.data$year == .env$year) |>
    dplyr::group_by(.data$iso3, .data$country) |>
    dplyr::summarise(total_cases = sum(.data$measles_total, na.rm = TRUE),
                     total_population = mean_or_na(.data$total_population),
                     custom_metric = if (metric_name %in% names(data)) {
                     mean_or_na(.data[[metric_name]])
                     } else {
                       NA_real_
                     }) |>
    dplyr::mutate(cases_per_100k = (.data$total_cases / .data$total_population) * 100000,
                  map_metric = dplyr::case_when(
                  metric_name == "cases_per_100k" ~ .data$cases_per_100k,
                  metric_name == "total_cases" ~ .data$total_cases,
                  metric_name == "total_population" ~ .data$total_population,
                  TRUE ~ .data$custom_metric))
  
  if (nrow(measles_country) == 0) {
    stop("No data found for the selected year.")
  }
  
  world <- rnaturalearth::ne_countries(scale = "medium",
                                       returnclass = "sf")
  
  world_measles <- world |>
    dplyr::left_join(measles_country, by = c("iso_a3" = "iso3"))
  
  pal <- leaflet::colorNumeric(
    palette = palette,
    domain = world_measles$map_metric,
    na.color = "#cccccc"
  )
  
  leaflet::leaflet(world_measles, height = "1000px") |>
    leaflet::addProviderTiles(
      leaflet::providers$CartoDB.Positron,
      options = leaflet::providerTileOptions(noWrap = TRUE)
    ) |>
    leaflet::setView(lng = 30, lat = 0, zoom = 3) |>
    leaflet::setMaxBounds(
      lng1 = -180,
      lat1 = -85,
      lng2 = 180,
      lat2 = 85
    ) |>
    leaflet::addPolygons(
      fillColor = ~pal(map_metric),
      fillOpacity = 0.8,
      color = "black",
      weight = 0.35,
      opacity = 0.8,
      popup = ~paste0(
        "<strong>", dplyr::coalesce(country, name), "</strong><br>",
        "Cases: ", total_cases, "<br>",
        "Population: ", total_population, "<br>",
        metric_name, ": ", round(map_metric, 3)
      )
    ) |>
    leaflet::addLegend(
      pal = pal,
      values = ~map_metric,
      title = metric_name,
      position = "topleft"
    )
}