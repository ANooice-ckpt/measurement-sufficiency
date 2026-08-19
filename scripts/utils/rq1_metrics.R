suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(LightLogR)
})

datetime_2_numeric <- function(x) {
  x |> mutate(across(where(is.POSIXct), function(y) as.numeric(hms::as_hms(y))))
}

rq1_daily_metrics <- function(data, include_pulses = TRUE) {
  base <- data |>
    group_by(site, Id, Date, configuration) |>
    summarise(
      duration_above_threshold(MEDI, Datetime, "above", 10, na.rm = TRUE, as.df = TRUE),
      duration_above_threshold(MEDI, Datetime, "above", 250, na.rm = TRUE, as.df = TRUE),
      duration_above_threshold(MEDI, Datetime, "above", 1000, na.rm = TRUE, as.df = TRUE),
      period_above_threshold(MEDI, Datetime, "above", 10, na.rm = TRUE, as.df = TRUE),
      period_above_threshold(MEDI, Datetime, "above", 250, na.rm = TRUE, as.df = TRUE),
      period_above_threshold(MEDI, Datetime, "above", 1000, na.rm = TRUE, as.df = TRUE),
      bright_dark_period(log_zero_inflated(MEDI), Datetime, "brightest", "10 hours", as.df = TRUE, na.rm = TRUE) |> datetime_2_numeric(),
      bright_dark_period(log_zero_inflated(MEDI), Datetime, "darkest", "10 hours", as.df = TRUE, loop = TRUE, na.rm = TRUE) |> datetime_2_numeric(),
      timing_above_threshold(MEDI, Datetime, "above", 10, as.df = TRUE) |> datetime_2_numeric(),
      timing_above_threshold(MEDI, Datetime, "above", 250, as.df = TRUE) |> datetime_2_numeric(),
      frequency_crossing_threshold(MEDI, 250, na.rm = TRUE, as.df = TRUE),
      timing_above_threshold(MEDI, Datetime, "above", 1000, as.df = TRUE) |> datetime_2_numeric(),
      barroso_lighting_metrics(MEDI, Datetime, loop = TRUE, na.rm = TRUE, as.df = TRUE),
      centroidLE(MEDI, Datetime, na.rm = TRUE, as.df = TRUE) |> datetime_2_numeric(),
      disparity_index(MEDI, TRUE, TRUE),
      midpointCE(MEDI, Datetime, TRUE, TRUE) |> datetime_2_numeric(),
      mean_MEDI = mean(log_zero_inflated(MEDI), na.rm = TRUE),
      dose(MEDI, Datetime, na.rm = TRUE, as.df = TRUE),
      .groups = "drop"
    ) |>
    mutate(across(where(lubridate::is.duration), as.numeric)) |>
    pivot_longer(-c(site, Id, Date, configuration), names_to = "metric", values_to = "value")
  if (!include_pulses) return(base)
  pulses <- data |> group_by(site, Id, Date, configuration) |> summarise(
    pulses_above_threshold(MEDI, Datetime, threshold = 250, na.rm = TRUE, as.df = TRUE) |> datetime_2_numeric(),
    pulses_above_threshold(MEDI, Datetime, threshold = 1000, na.rm = TRUE, as.df = TRUE) |> datetime_2_numeric(),
    .groups = "drop") |> mutate(across(where(lubridate::is.duration), as.numeric)) |>
    pivot_longer(-c(site, Id, Date, configuration), names_to = "metric", values_to = "value")
  bind_rows(base, pulses)
}

rq1_spectral_daily_metrics <- function(data) {
  data |>
    group_by(site, Id, Date, configuration) |>
    summarise(
      nvRD = mean(nvRD(MEDI, LIGHT, Datetime), na.rm = TRUE),
      MDER = median(MEDI / LIGHT, na.rm = TRUE),
      .groups = "drop"
    ) |>
    pivot_longer(-c(site, Id, Date, configuration), names_to = "metric", values_to = "value")
}

rq1_multiday_metrics <- function(data) {
  data |>
    group_by(site, Id, configuration) |>
    summarise(
      interdaily_stability(log_zero_inflated(MEDI), Datetime, na.rm = TRUE, as.df = TRUE),
      intradaily_variability(log_zero_inflated(MEDI), Datetime, na.rm = TRUE, as.df = TRUE),
      .groups = "drop"
    ) |>
    pivot_longer(-c(site, Id, configuration), names_to = "metric", values_to = "value") |>
    mutate(Date = as.Date(NA), .before = metric)
}

rq1_all_metrics <- function(data, include_spectral = TRUE, include_pulses = TRUE) {
  out <- bind_rows(rq1_daily_metrics(data, include_pulses), rq1_multiday_metrics(data))
  if (include_spectral) out <- bind_rows(out, rq1_spectral_daily_metrics(data))
  out
}
