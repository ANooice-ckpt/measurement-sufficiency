suppressPackageStartupMessages({ library(tidyverse); library(readxl) })
dir.create("data/derived", recursive = TRUE, showWarnings = FALSE)
dir.create("results/rq1", recursive = TRUE, showWarnings = FALSE)

types <- read.csv("data/interim/rq1/metric_types.csv")
files <- list.files("data/interim/rq1/metrics", pattern = "\\.rds$", full.names = TRUE)
raw <- map_dfr(files, readRDS)

emitted_values <- raw |> mutate(
  dimension = case_when(
    str_detect(configuration, "^duration_") ~ "duration",
    configuration %in% c("chest", "wrist") | str_detect(configuration, "^eye_pair_") ~ "placement",
    configuration == "eye_LIGHT_10s" ~ "optical",
    str_detect(configuration, "^eye_MEDI_(30s|1min|5min|15min|30min)$") ~ "temporal",
    TRUE ~ "reference"),
  subset_id = if ("subset_id" %in% names(raw)) as.character(subset_id) else NA_character_,
  n_days = if ("n_days" %in% names(raw)) as.integer(n_days) else NA_integer_,
  analysis_unit_id = coalesce(as.character(analysis_unit_id),
    if_else(is.na(Date), paste(site, Id, sep = ":"), paste(site, Id, Date, sep = ":"))),
  support_available = is.finite(value)) |>
  left_join(types, by = "metric") |>
  select(dimension, configuration, site, Id, analysis_unit_id, Date, subset_id, n_days,
         metric, metric_class, value, support_available)
units <- emitted_values |> distinct(dimension, configuration, site, Id, analysis_unit_id, Date, subset_id, n_days)
metric_values <- crossing(units, types) |>
  left_join(emitted_values |> select(-metric_class, -support_available),
            by = c("dimension", "configuration", "site", "Id", "analysis_unit_id", "Date", "subset_id", "n_days", "metric")) |>
  mutate(representation_available =
           !(configuration == "eye_LIGHT_10s" & metric %in% c("MDER", "nvRD")) &
           !(configuration %in% c("eye_MEDI_5min", "eye_MEDI_15min", "eye_MEDI_30min") & str_detect(metric, "pulses_above")),
         support_available = representation_available & is.finite(value))
saveRDS(metric_values, "data/derived/rq1_metric_values.rds")
write.csv(metric_values, "data/derived/rq1_metric_values.csv", row.names = FALSE, na = "")

pair_values <- function(candidate, reference, dimension, lattice) {
  keys <- c("site", "Id", "Date", "metric")
  cnd <- metric_values |> filter(configuration == candidate) |> select(all_of(keys), analysis_unit_id, subset_id, n_days, candidate_value = value)
  ref <- metric_values |> filter(configuration == reference) |> select(all_of(keys), reference_value = value)
  inner_join(cnd, ref, by = keys) |> mutate(dimension = dimension, configuration = candidate,
    reference_configuration = reference, lattice = lattice)
}

dist <- bind_rows(
  pair_values("eye_LIGHT_10s", "eye_MEDI_10s", "optical", "optical"),
  map_dfr(c("eye_MEDI_30s", "eye_MEDI_1min", "eye_MEDI_5min", "eye_MEDI_15min", "eye_MEDI_30min"),
          ~pair_values(.x, "eye_MEDI_10s", "temporal", "temporal")),
  pair_values("chest", "eye_pair_chest", "placement", "placement_chest"),
  pair_values("wrist", "eye_pair_wrist", "placement", "placement_wrist"))

# Duration reference is fixed per participant and metric, independent of candidate subset ID.
dur_ref <- metric_values |> filter(configuration == "duration_7d") |>
  select(site, Id, metric, reference_value = value)
dur <- metric_values |> filter(str_detect(configuration, "^duration_[1-6]d$")) |>
  select(site, Id, Date, metric, analysis_unit_id, subset_id, n_days, configuration, candidate_value = value) |>
  left_join(dur_ref, by = c("site", "Id", "metric")) |>
  mutate(dimension = "duration", reference_configuration = "duration_7d", lattice = "duration")
dist <- bind_rows(dist, dur)

timing_metrics <- types |> filter(metric_class == "timing") |> pull(metric)
circular_delta <- function(a, b) ((a - b + 43200) %% 86400) - 43200
dist <- dist |> mutate(delta = if_else(metric %in% timing_metrics,
  circular_delta(candidate_value, reference_value), candidate_value - reference_value))

standardizers <- dist |> distinct(lattice, site, Id, Date, metric, reference_value) |>
  group_by(lattice, metric) |> summarise(
    n_reference_units = sum(is.finite(reference_value)),
    standardizer = if (first(metric) %in% timing_metrics) {
      x <- reference_value[is.finite(reference_value)]
      if (length(x) < 2) NA_real_ else {
        center <- (atan2(mean(sin(2*pi*x/86400)), mean(cos(2*pi*x/86400))) %% (2*pi)) * 86400/(2*pi)
        sd(circular_delta(x, center))
      }
    } else sd(reference_value, na.rm = TRUE),
    .groups = "drop") |>
  mutate(zero_or_near_zero = !is.finite(standardizer) | standardizer <= sqrt(.Machine$double.eps))
write.csv(standardizers, "results/diagnostics/rq1_standardizer_audit.csv", row.names = FALSE, na = "")

canonical <- dist |> left_join(standardizers, by = c("lattice", "metric")) |> left_join(types, by = "metric") |>
  mutate(e = delta / standardizer, available = is.finite(reference_value) & is.finite(candidate_value) & !zero_or_near_zero,
    unavailable_reason = case_when(!is.finite(reference_value) ~ "reference metric value missing",
      !is.finite(candidate_value) ~ "candidate metric value missing",
      zero_or_near_zero ~ "reference dispersion zero or undefined", TRUE ~ NA_character_)) |>
  select(dimension, configuration, reference_configuration, site, Id, analysis_unit_id, Date, subset_id, n_days,
         metric, metric_class, reference_value, candidate_value, delta, standardizer, e, available, unavailable_reason)
saveRDS(canonical, "data/derived/rq1_distortion_long.rds")
write.csv(canonical, "data/derived/rq1_distortion_long.csv", row.names = FALSE, na = "")

# Explicit metric/configuration availability, including structurally unavailable operators.
configs <- tibble(configuration = c("chest", "wrist", "eye_LIGHT_10s", "eye_MEDI_30s", "eye_MEDI_1min", "eye_MEDI_5min", "eye_MEDI_15min", "eye_MEDI_30min", paste0("duration_", 1:6, "d"))) |>
  mutate(dimension = case_when(configuration %in% c("chest", "wrist") ~ "placement", configuration == "eye_LIGHT_10s" ~ "optical",
                               str_detect(configuration, "^eye_MEDI") ~ "temporal", TRUE ~ "duration"))
availability <- crossing(configs, types) |> left_join(canonical |> group_by(dimension, configuration, metric) |>
  summarise(n_available_units = sum(available), n_total_units = n(), .groups = "drop"),
  by = c("dimension", "configuration", "metric")) |>
  mutate(representation_available = !(dimension == "optical" & metric %in% c("MDER", "nvRD")) &
           !(dimension == "temporal" & configuration %in% c("eye_MEDI_5min", "eye_MEDI_15min", "eye_MEDI_30min") & str_detect(metric, "pulses_above")),
         unavailable_reason = case_when(dimension == "optical" & metric %in% c("MDER", "nvRD") ~ "intrinsically requires MEDI and LIGHT",
           dimension == "temporal" & configuration %in% c("eye_MEDI_5min", "eye_MEDI_15min", "eye_MEDI_30min") & str_detect(metric, "pulses_above") ~ "LightLogR pulse parameters shorter than/coincident with epoch",
           is.na(n_available_units) ~ "no emitted metric value", TRUE ~ NA_character_),
         n_available_units = replace_na(n_available_units, 0L), n_total_units = replace_na(n_total_units, 0L))
write.csv(availability, "results/rq1/rq1_metric_availability.csv", row.names = FALSE, na = "")

flow <- bind_rows(read.csv("results/rq1/rq1_reference_sample_flow.csv"), read.csv("results/rq1/rq1_sample_flow_partial.csv"),
                  read.csv("results/rq1/rq1_duration_sample_flow.csv"))
write.csv(flow, "results/rq1/rq1_sample_flow.csv", row.names = FALSE, na = "")
