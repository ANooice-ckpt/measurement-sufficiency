source("scripts/utils/melidos_io.R")
suppressPackageStartupMessages({
  library(tidyverse)
  library(LightLogR)
  library(melidosData)
})

required <- c("light_glasses", "light_chest", "light_wrist", "sleepdiaries", "wearlog")
sites <- Reduce(intersect, lapply(required, function(m) {
  available <- melidos_sites()[file.exists(vapply(melidos_sites(), raw_data_path, character(1), modality = m))]
  sites_for_modality(available, m)
}))
site_override <- Sys.getenv("REPRO_SITES", "")
if (nzchar(site_override)) sites <- intersect(sites, trimws(strsplit(site_override, ",", fixed = TRUE)[[1]]))
if (!length(sites)) stop("No site has all five inputs required by the Zauner three-position reproduction")

load_modality <- function(modality) setNames(lapply(sites, function(site) load_raw_file(raw_data_path(site, modality), modality)), sites)
light_glasses <- load_modality("light_glasses")
light_chest <- load_modality("light_chest")
light_wrist <- load_modality("light_wrist")
sleepdiary <- load_modality("sleepdiaries")
wearlog <- load_modality("wearlog")

sample_count <- function(x, stage) {
  flat <- structure(x, class = "melidos_data") |> flatten_data()
  flat |> mutate(Date = as.Date(Datetime)) |> group_by(site) |>
    summarise(stage = stage, n_rows = n(), n_participants = n_distinct(Id),
              n_participant_days = n_distinct(interaction(Id, Date)), .groups = "drop")
}

light <- imap(light_glasses, function(data, site) {
  data |> select(Id, Datetime, MEDI, LIGHT) |>
    data2reference(light_chest[[site]], Reference.column = MEDI_chest) |>
    data2reference(light_chest[[site]], Data.column = LIGHT, Reference.column = LIGHT_chest) |>
    data2reference(light_wrist[[site]], Reference.column = MEDI_wrist) |>
    data2reference(light_wrist[[site]], Data.column = LIGHT, Reference.column = LIGHT_wrist) |>
    add_photoperiod(melidos_coordinates[[site]]) |>
    rename(MEDI_glasses = MEDI, LIGHT_glasses = LIGHT)
})
flow <- sample_count(light, "aligned")

light <- map(light, function(data) data |> mutate(across(MEDI_glasses:LIGHT_wrist, function(x) {
  ifelse(is.na(MEDI_glasses) | is.na(MEDI_chest) | is.na(MEDI_wrist), NA, x)
})))
flow <- bind_rows(flow, sample_count(light, "three_position_concurrent"))

sleepdiary_adj <- map(sleepdiary, function(x) x |> select(Id, sleepprep, wake) |> group_by(Id) |>
  pivot_longer(-Id, names_to = "sleep", values_to = "Datetime") |>
  sc2interval(Statechange.colname = sleep, starting.state = "wake") |>
  sleep_int2Brown(sleep.state = "sleepprep", Brown.day = "wake", Brown.evening = "pre-sleep", Brown.night = "sleep") |>
  mutate(sleep = case_when(is.na(sleep) & State.Brown == "pre-sleep" ~ "wake", .default = sleep)))
wearlog_adj <- map(wearlog, function(x) x |> select(Id, start, end, wear = state))
light <- imap(light, function(x, site) x |> add_states(sleepdiary_adj[[site]], start = Interval, end = Interval) |> add_states(wearlog_adj[[site]]))

light <- map(light, function(x) x |> mutate(
  across(c(starts_with("MEDI"), starts_with("LIGHT")), function(y) replace_when(y,
    wear == "off" & (State.Brown != "sleep" | is.na(State.Brown)) ~ NA,
    y >= 100000 ~ NA)),
  across(MEDI_glasses:LIGHT_wrist, function(z) ifelse(is.na(MEDI_glasses) | is.na(MEDI_chest) | is.na(MEDI_wrist), NA, z))))
flow <- bind_rows(flow, sample_count(light, "nonwear_and_range_filtered"))

light_flat <- structure(light, class = "melidos_data") |> flatten_data() |> group_by(site, Id)
light_fin <- light_flat |> cut_Datetime(unit = "1 hour", group_by = TRUE, type = "floor") |>
  remove_partial_data(MEDI_glasses, threshold.missing = 0.5) |> ungroup(Datetime.rounded) |>
  select(-Datetime.rounded) |> add_Date_col(group.by = TRUE) |> gap_handler(full.days = TRUE) |>
  remove_partial_data(MEDI_glasses, threshold.missing = 0.2) |> ungroup(Date)
flow <- bind_rows(flow, light_fin |> distinct(site, Id, Date) |> group_by(site) |>
  summarise(stage = "hour_day_completeness", n_rows = NA_integer_, n_participants = n_distinct(Id),
            n_participant_days = n(), .groups = "drop"))

metrics_data <- light_fin |> distinct(site, Id, Datetime, .keep_all = TRUE) |>
  pivot_longer(MEDI_glasses:LIGHT_wrist, names_sep = "_", names_to = c("metric", "position")) |>
  pivot_wider(values_from = value, names_from = metric)

datetime_2_numeric <- function(x) x |> mutate(across(where(is.POSIXct), function(y) as.numeric(hms::as_hms(y))))
metrics <- metrics_data |> group_by(site, Id, Date, position) |> summarise(
  duration_above_threshold(MEDI, Datetime, "above", 10, na.rm = TRUE, as.df = TRUE),
  duration_above_threshold(MEDI, Datetime, "above", 250, na.rm = TRUE, as.df = TRUE),
  duration_above_threshold(MEDI, Datetime, "above", 1000, na.rm = TRUE, as.df = TRUE),
  period_above_threshold(MEDI, Datetime, "above", 10, na.rm = TRUE, as.df = TRUE),
  period_above_threshold(MEDI, Datetime, "above", 250, na.rm = TRUE, as.df = TRUE),
  period_above_threshold(MEDI, Datetime, "above", 1000, na.rm = TRUE, as.df = TRUE),
  pulses_above_threshold(MEDI, Datetime, threshold = 250, na.rm = TRUE, as.df = TRUE) |> datetime_2_numeric(),
  pulses_above_threshold(MEDI, Datetime, threshold = 1000, na.rm = TRUE, as.df = TRUE) |> datetime_2_numeric(),
  bright_dark_period(log_zero_inflated(MEDI), Datetime, "brightest", "10 hours", as.df = TRUE, na.rm = TRUE) |> datetime_2_numeric(),
  bright_dark_period(log_zero_inflated(MEDI), Datetime, "darkest", "10 hours", as.df = TRUE, loop = TRUE, na.rm = TRUE) |> datetime_2_numeric(),
  timing_above_threshold(MEDI, Datetime, "above", 10, as.df = TRUE) |> datetime_2_numeric(),
  timing_above_threshold(MEDI, Datetime, "above", 250, as.df = TRUE) |> datetime_2_numeric(),
  frequency_crossing_threshold(MEDI, 250, na.rm = TRUE, as.df = TRUE),
  timing_above_threshold(MEDI, Datetime, "above", 1000, as.df = TRUE) |> datetime_2_numeric(),
  barroso_lighting_metrics(MEDI, Datetime, loop = TRUE, na.rm = TRUE, as.df = TRUE),
  centroidLE(MEDI, Datetime, na.rm = TRUE, as.df = TRUE) |> datetime_2_numeric(),
  disparity_index(MEDI, TRUE, TRUE), midpointCE(MEDI, Datetime, TRUE, TRUE) |> datetime_2_numeric(),
  mean_MEDI = mean(log_zero_inflated(MEDI), na.rm = TRUE),
  nvRD = mean(nvRD(MEDI, LIGHT, Datetime), na.rm = TRUE), dose(MEDI, Datetime, na.rm = TRUE, as.df = TRUE),
  MDER = median(MEDI / LIGHT, na.rm = TRUE), .groups = "drop") |>
  mutate(across(where(lubridate::is.duration), as.numeric)) |>
  pivot_longer(-c(site, Id, Date, position), names_to = "metric")

metrics_multiday <- metrics_data |> group_by(site, Id, position) |> summarise(
  interdaily_stability(log_zero_inflated(MEDI), Datetime, na.rm = TRUE, as.df = TRUE),
  intradaily_variability(log_zero_inflated(MEDI), Datetime, na.rm = TRUE, as.df = TRUE)) |>
  pivot_longer(-c(site, Id, position), names_to = "metric")
metrics <- bind_rows(metrics, metrics_multiday)

dir.create(file.path("results", "core", "cache", "upstream"), recursive = TRUE, showWarnings = FALSE)
dir.create("results/diagnostics", recursive = TRUE, showWarnings = FALSE)
dir.create("logs", recursive = TRUE, showWarnings = FALSE)
saveRDS(light_fin, file.path("results", "core", "cache", "upstream", "zauner_primary_cleaned.rds"))
saveRDS(metrics_data, file.path("results", "core", "cache", "upstream", "zauner_metrics_input.rds"))
save(metrics, file = file.path("results", "core", "cache", "upstream", "zauner_metrics.RData"))
write.csv(flow, "logs/upstream_sample_flow.csv", row.names = FALSE, na = "")
catalog <- metrics |> distinct(metric) |> arrange(metric)
write.csv(catalog, "results/diagnostics/metric_catalog.csv", row.names = FALSE)
writeLines(capture.output(sessionInfo()), "logs/sessionInfo_reproduction.txt")

report <- c("# Upstream reproduction report", "", sprintf("Run: %s", format(Sys.time())), "",
  sprintf("Sites reproduced: %s", paste(sites, collapse = ", ")),
  sprintf("Distinct emitted metric fields: %s", nrow(catalog)), "",
  "The implementation follows Zauner v0.9.9 `JZ_metric_preparation.qmd` for the primary scenario.",
  "Its three-position concurrency rule is retained only for this upstream reproduction; it is not a default rule for later measurement-sufficiency operators.")
writeLines(report, "logs/upstream_reproduction_report.md")
if (nrow(catalog) != 54L) stop(sprintf("Expected 54 emitted metric fields, got %s", nrow(catalog)))
