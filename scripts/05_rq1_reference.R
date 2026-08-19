source("scripts/utils/melidos_io.R")
source("scripts/utils/rq1_metrics.R")
suppressPackageStartupMessages({
  library(tidyverse)
  library(melidosData)
  library(readxl)
})

dir.create("data/interim/rq1", recursive = TRUE, showWarnings = FALSE)
dir.create("results/rq1", recursive = TRUE, showWarnings = FALSE)
dir.create("results/diagnostics", recursive = TRUE, showWarnings = FALSE)

# Preserve the authorized upstream numerical discrepancy as a measured diagnostic.
up <- new.env(parent = emptyenv()); ours <- new.env(parent = emptyenv())
load("external/zauner_position/data/prepared_metrics.RData", envir = up)
load("data/derived/zauner_metrics.RData", envir = ours)
ref_up <- up$metrics |> filter(site %in% unique(ours$metrics$site))
diff_rows <- tibble(metric = ours$metrics$metric, abs_delta = abs(ref_up$value - ours$metrics$value)) |>
  filter(is.finite(abs_delta), abs_delta > 1e-12)
write.csv(diff_rows |> summarise(n = n(), maximum = max(abs_delta), median = median(abs_delta),
                                 p95 = quantile(abs_delta, .95)),
          "results/diagnostics/upstream_difference_summary.csv", row.names = FALSE)
write.csv(diff_rows |> group_by(metric) |> summarise(n = n(), maximum = max(abs_delta),
  median = median(abs_delta), p95 = quantile(abs_delta, .95), .groups = "drop") |>
  arrange(desc(maximum)), "results/diagnostics/upstream_difference_by_metric.csv", row.names = FALSE)

sites <- melidos_sites()
metric_types <- read_excel("external/zauner_position/data/metric_types.xlsx") |>
  transmute(metric = name, metric_class = metric_type)
write.csv(metric_types, "data/interim/rq1/metric_types.csv", row.names = FALSE)

annotate_and_filter <- function(light, sleep, wear) {
  sleep_adj <- sleep |> select(Id, sleepprep, wake) |> group_by(Id) |>
    pivot_longer(-Id, names_to = "sleep", values_to = "Datetime") |>
    sc2interval(Statechange.colname = sleep, starting.state = "wake") |>
    sleep_int2Brown(sleep.state = "sleepprep", Brown.day = "wake", Brown.evening = "pre-sleep", Brown.night = "sleep") |>
    mutate(sleep = case_when(is.na(sleep) & State.Brown == "pre-sleep" ~ "wake", .default = sleep))
  wear_adj <- wear |> select(Id, start, end, wear = state)
  light |> add_states(sleep_adj, start = Interval, end = Interval) |> add_states(wear_adj) |>
    mutate(across(c(MEDI, LIGHT), function(y) replace_when(y,
      wear == "off" & (State.Brown != "sleep" | is.na(State.Brown)) ~ NA,
      y >= 100000 ~ NA)))
}

complete_days <- function(x, value_col = "MEDI") {
  value_sym <- rlang::sym(value_col)
  x |> group_by(site, Id) |>
    cut_Datetime(unit = "1 hour", group_by = TRUE, type = "floor") |>
    remove_partial_data(!!value_sym, threshold.missing = .5) |>
    ungroup(Datetime.rounded) |> select(-Datetime.rounded) |>
    add_Date_col(group.by = TRUE) |> gap_handler(full.days = TRUE) |>
    remove_partial_data(!!value_sym, threshold.missing = .2) |> ungroup(Date)
}

flow <- list(); duration_audit <- list()
for (site in sites) {
  message("RQ1 eye reference: ", site)
  eye0 <- load_raw_file(raw_data_path(site, "light_glasses"), "light_glasses")
  sleep <- load_raw_file(raw_data_path(site, "sleepdiaries"), "sleepdiaries")
  wear <- load_raw_file(raw_data_path(site, "wearlog"), "wearlog")
  before <- eye0 |> mutate(Date = as.Date(Datetime))
  eye <- eye0 |> select(Id, Datetime, MEDI, LIGHT) |> mutate(site = site, .before = 1) |>
    annotate_and_filter(sleep, wear) |> complete_days()
  saveRDS(eye, file.path("data/interim/rq1", paste0("eye_clean_", site, ".rds")), compress = FALSE)
  flow[[site]] <- tibble(dimension = "eye_reference", configuration = "eye_10s_MEDI", site = site,
    participants_before = n_distinct(before$Id), participants_after = n_distinct(eye$Id),
    participant_days_before = n_distinct(interaction(before$Id, before$Date)),
    participant_days_after = n_distinct(interaction(eye$Id, eye$Date)),
    n_analysis_units = n_distinct(interaction(eye$Id, eye$Date)), exclusions_unavailable = NA_character_)
  duration_audit[[site]] <- eye |> distinct(site, Id, Date) |> count(site, Id, name = "n_valid_days")
  rm(eye0, eye, before, sleep, wear); gc()
}
write.csv(bind_rows(flow), "results/rq1/rq1_reference_sample_flow.csv", row.names = FALSE, na = "")
duration_audit <- bind_rows(duration_audit)
write.csv(duration_audit, "results/diagnostics/rq1_duration_valid_days.csv", row.names = FALSE)
write.csv(duration_audit |> count(site, n_valid_days, name = "n_participants"),
          "results/diagnostics/rq1_duration_cohort_audit.csv", row.names = FALSE)
writeLines(c("Duration eligibility rule: exactly seven valid near-corneal days after upstream hour/day completeness.",
             "Participants with more than seven valid days are excluded because no unambiguous canonical seven-day window is encoded in the harmonized source."),
           "results/diagnostics/rq1_duration_window_rule.txt")
writeLines(capture.output(sessionInfo()), "logs/sessionInfo_rq1.txt")
