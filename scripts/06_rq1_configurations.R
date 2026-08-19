source("scripts/utils/melidos_io.R")
source("scripts/utils/rq1_metrics.R")
suppressPackageStartupMessages({ library(tidyverse); library(melidosData) })

dir.create("data/interim/rq1/metrics", recursive = TRUE, showWarnings = FALSE)

annotate_pair <- function(light, sleep, wear) {
  sleep_adj <- sleep |> select(Id, sleepprep, wake) |> group_by(Id) |>
    pivot_longer(-Id, names_to = "sleep", values_to = "Datetime") |>
    sc2interval(Statechange.colname = sleep, starting.state = "wake") |>
    sleep_int2Brown(sleep.state = "sleepprep", Brown.day = "wake", Brown.evening = "pre-sleep", Brown.night = "sleep") |>
    mutate(sleep = case_when(is.na(sleep) & State.Brown == "pre-sleep" ~ "wake", .default = sleep))
  wear_adj <- wear |> select(Id, start, end, wear = state)
  light |> add_states(sleep_adj, start = Interval, end = Interval) |> add_states(wear_adj) |>
    mutate(across(c(MEDI_eye, LIGHT_eye, MEDI_candidate, LIGHT_candidate), function(y) replace_when(y,
      wear == "off" & (State.Brown != "sleep" | is.na(State.Brown)) ~ NA, y >= 100000 ~ NA)),
      across(c(MEDI_eye, LIGHT_eye, MEDI_candidate, LIGHT_candidate),
             function(y) ifelse(is.na(MEDI_eye) | is.na(MEDI_candidate), NA, y)))
}

complete_pair <- function(x) {
  x |> group_by(site, Id) |> cut_Datetime(unit = "1 hour", group_by = TRUE, type = "floor") |>
    remove_partial_data(MEDI_eye, threshold.missing = .5) |> ungroup(Datetime.rounded) |>
    select(-Datetime.rounded) |> add_Date_col(group.by = TRUE) |> gap_handler(full.days = TRUE) |>
    remove_partial_data(MEDI_eye, threshold.missing = .2) |> ungroup(Date)
}

flow <- list(); missing_support <- list(); metric_files <- character()

# Eye-only optical and temporal configurations.
for (site in melidos_sites()) {
  message("RQ1 optical/temporal metrics: ", site)
  eye <- readRDS(file.path("data/interim/rq1", paste0("eye_clean_", site, ".rds"))) |>
    distinct(site, Id, Datetime, .keep_all = TRUE)
  ref <- eye |> transmute(site, Id, Date, Datetime, MEDI, LIGHT, configuration = "eye_MEDI_10s")
  opt <- eye |> transmute(site, Id, Date, Datetime, MEDI = LIGHT, LIGHT = NA_real_, configuration = "eye_LIGHT_10s")
  m <- bind_rows(rq1_all_metrics(ref, TRUE), rq1_all_metrics(opt, FALSE))
  f <- file.path("data/interim/rq1/metrics", paste0("eye_configs_", site, ".rds")); saveRDS(m, f); metric_files <- c(metric_files, f)
  flow[[paste0("opt_", site)]] <- tibble(dimension = "optical", configuration = "eye_LIGHT_10s", site,
    participants_before = n_distinct(eye$Id), participants_after = n_distinct(eye$Id),
    participant_days_before = n_distinct(interaction(eye$Id, eye$Date)), participant_days_after = n_distinct(interaction(eye$Id, eye$Date)),
    n_analysis_units = n_distinct(interaction(eye$Id, eye$Date)), exclusions_unavailable = "MDER and nvRD unavailable for one-channel LIGHT")

  temporal <- list(ref)
  for (spec in list(c("30s", 30), c("1min", 60), c("5min", 300), c("15min", 900), c("30min", 1800))) {
    label <- spec[[1]]; seconds <- as.numeric(spec[[2]])
    agg <- eye |> mutate(bin = as.POSIXct(floor(as.numeric(Datetime) / seconds) * seconds,
                                          origin = "1970-01-01", tz = attr(Datetime, "tzone") %||% "UTC")) |>
      group_by(site, Id, Date, bin) |>
      summarise(MEDI = if (all(is.na(MEDI))) NA_real_ else mean(MEDI, na.rm = TRUE),
                LIGHT = if (all(is.na(LIGHT))) NA_real_ else mean(LIGHT, na.rm = TRUE),
                n_observed = sum(!is.na(MEDI)), .groups = "drop") |>
      rename(Datetime = bin) |> mutate(configuration = paste0("eye_MEDI_", label))
    missing_support[[paste(site, label)]] <- agg |> summarise(site = first(site), configuration = first(configuration),
      n_bins = n(), n_fully_missing_bins = sum(is.na(MEDI)), n_values_manufactured = sum(is.na(MEDI) & n_observed > 0))
    temporal[[length(temporal) + 1L]] <- agg |> select(-n_observed)
  }
  tm <- map_dfr(temporal[-1], function(x) {
    coarse_seconds <- median(diff(sort(unique(as.numeric(x$Datetime)))), na.rm = TRUE)
    rq1_all_metrics(x, TRUE, include_pulses = coarse_seconds < 300)
  })
  ft <- file.path("data/interim/rq1/metrics", paste0("temporal_", site, ".rds")); saveRDS(tm, ft); metric_files <- c(metric_files, ft)
  for (cfg in unique(tm$configuration)) flow[[paste(cfg, site)]] <- tibble(dimension = "temporal", configuration = cfg, site,
    participants_before = n_distinct(eye$Id), participants_after = n_distinct(tm$Id[tm$configuration == cfg]),
    participant_days_before = n_distinct(interaction(eye$Id, eye$Date)),
    participant_days_after = n_distinct(interaction(tm$Id[tm$configuration == cfg], tm$Date[tm$configuration == cfg], drop = TRUE)),
    n_analysis_units = sum(tm$configuration == cfg & !is.na(tm$value)), exclusions_unavailable = NA_character_)
  rm(eye, ref, opt, m, tm, temporal); gc()
}

# Pairwise placement supports, independently for chest and wrist.
for (position in c("chest", "wrist")) for (site in setdiff(melidos_sites(), "MPI")) {
  message("RQ1 placement ", position, ": ", site)
  eye0 <- load_raw_file(raw_data_path(site, "light_glasses"), "light_glasses")
  cand0 <- load_raw_file(raw_data_path(site, paste0("light_", position)), paste0("light_", position))
  sleep <- load_raw_file(raw_data_path(site, "sleepdiaries"), "sleepdiaries")
  wear <- load_raw_file(raw_data_path(site, "wearlog"), "wearlog")
  aligned <- eye0 |> select(Id, Datetime, MEDI, LIGHT) |>
    data2reference(cand0, Reference.column = MEDI_candidate) |>
    data2reference(cand0, Data.column = LIGHT, Reference.column = LIGHT_candidate) |>
    rename(MEDI_eye = MEDI, LIGHT_eye = LIGHT) |> mutate(site = site, .before = 1)
  pair <- annotate_pair(aligned, sleep, wear) |> complete_pair() |> distinct(site, Id, Datetime, .keep_all = TRUE)
  long <- bind_rows(
    pair |> transmute(site, Id, Date, Datetime, MEDI = MEDI_eye, LIGHT = LIGHT_eye, configuration = paste0("eye_pair_", position)),
    pair |> transmute(site, Id, Date, Datetime, MEDI = MEDI_candidate, LIGHT = LIGHT_candidate, configuration = position))
  pm <- rq1_all_metrics(long, TRUE)
  fp <- file.path("data/interim/rq1/metrics", paste0("placement_", position, "_", site, ".rds")); saveRDS(pm, fp); metric_files <- c(metric_files, fp)
  flow[[paste(position, site)]] <- tibble(dimension = "placement", configuration = position, site,
    participants_before = n_distinct(intersect(eye0$Id, cand0$Id)), participants_after = n_distinct(pair$Id),
    participant_days_before = n_distinct(interaction(aligned$Id, as.Date(aligned$Datetime))),
    participant_days_after = n_distinct(interaction(pair$Id, pair$Date)),
    n_analysis_units = sum(pm$configuration == position & !is.na(pm$value)), exclusions_unavailable = NA_character_)
  rm(eye0, cand0, sleep, wear, aligned, pair, long, pm); gc()
}

write.csv(bind_rows(flow), "results/rq1/rq1_sample_flow_partial.csv", row.names = FALSE, na = "")
write.csv(bind_rows(missing_support), "results/diagnostics/rq1_missing_support_audit.csv", row.names = FALSE)
saveRDS(metric_files, "data/interim/rq1/metric_files.rds")
