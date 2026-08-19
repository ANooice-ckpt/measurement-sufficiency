source("scripts/utils/rq1_metrics.R")
suppressPackageStartupMessages({ library(tidyverse) })

audit <- read.csv("results/diagnostics/rq1_duration_valid_days.csv")
eligible <- audit |> filter(n_valid_days == 7)
daily_all <- list(); multi_all <- list(); flow <- list()

for (i in seq_len(nrow(eligible))) {
  site_i <- eligible$site[i]; id_i <- eligible$Id[i]
  message("RQ1 duration: ", site_i, " / ", id_i)
  base_metrics <- readRDS(file.path("data/interim/rq1/metrics", paste0("eye_configs_", site_i, ".rds"))) |>
    filter(configuration == "eye_MEDI_10s", Id == id_i, !is.na(Date))
  dates <- sort(unique(base_metrics$Date))
  eye <- readRDS(file.path("data/interim/rq1", paste0("eye_clean_", site_i, ".rds"))) |>
    filter(Id == id_i, Date %in% dates) |> distinct(site, Id, Datetime, .keep_all = TRUE)
  for (d in 1:7) {
    subsets <- combn(dates, d, simplify = FALSE)
    for (j in seq_along(subsets)) {
      selected <- as.Date(subsets[[j]], origin = "1970-01-01")
      sid <- sprintf("%s_%s_d%d_s%03d", site_i, id_i, d, j)
      daily_all[[length(daily_all) + 1L]] <- base_metrics |> filter(Date %in% selected) |>
        group_by(site, Id, metric) |> summarise(value = mean(value, na.rm = TRUE), .groups = "drop") |>
        mutate(Date = as.Date(NA), configuration = paste0("duration_", d, "d"), subset_id = sid,
               n_days = d, analysis_unit_id = sid)
      md <- eye |> filter(Date %in% selected) |>
        transmute(site, Id, Date, Datetime, MEDI, LIGHT, configuration = paste0("duration_", d, "d")) |>
        rq1_multiday_metrics() |>
        mutate(subset_id = sid, n_days = d, analysis_unit_id = sid)
      multi_all[[length(multi_all) + 1L]] <- md
    }
  }
  flow[[length(flow) + 1L]] <- tibble(dimension = "duration", configuration = "duration_1d_to_6d", site = site_i,
    participants_before = sum(audit$site == site_i), participants_after = 1L,
    participant_days_before = sum(audit$n_valid_days[audit$site == site_i]), participant_days_after = 7L,
    n_analysis_units = 126L, exclusions_unavailable = "eligible only when exactly 7 valid days")
  rm(base_metrics, eye); gc()
}
duration_values <- bind_rows(daily_all, multi_all) |>
  select(site, Id, Date, configuration, subset_id, n_days, analysis_unit_id, metric, value)
saveRDS(duration_values, "data/interim/rq1/metrics/duration_metric_values.rds")
write.csv(bind_rows(flow), "results/rq1/rq1_duration_sample_flow.csv", row.names = FALSE)
