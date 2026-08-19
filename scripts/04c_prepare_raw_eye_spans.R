source("scripts/utils/melidos_io.R")
suppressPackageStartupMessages({
  library(tidyverse)
})

out <- map_dfr(melidos_sites(), function(site_i) {
  x <- load_raw_file(raw_data_path(site_i, "light_glasses"), "light_glasses")
  x |>
    filter(!is.na(Id), !is.na(Datetime)) |>
    group_by(Id) |>
    summarise(
      raw_eye_recording_start = min(Datetime),
      raw_eye_recording_end = max(Datetime),
      raw_eye_span_hours = as.numeric(
        difftime(max(Datetime), min(Datetime), units = "hours")
      ),
      raw_eye_calendar_day_count = n_distinct(as.Date(Datetime)),
      .groups = "drop"
    ) |>
    mutate(site = site_i, .before = 1)
})

dir.create("data/interim/core", recursive = TRUE, showWarnings = FALSE)
saveRDS(out, "data/interim/core/raw_eye_recording_spans.rds", compress = FALSE)
write.csv(
  out |>
    select(site, Id, raw_eye_recording_start, raw_eye_recording_end,
           raw_eye_span_hours, raw_eye_calendar_day_count),
  "logs/raw_eye_recording_spans.csv", row.names = FALSE, na = ""
)

message("Raw near-corneal recording spans preserved for ", nrow(out), " participants")
