source("scripts/utils/melidos_io.R")
suppressPackageStartupMessages({
  library(tidyverse)
})

dir.create(file.path("results", "core", "cache"), recursive = TRUE, showWarnings = FALSE)
dir.create("logs", recursive = TRUE, showWarnings = FALSE)

raw_spans <- map_dfr(melidos_sites(), function(site_i) {
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

trial_meta <- map_dfr(melidos_sites(), function(site_i) {
  path <- raw_data_path(site_i, "trial_times")
  if (!file.exists(path)) {
    warning("trial_times unavailable for ", site_i, "; protocol audit metadata will be absent")
    return(tibble())
  }
  tt <- tryCatch(
    load_raw_file(path, "trial_times") |> ungroup(),
    error = function(e) {
      warning("Could not read trial_times for ", site_i, ": ", conditionMessage(e))
      NULL
    }
  )
  if (is.null(tt)) return(tibble())
  required <- c("Id", "start", "end")
  missing <- setdiff(required, names(tt))
  if (length(missing)) {
    warning("trial_times for ", site_i, " missing columns: ", paste(missing, collapse = ", "))
    return(tibble())
  }
  tz_i <- lubridate::tz(tt$start)
  if (is.null(tz_i) || !length(tz_i) || is.na(tz_i) || !nzchar(tz_i)) tz_i <- "UTC"

  tt |>
    transmute(
      site = site_i,
      Id = as.character(Id),
      protocol_start = as.POSIXct(start),
      protocol_end = as.POSIXct(end)
    ) |>
    filter(!is.na(Id), !is.na(protocol_start), !is.na(protocol_end)) |>
    group_by(site, Id) |>
    summarise(
      protocol_start = min(protocol_start, na.rm = TRUE),
      protocol_end = max(protocol_end, na.rm = TRUE),
      .groups = "drop"
    ) |>
    mutate(
      protocol_start_date = as.Date(protocol_start, tz = tz_i),
      protocol_end_date = as.Date(protocol_end, tz = tz_i),
      protocol_span_hours = as.numeric(
        difftime(protocol_end, protocol_start, units = "hours")
      ),
      protocol_calendar_date_count = as.integer(protocol_end_date - protocol_start_date) + 1L,
      protocol_nominal_7d = is.finite(protocol_span_hours) &
        protocol_span_hours >= 6.5 * 24 & protocol_span_hours <= 7.5 * 24
    )
})
if (!nrow(trial_meta)) {
  trial_meta <- tibble(
    site = character(), Id = character(),
    protocol_start = as.POSIXct(character()), protocol_end = as.POSIXct(character()),
    protocol_start_date = as.Date(character()), protocol_end_date = as.Date(character()),
    protocol_span_hours = double(), protocol_calendar_date_count = integer(),
    protocol_nominal_7d = logical()
  )
}


if (anyDuplicated(trial_meta[c("site", "Id")])) {
  stop("Protocol trial-time metadata is not unique by site + participant")
}

participant_meta <- raw_spans |>
  left_join(trial_meta, by = c("site", "Id")) |>
  mutate(
    protocol_metadata_available =
      !is.na(protocol_start) & !is.na(protocol_end) &
      !is.na(protocol_start_date) & !is.na(protocol_end_date)
  )

missing_protocol <- participant_meta |>
  filter(!protocol_metadata_available) |>
  select(site, Id, raw_eye_recording_start, raw_eye_recording_end)
write.csv(
  missing_protocol,
  "logs/protocol_metadata_missing_participants.csv",
  row.names = FALSE, na = ""
)
if (nrow(missing_protocol)) {
  warning(
    nrow(missing_protocol),
    " near-corneal participants lack trial_times metadata; complete-day duration windows remain usable, but protocol audit/sensitivity fields are unavailable."
  )
}

saveRDS(raw_spans, file.path("results", "core", "cache", "raw_eye_recording_spans.rds"), compress = FALSE)
saveRDS(
  participant_meta,
file.path("results", "core", "cache", "protocol_participant_metadata.rds"),
  compress = FALSE
)

write.csv(
  raw_spans,
  "logs/raw_eye_recording_spans.csv",
  row.names = FALSE, na = ""
)
write.csv(
  participant_meta,
  "logs/protocol_participant_metadata.csv",
  row.names = FALSE, na = ""
)

message("Raw near-corneal recording spans preserved for ", nrow(raw_spans), " participants")
message(
  "Protocol trial metadata available for ",
  sum(participant_meta$protocol_metadata_available), "/", nrow(participant_meta),
  " participants"
)
