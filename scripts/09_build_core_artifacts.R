source("scripts/utils/melidos_io.R")
source("scripts/utils/rq1_metrics.R")
source("scripts/utils/core_artifacts.R")
source("scripts/utils/core_context.R")
source("scripts/utils/weather_era5.R")
suppressPackageStartupMessages({
  library(tidyverse)
  library(melidosData)
  library(readxl)
})

required_r <- package_version("4.5.0")
if (getRversion() != required_r) {
  stop(sprintf("Core artifact build is pinned to R 4.5.0; current runtime is %s", getRversion()))
}

Sys.setenv(OMP_NUM_THREADS = "1", OPENBLAS_NUM_THREADS = "1", MKL_NUM_THREADS = "1")
workers <- suppressWarnings(as.integer(Sys.getenv("CORE_WORKERS", unset = "16")))
if (!is.finite(workers) || workers < 1L) workers <- 1L
if (.Platform$OS.type == "windows" && workers > 1L) {
  message("CORE_WORKERS>1 uses forked workers only on Unix-like systems; falling back to 1 on Windows.")
  workers <- 1L
}
force_rebuild <- identical(Sys.getenv("CORE_FORCE", unset = "0"), "1")

dir.create("data/interim/core/supports", recursive = TRUE, showWarnings = FALSE)
dir.create("data/interim/core/metrics", recursive = TRUE, showWarnings = FALSE)
dir.create("data/interim/core/context", recursive = TRUE, showWarnings = FALSE)
dir.create("data/interim/core/weather", recursive = TRUE, showWarnings = FALSE)
dir.create("data/derived/core", recursive = TRUE, showWarnings = FALSE)
dir.create("logs", recursive = TRUE, showWarnings = FALSE)

message("Core artifact build: R ", getRversion(), ", workers=", workers, ", force=", force_rebuild)

metric_types <- read_excel("external/zauner_position/data/metric_types.xlsx") |>
  transmute(metric = name, metric_class = metric_type)
if (n_distinct(metric_types$metric) != 54L) stop("Expected 54 metric definitions")

site_meta <- core_site_metadata()
expected_weather_files <- file.path("data", "raw", "era5", paste0(site_meta$site, ".csv"))
missing_weather <- expected_weather_files[!file.exists(expected_weather_files)]
if (length(missing_weather)) {
  stop("Missing ERA5 files: ", paste(missing_weather, collapse = ", "))
}

# Stage 0: parse and QC all ERA5 payloads before starting the expensive metric run.
message("[weather] validate, summarize, and interpolate ERA5")
weather_build_one <- function(i) {
  site <- site_meta$site[i]
  timezone <- site_meta$timezone[i]
  prefix <- file.path("data/interim/core/weather", site)
  paths <- c(
    hourly = paste0(prefix, "__hourly.rds"),
    daily = paste0(prefix, "__daily.rds"),
    minute = paste0(prefix, "__1min.rds"),
    qc = paste0(prefix, "__qc.rds")
  )
  if (!force_rebuild && all(file.exists(paths))) return(paths)

  message("ERA5: ", site)
  w <- era5_build_site(site, timezone)
  saveRDS(w$hourly, paths[["hourly"]], compress = FALSE)
  saveRDS(w$daily, paths[["daily"]], compress = FALSE)
  saveRDS(w$minute, paths[["minute"]], compress = FALSE)
  saveRDS(w$qc, paths[["qc"]], compress = FALSE)
  paths
}
weather_blocks <- lapply(seq_len(nrow(site_meta)), weather_build_one)
weather_daily_paths <- vapply(weather_blocks, `[[`, character(1), "daily")
weather_minute_paths <- vapply(weather_blocks, `[[`, character(1), "minute")
weather_qc_paths <- vapply(weather_blocks, `[[`, character(1), "qc")

weather_daily <- map_dfr(weather_daily_paths, readRDS)
weather_1min <- map_dfr(weather_minute_paths, readRDS)
weather_qc <- map_dfr(weather_qc_paths, readRDS)
readr::write_csv(weather_1min, "data/derived/core/weather_1min.csv.gz", na = "")
readr::write_csv(weather_qc, "logs/era5_qc.csv", na = "")

# Stage 1: prepare explicit support lattices. Pairwise supports preserve maximum
# RQ1 samples; all-position supports are a reserve for later joint comparisons.
support_grid <- core_support_grid()
prepare_one <- function(i) {
  row <- support_grid[i, ]
  path <- file.path("data/interim/core/supports", paste0(row$site, "__", row$support_id, ".rds"))
  if (!force_rebuild && file.exists(path)) return(path)
  message("prepare support: ", row$site, " / ", row$support_id)
  x <- core_prepare_support(row$site, row$support_id)
  if (is.null(x)) return(NA_character_)
  saveRDS(x, path, compress = FALSE)
  path
}
idx <- seq_len(nrow(support_grid))
support_paths <- if (workers > 1L) {
  parallel::mclapply(idx, prepare_one, mc.cores = min(workers, length(idx)), mc.preschedule = FALSE)
} else lapply(idx, prepare_one)
support_paths <- unlist(support_paths, use.names = FALSE)
support_paths <- support_paths[!is.na(support_paths) & file.exists(support_paths)]
if (!length(support_paths)) stop("No support blocks were produced")

# Stage 2: compute every observable placement x optical x temporal-resolution
# configuration. Blocks persist independently so a pre-empted cloud run can resume.
compute_one <- function(path) {
  key <- tools::file_path_sans_ext(basename(path))
  metric_out <- file.path("data/interim/core/metrics", paste0(key, "__metrics.rds"))
  context_out <- file.path("data/interim/core/context", paste0(key, "__context.rds"))

  if (force_rebuild || !file.exists(metric_out)) {
    message("compute metric block: ", key)
    m <- core_compute_support_metrics(path)
    saveRDS(m, metric_out, compress = FALSE)
  }
  if (force_rebuild || !file.exists(context_out)) {
    message("compute context block: ", key)
    cx <- core_config_daily_context(path)
    saveRDS(cx, context_out, compress = FALSE)
  }
  c(metric = metric_out, context = context_out)
}
block_paths <- if (workers > 1L) {
  parallel::mclapply(
    support_paths, compute_one,
    mc.cores = min(workers, length(support_paths)), mc.preschedule = FALSE
  )
} else lapply(support_paths, compute_one)
metric_paths <- vapply(block_paths, `[[`, character(1), "metric")
context_paths <- vapply(block_paths, `[[`, character(1), "context")

# Stage 3: merge expensive metric values into the durable configuration cube.
emitted <- map_dfr(metric_paths, readRDS)
metric_cube <- core_finalize_metric_cube(emitted, metric_types)
metric_key <- c(
  "support_id", "site", "Id", "analysis_unit_type", "analysis_unit_id", "Date",
  "placement", "optical", "resolution_s", "config_id", "metric"
)
duplicate_metric_keys <- metric_cube |>
  group_by(across(all_of(metric_key))) |>
  summarise(n = n(), .groups = "drop") |>
  filter(n > 1L)
if (nrow(duplicate_metric_keys)) {
  write.csv(duplicate_metric_keys, "logs/core_metric_duplicate_keys.csv", row.names = FALSE)
  stop("Duplicate scientific keys in metric_cube; inspect logs/core_metric_duplicate_keys.csv")
}
readr::write_csv(metric_cube, "data/derived/core/metric_cube.csv.gz", na = "")

# Stage 4: configuration-level participant-day context. ERA5 daily summaries are
# attached by official site + local calendar date. Continuous weather remains in
# weather_1min to avoid redundant expansion across metric/configuration rows.
config_context <- map_dfr(context_paths, readRDS)
unit_context <- config_context |>
  left_join(site_meta, by = "site") |>
  left_join(weather_daily, by = c("site", "Date")) |>
  mutate(
    # Use the actual number of local hourly accumulation intervals. This is 23/25
    # on daylight-saving transition days rather than forcing a 24-hour denominator.
    era5_ssrd_daily_mean_w_m2 = if_else(
      is.finite(era5_ssrd_sum_mj_m2) & era5_hours_accumulated > 0,
      era5_ssrd_sum_mj_m2 * 1e6 / (era5_hours_accumulated * 3600), NA_real_
    ),
    era5_fdir_daily_mean_w_m2 = if_else(
      is.finite(era5_fdir_sum_mj_m2) & era5_hours_accumulated > 0,
      era5_fdir_sum_mj_m2 * 1e6 / (era5_hours_accumulated * 3600), NA_real_
    ),
    era5_complete_local_day =
      era5_hours_instantaneous == era5_hours_accumulated &
      era5_hours_instantaneous %in% c(23L, 24L, 25L),
    year = lubridate::year(Date),
    month = lubridate::month(Date),
    day_of_year = lubridate::yday(Date),
    weekday = as.character(lubridate::wday(Date, label = TRUE, abbr = TRUE)),
    era5_context_available = !is.na(era5_hours_instantaneous)
  ) |>
  select(
    support_id, site, Id, analysis_unit_type, analysis_unit_id, Date,
    valid_day_index, support_valid_day_count,
    raw_eye_recording_start, raw_eye_recording_end,
    raw_eye_span_hours, raw_eye_calendar_day_count,
    support_recording_start, support_recording_end, support_span_hours,
    placement, optical, resolution_s, is_primary_resolution, config_id,
    city, country, timezone, latitude, longitude,
    expected_values, valid_values, valid_fraction, n_missing_blocks,
    largest_missing_gap_s, first_valid_time, last_valid_time,
    year, month, day_of_year, weekday, era5_context_available,
    starts_with("era5_"), starts_with("isiv_h"), everything()
  )
context_key <- c("support_id", "site", "Id", "Date", "config_id")
duplicate_context_keys <- unit_context |>
  group_by(across(all_of(context_key))) |>
  summarise(n = n(), .groups = "drop") |>
  filter(n > 1L)
if (nrow(duplicate_context_keys)) {
  write.csv(duplicate_context_keys, "logs/core_context_duplicate_keys.csv", row.names = FALSE)
  stop("Duplicate scientific keys in unit_context; inspect logs/core_context_duplicate_keys.csv")
}
readr::write_csv(unit_context, "data/derived/core/unit_context.csv.gz", na = "")

missing_study_weather <- unit_context |>
  filter(!era5_context_available) |>
  distinct(site, Date) |>
  arrange(site, Date)
write.csv(missing_study_weather, "logs/era5_missing_study_dates.csv", row.names = FALSE)

n_metric_participants <- metric_cube |> distinct(site, Id) |> nrow()
n_context_participants <- unit_context |> distinct(site, Id) |> nrow()
diag <- tibble(
  artifact = c("metric_cube", "unit_context", "weather_1min"),
  rows = c(nrow(metric_cube), nrow(unit_context), nrow(weather_1min)),
  participants = c(n_metric_participants, n_context_participants, NA_integer_),
  sites = c(n_distinct(metric_cube$site), n_distinct(unit_context$site), n_distinct(weather_1min$site))
)
write.csv(diag, "logs/core_artifact_summary.csv", row.names = FALSE)
writeLines(capture.output(sessionInfo()), "logs/sessionInfo_core_artifacts.txt")

message("Done:")
message("  data/derived/core/metric_cube.csv.gz")
message("  data/derived/core/unit_context.csv.gz")
message("  data/derived/core/weather_1min.csv.gz")
message("Diagnostics:")
message("  logs/core_artifact_summary.csv")
message("  logs/era5_qc.csv")
message("  logs/era5_missing_study_dates.csv")
