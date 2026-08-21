source("scripts/utils/melidos_io.R")
source("scripts/utils/rq1_metrics.R")
source("scripts/utils/core_artifacts.R")
source("scripts/utils/core_temporal_sampling.R")
source("scripts/utils/core_context.R")
source("scripts/utils/protocol_windows.R")
source("scripts/utils/duration_artifacts.R")
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

Sys.setenv(
  OMP_NUM_THREADS = "1", OPENBLAS_NUM_THREADS = "1", MKL_NUM_THREADS = "1",
  VECLIB_MAXIMUM_THREADS = "1", NUMEXPR_NUM_THREADS = "1"
)
workers <- suppressWarnings(as.integer(Sys.getenv("CORE_WORKERS", unset = "16")))
if (!is.finite(workers) || workers < 1L) workers <- 1L
force_rebuild <- identical(Sys.getenv("CORE_FORCE", unset = "0"), "1")
CORE_VERSION <- core_artifact_version()
CORE_ROOT <- file.path("results", "core")
INTERIM_ROOT <- file.path(CORE_ROOT, "cache", CORE_VERSION)
SUPPORT_DIR <- file.path(INTERIM_ROOT, "supports")
METRIC_DIR <- file.path(INTERIM_ROOT, "metrics")
CONTEXT_DIR <- file.path(INTERIM_ROOT, "context")
WEATHER_DIR <- file.path(INTERIM_ROOT, "weather")
DURATION_PART_DIR <- file.path(INTERIM_ROOT, "duration_parts")

for (d in c(SUPPORT_DIR, METRIC_DIR, CONTEXT_DIR, WEATHER_DIR, DURATION_PART_DIR, CORE_ROOT, "results/diagnostics", "results/logs")) {
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
}
write_core_csv <- function(x, path) {
  x <- as.data.frame(x)
  if (grepl("\\.gz$", path)) {
    con <- gzfile(path, open = "wt")
    utils::write.csv(x, con, row.names = FALSE, na = "")
    close(con)
  } else {
    utils::write.csv(x, path, row.names = FALSE, na = "")
  }
}
message("Core artifact build: version=", CORE_VERSION, ", R ", getRversion(), ", workers=", workers, ", force=", force_rebuild)

# Dynamic fork workers remain the lightest path on Unix. Windows uses PSOCK
# workers initialized from the same pinned repository code, so the scientific
# functions and task granularity stay identical across local and ECS runs.
parallel_core_lapply <- function(X, FUN, exports = character()) {
  if (!length(X)) return(list())
  n_workers <- min(max(1L, as.integer(workers)), length(X))
  if (n_workers <= 1L) return(lapply(X, FUN))
  if (.Platform$OS.type != "windows") {
    return(parallel::mclapply(
      X, FUN, mc.cores = n_workers, mc.preschedule = FALSE, mc.set.seed = TRUE
    ))
  }

  cl <- parallel::makePSOCKcluster(n_workers)
  on.exit(parallel::stopCluster(cl), add = TRUE)
  root <- normalizePath(".", winslash = "/", mustWork = TRUE)
  parallel::clusterCall(cl, function(root) {
    setwd(root)
    Sys.setenv(
      OMP_NUM_THREADS = "1", OPENBLAS_NUM_THREADS = "1", MKL_NUM_THREADS = "1",
      VECLIB_MAXIMUM_THREADS = "1", NUMEXPR_NUM_THREADS = "1"
    )
    suppressPackageStartupMessages({
      library(tidyverse)
      library(melidosData)
      library(readxl)
    })
    source("scripts/utils/melidos_io.R")
    source("scripts/utils/rq1_metrics.R")
    source("scripts/utils/core_artifacts.R")
    source("scripts/utils/core_temporal_sampling.R")
    source("scripts/utils/core_context.R")
    source("scripts/utils/protocol_windows.R")
    source("scripts/utils/duration_artifacts.R")
    source("scripts/utils/weather_era5.R")
    NULL
  }, root)
  exports <- intersect(unique(exports), ls(envir = .GlobalEnv))
  if (length(exports)) parallel::clusterExport(cl, exports, envir = .GlobalEnv)
  parallel::parLapplyLB(cl, X, FUN, chunk.size = 1L)
}

metric_types <- read_excel("external/zauner_position/data/metric_types.xlsx") |>
  transmute(metric = name, metric_class = metric_type)
if (n_distinct(metric_types$metric) != 54L) stop("Expected 54 metric definitions")

emit_duration_artifacts <- function(duration_artifacts) {
  duration_manifest <- duration_artifacts$manifest
  duration_metric_cube <- duration_artifacts$cube
  if (nrow(duration_manifest)) {
    saveRDS(duration_manifest, file.path(CORE_ROOT, "duration_window_manifest.rds"), compress = "xz")
    write_core_csv(duration_manifest |>
      dplyr::mutate(member_dates = purrr::map_chr(member_dates, ~paste(as.character(.x), collapse = ";"))),
      file.path(CORE_ROOT, "duration_window_manifest.csv"))
  } else {
    saveRDS(duration_manifest, file.path(CORE_ROOT, "duration_window_manifest.rds"), compress = "xz")
  }
  if (duration_cube_is_partitioned(duration_metric_cube)) {
    duration_metric_cube$core_artifact_version <- CORE_VERSION
    saveRDS(duration_metric_cube, file.path(CORE_ROOT, "duration_metric_cube.rds"), compress = "xz")
    part_manifest <- duration_metric_cube$part_manifest |>
      dplyr::mutate(
        artifact_type = duration_metric_cube$artifact_type,
        core_artifact_version = CORE_VERSION,
        duration_artifact_version = duration_metric_cube$duration_artifact_version,
        .before = 1
      )
    write_core_csv(part_manifest, file.path(CORE_ROOT, "duration_metric_cube.csv.gz"))
    write_core_csv(part_manifest, file.path(CORE_ROOT, "duration_metric_cube_parts.csv"))
    duration_rows <- sum(part_manifest$rows, na.rm = TRUE)
    # Part keys are validated while each part is materialised; do not read all
    # large parts back merely to repeat the same check at manifest time.
  } else {
    duration_metric_cube <- duration_metric_cube |>
      dplyr::mutate(core_artifact_version = CORE_VERSION, .before = 1)
    saveRDS(duration_metric_cube, file.path(CORE_ROOT, "duration_metric_cube.rds"), compress = "xz")
    write_core_csv(duration_metric_cube, file.path(CORE_ROOT, "duration_metric_cube.csv.gz"))
    duration_rows <- nrow(duration_metric_cube)
    if (nrow(duration_metric_cube)) {
      duration_key <- c("support_id", "site", "Id", "window_id", "placement", "optical", "resolution_s", "metric")
      if (anyDuplicated(duration_metric_cube[duration_key])) stop("Duplicate scientific keys in duration_metric_cube")
    }
  }
  write_core_csv(duration_artifacts$audit, file.path("results", "diagnostics", "duration_cohort_audit.csv"))
  attr(duration_metric_cube, "duration_rows") <- duration_rows
  duration_metric_cube
}

# Keep the key/value manifest synchronized when only the durable duration stage
# is resumed after the expensive support/metric/context stage already exists.
write_core_manifest <- function() {
  manifest <- tibble(
    key = c(
      "core_artifact_version", "temporal_operator", "source_grid_s",
      "primary_resolutions_s", "reserve_resolutions_s", "duration_primary_domain",
      "duration_operator", "duration_window_artifact"
    ),
    value = c(
      CORE_VERSION,
      "participant-source-grid-phase-anchored systematic sparse subsampling; retained source values unchanged; no bin averaging or interpolation",
      "10",
      paste(core_primary_resolutions(), collapse = ","),
      paste(core_reserve_resolutions(), collapse = ","),
      "1-6 complete analysis days from consecutive complete-day runs",
      "daily metrics aggregated from participant-day cube; IS/IV rebuilt from exact selected dates using stored hourly basis",
      "duration_window_manifest + partitioned duration_metric_cube manifest and RDS parts"
    )
  )
  write_core_csv(manifest, file.path(CORE_ROOT, "core_manifest.csv"))
}

# A killed process no longer requires re-running extraction and all support
# blocks.  Set CORE_DURATION_ONLY=1 to resume the durable duration stage from
# already-materialised v3 core files on either Windows or Linux.
if (identical(Sys.getenv("CORE_DURATION_ONLY", unset = "0"), "1")) {
  metric_path <- file.path(CORE_ROOT, "metric_cube.csv.gz")
  context_path <- file.path(CORE_ROOT, "unit_context.csv.gz")
  if (!file.exists(metric_path) || !file.exists(context_path)) {
    stop("CORE_DURATION_ONLY=1 requires results/core/metric_cube.csv.gz and unit_context.csv.gz")
  }
  message("[duration-only] reading durable v3 core inputs")
  metric_cube <- readr::read_csv(metric_path, show_col_types = FALSE, progress = FALSE) |>
    dplyr::mutate(Date = as.Date(Date))
  unit_context <- readr::read_csv(context_path, show_col_types = FALSE, progress = FALSE) |>
    dplyr::mutate(Date = as.Date(Date))
  if (!all(unique(na.omit(metric_cube$core_artifact_version)) == CORE_VERSION)) {
    stop("CORE_DURATION_ONLY inputs do not match current core version: ", CORE_VERSION)
  }
  duration_artifacts <- build_duration_metric_cube(metric_cube, unit_context, metric_types, max_days = 6L, part_dir = DURATION_PART_DIR, reuse_parts = !force_rebuild)
  invisible(emit_duration_artifacts(duration_artifacts))
  write_core_manifest()
  message("Done: duration artifacts resumed for ", CORE_VERSION)
  quit(save = "no", status = 0)
}

site_meta <- core_site_metadata()
expected_weather_files <- file.path("data", "raw", "era5", paste0(site_meta$site, ".csv"))
missing_weather <- expected_weather_files[!file.exists(expected_weather_files)]
if (length(missing_weather)) stop("Missing ERA5 files: ", paste(missing_weather, collapse = ", "))

# Stage 0: validate / cache weather under the core version. Versioning prevents
# a mixed old/new artifact tree when the scientific core changes.
message("[weather] validate, summarize, and interpolate ERA5")
weather_build_one <- function(i) {
  site <- site_meta$site[i]
  timezone <- site_meta$timezone[i]
  prefix <- file.path(WEATHER_DIR, site)
  paths <- c(
    hourly = paste0(prefix, "__hourly.rds"), daily = paste0(prefix, "__daily.rds"),
    minute = paste0(prefix, "__1min.rds"), qc = paste0(prefix, "__qc.rds")
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
weather_1min <- map_dfr(weather_minute_paths, readRDS) |>
  mutate(core_artifact_version = CORE_VERSION, .before = 1)
weather_qc <- map_dfr(weather_qc_paths, readRDS)
# readr/vroom on the pinned Windows runtime can reject POSIXct raw-backed
# columns in this large table; base write.csv over a gz connection is equivalent
# and keeps the weather artifact portable.
utils::write.csv(weather_1min, gzfile(file.path(CORE_ROOT, "weather_1min.csv.gz")), row.names = FALSE, na = "")
write_core_csv(weather_qc, file.path("results", "logs", "era5_qc.csv"))

# Stage 1: explicit support lattices.
message("[supports] prepare explicit comparison supports")
support_grid <- core_support_grid()
prepare_one <- function(i) {
  row <- support_grid[i, ]
  path <- file.path(SUPPORT_DIR, paste0(row$site, "__", row$support_id, ".rds"))
  if (!force_rebuild && file.exists(path)) return(path)
  message("prepare support: ", row$site, " / ", row$support_id)
  x <- core_prepare_support(row$site, row$support_id)
  if (is.null(x)) return(NA_character_)
  saveRDS(x, path, compress = FALSE)
  path
}
idx <- seq_len(nrow(support_grid))
support_paths <- parallel_core_lapply(
  idx, prepare_one,
  exports = c("site_meta", "support_grid", "SUPPORT_DIR", "force_rebuild")
)
support_paths <- unlist(support_paths, use.names = FALSE)
support_paths <- support_paths[!is.na(support_paths) & file.exists(support_paths)]
if (!length(support_paths)) stop("No support blocks were produced")

# Smoke-test the scientific temporal operator before any expensive metric block.
# A coarse series must be an exact timestamp/value subset of the 10-s source and
# retain the participant-specific phase of that harmonized source grid.
message("[temporal] sparse-sampling invariants")
smoke_support <- readRDS(support_paths[[1]])
ref <- core_make_series(smoke_support, "eye", "MEDI", 10L)
temporal_audit <- map_dfr(setdiff(core_all_resolutions(), 10L), function(r) {
  can <- core_make_series(smoke_support, "eye", "MEDI", r)
  key_ref <- paste(ref$site, ref$Id, as.numeric(ref$Datetime), sep = "|")
  key_can <- paste(can$site, can$Id, as.numeric(can$Datetime), sep = "|")
  ii <- match(key_can, key_ref)
  subset_pass <- !anyNA(ii)
  can_sec <- round(as.numeric(can$Datetime))
  phase10 <- core_source_grid_phase(can$site, can$Id, can_sec)
  clock_anchor_pass <- all(((can_sec - phase10) %% r) == 0L)
  same_num <- function(a, b) all((is.na(a) & is.na(b)) | (!is.na(a) & !is.na(b) & a == b))
  medi_value_pass <- subset_pass && same_num(can$MEDI, ref$MEDI[ii])
  light_value_pass <- subset_pass && same_num(can$LIGHT, ref$LIGHT[ii])
  value_pass <- medi_value_pass && light_value_pass
  if (!subset_pass) stop("Temporal invariant failed: candidate is not a source subset at ", r, " s")
  if (!clock_anchor_pass) stop("Temporal source-grid anchoring failed at ", r, " s")
  if (!value_pass) stop("Temporal retained-value invariant failed at ", r, " s")
  tibble(
    resolution_s = r, source_rows = nrow(ref), candidate_rows = nrow(can),
    subset_pass = subset_pass, clock_anchor_pass = clock_anchor_pass,
    retained_MEDI_exact_pass = medi_value_pass, retained_LIGHT_exact_pass = light_value_pass
  )
})
write_core_csv(temporal_audit, file.path("results", "logs", "core_temporal_sampling_audit.csv"))

# Stage 2: configuration-level metrics/context. The versioned directory is the
# cache key, so artifacts from an older temporal operator can never be silently
# reused.
compute_one <- function(path) {
  key <- tools::file_path_sans_ext(basename(path))
  metric_out <- file.path(METRIC_DIR, paste0(key, "__metrics.rds"))
  context_out <- file.path(CONTEXT_DIR, paste0(key, "__context.rds"))
  if (force_rebuild || !file.exists(metric_out)) {
    message("compute metric block: ", key)
    saveRDS(core_compute_support_metrics(path), metric_out, compress = FALSE)
  }
  if (force_rebuild || !file.exists(context_out)) {
    message("compute context block: ", key)
    saveRDS(core_config_daily_context(path), context_out, compress = FALSE)
  }
  c(metric = metric_out, context = context_out)
}
block_paths <- parallel_core_lapply(
  support_paths, compute_one,
  exports = c("METRIC_DIR", "CONTEXT_DIR", "force_rebuild")
)
metric_paths <- vapply(block_paths, `[[`, character(1), "metric")
context_paths <- vapply(block_paths, `[[`, character(1), "context")

# Stage 3: durable metric cube.
emitted <- map_dfr(metric_paths, readRDS)
metric_cube <- core_finalize_metric_cube(emitted, metric_types) |>
  mutate(core_artifact_version = CORE_VERSION, .before = 1)
metric_key <- c(
  "support_id", "site", "Id", "analysis_unit_type", "analysis_unit_id", "Date",
  "placement", "optical", "resolution_s", "config_id", "metric"
)
duplicate_metric_keys <- metric_cube |>
  group_by(across(all_of(metric_key))) |>
  summarise(n = n(), .groups = "drop") |>
  filter(n > 1L)
if (nrow(duplicate_metric_keys)) {
  write.csv(duplicate_metric_keys, file.path("results", "logs", "core_metric_duplicate_keys.csv"), row.names = FALSE)
  stop("Duplicate scientific keys in metric_cube")
}
if (any(metric_cube$resolution_s == 15L)) stop("15-s configuration survived into v3 metric cube")
write_core_csv(metric_cube, file.path(CORE_ROOT, "metric_cube.csv.gz"))

# Stage 4: daily configuration context, including protocol dates.
config_context <- map_dfr(context_paths, readRDS)
unit_context <- config_context |>
  left_join(site_meta, by = "site") |>
  left_join(weather_daily, by = c("site", "Date")) |>
  mutate(
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
    year = lubridate::year(Date), month = lubridate::month(Date),
    day_of_year = lubridate::yday(Date),
    weekday = as.character(lubridate::wday(Date, label = TRUE, abbr = TRUE)),
    era5_context_available = !is.na(era5_hours_instantaneous),
    core_artifact_version = CORE_VERSION
  ) |>
  select(
    core_artifact_version,
    support_id, site, Id, analysis_unit_type, analysis_unit_id, Date,
    valid_day_index, support_valid_day_count,
    raw_eye_recording_start, raw_eye_recording_end,
    raw_eye_span_hours, raw_eye_calendar_day_count,
    protocol_start, protocol_end, protocol_start_date, protocol_end_date,
    protocol_span_hours, protocol_calendar_date_count, protocol_nominal_7d,
    protocol_metadata_available, protocol_day_index, is_within_protocol_interval,
    is_protocol_day1_7, is_protocol_return_date,
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
  write.csv(duplicate_context_keys, file.path("results", "logs", "core_context_duplicate_keys.csv"), row.names = FALSE)
  stop("Duplicate scientific keys in unit_context")
}
if (any(unit_context$resolution_s == 15L)) stop("15-s configuration survived into v3 unit_context")
write_core_csv(unit_context, file.path(CORE_ROOT, "unit_context.csv.gz"))

missing_study_weather <- unit_context |>
  filter(!era5_context_available) |>
  distinct(site, Date) |>
  arrange(site, Date)
write.csv(missing_study_weather, file.path("results", "logs", "era5_missing_study_dates.csv"), row.names = FALSE)

# Stage 5: complete-analysis-day duration artifact. This is intentionally built
# from durable participant-day values plus the stored hourly IS/IV basis; it does
# not revisit raw 10-s observations or redefine daily metric operators.
message("[duration] complete-day runs and 1-6 d windows")
duration_artifacts <- build_duration_metric_cube(metric_cube, unit_context, metric_types, max_days = 6L, part_dir = DURATION_PART_DIR, reuse_parts = !force_rebuild)
duration_metric_cube <- emit_duration_artifacts(duration_artifacts)

write_core_manifest()

n_metric_participants <- metric_cube |> distinct(site, Id) |> nrow()
n_context_participants <- unit_context |> distinct(site, Id) |> nrow()
diag <- tibble(
  artifact = c("metric_cube", "unit_context", "duration_metric_cube", "weather_1min"),
  rows = c(nrow(metric_cube), nrow(unit_context), attr(duration_metric_cube, "duration_rows"), nrow(weather_1min)),
  participants = c(n_metric_participants, n_context_participants, NA_integer_),
  sites = c(n_distinct(metric_cube$site), n_distinct(unit_context$site), n_distinct(weather_1min$site)),
  core_artifact_version = CORE_VERSION
)
write.csv(diag, file.path("results", "logs", "core_artifact_summary.csv"), row.names = FALSE)
writeLines(capture.output(sessionInfo()), file.path("results", "logs", "sessionInfo_core_artifacts.txt"))

message("Done: ", CORE_VERSION)
message("  results/core/metric_cube.csv.gz")
message("  results/core/unit_context.csv.gz")
message("  results/core/duration_metric_cube.rds")
message("  results/core/duration_window_manifest.rds")
message("  results/core/weather_1min.csv.gz")
message("  results/core/core_manifest.csv")
