# Bounded core fixtures. No core entrypoint is sourced and no results are written.
suppressPackageStartupMessages(library(dplyr))
source("scripts/utils/core_artifacts.R")
source("scripts/utils/core_context.R")
source("scripts/utils/protocol_windows.R")
source("scripts/utils/duration_artifacts.R")
expect_error <- function(expr) stopifnot(inherits(tryCatch(force(expr), error = identity), "error"))

source_fixture <- function(Id, cadence = 10, phase = 8, n = 100L) {
  tibble(Id = Id, Datetime = as.POSIXct("2024-01-01", tz = "UTC") + phase + cadence * (seq_len(n) - 1L),
         MEDI = as.numeric(seq_len(n)), LIGHT = as.numeric(seq_len(n)) * 2)
}
eye <- bind_rows(source_fixture("native"), source_fixture("minute", 60, 38))
chest <- bind_rows(source_fixture("native"), source_fixture("minute"))
wrist <- bind_rows(source_fixture("native", 60), source_fixture("minute"))
checked <- core_filter_native_sources(list(eye = eye, chest = chest), "test", "eye_chest_medi")
stopifnot(identical(unique(checked$sources$eye$Id), "native"),
          identical(checked$sources$eye, eye[eye$Id == "native", ]),
          !checked$audit$source_available[checked$audit$Id == "minute" & checked$audit$placement == "eye"],
          all(checked$audit$source_min_interval_s[checked$audit$Id == "minute" & checked$audit$placement == "eye"] == 60),
          all(!checked$audit$support_available[checked$audit$Id == "minute"]))
# A bad wrist must not remove a valid eye/chest participant.
wrist_checked <- core_filter_native_sources(list(eye = eye, wrist = wrist), "test", "eye_wrist_medi")
stopifnot(nrow(wrist_checked$sources$eye) == 0L,
          all(!wrist_checked$audit$support_available),
          nrow(checked$sources$eye) > 0L)
missing_checked <- core_filter_native_sources(list(eye = eye, chest = chest[chest$Id == "minute", ]), "test", "eye_chest_medi")
stopifnot(any(missing_checked$audit$unavailable_reason == "required placement source is absent", na.rm = TRUE))
gapped <- source_fixture("gapped")[-c(2L, 10:30), ]
stopifnot(core_source_sampling_audit(gapped, "test", "eye")$source_available)
bad_phase <- gapped; bad_phase$Datetime[2L] <- bad_phase$Datetime[2L] + 1
stopifnot(!core_source_sampling_audit(bad_phase, "test", "eye")$source_available)
stopifnot(!core_source_sampling_audit(source_fixture("single", n = 1L), "test", "eye")$source_available)
local({
  # An entirely ineligible block returns its audit before alignment, diaries,
  # or completeness processing, allowing the runner to continue other sites.
  e <- new.env(parent = globalenv())
  e$load_raw_file <- function(path, modality) {
    stopifnot(modality == "light_glasses")
    source_fixture("minute", 60, 38)
  }
  e$raw_data_path <- function(site, modality) "unused"
  prepare <- core_prepare_support; environment(prepare) <- e
  empty <- prepare("test", "eye_medi")
  stopifnot(nrow(empty) == 0L, nrow(attr(empty, "source_sampling_audit")) == 1L,
            !attr(empty, "source_sampling_audit")$support_available)
})

as_support <- function(x) x |>
  transmute(site = "test", Id, Date = as.Date(Datetime, tz = "UTC"), Datetime, MEDI_eye = MEDI, LIGHT_eye = LIGHT)
expect_error(core_make_series(as_support(source_fixture("minute", 60, 38)), "eye", "MEDI", 10L))
support <- as_support(gapped)
for (cadence in core_all_resolutions()) {
  sampled <- core_make_series(support, "eye", "MEDI", cadence)
  ii <- match(sampled$Datetime, support$Datetime)
  stopifnot(!anyNA(ii), identical(sampled$MEDI, support$MEDI_eye[ii]),
            identical(sampled$LIGHT, support$LIGHT_eye[ii]),
            all((as.numeric(sampled$Datetime) - 8) %% cadence == 0))
}

hour_fixture <- function(start, end, tz = "Europe/Berlin") {
  dt <- seq(as.POSIXct(start, tz = tz), as.POSIXct(end, tz = tz) - 10, by = 10)
  # Distinct repeated-clock-hour observations make collapsing DST hours visible.
  medi <- exp(1 + sin(seq_along(dt) / 290) + seq_along(dt) / 9000)
  medi[seq_along(dt) %% 79L == 0L] <- NA_real_
  tibble(site = "test", Id = "P", Date = as.Date(dt, tz = tz), Datetime = dt, MEDI = medi)
}
compare_isiv <- function(series) {
  context <- core_isiv_hourly_basis(series)
  observed <- duration_isiv_from_context(context)$value
  expected <- suppressWarnings(c(
    LightLogR::interdaily_stability(LightLogR::log_zero_inflated(series$MEDI), series$Datetime, na.rm = TRUE),
    LightLogR::intradaily_variability(LightLogR::log_zero_inflated(series$MEDI), series$Datetime, na.rm = TRUE)
  ))
  stopifnot(max(abs(observed - expected)) < 1e-12)
  context
}
fall <- hour_fixture("2024-10-27", "2024-10-28")
fall_context <- compare_isiv(fall)
fall_basis <- duration_decode_hourly_basis(fall_context$isiv_hourly_basis)
stopifnot(nrow(fall_basis) == 25L, sum(fall_basis[, 2L] == 2L) == 2L)
spring_context <- compare_isiv(hour_fixture("2024-03-31", "2024-04-01"))
stopifnot(nrow(duration_decode_hourly_basis(spring_context$isiv_hourly_basis)) == 23L)
invisible(compare_isiv(hour_fixture("2024-10-26", "2024-10-27")))
series <- hour_fixture("2024-10-26", "2024-10-29")
# Include a completely missing hour and unequal partial-hour support.
series$MEDI[series$Date == as.Date("2024-10-26") & lubridate::hour(series$Datetime) == 4L] <- NA_real_
context <- compare_isiv(series)
expect_error(duration_isiv_from_context(tibble(Date = as.Date("2024-01-01"), isiv_h00 = 1)))
expect_error(duration_decode_hourly_basis("1:2:3;1:2:4"))

local({
  root <- tempfile("core_sampling_hours_"); dir.create(root)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  csv <- file.path(root, "context.csv")
  write.csv(context, csv, row.names = FALSE, na = "")
  restored <- readr::read_csv(csv, show_col_types = FALSE)
  stopifnot(identical(restored$isiv_hourly_basis, context$isiv_hourly_basis),
            identical(duration_isiv_from_context(restored), duration_isiv_from_context(context)))

  unit_context <- context |>
    mutate(support_id = "eye_medi", analysis_unit_type = "participant_day", placement = "eye",
           optical = "MEDI", resolution_s = 10L, is_primary_resolution = TRUE, config_id = "eye__MEDI__10s")
  metric_cube <- unit_context |>
    select(-isiv_hourly_basis) |>
    mutate(metric = "mean_MEDI", metric_class = "level", metric_scope = "daily", metric_geometry = "linear",
           value = seq_len(n()), available = TRUE, unavailable_reason = NA_character_)
  metric_types <- tibble(metric = c("mean_MEDI", "interdaily_stability", "intradaily_variability"),
                         metric_class = c("level", "regularity", "regularity"))
  part_dir <- file.path(root, "parts")
  first <- build_duration_metric_cube(metric_cube, unit_context, metric_types, part_dir = part_dir)
  resumed <- build_duration_metric_cube(metric_cube, unit_context, metric_types, part_dir = part_dir)
  stopifnot(first$cube$rows > 0, identical(first$cube$rows, resumed$cube$rows),
            all(is.finite(resumed$cube$part_manifest$rows)))
  # A historical one-line marker must recover its count rather than imply zero.
  marker <- paste0(file.path(part_dir, first$cube$parts[[1L]]), ".ok")
  writeLines("duration_complete_analysis_days_v2_exact_hours", marker)
  recovered <- build_duration_metric_cube(metric_cube, unit_context, metric_types, part_dir = part_dir)
  stopifnot(identical(first$cube$rows, recovered$cube$rows))
  writeLines("duration_complete_analysis_days_v1", marker)
  replaced <- build_duration_metric_cube(metric_cube, unit_context, metric_types, part_dir = part_dir)
  stopifnot(identical(first$cube$rows, replaced$cube$rows),
            identical(readLines(marker, n = 1L), "duration_complete_analysis_days_v2_exact_hours"))
  actual <- load_duration_metric_cube(first$cube)
  full <- actual |> filter(n_days == 3L, metric %in% c("interdaily_stability", "intradaily_variability")) |> arrange(metric)
  oracle <- duration_isiv_from_context(context) |> arrange(metric)
  stopifnot(max(abs(full$value - oracle$value)) < 1e-12)
})
cat("PASS: native source cadence/support isolation, exact sparse values, DST IS/IV, CSV persistence, duration resume counts\n")
