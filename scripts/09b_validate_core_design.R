suppressPackageStartupMessages(library(tidyverse))
source("scripts/utils/analysis_design.R")

CORE_ROOT <- file.path("results", "core")
METRIC_CUBE <- file.path(CORE_ROOT, "metric_cube.csv.gz")
CORE_MANIFEST <- file.path(CORE_ROOT, "core_manifest.csv")
DURATION_MANIFEST <- file.path(CORE_ROOT, "duration_window_manifest.rds")
DIAG <- file.path("results", "diagnostics")
dir.create(DIAG, recursive = TRUE, showWarnings = FALSE)

for (p in c(METRIC_CUBE, CORE_MANIFEST, DURATION_MANIFEST)) {
  if (!file.exists(p)) stop("Missing core-design validation input: ", p)
}

primary <- sort(ms_primary_temporal_s())
reserve <- sort(ms_reserve_temporal_s())
expected <- sort(ms_all_temporal_s())
duration_days <- ms_primary_duration_days()

cube <- readr::read_csv(METRIC_CUBE, show_col_types = FALSE, progress = FALSE)
manifest <- readr::read_csv(CORE_MANIFEST, show_col_types = FALSE, progress = FALSE)
duration_manifest <- readRDS(DURATION_MANIFEST)

observed <- sort(unique(as.integer(cube$resolution_s)))
if (!identical(observed, expected)) {
  stop(
    "Core temporal lattice mismatch. Expected ", paste(expected, collapse = ","),
    "; observed ", paste(observed, collapse = ",")
  )
}

if (any(cube$is_primary_resolution != (cube$resolution_s %in% primary), na.rm = TRUE)) {
  stop("Core is_primary_resolution flags do not match the frozen primary lattice")
}

support_audit <- cube |>
  distinct(support_id, site, resolution_s, is_primary_resolution) |>
  group_by(support_id, site) |>
  summarise(
    temporal_states = paste(sort(unique(resolution_s)), collapse = ","),
    primary_states = paste(sort(unique(resolution_s[is_primary_resolution])), collapse = ","),
    all_states_complete = setequal(unique(resolution_s), expected),
    primary_states_complete = setequal(unique(resolution_s[is_primary_resolution]), primary),
    .groups = "drop"
  )
if (any(!support_audit$all_states_complete) || any(!support_audit$primary_states_complete)) {
  readr::write_csv(support_audit, file.path(DIAG, "core_design_support_audit.csv"), na = "")
  stop("At least one support/site block is missing a frozen temporal state")
}

manifest_lookup <- setNames(as.character(manifest$value), as.character(manifest$key))
expected_primary_text <- paste(primary, collapse = ",")
expected_reserve_text <- paste(reserve, collapse = ",")
if (!identical(unname(manifest_lookup[["primary_resolutions_s"]]), expected_primary_text)) {
  stop("core_manifest primary_resolutions_s does not match analysis_design.R")
}
if (!identical(unname(manifest_lookup[["reserve_resolutions_s"]]), expected_reserve_text)) {
  stop("core_manifest reserve_resolutions_s does not match analysis_design.R")
}

observed_duration <- sort(unique(as.integer(duration_manifest$n_days)))
if (!identical(observed_duration, sort(as.integer(duration_days)))) {
  stop(
    "Duration lattice mismatch. Expected ", paste(duration_days, collapse = ","),
    "; observed ", paste(observed_duration, collapse = ",")
  )
}

summary_audit <- tibble(
  analysis_design_id = ms_analysis_design_id(),
  primary_temporal_s = expected_primary_text,
  reserve_temporal_s = expected_reserve_text,
  duration_days = paste(duration_days, collapse = ","),
  n_support_site_blocks = nrow(support_audit),
  all_support_blocks_complete = all(support_audit$all_states_complete & support_audit$primary_states_complete),
  pass = TRUE
)
readr::write_csv(support_audit, file.path(DIAG, "core_design_support_audit.csv"), na = "")
readr::write_csv(summary_audit, file.path(DIAG, "core_design_audit.csv"), na = "")

message(
  "Core design audit passed: primary=", expected_primary_text,
  "; reserve=", expected_reserve_text,
  "; duration=", paste(duration_days, collapse = ",")
)
