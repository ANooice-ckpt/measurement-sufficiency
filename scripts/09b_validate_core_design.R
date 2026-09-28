suppressPackageStartupMessages(library(tidyverse))
source("scripts/utils/analysis_design.R")
source("scripts/utils/core_artifacts.R")
source("scripts/utils/artifact_validation.R")

CORE_ROOT <- file.path("results", "core")
METRIC_CUBE <- file.path(CORE_ROOT, "metric_cube.csv.gz")
CORE_MANIFEST <- file.path(CORE_ROOT, "core_manifest.csv")
DURATION_MANIFEST <- file.path(CORE_ROOT, "duration_window_manifest.rds")
SOURCE_AUDIT <- file.path("results", "diagnostics", "core_source_sampling_audit.csv")
DIAG <- file.path("results", "diagnostics")
dir.create(DIAG, recursive = TRUE, showWarnings = FALSE)

for (p in c(METRIC_CUBE, CORE_MANIFEST, DURATION_MANIFEST, SOURCE_AUDIT)) {
  if (!file.exists(p)) stop("Missing core-design validation input: ", p)
}

primary <- sort(ms_primary_temporal_s())
reserve <- sort(ms_reserve_temporal_s())
expected <- sort(ms_all_temporal_s())
duration_days <- ms_primary_duration_days()

cube <- readr::read_csv(METRIC_CUBE, show_col_types = FALSE, progress = FALSE)
manifest <- readr::read_csv(CORE_MANIFEST, show_col_types = FALSE, progress = FALSE)
duration_manifest <- readRDS(DURATION_MANIFEST)
source_audit <- readr::read_csv(SOURCE_AUDIT, show_col_types = FALSE, progress = FALSE)
ms_assert_version(cube, "core_artifact_version", core_artifact_version())
ms_assert_version(source_audit, "core_artifact_version", core_artifact_version())
ms_assert_unique(source_audit, c("support_id", "site", "Id", "placement"), "source sampling audit")
source_status <- source_audit |>
  group_by(support_id, site, Id) |>
  summarise(source_eligible = all(source_available %in% TRUE),
            support_consistent = all(!is.na(support_available) & support_available == source_eligible),
            .groups = "drop")
if (any(!source_status$support_consistent)) stop("Inconsistent source-support eligibility audit")
included_sources <- cube |>
  distinct(support_id, site, Id) |>
  left_join(source_status, by = c("support_id", "site", "Id"))
if (any(!included_sources$source_eligible | is.na(included_sources$source_eligible))) {
  stop("Core metric cube contains an unaudited or ineligible native source")
}

# A site-level union can conceal participants missing most of the cadence grid.
participant_states <- cube |>
  distinct(support_id, site, Id, placement, optical, resolution_s) |>
  group_by(support_id, site, Id, placement, optical) |>
  summarise(states_complete = setequal(resolution_s, expected), .groups = "drop")
if (any(!participant_states$states_complete)) {
  stop("At least one participant/configuration is missing a frozen temporal state")
}

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
if (!identical(unname(manifest_lookup[["core_artifact_version"]]), core_artifact_version())) {
  stop("core_manifest does not match the current native-source/hourly-basis contract")
}
expected_primary_text <- paste(primary, collapse = ",")
expected_reserve_text <- paste(reserve, collapse = ",")
if (!identical(unname(manifest_lookup[["primary_resolutions_s"]]), expected_primary_text)) {
  stop("core_manifest primary_resolutions_s does not match analysis_design.R")
}
if (!identical(unname(manifest_lookup[["reserve_resolutions_s"]]), expected_reserve_text)) {
  stop("core_manifest reserve_resolutions_s does not match analysis_design.R")
}

observed_duration <- sort(unique(as.integer(duration_manifest$n_days)))
adjacent_ids <- c(duration_manifest$adjacent_lower_window_id, duration_manifest$adjacent_higher_window_id)
if (any(!adjacent_ids[!is.na(adjacent_ids)] %in% duration_manifest$window_id)) {
  stop("Duration manifest contains an adjacent ID that is not an observed window")
}
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
  n_source_ineligible_support_participants = sum(!source_status$source_eligible),
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
