options(encoding = "UTF-8")
if (.Platform$OS.type == "windows") invisible(suppressWarnings(Sys.setlocale("LC_CTYPE", "English_United States.utf8")))
# Canonical RQ3 plotting source. All accepted display refinements are consolidated here.
.ms_file <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
if (length(.ms_file)) {
  .ms_script <- normalizePath(sub("^--file=", "", .ms_file[[1]]), winslash = "/", mustWork = TRUE)
  .ms_root <- normalizePath(file.path(dirname(.ms_script), ".."), winslash = "/", mustWork = TRUE)
  if (!file.exists(file.path(.ms_root, "scripts", "utils", "figure_style.R"))) {
    stop("Could not resolve measurement-sufficiency repository root from ", .ms_script, call. = FALSE)
  }
  setwd(.ms_root)
}
rm(.ms_file)
if (exists(".ms_script")) rm(.ms_script)
if (exists(".ms_root")) rm(.ms_root)
suppressPackageStartupMessages({library(tidyverse); library(cowplot)})
source("scripts/utils/figure_style.R")
source("scripts/utils/figure_atlas.R")
source("scripts/utils/plot_contracts.R")
source("scripts/utils/analysis_design.R")
source("scripts/utils/artifact_validation.R")

RQ1_SUMMARY_CSV <- file.path("results", "rq1", "rq1_pairwise_summary.csv")
OBSERVED_RDS <- file.path("results", "rq3", "rq3_sufficiency_long.rds")
SUFFICIENCY_CSV <- file.path("results", "rq3", "rq3_sufficiency_long.csv")
REQUIREMENT_CSV <- file.path("results", "rq3", "rq3_single_dimension_requirement.csv")
UNORDERED_CSV <- file.path("results", "rq3", "rq3_unordered_substitutability.csv")
COVERAGE_CSV <- file.path("results", "rq3", "rq3_unordered_coverage_curves.csv")
CONVERGENCE_CSV <- file.path("results", "rq3", "rq3_convergence_profile.csv")
JOINT_CSV <- file.path("results", "rq3", "rq3_joint_summary.csv")
JOINT_RDS <- file.path("results", "rq3", "rq3_joint_stability.rds")
COMPOSITION_CSV <- file.path("results", "rq3", "rq3_composition_failure_summary.csv")
OUT_DIR <- file.path("results", "rq3", "figures")
ms_plot_require_files(c(RQ1_SUMMARY_CSV, OBSERVED_RDS, SUFFICIENCY_CSV, REQUIREMENT_CSV,
                        UNORDERED_CSV, COVERAGE_CSV, CONVERGENCE_CSV, JOINT_CSV,
                        COMPOSITION_CSV, JOINT_RDS),
                      "RQ3 v8 plotting inputs")
if (!ms_plot_prep_only()) {
  dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)
}

METRIC_CLASSES <- MS_METRIC_CLASSES
ORDERED_DIMS <- c("temporal", "duration")
ORDERED_TITLES <- c(temporal = "Temporal resolution", duration = "Monitoring duration")
RES_LEVELS <- rev(ms_primary_temporal_s())
RES_LABELS <- ms_temporal_label(RES_LEVELS)
DURATION_LEVELS <- ms_primary_duration_days()
ORDERED_MAX_RANK <- max(length(RES_LEVELS), length(DURATION_LEVELS))
NUMERIC_TOL <- 1e-12

rq1_summary <- readr::read_csv(RQ1_SUMMARY_CSV, show_col_types = FALSE, progress = FALSE)
observed <- readRDS(OBSERVED_RDS)
sufficiency <- readr::read_csv(SUFFICIENCY_CSV, show_col_types = FALSE, progress = FALSE)
requirement <- readr::read_csv(REQUIREMENT_CSV, show_col_types = FALSE, progress = FALSE)
unordered <- readr::read_csv(UNORDERED_CSV, show_col_types = FALSE, progress = FALSE)
coverage <- readr::read_csv(COVERAGE_CSV, show_col_types = FALSE, progress = FALSE)
convergence <- readr::read_csv(CONVERGENCE_CSV, show_col_types = FALSE, progress = FALSE)
joint <- readr::read_csv(JOINT_CSV, show_col_types = FALSE, progress = FALSE)
task_projection <- attr(readRDS(JOINT_RDS), "task_projection", exact = TRUE)
if (!identical(task_projection$task_projection_version, "rq3_task_projection_v1")) {
  stop("Frozen RQ3 task projection is missing; run RQ3 analysis/projection before plotting", call. = FALSE)
}
composition_summary <- readr::read_csv(COMPOSITION_CSV,show_col_types=FALSE,progress=FALSE)
ms_plot_require_columns(composition_summary,
  c("core_artifact_version", "rq1_analysis_version", "rq3_analysis_version",
    "summary_scope", "epsilon", "placement", "optical", "resolution_s", "n_days",
    "n_states", "n_resolved", "n_axis_pass", "n_failure", "failure_rate"),
  "rq3_composition_failure_summary.csv")

ms_plot_require_columns(rq1_summary, c("metric", "metric_class", "dimension", "A_mean_absolute"),
                        "rq1_pairwise_summary.csv")
ms_plot_require_columns(observed,
  c("core_artifact_version", "rq1_analysis_version", "rq3_analysis_version", "dimension",
    "metric", "metric_class", "state_label", "requirement_rank", "R_obs", "status"),
  "rq3_sufficiency_long.rds")
ms_plot_require_columns(sufficiency,
  c("dimension", "metric", "metric_class", "epsilon", "sufficient", "status"),
  "rq3_sufficiency_long.csv")
ms_plot_require_columns(requirement,
  c("dimension", "metric", "epsilon", "sufficient_states", "sufficient_set_threshold_like"),
  "rq3_single_dimension_requirement.csv")
ms_plot_require_columns(unordered,
  c("dimension", "comparison_pair_id", "config_a_label", "config_b_label", "metric", "metric_class",
    "orientation_type", "epsilon_entry", "A", "B"),
  "rq3_unordered_substitutability.csv")
ms_plot_require_columns(coverage,
  c("dimension", "comparison_pair_id", "epsilon", "fraction_metrics_substitutable"),
  "rq3_unordered_coverage_curves.csv")
ms_plot_require_columns(convergence,
  c("dimension", "metric", "metric_class", "G", "requirement_position", "boundary_proximity"),
  "rq3_convergence_profile.csv")
ms_plot_require_columns(joint,
  c("core_artifact_version", "rq1_analysis_version", "rq3_analysis_version", "support_id", "placement",
    "optical", "resolution_s", "n_days", "metric", "status", "epsilon_entry",
    "worst_higher_config"),
  "rq3_joint_summary.csv")

RQ1_VERSION <- ms_plot_one_version(c(observed$rq1_analysis_version, joint$rq1_analysis_version),
                                   "rq1_analysis_version")
RQ3_VERSION <- ms_plot_one_version(c(observed$rq3_analysis_version, joint$rq3_analysis_version),
                                   "rq3_analysis_version")
CORE_VERSION <- ms_plot_assert_core(c(observed$core_artifact_version, joint$core_artifact_version))
ms_plot_assert_prefix(RQ1_VERSION, "rq1_v5_", "rq1_analysis_version")
ms_plot_assert_prefix(RQ3_VERSION, "rq3_v8_", "rq3_analysis_version")
ms_assert_version(composition_summary,"rq3_analysis_version",RQ3_VERSION)
ms_assert_version(composition_summary,"rq1_analysis_version",RQ1_VERSION)
ms_assert_version(composition_summary,"core_artifact_version",CORE_VERSION)
ms_assert_version(task_projection,"rq3_analysis_version",RQ3_VERSION)
ms_assert_version(task_projection,"rq1_analysis_version",RQ1_VERSION)
ms_assert_version(task_projection,"core_artifact_version",CORE_VERSION)
if (!all(sort(unique(joint$resolution_s)) %in% sort(ms_primary_temporal_s()))) {
  stop("RQ3 joint artifact contains temporal states outside the frozen primary design", call. = FALSE)
}
if (!all(sort(unique(joint$n_days)) %in% DURATION_LEVELS)) {
  stop("RQ3 joint artifact contains duration states outside the frozen primary design", call. = FALSE)
}
if (!grepl(ms_analysis_design_id(), RQ3_VERSION, fixed = TRUE)) {
  stop("RQ3 plotting inputs do not match the current frozen analysis design", call. = FALSE)
}

safe_median <- function(x) {
  x <- x[is.finite(x)]
  if (length(x)) median(x) else NA_real_
}
safe_q <- function(x, p) {
  x <- x[is.finite(x)]
  if (length(x)) unname(quantile(x, p, names = FALSE)) else NA_real_
}
safe_mean <- function(x) {
  x <- as.numeric(x)
  x <- x[is.finite(x)]
  if (length(x)) mean(x) else NA_real_
}

theme_rq3 <- function(base_size = 6.7, legend_position = "none") {
  theme_ms_axes(base_size = base_size, legend_position = legend_position)
}

# =============================================================================
# Fig. 5 — joint temporal × duration sufficiency phase diagrams
# =============================================================================

# The measurement lattice is intrinsically discrete (6 temporal states × 6
# monitoring durations). All fields below therefore retain discrete cells and
# stepped boundaries. No interpolation, smoothing or monotonicity is imposed.
metric_class_lookup5 <- rq1_summary |>
  distinct(metric, metric_class)

joint_plot_base <- joint |>
  left_join(metric_class_lookup5, by = "metric", suffix = c("", ".lookup")) |>
  mutate(
    metric_class = coalesce(metric_class, metric_class.lookup),
    resolution_s = as.numeric(resolution_s),
    n_days = as.numeric(n_days)
  ) |>
  filter(is.finite(resolution_s), is.finite(n_days))

fig5_res_levels <- RES_LEVELS
fig5_days <- DURATION_LEVELS
if (!setequal(unique(joint_plot_base$resolution_s), fig5_res_levels) ||
    !setequal(unique(joint_plot_base$n_days), fig5_days)) {
  stop("RQ3 joint artifact does not contain the frozen 6 x 6 primary lattice", call. = FALSE)
}

format_resolution_compact5 <- function(x) {
  x <- as.numeric(x)
  ifelse(
    x >= 60 & abs(x / 60 - round(x / 60)) < 1e-9,
    paste0(format(round(x / 60), trim = TRUE), "m"),
    paste0(format(x, trim = TRUE), "s")
  )
}
fig5_res_labels_compact <- format_resolution_compact5(fig5_res_levels)
joint_plot_base <- joint_plot_base |>
  mutate(resolution_rank = match(resolution_s, fig5_res_levels))

# Equal-weight metric entry surface: support / placement / optical facets are
# collapsed within metric first, followed by the metric-level distribution.
entry_metric_surface <- joint_plot_base |>
  filter(status == "resolved", is.finite(epsilon_entry), !is.na(metric_class)) |>
  group_by(metric, metric_class, resolution_s, resolution_rank, n_days) |>
  summarise(
    epsilon_metric = median(epsilon_entry, na.rm = TRUE),
    n_facets = n(),
    .groups = "drop"
  )

entry_surface <- entry_metric_surface |>
  group_by(resolution_s, resolution_rank, n_days) |>
  summarise(
    n_metrics = n_distinct(metric),
    epsilon_entry_median = median(epsilon_metric, na.rm = TRUE),
    epsilon_entry_q25 = quantile(epsilon_metric, .25, na.rm = TRUE, names = FALSE),
    epsilon_entry_q75 = quantile(epsilon_metric, .75, na.rm = TRUE, names = FALSE),
    .groups = "drop"
  )

joint_cell_status <- joint_plot_base |>
  group_by(resolution_rank, n_days) |>
  summarise(
    cell_unresolved = any(status == "boundary_unresolved"),
    .groups = "drop"
  )

entry_grid <- tidyr::crossing(
  resolution_rank = seq_along(fig5_res_levels),
  n_days = fig5_days
) |>
  left_join(
    entry_surface |>
      select(resolution_rank, n_days, n_metrics,
             epsilon_entry_median, epsilon_entry_q25, epsilon_entry_q75),
    by = c("resolution_rank", "n_days")
  ) |>
  left_join(joint_cell_status, by = c("resolution_rank", "n_days")) |>
  mutate(cell_unresolved = replace_na(cell_unresolved, FALSE))

# -----------------------------------------------------------------------------
# c. Class-specific sufficient-fraction phase diagrams
# -----------------------------------------------------------------------------
# At the shared epsilon = .50 slice, each cell is the fraction of metrics in the
# class whose pooled entry tolerance is <= .50. Facets are ordered stringent to
# permissive by the share of resolved lattice cells with >= 50% metrics sufficient.
class_candidates5 <- c("timing", "duration", "level", "temporal dynamics")
class_name5 <- c(
  "timing" = "Timing",
  "duration" = "Duration",
  "level" = "Level",
  "temporal dynamics" = "Temporal dynamics"
)
class_threshold5 <- .50
class_metric_counts5 <- metric_class_lookup5 |>
  filter(metric_class %in% class_candidates5) |>
  group_by(metric_class) |>
  summarise(n_class_metrics = n_distinct(metric), .groups = "drop")

metric_cell_status_all5 <- joint_plot_base |>
  group_by(metric, resolution_rank, n_days) |>
  summarise(
    cell_unresolved = any(status == "boundary_unresolved"),
    .groups = "drop"
  )

class_surface5 <- entry_metric_surface |>
  filter(metric_class %in% class_candidates5) |>
  group_by(metric_class, resolution_rank, n_days) |>
  summarise(
    n_resolved_metrics = n_distinct(metric),
    suff_fraction = mean(epsilon_metric <= class_threshold5 + NUMERIC_TOL),
    .groups = "drop"
  )

class_status5 <- metric_cell_status_all5 |>
  left_join(metric_class_lookup5, by = "metric") |>
  filter(metric_class %in% class_candidates5) |>
  group_by(metric_class, resolution_rank, n_days) |>
  summarise(class_unresolved = any(cell_unresolved), .groups = "drop")

class_grid_raw5 <- tidyr::crossing(
  metric_class = class_candidates5,
  resolution_rank = seq_along(fig5_res_levels),
  n_days = fig5_days
) |>
  left_join(class_surface5, by = c("metric_class", "resolution_rank", "n_days")) |>
  left_join(class_metric_counts5, by = "metric_class") |>
  left_join(class_status5, by = c("metric_class", "resolution_rank", "n_days")) |>
  mutate(
    class_unresolved = replace_na(class_unresolved, FALSE),
    cell_unresolved = class_unresolved,
    suff_fraction = if_else(class_unresolved, NA_real_, suff_fraction)
  )

class_rank5 <- class_grid_raw5 |>
  group_by(metric_class) |>
  summarise(
    region_share = safe_mean(as.numeric(suff_fraction >= .50)),
    mean_sufficient_fraction = safe_mean(suff_fraction),
    .groups = "drop"
  ) |>
  arrange(region_share, mean_sufficient_fraction, metric_class) |>
  mutate(
    class_rank = row_number(),
    class_label = paste0(
      unname(class_name5[metric_class]), " · ",
      if_else(is.finite(region_share), sprintf("%.0f%%", 100 * region_share), "NA")
    )
  )
class_label_levels5 <- class_rank5$class_label

class_grid5 <- class_grid_raw5 |>
  left_join(class_rank5, by = "metric_class") |>
  mutate(class_label = factor(class_label, levels = class_label_levels5))

# Fig. 6 redesign consumes the same display grids without changing their values.
source("scripts/utils/fig6_redesign.R")
# The main decision slice uses three transparent target bundles at the reference
# placement/optical facet. All bundles, facets and tolerance intervals are frozen
# in task_projection; selecting a slice never reconstructs a sufficient set.
task_slice5 <- task_projection$frontiers |>
  filter(task_id %in% c("level", "timing", "temporal dynamics"),
         placement == "eye", optical == "MEDI",
         epsilon_interval_start <= .50 + NUMERIC_TOL,
         epsilon_interval_end > .50 + NUMERIC_TOL | terminal_endpoint) |>
  group_by(task_id, placement, optical, config_id) |>
  slice_max(epsilon_interval_start, n = 1, with_ties = FALSE) |> ungroup() |>
  mutate(resolution_rank = match(resolution_s, fig5_res_levels),
         task_id = factor(task_id, levels = c("level", "timing", "temporal dynamics")))
fig6_display <- ms_fig6_redesign(entry_grid, task_slice5, class_grid5,
                                fig5_res_labels_compact, fig5_days, composition_summary,
                                prep_only = ms_plot_prep_only())
fig6_redesigned <- fig6_display$plot
p5a <- fig6_display$a; p5b <- fig6_display$b; p5c <- fig6_display$c
ms_plot_save(fig6_redesigned, file.path(OUT_DIR, "Fig5_RQ3.png"), 7.40, 6.10)
ms_plot_save(fig6_display$class_profiles,
  file.path(OUT_DIR,"FigS_RQ3_class_sufficiency_profiles.png"),7.4,2.8)
ms_plot_save(fig6_display$composition_facets,
  file.path(OUT_DIR,"FigS_RQ3_composition_failure_facets.png"),7.4,5.0)

ms_plot_write_manifest(
  file.path(OUT_DIR, "figure_artifact_manifest.csv"),
  tibble(
    figure = "Fig5_RQ3",
    input_artifact = "rq3_joint_summary+rq3_joint_stability.task_projection+rq3_composition_failure_summary",
    core_artifact_version = CORE_VERSION,
    rq1_analysis_version = RQ1_VERSION,
    rq2_analysis_version = NA_character_,
    rq3_analysis_version = RQ3_VERSION
  )
)

message("Fig. 6 complete: joint stability, task-conditioned sufficient/Pareto sets and single-axis composition failure.")
