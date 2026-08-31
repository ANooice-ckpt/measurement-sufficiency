# Shared frozen inputs and display helpers for RQ1 main/supplementary figures.
# This file contains no figure composition or save calls.

suppressPackageStartupMessages({
  library(tidyverse)
  library(cowplot)
})
source("scripts/utils/analysis_design.R")
source("scripts/utils/figure_style.R")
source("scripts/utils/figure_atlas.R")
source("scripts/utils/plot_contracts.R")

SUMMARY_CSV <- file.path("results", "rq1", "rq1_pairwise_summary.csv")
AVAILABILITY_CSV <- file.path("results", "rq1", "rq1_metric_availability.csv")
LOCAL_CSV <- file.path("results", "rq1", "rq1_local_transition_summary.csv")
RANK_METRIC_CSV <- file.path("results", "rq1", "rq1_relational_preservation_dimension_metric.csv")
RANK_SUMMARY_CSV <- file.path("results", "rq1", "rq1_relational_preservation_dimension_summary.csv")
RANK_ASSOC_CSV <- file.path("results", "rq1", "rq1_distortion_rank_association.csv")
OUT_DIR <- file.path("results", "rq1", "figures")
FIG1_WIDTH_IN <- 7.2
FIG1_HEIGHT_IN <- 3.325
ms_plot_require_files(
  c(SUMMARY_CSV, AVAILABILITY_CSV, LOCAL_CSV, RANK_METRIC_CSV, RANK_SUMMARY_CSV, RANK_ASSOC_CSV),
  "RQ1 plotting inputs"
)
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

DIMENSIONS <- c("placement", "optical", "temporal", "duration")
DIM_TITLES <- c(
  placement = "Placement", optical = "Optical representation",
  temporal = "Temporal resolution", duration = "Monitoring duration"
)
METRIC_CLASSES <- MS_METRIC_CLASSES
FIG1_PANEL_TITLE_SIZE <- 8.0
FIG1_SUBPANEL_TITLE_SIZE <- 7.0
TEMPORAL_TRANSITION_ORDER <- ms_temporal_transition_labels()
NUMERIC_TOL <- 1e-12

summary <- readr::read_csv(SUMMARY_CSV, show_col_types = FALSE, progress = FALSE)
availability <- readr::read_csv(AVAILABILITY_CSV, show_col_types = FALSE, progress = FALSE)
local <- readr::read_csv(LOCAL_CSV, show_col_types = FALSE, progress = FALSE)
dimension_metric <- readr::read_csv(RANK_METRIC_CSV, show_col_types = FALSE, progress = FALSE)
dimension_summary <- readr::read_csv(RANK_SUMMARY_CSV, show_col_types = FALSE, progress = FALSE)
dimension_assoc <- readr::read_csv(RANK_ASSOC_CSV, show_col_types = FALSE, progress = FALSE)

ms_plot_require_columns(
  summary,
  c("core_artifact_version", "rq1_analysis_version", "dimension", "comparison_lattice",
    "comparison_pair_id", "config_a_id", "config_b_id", "config_a_label", "config_b_label",
    "orientation_type", "orientation_basis", "metric", "metric_class", "median_z", "q25_z",
    "q75_z", "p025_z", "p975_z", "B_mean_signed", "A_mean_absolute"),
  "rq1_pairwise_summary.csv"
)
ms_plot_require_columns(
  availability,
  c("core_artifact_version", "rq1_analysis_version", "dimension", "comparison_pair_id",
    "metric", "metric_class", "representation_available"),
  "rq1_metric_availability.csv"
)
ms_plot_require_columns(
  local,
  c("dimension", "metric", "metric_class", "lower_level", "higher_level",
    "orientation_type", "orientation_basis", "G", "A", "B"),
  "rq1_local_transition_summary.csv"
)
ms_plot_require_columns(
  dimension_metric,
  c("core_artifact_version", "rq1_analysis_version", "dimension", "metric", "metric_class",
    "n_oriented_pairs", "A_typical", "rank_loss_typical"),
  "rq1_relational_preservation_dimension_metric.csv"
)
ms_plot_require_columns(
  dimension_summary,
  c("core_artifact_version", "rq1_analysis_version", "dimension", "metric_class", "n_metrics",
    "A_median", "A_q25", "A_q75", "rank_loss_median", "rank_loss_q25", "rank_loss_q75"),
  "rq1_relational_preservation_dimension_summary.csv"
)
ms_plot_require_columns(
  dimension_assoc,
  c("core_artifact_version", "rq1_analysis_version", "dimension", "n_metrics", "rho_A_rank"),
  "rq1_distortion_rank_association.csv"
)

RQ1_VERSION <- ms_plot_one_version(
  c(summary$rq1_analysis_version, availability$rq1_analysis_version, local$rq1_analysis_version,
    dimension_metric$rq1_analysis_version, dimension_summary$rq1_analysis_version,
    dimension_assoc$rq1_analysis_version),
  "rq1_analysis_version"
)
CORE_VERSION <- ms_plot_assert_core(
  c(summary$core_artifact_version, availability$core_artifact_version, local$core_artifact_version,
    dimension_metric$core_artifact_version, dimension_summary$core_artifact_version,
    dimension_assoc$core_artifact_version)
)
ms_plot_assert_prefix(RQ1_VERSION, "rq1_v5_", "rq1_analysis_version")
if (!grepl(ms_analysis_design_id(), RQ1_VERSION, fixed = TRUE)) {
  stop("Fig. 1 inputs do not match the current frozen analysis design", call. = FALSE)
}

safe_q <- function(x, p) {
  x <- as.numeric(x)
  x <- x[is.finite(x)]
  if (length(x)) unname(stats::quantile(x, p, names = FALSE, type = 8)) else NA_real_
}

pretty_transition <- function(x) {
  stringr::str_replace_all(as.character(x), "\\s+to\\s+", " → ")
}

theme_fig1 <- function(base_size = 7.1, legend_position = "none") {
  theme_ms_axes(
    base_size = base_size,
    legend_position = legend_position,
    plot_title_size = FIG1_PANEL_TITLE_SIZE,
    plot_margin = margin(2, 3, 2, 3)
  )
}

summary <- summary |>
  mutate(
    metric_class = factor(metric_class, levels = METRIC_CLASSES),
    dimension = factor(dimension, levels = DIMENSIONS),
    pair_label = paste(config_a_label, "to", config_b_label),
    direction_ratio = ms_direction_ratio(B_mean_signed, A_mean_absolute)
  )
availability <- availability |>
  mutate(
    metric_class = factor(metric_class, levels = METRIC_CLASSES),
    dimension = factor(dimension, levels = DIMENSIONS)
  ) |>
  left_join(
    summary |> distinct(dimension, comparison_pair_id, pair_label),
    by = c("dimension", "comparison_pair_id")
  ) |>
  mutate(pair_label = coalesce(pair_label, as.character(comparison_pair_id))) |>
  distinct(dimension, pair_label, metric, metric_class, representation_available)
local <- local |>
  mutate(
    metric_class = factor(metric_class, levels = METRIC_CLASSES),
    dimension = factor(dimension, levels = DIMENSIONS)
  )

metric_order <- ms_metric_order(summary |> mutate(dimension = as.character(dimension)))
readr::write_csv(metric_order, file.path("results", "rq1", "figure_metric_order.csv"), na = "")
summary_plot <- summary |> mutate(dimension = as.character(dimension)) |> ms_add_metric_order(metric_order)
availability_plot <- availability |> mutate(dimension = as.character(dimension)) |> ms_add_metric_order(metric_order)
