# Canonical RQ2 plotting source. All accepted display refinements are consolidated here.
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

RQ1_SUMMARY_CSV <- file.path("results", "rq1", "rq1_pairwise_summary.csv")
CONDITION_RDS <- file.path("results", "rq2", "rq2_condition_long.rds")
COND_GEOM_CSV <- file.path("results", "rq2", "rq2_conditional_geometry.csv")
MODEL_COEF_CSV <- file.path("results", "rq2", "rq2_model_coefficients.csv")
MODEL_PERF_CSV <- file.path("results", "rq2", "rq2_model_performance.csv")
MODEL_MANIFEST_CSV <- file.path("results", "rq2", "rq2_model_artifact_manifest.csv")
GAMMA_RDS <- file.path("results", "rq2", "rq2_gamma_long.rds")
GAMMA_SUMMARY_CSV <- file.path("results", "rq2", "rq2_gamma_summary.csv")
SCOPE_CSV <- file.path("results", "rq2", "rq2_interaction_scope.csv")
OUT_DIR <- file.path("results", "rq2", "figures")
ms_plot_require_files(c(RQ1_SUMMARY_CSV, CONDITION_RDS, COND_GEOM_CSV, MODEL_COEF_CSV,
                        MODEL_PERF_CSV, MODEL_MANIFEST_CSV, GAMMA_RDS,
                        GAMMA_SUMMARY_CSV, SCOPE_CSV),
                      "RQ2 v5 plotting inputs")
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

METRIC_CLASSES <- MS_METRIC_CLASSES
DIMENSIONS <- c("placement", "optical", "temporal", "duration")
DIM_TITLES <- c(
  placement = "Placement", optical = "Optical representation",
  temporal = "Temporal resolution", duration = "Monitoring duration"
)

rq1_summary <- readr::read_csv(RQ1_SUMMARY_CSV, show_col_types = FALSE, progress = FALSE)
condition <- readRDS(CONDITION_RDS)
conditional <- readr::read_csv(COND_GEOM_CSV, show_col_types = FALSE, progress = FALSE)
coefficients <- readr::read_csv(MODEL_COEF_CSV, show_col_types = FALSE, progress = FALSE)
performance <- readr::read_csv(MODEL_PERF_CSV, show_col_types = FALSE, progress = FALSE)
model_manifest <- readr::read_csv(MODEL_MANIFEST_CSV, show_col_types = FALSE, progress = FALSE)
gamma_long <- readRDS(GAMMA_RDS)
gamma_summary <- readr::read_csv(GAMMA_SUMMARY_CSV, show_col_types = FALSE, progress = FALSE)
scope <- readr::read_csv(SCOPE_CSV, show_col_types = FALSE, progress = FALSE)

ms_plot_require_columns(rq1_summary, c("metric", "metric_class", "dimension", "A_mean_absolute"),
                        "rq1_pairwise_summary.csv")
ms_plot_require_columns(conditional,
  c("core_artifact_version", "rq1_analysis_version", "rq2_analysis_version", "dimension",
    "comparison_pair_id", "config_a_label", "config_b_label", "metric", "metric_class",
    "state_bin_label", "A_conditional", "B_conditional"), "rq2_conditional_geometry.csv")
ms_plot_require_columns(coefficients,
  c("dimension", "comparison_pair_id", "metric", "outcome", "model_family", "term",
    "estimate", "std_error", "p_value"), "rq2_model_coefficients.csv")
ms_plot_require_columns(performance,
  c("dimension", "comparison_pair_id", "metric", "outcome", "model_family", "validation_scheme",
    "n_test", "rmse", "mae", "r2"), "rq2_model_performance.csv")
ms_plot_require_columns(model_manifest,
  c("core_artifact_version", "rq1_analysis_version", "rq2_analysis_version"),
  "rq2_model_artifact_manifest.csv")
ms_plot_require_columns(gamma_summary,
  c("dimension_a", "dimension_b", "comparison_lattice", "transition", "metric", "metric_class", "R", "Q"),
  "rq2_gamma_summary.csv")
ms_plot_require_columns(scope, c("dimension_pair", "primary_scope"), "rq2_interaction_scope.csv")

condition_core <- if (is.list(condition)) condition$core_artifact_version else NULL
condition_rq1 <- if (is.list(condition)) condition$rq1_analysis_version else NULL
condition_rq2 <- if (is.list(condition)) condition$rq2_analysis_version else NULL
RQ1_VERSION <- ms_plot_one_version(c(condition_rq1, conditional$rq1_analysis_version,
                                     model_manifest$rq1_analysis_version, gamma_long$rq1_analysis_version),
                                   "rq1_analysis_version")
RQ2_VERSION <- ms_plot_one_version(c(condition_rq2, conditional$rq2_analysis_version,
                                     model_manifest$rq2_analysis_version, gamma_long$rq2_analysis_version),
                                   "rq2_analysis_version")
CORE_VERSION <- ms_plot_assert_core(c(condition_core, conditional$core_artifact_version,
                                     model_manifest$core_artifact_version, gamma_long$core_artifact_version))
ms_plot_assert_prefix(RQ1_VERSION, "rq1_v5_", "rq1_analysis_version")
ms_plot_assert_prefix(RQ2_VERSION, "rq2_v5_", "rq2_analysis_version")
# Guard against plotting stale RQ2 artifacts after a measurement-lattice change.
if (is.list(condition) && !is.null(condition$analysis_design_id) &&
    !identical(as.character(condition$analysis_design_id[[1]]), ms_analysis_design_id())) {
  stop("RQ2 plotting inputs do not match the current frozen analysis design", call. = FALSE)
}

metric_order <- ms_metric_order(rq1_summary)
metric_class_lookup <- rq1_summary |> distinct(metric, metric_class)
conditional <- conditional |>
  mutate(
    metric_class = factor(metric_class, levels = METRIC_CLASSES),
    transition_family = factor(
      if_else(dimension %in% c("placement", "optical"),
              "Target alignment", "Measurement requirement"),
      levels = c("Target alignment", "Measurement requirement")
    ),
    dimension = factor(dimension, levels = DIMENSIONS),
    pair_label = paste(config_a_label, "→", config_b_label),
    state_bin_label = factor(state_bin_label, levels = c("Low", "Middle", "High")),
    direction_ratio = ms_direction_ratio(B_conditional, A_conditional)
  ) |>
  mutate(dimension = as.character(dimension)) |>
  ms_add_metric_order(metric_order)

theme_rq2 <- function(base_size = 6.7, legend_position = "none") {
  theme_ms_axes(base_size = base_size, legend_position = legend_position)
}


metric_legend <- ms_metric_legend(text_size = 5.35, point_size = 1.5, key_width_mm = 3.5)

# Main-text display windows are intentionally robust to a small number of
# pathological model/geometry values. Statistical summaries always use the full
# frozen artifacts; only background/raw points determine these viewing windows.
robust_symmetric_display_limit <- function(x, summary_values = numeric(), prob = .95,
                                           pad = 1.08, fallback = 1) {
  x <- abs(as.numeric(x))
  x <- x[is.finite(x)]
  s <- abs(as.numeric(summary_values))
  s <- s[is.finite(s)]
  core <- if (length(x)) as.numeric(stats::quantile(x, prob, na.rm = TRUE,
                                                    names = FALSE, type = 8)) else NA_real_
  summary_extent <- if (length(s)) max(s, na.rm = TRUE) else NA_real_
  lim <- suppressWarnings(max(c(core, summary_extent), na.rm = TRUE))
  if (!is.finite(lim) || lim <= 0) lim <- fallback
  lim * pad
}

robust_upper_display_limit <- function(x, summary_values = numeric(), prob = .95,
                                       pad = 1.08, fallback = 1) {
  x <- as.numeric(x)
  x <- x[is.finite(x)]
  s <- as.numeric(summary_values)
  s <- s[is.finite(s)]
  core <- if (length(x)) as.numeric(stats::quantile(x, prob, na.rm = TRUE,
                                                    names = FALSE, type = 8)) else NA_real_
  summary_extent <- if (length(s)) max(s, na.rm = TRUE) else NA_real_
  lim <- suppressWarnings(max(c(core, summary_extent), na.rm = TRUE))
  if (!is.finite(lim) || lim <= 0) lim <- fallback
  lim * pad
}

robust_bounded_display_range <- function(x, summary_values = numeric(), probs = c(.05, .95),
                                         lower = -1, upper = 1, min_span = .30,
                                         pad_fraction = .08) {
  x <- as.numeric(x)
  x <- x[is.finite(x)]
  s <- as.numeric(summary_values)
  s <- s[is.finite(s)]
  if (!length(x) && !length(s)) return(c(lower, upper))
  q <- if (length(x)) {
    as.numeric(stats::quantile(x, probs, na.rm = TRUE, names = FALSE, type = 8))
  } else c(min(s), max(s))
  lo <- min(c(q[[1]], s), na.rm = TRUE)
  hi <- max(c(q[[2]], s), na.rm = TRUE)
  lo <- max(lower, lo)
  hi <- min(upper, hi)
  if (!is.finite(lo) || !is.finite(hi) || lo >= hi) return(c(lower, upper))
  span <- hi - lo
  if (span < min_span) {
    center <- (lo + hi) / 2
    lo <- center - min_span / 2
    hi <- center + min_span / 2
  }
  span <- hi - lo
  lo <- max(lower, lo - span * pad_fraction)
  hi <- min(upper, hi + span * pad_fraction)
  if ((hi - lo) < min_span) {
    if (lo <= lower + 1e-12) hi <- min(upper, lower + min_span)
    if (hi >= upper - 1e-12) lo <- max(lower, upper - min_span)
  }
  c(lo, hi)
}

# =============================================================================
# Fig. 2 — context dependence of distortion
# =============================================================================


# a. Conditional distortion geometry combines magnitude and directional coherence
# on the same exposure-state axis. Transition-level trajectories retain the paired
# state response, while metric-class summaries remain the visual foreground.
conditional_trajectory_state <- conditional |>
  mutate(
    metric = as.character(metric),
    metric_class = factor(as.character(metric_class), levels = METRIC_CLASSES),
    state_num = as.integer(state_bin_label),
    class_num = as.integer(metric_class),
    class_offset = (class_num - (length(METRIC_CLASSES) + 1) / 2) * .047,
    x_pos = state_num + class_offset
  ) |>
  filter(
    is.finite(A_conditional), is.finite(direction_ratio), is.finite(state_num),
    state_bin_label %in% c("Low", "Middle", "High")
  ) |>
  group_by(
    dimension, comparison_pair_id, pair_label, metric, metric_class,
    state_bin_label, state_num, class_num, class_offset, x_pos
  ) |>
  summarise(
    A_state = median(A_conditional, na.rm = TRUE),
    direction_state = median(direction_ratio, na.rm = TRUE),
    .groups = "drop"
  )

conditional_metric_state <- conditional_trajectory_state |>
  group_by(
    dimension, metric, metric_class, state_bin_label,
    state_num, class_num, class_offset, x_pos
  ) |>
  summarise(
    A_state = median(A_state, na.rm = TRUE),
    direction_state = median(direction_state, na.rm = TRUE),
    .groups = "drop"
  )

conditional_profile_summary <- conditional_metric_state |>
  group_by(
    dimension, metric_class, state_bin_label,
    state_num, class_num, class_offset, x_pos
  ) |>
  summarise(
    n_metrics = n_distinct(metric),
    A_median = median(A_state, na.rm = TRUE),
    A_q25 = quantile(A_state, .25, na.rm = TRUE, names = FALSE),
    A_q75 = quantile(A_state, .75, na.rm = TRUE, names = FALSE),
    direction_median = median(direction_state, na.rm = TRUE),
    direction_q25 = quantile(direction_state, .25, na.rm = TRUE, names = FALSE),
    direction_q75 = quantile(direction_state, .75, na.rm = TRUE, names = FALSE),
    .groups = "drop"
  )

make_conditional_state_block <- function(dim_name) {
  tr <- conditional_trajectory_state |> filter(dimension == dim_name)
  sm <- conditional_profile_summary |> filter(dimension == dim_name)

  mag_limit <- robust_upper_display_limit(
    tr$A_state,
    c(sm$A_median, sm$A_q25, sm$A_q75),
    prob = .95, pad = 1.08, fallback = .25
  )
  dir_range <- robust_bounded_display_range(
    tr$direction_state,
    c(sm$direction_median, sm$direction_q25, sm$direction_q75),
    probs = c(.05, .95), lower = -1, upper = 1, min_span = .30,
    pad_fraction = .08
  )

  p_mag <- ggplot() +
    geom_line(
      data = tr,
      aes(x_pos, A_state, group = interaction(metric, comparison_pair_id), color = metric_class),
      linewidth = .09, alpha = .018
    ) +
    geom_point(
      data = tr,
      aes(x_pos, A_state, color = metric_class),
      size = .18, alpha = .055
    ) +
    geom_linerange(
      data = sm,
      aes(x_pos, ymin = A_q25, ymax = A_q75, color = metric_class),
      linewidth = .60, alpha = .48
    ) +
    geom_line(
      data = sm,
      aes(x_pos, A_median, group = metric_class, color = metric_class),
      linewidth = .62, alpha = .96
    ) +
    geom_point(
      data = sm,
      aes(x_pos, A_median, color = metric_class),
      shape = 18, size = 1.22
    ) +
    scale_color_ms_metric(guide = "none") +
    scale_x_continuous(
      breaks = 1:3, labels = c("Low", "Middle", "High"),
      limits = c(.70, 3.30), expand = expansion(mult = c(0, 0))
    ) +
    scale_y_continuous(breaks = scales::breaks_extended(n = 4)) +
    coord_cartesian(ylim = c(0, mag_limit), clip = "on") +
    labs(
      title = unname(DIM_TITLES[[dim_name]]),
      x = NULL, y = "conditional A"
    ) +
    theme_rq2(base_size = 5.95) +
    theme(
      panel.grid.major.x = element_blank(),
      axis.text.x = element_blank(), axis.ticks.x = element_blank(),
      axis.title.x = element_blank(),
      strip.text = element_blank(),
      plot.title = element_text(size = 6.0, hjust = .5, margin = margin(b = 1.5)),
      plot.margin = margin(1.5, 2.5, 0, 2.5)
    )

  p_dir <- ggplot() +
    geom_hline(yintercept = 0, linewidth = .28, color = "#9DA2A5") +
    geom_line(
      data = tr,
      aes(x_pos, direction_state, group = interaction(metric, comparison_pair_id), color = metric_class),
      linewidth = .08, alpha = .014
    ) +
    geom_point(
      data = tr,
      aes(x_pos, direction_state, color = metric_class),
      size = .17, alpha = .045
    ) +
    geom_linerange(
      data = sm,
      aes(x_pos, ymin = direction_q25, ymax = direction_q75, color = metric_class),
      linewidth = .55, alpha = .48
    ) +
    geom_line(
      data = sm,
      aes(x_pos, direction_median, group = metric_class, color = metric_class),
      linewidth = .58, alpha = .96
    ) +
    geom_point(
      data = sm,
      aes(x_pos, direction_median, color = metric_class),
      shape = 18, size = 1.06
    ) +
    scale_color_ms_metric(guide = "none") +
    scale_x_continuous(
      breaks = 1:3, labels = c("Low", "Middle", "High"),
      limits = c(.70, 3.30), expand = expansion(mult = c(0, 0))
    ) +
    scale_y_continuous(breaks = scales::breaks_extended(n = 3)) +
    coord_cartesian(ylim = dir_range, clip = "on") +
    labs(x = "transition-local exposure state", y = "B / A") +
    theme_rq2(base_size = 5.55) +
    theme(
      panel.grid.major.x = element_blank(),
      axis.text.x = element_text(size = 4.8),
      axis.text.y = element_text(size = 4.5),
      axis.title.x = element_text(size = 5.15),
      axis.title.y = element_text(size = 4.9),
      plot.margin = margin(0, 2.5, 1.5, 2.5)
    )

  cowplot::plot_grid(
    p_mag, p_dir, ncol = 1, rel_heights = c(.83, .17),
    align = "v", axis = "lr", greedy = TRUE
  )
}

state_blocks <- lapply(DIMENSIONS, make_conditional_state_block)
p2b_core <- cowplot::plot_grid(
  plotlist = state_blocks, ncol = 2,
  align = "hv", axis = "tblr", greedy = TRUE
)
p2b <- cowplot::ggdraw() +
  cowplot::draw_plot(p2b_core, x = 0, y = 0, width = 1, height = .955) +
  cowplot::draw_label(
    "b  Conditional distortion geometry",
    x = .002, y = .998, hjust = 0, vjust = 1,
    fontface = "bold", size = 6.35
  ) +
  cowplot::draw_label(
    "transition-local exposure-state tertiles",
    x = .002, y = .970, hjust = 0, vjust = 1,
    colour = "#666A6D", size = 3.95
  )

# Transition-resolved state geometry. The transition-spread view remains a
# supplement; Fig. 2c reports held-out contextual predictability.
transition_state <- conditional |>
  mutate(
    metric = as.character(metric),
    metric_class = as.character(metric_class),
    state_bin_label = as.character(state_bin_label)
  ) |>
  filter(is.finite(A_conditional), is.finite(direction_ratio),
         state_bin_label %in% c("Low", "Middle", "High")) |>
  group_by(dimension, comparison_pair_id, pair_label, metric, metric_class, state_bin_label) |>
  summarise(
    A_state = median(A_conditional, na.rm = TRUE),
    direction_state = median(direction_ratio, na.rm = TRUE),
    .groups = "drop"
  ) |>
  pivot_wider(
    names_from = state_bin_label,
    values_from = c(A_state, direction_state),
    names_sep = "_"
  ) |>
  rowwise() |>
  mutate(
    n_A_states = sum(is.finite(c_across(starts_with("A_state_")))),
    A_span = if (n_A_states >= 2L) diff(range(c_across(starts_with("A_state_")), na.rm = TRUE)) else NA_real_,
    delta_A_HL = A_state_High - A_state_Low,
    delta_direction_HL = direction_state_High - direction_state_Low
  ) |>
  ungroup()

transition_spread <- transition_state |>
  filter(is.finite(A_span)) |>
  group_by(dimension, comparison_pair_id, pair_label) |>
  summarise(
    n_metrics = n_distinct(metric),
    span_median = median(A_span, na.rm = TRUE),
    span_q25 = quantile(A_span, .25, na.rm = TRUE, names = FALSE),
    span_q75 = quantile(A_span, .75, na.rm = TRUE, names = FALSE),
    .groups = "drop"
  ) |>
  group_by(dimension) |>
  slice_max(span_median, n = 3, with_ties = FALSE) |>
  ungroup() |>
  mutate(
    dimension = factor(dimension, levels = DIMENSIONS, labels = unname(DIM_TITLES[DIMENSIONS])),
    transition_key = paste(as.character(dimension), pair_label, sep = "|||"),
    transition_key = forcats::fct_reorder(transition_key, span_median)
  )

PREDICTOR_COLORS <- c(
  "External opportunity" = MS_PRIMARY,
  "Micro-environment" = "#5F8F84",
  "Behaviour" = MS_NEUTRAL,
  "Exposure state" = MS_SECONDARY
)
OUTCOME_SHAPES <- c("Signed" = 16, "Absolute" = 17)
OUTCOME_OFFSET <- .15
PREDICTOR_ROW_STEP <- .76
FAMILY_GAP <- .55

quantile_or_na <- function(x, probability) {
  x <- as.numeric(x)
  x <- x[is.finite(x)]
  if (!length(x)) return(NA_real_)
  as.numeric(stats::quantile(x, probability, na.rm = TRUE, names = FALSE, type = 8))
}

squish_to_limits <- function(x, limits) {
  pmax(limits[[1]], pmin(limits[[2]], x))
}

robust_symmetric_display_window <- function(x, summary_values = numeric(), probs = c(.08, .92),
                                            min_half = .02, pad = 1.06, hard_cap = NULL) {
  x <- as.numeric(x); x <- x[is.finite(x)]
  s <- as.numeric(summary_values); s <- s[is.finite(s)]
  vals <- c(x, s)
  if (!length(vals)) return(c(-min_half, min_half))
  q <- as.numeric(stats::quantile(vals, probs, na.rm = TRUE, names = FALSE, type = 8))
  half <- max(abs(q), na.rm = TRUE)
  if (length(s)) half <- max(half, min(abs(stats::median(s, na.rm = TRUE)) * 3, max(abs(s), na.rm = TRUE) / 2), na.rm = TRUE)
  if (!is.finite(half) || half <= 0) half <- min_half
  half <- max(half, min_half)
  if (length(hard_cap) && is.finite(hard_cap)) half <- min(half, hard_cap)
  half <- half * pad
  c(-half, half)
}

FAMILY_LEVELS <- c("External opportunity", "Micro-environment", "Behaviour", "Exposure state")
PREDICTOR_CATALOG <- tribble(
  ~term, ~label, ~predictor_family,
  "external_radiation", "Solar radiation", "External opportunity",
  "external_direct_fraction", "Direct fraction", "External opportunity",
  "external_cloud", "Cloud cover", "External opportunity",
  "external_cloud_variability", "Cloud-cover variability", "External opportunity",
  "solar_noon_elevation_deg", "Solar-noon elevation", "External opportunity",
  "external_photoperiod_h", "Civil photoperiod", "External opportunity",
  "external_temperature_c", "Temperature", "External opportunity",
  "external_wet_hours", "Wet hours", "External opportunity",
  "micro_outdoor_fraction", "Outdoor fraction", "Micro-environment",
  "micro_daylight_indoor_fraction", "Indoor daylight fraction", "Micro-environment",
  "micro_daylight_outdoor_fraction", "Outdoor daylight fraction", "Micro-environment",
  "micro_display_fraction", "Display-light fraction", "Micro-environment",
  "behaviour_home_fraction", "Home fraction", "Behaviour",
  "behaviour_work_fraction", "Work fraction", "Behaviour",
  "behaviour_vehicle_fraction", "Vehicle fraction", "Behaviour",
  "behaviour_workday", "Work/free day", "Behaviour",
  "behaviour_exercise_level", "Exercise level", "Behaviour",
  "behaviour_prior_sleep_h", "Prior sleep duration", "Behaviour",
  "primary_state_raw", "Primary exposure state", "Exposure state",
  "duration_day_variability", "Day-to-day variability", "Exposure state"
) |>
  mutate(predictor_family = factor(predictor_family, levels = FAMILY_LEVELS))

coef_metric <- coefficients |>
  filter(model_family == "joint", term %in% PREDICTOR_CATALOG$term, is.finite(estimate)) |>
  group_by(dimension, metric, outcome, term) |>
  summarise(estimate = median(estimate, na.rm = TRUE), .groups = "drop") |>
  left_join(metric_class_lookup, by = "metric") |>
  left_join(PREDICTOR_CATALOG, by = "term") |>
  mutate(
    outcome_label = recode(outcome, signed = "Signed", magnitude = "Absolute", .default = outcome),
    outcome_label = factor(outcome_label, levels = c("Signed", "Absolute"))
  ) |>
  filter(dimension %in% DIMENSIONS, !is.na(predictor_family), !is.na(outcome_label))

predictor_overall <- coef_metric |>
  group_by(term) |>
  summarise(
    overall_abs = median(abs(estimate), na.rm = TRUE),
    n_display_units = n(),
    .groups = "drop"
  )

family_sizes <- PREDICTOR_CATALOG |>
  count(predictor_family, name = "n_predictors") |>
  arrange(predictor_family) |>
  mutate(
    family_index = row_number(),
    family_top_offset = c(0, head(cumsum(n_predictors * PREDICTOR_ROW_STEP + FAMILY_GAP), -1))
  )
atlas_top <- max(family_sizes$family_top_offset + (family_sizes$n_predictors - 1) * PREDICTOR_ROW_STEP)

predictor_order <- PREDICTOR_CATALOG |>
  left_join(predictor_overall, by = "term") |>
  mutate(overall_abs_sort = if_else(is.finite(overall_abs), overall_abs, -Inf)) |>
  arrange(predictor_family, desc(overall_abs_sort), label) |>
  group_by(predictor_family) |>
  mutate(family_row = row_number()) |>
  ungroup() |>
  left_join(family_sizes |> select(predictor_family, family_index, family_top_offset), by = "predictor_family") |>
  mutate(y = atlas_top - family_top_offset - (family_row - 1) * PREDICTOR_ROW_STEP) |>
  select(-overall_abs_sort)
predictor_y_limits <- range(predictor_order$y) + c(-.55, .55)

coef_metric <- coef_metric |>
  left_join(predictor_order |> select(term, y), by = "term") |>
  mutate(y_pos = y + if_else(outcome_label == "Signed", -OUTCOME_OFFSET, OUTCOME_OFFSET))

coef_summary <- coef_metric |>
  group_by(dimension, term, label, predictor_family, y, outcome_label) |>
  summarise(
    n_display_units = n(), n_metrics = n_distinct(metric),
    estimate_q05 = quantile_or_na(estimate, .05), estimate_q25 = quantile_or_na(estimate, .25),
    estimate_q50 = quantile_or_na(estimate, .50), estimate_q75 = quantile_or_na(estimate, .75),
    estimate_q95 = quantile_or_na(estimate, .95), .groups = "drop"
  ) |>
  mutate(estimate_median = estimate_q50, y_pos = y + if_else(outcome_label == "Signed", -OUTCOME_OFFSET, OUTCOME_OFFSET))

coef_summary_all <- coef_metric |>
  group_by(term, label, predictor_family, y, outcome_label) |>
  summarise(
    n_display_units = n(), n_metrics = n_distinct(dimension, metric),
    estimate_q05 = quantile_or_na(estimate, .05), estimate_q25 = quantile_or_na(estimate, .25),
    estimate_q50 = quantile_or_na(estimate, .50), estimate_q75 = quantile_or_na(estimate, .75),
    estimate_q95 = quantile_or_na(estimate, .95), .groups = "drop"
  ) |>
  mutate(estimate_median = estimate_q50, y_pos = y + if_else(outcome_label == "Signed", -OUTCOME_OFFSET, OUTCOME_OFFSET))

family_boundaries <- family_sizes |>
  filter(family_index < length(FAMILY_LEVELS)) |>
  mutate(
    last_y = atlas_top - family_top_offset - (n_predictors - 1) * PREDICTOR_ROW_STEP,
    boundary = last_y - (PREDICTOR_ROW_STEP + FAMILY_GAP) / 2
  ) |>
  pull(boundary)

coef_window_global <- robust_symmetric_display_window(
  coef_metric$estimate,
  c(coef_summary$estimate_q05, coef_summary$estimate_q25, coef_summary$estimate_q50,
    coef_summary$estimate_q75, coef_summary$estimate_q95, coef_summary_all$estimate_q05,
    coef_summary_all$estimate_q95),
  probs = c(.05, .95), min_half = .025, pad = 1.10
)
coef_metric_global <- coef_metric |> mutate(estimate_plot = squish_to_limits(estimate, coef_window_global))
coef_summary_all_global <- coef_summary_all |>
  mutate(
    estimate_q05_plot = squish_to_limits(estimate_q05, coef_window_global),
    estimate_q25_plot = squish_to_limits(estimate_q25, coef_window_global),
    estimate_q50_plot = squish_to_limits(estimate_q50, coef_window_global),
    estimate_q75_plot = squish_to_limits(estimate_q75, coef_window_global),
    estimate_q95_plot = squish_to_limits(estimate_q95, coef_window_global)
  )
overall_limit <- robust_upper_display_limit(predictor_order$overall_abs, predictor_order$overall_abs,
                                            prob = .90, pad = 1.08, fallback = .02)
predictor_order <- predictor_order |>
  mutate(overall_abs_plot = pmin(overall_abs, overall_limit),
         overall_clipped = is.finite(overall_abs) & overall_abs > overall_limit)

add_row_guides <- function(p) {
  if (length(family_boundaries)) p + geom_hline(yintercept = family_boundaries, linewidth = .26, color = "#D8DBDD") else p
}

p_labels <- ggplot(predictor_order, aes(y = y)) +
  geom_text(aes(x = .955, label = label), hjust = 1, size = 1.40, color = "#34383B") +
  geom_point(aes(x = .995, color = predictor_family), size = .72, alpha = .95) +
  scale_color_manual(values = PREDICTOR_COLORS, drop = FALSE, guide = "none") +
  scale_x_continuous(limits = c(0, 1), expand = expansion(mult = c(0, 0))) +
  scale_y_continuous(limits = predictor_y_limits, expand = expansion(mult = c(0, 0))) +
  labs(title = "Predictor", x = NULL, y = NULL) +
  theme_void(base_family = MS_FONT) +
  theme(plot.title = element_text(size = 5.05, hjust = 1, face = "bold", margin = margin(b = 1.2)),
        plot.margin = margin(1.5, .6, 1.5, .6))
p_labels <- add_row_guides(p_labels)

p_overall <- ggplot(predictor_order, aes(fill = predictor_family)) +
  geom_rect(aes(xmin = 0, xmax = overall_abs_plot, ymin = y - .18, ymax = y + .18), alpha = .48, na.rm = TRUE) +
  geom_point(data = predictor_order |> filter(overall_clipped), aes(x = overall_limit, y = y), inherit.aes = FALSE,
             shape = 17, size = .62, color = "#4E5559") +
  geom_point(data = predictor_order |> filter(!is.finite(overall_abs)), aes(x = 0, y = y), inherit.aes = FALSE,
             shape = 4, size = .75, stroke = .35, color = "#AEB2B5") +
  scale_fill_manual(values = PREDICTOR_COLORS, drop = FALSE, guide = "none") +
  scale_x_continuous(limits = c(0, overall_limit), breaks = scales::breaks_extended(n = 3), expand = expansion(mult = c(0, .02))) +
  scale_y_continuous(limits = predictor_y_limits, expand = expansion(mult = c(0, 0))) +
  labs(title = "Overall", x = "median |β|", y = NULL) +
  theme_rq2(base_size = 5.65) +
  theme(panel.grid = element_blank(), axis.line.y = element_blank(), axis.ticks.y = element_blank(), axis.text.y = element_blank(),
        axis.text.x = element_text(size = 4.25), axis.title.x = element_text(size = 4.55),
        plot.title = element_text(size = 5.35, hjust = .5, face = "bold", margin = margin(b = 1.2)),
        plot.margin = margin(1.5, 1.2, 1.5, 1.5))
p_overall <- add_row_guides(p_overall)

p_spread <- ggplot(coef_summary_all_global, aes(color = predictor_family)) +
  geom_vline(xintercept = 0, linewidth = .28, color = "#A1A6A9") +
  geom_segment(aes(x = estimate_q05_plot, xend = estimate_q95_plot, y = y_pos, yend = y_pos), linewidth = .34, alpha = .34, lineend = "round") +
  geom_segment(aes(x = estimate_q25_plot, xend = estimate_q75_plot, y = y_pos, yend = y_pos), linewidth = .92, alpha = .76, lineend = "round") +
  geom_point(aes(estimate_q50_plot, y_pos, shape = outcome_label), size = .92, alpha = .98) +
  scale_color_manual(values = PREDICTOR_COLORS, drop = FALSE, guide = "none") +
  scale_shape_manual(values = OUTCOME_SHAPES, drop = FALSE, guide = "none") +
  scale_x_continuous(limits = coef_window_global, breaks = scales::breaks_extended(n = 3)) +
  scale_y_continuous(limits = predictor_y_limits, expand = expansion(mult = c(0, 0))) +
  labs(title = "All tasks", x = "β", y = NULL) +
  theme_rq2(base_size = 5.35) +
  theme(panel.grid.major.y = element_blank(), axis.line.y = element_blank(), axis.ticks.y = element_blank(), axis.text.y = element_blank(),
        axis.text.x = element_text(size = 3.15), axis.ticks.x = element_line(linewidth = .18), axis.title.x = element_text(size = 4.05),
        plot.title = element_text(size = 5.15, hjust = .5, face = "bold", margin = margin(b = 1.2)), plot.margin = margin(1.5, .6, 1.5, .6))
p_spread <- add_row_guides(p_spread)

status_grid <- tidyr::crossing(term = PREDICTOR_CATALOG$term, dimension = DIMENSIONS,
                               outcome_label = factor(c("Signed", "Absolute"), levels = c("Signed", "Absolute"))) |>
  left_join(predictor_order |> select(term, y), by = "term") |>
  left_join(coef_summary |> select(dimension, term, outcome_label, estimate_q50), by = c("dimension", "term", "outcome_label")) |>
  mutate(
    structural_na = term == "duration_day_variability" & dimension != "duration",
    status_label = case_when(structural_na ~ "—", !is.finite(estimate_q50) ~ "×", TRUE ~ NA_character_),
    y_pos = y + if_else(outcome_label == "Signed", -OUTCOME_OFFSET, OUTCOME_OFFSET)
  )

make_coef_dimension <- function(dim_name) {
  raw <- coef_metric |> filter(dimension == dim_name, is.finite(estimate))
  sm <- coef_summary |> filter(dimension == dim_name, is.finite(estimate_q50))
  miss <- status_grid |> filter(dimension == dim_name, !is.na(status_label))
  dim_window <- robust_symmetric_display_window(raw$estimate,
    c(sm$estimate_q05, sm$estimate_q25, sm$estimate_q50, sm$estimate_q75, sm$estimate_q95),
    probs = c(.05, .95), min_half = .020, pad = 1.10)
  sm <- sm |> mutate(
    estimate_q05_plot = squish_to_limits(estimate_q05, dim_window), estimate_q25_plot = squish_to_limits(estimate_q25, dim_window),
    estimate_q50_plot = squish_to_limits(estimate_q50, dim_window), estimate_q75_plot = squish_to_limits(estimate_q75, dim_window),
    estimate_q95_plot = squish_to_limits(estimate_q95, dim_window)
  )
  p <- ggplot() +
    geom_vline(xintercept = 0, linewidth = .28, color = "#A1A6A9") +
    geom_segment(data = sm, aes(x = estimate_q05_plot, xend = estimate_q95_plot, y = y_pos, yend = y_pos, color = predictor_family), linewidth = .34, alpha = .34, lineend = "round") +
    geom_segment(data = sm, aes(x = estimate_q25_plot, xend = estimate_q75_plot, y = y_pos, yend = y_pos, color = predictor_family), linewidth = .92, alpha = .76, lineend = "round") +
    geom_point(data = sm, aes(estimate_q50_plot, y_pos, color = predictor_family, shape = outcome_label), size = .98, alpha = .98) +
    geom_text(data = miss, aes(x = 0, y = y_pos, label = status_label), size = 1.45, color = "#B3B7BA") +
    scale_color_manual(values = PREDICTOR_COLORS, drop = FALSE, guide = "none") +
    scale_shape_manual(values = OUTCOME_SHAPES, drop = FALSE, guide = "none") +
    scale_x_continuous(limits = dim_window, breaks = scales::breaks_extended(n = 3)) +
    scale_y_continuous(limits = predictor_y_limits, expand = expansion(mult = c(0, 0))) +
    labs(title = unname(DIM_TITLES[[dim_name]]), x = "β", y = NULL) +
    theme_rq2(base_size = 5.25) +
    theme(panel.grid.major.y = element_blank(), axis.line.y = element_blank(), axis.ticks.y = element_blank(), axis.text.y = element_blank(),
          axis.text.x = element_text(size = 3.95), axis.title.x = element_text(size = 4.35),
          plot.title = element_text(size = 5.05, hjust = .5, face = "bold", margin = margin(b = 1.2)), plot.margin = margin(1.5, .45, 1.5, .45))
  add_row_guides(p)
}
coef_dim_plots <- lapply(DIMENSIONS, make_coef_dimension)

predictor_legend_plot <- ggplot(coef_metric, aes(estimate, y, color = predictor_family, shape = outcome_label)) +
  geom_point(size = 1.15, alpha = 1) +
  scale_color_manual(values = PREDICTOR_COLORS, drop = FALSE,
                     labels = c("External opportunity" = "External", "Micro-environment" = "Micro-env.",
                                "Behaviour" = "Behaviour", "Exposure state" = "Exposure state")) +
  scale_shape_manual(values = OUTCOME_SHAPES, drop = FALSE) +
  guides(color = guide_legend(title = NULL, nrow = 1, order = 1, override.aes = list(alpha = 1, size = 1.05)),
         shape = guide_legend(title = NULL, nrow = 1, order = 2, override.aes = list(alpha = 1, size = 1.05))) +
  theme_void(base_family = MS_FONT) +
  theme(legend.position = "bottom", legend.text = element_text(size = 4.25), legend.key.width = grid::unit(2.4, "mm"),
        legend.spacing.x = grid::unit(.55, "mm"), legend.margin = margin(0, 0, 0, 0))
predictor_legend <- cowplot::get_legend(predictor_legend_plot)

p2a_core <- cowplot::plot_grid(p_labels, p_overall, p_spread, coef_dim_plots[[1]], coef_dim_plots[[2]], coef_dim_plots[[3]], coef_dim_plots[[4]],
                               ncol = 7, rel_widths = c(.18, .08, .08, .165, .165, .165, .165), align = "hv", axis = "tb", greedy = TRUE)
p2a_body <- cowplot::plot_grid(p2a_core, predictor_legend, ncol = 1, rel_heights = c(.955, .045), align = "v", axis = "l", greedy = TRUE)
p2a <- cowplot::ggdraw() +
  cowplot::draw_plot(p2a_body, x = 0, y = 0, width = 1, height = .965) +
  cowplot::draw_label("a  Contextual predictor atlas", x = .002, y = .998, hjust = 0, vjust = 1, fontface = "bold", size = 7.0) +
  cowplot::draw_label("all prespecified predictors; bars = median |β| across estimable display units", x = .002, y = .972, hjust = 0, vjust = 1, colour = "#666A6D", size = 4.4)

coef_metric_display <- coef_metric |> mutate(displayed_in_fig2a = TRUE)

# Retain predictor-family grouped-CV increments as a supplementary validation of
# independent information; this is useful diagnostically but too coarse for the
# main contextual panel.
context_task <- performance |>
  filter(str_detect(validation_scheme, "^participant_grouped"),
         model_family %in% c("external_context", "exposure_state", "joint")) |>
  group_by(dimension, comparison_pair_id, metric, outcome, model_family) |>
  summarise(r2 = median(r2, na.rm = TRUE), n_test = max(n_test, na.rm = TRUE), .groups = "drop") |>
  pivot_wider(names_from = model_family, values_from = c(r2, n_test), names_sep = "__")
for (nm in c("r2__external_context", "r2__exposure_state", "r2__joint",
             "n_test__external_context", "n_test__exposure_state", "n_test__joint")) {
  if (!nm %in% names(context_task)) context_task[[nm]] <- NA_real_
}
context_task <- context_task |>
  mutate(
    external_beyond_state = if_else(
      is.finite(r2__joint) & is.finite(r2__exposure_state) &
        n_test__joint == n_test__exposure_state,
      r2__joint - r2__exposure_state, NA_real_
    ),
    state_beyond_external = if_else(
      is.finite(r2__joint) & is.finite(r2__external_context) &
        n_test__joint == n_test__external_context,
      r2__joint - r2__external_context, NA_real_
    )
  ) |>
  select(dimension, comparison_pair_id, metric, outcome,
         external_beyond_state, state_beyond_external) |>
  pivot_longer(c(external_beyond_state, state_beyond_external),
               names_to = "information", values_to = "delta_r2") |>
  mutate(
    information = recode(information,
      external_beyond_state = "External beyond state",
      state_beyond_external = "State beyond external"
    )
  ) |>
  filter(is.finite(delta_r2)) |>
  group_by(dimension, metric, outcome, information) |>
  summarise(delta_r2 = median(delta_r2, na.rm = TRUE), .groups = "drop") |>
  mutate(
    dimension = factor(dimension, levels = DIMENSIONS, labels = unname(DIM_TITLES[DIMENSIONS])),
    outcome_label = recode(outcome, signed = "Signed distortion", magnitude = "Absolute distortion", .default = outcome),
    information = factor(information, levels = c("External beyond state", "State beyond external"))
  )

context_summary <- context_task |>
  group_by(dimension, outcome_label, information) |>
  summarise(
    n_metrics = n_distinct(metric),
    delta_median = median(delta_r2, na.rm = TRUE),
    delta_q25 = quantile(delta_r2, .25, na.rm = TRUE, names = FALSE),
    delta_q75 = quantile(delta_r2, .75, na.rm = TRUE, names = FALSE),
    .groups = "drop"
  )


# c. Out-of-sample contextual predictability from the joint contextual model.
# This uses the participant-grouped CV R2 already produced by RQ2. It does not
# subtract model-family R2 values evaluated on different complete-case samples.
joint_cv_metric <- performance |>
  filter(
    str_detect(validation_scheme, "^participant_grouped"),
    model_family == "joint", is.finite(r2)
  ) |>
  group_by(dimension, comparison_pair_id, metric, outcome) |>
  summarise(
    r2 = median(r2, na.rm = TRUE),
    n_test = max(n_test, na.rm = TRUE),
    .groups = "drop"
  ) |>
  group_by(dimension, metric, outcome) |>
  summarise(
    r2 = median(r2, na.rm = TRUE),
    n_test = max(n_test, na.rm = TRUE),
    .groups = "drop"
  ) |>
  left_join(metric_class_lookup, by = "metric") |>
  mutate(
    metric_class = factor(metric_class, levels = METRIC_CLASSES),
    dimension = factor(
      dimension, levels = DIMENSIONS,
      labels = unname(DIM_TITLES[DIMENSIONS])
    ),
    outcome_label = recode(
      outcome,
      signed = "Signed distortion",
      magnitude = "Absolute distortion",
      .default = outcome
    )
  ) |>
  filter(!is.na(dimension), !is.na(metric_class), is.finite(r2))

joint_cv_summary <- joint_cv_metric |>
  group_by(dimension, outcome_label, metric_class) |>
  summarise(
    n_metrics = n_distinct(metric),
    r2_median = median(r2, na.rm = TRUE),
    r2_q25 = quantile(r2, .25, na.rm = TRUE, names = FALSE),
    r2_q75 = quantile(r2, .75, na.rm = TRUE, names = FALSE),
    .groups = "drop"
  )

joint_cv_positive <- joint_cv_metric |>
  group_by(dimension, outcome_label) |>
  summarise(
    n_metrics = n_distinct(metric),
    fraction_positive = mean(r2 > 0, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    dimension_num = as.integer(dimension),
    y_label = dimension_num + .25,
    label = paste0(round(100 * fraction_positive), "% > 0")
  )

if (nrow(joint_cv_metric)) {
  joint_cv_limit <- robust_symmetric_display_limit(
    joint_cv_metric$r2,
    c(joint_cv_summary$r2_median, joint_cv_summary$r2_q25, joint_cv_summary$r2_q75),
    prob = .95, pad = 1.08, fallback = .50
  )

  joint_cv_plot <- joint_cv_metric |>
    mutate(
      dimension_num = as.integer(dimension),
      class_num = as.integer(metric_class),
      class_offset = (class_num - (length(METRIC_CLASSES) + 1) / 2) * .060,
      y_pos = dimension_num + class_offset,
      displayed_in_fig2c = is.finite(r2) & abs(r2) <= joint_cv_limit
    ) |>
    filter(displayed_in_fig2c)

  joint_cv_plot_summary <- joint_cv_summary |>
    mutate(
      dimension_num = as.integer(dimension),
      class_num = as.integer(metric_class),
      class_offset = (class_num - (length(METRIC_CLASSES) + 1) / 2) * .060,
      y_pos = dimension_num + class_offset
    )

  p2c <- ggplot(joint_cv_plot, aes(r2, y_pos, color = metric_class)) +
    geom_vline(xintercept = 0, linewidth = .30, color = "#9DA2A5") +
    geom_point(
      position = position_jitter(width = 0, height = .018, seed = 56),
      size = .54, alpha = .22
    ) +
    geom_segment(
      data = joint_cv_plot_summary,
      aes(x = r2_q25, xend = r2_q75, y = y_pos, yend = y_pos, color = metric_class),
      inherit.aes = FALSE, linewidth = .88, alpha = .56, lineend = "round"
    ) +
    geom_point(
      data = joint_cv_plot_summary,
      aes(r2_median, y_pos, color = metric_class),
      inherit.aes = FALSE, shape = 18, size = 1.45
    ) +
    geom_text(
      data = joint_cv_positive,
      aes(x = Inf, y = y_label, label = label),
      inherit.aes = FALSE, hjust = 1.08, vjust = .5,
      size = 1.55, color = MS_NEUTRAL
    ) +
    facet_wrap(~outcome_label, nrow = 1) +
    scale_color_ms_metric(guide = "none") +
    scale_x_continuous(
      limits = c(-joint_cv_limit, joint_cv_limit),
      breaks = scales::breaks_extended(n = 5)
    ) +
    scale_y_continuous(
      breaks = seq_along(DIMENSIONS),
      labels = unname(DIM_TITLES[DIMENSIONS]),
      limits = c(.55, length(DIMENSIONS) + .45)
    ) +
    labs(
      title = "c  Out-of-sample contextual predictability",
      subtitle = "class summaries use all joint-model CV results; right labels = fraction of metrics with CV R² > 0",
      x = "participant-grouped CV R²", y = NULL
    ) +
    theme_rq2(base_size = 6.05) +
    theme(
      panel.grid.major.y = element_blank(),
      axis.line.y = element_blank(), axis.ticks.y = element_blank(),
      axis.text.y = element_text(size = 4.9),
      strip.text = element_text(size = 5.35),
      plot.subtitle = element_text(
        size = 4.25, colour = "#666A6D", margin = margin(t = -1, b = 2)
      ),
      panel.spacing = grid::unit(1.8, "mm")
    )
} else {
  joint_cv_limit <- NA_real_
  p2c <- ggplot() + theme_void(base_family = MS_FONT) +
    annotate(
      "text", x = 0, y = 0,
      label = "c  Out-of-sample contextual predictability\nNo joint grouped-CV results",
      size = 2.2, colour = "#55595C"
    )
}

# Replace the legacy Fig. 2c drawing with the compact participant-grouped CV
# display used by the redesigned Fig. 2. The frozen CV rows and estimand are
# unchanged; only display coordinates, clipping and summaries are rebuilt here.
joint_cv_metric_panel <- performance |>
  filter(
    str_detect(validation_scheme, "^participant_grouped"),
    model_family == "joint", is.finite(r2)
  ) |>
  group_by(dimension, comparison_pair_id, metric, outcome) |>
  summarise(r2 = median(r2, na.rm = TRUE), n_test = max(n_test, na.rm = TRUE), .groups = "drop") |>
  group_by(dimension, metric, outcome) |>
  summarise(r2 = median(r2, na.rm = TRUE), n_test = max(n_test, na.rm = TRUE), .groups = "drop") |>
  left_join(metric_class_lookup, by = "metric") |>
  mutate(
    metric_class = factor(metric_class, levels = METRIC_CLASSES),
    outcome_label = recode(outcome, signed = "Signed distortion", magnitude = "Absolute distortion", .default = outcome),
    outcome_label = factor(outcome_label, levels = c("Absolute distortion", "Signed distortion"))
  ) |>
  filter(dimension %in% DIMENSIONS, !is.na(metric_class), !is.na(outcome_label), is.finite(r2))

dim_map_panel <- tibble(
  dimension = DIMENSIONS,
  dimension_label = unname(DIM_TITLES[DIMENSIONS]),
  y = rev(seq(.90, by = .78, length.out = length(DIMENSIONS)))
)
joint_cv_metric_panel <- joint_cv_metric_panel |>
  left_join(dim_map_panel, by = "dimension") |>
  mutate(
    class_num = as.integer(metric_class),
    class_offset = (class_num - (length(METRIC_CLASSES) + 1) / 2) * .045,
    y_pos = y + class_offset
  )
joint_cv_summary_panel <- joint_cv_metric_panel |>
  group_by(dimension, dimension_label, y, outcome_label, metric_class) |>
  summarise(
    n_metrics = n_distinct(metric),
    r2_median = median(r2, na.rm = TRUE),
    r2_q25 = quantile(r2, .25, na.rm = TRUE, names = FALSE),
    r2_q75 = quantile(r2, .75, na.rm = TRUE, names = FALSE),
    .groups = "drop"
  ) |>
  mutate(
    class_num = as.integer(metric_class),
    class_offset = (class_num - (length(METRIC_CLASSES) + 1) / 2) * .045,
    y_pos = y + class_offset
  )
joint_cv_positive_panel <- joint_cv_metric_panel |>
  group_by(dimension, dimension_label, y, outcome_label) |>
  summarise(n_metrics = n_distinct(metric), fraction_positive = mean(r2 > 0, na.rm = TRUE), .groups = "drop") |>
  mutate(
    signed_direction = if_else(outcome_label == "Signed distortion", -1, 1),
    fraction_plot = signed_direction * fraction_positive,
    label = paste0(round(100 * fraction_positive), "%")
  )
joint_cv_window_panel <- robust_bounded_display_range(
  joint_cv_metric_panel$r2,
  c(joint_cv_summary_panel$r2_median, joint_cv_summary_panel$r2_q25, joint_cv_summary_panel$r2_q75),
  probs = c(.08, .92), lower = -.5, upper = .5, min_span = .12, pad_fraction = .08
)
joint_cv_metric_panel <- joint_cv_metric_panel |>
  mutate(r2_plot = squish_to_limits(r2, joint_cv_window_panel))
joint_cv_summary_panel <- joint_cv_summary_panel |>
  mutate(
    r2_median_plot = squish_to_limits(r2_median, joint_cv_window_panel),
    r2_q25_plot = squish_to_limits(r2_q25, joint_cv_window_panel),
    r2_q75_plot = squish_to_limits(r2_q75, joint_cv_window_panel)
  )

p_cv_panel <- ggplot(joint_cv_metric_panel, aes(r2_plot, y_pos, color = metric_class)) +
  geom_vline(xintercept = 0, linewidth = .27, color = "#A1A6A9") +
  geom_point(position = position_jitter(width = 0, height = .014, seed = 63), size = .34, alpha = .18) +
  geom_segment(
    data = joint_cv_summary_panel,
    aes(x = r2_q25_plot, xend = r2_q75_plot, y = y_pos, yend = y_pos, color = metric_class),
    inherit.aes = FALSE, linewidth = .58, alpha = .56, lineend = "round"
  ) +
  geom_point(
    data = joint_cv_summary_panel,
    aes(r2_median_plot, y_pos, color = metric_class),
    inherit.aes = FALSE, shape = 18, size = .98
  ) +
  facet_wrap(~outcome_label, nrow = 1) +
  scale_color_ms_metric(guide = "none") +
  scale_x_continuous(limits = joint_cv_window_panel, breaks = scales::breaks_extended(n = 3)) +
  scale_y_continuous(
    breaks = dim_map_panel$y, labels = dim_map_panel$dimension_label,
    limits = range(dim_map_panel$y) + c(-.34, .34), expand = expansion(mult = c(0, 0))
  ) +
  labs(x = "participant-grouped CV R²", y = NULL) +
  theme_rq2(base_size = 4.9) +
  theme(
    panel.grid.major.y = element_blank(), axis.line.y = element_blank(), axis.ticks.y = element_blank(),
    axis.text.y = element_text(size = 3.85), axis.text.x = element_text(size = 3.7), axis.title.x = element_text(size = 4.15),
    strip.text = element_text(size = 4.15, face = "bold"), panel.spacing = grid::unit(.45, "mm"), plot.margin = margin(.7, .7, .7, .7)
  )

FRACTION_COLORS_PANEL <- c("Absolute distortion" = "#707B83", "Signed distortion" = "#B7BDC1")
p_frac_panel <- ggplot(joint_cv_positive_panel, aes(fraction_plot, y)) +
  geom_vline(xintercept = 0, linewidth = .27, color = "#A1A6A9") +
  geom_segment(
    aes(x = 0, xend = fraction_plot, y = y, yend = y, color = outcome_label),
    linewidth = 2.4, alpha = .82, lineend = "butt"
  ) +
  geom_text(
    aes(x = fraction_plot, label = label, hjust = if_else(fraction_plot >= 0, -.08, 1.08)),
    size = 1.35, color = "#54595C"
  ) +
  scale_color_manual(values = FRACTION_COLORS_PANEL, guide = "none") +
  scale_x_continuous(limits = c(-1.16, 1.16), breaks = c(-1, -.5, 0, .5, 1), labels = c("100", "50", "0", "50", "100")) +
  scale_y_continuous(limits = range(dim_map_panel$y) + c(-.34, .34), expand = expansion(mult = c(0, 0))) +
  labs(title = "% metrics with CV R² > 0", x = "%", y = NULL) +
  theme_rq2(base_size = 4.75) +
  theme(
    panel.grid.major.y = element_blank(), panel.grid.minor = element_blank(), axis.line.y = element_blank(),
    axis.ticks.y = element_blank(), axis.text.y = element_blank(), axis.text.x = element_text(size = 3.55), axis.title.x = element_text(size = 3.9),
    plot.title = element_text(size = 4.05, hjust = .5, face = "bold", margin = margin(b = .7)), plot.margin = margin(.7, 1.15, .7, .3)
  )
p2c_core <- cowplot::plot_grid(p_cv_panel, p_frac_panel, ncol = 2, rel_widths = c(.77, .23), align = "hv", axis = "tb", greedy = TRUE)
p2c <- cowplot::ggdraw() +
  cowplot::draw_plot(p2c_core, x = 0, y = 0, width = 1, height = .955) +
  cowplot::draw_label("c  Out-of-sample contextual predictability", x = .004, y = .997, hjust = 0, vjust = 1, fontface = "bold", size = 6.15)

metric_legend_plot <- ggplot(
  tibble(metric_class = factor(METRIC_CLASSES, levels = METRIC_CLASSES), x = 1, y = 1),
  aes(x, y, color = metric_class)
) +
  geom_point(size = 1.05) +
  scale_color_ms_metric() +
  guides(color = guide_legend(title = NULL, nrow = 2, byrow = TRUE,
                              override.aes = list(size = 1.05, alpha = 1))) +
  theme_void(base_family = MS_FONT) +
  theme(
    legend.position = "bottom", legend.text = element_text(size = 3.75),
    legend.key.width = grid::unit(2.1, "mm"), legend.spacing.x = grid::unit(.45, "mm"),
    legend.margin = margin(0, 0, 0, 0)
  )
metric_legend_right <- cowplot::get_legend(metric_legend_plot)

readr::write_csv(
  joint_cv_metric |>
    mutate(
      dimension = as.character(dimension),
      metric_class = as.character(metric_class)
    ),
  file.path("results", "rq2", "fig2_joint_context_cv.csv"), na = ""
)

# Redesigned main-text structure: predictor atlas at left; conditional geometry
# above participant-grouped predictability at right.
right_top <- cowplot::plot_grid(
  metric_legend_right, p2b,
  ncol = 1, rel_heights = c(.10, .90),
  align = "v", axis = "l", greedy = TRUE
)
right_column <- cowplot::plot_grid(
  right_top, p2c,
  ncol = 1, rel_heights = c(.70, .30),
  align = "v", axis = "l", greedy = TRUE
)
p2 <- cowplot::plot_grid(
  p2a, right_column,
  ncol = 2, rel_widths = c(.64, .36),
  align = "hv", axis = "tb", greedy = TRUE
)
ms_plot_save(p2, file.path(OUT_DIR, "Fig2_RQ2.pdf"), 8.2, 4.64)
ms_plot_save(p2, file.path(OUT_DIR, "Fig2_RQ2.png"), 8.2, 4.64)

readr::write_csv(conditional_profile_summary |>
  mutate(metric_class = as.character(metric_class), state_bin_label = as.character(state_bin_label)),
  file.path("results", "rq2", "fig2_conditional_profile.csv"), na = "")
readr::write_csv(predictor_order |>
  mutate(predictor_family = as.character(predictor_family)),
  file.path("results", "rq2", "fig2_context_predictor_order.csv"), na = "")
readr::write_csv(coef_metric_display |>
  mutate(predictor_family = as.character(predictor_family), outcome_label = as.character(outcome_label)),
  file.path("results", "rq2", "fig2_context_predictor_display_diagnostics.csv"), na = "")
readr::write_csv(coef_summary |>
  mutate(predictor_family = as.character(predictor_family), outcome_label = as.character(outcome_label)),
  file.path("results", "rq2", "fig2_context_predictor_summary.csv"), na = "")
readr::write_csv(
  tidyr::crossing(
    PREDICTOR_CATALOG |>
      transmute(term, predictor = label, predictor_family = as.character(predictor_family)),
    dimension = DIMENSIONS,
    outcome = c("Signed", "Absolute")
  ) |>
    left_join(
      coef_summary |>
        transmute(term, dimension, outcome = as.character(outcome_label), n_display_units, n_metrics,
                  Q05 = estimate_q05, Q25 = estimate_q25, Q50 = estimate_q50,
                  Q75 = estimate_q75, Q95 = estimate_q95),
      by = c("term", "dimension", "outcome")
    ) |>
    arrange(factor(predictor_family, levels = FAMILY_LEVELS), predictor, dimension, outcome),
  file.path("results", "rq2", "fig2_context_predictor_quantiles.csv"), na = ""
)
readr::write_csv(context_task |>
  mutate(dimension = as.character(dimension), information = as.character(information)),
  file.path("results", "rq2", "fig2_context_increment.csv"), na = "")
readr::write_csv(transition_spread |>
  mutate(dimension = as.character(dimension), transition_key = as.character(transition_key)),
  file.path("results", "rq2", "fig2_transition_spread.csv"), na = "")

# Complete conditional atlas retained as supplementary audit view.
p2_atlas <- ggplot(conditional, aes(interaction(pair_label, state_bin_label, sep = "\n"), metric)) +
  geom_point(aes(size = A_conditional, fill = direction_ratio), shape = 21,
             color = "#3B3B3B", stroke = .14, alpha = .92) +
  facet_grid(rows = vars(metric_class), cols = vars(transition_family, dimension),
             scales = "free", space = "free", switch = "y") +
  ms_direction_scale(name = "B / A") +
  ms_magnitude_size_scale(name = "A = conditional mean |z|", range = c(.25, 3.0)) +
  labs(title = "Complete conditional geometry atlas",
       x = "oriented transition × transition-local exposure state", y = NULL) +
  ms_atlas_theme(base_size = 6.0, x_angle = 52) +
  theme(axis.text.x = element_text(size = 4.5))
ms_plot_save(p2_atlas, file.path(OUT_DIR, "FigS_RQ2_conditional_atlas.pdf"), 16, 11.5)
ms_plot_save(p2_atlas, file.path(OUT_DIR, "FigS_RQ2_conditional_atlas.png"), 16, 11.5)
readr::write_csv(conditional |>
  mutate(metric = as.character(metric), metric_class = as.character(metric_class),
         dimension = as.character(dimension), transition_family = as.character(transition_family)),
  file.path("results", "rq2", "fig2_conditional_geometry_atlas.csv"), na = "")

# Former main-text transition-spread panel retained as a compact supplement.
p2_spread_s <- ggplot(transition_spread, aes(span_median, transition_key)) +
  geom_segment(aes(x = span_q25, xend = span_q75, yend = transition_key),
               linewidth = 1.0, color = "#9FB7C6", alpha = .58, lineend = "round") +
  geom_point(shape = 18, size = 2.0, color = MS_PRIMARY) +
  facet_wrap(~dimension, ncol = 2, scales = "free_y") +
  scale_y_discrete(labels = function(x) sub("^.*\\|\\|\\|", "", x)) +
  scale_x_continuous(trans = scales::transform_asinh(), breaks = scales::breaks_extended(n = 4)) +
  labs(title = "Transitions with the largest state-dependent spread",
       x = "median state span in A", y = NULL) +
  theme_rq2(base_size = 6.5) +
  theme(panel.grid.major.y = element_blank(), axis.line.y = element_blank(), axis.ticks.y = element_blank())
ms_plot_save(p2_spread_s, file.path(OUT_DIR, "FigS_RQ2_state_spread.pdf"), 7.4, 4.6)
ms_plot_save(p2_spread_s, file.path(OUT_DIR, "FigS_RQ2_state_spread.png"), 7.4, 4.6)

# Family-level incremental grouped-CV information retained as a supplement.
CONTEXT_COLORS <- c("External beyond state" = MS_PRIMARY, "State beyond external" = MS_SECONDARY)
CONTEXT_SHAPES <- c("External beyond state" = 16, "State beyond external" = 17)
if (nrow(context_task)) {
  context_s_plot <- context_task |>
    mutate(dimension_num = as.integer(dimension),
           y_pos = dimension_num + if_else(information == "External beyond state", -.11, .11))
  context_s_summary <- context_summary |>
    mutate(dimension_num = as.integer(dimension),
           y_pos = dimension_num + if_else(information == "External beyond state", -.11, .11))
  p2_context_s <- ggplot(context_s_plot, aes(delta_r2, y_pos, color = information, shape = information)) +
    geom_vline(xintercept = 0, linewidth = .30, color = "#9DA2A5") +
    geom_point(position = position_jitter(width = 0, height = .035, seed = 56), size = .58, alpha = .18) +
    geom_segment(data = context_s_summary,
                 aes(x = delta_q25, xend = delta_q75, y = y_pos, yend = y_pos, color = information),
                 inherit.aes = FALSE, linewidth = 1.0, alpha = .58, lineend = "round") +
    geom_point(data = context_s_summary,
               aes(delta_median, y_pos, color = information, shape = information),
               inherit.aes = FALSE, size = 1.55) +
    facet_wrap(~outcome_label, nrow = 1) +
    scale_color_manual(values = CONTEXT_COLORS, drop = FALSE) +
    scale_shape_manual(values = CONTEXT_SHAPES, drop = FALSE) +
    scale_x_continuous(trans = scales::transform_asinh(), breaks = scales::breaks_extended(n = 4)) +
    scale_y_continuous(breaks = seq_along(DIMENSIONS), labels = unname(DIM_TITLES[DIMENSIONS]),
                       limits = c(.55, length(DIMENSIONS) + .45)) +
    labs(title = "Independent contextual information",
         x = "incremental participant-grouped CV R²", y = NULL) +
    theme_rq2(base_size = 6.4, legend_position = "bottom") +
    theme(panel.grid.major.y = element_blank(), axis.line.y = element_blank(), axis.ticks.y = element_blank(),
          axis.text.y = element_text(size = 5.0), strip.text = element_text(size = 5.7),
          legend.text = element_text(size = 4.7))
} else {
  p2_context_s <- ggplot() + theme_void(base_family = MS_FONT) +
    annotate("text", x = 0, y = 0, label = "No paired grouped-CV model results", size = 2.2)
}
ms_plot_save(p2_context_s, file.path(OUT_DIR, "FigS_RQ2_context_increment.pdf"), 7.6, 4.8)
ms_plot_save(p2_context_s, file.path(OUT_DIR, "FigS_RQ2_context_increment.png"), 7.6, 4.8)

# =============================================================================
# Fig. 3 — cross-dimensional non-additivity
# =============================================================================

# The canonical gamma artifact is summarized here only for display. The
# scientific estimand remains the transition-level R = mean(gamma) and
# Q = mean(abs(gamma)); no new interaction estimand is introduced.
format_gamma_transition <- function(x) {
  x |>
    str_replace_all("_LIGHT_to_MEDI", paste0(" \u00B7 LIGHT \u2192 MEDI")) |>
    str_replace_all("([0-9]+)to([0-9]+)", "\\1 \u2192 \\2 s") |>
    str_replace_all("_", " \u00B7 ")
}

PAIR_LEVELS <- c(
  paste("Placement", "optical", sep = " \u00D7 "),
  paste("Optical", "temporal", sep = " \u00D7 "),
  paste("Placement", "temporal", sep = " \u00D7 ")
)
PAIR_CODES <- c(
  "placement__optical", "optical__temporal", "placement__temporal"
)
names(PAIR_CODES) <- PAIR_LEVELS
PAIR_CODE_TO_LABEL <- setNames(names(PAIR_CODES), unname(PAIR_CODES))
NUMERIC_TOL <- 1e-12
PAIR_LABELS <- setNames(PAIR_LEVELS, PAIR_LEVELS)

# Plot-only summaries must remain defined when this script is run standalone;
# these helpers remove non-finite values without changing the frozen artifact.
safe_median <- function(x) {
  x <- as.numeric(x)
  x <- x[is.finite(x)]
  if (length(x)) unname(stats::median(x)) else NA_real_
}
safe_q <- function(x, p) {
  x <- as.numeric(x)
  x <- x[is.finite(x)]
  if (length(x)) unname(stats::quantile(x, p, names = FALSE)) else NA_real_
}

gamma_plot <- gamma_summary |>
  mutate(
    dimension_pair = case_when(
      dimension_a == "placement" & dimension_b == "optical" ~ PAIR_LEVELS[[1]],
      dimension_a == "placement" & dimension_b == "temporal" ~ PAIR_LEVELS[[3]],
      dimension_a == "optical" & dimension_b == "temporal" ~ PAIR_LEVELS[[2]],
      TRUE ~ paste(dimension_a, "\u00D7", dimension_b)
    ),
    pair_code = case_when(
      dimension_a == "placement" & dimension_b == "optical" ~ PAIR_CODES[[1]],
      dimension_a == "optical" & dimension_b == "temporal" ~ PAIR_CODES[[2]],
      dimension_a == "placement" & dimension_b == "temporal" ~ PAIR_CODES[[3]],
      TRUE ~ NA_character_
    ),
    dimension_pair = factor(dimension_pair, levels = PAIR_LEVELS),
    metric = as.character(metric),
    metric_class = factor(metric_class, levels = METRIC_CLASSES),
    transition_display = format_gamma_transition(transition),
    Q = as.numeric(Q),
    R = as.numeric(R)
  )
if (any(is.finite(gamma_plot$Q) & gamma_plot$Q < -NUMERIC_TOL, na.rm = TRUE)) {
  stop("RQ2 gamma Q contains a negative value", call. = FALSE)
}
gamma_plot <- gamma_plot |> mutate(Q = abs(Q))

# One row per metric and dimension pair. Q_metric and C_metric are display
# projections of the canonical transition-level summaries:
# Q_metric = median_t(Q_mpt), C_metric = median_t(R_mpt / Q_mpt).
gamma_metric <- gamma_plot |>
  filter(is.finite(R) | is.finite(Q)) |>
  group_by(pair_code, dimension_pair, metric, metric_class) |>
  summarise(
    Q_metric = safe_median(Q),
    R_metric = safe_median(R),
    C_metric = safe_median(if_else(
      is.finite(R) & is.finite(Q) & Q > NUMERIC_TOL, R / Q, NA_real_
    )),
    n_transitions = n_distinct(transition),
    .groups = "drop"
  ) |>
  mutate(
    C_metric = if_else(is.finite(C_metric), pmax(-1, pmin(1, C_metric)), NA_real_),
    dimension_pair = factor(dimension_pair, levels = PAIR_LEVELS)
  )

metric_table <- gamma_plot |>
  distinct(metric, metric_class) |>
  mutate(metric = as.character(metric), metric_class = as.character(metric_class)) |>
  left_join(
    metric_order |>
      transmute(metric = as.character(metric), rq1_metric_order = metric_order),
    by = "metric"
  ) |>
  distinct(metric, .keep_all = TRUE)

# -----------------------------------------------------------------------------
# Panel a — target-level susceptibility
# -----------------------------------------------------------------------------
if (FALSE) {
metric_pair_grid <- tidyr::crossing(
  metric = metric_table$metric,
  dimension_pair = PAIR_LEVELS
) |>
  left_join(metric_table, by = "metric") |>
  left_join(
    gamma_metric |>
      mutate(dimension_pair = as.character(dimension_pair)) |>
      select(dimension_pair, metric, Q_metric),
    by = c("metric", "dimension_pair")
  )

metric_row_table <- metric_pair_grid |>
  group_by(metric, metric_class, rq1_metric_order) |>
  summarise(overall_Q = safe_median(Q_metric), .groups = "drop") |>
  arrange(desc(overall_Q), rq1_metric_order, metric) |>
  mutate(
    metric_axis = factor(metric, levels = rev(metric)),
    metric_label = str_to_sentence(str_replace_all(metric, "_", " ")),
    metric_label_short = metric_label |>
      str_replace("^Mean ", "") |>
      str_replace_all(" pulses above ", " pulses >") |>
      str_replace_all(" above ", " >") |>
      str_replace_all(" number of ", " n ")
  )

metric_pair_grid <- metric_pair_grid |>
  left_join(metric_row_table |> select(metric, metric_axis), by = "metric") |>
  mutate(
    dimension_pair = factor(dimension_pair, levels = PAIR_LEVELS),
    pair_index = match(as.character(dimension_pair), PAIR_LEVELS),
    metric_y = as.numeric(metric_axis)
  )

metric_label_top <- metric_row_table |>
  filter(is.finite(overall_Q)) |>
  slice_max(overall_Q, n = 6L, with_ties = FALSE) |>
  pull(metric)

metric_label_directional <- gamma_metric |>
  filter(is.finite(Q_metric), is.finite(C_metric), Q_metric >= safe_median(Q_metric)) |>
  group_by(metric) |>
  summarise(max_abs_C = max(abs(C_metric), na.rm = TRUE), .groups = "drop") |>
  filter(is.finite(max_abs_C)) |>
  slice_max(max_abs_C, n = 4L, with_ties = FALSE) |>
  pull(metric)

metric_label_set <- union(metric_label_top, metric_label_directional)
metric_pair_grid <- metric_pair_grid |>
  mutate(highlight = metric %in% metric_label_set)
metric_label_table <- metric_row_table |>
  mutate(
    label = if_else(
      metric %in% metric_label_set,
      metric_label_short,
      ""
    )
  )
# Highlight a small, data-driven set: the overall Q leaders plus coherent
# directional exceptions. The labels are short display names only.

a_q_max <- max(metric_pair_grid$Q_metric, na.rm = TRUE)
if (!is.finite(a_q_max) || a_q_max <= 0) a_q_max <- 1
a_q_max <- a_q_max * 1.04
a_class_x <- -a_q_max * .64
a_gap <- a_q_max * .30
a_starts <- (seq_along(PAIR_LEVELS) - 1) * (a_q_max + a_gap)
metric_pair_grid <- metric_pair_grid |>
  mutate(x_plot = a_starts[pair_index] + Q_metric)
a_ticks <- pretty(c(0, a_q_max), n = 4)
a_ticks <- a_ticks[a_ticks >= 0 & a_ticks <= a_q_max]
if (!length(a_ticks) || a_ticks[[1]] > 0) a_ticks <- c(0, a_ticks)
a_tick_breaks <- unlist(lapply(a_starts, function(x) x + a_ticks), use.names = FALSE)
a_tick_labels <- format(a_ticks, trim = TRUE, scientific = FALSE)
a_headers <- tibble(
  x = a_starts + a_q_max / 2,
  y = nrow(metric_row_table) + .42,
  label = c(
    paste0("Placement ", "\u00D7", "\noptical"),
    paste0("Optical ", "\u00D7", "\ntemporal"),
    paste0("Placement ", "\u00D7", "\ntemporal")
  )
)

p3a <- ggplot(metric_pair_grid, aes(pair_index, metric_y)) +
  geom_vline(
    xintercept = a_starts[-1] - a_gap / 2,
    colour = "#E5E8E9", linewidth = .28
  ) +
  geom_line(
    data = metric_pair_grid |> filter(highlight),
    aes(x = x_plot, group = metric), colour = "#D7DCDE", linewidth = .38,
    na.rm = TRUE
  ) +
  geom_tile(
    data = metric_row_table,
    aes(x = a_class_x, y = as.numeric(metric_axis), fill = metric_class),
    inherit.aes = FALSE, width = a_q_max * .075, height = .76,
    colour = "white", linewidth = .12
  ) +
  geom_point(
    data = metric_pair_grid |> filter(!is.finite(Q_metric)),
    aes(x = x_plot),
    shape = 4, size = .58, stroke = .28, colour = "#D1D7DA", na.rm = TRUE
  ) +
  geom_point(
    data = metric_pair_grid |> filter(is.finite(Q_metric), !highlight),
    aes(x = x_plot),
    shape = 16, size = .68, colour = "#B9C1C5", alpha = .76
  ) +
  geom_point(
    data = metric_pair_grid |> filter(is.finite(Q_metric), highlight),
    aes(x = x_plot, color = metric_class), shape = 16, size = 1.18, alpha = .98
  ) +
  geom_text(
    data = metric_label_table |> filter(nzchar(label)),
    aes(x = a_starts[[1]] - .035, y = as.numeric(metric_axis), label = label),
    inherit.aes = FALSE, hjust = 1, colour = "#394044", size = 1.55,
    check_overlap = TRUE
  ) +
  geom_text(
    data = a_headers, aes(x, y, label = label), inherit.aes = FALSE,
    fontface = "bold", size = 1.75, lineheight = .88, colour = "#303437"
  ) +
  scale_color_ms_metric(guide = "none") +
  scale_fill_ms_metric(guide = "none") +
  scale_x_continuous(
    breaks = a_tick_breaks, labels = rep(a_tick_labels, length(a_starts)),
    limits = c(a_starts[[1]] - a_q_max * .80, tail(a_starts, 1) + a_q_max + a_gap * .12),
    expand = expansion(mult = c(0, .01))
  ) +
  scale_y_continuous(
    breaks = NULL, limits = c(.45, nrow(metric_row_table) + .78),
    expand = expansion(mult = c(0, 0))
  ) +
  labs(
    title = "a  Target representations differ systematically in non-additivity",
    subtitle = "within each column, x = median Q across local transitions; lines link the same target representation",
    x = "median Q", y = NULL
  ) +
  coord_cartesian(clip = "off") +
  theme_rq2(base_size = 5.9) +
  theme(
    panel.grid.major.y = element_blank(), panel.grid.major.x = element_blank(),
    axis.line.y = element_blank(), axis.ticks.y = element_blank(),
    axis.text.y = element_blank(), axis.text.x = element_text(size = 4.0, lineheight = .85),
    axis.ticks.x = element_line(colour = "#505457", linewidth = .28),
    axis.title.x = element_text(size = 4.45),
    plot.title = element_text(size = 6.35, hjust = 0, margin = margin(b = 2)),
    plot.subtitle = element_text(size = 4.25, colour = "#666A6D", hjust = 0,
                                 margin = margin(t = -1, b = 3)),
    plot.margin = margin(1, 3, 1, 3)
  )

# -----------------------------------------------------------------------------
# Panel b — full transition distribution
# -----------------------------------------------------------------------------
# This uses the original metric-by-transition Q rows. The only additional
# objects are display summaries for the interval and median overlays.
format_transition_c <- function(pair, x) {
  out <- x
  is_placement_optical <- pair == PAIR_CODES[[1]]
  out[is_placement_optical] <- recode(
    x[is_placement_optical],
    chest_LIGHT_to_MEDI = paste0("chest \u00B7 LIGHT \u2192 MEDI"),
    wrist_LIGHT_to_MEDI = paste0("wrist \u00B7 LIGHT \u2192 MEDI"),
    .default = x[is_placement_optical]
  )
  out[!is_placement_optical] <- str_replace_all(
    str_replace_all(x[!is_placement_optical], "^(chest|wrist)_", paste0("\\1 \u00B7 ")),
    "([0-9]+)to([0-9]+)", "\\1 \u2192 \\2 s"
  )
  out
}

transition_order_raw <- c(
  "chest_LIGHT_to_MEDI", "wrist_LIGHT_to_MEDI",
  "120to60", "60to40", "40to30", "30to20", "20to10",
  "chest_120to60", "chest_60to40", "chest_40to30",
  "chest_30to20", "chest_20to10",
  "wrist_120to60", "wrist_60to40", "wrist_40to30",
  "wrist_30to20", "wrist_20to10"
)
transition_levels_b <- paste(
  rep(unname(PAIR_CODES), c(2L, 5L, 10L)), transition_order_raw, sep = "|||"
)
transition_y_b <- setNames(
  rev(seq_along(transition_levels_b)), transition_levels_b
)

quasirandom_offset <- function(n, width = .09) {
  if (n <= 1L) return(0)
  # A deterministic low-discrepancy sequence gives a compact beeswarm-like
  # vertical spread without introducing stochastic plotting variation.
  (((seq_len(n) * 0.61803398875) %% 1) - .5) * 2 * width
}

gamma_transition_raw <- gamma_plot |>
  filter(is.finite(Q)) |>
  mutate(
    pair_code = as.character(pair_code),
    transition_key = paste(pair_code, transition, sep = "|||"),
    transition_y = unname(transition_y_b[transition_key])
  ) |>
  arrange(transition_y, Q, metric) |>
  group_by(pair_code, transition) |>
  mutate(transition_y_jitter = transition_y + quasirandom_offset(n(), .085)) |>
  ungroup()

b_background_bands <- tibble(
  ymin = c(.5, 15.5), ymax = c(10.5, 17.5)
)

gamma_transition_raw <- gamma_transition_raw |>
  mutate(transition_y_jitter = pmax(.42, pmin(17.58, transition_y_jitter)))

gamma_transition_stats <- gamma_plot |>
  filter(is.finite(Q) | is.finite(R)) |>
  mutate(
    pair_code = as.character(pair_code),
    transition_key = paste(pair_code, transition, sep = "|||"),
    transition_y = unname(transition_y_b[transition_key]),
    transition_display = format_transition_c(pair_code, transition)
  ) |>
  group_by(pair_code, transition_key, transition_y, transition_display) |>
  summarise(
    n_metrics = n_distinct(metric),
    Q_median = safe_median(Q),
    Q_q25 = safe_q(Q, .25),
    Q_q75 = safe_q(Q, .75),
    coherence_median = safe_median(if_else(
      is.finite(R) & is.finite(Q) & Q > NUMERIC_TOL, R / Q, NA_real_
    )),
    .groups = "drop"
  ) |>
  mutate(
    coherence_median = if_else(
      is.finite(coherence_median), pmax(-1, pmin(1, coherence_median)), NA_real_
    )
  )

b_q_max <- max(c(gamma_transition_raw$Q, gamma_transition_stats$Q_q75), na.rm = TRUE)
if (!is.finite(b_q_max) || b_q_max <= 0) b_q_max <- 1
b_q_max <- b_q_max * 1.08

transition_labels_b <- gamma_transition_stats |>
  arrange(match(transition_key, transition_levels_b)) |>
  pull(transition_display)

p3b <- ggplot() +
  geom_rect(
    data = b_background_bands,
    aes(xmin = 0, xmax = b_q_max, ymin = ymin, ymax = ymax),
    inherit.aes = FALSE, fill = "#F7F9FA", alpha = .72, colour = NA
  ) +
  geom_hline(
    yintercept = c(15.5, 10.5), linewidth = .42, colour = "#B7BEC1",
    linetype = "22"
  ) +
  geom_point(
    data = gamma_transition_raw,
    aes(Q, transition_y_jitter),
    shape = 16, size = .56, alpha = .38, colour = "#7F898E"
  ) +
  geom_segment(
    data = gamma_transition_stats,
    aes(x = Q_q25, xend = Q_q75, y = transition_y, yend = transition_y),
    colour = "#969FA3", linewidth = 1.36, alpha = .82, lineend = "round"
  ) +
  geom_point(
    data = gamma_transition_stats,
    aes(Q_median, transition_y, fill = coherence_median),
    shape = 21, size = 2.78, colour = "#374044", stroke = .28
  ) +
  scale_fill_ms_diverging(
    1, name = "median R/Q", breaks = c(-1, 0, 1),
    labels = c("-1", "0", "+1"), guide = guide_colorbar(
      barwidth = grid::unit(24, "mm"), barheight = grid::unit(2.2, "mm"),
      ticks = TRUE, title.position = "top"
    )
  ) +
  scale_x_continuous(
    limits = c(0, b_q_max), breaks = scales::breaks_extended(n = 5),
    expand = expansion(mult = c(0, .015))
  ) +
  scale_y_continuous(
    limits = c(.35, 17.65), breaks = seq(17, 1), labels = transition_labels_b,
    expand = expansion(mult = c(0, 0))
  ) +
  labs(
    title = "b  Non-additivity varies across configuration transitions",
    subtitle = "raw points = metric-level Q; line = Q25-Q75; fill = median R/Q",
    x = "Q = mean(|gamma|) at each metric x transition", y = NULL
  ) +
  theme_rq2(base_size = 5.85, legend_position = "bottom") +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(colour = "#ECEFF0", linewidth = .20),
    axis.text.y = element_text(size = 4.45, colour = "#303030"),
    axis.text.x = element_text(size = 4.65), axis.title.x = element_text(size = 4.8),
    axis.ticks.y = element_blank(), axis.line.y = element_blank(),
    plot.title = element_text(size = 6.2, hjust = 0, margin = margin(b = 2)),
    plot.subtitle = element_text(size = 4.25, colour = "#666A6D", hjust = 0,
                                 margin = margin(t = -1, b = 2)),
    legend.title = element_text(size = 4.5), legend.text = element_text(size = 4.25),
    legend.margin = margin(0, 0, 0, 0), plot.margin = margin(2, 3, 1, 3)
  )

# -----------------------------------------------------------------------------
# Panel c — directional coherence
# -----------------------------------------------------------------------------
}
# Compact raincloud-style display: the polygon is the density half above the
# baseline, while metric-level points sit just below it.
coherence_points <- gamma_metric |>
  filter(is.finite(C_metric)) |>
  mutate(
    pair_code = as.character(pair_code),
    pair_y = unname(setNames(3:1, unname(PAIR_CODES))[pair_code])
  )

coherence_summary <- coherence_points |>
  group_by(pair_code, pair_y) |>
  summarise(C_median = safe_median(C_metric), n_metrics = n(), .groups = "drop")

coherence_polygon <- function(pair_code_name) {
  values <- coherence_points |>
    filter(pair_code == pair_code_name) |>
    pull(C_metric)
  pair_y <- unname(setNames(3:1, unname(PAIR_CODES))[pair_code_name])
  if (!length(values)) return(tibble())
  if (length(unique(values)) < 2L) {
    density_x <- c(values[[1]] - .01, values[[1]] + .01)
    density_h <- c(.01, .01)
  } else {
    density_fit <- stats::density(values, from = -1, to = 1, n = 256, adjust = 1)
    density_x <- density_fit$x
    density_h <- .33 * density_fit$y / max(density_fit$y)
  }
  tibble(
    pair_code = pair_code_name,
    x = c(density_x, rev(density_x)),
    y = c(pair_y + density_h, rep(pair_y, length(density_x)))
  )
}
coherence_polygons <- purrr::map_dfr(unname(PAIR_CODES), coherence_polygon)
coherence_y_labels <- c(
  paste0("Placement ", "\u00D7", "\noptical"),
  paste0("Optical ", "\u00D7", "\ntemporal"),
  paste0("Placement ", "\u00D7", "\ntemporal")
)

p3c <- ggplot() +
  geom_vline(xintercept = 0, linewidth = .32, colour = "#7E878B") +
  geom_polygon(
    data = coherence_polygons,
    aes(x, y, group = pair_code),
    fill = "#A9C7DB", colour = "#6F9CBD", linewidth = .25, alpha = .62
  ) +
  geom_segment(
    data = coherence_summary,
    aes(x = -1, xend = 1, y = pair_y, yend = pair_y),
    colour = "#D1D7DA", linewidth = .32
  ) +
  geom_point(
    data = coherence_points,
    aes(C_metric, pair_y - .10),
    position = position_jitter(width = 0, height = .085, seed = 89),
    shape = 16, size = .70, alpha = .54, colour = "#59666C"
  ) +
  geom_point(
    data = coherence_summary,
    aes(C_median, pair_y - .10),
    shape = 23, size = 2.05, fill = MS_PRIMARY, colour = "#273F50", stroke = .25
  ) +
  scale_x_continuous(
    limits = c(-1, 1), breaks = c(-1, -.5, 0, .5, 1),
    labels = c("-1", "-.5", "0", ".5", "+1"),
    expand = expansion(mult = c(.015, .015))
  ) +
  scale_y_continuous(
    limits = c(.45, 3.55), breaks = 3:1, labels = coherence_y_labels,
    expand = expansion(mult = c(0, 0))
  ) +
  labs(
    title = "c  Directional coherence is\nfrequently weak",
    subtitle = "C = median_t(R_mpt / Q_mpt)", x = "directional coherence C", y = NULL
  ) +
  theme_rq2(base_size = 5.25) +
  theme(
    panel.grid.major.y = element_blank(), panel.grid.major.x = element_line(
      colour = "#ECEFF0", linewidth = .18
    ),
    axis.text.y = element_text(size = 3.85, lineheight = .84),
    axis.text.x = element_text(size = 3.8), axis.title.x = element_text(size = 4.1),
    axis.ticks.y = element_blank(), axis.line.y = element_blank(),
    plot.title = element_text(size = 5.55, hjust = 0, margin = margin(b = 2)),
    plot.subtitle = element_text(size = 4.0, colour = "#666A6D", hjust = 0,
                                 margin = margin(t = -1, b = 2)),
    plot.margin = margin(2, 2, 2, 2)
  )

# Shared metric-class legend sits once above the figure; R/Q is encoded once in
# the median points of panel b.
if (FALSE) {
p3_bottom <- cowplot::plot_grid(
  p3b, p3c, ncol = 2, rel_widths = c(.75, .25),
  align = "hv", axis = "tblr", greedy = TRUE
)
p3 <- cowplot::plot_grid(
  metric_legend,
  cowplot::plot_grid(
    p3a, p3_bottom, ncol = 1, rel_heights = c(.86, 1.14),
    align = "v", axis = "l", greedy = TRUE
  ),
  ncol = 1, rel_heights = c(.045, 1), align = "v", axis = "l", greedy = TRUE
)
# Legacy class-first Fig.3 composition retained only as code history.
if (FALSE) {
}
ATLAS2_CLASS_ORDER <- c(
  "Overall", "timing", "duration", "level", "temporal dynamics",
  "exposure history", "spectrum"
)
ATLAS2_PAIR_LABELS <- c(
  "Placement \u00D7\noptical", "Optical \u00D7\ntemporal", "Placement \u00D7\ntemporal"
)
names(ATLAS2_PAIR_LABELS) <- PAIR_LEVELS
ATLAS2_CLASS_COLORS <- c("Overall" = "#343B3F", MS_METRIC_COLORS)

atlas2_class_counts <- metric_table |>
  mutate(metric_class = as.character(metric_class)) |>
  count(metric_class, name = "n_metrics")
atlas2_n_lookup <- c(
  Overall = nrow(metric_table),
  setNames(atlas2_class_counts$n_metrics, atlas2_class_counts$metric_class)
)
atlas2_row_labels <- setNames(
  paste0(
    c("Overall", str_to_sentence(ATLAS2_CLASS_ORDER[-1])),
    " (n=", unname(atlas2_n_lookup[ATLAS2_CLASS_ORDER]), ")"
  ),
  ATLAS2_CLASS_ORDER
)

atlas2_metric_q <- gamma_metric |>
  filter(is.finite(Q_metric)) |>
  mutate(
    pair_code = as.character(pair_code),
    metric_class = as.character(metric_class),
    atlas_class = metric_class
  ) |>
  select(pair_code, metric, metric_class, atlas_class, Q_metric)
atlas2_metric_q <- bind_rows(
  atlas2_metric_q,
  atlas2_metric_q |> mutate(atlas_class = "Overall")
) |>
  mutate(
    atlas_row = factor(
      unname(atlas2_row_labels[atlas_class]), levels = unname(atlas2_row_labels)
    ),
    dimension_pair = factor(
      unname(ATLAS2_PAIR_LABELS[PAIR_CODE_TO_LABEL[pair_code]]),
      levels = unname(ATLAS2_PAIR_LABELS)
    )
  )

atlas2_q_limit <- max(atlas2_metric_q$Q_metric, na.rm = TRUE)
if (!is.finite(atlas2_q_limit) || atlas2_q_limit <= 0) atlas2_q_limit <- 1
atlas2_q_limit <- atlas2_q_limit * 1.06

atlas2_cells <- tidyr::expand_grid(
  atlas_class = ATLAS2_CLASS_ORDER,
  pair_code = PAIR_CODES
) |>
  mutate(
    atlas_row = factor(
      unname(atlas2_row_labels[atlas_class]), levels = unname(atlas2_row_labels)
    ),
    dimension_pair = factor(
      unname(ATLAS2_PAIR_LABELS[PAIR_CODE_TO_LABEL[pair_code]]),
      levels = unname(ATLAS2_PAIR_LABELS)
    )
  )

atlas2_stats <- atlas2_metric_q |>
  group_by(atlas_class, atlas_row, dimension_pair) |>
  summarise(
    Q_median = safe_median(Q_metric),
    Q_q25 = safe_q(Q_metric, .25),
    Q_q75 = safe_q(Q_metric, .75),
    .groups = "drop"
  )

atlas2_density <- atlas2_metric_q |>
  group_by(atlas_class, dimension_pair) |>
  group_modify(~ {
    values <- .x$Q_metric[is.finite(.x$Q_metric)]
    if (length(values) < 3L || diff(range(values)) <= NUMERIC_TOL) {
      return(tibble(x = numeric(), slab_y = numeric()))
    }
    density_fit <- stats::density(
      values, from = 0, to = atlas2_q_limit, n = 128, adjust = .9
    )
    tibble(
      x = density_fit$x,
      slab_y = .62 * density_fit$y / max(density_fit$y)
    )
  }) |>
  ungroup() |>
  mutate(
    atlas_row = factor(
      unname(atlas2_row_labels[atlas_class]), levels = unname(atlas2_row_labels)
    )
  )

atlas2_raw <- atlas2_metric_q |>
  arrange(atlas_class, dimension_pair, metric) |>
  group_by(atlas_class, dimension_pair) |>
  mutate(raw_y = .025 + .095 * ((row_number() * .61803398875) %% 1)) |>
  ungroup()

p3a <- ggplot() +
  geom_blank(data = atlas2_cells, aes(x = 0, y = 0), inherit.aes = FALSE) +
  geom_hline(yintercept = 0, linewidth = .28, colour = "#AEB6BA") +
  geom_ribbon(
    data = atlas2_density,
    aes(x = x, ymin = 0, ymax = slab_y, fill = atlas_class,
        group = interaction(atlas_class, dimension_pair)),
    alpha = .56, colour = NA
  ) +
  geom_point(
    data = atlas2_raw,
    aes(Q_metric, raw_y), shape = 16, size = .50, alpha = .36,
    colour = "#667277"
  ) +
  geom_segment(
    data = atlas2_stats,
    aes(x = Q_q25, xend = Q_q75, y = .145, yend = .145,
        colour = atlas_class), linewidth = .82, lineend = "round"
  ) +
  geom_point(
    data = atlas2_stats,
    aes(Q_median, .145, fill = atlas_class), shape = 21,
    size = 1.72, colour = "#30383C", stroke = .24
  ) +
  scale_fill_manual(values = ATLAS2_CLASS_COLORS, guide = "none") +
  scale_colour_manual(values = ATLAS2_CLASS_COLORS, guide = "none") +
  scale_x_continuous(
    limits = c(0, atlas2_q_limit), breaks = scales::breaks_extended(n = 4),
    expand = expansion(mult = c(0, .015))
  ) +
  scale_y_continuous(
    limits = c(-.035, .72), breaks = NULL,
    expand = expansion(mult = c(0, 0))
  ) +
  facet_grid(
    atlas_row ~ dimension_pair, scales = "fixed", drop = FALSE, switch = "y"
  ) +
  labs(
    title = "a  Class-level non-additivity distribution atlas",
    subtitle = "Q_mp = median_t(Q_mpt); slab = within-class distribution, line = IQR, point = class median",
    x = "median Q per metric", y = NULL
  ) +
  theme_rq2(base_size = 5.75) +
  theme(
    panel.grid = element_blank(),
    panel.spacing = grid::unit(.75, "mm"),
    strip.background = element_blank(),
    strip.text.x = element_text(size = 5.0, face = "bold", lineheight = .86),
    strip.text.y.left = element_text(
      size = 4.55, angle = 0, hjust = 1, lineheight = .86,
      margin = margin(r = 2)
    ),
    strip.placement = "outside",
    axis.text.x = element_text(size = 4.0),
    axis.ticks.x = element_line(colour = "#505457", linewidth = .25),
    axis.title.x = element_text(size = 4.35),
    axis.line.x = element_line(colour = "#505457", linewidth = .30),
    plot.title = element_text(size = 6.25, hjust = 0, margin = margin(b = 2)),
    plot.subtitle = element_text(size = 4.15, colour = "#666A6D", hjust = 0,
                                 margin = margin(t = -1, b = 2)),
    plot.margin = margin(1, 3, 1, 3)
  )

format_transition_c <- function(pair, x) {
  out <- x
  is_placement_optical <- pair == PAIR_CODES[[1]]
  out[is_placement_optical] <- recode(
    x[is_placement_optical],
    chest_LIGHT_to_MEDI = paste0("chest \u00B7 LIGHT \u2192 MEDI"),
    wrist_LIGHT_to_MEDI = paste0("wrist \u00B7 LIGHT \u2192 MEDI"),
    .default = x[is_placement_optical]
  )
  out[!is_placement_optical] <- str_replace_all(
    str_replace_all(x[!is_placement_optical], "^(chest|wrist)_", paste0("\\1 \u00B7 ")),
    "([0-9]+)to([0-9]+)", "\\1 \u2192 \\2 s"
  )
  out
}

transition_order_raw <- c(
  "chest_LIGHT_to_MEDI", "wrist_LIGHT_to_MEDI",
  "120to60", "60to40", "40to30", "30to20", "20to10",
  "chest_120to60", "chest_60to40", "chest_40to30",
  "chest_30to20", "chest_20to10",
  "wrist_120to60", "wrist_60to40", "wrist_40to30",
  "wrist_30to20", "wrist_20to10"
)

format_b2_transition <- function(pair, x) {
  out <- format_transition_c(pair, x)
  is_placement_temporal <- pair == PAIR_CODES[[3]]
  out[is_placement_temporal] <- str_replace(
    out[is_placement_temporal], "^(chest|wrist) \u00B7 ", ""
  )
  out
}

transition_pairs_b2 <- rep(unname(PAIR_CODES), c(2L, 5L, 10L))
transition_order_b2 <- tibble(
  pair_code = transition_pairs_b2,
  transition = transition_order_raw,
  x_label = format_b2_transition(transition_pairs_b2, transition_order_raw)
) |>
  mutate(
    x_label = if_else(
      pair_code == PAIR_CODES[[1]],
      str_replace(x_label, " \u00B7 ", " \u00B7\n"),
      x_label
    )
  )

gamma_transition_raw_b2 <- gamma_plot |>
  filter(is.finite(Q)) |>
  mutate(pair_code = as.character(pair_code), transition = as.character(transition)) |>
  left_join(transition_order_b2, by = c("pair_code", "transition"))

b2_class_stats <- gamma_plot |>
  filter(is.finite(Q)) |>
  mutate(
    pair_code = as.character(pair_code), transition = as.character(transition),
    metric_class = as.character(metric_class)
  ) |>
  group_by(pair_code, transition, metric_class) |>
  summarise(
    Q_median = safe_median(Q), Q_q25 = safe_q(Q, .25), Q_q75 = safe_q(Q, .75),
    .groups = "drop"
  )
b2_overall_stats <- gamma_plot |>
  filter(is.finite(Q)) |>
  mutate(pair_code = as.character(pair_code), transition = as.character(transition)) |>
  group_by(pair_code, transition) |>
  summarise(
    Q_median = safe_median(Q), Q_q25 = safe_q(Q, .25), Q_q75 = safe_q(Q, .75),
    .groups = "drop"
  ) |>
  mutate(metric_class = "Overall")
b2_transition_stats <- bind_rows(b2_overall_stats, b2_class_stats) |>
  left_join(transition_order_b2, by = c("pair_code", "transition")) |>
  mutate(
    placement = case_when(
      pair_code == PAIR_CODES[[3]] & str_starts(transition, "chest_") ~ "chest",
      pair_code == PAIR_CODES[[3]] & str_starts(transition, "wrist_") ~ "wrist",
      TRUE ~ "all"
    )
  )

b2_q_limit <- max(c(gamma_transition_raw_b2$Q, b2_transition_stats$Q_q75), na.rm = TRUE)
if (!is.finite(b2_q_limit) || b2_q_limit <= 0) b2_q_limit <- 1
b2_q_limit <- b2_q_limit * 1.08
B2_CLASS_COLORS <- c("Overall" = "#343B3F", MS_METRIC_COLORS)

make_b2_section <- function(pair_name, section_title, section_type, x_order) {
  raw <- gamma_transition_raw_b2 |>
    filter(pair_code == pair_name) |>
    mutate(x_index = match(x_label, x_order))
  stats <- b2_transition_stats |>
    filter(pair_code == pair_name) |>
    mutate(x_index = match(x_label, x_order))
  stats_class <- stats |> filter(metric_class != "Overall")
  stats_overall <- stats |> filter(metric_class == "Overall")
  placement_type <- section_type == "placement_temporal"

  p <- ggplot() +
    geom_point(
      data = raw, aes(x_index, Q), shape = 16, size = .36,
      alpha = .14, colour = "#707B80",
      position = position_jitter(width = .055, height = 0, seed = 73)
    )

  if (section_type == "placement_optical") {
    p <- p +
      geom_errorbar(
        data = stats_class,
        aes(x_index, ymin = Q_q25, ymax = Q_q75, colour = metric_class),
        width = .10, linewidth = .62, alpha = .86
      ) +
      geom_errorbar(
        data = stats_overall,
        aes(x_index, ymin = Q_q25, ymax = Q_q75),
        width = .12, linewidth = .92, colour = B2_CLASS_COLORS[["Overall"]]
      )
  } else {
    p <- p +
      geom_ribbon(
        data = stats_class,
        aes(
          x = x_index, ymin = Q_q25, ymax = Q_q75, fill = metric_class,
          group = if (placement_type) interaction(metric_class, placement) else metric_class
        ),
        alpha = .12, colour = NA
      ) +
      geom_ribbon(
        data = stats_overall,
        aes(x = x_index, ymin = Q_q25, ymax = Q_q75, group = 1),
        fill = B2_CLASS_COLORS[["Overall"]], alpha = .12, colour = NA
      )
  }

  if (!placement_type) {
    p <- p +
      geom_line(
        data = stats_class,
        aes(x_index, Q_median, colour = metric_class, group = metric_class),
        linewidth = .64, alpha = .86, na.rm = TRUE
      ) +
      geom_line(
        data = stats_overall,
        aes(x_index, Q_median, group = 1),
        linewidth = 1.02, colour = B2_CLASS_COLORS[["Overall"]], na.rm = TRUE
      )
  } else {
    p <- p +
      geom_line(
        data = stats_class |> filter(placement == "chest"),
        aes(x_index, Q_median, colour = metric_class, group = metric_class),
        linewidth = .64, alpha = .86, linetype = "solid", na.rm = TRUE
      ) +
      geom_line(
        data = stats_class |> filter(placement == "wrist"),
        aes(x_index, Q_median, colour = metric_class, group = metric_class),
        linewidth = .64, alpha = .86, linetype = "22", na.rm = TRUE
      ) +
      geom_line(
        data = stats_overall |> filter(placement == "chest"),
        aes(x_index, Q_median, group = 1),
        linewidth = 1.02, colour = B2_CLASS_COLORS[["Overall"]],
        linetype = "solid", na.rm = TRUE
      ) +
      geom_line(
        data = stats_overall |> filter(placement == "wrist"),
        aes(x_index, Q_median, group = 1),
        linewidth = 1.02, colour = B2_CLASS_COLORS[["Overall"]],
        linetype = "22", na.rm = TRUE
      )
  }

  p <- p +
    geom_point(
      data = stats_class,
      aes(x_index, Q_median, colour = metric_class), shape = 16,
      size = 1.16, alpha = .96
    ) +
    geom_point(
      data = stats_overall,
      aes(x_index, Q_median), shape = 21, size = 1.78,
      fill = B2_CLASS_COLORS[["Overall"]], colour = "white", stroke = .26
    ) +
    scale_x_continuous(
      breaks = seq_along(x_order), labels = x_order,
      limits = c(.72, length(x_order) + .28), expand = expansion(mult = c(0, 0))
    ) +
    scale_y_continuous(
      limits = c(0, b2_q_limit), breaks = scales::breaks_extended(n = 4),
      expand = expansion(mult = c(0, .015))
    ) +
    scale_colour_manual(
      values = B2_CLASS_COLORS, limits = names(B2_CLASS_COLORS), guide = "none"
    ) +
    scale_fill_manual(
      values = B2_CLASS_COLORS, limits = names(B2_CLASS_COLORS), guide = "none"
    ) +
    labs(
      title = section_title,
      x = NULL,
      y = if (section_type == "placement_optical") "Q" else NULL
    ) +
    theme_rq2(base_size = 5.45) +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(colour = "#ECEFF0", linewidth = .18),
      axis.text.x = element_text(size = 4.05, lineheight = .82, margin = margin(t = 1)),
      axis.text.y = element_text(size = 3.9),
      axis.title.y = if (section_type == "placement_optical") {
        element_text(size = 4.1, margin = margin(r = 2))
      } else {
        element_blank()
      },
      axis.ticks.x = element_line(colour = "#505457", linewidth = .24),
      axis.ticks.y = element_line(colour = "#505457", linewidth = .20),
      axis.line.x = element_line(colour = "#505457", linewidth = .28),
      axis.line.y = element_blank(),
      plot.title = element_text(size = 5.05, face = "bold", hjust = 0,
                                margin = margin(b = 1)),
      plot.margin = margin(0, 3, 0, 3)
    )
  p
}

b2_po_order <- transition_order_b2 |>
  filter(pair_code == PAIR_CODES[[1]]) |>
  pull(x_label)
b2_ot_order <- transition_order_b2 |>
  filter(pair_code == PAIR_CODES[[2]]) |>
  pull(x_label)
b2_pt_order <- transition_order_b2 |>
  filter(pair_code == PAIR_CODES[[3]]) |>
  distinct(x_label) |>
  pull(x_label)

p3b_po <- make_b2_section(
  PAIR_CODES[[1]], "Placement \u00D7 optical", "placement_optical", b2_po_order
)
p3b_ot <- make_b2_section(
  PAIR_CODES[[2]], "Optical \u00D7 temporal", "optical_temporal", b2_ot_order
)
p3b_pt <- make_b2_section(
  PAIR_CODES[[3]], "Placement \u00D7 temporal", "placement_temporal", b2_pt_order
)
p3b_header <- cowplot::ggdraw() +
  cowplot::draw_label(
    "b  Where non-additivity accrues across ordered transitions",
    x = 0, y = .52, hjust = 0, vjust = .5, fontfamily = MS_FONT,
    fontface = "bold", size = 6.2, colour = "#151515"
  )
p3b <- cowplot::plot_grid(
  p3b_header, p3b_po, p3b_ot, p3b_pt, ncol = 1,
  rel_heights = c(.22, 1, 1, 1.10), align = "v", axis = "l", greedy = TRUE
)

p3_bottom <- cowplot::plot_grid(
  p3b, p3c, ncol = 2, rel_widths = c(.75, .25),
  align = "hv", axis = "tblr", greedy = TRUE
)
p3 <- cowplot::plot_grid(
  metric_legend,
  cowplot::plot_grid(
    p3a, p3_bottom, ncol = 1, rel_heights = c(1.20, 1.00),
    align = "v", axis = "l", greedy = TRUE
  ),
  ncol = 1, rel_heights = c(.045, 1), align = "v", axis = "l", greedy = TRUE
)
ms_plot_save(p3, file.path(OUT_DIR, "Fig3_RQ2.png"), 7.20, 7.05)
}
# Current main-text Fig.3 composition: summary strips first, ordered transition
# backbones second, and the existing coherence supporting panel at right.
ATLAS3_CLASS_ORDER <- c(
  "Overall", "temporal dynamics", "timing", "duration", "level"
)
ATLAS3_PAIR_LABELS <- c(
  "Placement \u00D7\noptical", "Optical \u00D7\ntemporal", "Placement \u00D7\ntemporal"
)
names(ATLAS3_PAIR_LABELS) <- PAIR_LEVELS
ATLAS3_CLASS_COLORS <- c("Overall" = "#343B3F", MS_METRIC_COLORS)

atlas3_class_counts <- metric_table |>
  mutate(metric_class = as.character(metric_class)) |>
  count(metric_class, name = "n_metrics")
atlas3_n_lookup <- c(
  Overall = nrow(metric_table),
  setNames(atlas3_class_counts$n_metrics, atlas3_class_counts$metric_class)
)
atlas3_row_labels <- setNames(
  paste0(
    c("Overall", str_to_sentence(ATLAS3_CLASS_ORDER[-1])),
    " (n=", unname(atlas3_n_lookup[ATLAS3_CLASS_ORDER]), ")"
  ),
  ATLAS3_CLASS_ORDER
)

atlas3_metric_q <- gamma_metric |>
  filter(is.finite(Q_metric)) |>
  mutate(
    pair_code = as.character(pair_code),
    metric_class = as.character(metric_class),
    atlas_class = metric_class
  ) |>
  select(pair_code, metric, metric_class, atlas_class, Q_metric)
atlas3_metric_q <- bind_rows(
  atlas3_metric_q |> filter(metric_class %in% ATLAS3_CLASS_ORDER[-1]),
  atlas3_metric_q |> mutate(atlas_class = "Overall")
) |>
  mutate(
    atlas_row = factor(
      unname(atlas3_row_labels[atlas_class]), levels = unname(atlas3_row_labels)
    ),
    dimension_pair = factor(
      unname(ATLAS3_PAIR_LABELS[PAIR_CODE_TO_LABEL[pair_code]]),
      levels = unname(ATLAS3_PAIR_LABELS)
    )
  )

atlas3_q_limit <- max(atlas3_metric_q$Q_metric, na.rm = TRUE)
if (!is.finite(atlas3_q_limit) || atlas3_q_limit <= 0) atlas3_q_limit <- 1
atlas3_q_limit <- atlas3_q_limit * 1.06

atlas3_cells <- tidyr::expand_grid(
  atlas_class = ATLAS3_CLASS_ORDER,
  pair_code = PAIR_CODES
) |>
  mutate(
    atlas_row = factor(
      unname(atlas3_row_labels[atlas_class]), levels = unname(atlas3_row_labels)
    ),
    dimension_pair = factor(
      unname(ATLAS3_PAIR_LABELS[PAIR_CODE_TO_LABEL[pair_code]]),
      levels = unname(ATLAS3_PAIR_LABELS)
    )
  )

atlas3_stats <- atlas3_metric_q |>
  group_by(atlas_class, atlas_row, dimension_pair) |>
  summarise(
    Q_median = safe_median(Q_metric),
    Q_q25 = safe_q(Q_metric, .25),
    Q_q75 = safe_q(Q_metric, .75),
    .groups = "drop"
  )

atlas3_raw <- atlas3_metric_q |>
  arrange(atlas_class, dimension_pair, metric) |>
  group_by(atlas_class, dimension_pair) |>
  mutate(raw_y = .18 + .14 * ((row_number() * .61803398875) %% 1)) |>
  ungroup()

p3a <- ggplot() +
  geom_blank(data = atlas3_cells, aes(x = 0, y = 0), inherit.aes = FALSE) +
  geom_point(
    data = atlas3_raw,
    aes(Q_metric, raw_y, colour = atlas_class), shape = 16,
    size = .76, alpha = .78
  ) +
  geom_segment(
    data = atlas3_stats,
    aes(x = Q_q25, xend = Q_q75, y = .405, yend = .405,
        colour = atlas_class), linewidth = 1.62, lineend = "round"
  ) +
  geom_point(
    data = atlas3_stats,
    aes(Q_median, .405, fill = atlas_class), shape = 21,
    size = 1.92, colour = "#30383C", stroke = .25
  ) +
  scale_colour_manual(values = ATLAS3_CLASS_COLORS, guide = "none") +
  scale_fill_manual(values = ATLAS3_CLASS_COLORS, guide = "none") +
  scale_x_continuous(
    limits = c(0, atlas3_q_limit), breaks = scales::breaks_extended(n = 4),
    expand = expansion(mult = c(0, .015))
  ) +
  scale_y_continuous(
    limits = c(-.02, .54), breaks = NULL, expand = expansion(mult = c(0, 0))
  ) +
  facet_grid(
    atlas_row ~ dimension_pair, scales = "fixed", drop = FALSE, switch = "y"
  ) +
  labs(
    title = "a  Class-level non-additivity distribution strips",
    subtitle = "Q_mp = median_t(Q_mpt); dots = metrics, thick line = IQR, point = class median",
    x = "median Q per metric", y = NULL
  ) +
  theme_rq2(base_size = 5.75) +
  theme(
    panel.grid = element_blank(),
    panel.spacing = grid::unit(.75, "mm"),
    strip.background = element_blank(),
    strip.text.x = element_text(size = 5.0, face = "bold", lineheight = .86),
    strip.text.y.left = element_text(
      size = 4.55, angle = 0, hjust = 1, lineheight = .86,
      margin = margin(r = 2)
    ),
    strip.placement = "outside",
    axis.text.x = element_text(size = 4.0),
    axis.ticks.x = element_line(colour = "#505457", linewidth = .25),
    axis.title.x = element_text(size = 4.35),
    axis.line.x = element_line(colour = "#505457", linewidth = .30),
    plot.title = element_text(size = 6.25, hjust = 0, margin = margin(b = 2)),
    plot.subtitle = element_text(size = 4.15, colour = "#666A6D", hjust = 0,
                                 margin = margin(t = -1, b = 2)),
    plot.margin = margin(1, 3, 1, 3)
  )

b3_transition_pairs <- rep(unname(PAIR_CODES), c(2L, 5L, 10L))
b3_transition_order <- tibble(
  pair_code = b3_transition_pairs,
  transition = c(
    "chest_LIGHT_to_MEDI", "wrist_LIGHT_to_MEDI",
    "120to60", "60to40", "40to30", "30to20", "20to10",
    "chest_120to60", "chest_60to40", "chest_40to30", "chest_30to20", "chest_20to10",
    "wrist_120to60", "wrist_60to40", "wrist_40to30", "wrist_30to20", "wrist_20to10"
  ),
  x_label = c(
    "chest \u00B7\nLIGHT \u2192 MEDI", "wrist \u00B7\nLIGHT \u2192 MEDI",
    "120 \u2192 60 s", "60 \u2192 40 s", "40 \u2192 30 s", "30 \u2192 20 s", "20 \u2192 10 s",
    rep(c("120 \u2192 60 s", "60 \u2192 40 s", "40 \u2192 30 s", "30 \u2192 20 s", "20 \u2192 10 s"), 2)
  ),
  placement = c(rep("all", 7L), rep("chest", 5L), rep("wrist", 5L))
)

gamma_b3_raw <- gamma_plot |>
  filter(is.finite(Q), metric_class %in% ATLAS3_CLASS_ORDER[-1]) |>
  mutate(pair_code = as.character(pair_code), transition = as.character(transition)) |>
  left_join(b3_transition_order, by = c("pair_code", "transition"))

b3_class_stats <- gamma_plot |>
  filter(is.finite(Q)) |>
  mutate(
    pair_code = as.character(pair_code), transition = as.character(transition),
    metric_class = as.character(metric_class)
  ) |>
  group_by(pair_code, transition, metric_class) |>
  summarise(
    Q_median = safe_median(Q), Q_q25 = safe_q(Q, .25), Q_q75 = safe_q(Q, .75),
    .groups = "drop"
  )
b3_overall_stats <- gamma_plot |>
  filter(is.finite(Q)) |>
  mutate(pair_code = as.character(pair_code), transition = as.character(transition)) |>
  group_by(pair_code, transition) |>
  summarise(
    Q_median = safe_median(Q), Q_q25 = safe_q(Q, .25), Q_q75 = safe_q(Q, .75),
    .groups = "drop"
  ) |>
  mutate(metric_class = "Overall")
b3_transition_stats <- bind_rows(b3_overall_stats, b3_class_stats) |>
  left_join(b3_transition_order, by = c("pair_code", "transition"))

b3_q_limit <- max(c(gamma_b3_raw$Q, b3_transition_stats$Q_q75), na.rm = TRUE)
if (!is.finite(b3_q_limit) || b3_q_limit <= 0) b3_q_limit <- 1
b3_q_limit <- b3_q_limit * 1.08
B3_CLASS_COLORS <- c("Overall" = "#343B3F", MS_METRIC_COLORS)
B3_CLASS_ORDER <- ATLAS3_CLASS_ORDER[-1]
B3_CLASS_OFFSETS <- setNames(seq(-.27, .27, length.out = length(B3_CLASS_ORDER)), B3_CLASS_ORDER)
metric_legend_main <- cowplot::get_legend(
  ggplot(
    tibble(
      metric_class = factor(B3_CLASS_ORDER, levels = B3_CLASS_ORDER), x = 1, y = 1
    ),
    aes(x, y, colour = metric_class)
  ) +
    geom_point(size = 1.05) +
    scale_colour_manual(values = MS_METRIC_COLORS, limits = B3_CLASS_ORDER) +
    guides(colour = guide_legend(
      title = NULL, nrow = 1, byrow = TRUE,
      override.aes = list(size = 1.05, alpha = 1)
    )) +
    theme_void(base_family = MS_FONT) +
    theme(
      legend.position = "bottom", legend.text = element_text(size = 3.75),
      legend.key.width = grid::unit(2.1, "mm"), legend.spacing.x = grid::unit(.45, "mm"),
      legend.margin = margin(0, 0, 0, 0)
    )
)

b3_quasirandom_offset <- function(n, width = .13) {
  if (n <= 1L) return(0)
  (((seq_len(n) * .61803398875) %% 1) - .5) * 2 * width
}

make_b3_row_plot <- function(pair_name, placement_name = "all", section_title,
                             show_y = TRUE, show_x_title = FALSE) {
  transitions <- b3_transition_order |>
    filter(pair_code == pair_name, placement == placement_name) |>
    mutate(row_y = rev(seq_len(n())))
  raw <- gamma_b3_raw |>
    filter(pair_code == pair_name, placement == placement_name) |>
    left_join(transitions |> select(transition, row_y), by = "transition") |>
    arrange(row_y, Q, metric) |>
    group_by(transition) |>
    mutate(raw_y = row_y + b3_quasirandom_offset(n(), .13)) |>
    ungroup()
  stats <- b3_transition_stats |>
    filter(pair_code == pair_name, placement == placement_name) |>
    left_join(transitions |> select(transition, row_y), by = "transition") |>
    mutate(
      summary_y = row_y + if_else(
        metric_class == "Overall", 0, unname(B3_CLASS_OFFSETS[metric_class])
      )
    )
  stats_class <- stats |> filter(metric_class %in% B3_CLASS_ORDER)
  stats_overall <- stats |> filter(metric_class == "Overall")

  ggplot() +
    geom_point(
      data = raw, aes(Q, raw_y), shape = 16, size = .34,
      alpha = .15, colour = "#707B80"
    ) +
    geom_segment(
      data = stats_overall,
      aes(x = Q_q25, xend = Q_q75, y = summary_y, yend = summary_y),
      colour = B3_CLASS_COLORS[["Overall"]], linewidth = 2.35, lineend = "round"
    ) +
    geom_point(
      data = stats_overall,
      aes(Q_median, summary_y), shape = 21, size = 2.35,
      fill = B3_CLASS_COLORS[["Overall"]], colour = "white", stroke = .26
    ) +
    geom_segment(
      data = stats_class,
      aes(x = Q_q25, xend = Q_q75, y = summary_y, yend = summary_y,
          colour = metric_class), linewidth = .56, alpha = .86, lineend = "round"
    ) +
    geom_point(
      data = stats_class,
      aes(Q_median, summary_y, colour = metric_class), shape = 16,
      size = 1.02, alpha = .96
    ) +
    scale_colour_manual(
      values = B3_CLASS_COLORS, limits = names(B3_CLASS_COLORS), guide = "none"
    ) +
    scale_x_continuous(
      limits = c(0, b3_q_limit), breaks = scales::breaks_extended(n = 4),
      expand = expansion(mult = c(0, .015))
    ) +
    scale_y_continuous(
      limits = c(.34, nrow(transitions) + .66),
      breaks = transitions$row_y, labels = transitions$x_label,
      expand = expansion(mult = c(0, 0))
    ) +
    labs(
      title = section_title,
      x = if (show_x_title) "Q = mean(|gamma|)" else NULL,
      y = if (show_y) "ordered transition" else NULL
    ) +
    theme_rq2(base_size = 5.35) +
    theme(
      panel.grid.major.y = element_blank(),
      panel.grid.major.x = element_line(colour = "#ECEFF0", linewidth = .18),
      axis.text.y = if (show_y) element_text(size = 3.95, lineheight = .82) else element_blank(),
      axis.text.x = element_text(size = 3.85),
      axis.title.y = if (show_y) element_text(size = 4.0, margin = margin(r = 2)) else element_blank(),
      axis.title.x = if (show_x_title) element_text(size = 4.2, margin = margin(t = 2)) else element_blank(),
      axis.ticks.y = if (show_y) element_line(colour = "#505457", linewidth = .20) else element_blank(),
      axis.ticks.x = element_line(colour = "#505457", linewidth = .24),
      axis.line.y = element_blank(), axis.line.x = element_line(colour = "#505457", linewidth = .28),
      plot.title = element_text(size = 5.0, face = "bold", hjust = 0,
                                margin = margin(b = 1)),
      plot.margin = margin(0, 3, 0, 3)
    )
}

p3b_po <- make_b3_row_plot(
  PAIR_CODES[[1]], section_title = "Placement \u00D7 optical", show_y = TRUE
)
p3b_ot <- make_b3_row_plot(
  PAIR_CODES[[2]], section_title = "Optical \u00D7 temporal", show_y = TRUE
)
p3b_pt_chest <- make_b3_row_plot(
  PAIR_CODES[[3]], placement_name = "chest", section_title = "Chest",
  show_y = TRUE, show_x_title = TRUE
)
p3b_pt_wrist <- make_b3_row_plot(
  PAIR_CODES[[3]], placement_name = "wrist", section_title = "Wrist",
  show_y = FALSE, show_x_title = FALSE
)
p3b_pt <- cowplot::plot_grid(
  p3b_pt_chest, p3b_pt_wrist, ncol = 2, rel_widths = c(1, 1),
  align = "hv", axis = "tblr", greedy = TRUE
)
p3b_header <- cowplot::ggdraw() +
  cowplot::draw_label(
    "b  Ordered-transition backbone with class overlays",
    x = 0, y = .70, hjust = 0, vjust = .5, fontfamily = MS_FONT,
    fontface = "bold", size = 6.2, colour = "#151515"
  ) +
  cowplot::draw_label(
    "overall = median + IQR; coloured marks = class medians + IQR; faint points = metric-level Q",
    x = 0, y = .20, hjust = 0, vjust = .5, fontfamily = MS_FONT,
    size = 4.0, colour = "#666A6D"
  )
p3b <- cowplot::plot_grid(
  p3b_header, p3b_po, p3b_ot, p3b_pt, ncol = 1,
  rel_heights = c(.28, .90, 1.32, 1.56), align = "v", axis = "l", greedy = TRUE
)

p3_bottom <- cowplot::plot_grid(
  p3b, p3c, ncol = 2, rel_widths = c(.75, .25),
  align = "hv", axis = "tblr", greedy = TRUE
)
p3 <- cowplot::plot_grid(
  metric_legend_main,
  cowplot::plot_grid(
    p3a, p3_bottom, ncol = 1, rel_heights = c(1.08, 1.20),
    align = "v", axis = "l", greedy = TRUE
  ),
  ncol = 1, rel_heights = c(.045, 1), align = "v", axis = "l", greedy = TRUE
)
ms_plot_save(p3, file.path(OUT_DIR, "Fig3_RQ2.png"), 7.20, 7.05)

# Complete gamma atlas retained as supplementary audit view.
gamma_atlas <- gamma_plot |>
  mutate(
    dimension = as.character(dimension_a),
    transition_display = factor(transition_display, levels = unique(transition_display))
  ) |>
  ms_add_metric_order(metric_order)
gamma_limit <- max(abs(gamma_atlas$R), na.rm = TRUE)
if (!is.finite(gamma_limit) || gamma_limit <= 0) gamma_limit <- 1
p3_atlas <- ggplot(gamma_atlas, aes(transition_display, R, color = metric_class)) +
  geom_hline(yintercept = 0, linewidth = .28, color = "#8A8A8A") +
  geom_segment(aes(xend = transition_display, y = 0, yend = R), alpha = .34, linewidth = .40) +
  geom_point(aes(size = Q), alpha = .90) +
  facet_grid(metric_class ~ dimension_pair, scales = "free_x", space = "free", switch = "y") +
  scale_color_ms_metric() +
  scale_size_continuous(range = c(.35, 2.8), name = "Q = mean |gamma|") +
  scale_y_continuous(limits = c(-gamma_limit * 1.05, gamma_limit * 1.05),
                     breaks = scales::breaks_extended(n = 5)) +
  labs(title = "Complete cross-dimensional interaction atlas",
       x = "oriented local transition", y = "R = mean γ") +
  ms_atlas_theme(base_size = 6.2, x_angle = 48)
ms_plot_save(p3_atlas, file.path(OUT_DIR, "FigS_RQ2_gamma_atlas.pdf"), 14.5, 10.5)
ms_plot_save(p3_atlas, file.path(OUT_DIR, "FigS_RQ2_gamma_atlas.png"), 14.5, 10.5)
readr::write_csv(gamma_atlas |>
  mutate(metric = as.character(metric), metric_class = as.character(metric_class),
         transition_display = as.character(transition_display), dimension_pair = as.character(dimension_pair)),
  file.path("results", "rq2", "fig3_gamma_atlas.csv"), na = "")

# =============================================================================
# Supplementary model validation diagnostics
# =============================================================================
if (nrow(performance)) {
  perf_plot <- performance |>
    filter(is.finite(rmse) | is.finite(mae) | is.finite(r2)) |>
    group_by(dimension, model_family, outcome, validation_scheme) |>
    summarise(rmse = median(rmse, na.rm = TRUE), mae = median(mae, na.rm = TRUE),
              r2 = median(r2, na.rm = TRUE), .groups = "drop") |>
    pivot_longer(c(rmse, mae, r2), names_to = "measure", values_to = "value") |>
    mutate(dimension = factor(dimension, levels = DIMENSIONS))
  p_perf <- ggplot(perf_plot, aes(interaction(model_family, validation_scheme, sep = "\n"), outcome, fill = value)) +
    geom_tile(color = "white", linewidth = .12) +
    facet_grid(measure ~ dimension, scales = "free", space = "free", switch = "y") +
    scale_fill_ms_sequential(name = "median value") +
    labs(title = "RQ2 model validation diagnostics", x = "model family × validation scheme", y = NULL) +
    ms_atlas_theme(base_size = 6.1, x_angle = 48)
} else {
  p_perf <- ggplot() + theme_void() +
    annotate("text", x = 0, y = 0,
             label = "No model-performance rows; RQ2_RUN_MODELS=0 or no eligible tasks.")
}
ms_plot_save(p_perf, file.path(OUT_DIR, "FigS_RQ2_model_performance.pdf"), 13, 8.5)
ms_plot_save(p_perf, file.path(OUT_DIR, "FigS_RQ2_model_performance.png"), 13, 8.5)

ms_plot_write_manifest(
  file.path(OUT_DIR, "figure_artifact_manifest.csv"),
  tibble(
    figure = c(
      "Fig2_RQ2", "Fig3_RQ2", "FigS_RQ2_conditional_atlas", "FigS_RQ2_state_spread",
      "FigS_RQ2_context_increment", "FigS_RQ2_gamma_atlas", "FigS_RQ2_model_performance"
    ),
    input_artifact = c(
      "rq2_conditional_geometry+rq2_model_coefficients",
      "rq2_gamma_summary",
      "rq2_conditional_geometry",
      "rq2_conditional_geometry",
      "rq2_model_performance",
      "rq2_gamma_summary",
      "rq2_model_performance"
    ),
    core_artifact_version = CORE_VERSION,
    rq1_analysis_version = RQ1_VERSION,
    rq2_analysis_version = RQ2_VERSION,
    rq3_analysis_version = NA_character_
  ))
message("RQ2 v5 figures complete: Fig. 2 combines state-conditioned geometry, layered contextual effects and held-out predictability; Fig. 3 reports interaction sign, magnitude and coherence.")
