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
# Fig. 3 — cross-dimensional dependence
# =============================================================================

format_gamma_transition <- function(x) {
  x |>
    str_replace_all("_LIGHT_to_MEDI", " · LIGHT → MEDI") |>
    str_replace_all("([0-9]+)to([0-9]+)", "\\1 → \\2") |>
    str_replace_all("_", " · ")
}

gamma_plot <- gamma_summary |>
  mutate(
    dimension_pair = case_when(
      dimension_a == "placement" & dimension_b == "optical" ~ "Placement × optical",
      dimension_a == "placement" & dimension_b == "temporal" ~ "Placement × temporal",
      dimension_a == "optical" & dimension_b == "temporal" ~ "Optical × temporal",
      TRUE ~ paste(dimension_a, "×", dimension_b)
    ),
    metric = as.character(metric),
    metric_class = factor(metric_class, levels = METRIC_CLASSES),
    transition_display = format_gamma_transition(transition),
    Q = abs(Q)
  )

PAIR_LEVELS <- c("Placement × optical", "Optical × temporal", "Placement × temporal")
gamma_plot <- gamma_plot |>
  mutate(dimension_pair = factor(dimension_pair, levels = PAIR_LEVELS))

gamma_metric <- gamma_plot |>
  filter(is.finite(R), is.finite(Q)) |>
  group_by(dimension_pair, metric, metric_class) |>
  summarise(R_metric = median(R, na.rm = TRUE), Q_metric = median(Q, na.rm = TRUE), .groups = "drop")

gamma_r_summary <- gamma_metric |>
  group_by(dimension_pair, metric_class) |>
  summarise(
    n_metrics = n_distinct(metric),
    R_median = median(R_metric, na.rm = TRUE),
    R_q25 = quantile(R_metric, .25, na.rm = TRUE, names = FALSE),
    R_q75 = quantile(R_metric, .75, na.rm = TRUE, names = FALSE),
    .groups = "drop"
  )

gamma_q_summary <- gamma_metric |>
  group_by(dimension_pair, metric_class) |>
  summarise(
    n_metrics = n_distinct(metric),
    Q_median = median(Q_metric, na.rm = TRUE),
    Q_q25 = quantile(Q_metric, .25, na.rm = TRUE, names = FALSE),
    Q_q75 = quantile(Q_metric, .75, na.rm = TRUE, names = FALSE),
    .groups = "drop"
  )

r_limit <- robust_symmetric_display_limit(
  gamma_metric$R_metric,
  c(gamma_r_summary$R_median, gamma_r_summary$R_q25, gamma_r_summary$R_q75),
  prob = .95, pad = 1.08, fallback = .05
)
q_limit <- robust_upper_display_limit(
  gamma_metric$Q_metric,
  c(gamma_q_summary$Q_median, gamma_q_summary$Q_q25, gamma_q_summary$Q_q75),
  prob = .95, pad = 1.08, fallback = .10
)
gamma_metric_r_display <- gamma_metric |> filter(abs(R_metric) <= r_limit)
gamma_metric_q_display <- gamma_metric |> filter(Q_metric <= q_limit)

p3a <- ggplot(gamma_metric_r_display, aes(R_metric, metric_class, color = metric_class)) +
  geom_vline(xintercept = 0, linewidth = .30, color = "#969B9E") +
  geom_point(position = position_jitter(width = 0, height = .09, seed = 71), size = .70, alpha = .28) +
  geom_segment(
    data = gamma_r_summary,
    aes(x = R_q25, xend = R_q75, y = metric_class, yend = metric_class, color = metric_class),
    inherit.aes = FALSE, linewidth = 1.05, alpha = .46, lineend = "round"
  ) +
  geom_point(data = gamma_r_summary, aes(R_median, metric_class, color = metric_class),
             inherit.aes = FALSE, shape = 18, size = 1.9) +
  facet_grid(. ~ dimension_pair) +
  scale_color_ms_metric(guide = "none") +
  scale_x_continuous(limits = c(-r_limit * 1.04, r_limit * 1.04), breaks = scales::breaks_extended(n = 5)) +
  labs(title = "a  Signed cross-dimensional interaction",
       subtitle = "raw metric points show central 95%; summaries use all metrics",
       x = "R = median signed γ across local transitions", y = NULL) +
  theme_rq2(base_size = 6.5) +
  theme(panel.grid.major.y = element_blank(), axis.line.y = element_blank(), axis.ticks.y = element_blank(),
        axis.text.y = element_text(size = 5.4), strip.text = element_text(size = 6.0),
        plot.subtitle = element_text(size = 4.45, colour = "#666A6D", margin = margin(t = -1, b = 1.5)),
        panel.spacing.x = grid::unit(2.3, "mm"))

p3b <- ggplot(gamma_metric_q_display, aes(Q_metric, metric_class, color = metric_class)) +
  geom_point(position = position_jitter(width = 0, height = .09, seed = 73), size = .70, alpha = .28) +
  geom_segment(
    data = gamma_q_summary,
    aes(x = Q_q25, xend = Q_q75, y = metric_class, yend = metric_class, color = metric_class),
    inherit.aes = FALSE, linewidth = 1.05, alpha = .46, lineend = "round"
  ) +
  geom_point(data = gamma_q_summary, aes(Q_median, metric_class, color = metric_class),
             inherit.aes = FALSE, shape = 18, size = 1.9) +
  facet_grid(. ~ dimension_pair) +
  scale_color_ms_metric(guide = "none") +
  scale_x_continuous(limits = c(0, q_limit * 1.04), breaks = scales::breaks_extended(n = 5)) +
  labs(title = "b  Magnitude of cross-dimensional interaction",
       subtitle = "raw metric points show central 95%; summaries use all metrics",
       x = "Q = median |γ| across local transitions", y = NULL) +
  theme_rq2(base_size = 6.5) +
  theme(panel.grid.major.y = element_blank(), axis.line.y = element_blank(), axis.ticks.y = element_blank(),
        axis.text.y = element_text(size = 5.4), strip.text = element_text(size = 6.0),
        plot.subtitle = element_text(size = 4.45, colour = "#666A6D", margin = margin(t = -1, b = 1.5)),
        panel.spacing.x = grid::unit(2.3, "mm"))

gamma_transition <- gamma_plot |>
  filter(is.finite(R), is.finite(Q)) |>
  group_by(dimension_pair, transition_display) |>
  summarise(
    n_metrics = n_distinct(metric),
    Q_median = median(Q, na.rm = TRUE),
    Q_q25 = quantile(Q, .25, na.rm = TRUE, names = FALSE),
    Q_q75 = quantile(Q, .75, na.rm = TRUE, names = FALSE),
    R_median = median(R, na.rm = TRUE),
    coherence_median = median(if_else(Q > 1e-12, R / Q, NA_real_), na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    coherence_median = if_else(
      is.finite(coherence_median),
      pmax(-1, pmin(1, coherence_median)), NA_real_
    )
  ) |>
  group_by(dimension_pair) |>
  slice_max(Q_median, n = 4, with_ties = FALSE) |>
  ungroup() |>
  mutate(
    transition_key = paste(as.character(dimension_pair), transition_display, sep = "|||"),
    transition_key = forcats::fct_reorder(transition_key, Q_median)
  )

p3c <- ggplot(gamma_transition, aes(Q_median, transition_key)) +
  geom_segment(aes(x = Q_q25, xend = Q_q75, yend = transition_key),
               color = "#A8ADB0", linewidth = .90, alpha = .62, lineend = "round") +
  geom_point(aes(fill = coherence_median), shape = 21, size = 2.15, color = "#3E4245", stroke = .22) +
  facet_grid(. ~ dimension_pair, scales = "free_y", space = "free_x") +
  scale_y_discrete(labels = function(x) stringr::str_wrap(sub("^.*\\|\\|\\|", "", x), width = 24)) +
  scale_x_continuous(breaks = scales::breaks_extended(n = 4)) +
  scale_fill_ms_diverging(1, name = "median R / Q") +
  labs(
    title = "c  Strong interactions differ in directional coherence",
    subtitle = "x = interaction magnitude Q; fill = signed coherence R/Q (0 = cancellation)",
    x = "median Q across metrics", y = NULL
  ) +
  theme_rq2(base_size = 6.3, legend_position = "bottom") +
  theme(panel.grid.major.y = element_blank(), axis.line.y = element_blank(), axis.ticks.y = element_blank(),
        axis.text.y = element_text(size = 4.9), strip.text = element_text(size = 5.8),
        plot.subtitle = element_text(size = 4.35, colour = "#666A6D", margin = margin(t = -1, b = 1.5)),
        legend.title = element_text(size = 5.3), legend.text = element_text(size = 5.0),
        panel.spacing.x = grid::unit(2.4, "mm"))

p3body <- cowplot::plot_grid(p3a, p3b, p3c, ncol = 1, rel_heights = c(.82, .82, 1.0),
                             align = "v", axis = "l", greedy = TRUE)
p3 <- cowplot::plot_grid(metric_legend, p3body, ncol = 1, rel_heights = c(.045, 1),
                         align = "v", axis = "l", greedy = TRUE)
ms_plot_save(p3, file.path(OUT_DIR, "Fig3_RQ2.pdf"), 9.0, 6.3)
ms_plot_save(p3, file.path(OUT_DIR, "Fig3_RQ2.png"), 9.0, 6.3)

readr::write_csv(gamma_r_summary |>
  mutate(metric_class = as.character(metric_class), dimension_pair = as.character(dimension_pair)),
  file.path("results", "rq2", "fig3_signed_interaction_summary.csv"), na = "")
readr::write_csv(gamma_q_summary |>
  mutate(metric_class = as.character(metric_class), dimension_pair = as.character(dimension_pair)),
  file.path("results", "rq2", "fig3_interaction_magnitude_summary.csv"), na = "")
readr::write_csv(gamma_transition |>
  mutate(dimension_pair = as.character(dimension_pair), transition_key = as.character(transition_key)),
  file.path("results", "rq2", "fig3_top_interaction_transitions.csv"), na = "")

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
