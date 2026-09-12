# Canonical RQ2 plotting source. All accepted display refinements are consolidated here.
options(encoding = "UTF-8")
if (.Platform$OS.type == "windows") invisible(suppressWarnings(Sys.setlocale("LC_CTYPE", "English_United States.utf8")))
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
OUT_DIR <- file.path("results", "rq2")
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
DIM_SHORT <- c(placement = "P", optical = "O", temporal = "T", duration = "D")
DIMENSION_SHAPES <- c(
  "Placement" = 21,
  "Optical representation" = 22,
  "Temporal resolution" = 24,
  "Monitoring duration" = 23
)
DIMENSION_OFFSETS <- c(placement = -.16, optical = -.055, temporal = .055, duration = .16)
NUMERIC_TOL <- 1e-12

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
    pair_label = paste(config_a_label, "\u2192", config_b_label),
    state_bin_label = factor(state_bin_label, levels = c("Low", "Middle", "High")),
    direction_ratio = ms_direction_ratio(B_conditional, A_conditional)
  ) |>
  mutate(dimension = as.character(dimension)) |>
  ms_add_metric_order(metric_order)

theme_rq2 <- function(base_size = 6.7, legend_position = "none") {
  theme_ms_axes(base_size = base_size, legend_position = legend_position)
}

robust_symmetric_display_limit <- function(x, summary_values = numeric(), prob = .95,
                                           pad = 1.08, fallback = 1) {
  x <- abs(as.numeric(x)); x <- x[is.finite(x)]
  s <- abs(as.numeric(summary_values)); s <- s[is.finite(s)]
  core <- if (length(x)) as.numeric(stats::quantile(x, prob, na.rm = TRUE,
                                                    names = FALSE, type = 8)) else NA_real_
  summary_extent <- if (length(s)) max(s, na.rm = TRUE) else NA_real_
  lim <- suppressWarnings(max(c(core, summary_extent), na.rm = TRUE))
  if (!is.finite(lim) || lim <= 0) lim <- fallback
  lim * pad
}

robust_upper_display_limit <- function(x, summary_values = numeric(), prob = .95,
                                       pad = 1.08, fallback = 1) {
  x <- as.numeric(x); x <- x[is.finite(x)]
  s <- as.numeric(summary_values); s <- s[is.finite(s)]
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
  x <- as.numeric(x); x <- x[is.finite(x)]
  s <- as.numeric(summary_values); s <- s[is.finite(s)]
  if (!length(x) && !length(s)) return(c(lower, upper))
  q <- if (length(x)) {
    as.numeric(stats::quantile(x, probs, na.rm = TRUE, names = FALSE, type = 8))
  } else c(min(s), max(s))
  lo <- min(c(q[[1]], s), na.rm = TRUE)
  hi <- max(c(q[[2]], s), na.rm = TRUE)
  lo <- max(lower, lo); hi <- min(upper, hi)
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

robust_symmetric_display_window <- function(x, summary_values = numeric(), probs = c(.08, .92),
                                            min_half = .02, pad = 1.06, hard_cap = NULL) {
  x <- as.numeric(x); x <- x[is.finite(x)]
  s <- as.numeric(summary_values); s <- s[is.finite(s)]
  vals <- c(x, s)
  if (!length(vals)) return(c(-min_half, min_half))
  q <- as.numeric(stats::quantile(vals, probs, na.rm = TRUE, names = FALSE, type = 8))
  half <- max(abs(q), na.rm = TRUE)
  if (length(s)) {
    half <- max(half,
                min(abs(stats::median(s, na.rm = TRUE)) * 3,
                    max(abs(s), na.rm = TRUE) / 2), na.rm = TRUE)
  }
  if (!is.finite(half) || half <= 0) half <- min_half
  half <- max(half, min_half)
  if (length(hard_cap) && is.finite(hard_cap)) half <- min(half, hard_cap)
  half <- half * pad
  c(-half, half)
}

quantile_or_na <- function(x, probability) {
  x <- as.numeric(x); x <- x[is.finite(x)]
  if (!length(x)) return(NA_real_)
  as.numeric(stats::quantile(x, probability, na.rm = TRUE, names = FALSE, type = 8))
}

squish_to_limits <- function(x, limits) {
  pmax(limits[[1]], pmin(limits[[2]], x))
}

# =============================================================================
# Fig. 2 \u2014 context dependence of distortion
# =============================================================================

# -----------------------------------------------------------------------------
# b. Context-induced displacement of distortion geometry
# -----------------------------------------------------------------------------
# Low exposure state is the display origin. The plotted object is therefore the
# state-induced displacement of the conditional geometry, not the absolute
# geometry already established in Fig. 1. Pair-specific conditional estimates
# are pooled within metric before the Low/Middle/High displacement is computed.
conditional_trajectory_state <- conditional |>
  mutate(
    metric = as.character(metric),
    metric_class = factor(as.character(metric_class), levels = METRIC_CLASSES),
    state_num = as.integer(state_bin_label)
  ) |>
  filter(
    is.finite(A_conditional), is.finite(direction_ratio), is.finite(state_num),
    state_bin_label %in% c("Low", "Middle", "High")
  ) |>
  group_by(
    dimension, comparison_pair_id, pair_label, metric, metric_class,
    state_bin_label, state_num
  ) |>
  summarise(
    A_state = median(A_conditional, na.rm = TRUE),
    direction_state = median(direction_ratio, na.rm = TRUE),
    .groups = "drop"
  )

conditional_metric_state <- conditional_trajectory_state |>
  group_by(dimension, metric, metric_class, state_bin_label, state_num) |>
  summarise(
    A_state = median(A_state, na.rm = TRUE),
    direction_state = median(direction_state, na.rm = TRUE),
    .groups = "drop"
  )

# Keep the absolute state summaries as an audit table and supplementary source.
conditional_profile_summary <- conditional_metric_state |>
  group_by(dimension, metric_class, state_bin_label, state_num) |>
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

conditional_metric_wide <- conditional_metric_state |>
  select(dimension, metric, metric_class, state_bin_label, A_state, direction_state) |>
  pivot_wider(
    names_from = state_bin_label,
    values_from = c(A_state, direction_state),
    names_sep = "_"
  )

conditional_shift_metric <- bind_rows(
  conditional_metric_wide |>
    transmute(
      dimension, metric, metric_class,
      state = "Middle", state_num = 2L,
      delta_A = A_state_Middle - A_state_Low,
      delta_direction = direction_state_Middle - direction_state_Low
    ),
  conditional_metric_wide |>
    transmute(
      dimension, metric, metric_class,
      state = "High", state_num = 3L,
      delta_A = A_state_High - A_state_Low,
      delta_direction = direction_state_High - direction_state_Low
    )
) |>
  filter(is.finite(delta_A), is.finite(delta_direction)) |>
  mutate(
    state = factor(state, levels = c("Middle", "High")),
    metric_class = factor(metric_class, levels = METRIC_CLASSES)
  )

conditional_shift_inventory <- conditional_metric_wide |>
  distinct(dimension, metric, metric_class) |>
  mutate(metric_class = factor(metric_class, levels = METRIC_CLASSES))

conditional_shift_path_metric <- bind_rows(
  conditional_shift_inventory |>
    mutate(state = factor("Low", levels = c("Low", "Middle", "High")),
           state_num = 1L, delta_A = 0, delta_direction = 0),
  conditional_shift_metric |>
    mutate(state = factor(as.character(state), levels = c("Low", "Middle", "High")))
) |>
  arrange(dimension, metric, state_num)

shift_class_counts <- conditional_shift_inventory |>
  group_by(metric_class) |>
  summarise(n_metrics = n_distinct(metric), .groups = "drop")
shift_display_classes <- shift_class_counts |>
  filter(n_metrics >= 3L) |>
  pull(metric_class) |>
  as.character()

conditional_shift_class <- conditional_shift_metric |>
  filter(as.character(metric_class) %in% shift_display_classes) |>
  group_by(dimension, metric_class, state, state_num) |>
  summarise(
    n_metrics = n_distinct(metric),
    delta_A = median(delta_A, na.rm = TRUE),
    delta_direction = median(delta_direction, na.rm = TRUE),
    .groups = "drop"
  )
conditional_shift_class_path <- bind_rows(
  conditional_shift_inventory |>
    filter(as.character(metric_class) %in% shift_display_classes) |>
    distinct(dimension, metric_class) |>
    mutate(state = factor("Low", levels = c("Low", "Middle", "High")),
           state_num = 1L, delta_A = 0, delta_direction = 0),
  conditional_shift_class |>
    mutate(state = factor(as.character(state), levels = c("Low", "Middle", "High")))
) |>
  arrange(dimension, metric_class, state_num)

conditional_shift_overall <- conditional_shift_metric |>
  group_by(dimension, state, state_num) |>
  summarise(
    n_metrics = n_distinct(metric),
    delta_A = median(delta_A, na.rm = TRUE),
    delta_direction = median(delta_direction, na.rm = TRUE),
    .groups = "drop"
  )
conditional_shift_overall_path <- bind_rows(
  conditional_shift_inventory |>
    distinct(dimension) |>
    mutate(state = factor("Low", levels = c("Low", "Middle", "High")),
           state_num = 1L, delta_A = 0, delta_direction = 0,
           n_metrics = NA_integer_),
  conditional_shift_overall |>
    mutate(state = factor(as.character(state), levels = c("Low", "Middle", "High")))
) |>
  arrange(dimension, state_num)

shift_x_limit <- robust_symmetric_display_limit(
  conditional_shift_metric$delta_A,
  c(conditional_shift_class$delta_A, conditional_shift_overall$delta_A),
  prob = .95, pad = 1.08, fallback = .08
)
shift_y_limit <- robust_symmetric_display_limit(
  conditional_shift_metric$delta_direction,
  c(conditional_shift_class$delta_direction, conditional_shift_overall$delta_direction),
  prob = .95, pad = 1.08, fallback = .12
)
shift_y_limit <- min(2.05, shift_y_limit)

shift_dim_levels <- unname(DIM_TITLES[DIMENSIONS])
conditional_shift_path_metric <- conditional_shift_path_metric |>
  mutate(dimension_label = factor(unname(DIM_TITLES[dimension]), levels = shift_dim_levels))
conditional_shift_class_path <- conditional_shift_class_path |>
  mutate(dimension_label = factor(unname(DIM_TITLES[dimension]), levels = shift_dim_levels))
conditional_shift_overall_path <- conditional_shift_overall_path |>
  mutate(
    dimension_label = factor(unname(DIM_TITLES[dimension]), levels = shift_dim_levels),
    state_label = case_when(
      state_num == 2L ~ "M",
      state_num == 3L ~ "H",
      TRUE ~ ""
    )
  )

p2b <- ggplot() +
  geom_hline(yintercept = 0, linewidth = .24, colour = "#C5C9CB") +
  geom_vline(xintercept = 0, linewidth = .24, colour = "#C5C9CB") +
  geom_path(
    data = conditional_shift_path_metric,
    aes(delta_A, delta_direction, group = interaction(metric, metric_class), colour = metric_class),
    linewidth = .09, alpha = .035
  ) +
  geom_point(
    data = conditional_shift_path_metric |> filter(state_num > 1L),
    aes(delta_A, delta_direction, colour = metric_class),
    size = .20, alpha = .075
  ) +
  geom_path(
    data = conditional_shift_class_path,
    aes(delta_A, delta_direction, group = metric_class, colour = metric_class),
    linewidth = .52, alpha = .72
  ) +
  geom_point(
    data = conditional_shift_class_path |> filter(state_num > 1L),
    aes(delta_A, delta_direction, colour = metric_class, shape = state),
    size = .88, alpha = .95
  ) +
  geom_path(
    data = conditional_shift_overall_path,
    aes(delta_A, delta_direction, group = dimension_label),
    linewidth = 1.02, colour = "#343B3F"
  ) +
  geom_point(
    data = conditional_shift_overall_path,
    aes(delta_A, delta_direction, shape = state),
    size = 1.38, colour = "#343B3F", fill = "white", stroke = .30
  ) +
  geom_text(
    data = conditional_shift_overall_path |> filter(state_num > 1L),
    aes(delta_A, delta_direction, label = state_label),
    nudge_x = .035 * shift_x_limit, nudge_y = .045 * shift_y_limit,
    size = 1.40, colour = "#343B3F", fontface = "bold"
  ) +
  facet_wrap(~dimension_label, ncol = 2) +
  scale_colour_ms_metric(guide = "none") +
  scale_shape_manual(
    values = c("Low" = 1, "Middle" = 16, "High" = 18),
    breaks = c("Middle", "High"), labels = c("Middle", "High"),
    name = "state"
  ) +
  scale_x_continuous(
    limits = c(-shift_x_limit, shift_x_limit),
    breaks = scales::breaks_extended(n = 3)
  ) +
  scale_y_continuous(
    limits = c(-shift_y_limit, shift_y_limit),
    breaks = scales::breaks_extended(n = 3)
  ) +
  labs(
    title = "b  Context-induced geometry shifts",
    subtitle = "Low state is the origin; M/H = displacement at Middle/High exposure state",
    x = expression(Delta * " distortion magnitude, A"),
    y = expression(Delta * " directional coherence, B/A")
  ) +
  theme_rq2(base_size = 5.35, legend_position = "bottom") +
  theme(
    panel.grid = element_blank(),
    strip.text = element_text(size = 4.55, face = "bold"),
    axis.text = element_text(size = 3.65),
    axis.title = element_text(size = 4.05),
    plot.title = element_text(size = 6.25, hjust = 0, margin = margin(b = 2)),
    plot.subtitle = element_text(size = 3.80, colour = "#666A6D", hjust = 0,
                                 margin = margin(t = -1, b = 2)),
    panel.spacing = grid::unit(.75, "mm"),
    legend.position = "none",
    plot.margin = margin(1.5, 2, 1.5, 2)
  )

# Transition-resolved state spread remains supplementary.
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

# -----------------------------------------------------------------------------
# a. Hierarchical contextual predictor atlas
# -----------------------------------------------------------------------------
# Predictor strength is the primary backbone. Signed and absolute coefficient
# panels retain one overall distribution per predictor; dimension-specific
# medians/IQRs are deliberately demoted to a secondary fingerprint within that
# shared coordinate system instead of receiving four equal-status panels.
PREDICTOR_COLORS <- c(
  "External opportunity" = MS_PRIMARY,
  "Micro-environment" = "#5F8F84",
  "Behaviour" = MS_NEUTRAL,
  "Exposure state" = MS_SECONDARY
)
FAMILY_LEVELS <- c("External opportunity", "Micro-environment", "Behaviour", "Exposure state")
PREDICTOR_ROW_STEP <- .76
FAMILY_GAP <- .55

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
  left_join(predictor_order |> select(term, y), by = "term")

coef_summary <- coef_metric |>
  group_by(dimension, term, label, predictor_family, y, outcome_label) |>
  summarise(
    n_display_units = n(), n_metrics = n_distinct(metric),
    estimate_q05 = quantile_or_na(estimate, .05), estimate_q25 = quantile_or_na(estimate, .25),
    estimate_q50 = quantile_or_na(estimate, .50), estimate_q75 = quantile_or_na(estimate, .75),
    estimate_q95 = quantile_or_na(estimate, .95), .groups = "drop"
  )

coef_summary_all <- coef_metric |>
  group_by(term, label, predictor_family, y, outcome_label) |>
  summarise(
    n_display_units = n(), n_metrics = n_distinct(dimension, metric),
    estimate_q05 = quantile_or_na(estimate, .05), estimate_q25 = quantile_or_na(estimate, .25),
    estimate_q50 = quantile_or_na(estimate, .50), estimate_q75 = quantile_or_na(estimate, .75),
    estimate_q95 = quantile_or_na(estimate, .95), .groups = "drop"
  )

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
    coef_summary$estimate_q75, coef_summary$estimate_q95,
    coef_summary_all$estimate_q05, coef_summary_all$estimate_q95),
  probs = c(.05, .95), min_half = .025, pad = 1.10
)
coef_metric_plot <- coef_metric |>
  mutate(estimate_plot = squish_to_limits(estimate, coef_window_global))
coef_summary_all_plot <- coef_summary_all |>
  mutate(
    estimate_q05_plot = squish_to_limits(estimate_q05, coef_window_global),
    estimate_q25_plot = squish_to_limits(estimate_q25, coef_window_global),
    estimate_q50_plot = squish_to_limits(estimate_q50, coef_window_global),
    estimate_q75_plot = squish_to_limits(estimate_q75, coef_window_global),
    estimate_q95_plot = squish_to_limits(estimate_q95, coef_window_global)
  )
coef_summary_dim_plot <- coef_summary |>
  mutate(
    estimate_q25_plot = squish_to_limits(estimate_q25, coef_window_global),
    estimate_q50_plot = squish_to_limits(estimate_q50, coef_window_global),
    estimate_q75_plot = squish_to_limits(estimate_q75, coef_window_global),
    dimension_label = factor(unname(DIM_TITLES[dimension]), levels = unname(DIM_TITLES[DIMENSIONS])),
    y_pos = y + unname(DIMENSION_OFFSETS[dimension])
  )

status_grid <- tidyr::crossing(
  term = PREDICTOR_CATALOG$term,
  dimension = DIMENSIONS,
  outcome_label = factor(c("Signed", "Absolute"), levels = c("Signed", "Absolute"))
) |>
  left_join(predictor_order |> select(term, y), by = "term") |>
  left_join(coef_summary |> select(dimension, term, outcome_label, estimate_q50),
            by = c("dimension", "term", "outcome_label")) |>
  mutate(
    structural_na = term == "duration_day_variability" & dimension != "duration",
    status_label = case_when(structural_na ~ "\u2014", !is.finite(estimate_q50) ~ "\u00d7", TRUE ~ NA_character_),
    dimension_label = factor(unname(DIM_TITLES[dimension]), levels = unname(DIM_TITLES[DIMENSIONS])),
    y_pos = y + unname(DIMENSION_OFFSETS[dimension])
  )

overall_limit <- robust_upper_display_limit(
  predictor_order$overall_abs, predictor_order$overall_abs,
  prob = .90, pad = 1.08, fallback = .02
)
predictor_order <- predictor_order |>
  mutate(
    overall_abs_plot = pmin(overall_abs, overall_limit),
    overall_clipped = is.finite(overall_abs) & overall_abs > overall_limit
  )

add_row_guides <- function(p) {
  if (length(family_boundaries)) {
    p + geom_hline(yintercept = family_boundaries, linewidth = .24, colour = "#DDE0E2")
  } else p
}

p_labels <- ggplot(predictor_order, aes(y = y)) +
  geom_text(aes(x = .955, label = label), hjust = 1, size = 1.38, colour = "#34383B") +
  geom_point(aes(x = .995, colour = predictor_family), size = .70, alpha = .95) +
  scale_colour_manual(values = PREDICTOR_COLORS, drop = FALSE, guide = "none") +
  scale_x_continuous(limits = c(0, 1), expand = expansion(mult = c(0, 0))) +
  scale_y_continuous(limits = predictor_y_limits, expand = expansion(mult = c(0, 0))) +
  labs(title = "Predictor", x = NULL, y = NULL) +
  theme_void(base_family = MS_FONT) +
  theme(
    plot.title = element_text(size = 5.15, hjust = 1, face = "bold", margin = margin(b = 1.2)),
    plot.margin = margin(1.5, .5, 1.5, .6)
  )
p_labels <- add_row_guides(p_labels)

p_strength <- ggplot(predictor_order, aes(fill = predictor_family)) +
  geom_rect(
    aes(xmin = 0, xmax = overall_abs_plot, ymin = y - .18, ymax = y + .18),
    alpha = .48, na.rm = TRUE
  ) +
  geom_point(
    data = predictor_order |> filter(overall_clipped),
    aes(x = overall_limit, y = y), inherit.aes = FALSE,
    shape = 17, size = .60, colour = "#4E5559"
  ) +
  geom_point(
    data = predictor_order |> filter(!is.finite(overall_abs)),
    aes(x = 0, y = y), inherit.aes = FALSE,
    shape = 4, size = .72, stroke = .32, colour = "#AEB2B5"
  ) +
  scale_fill_manual(values = PREDICTOR_COLORS, drop = FALSE, guide = "none") +
  scale_x_continuous(
    limits = c(0, overall_limit), breaks = scales::breaks_extended(n = 3),
    expand = expansion(mult = c(0, .02))
  ) +
  scale_y_continuous(limits = predictor_y_limits, expand = expansion(mult = c(0, 0))) +
  labs(title = "Strength", x = "median |\u03b2|", y = NULL) +
  theme_rq2(base_size = 5.45) +
  theme(
    panel.grid = element_blank(), axis.line.y = element_blank(), axis.ticks.y = element_blank(),
    axis.text.y = element_blank(), axis.text.x = element_text(size = 3.75),
    axis.title.x = element_text(size = 4.25),
    plot.title = element_text(size = 5.25, hjust = .5, face = "bold", margin = margin(b = 1.2)),
    plot.margin = margin(1.5, 1.0, 1.5, 1.0)
  )
p_strength <- add_row_guides(p_strength)

make_effect_panel <- function(outcome_name, panel_title) {
  raw <- coef_metric_plot |> filter(outcome_label == outcome_name)
  overall <- coef_summary_all_plot |> filter(outcome_label == outcome_name)
  dim <- coef_summary_dim_plot |> filter(outcome_label == outcome_name)
  miss <- status_grid |> filter(outcome_label == outcome_name, !is.na(status_label))

  p <- ggplot() +
    geom_vline(xintercept = 0, linewidth = .27, colour = "#A1A6A9") +
    geom_point(
      data = raw,
      aes(estimate_plot, y, colour = predictor_family),
      position = position_jitter(width = 0, height = .055, seed = 72),
      shape = 16, size = .15, alpha = .025
    ) +
    geom_segment(
      data = overall,
      aes(x = estimate_q05_plot, xend = estimate_q95_plot, y = y, yend = y),
      linewidth = .30, alpha = .34, colour = "#596166", lineend = "round"
    ) +
    geom_segment(
      data = overall,
      aes(x = estimate_q25_plot, xend = estimate_q75_plot, y = y, yend = y),
      linewidth = .90, alpha = .88, colour = "#343B3F", lineend = "round"
    ) +
    geom_point(
      data = overall,
      aes(estimate_q50_plot, y, colour = predictor_family),
      shape = 18, size = 1.05, alpha = 1
    ) +
    geom_segment(
      data = dim,
      aes(x = estimate_q25_plot, xend = estimate_q75_plot, y = y_pos, yend = y_pos),
      linewidth = .20, alpha = .34, colour = "#70787C", lineend = "round"
    ) +
    geom_point(
      data = dim,
      aes(estimate_q50_plot, y_pos, shape = dimension_label),
      size = .58, stroke = .23, colour = "#616A6F", fill = "white", alpha = .90
    ) +
    geom_text(
      data = miss,
      aes(x = 0, y = y_pos, label = status_label),
      size = 1.05, colour = "#BEC2C4"
    ) +
    scale_colour_manual(values = PREDICTOR_COLORS, drop = FALSE, guide = "none") +
    scale_shape_manual(values = DIMENSION_SHAPES, drop = FALSE, guide = "none") +
    scale_x_continuous(limits = coef_window_global, breaks = scales::breaks_extended(n = 3)) +
    scale_y_continuous(limits = predictor_y_limits, expand = expansion(mult = c(0, 0))) +
    labs(title = panel_title, x = "standardized \u03b2", y = NULL) +
    theme_rq2(base_size = 5.30) +
    theme(
      panel.grid.major.y = element_blank(), axis.line.y = element_blank(),
      axis.ticks.y = element_blank(), axis.text.y = element_blank(),
      axis.text.x = element_text(size = 3.55), axis.title.x = element_text(size = 4.15),
      plot.title = element_text(size = 5.25, hjust = .5, face = "bold", margin = margin(b = 1.2)),
      plot.margin = margin(1.5, .7, 1.5, .7)
    )
  add_row_guides(p)
}

p_signed <- make_effect_panel("Signed", "Signed effect")
p_absolute <- make_effect_panel("Absolute", "Absolute effect")

predictor_legend <- cowplot::get_legend(
  ggplot(
    tibble(
      predictor_family = factor(FAMILY_LEVELS, levels = FAMILY_LEVELS),
      x = seq_along(FAMILY_LEVELS), y = 1
    ), aes(x, y, colour = predictor_family)
  ) +
    geom_point(size = 1.05) +
    scale_colour_manual(
      values = PREDICTOR_COLORS, drop = FALSE,
      labels = c("External opportunity" = "External", "Micro-environment" = "Micro-env.",
                 "Behaviour" = "Behaviour", "Exposure state" = "Exposure state")
    ) +
    guides(colour = guide_legend(title = NULL, nrow = 1, byrow = TRUE)) +
    theme_void(base_family = MS_FONT) +
    theme(
      legend.position = "bottom", legend.text = element_text(size = 4.00),
      legend.key.width = grid::unit(2.3, "mm"), legend.spacing.x = grid::unit(.45, "mm"),
      legend.margin = margin(0, 0, 0, 0)
    )
)

dimension_legend <- cowplot::get_legend(
  ggplot(
    tibble(
      dimension_label = factor(unname(DIM_TITLES[DIMENSIONS]), levels = unname(DIM_TITLES[DIMENSIONS])),
      x = seq_along(DIMENSIONS), y = 1
    ), aes(x, y, shape = dimension_label)
  ) +
    geom_point(size = 1.00, stroke = .28, colour = "#616A6F", fill = "white") +
    scale_shape_manual(values = DIMENSION_SHAPES, drop = FALSE) +
    guides(shape = guide_legend(title = "dimension fingerprint", nrow = 1, byrow = TRUE)) +
    theme_void(base_family = MS_FONT) +
    theme(
      legend.position = "bottom", legend.text = element_text(size = 3.75),
      legend.title = element_text(size = 3.75, colour = "#666A6D"),
      legend.key.width = grid::unit(2.2, "mm"), legend.spacing.x = grid::unit(.40, "mm"),
      legend.margin = margin(0, 0, 0, 0)
    )
)

p2a_core <- cowplot::plot_grid(
  p_labels, p_strength, p_signed, p_absolute,
  ncol = 4, rel_widths = c(.23, .12, .325, .325),
  align = "hv", axis = "tb", greedy = TRUE
)
p2a_legends <- cowplot::plot_grid(
  predictor_legend, dimension_legend,
  ncol = 2, rel_widths = c(.48, .52), align = "h", axis = "b", greedy = TRUE
)
p2a_body <- cowplot::plot_grid(
  p2a_core, p2a_legends,
  ncol = 1, rel_heights = c(.935, .065), align = "v", axis = "l", greedy = TRUE
)
p2a <- cowplot::ggdraw() +
  cowplot::draw_plot(p2a_body, x = 0, y = 0, width = 1, height = .965) +
  cowplot::draw_label(
    "a  Contextual predictor hierarchy",
    x = .002, y = .998, hjust = 0, vjust = 1,
    fontface = "bold", size = 7.0
  ) +
  cowplot::draw_label(
    "overall coefficient distributions are foreground; dimension-specific estimates form the secondary fingerprint",
    x = .002, y = .972, hjust = 0, vjust = 1,
    colour = "#666A6D", size = 4.25
  )

coef_metric_display <- coef_metric |> mutate(displayed_in_fig2a = TRUE)

# -----------------------------------------------------------------------------
# Supplementary contextual information diagnostic
# -----------------------------------------------------------------------------
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
    information = recode(
      information,
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

# -----------------------------------------------------------------------------
# c. Out-of-sample contextual predictability
# -----------------------------------------------------------------------------
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
    outcome_label = recode(
      outcome,
      signed = "Signed distortion", magnitude = "Absolute distortion", .default = outcome
    ),
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
  summarise(
    n_metrics = n_distinct(metric),
    fraction_positive = mean(r2 > 0, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    signed_direction = if_else(outcome_label == "Signed distortion", -1, 1),
    fraction_plot = signed_direction * fraction_positive,
    label = paste0(round(100 * fraction_positive), "%")
  )

joint_cv_window_panel <- robust_bounded_display_range(
  joint_cv_metric_panel$r2,
  c(joint_cv_summary_panel$r2_median,
    joint_cv_summary_panel$r2_q25, joint_cv_summary_panel$r2_q75),
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

p_cv_panel <- ggplot(joint_cv_metric_panel, aes(r2_plot, y_pos, colour = metric_class)) +
  geom_vline(xintercept = 0, linewidth = .27, colour = "#A1A6A9") +
  geom_point(
    position = position_jitter(width = 0, height = .014, seed = 63),
    size = .34, alpha = .18
  ) +
  geom_segment(
    data = joint_cv_summary_panel,
    aes(x = r2_q25_plot, xend = r2_q75_plot, y = y_pos, yend = y_pos, colour = metric_class),
    inherit.aes = FALSE, linewidth = .58, alpha = .56, lineend = "round"
  ) +
  geom_point(
    data = joint_cv_summary_panel,
    aes(r2_median_plot, y_pos, colour = metric_class),
    inherit.aes = FALSE, shape = 18, size = .98
  ) +
  facet_wrap(~outcome_label, nrow = 1) +
  scale_colour_ms_metric(guide = "none") +
  scale_x_continuous(limits = joint_cv_window_panel, breaks = scales::breaks_extended(n = 3)) +
  scale_y_continuous(
    breaks = dim_map_panel$y, labels = dim_map_panel$dimension_label,
    limits = range(dim_map_panel$y) + c(-.34, .34), expand = expansion(mult = c(0, 0))
  ) +
  labs(x = "participant-grouped CV R\u00b2", y = NULL) +
  theme_rq2(base_size = 4.9) +
  theme(
    panel.grid.major.y = element_blank(), axis.line.y = element_blank(), axis.ticks.y = element_blank(),
    axis.text.y = element_text(size = 3.85), axis.text.x = element_text(size = 3.7),
    axis.title.x = element_text(size = 4.15), strip.text = element_text(size = 4.15, face = "bold"),
    panel.spacing = grid::unit(.45, "mm"), plot.margin = margin(.7, .7, .7, .7)
  )

FRACTION_COLORS_PANEL <- c(
  "Absolute distortion" = "#707B83",
  "Signed distortion" = "#B7BDC1"
)
p_frac_panel <- ggplot(joint_cv_positive_panel, aes(fraction_plot, y)) +
  geom_vline(xintercept = 0, linewidth = .27, colour = "#A1A6A9") +
  geom_segment(
    aes(x = 0, xend = fraction_plot, y = y, yend = y, colour = outcome_label),
    linewidth = 2.4, alpha = .82, lineend = "butt"
  ) +
  geom_text(
    aes(x = fraction_plot, label = label,
        hjust = if_else(fraction_plot >= 0, -.08, 1.08)),
    size = 1.35, colour = "#54595C"
  ) +
  scale_colour_manual(values = FRACTION_COLORS_PANEL, guide = "none") +
  scale_x_continuous(
    limits = c(-1.16, 1.16), breaks = c(-1, -.5, 0, .5, 1),
    labels = c("100", "50", "0", "50", "100")
  ) +
  scale_y_continuous(
    limits = range(dim_map_panel$y) + c(-.34, .34), expand = expansion(mult = c(0, 0))
  ) +
  labs(title = "% metrics with CV R\u00b2 > 0", x = "%", y = NULL) +
  theme_rq2(base_size = 4.75) +
  theme(
    panel.grid.major.y = element_blank(), panel.grid.minor = element_blank(), axis.line.y = element_blank(),
    axis.ticks.y = element_blank(), axis.text.y = element_blank(), axis.text.x = element_text(size = 3.55),
    axis.title.x = element_text(size = 3.9),
    plot.title = element_text(size = 4.05, hjust = .5, face = "bold", margin = margin(b = .7)),
    plot.margin = margin(.7, 1.15, .7, .3)
  )

p2c_core <- cowplot::plot_grid(
  p_cv_panel, p_frac_panel, ncol = 2, rel_widths = c(.77, .23),
  align = "hv", axis = "tb", greedy = TRUE
)
p2c <- cowplot::ggdraw() +
  cowplot::draw_plot(p2c_core, x = 0, y = 0, width = 1, height = .955) +
  cowplot::draw_label(
    "c  Out-of-sample contextual predictability",
    x = .004, y = .997, hjust = 0, vjust = 1,
    fontface = "bold", size = 6.15
  )

metric_legend_plot <- ggplot(
  tibble(metric_class = factor(METRIC_CLASSES, levels = METRIC_CLASSES), x = 1, y = 1),
  aes(x, y, colour = metric_class)
) +
  geom_point(size = 1.05) +
  scale_colour_ms_metric() +
  guides(colour = guide_legend(
    title = NULL, nrow = 2, byrow = TRUE,
    override.aes = list(size = 1.05, alpha = 1)
  )) +
  theme_void(base_family = MS_FONT) +
  theme(
    legend.position = "bottom", legend.text = element_text(size = 3.75),
    legend.key.width = grid::unit(2.1, "mm"), legend.spacing.x = grid::unit(.45, "mm"),
    legend.margin = margin(0, 0, 0, 0)
  )
metric_legend_right <- cowplot::get_legend(metric_legend_plot)

# -----------------------------------------------------------------------------
# Main composition
# -----------------------------------------------------------------------------
# Preserve the accepted predictor atlas, but compose the new scientific panels
# here. Legacy whole-figure refinement must not overwrite this composition.
accepted <- ms_fig2_refine_main(environment())
if (is.null(accepted$p2a)) stop("Missing accepted Fig. 3a predictor atlas")
p2a <- accepted$p2a
theme_dense <- function() theme_ms_axes(base_size = 6, legend_position = "none") +
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_line(colour = "#ECEFF0", linewidth = .18),
    strip.text = element_text(size = 6, face = "bold"), axis.text = element_text(size = 5.4),
    plot.title = element_text(size = 7, face = "bold"), plot.subtitle = element_text(size = 5.2),
    plot.margin = margin(3, 4, 3, 4))
section <- function(plot, title, subtitle, header = .15) ggdraw() +
  draw_plot(plot, 0, 0, 1, 1 - header) +
  draw_label(title, x = .012, y = .995, hjust = 0, vjust = 1, size = 7.4, fontface = "bold", fontfamily = MS_FONT) +
  draw_label(subtitle, x = .012, y = 1 - header * .48, hjust = 0, vjust = 1,
    size = 5.1, colour = "#666A6D", fontfamily = MS_FONT)

# Every metric is visible. Smooth, zero-preserving display transforms retain
# tails without clipping or using different windows for different dimensions.
p_shift <- ggplot(conditional_shift_path_metric,
    aes(delta_A, delta_direction, group = metric, colour = metric_class)) +
  geom_hline(yintercept = 0, colour = "#BFC5C8", linewidth = .25) +
  geom_vline(xintercept = 0, colour = "#BFC5C8", linewidth = .25) +
  geom_path(linewidth = .18, alpha = .18) +
  geom_point(data = conditional_shift_path_metric |> filter(state_num > 1),
    aes(shape = state), size = .7, alpha = .40) +
  geom_path(data = conditional_shift_class_path, aes(group = metric_class), linewidth = .65) +
  geom_point(data = conditional_shift_class_path |> filter(state_num > 1), aes(shape = state, group = metric_class), size = 1.4) +
  facet_wrap(~dimension_label, ncol = 2) +
  scale_colour_manual(values = MS_METRIC_COLORS) + scale_shape_manual(values = c(Low = 1, Middle = 16, High = 18)) +
  scale_x_continuous(trans = scales::pseudo_log_trans(sigma = .025), breaks = c(-.5, -.1, 0, .1, .5)) +
  scale_y_continuous(trans = scales::pseudo_log_trans(sigma = .05), breaks = c(-1, -.2, 0, .2, 1)) +
  labs(x = expression(Delta*A~"relative to low state"), y = expression(Delta*(B/A))) + theme_dense()
p2b <- section(p_shift, "b  Exposure state modulates distortion",
  "Low \u2192 middle (circle) \u2192 high (diamond); lines = metric-class medians", .13)

# Full CV distributions replace the selected top-eight showcase. Exposure-state
# and joint models are explanatory and are not deployable recovery models.
cv_display <- performance |> filter(validation_scheme == "participant_grouped", is.finite(r2)) |>
  group_by(dimension, metric, model_family, outcome) |>
  summarise(r2 = median(r2), .groups = "drop") |>
  mutate(family = factor(model_family, levels = rev(c("external_context", "microenvironment", "behaviour", "exposure_state", "joint")),
      labels = rev(c("External", "Micro-env.", "Behaviour", "Exposure", "Joint"))),
    dimension = factor(dimension, levels = DIMENSIONS, labels = c("Placement", "Optical", "Temporal", "Duration")),
    outcome = factor(outcome, levels = c("signed", "magnitude"), labels = c("Signed", "Absolute")))
cv_summary <- cv_display |> group_by(dimension, family, outcome) |>
  summarise(mid = median(r2), lo = quantile(r2, .25), hi = quantile(r2, .75), .groups = "drop")
p_cv <- ggplot(cv_display, aes(r2, family, colour = outcome)) +
  geom_vline(xintercept = 0, colour = "#A7ADB0", linewidth = .3) +
  geom_point(position = position_jitter(width = 0, height = .14, seed = 12), size = .4, alpha = .22) +
  geom_linerange(data = cv_summary, aes(x = mid, xmin = lo, xmax = hi),
    position = position_dodge(width = .4), linewidth = .65) +
  geom_point(data = cv_summary, aes(x = mid), position = position_dodge(width = .4), size = 1.1) +
  facet_wrap(~dimension, nrow = 1) +
  scale_colour_manual(values = c(Signed = "#2F5D7E", Absolute = "#C67C2E")) +
  scale_x_continuous(trans = scales::pseudo_log_trans(sigma = .003), breaks = c(-1, 0, .5)) +
  labs(x = expression("Participant-grouped CV "*R^2), y = NULL) + theme_dense() +
  theme(axis.text.x = element_text(size = 4.7), panel.spacing = unit(1.2, "mm"))
p2c <- section(p_cv, "c  Which information predicts distortion?",
  "All metrics; median / IQR \u00b7 blue = signed, ochre = absolute", .21)

# Frozen four-cell gamma only: reorganize Q and R/Q, never recompute gamma.
if (any(gamma_summary$Q < 0, na.rm = TRUE) ||
    any(abs(gamma_summary$R) > gamma_summary$Q + 1e-8, na.rm = TRUE)) stop("Invalid frozen gamma geometry")
gamma_display <- gamma_summary |>
  mutate(pair = paste(dimension_a, dimension_b, sep = " \u00d7 "),
    pair = factor(pair, levels = c("placement \u00d7 optical", "optical \u00d7 temporal", "placement \u00d7 temporal"),
      labels = c("Placement \u00d7 optical", "Optical \u00d7 temporal", "Placement \u00d7 temporal")),
    placement = case_when(str_detect(transition, "^chest") ~ "Chest", str_detect(transition, "^wrist") ~ "Wrist", TRUE ~ "Eye"),
    step = str_remove(transition, "^(chest|wrist)_"),
    step_index = if_else(dimension_b == "optical", if_else(placement == "Chest", 1L, 2L),
      match(step, c("120to60", "60to40", "40to30", "30to20", "20to10"))),
    C = if_else(Q > NUMERIC_TOL, R / Q, NA_real_)) |>
  filter(!is.na(pair), !is.na(step_index))
gamma_class <- gamma_display |> group_by(pair, placement, step_index, metric_class) |>
  summarise(Q = median(Q, na.rm = TRUE), C = median(C, na.rm = TRUE), .groups = "drop")
# A compact transition atlas replaces sparse columns and overlapping lines.
# Both quantities are medians of the existing metric-level frozen summaries.
gamma_tiles <- gamma_class |> mutate(
  block = case_when(pair == "Placement \u00d7 optical" ~ "P \u00d7 O",
    pair == "Optical \u00d7 temporal" ~ "O \u00d7 T", placement == "Chest" ~ "P \u00d7 T: chest", TRUE ~ "P \u00d7 T: wrist"),
  block = factor(block, levels = c("P \u00d7 O", "O \u00d7 T", "P \u00d7 T: chest", "P \u00d7 T: wrist")),
  step_label = if_else(pair == "Placement \u00d7 optical", if_else(placement == "Chest", "Ch", "Wr"), as.character(step_index)),
  step_label = factor(step_label, levels = c("Ch", "Wr", as.character(1:5))),
  class_plot = factor(metric_class, levels = rev(MS_METRIC_CLASSES),
    labels = rev(c("Duration", "History", "Level", "Spectrum", "Dynamics", "Timing"))))
gamma_tiles <- gamma_tiles |> group_by(block) |>
  tidyr::complete(tidyr::nesting(step_label), class_plot) |> ungroup()
gamma_panel <- function(field) {
  limit <- max(abs(gamma_tiles[[field]]), na.rm = TRUE)
  ggplot(gamma_tiles, aes(step_label, class_plot, fill = .data[[field]])) +
    geom_tile(colour = "white", linewidth = .4) +
    geom_point(data = gamma_tiles |> filter(!is.finite(.data[[field]])), shape = 4, size = .7,
      colour = "#879198", stroke = .3) +
    facet_grid(~block, scales = "free_x", space = "free_x") +
    {if (field == "Q") scale_fill_gradientn(colours = c("#F2F6F7", "#B4CBD9", "#5E91B0", "#254F6D"),
      trans = scales::pseudo_log_trans(sigma = .015), limits = c(0, limit), breaks = c(0, .03, .1, .3), name = "Median Q", na.value = "#E5E8EA")
      else scale_fill_gradient2(low = "#31678C", mid = "#F5F5F0", high = "#B66A30",
        limits = c(-limit, limit), breaks = c(-limit, 0, limit), labels = function(x) signif(x, 2), name = "Median R/Q", na.value = "#E5E8EA")} +
    labs(x = NULL, y = NULL) + theme_dense() +
    theme(panel.grid = element_blank(), axis.line = element_blank(), axis.ticks = element_blank(),
      axis.text.y = element_text(size = 5.4), strip.text = element_text(size = 5),
      panel.spacing.x = unit(1.4, "mm"), legend.position = "right", legend.title = element_text(size = 4.9),
      legend.text = element_text(size = 4.7), legend.key.height = unit(5, "mm"), legend.key.width = unit(2, "mm"))
}
gamma_body <- plot_grid(gamma_panel("Q"), gamma_panel("C"), ncol = 1, align = "v", axis = "lr")
p2d <- section(gamma_body, "d  Where do configuration effects interact?",
  "Class medians: magnitude (top), coherence (bottom); crosses = unavailable", .15)
right_column <- plot_grid(p2b, p2c, p2d, ncol = 1, rel_heights = c(.40, .24, .36))
main_body <- plot_grid(p2a, right_column, ncol = 2, rel_widths = c(.49, .51))
legends <- plot_grid(predictor_legend, dimension_legend, ms_metric_legend(text_size = 5.3),
  ncol = 1, rel_heights = c(1, 1, 1))
foot <- ggdraw() + draw_label(paste0(
  "State-conditioned explanation includes high-information exposure state; it is excluded from recovery in Fig. 4.\n",
  "d: placement \u00d7 optical steps 1/2 = chest/wrist; temporal steps 1\u20135 = 120\u219260, 60\u219240, 40\u219230, 30\u219220, 20\u219210 s.\n",
  "Display summaries weight metrics equally. IQRs describe metric heterogeneity, not confidence intervals."),
  x = .015, hjust = 0, size = 5.1, colour = "#626A70", fontfamily = MS_FONT)
p2 <- plot_grid(main_body, legends, foot, ncol = 1, rel_heights = c(1, .07, .06))
ms_fig2_refine_main <- function(...) NULL
ms_polish_main_figure <- function(plot, path, caller_env, width, height) list(plot = plot, width = width, height = height)
ms_plot_save(p2, file.path(OUT_DIR, "Fig2_RQ2.png"), 7.40, 8.60)
write_csv(cv_display, file.path(OUT_DIR, "fig3_context_cv_complete.csv"))
write_csv(gamma_display, file.path(OUT_DIR, "fig3_nonadditivity_display.csv"))

# -----------------------------------------------------------------------------
# Audit/display tables
# -----------------------------------------------------------------------------
readr::write_csv(
  conditional_profile_summary |>
    mutate(metric_class = as.character(metric_class), state_bin_label = as.character(state_bin_label)),
  file.path("results", "rq2", "fig2_conditional_profile.csv"), na = ""
)
readr::write_csv(
  conditional_shift_metric |>
    mutate(metric_class = as.character(metric_class), state = as.character(state)),
  file.path("results", "rq2", "fig2_conditional_shift.csv"), na = ""
)
readr::write_csv(
  predictor_order |> mutate(predictor_family = as.character(predictor_family)),
  file.path("results", "rq2", "fig2_context_predictor_order.csv"), na = ""
)
readr::write_csv(
  coef_metric_display |>
    mutate(predictor_family = as.character(predictor_family), outcome_label = as.character(outcome_label)),
  file.path("results", "rq2", "fig2_context_predictor_display_diagnostics.csv"), na = ""
)
readr::write_csv(
  coef_summary |>
    mutate(predictor_family = as.character(predictor_family), outcome_label = as.character(outcome_label)),
  file.path("results", "rq2", "fig2_context_predictor_summary.csv"), na = ""
)
readr::write_csv(
  tidyr::crossing(
    PREDICTOR_CATALOG |>
      transmute(term, predictor = label, predictor_family = as.character(predictor_family)),
    dimension = DIMENSIONS,
    outcome = c("Signed", "Absolute")
  ) |>
    left_join(
      coef_summary |>
        transmute(
          term, dimension, outcome = as.character(outcome_label),
          n_display_units, n_metrics,
          Q05 = estimate_q05, Q25 = estimate_q25, Q50 = estimate_q50,
          Q75 = estimate_q75, Q95 = estimate_q95
        ),
      by = c("term", "dimension", "outcome")
    ) |>
    arrange(factor(predictor_family, levels = FAMILY_LEVELS), predictor, dimension, outcome),
  file.path("results", "rq2", "fig2_context_predictor_quantiles.csv"), na = ""
)
readr::write_csv(
  context_task |> mutate(dimension = as.character(dimension), information = as.character(information)),
  file.path("results", "rq2", "fig2_context_increment.csv"), na = ""
)
readr::write_csv(
  transition_spread |>
    mutate(dimension = as.character(dimension), transition_key = as.character(transition_key)),
  file.path("results", "rq2", "fig2_transition_spread.csv"), na = ""
)
readr::write_csv(
  joint_cv_metric_panel |>
    mutate(metric_class = as.character(metric_class), outcome_label = as.character(outcome_label)),
  file.path("results", "rq2", "fig2_joint_context_cv.csv"), na = ""
)

ms_plot_write_manifest(
  file.path(OUT_DIR, "figure_artifact_manifest.csv"),
  tibble(
    figure = "Fig2_RQ2",
    input_artifact = "rq2_conditional_geometry+rq2_model_coefficients+rq2_model_performance+rq2_gamma_summary",
    core_artifact_version = CORE_VERSION,
    rq1_analysis_version = RQ1_VERSION,
    rq2_analysis_version = RQ2_VERSION,
    rq3_analysis_version = NA_character_
  )
)

message("Fig. 3 complete: contextual effects, state modulation, full held-out predictability and cross-dimensional non-additivity.")
