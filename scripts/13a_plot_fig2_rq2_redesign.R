# Fig. 2 redesign: dense contextual-predictor atlas + compact conditional geometry
# + held-out predictability. This script is intentionally Fig. 2-only so the new
# main-text design can be iterated without touching the canonical Fig. 3 pipeline.
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

suppressPackageStartupMessages({
  library(tidyverse)
  library(cowplot)
})
source("scripts/utils/figure_style.R")
source("scripts/utils/plot_contracts.R")
source("scripts/utils/analysis_design.R")

RQ1_SUMMARY_CSV <- file.path("results", "rq1", "rq1_pairwise_summary.csv")
COND_GEOM_CSV <- file.path("results", "rq2", "rq2_conditional_geometry.csv")
MODEL_COEF_CSV <- file.path("results", "rq2", "rq2_model_coefficients.csv")
MODEL_PERF_CSV <- file.path("results", "rq2", "rq2_model_performance.csv")
OUT_DIR <- file.path("results", "rq2", "figures")

ms_plot_require_files(
  c(RQ1_SUMMARY_CSV, COND_GEOM_CSV, MODEL_COEF_CSV, MODEL_PERF_CSV),
  "Fig. 2 redesigned plotting inputs"
)
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

rq1_summary <- readr::read_csv(RQ1_SUMMARY_CSV, show_col_types = FALSE, progress = FALSE)
conditional <- readr::read_csv(COND_GEOM_CSV, show_col_types = FALSE, progress = FALSE)
coefficients <- readr::read_csv(MODEL_COEF_CSV, show_col_types = FALSE, progress = FALSE)
performance <- readr::read_csv(MODEL_PERF_CSV, show_col_types = FALSE, progress = FALSE)

ms_plot_require_columns(
  rq1_summary, c("metric", "metric_class", "dimension", "A_mean_absolute"),
  "rq1_pairwise_summary.csv"
)
ms_plot_require_columns(
  conditional,
  c("dimension", "comparison_pair_id", "config_a_label", "config_b_label", "metric", "metric_class",
    "state_bin_label", "A_conditional", "B_conditional"),
  "rq2_conditional_geometry.csv"
)
ms_plot_require_columns(
  coefficients,
  c("dimension", "comparison_pair_id", "metric", "outcome", "model_family", "term",
    "estimate", "std_error", "p_value"),
  "rq2_model_coefficients.csv"
)
ms_plot_require_columns(
  performance,
  c("dimension", "comparison_pair_id", "metric", "outcome", "model_family",
    "validation_scheme", "n_test", "rmse", "mae", "r2"),
  "rq2_model_performance.csv"
)

METRIC_CLASSES <- MS_METRIC_CLASSES
DIMENSIONS <- c("placement", "optical", "temporal", "duration")
DIM_TITLES <- c(
  placement = "Placement",
  optical = "Optical representation",
  temporal = "Temporal resolution",
  duration = "Monitoring duration"
)
metric_class_lookup <- rq1_summary |> distinct(metric, metric_class)

theme_rq2 <- function(base_size = 6.2, legend_position = "none") {
  theme_ms_axes(base_size = base_size, legend_position = legend_position)
}

robust_symmetric_display_limit <- function(x, summary_values = numeric(), prob = .97,
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

robust_upper_display_limit <- function(x, summary_values = numeric(), prob = .97,
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

robust_bounded_display_range <- function(x, summary_values = numeric(), probs = c(.03, .97),
                                         lower = -1, upper = 1, min_span = .30,
                                         pad_fraction = .07) {
  x <- as.numeric(x); x <- x[is.finite(x)]
  s <- as.numeric(summary_values); s <- s[is.finite(s)]
  if (!length(x) && !length(s)) return(c(lower, upper))
  q <- if (length(x)) {
    as.numeric(stats::quantile(x, probs, na.rm = TRUE, names = FALSE, type = 8))
  } else c(min(s), max(s))
  lo <- max(lower, min(c(q[[1]], s), na.rm = TRUE))
  hi <- min(upper, max(c(q[[2]], s), na.rm = TRUE))
  if (!is.finite(lo) || !is.finite(hi) || lo >= hi) return(c(lower, upper))
  span <- hi - lo
  if (span < min_span) {
    center <- (lo + hi) / 2
    lo <- max(lower, center - min_span / 2)
    hi <- min(upper, center + min_span / 2)
  }
  span <- hi - lo
  c(max(lower, lo - span * pad_fraction), min(upper, hi + span * pad_fraction))
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

squish_to_limits <- function(x, limits) {
  pmax(limits[[1]], pmin(limits[[2]], x))
}

# =============================================================================
# a. Full contextual-predictor atlas
# =============================================================================

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

PREDICTOR_COLORS <- c(
  "External opportunity" = MS_PRIMARY,
  "Micro-environment" = "#5F8F84",
  "Behaviour" = MS_NEUTRAL,
  "Exposure state" = MS_SECONDARY
)
OUTCOME_SHAPES <- c("Signed" = 16, "Absolute" = 17)

# Collapse repeated transition-specific fits within metric before visualization.
# This avoids allowing dimensions with many local transitions to dominate the
# visual summary while retaining metric-level heterogeneity as the raw layer.
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

predictor_order <- PREDICTOR_CATALOG |>
  left_join(predictor_overall, by = "term") |>
  mutate(overall_abs_sort = if_else(is.finite(overall_abs), overall_abs, -Inf)) |>
  arrange(predictor_family, desc(overall_abs_sort), label) |>
  mutate(
    row_index = row_number(),
    y = n() - row_index + 1L
  ) |>
  select(-overall_abs_sort)

coef_metric <- coef_metric |>
  left_join(predictor_order |> select(term, y), by = "term") |>
  mutate(y_pos = y + if_else(outcome_label == "Signed", -.115, .115))

coef_summary <- coef_metric |>
  group_by(dimension, term, label, predictor_family, y, outcome_label) |>
  summarise(
    n_metrics = n_distinct(metric),
    estimate_median = median(estimate, na.rm = TRUE),
    estimate_q25 = quantile(estimate, .25, na.rm = TRUE, names = FALSE),
    estimate_q75 = quantile(estimate, .75, na.rm = TRUE, names = FALSE),
    .groups = "drop"
  ) |>
  mutate(y_pos = y + if_else(outcome_label == "Signed", -.115, .115))

predictor_axis <- predictor_order |> arrange(y)
family_boundaries <- predictor_order |>
  group_by(predictor_family) |>
  summarise(boundary = min(y) - .5, .groups = "drop") |>
  arrange(predictor_family) |>
  slice_head(n = length(FAMILY_LEVELS) - 1L) |>
  pull(boundary)

coef_window_global <- robust_symmetric_display_window(
  coef_metric$estimate,
  c(coef_summary$estimate_median, coef_summary$estimate_q25, coef_summary$estimate_q75),
  probs = c(.08, .92), min_half = .02, pad = 1.06
)
coef_metric_global <- coef_metric |>
  mutate(estimate_plot = squish_to_limits(estimate, coef_window_global))
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
    p + geom_hline(yintercept = family_boundaries, linewidth = .26, color = "#D8DBDD")
  } else p
}

p_overall <- ggplot(
  predictor_order,
  aes(overall_abs_plot, y, fill = predictor_family)
) +
  geom_col(width = .58, alpha = .90, na.rm = TRUE) +
  geom_point(
    data = predictor_order |> filter(overall_clipped),
    aes(x = overall_limit, y = y), inherit.aes = FALSE,
    shape = 17, size = .70, color = "#4E5559"
  ) +
  geom_point(
    data = predictor_order |> filter(!is.finite(overall_abs)),
    aes(x = 0, y = y), inherit.aes = FALSE,
    shape = 4, size = .75, stroke = .35, color = "#AEB2B5"
  ) +
  scale_fill_manual(values = PREDICTOR_COLORS, drop = FALSE, guide = "none") +
  scale_x_continuous(
    limits = c(0, overall_limit),
    breaks = scales::breaks_extended(n = 3),
    expand = expansion(mult = c(0, .02))
  ) +
  scale_y_continuous(
    breaks = predictor_axis$y,
    labels = predictor_axis$label,
    limits = c(.5, nrow(predictor_order) + .5),
    expand = expansion(mult = c(0, 0))
  ) +
  labs(title = "Overall", x = "median |β|", y = NULL) +
  theme_rq2(base_size = 5.65) +
  theme(
    panel.grid = element_blank(),
    axis.line.y = element_blank(), axis.ticks.y = element_blank(),
    axis.text.y = element_text(size = 4.25, colour = "#34383B", hjust = 1),
    axis.text.x = element_text(size = 4.25), axis.title.x = element_text(size = 4.55),
    plot.title = element_text(size = 5.35, hjust = .5, face = "bold", margin = margin(b = 1.2)),
    plot.margin = margin(1.5, 1.2, 1.5, 1.5)
  )
p_overall <- add_row_guides(p_overall)

p_spread <- ggplot(coef_metric_global, aes(estimate_plot, y, color = predictor_family)) +
  geom_vline(xintercept = 0, linewidth = .28, color = "#A1A6A9") +
  geom_point(
    position = position_jitter(width = 0, height = .075, seed = 59),
    size = .34, alpha = .16
  ) +
  scale_color_manual(values = PREDICTOR_COLORS, drop = FALSE, guide = "none") +
  scale_x_continuous(
    limits = coef_window_global,
    breaks = scales::breaks_extended(n = 3)
  ) +
  scale_y_continuous(limits = c(.5, nrow(predictor_order) + .5), expand = expansion(mult = c(0, 0))) +
  labs(title = "Task spread", x = "β", y = NULL) +
  theme_rq2(base_size = 5.35) +
  theme(
    panel.grid.major.y = element_blank(),
    axis.line.y = element_blank(), axis.ticks.y = element_blank(), axis.text.y = element_blank(),
    axis.text.x = element_text(size = 4.05), axis.title.x = element_text(size = 4.4),
    plot.title = element_text(size = 5.15, hjust = .5, face = "bold", margin = margin(b = 1.2)),
    plot.margin = margin(1.5, .6, 1.5, .6)
  )
p_spread <- add_row_guides(p_spread)

status_grid <- tidyr::crossing(
  term = PREDICTOR_CATALOG$term,
  dimension = DIMENSIONS,
  outcome_label = factor(c("Signed", "Absolute"), levels = c("Signed", "Absolute"))
) |>
  left_join(predictor_order |> select(term, y), by = "term") |>
  left_join(
    coef_summary |> select(dimension, term, outcome_label, estimate_median),
    by = c("dimension", "term", "outcome_label")
  ) |>
  mutate(
    structural_na = term == "duration_day_variability" & dimension != "duration",
    status_label = case_when(
      structural_na ~ "—",
      !is.finite(estimate_median) ~ "×",
      TRUE ~ NA_character_
    ),
    y_pos = y + if_else(outcome_label == "Signed", -.115, .115)
  )

make_coef_dimension <- function(dim_name) {
  raw <- coef_metric |> filter(dimension == dim_name, is.finite(estimate))
  sm <- coef_summary |> filter(dimension == dim_name, is.finite(estimate_median))
  miss <- status_grid |> filter(dimension == dim_name, !is.na(status_label))
  dim_window <- robust_symmetric_display_window(
    raw$estimate,
    c(sm$estimate_median, sm$estimate_q25, sm$estimate_q75),
    probs = c(.08, .92), min_half = .015, pad = 1.05
  )
  raw <- raw |> mutate(estimate_plot = squish_to_limits(estimate, dim_window))
  sm <- sm |>
    mutate(
      estimate_median_plot = squish_to_limits(estimate_median, dim_window),
      estimate_q25_plot = squish_to_limits(estimate_q25, dim_window),
      estimate_q75_plot = squish_to_limits(estimate_q75, dim_window)
    )

  p <- ggplot() +
    geom_vline(xintercept = 0, linewidth = .28, color = "#A1A6A9") +
    geom_point(
      data = raw,
      aes(estimate_plot, y_pos, color = predictor_family, shape = outcome_label),
      position = position_jitter(width = 0, height = .035, seed = 61),
      size = .34, alpha = .12
    ) +
    geom_segment(
      data = sm,
      aes(x = estimate_q25_plot, xend = estimate_q75_plot, y = y_pos, yend = y_pos, color = predictor_family),
      linewidth = .72, alpha = .58, lineend = "round"
    ) +
    geom_point(
      data = sm,
      aes(estimate_median_plot, y_pos, color = predictor_family, shape = outcome_label),
      size = 1.12, alpha = .98
    ) +
    geom_text(
      data = miss,
      aes(x = 0, y = y_pos, label = status_label),
      size = 1.45, color = "#B3B7BA"
    ) +
    scale_color_manual(values = PREDICTOR_COLORS, drop = FALSE, guide = "none") +
    scale_shape_manual(values = OUTCOME_SHAPES, drop = FALSE, guide = "none") +
    scale_x_continuous(
      limits = dim_window,
      breaks = scales::breaks_extended(n = 3)
    ) +
    scale_y_continuous(limits = c(.5, nrow(predictor_order) + .5), expand = expansion(mult = c(0, 0))) +
    labs(title = unname(DIM_TITLES[[dim_name]]), x = "β", y = NULL) +
    theme_rq2(base_size = 5.25) +
    theme(
      panel.grid.major.y = element_blank(),
      axis.line.y = element_blank(), axis.ticks.y = element_blank(), axis.text.y = element_blank(),
      axis.text.x = element_text(size = 3.95), axis.title.x = element_text(size = 4.35),
      plot.title = element_text(size = 5.05, hjust = .5, face = "bold", margin = margin(b = 1.2)),
      plot.margin = margin(1.5, .45, 1.5, .45)
    )
  add_row_guides(p)
}

coef_dim_plots <- lapply(DIMENSIONS, make_coef_dimension)

predictor_legend_plot <- ggplot(
  coef_metric |> filter(is.finite(estimate)),
  aes(estimate, y, color = predictor_family, shape = outcome_label)
) +
  geom_point(size = 1.15, alpha = 1) +
  scale_color_manual(
    values = PREDICTOR_COLORS, drop = FALSE,
    labels = c(
      "External opportunity" = "External",
      "Micro-environment" = "Micro-env.",
      "Behaviour" = "Behaviour",
      "Exposure state" = "Exposure state"
    )
  ) +
  scale_shape_manual(values = OUTCOME_SHAPES, drop = FALSE) +
  guides(
    color = guide_legend(title = NULL, nrow = 1, order = 1,
                         override.aes = list(alpha = 1, size = 1.05)),
    shape = guide_legend(title = NULL, nrow = 1, order = 2,
                         override.aes = list(alpha = 1, size = 1.05))
  ) +
  theme_void(base_family = MS_FONT) +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 4.25),
    legend.key.width = grid::unit(2.4, "mm"),
    legend.spacing.x = grid::unit(.55, "mm"),
    legend.margin = margin(0, 0, 0, 0)
  )
predictor_legend <- cowplot::get_legend(predictor_legend_plot)

p2a_core <- cowplot::plot_grid(
  p_overall, p_spread,
  coef_dim_plots[[1]], coef_dim_plots[[2]], coef_dim_plots[[3]], coef_dim_plots[[4]],
  ncol = 6,
  rel_widths = c(.305, .125, .1425, .1425, .1425, .1425),
  align = "hv", axis = "tb", greedy = TRUE
)
p2a_body <- cowplot::plot_grid(
  p2a_core, predictor_legend,
  ncol = 1, rel_heights = c(.955, .045),
  align = "v", axis = "l", greedy = TRUE
)
p2a <- cowplot::ggdraw() +
  cowplot::draw_plot(p2a_body, x = 0, y = 0, width = 1, height = .965) +
  cowplot::draw_label(
    "a  Contextual predictor atlas",
    x = .002, y = .998, hjust = 0, vjust = 1,
    fontface = "bold", size = 7.0
  ) +
  cowplot::draw_label(
    "all prespecified predictors; bars = median |β| across estimable display units",
    x = .002, y = .972, hjust = 0, vjust = 1,
    colour = "#666A6D", size = 4.4
  )

# =============================================================================
# b. Conditional distortion geometry across exposure state
# =============================================================================

conditional <- conditional |>
  mutate(
    metric = as.character(metric),
    metric_class = factor(metric_class, levels = METRIC_CLASSES),
    dimension = as.character(dimension),
    state_bin_label = factor(state_bin_label, levels = c("Low", "Middle", "High")),
    state_num = as.integer(state_bin_label),
    direction_ratio = ms_direction_ratio(B_conditional, A_conditional),
    pair_label = paste(config_a_label, "→", config_b_label)
  )

conditional_trajectory_state <- conditional |>
  filter(
    dimension %in% DIMENSIONS,
    is.finite(A_conditional), is.finite(direction_ratio), is.finite(state_num)
  ) |>
  group_by(
    dimension, comparison_pair_id, pair_label, metric, metric_class, state_bin_label, state_num
  ) |>
  summarise(
    A_state = median(A_conditional, na.rm = TRUE),
    direction_state = median(direction_ratio, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    class_num = as.integer(metric_class),
    class_offset = (class_num - (length(METRIC_CLASSES) + 1) / 2) * .047,
    x_pos = state_num + class_offset
  )

conditional_metric_state <- conditional_trajectory_state |>
  group_by(dimension, metric, metric_class, state_bin_label, state_num, class_num, class_offset, x_pos) |>
  summarise(
    A_state = median(A_state, na.rm = TRUE),
    direction_state = median(direction_state, na.rm = TRUE),
    .groups = "drop"
  )

conditional_profile_summary <- conditional_metric_state |>
  group_by(dimension, metric_class, state_bin_label, state_num, class_num, class_offset, x_pos) |>
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
    tr$A_state, c(sm$A_median, sm$A_q25, sm$A_q75),
    prob = .97, pad = 1.06, fallback = .25
  )
  dir_range <- robust_bounded_display_range(
    tr$direction_state, c(sm$direction_median, sm$direction_q25, sm$direction_q75),
    probs = c(.03, .97), lower = -1, upper = 1, min_span = .30
  )

  p_mag <- ggplot() +
    # Keep the pairwise trajectory information, but demote it strongly so the
    # large blank line segments no longer dominate the visual field.
    geom_line(
      data = tr,
      aes(x_pos, A_state, group = interaction(metric, comparison_pair_id), color = metric_class),
      linewidth = .18, alpha = .035
    ) +
    geom_point(
      data = tr,
      aes(x_pos, A_state, color = metric_class),
      size = .27, alpha = .13
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
      limits = c(.76, 3.24), expand = expansion(mult = c(0, 0))
    ) +
    scale_y_continuous(breaks = scales::breaks_extended(n = 3)) +
    coord_cartesian(ylim = c(0, mag_limit), clip = "on") +
    labs(title = unname(DIM_TITLES[[dim_name]]), x = NULL, y = "A") +
    theme_rq2(base_size = 5.05) +
    theme(
      panel.grid.major.x = element_blank(),
      axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.title.x = element_blank(),
      axis.text.y = element_text(size = 3.95), axis.title.y = element_text(size = 4.25),
      plot.title = element_text(size = 4.75, hjust = .5, face = "bold", margin = margin(b = .8)),
      plot.margin = margin(.8, 1.0, 0, 1.0)
    )

  p_dir <- ggplot() +
    geom_hline(yintercept = 0, linewidth = .25, color = "#A1A6A9") +
    geom_line(
      data = tr,
      aes(x_pos, direction_state, group = interaction(metric, comparison_pair_id), color = metric_class),
      linewidth = .16, alpha = .028
    ) +
    geom_point(
      data = tr,
      aes(x_pos, direction_state, color = metric_class),
      size = .24, alpha = .11
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
      limits = c(.76, 3.24), expand = expansion(mult = c(0, 0))
    ) +
    scale_y_continuous(breaks = scales::breaks_extended(n = 2)) +
    coord_cartesian(ylim = dir_range, clip = "on") +
    labs(x = NULL, y = "B/A") +
    theme_rq2(base_size = 4.85) +
    theme(
      panel.grid.major.x = element_blank(),
      axis.text.x = element_text(size = 3.75),
      axis.text.y = element_text(size = 3.65), axis.title.y = element_text(size = 4.0),
      plot.margin = margin(0, 1.0, .8, 1.0)
    )

  cowplot::plot_grid(
    p_mag, p_dir, ncol = 1, rel_heights = c(.79, .21),
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
    x = .004, y = .997, hjust = 0, vjust = 1,
    fontface = "bold", size = 6.35
  ) +
  cowplot::draw_label(
    "transition-local exposure-state tertiles",
    x = .004, y = .970, hjust = 0, vjust = 1,
    colour = "#666A6D", size = 3.95
  )

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
    legend.position = "bottom",
    legend.text = element_text(size = 3.75),
    legend.key.width = grid::unit(2.1, "mm"),
    legend.spacing.x = grid::unit(.45, "mm"),
    legend.margin = margin(0, 0, 0, 0)
  )
metric_legend_right <- cowplot::get_legend(metric_legend_plot)

# =============================================================================
# c. Out-of-sample contextual predictability
# =============================================================================

joint_cv_metric <- performance |>
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
      signed = "Signed distortion",
      magnitude = "Absolute distortion",
      .default = outcome
    ),
    outcome_label = factor(outcome_label, levels = c("Absolute distortion", "Signed distortion"))
  ) |>
  filter(dimension %in% DIMENSIONS, !is.na(metric_class), !is.na(outcome_label), is.finite(r2))

dim_map <- tibble(
  dimension = DIMENSIONS,
  dimension_label = unname(DIM_TITLES[DIMENSIONS]),
  y = rev(seq_along(DIMENSIONS))
)
joint_cv_metric <- joint_cv_metric |>
  left_join(dim_map, by = "dimension") |>
  mutate(
    class_num = as.integer(metric_class),
    class_offset = (class_num - (length(METRIC_CLASSES) + 1) / 2) * .045,
    y_pos = y + class_offset
  )

joint_cv_summary <- joint_cv_metric |>
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

joint_cv_positive <- joint_cv_metric |>
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

joint_cv_window <- robust_bounded_display_range(
  joint_cv_metric$r2,
  c(joint_cv_summary$r2_median, joint_cv_summary$r2_q25, joint_cv_summary$r2_q75),
  probs = c(.08, .92), lower = -0.5, upper = 0.5, min_span = .12, pad_fraction = .08
)
joint_cv_metric <- joint_cv_metric |>
  mutate(r2_plot = squish_to_limits(r2, joint_cv_window))
joint_cv_summary <- joint_cv_summary |>
  mutate(
    r2_median_plot = squish_to_limits(r2_median, joint_cv_window),
    r2_q25_plot = squish_to_limits(r2_q25, joint_cv_window),
    r2_q75_plot = squish_to_limits(r2_q75, joint_cv_window)
  )

p_cv <- ggplot(joint_cv_metric, aes(r2_plot, y_pos, color = metric_class)) +
  geom_vline(xintercept = 0, linewidth = .27, color = "#A1A6A9") +
  geom_point(
    position = position_jitter(width = 0, height = .014, seed = 63),
    size = .34, alpha = .18
  ) +
  geom_segment(
    data = joint_cv_summary,
    aes(x = r2_q25_plot, xend = r2_q75_plot, y = y_pos, yend = y_pos, color = metric_class),
    inherit.aes = FALSE, linewidth = .58, alpha = .56, lineend = "round"
  ) +
  geom_point(
    data = joint_cv_summary,
    aes(r2_median_plot, y_pos, color = metric_class),
    inherit.aes = FALSE, shape = 18, size = .98
  ) +
  facet_wrap(~outcome_label, nrow = 1) +
  scale_color_ms_metric(guide = "none") +
  scale_x_continuous(
    limits = joint_cv_window,
    breaks = scales::breaks_extended(n = 3)
  ) +
  scale_y_continuous(
    breaks = dim_map$y,
    labels = dim_map$dimension_label,
    limits = c(.55, length(DIMENSIONS) + .45),
    expand = expansion(mult = c(0, 0))
  ) +
  labs(x = "participant-grouped CV R²", y = NULL) +
  theme_rq2(base_size = 4.9) +
  theme(
    panel.grid.major.y = element_blank(),
    axis.line.y = element_blank(), axis.ticks.y = element_blank(),
    axis.text.y = element_text(size = 3.85), axis.text.x = element_text(size = 3.7),
    axis.title.x = element_text(size = 4.15),
    strip.text = element_text(size = 4.15, face = "bold"),
    panel.spacing = grid::unit(.9, "mm"),
    plot.margin = margin(.7, .7, .7, .7)
  )

FRACTION_COLORS <- c("Absolute distortion" = "#707B83", "Signed distortion" = "#B7BDC1")
p_frac <- ggplot(joint_cv_positive, aes(fraction_plot, y, fill = outcome_label)) +
  geom_vline(xintercept = 0, linewidth = .27, color = "#A1A6A9") +
  geom_col(width = .52, alpha = .92) +
  geom_text(
    aes(
      x = fraction_plot,
      label = label,
      hjust = if_else(fraction_plot >= 0, -0.08, 1.08)
    ),
    size = 1.35, color = "#54595C"
  ) +
  scale_fill_manual(values = FRACTION_COLORS, guide = "none") +
  scale_x_continuous(
    limits = c(-1.16, 1.16),
    breaks = c(-1, -.5, 0, .5, 1),
    labels = c("100", "50", "0", "50", "100")
  ) +
  scale_y_continuous(limits = c(.55, length(DIMENSIONS) + .45), expand = expansion(mult = c(0, 0))) +
  labs(title = "% metrics with CV R² > 0", x = "%", y = NULL) +
  theme_rq2(base_size = 4.75) +
  theme(
    panel.grid.major.y = element_blank(), panel.grid.minor = element_blank(),
    axis.line.y = element_blank(), axis.ticks.y = element_blank(), axis.text.y = element_blank(),
    axis.text.x = element_text(size = 3.55), axis.title.x = element_text(size = 3.9),
    plot.title = element_text(size = 4.05, hjust = .5, face = "bold", margin = margin(b = .7)),
    plot.margin = margin(.7, 1.7, .7, .3)
  )

p2c_core <- cowplot::plot_grid(
  p_cv, p_frac, ncol = 2, rel_widths = c(.71, .29),
  align = "hv", axis = "tb", greedy = TRUE
)
p2c <- cowplot::ggdraw() +
  cowplot::draw_plot(p2c_core, x = 0, y = 0, width = 1, height = .935) +
  cowplot::draw_label(
    "c  Out-of-sample contextual predictability",
    x = .004, y = .997, hjust = 0, vjust = 1,
    fontface = "bold", size = 6.15
  )

# =============================================================================
# Final composition: predictor atlas dominates the left; the existing state
# geometry is compacted to the upper-right; predictability closes the argument
# in the lower-right. Metric-class colour semantics are shared by b and c.
# =============================================================================

right_top <- cowplot::plot_grid(
  metric_legend_right, p2b,
  ncol = 1, rel_heights = c(.070, .930),
  align = "v", axis = "l", greedy = TRUE
)
right_column <- cowplot::plot_grid(
  right_top, p2c,
  ncol = 1, rel_heights = c(.655, .345),
  align = "v", axis = "l", greedy = TRUE
)
p2 <- cowplot::plot_grid(
  p2a, right_column,
  ncol = 2, rel_widths = c(.62, .38),
  align = "hv", axis = "tb", greedy = TRUE
)

ms_plot_save(p2, file.path(OUT_DIR, "Fig2_RQ2.pdf"), 9.2, 7.25)
ms_plot_save(p2, file.path(OUT_DIR, "Fig2_RQ2.png"), 9.2, 7.25)

# Diagnostics make the figure fully auditable and document the complete predictor
# set instead of the former representative subset.
readr::write_csv(
  predictor_order |>
    mutate(predictor_family = as.character(predictor_family)),
  file.path("results", "rq2", "fig2_context_predictor_order.csv"), na = ""
)
readr::write_csv(
  coef_metric |>
    mutate(
      predictor_family = as.character(predictor_family),
      outcome_label = as.character(outcome_label)
    ),
  file.path("results", "rq2", "fig2_context_predictor_display_diagnostics.csv"), na = ""
)
readr::write_csv(
  coef_summary |>
    mutate(
      predictor_family = as.character(predictor_family),
      outcome_label = as.character(outcome_label)
    ),
  file.path("results", "rq2", "fig2_context_predictor_summary.csv"), na = ""
)
readr::write_csv(
  conditional_profile_summary |>
    mutate(
      metric_class = as.character(metric_class),
      state_bin_label = as.character(state_bin_label)
    ),
  file.path("results", "rq2", "fig2_conditional_profile.csv"), na = ""
)
readr::write_csv(
  joint_cv_metric |>
    mutate(
      metric_class = as.character(metric_class),
      outcome_label = as.character(outcome_label)
    ),
  file.path("results", "rq2", "fig2_joint_context_cv.csv"), na = ""
)
readr::write_csv(
  joint_cv_positive |>
    mutate(outcome_label = as.character(outcome_label)),
  file.path("results", "rq2", "fig2_joint_context_cv_positive_fraction.csv"), na = ""
)

message(
  "Fig. 2 redesign complete: full 20-predictor atlas at left, conditional geometry upper-right, ",
  "and participant-grouped joint-model predictability lower-right."
)
