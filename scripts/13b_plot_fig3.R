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


ms_plot_write_manifest(
  file.path(OUT_DIR, "figure_artifact_manifest.csv"),
  tibble(
    figure = 'Fig3_RQ2',
    input_artifact = 'rq2_gamma_summary',
    core_artifact_version = CORE_VERSION,
    rq1_analysis_version = RQ1_VERSION,
    rq2_analysis_version = RQ2_VERSION,
    rq3_analysis_version = NA_character_
  )
)

message("Fig. 3 complete: cross-dimensional interaction sign, magnitude and coherence.")
