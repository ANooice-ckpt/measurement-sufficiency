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

format_gamma_transition <- function(x) {
  x |>
    str_replace_all("_LIGHT_to_MEDI", " · LIGHT → MEDI") |>
    str_replace_all("([0-9]+)to([0-9]+)", "\\1 → \\2 s") |>
    str_replace_all("_", " · ")
}

PAIR_LEVELS <- c(
  paste("Placement", "optical", sep = " × "),
  paste("Optical", "temporal", sep = " × "),
  paste("Placement", "temporal", sep = " × ")
)
PAIR_CODES <- c("placement__optical", "optical__temporal", "placement__temporal")
names(PAIR_CODES) <- PAIR_LEVELS
PAIR_CODE_TO_LABEL <- setNames(names(PAIR_CODES), unname(PAIR_CODES))
NUMERIC_TOL <- 1e-12

safe_median <- function(x) {
  x <- as.numeric(x)
  x <- x[is.finite(x)]
  if (length(x)) unname(stats::median(x)) else NA_real_
}
safe_q <- function(x, p) {
  x <- as.numeric(x)
  x <- x[is.finite(x)]
  if (length(x)) unname(stats::quantile(x, p, names = FALSE, type = 8)) else NA_real_
}

gamma_plot <- gamma_summary |>
  mutate(
    dimension_pair = case_when(
      dimension_a == "placement" & dimension_b == "optical" ~ PAIR_LEVELS[[1]],
      dimension_a == "placement" & dimension_b == "temporal" ~ PAIR_LEVELS[[3]],
      dimension_a == "optical" & dimension_b == "temporal" ~ PAIR_LEVELS[[2]],
      TRUE ~ paste(dimension_a, "×", dimension_b)
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
    transition = as.character(transition),
    transition_display = format_gamma_transition(transition),
    Q = as.numeric(Q), R = as.numeric(R)
  )
if (any(is.finite(gamma_plot$Q) & gamma_plot$Q < -NUMERIC_TOL, na.rm = TRUE)) {
  stop("RQ2 gamma Q contains a negative value", call. = FALSE)
}
gamma_plot <- gamma_plot |> mutate(Q = abs(Q))

gamma_metric <- gamma_plot |>
  filter(is.finite(R) | is.finite(Q)) |>
  group_by(pair_code, dimension_pair, metric, metric_class) |>
  summarise(
    Q_metric = safe_median(Q),
    R_metric = safe_median(R),
    C_metric = safe_median(if_else(
      is.finite(R) & is.finite(Q) & Q > NUMERIC_TOL, R / Q, NA_real_
    )),
    n_transitions = n_distinct(transition), .groups = "drop"
  ) |>
  mutate(
    C_metric = if_else(is.finite(C_metric), pmax(-1, pmin(1, C_metric)), NA_real_),
    dimension_pair = factor(dimension_pair, levels = PAIR_LEVELS)
  )

metric_table <- gamma_plot |>
  distinct(metric, metric_class) |>
  mutate(metric = as.character(metric), metric_class = as.character(metric_class)) |>
  left_join(
    metric_order |> transmute(metric = as.character(metric), rq1_metric_order = metric_order),
    by = "metric"
  ) |>
  distinct(metric, .keep_all = TRUE)

# Keep representation classes with enough metrics for a genuine distribution.
# Sparse n=1/2 classes remain part of Overall and the faint raw layer but are not
# promoted to their own main-text summary row/profile.
fig3_class_counts <- metric_table |>
  filter(!is.na(metric_class)) |>
  count(metric_class, name = "n_metrics")
fig3_candidate_classes <- fig3_class_counts |>
  filter(n_metrics >= 3L) |>
  pull(metric_class)
fig3_class_rank <- gamma_metric |>
  filter(is.finite(Q_metric), as.character(metric_class) %in% fig3_candidate_classes) |>
  mutate(metric_class = as.character(metric_class)) |>
  group_by(metric_class) |>
  summarise(pooled_Q = safe_median(Q_metric), .groups = "drop") |>
  arrange(desc(pooled_Q), match(metric_class, METRIC_CLASSES))
FIG3_DISPLAY_CLASSES <- fig3_class_rank$metric_class
if (!length(FIG3_DISPLAY_CLASSES)) stop("No metric classes with n >= 3 for Fig. 3", call. = FALSE)
FIG3_CLASS_ORDER <- c("Overall", FIG3_DISPLAY_CLASSES)
FIG3_CLASS_COLORS <- c("Overall" = "#343B3F", MS_METRIC_COLORS)
fig3_n_lookup <- setNames(fig3_class_counts$n_metrics, fig3_class_counts$metric_class)
fig3_row_labels <- setNames(
  vapply(FIG3_CLASS_ORDER, function(z) {
    n <- if (z == "Overall") nrow(metric_table) else unname(fig3_n_lookup[[z]])
    paste0(if (z == "Overall") "Overall" else str_to_sentence(z), " (n=", n, ")")
  }, character(1)),
  FIG3_CLASS_ORDER
)

# -----------------------------------------------------------------------------
# Shared Q display mapping for panels a and b
# -----------------------------------------------------------------------------
make_fig3_focus_tail_axis <- function(values, foreground, prob = .95,
                                      tail_ratio = 1.30, gutter_fraction = .10,
                                      fallback = .25) {
  values <- as.numeric(values)
  values <- values[is.finite(values) & values >= 0]
  foreground <- as.numeric(foreground)
  foreground <- foreground[is.finite(foreground) & foreground >= 0]
  if (!length(values)) {
    return(list(use_tail = FALSE, focus = fallback, raw_max = fallback,
                display_max = fallback, tick_max = fallback,
                map = function(x) as.numeric(x)))
  }
  raw_max <- max(values)
  robust_focus <- safe_q(values, prob)
  foreground_max <- if (length(foreground)) max(foreground) else 0
  focus <- max(c(robust_focus, foreground_max, raw_max * .15), na.rm = TRUE)
  focus <- min(focus, raw_max)
  use_tail <- raw_max > focus + NUMERIC_TOL &&
    (focus <= NUMERIC_TOL || raw_max / focus >= tail_ratio)
  if (!use_tail) {
    display_max <- max(raw_max * 1.06, fallback)
    mapper <- function(x) {
      x <- as.numeric(x)
      ifelse(is.finite(x), pmax(0, pmin(x, display_max)), NA_real_)
    }
    return(list(use_tail = FALSE, focus = raw_max, raw_max = raw_max,
                display_max = display_max, tick_max = raw_max, map = mapper))
  }
  gutter <- max(focus * gutter_fraction, raw_max * .018, fallback * .015)
  display_max <- focus + gutter
  denom <- log1p(raw_max - focus)
  mapper <- function(x) {
    x <- as.numeric(x)
    out <- rep(NA_real_, length(x))
    ok <- is.finite(x)
    low <- ok & x <= focus
    high <- ok & x > focus
    out[low] <- pmax(0, x[low])
    out[high] <- focus + gutter * log1p(pmin(x[high], raw_max) - focus) / denom
    out
  }
  list(use_tail = TRUE, focus = focus, raw_max = raw_max,
       display_max = display_max, tick_max = focus, map = mapper)
}

# -----------------------------------------------------------------------------
# Panel a data — ranked class half-eye atlas
# -----------------------------------------------------------------------------
a_metric_q <- gamma_metric |>
  filter(is.finite(Q_metric)) |>
  mutate(pair_code = as.character(pair_code), metric_class = as.character(metric_class))
a_metric_q <- bind_rows(
  a_metric_q |>
    filter(metric_class %in% FIG3_DISPLAY_CLASSES) |>
    mutate(atlas_class = metric_class),
  a_metric_q |> mutate(atlas_class = "Overall")
)
a_stats_raw <- a_metric_q |>
  group_by(atlas_class, pair_code) |>
  summarise(
    Q_q10 = safe_q(Q_metric, .10), Q_q25 = safe_q(Q_metric, .25),
    Q_median = safe_median(Q_metric), Q_q75 = safe_q(Q_metric, .75),
    Q_q90 = safe_q(Q_metric, .90), .groups = "drop"
  )

# -----------------------------------------------------------------------------
# Panel b data — explicit ordered transition sequences
# -----------------------------------------------------------------------------
fig3_transition_order <- bind_rows(
  tibble(
    pair_code = PAIR_CODES[[1]],
    transition = c("chest_LIGHT_to_MEDI", "wrist_LIGHT_to_MEDI"),
    step_index = 1:2,
    x_label = c("chest\nLIGHT → MEDI", "wrist\nLIGHT → MEDI"),
    placement = c("chest", "wrist")
  ),
  tibble(
    pair_code = PAIR_CODES[[2]],
    transition = c("120to60", "60to40", "40to30", "30to20", "20to10"),
    step_index = 1:5,
    x_label = c("120→60", "60→40", "40→30", "30→20", "20→10 s"),
    placement = "all"
  ),
  tibble(
    pair_code = PAIR_CODES[[3]],
    transition = c(
      "chest_120to60", "chest_60to40", "chest_40to30", "chest_30to20", "chest_20to10",
      "wrist_120to60", "wrist_60to40", "wrist_40to30", "wrist_30to20", "wrist_20to10"
    ),
    step_index = rep(1:5, 2),
    x_label = rep(c("120→60", "60→40", "40→30", "30→20", "20→10 s"), 2),
    placement = rep(c("chest", "wrist"), each = 5)
  )
)

b_raw <- gamma_plot |>
  filter(is.finite(Q)) |>
  mutate(pair_code = as.character(pair_code), metric_class = as.character(metric_class)) |>
  inner_join(fig3_transition_order, by = c("pair_code", "transition")) |>
  mutate(
    placement_offset = case_when(
      pair_code == PAIR_CODES[[3]] & placement == "chest" ~ -.055,
      pair_code == PAIR_CODES[[3]] & placement == "wrist" ~ .055,
      TRUE ~ 0
    ),
    x_plot = step_index + placement_offset
  )

b_class_stats_raw <- b_raw |>
  filter(metric_class %in% FIG3_DISPLAY_CLASSES) |>
  group_by(pair_code, transition, step_index, x_label, placement, placement_offset, metric_class) |>
  summarise(
    Q_q25 = safe_q(Q, .25), Q_median = safe_median(Q), Q_q75 = safe_q(Q, .75),
    .groups = "drop"
  ) |>
  mutate(x_plot = step_index + placement_offset)
b_overall_stats_raw <- b_raw |>
  group_by(pair_code, transition, step_index, x_label, placement, placement_offset) |>
  summarise(
    Q_q25 = safe_q(Q, .25), Q_median = safe_median(Q), Q_q75 = safe_q(Q, .75),
    .groups = "drop"
  ) |>
  mutate(x_plot = step_index + placement_offset)

fig3_q_axis <- make_fig3_focus_tail_axis(
  c(a_metric_q$Q_metric, b_raw$Q),
  c(a_stats_raw$Q_q90, b_class_stats_raw$Q_q75, b_overall_stats_raw$Q_q75),
  prob = .95, tail_ratio = 1.30, gutter_fraction = .10, fallback = .25
)
fig3_q_breaks_raw <- scales::breaks_extended(n = 5)(c(0, fig3_q_axis$tick_max))
fig3_q_breaks_raw <- sort(unique(c(
  0, fig3_q_breaks_raw[
    is.finite(fig3_q_breaks_raw) & fig3_q_breaks_raw >= 0 &
      fig3_q_breaks_raw <= fig3_q_axis$tick_max
  ]
)))
fig3_q_breaks <- fig3_q_axis$map(fig3_q_breaks_raw)
fig3_q_labels <- scales::label_number(
  accuracy = if (fig3_q_axis$tick_max <= .5) .05 else .1
)(fig3_q_breaks_raw)

a_metric_plot <- a_metric_q |>
  mutate(
    Q_plot = fig3_q_axis$map(Q_metric),
    atlas_row = factor(unname(fig3_row_labels[atlas_class]),
                       levels = unname(fig3_row_labels[FIG3_CLASS_ORDER])),
    dimension_pair = factor(
      unname(c(
        placement__optical = "Placement ×\noptical",
        optical__temporal = "Optical ×\ntemporal",
        placement__temporal = "Placement ×\ntemporal"
      )[pair_code]),
      levels = c("Placement ×\noptical", "Optical ×\ntemporal", "Placement ×\ntemporal")
    )
  ) |>
  arrange(atlas_class, pair_code, metric) |>
  group_by(atlas_class, pair_code) |>
  mutate(raw_y = -.145 + .070 * ((row_number() * .61803398875) %% 1)) |>
  ungroup()

a_stats <- a_stats_raw |>
  mutate(
    Q_q10 = fig3_q_axis$map(Q_q10), Q_q25 = fig3_q_axis$map(Q_q25),
    Q_median = fig3_q_axis$map(Q_median), Q_q75 = fig3_q_axis$map(Q_q75),
    Q_q90 = fig3_q_axis$map(Q_q90),
    atlas_row = factor(unname(fig3_row_labels[atlas_class]),
                       levels = unname(fig3_row_labels[FIG3_CLASS_ORDER])),
    dimension_pair = factor(
      unname(c(
        placement__optical = "Placement ×\noptical",
        optical__temporal = "Optical ×\ntemporal",
        placement__temporal = "Placement ×\ntemporal"
      )[pair_code]),
      levels = c("Placement ×\noptical", "Optical ×\ntemporal", "Placement ×\ntemporal")
    )
  )

a_density <- a_metric_plot |>
  group_by(atlas_class, atlas_row, dimension_pair) |>
  group_modify(~ {
    values <- .x$Q_plot[is.finite(.x$Q_plot)]
    if (length(values) < 3L || diff(range(values)) <= NUMERIC_TOL) {
      return(tibble(x = numeric(), density_y = numeric()))
    }
    fit <- stats::density(values, from = 0, to = fig3_q_axis$display_max,
                          n = 160, adjust = .90)
    tibble(x = fit$x, density_y = .050 + .43 * fit$y / max(fit$y))
  }) |>
  ungroup()

p3a <- ggplot() +
  {if (fig3_q_axis$use_tail) annotate(
    "rect", xmin = fig3_q_axis$focus, xmax = fig3_q_axis$display_max,
    ymin = -Inf, ymax = Inf, fill = "#F5F6F6", colour = NA
  ) else NULL} +
  {if (fig3_q_axis$use_tail) geom_vline(
    xintercept = fig3_q_axis$focus, colour = "#B9BEC1",
    linewidth = .25, linetype = "22"
  ) else NULL} +
  geom_ribbon(
    data = a_density,
    aes(x = x, ymin = .050, ymax = density_y, fill = atlas_class,
        group = interaction(atlas_class, atlas_row, dimension_pair)),
    alpha = .18, colour = NA
  ) +
  geom_line(
    data = a_density,
    aes(x = x, y = density_y, colour = atlas_class,
        group = interaction(atlas_class, atlas_row, dimension_pair)),
    linewidth = .30, alpha = .60
  ) +
  geom_point(
    data = a_metric_plot,
    aes(Q_plot, raw_y, colour = atlas_class),
    shape = 16, size = .46, alpha = .22
  ) +
  geom_segment(
    data = a_stats,
    aes(x = Q_q10, xend = Q_q90, y = -.020, yend = -.020, colour = atlas_class),
    linewidth = .28, alpha = .48, lineend = "round"
  ) +
  geom_segment(
    data = a_stats,
    aes(x = Q_q25, xend = Q_q75, y = -.020, yend = -.020, colour = atlas_class),
    linewidth = .92, alpha = .92, lineend = "round"
  ) +
  geom_point(
    data = a_stats,
    aes(Q_median, -.020, fill = atlas_class),
    shape = 21, size = 1.50, colour = "#30383C", stroke = .22
  ) +
  scale_colour_manual(values = FIG3_CLASS_COLORS, guide = "none") +
  scale_fill_manual(values = FIG3_CLASS_COLORS, guide = "none") +
  scale_x_continuous(
    limits = c(0, fig3_q_axis$display_max), breaks = fig3_q_breaks,
    labels = fig3_q_labels, expand = expansion(mult = c(0, .012))
  ) +
  scale_y_continuous(limits = c(-.19, .52), breaks = NULL,
                     expand = expansion(mult = c(0, 0))) +
  facet_grid(atlas_row ~ dimension_pair, scales = "fixed", drop = FALSE, switch = "y") +
  labs(
    title = "a  Non-additivity across representation classes",
    subtitle = "Qₘₚ = medianₜ(Qₘₚₜ); class rows ranked by pooled median Q",
    x = "Median Q per metric", y = NULL
  ) +
  theme_rq2(base_size = 5.65) +
  theme(
    panel.grid = element_blank(), panel.spacing = grid::unit(.72, "mm"),
    strip.background = element_blank(), strip.placement = "outside",
    strip.text.x = element_text(size = 4.75, face = "bold", lineheight = .86),
    strip.text.y.left = element_text(size = 4.40, angle = 0, hjust = 1,
                                     lineheight = .86, margin = margin(r = 2)),
    axis.text.x = element_text(size = 3.95), axis.title.x = element_text(size = 4.35),
    axis.line.x = element_line(colour = "#505457", linewidth = .28),
    axis.ticks.x = element_line(colour = "#505457", linewidth = .22),
    plot.title = element_text(size = 6.25, hjust = 0, margin = margin(b = 2)),
    plot.subtitle = element_text(size = 4.05, colour = "#666A6D", hjust = 0,
                                 margin = margin(t = -1, b = 2)),
    plot.margin = margin(1, 3, 1, 3)
  )

# -----------------------------------------------------------------------------
# Panel c — directional coherence with class-level second layer
# -----------------------------------------------------------------------------
coherence_points <- gamma_metric |>
  filter(is.finite(C_metric)) |>
  mutate(
    pair_code = as.character(pair_code), metric_class = as.character(metric_class),
    pair_y = unname(c(
      placement__optical = 3,
      optical__temporal = 2,
      placement__temporal = 1
    )[pair_code])
  ) |>
  arrange(pair_y, C_metric, metric) |>
  group_by(pair_code) |>
  mutate(raw_y = pair_y - .105 + .075 * (((row_number() * .61803398875) %% 1) - .5)) |>
  ungroup()

coherence_overall <- coherence_points |>
  group_by(pair_code, pair_y) |>
  summarise(
    C_q25 = safe_q(C_metric, .25), C_median = safe_median(C_metric),
    C_q75 = safe_q(C_metric, .75), .groups = "drop"
  )
fig3_c_offsets <- setNames(seq(-.205, -.355, length.out = length(FIG3_DISPLAY_CLASSES)),
                           FIG3_DISPLAY_CLASSES)
coherence_class <- coherence_points |>
  filter(metric_class %in% FIG3_DISPLAY_CLASSES) |>
  group_by(pair_code, pair_y, metric_class) |>
  summarise(
    C_q25 = safe_q(C_metric, .25), C_median = safe_median(C_metric),
    C_q75 = safe_q(C_metric, .75), .groups = "drop"
  ) |>
  mutate(y_summary = pair_y + unname(fig3_c_offsets[metric_class]))

coherence_density <- coherence_points |>
  group_by(pair_code, pair_y) |>
  group_modify(~ {
    values <- .x$C_metric[is.finite(.x$C_metric)]
    if (length(values) < 3L || diff(range(values)) <= NUMERIC_TOL) {
      return(tibble(x = numeric(), density_y = numeric()))
    }
    fit <- stats::density(values, from = -1, to = 1, n = 192, adjust = 1)
    tibble(x = fit$x, density_y = .x$pair_y[[1]] + .035 + .26 * fit$y / max(fit$y))
  }) |>
  ungroup()
coherence_polygons <- coherence_density |>
  group_by(pair_code, pair_y) |>
  group_modify(~ tibble(
    x = c(.x$x, rev(.x$x)),
    y = c(.x$density_y, rep(.x$pair_y[[1]] + .035, nrow(.x)))
  )) |>
  ungroup()

p3c <- ggplot() +
  geom_vline(xintercept = 0, linewidth = .30, colour = "#7E878B") +
  geom_polygon(
    data = coherence_polygons,
    aes(x, y, group = pair_code),
    fill = "#E5E8EA", colour = "#A5ADB1", linewidth = .25, alpha = .92
  ) +
  geom_point(
    data = coherence_points,
    aes(C_metric, raw_y, colour = metric_class),
    shape = 16, size = .50, alpha = .20
  ) +
  geom_segment(
    data = coherence_class,
    aes(x = C_q25, xend = C_q75, y = y_summary, yend = y_summary,
        colour = metric_class),
    linewidth = .58, alpha = .82, lineend = "round"
  ) +
  geom_point(
    data = coherence_class,
    aes(C_median, y_summary, colour = metric_class),
    shape = 16, size = .90, alpha = .96
  ) +
  geom_segment(
    data = coherence_overall,
    aes(x = C_q25, xend = C_q75, y = pair_y, yend = pair_y),
    linewidth = 1.15, colour = "#343B3F", lineend = "round"
  ) +
  geom_point(
    data = coherence_overall,
    aes(C_median, pair_y),
    shape = 23, size = 1.85, fill = "#343B3F", colour = "white", stroke = .22
  ) +
  scale_colour_manual(values = MS_METRIC_COLORS, guide = "none") +
  scale_x_continuous(
    limits = c(-1, 1), breaks = c(-1, -.5, 0, .5, 1),
    labels = c("−1", "−.5", "0", ".5", "+1"),
    expand = expansion(mult = c(.012, .012))
  ) +
  scale_y_continuous(
    limits = c(.56, 3.38), breaks = 3:1,
    labels = c("Placement ×\noptical", "Optical ×\ntemporal", "Placement ×\ntemporal"),
    expand = expansion(mult = c(0, 0))
  ) +
  labs(
    title = "c  Directional coherence",
    subtitle = "C = medianₜ(Rₘₚₜ / Qₘₚₜ)", x = "Directional coherence, C", y = NULL
  ) +
  theme_rq2(base_size = 5.20) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(colour = "#ECEFF0", linewidth = .17),
    axis.text.y = element_text(size = 3.80, lineheight = .84),
    axis.text.x = element_text(size = 3.75), axis.title.x = element_text(size = 4.05),
    axis.ticks.y = element_blank(), axis.line.y = element_blank(),
    plot.title = element_text(size = 6.0, hjust = 0, margin = margin(b = 2)),
    plot.subtitle = element_text(size = 3.95, colour = "#666A6D", hjust = 0,
                                 margin = margin(t = -1, b = 2)),
    plot.margin = margin(1, 2.5, 1, 2.5)
  )

# -----------------------------------------------------------------------------
# Panel b — ordered-transition backbone
# -----------------------------------------------------------------------------
b_raw_plot <- b_raw |>
  mutate(Q_plot = fig3_q_axis$map(Q))
b_class_stats <- b_class_stats_raw |>
  mutate(
    Q_q25_plot = fig3_q_axis$map(Q_q25),
    Q_median_plot = fig3_q_axis$map(Q_median),
    Q_q75_plot = fig3_q_axis$map(Q_q75)
  )
b_overall_stats <- b_overall_stats_raw |>
  mutate(
    Q_q25_plot = fig3_q_axis$map(Q_q25),
    Q_median_plot = fig3_q_axis$map(Q_median),
    Q_q75_plot = fig3_q_axis$map(Q_q75)
  )
fig3_class_x_offsets <- setNames(
  seq(-.18, .18, length.out = length(FIG3_DISPLAY_CLASSES)), FIG3_DISPLAY_CLASSES
)

make_fig3_backbone_panel <- function(pair_name, title, type = c("categorical", "ordered"),
                                     show_y = TRUE, subtitle = NULL) {
  type <- match.arg(type)
  raw <- b_raw_plot |> filter(pair_code == pair_name)
  cls <- b_class_stats |> filter(pair_code == pair_name)
  overall <- b_overall_stats |> filter(pair_code == pair_name)
  steps <- fig3_transition_order |>
    filter(pair_code == pair_name) |>
    distinct(step_index, x_label) |>
    arrange(step_index)
  if (!nrow(raw) || !nrow(overall)) stop("Missing Fig. 3 transition rows for ", pair_name)

  p <- ggplot() +
    {if (fig3_q_axis$use_tail) annotate(
      "rect", xmin = -Inf, xmax = Inf, ymin = fig3_q_axis$focus,
      ymax = fig3_q_axis$display_max, fill = "#F5F6F6", colour = NA
    ) else NULL} +
    {if (fig3_q_axis$use_tail) geom_hline(
      yintercept = fig3_q_axis$focus, colour = "#B9BEC1",
      linewidth = .25, linetype = "22"
    ) else NULL} +
    geom_point(
      data = raw,
      aes(x_plot, Q_plot, colour = metric_class),
      position = position_jitter(width = .055, height = 0, seed = 73),
      shape = 16, size = .36, alpha = .10
    )

  if (type == "categorical") {
    cls <- cls |>
      mutate(x_class = x_plot + unname(fig3_class_x_offsets[metric_class]))
    p <- p +
      geom_errorbar(
        data = cls,
        aes(x = x_class, ymin = Q_q25_plot, ymax = Q_q75_plot, colour = metric_class),
        width = .045, linewidth = .43, alpha = .72
      ) +
      geom_point(
        data = cls,
        aes(x_class, Q_median_plot, colour = metric_class),
        shape = 16, size = .92, alpha = .96
      ) +
      geom_errorbar(
        data = overall,
        aes(x = x_plot, ymin = Q_q25_plot, ymax = Q_q75_plot),
        width = .075, linewidth = 1.02, colour = "#343B3F"
      ) +
      geom_point(
        data = overall,
        aes(x_plot, Q_median_plot),
        shape = 21, size = 1.70, fill = "#343B3F", colour = "white", stroke = .22
      )
  } else {
    p <- p +
      geom_ribbon(
        data = overall,
        aes(x = x_plot, ymin = Q_q25_plot, ymax = Q_q75_plot, group = placement),
        fill = "#343B3F", alpha = .085, colour = NA
      ) +
      geom_errorbar(
        data = cls,
        aes(x = x_plot, ymin = Q_q25_plot, ymax = Q_q75_plot,
            colour = metric_class),
        width = .025, linewidth = .28, alpha = .28
      ) +
      geom_line(
        data = cls,
        aes(x_plot, Q_median_plot, colour = metric_class,
            linetype = placement, group = interaction(metric_class, placement)),
        linewidth = .52, alpha = .72
      ) +
      geom_point(
        data = cls,
        aes(x_plot, Q_median_plot, colour = metric_class),
        shape = 16, size = .88, alpha = .90
      ) +
      geom_line(
        data = overall,
        aes(x_plot, Q_median_plot, linetype = placement, group = placement),
        linewidth = 1.08, colour = "#343B3F"
      ) +
      geom_point(
        data = overall,
        aes(x_plot, Q_median_plot),
        shape = 21, size = 1.62, fill = "#343B3F", colour = "white", stroke = .22
      )
  }

  p +
    scale_colour_manual(values = MS_METRIC_COLORS, guide = "none") +
    scale_linetype_manual(values = c(all = "solid", chest = "solid", wrist = "22"),
                          guide = "none") +
    scale_x_continuous(
      limits = c(.62, max(steps$step_index) + .38),
      breaks = steps$step_index, labels = steps$x_label,
      expand = expansion(mult = c(0, 0))
    ) +
    scale_y_continuous(
      limits = c(0, fig3_q_axis$display_max), breaks = fig3_q_breaks,
      labels = if (show_y) fig3_q_labels else NULL,
      expand = expansion(mult = c(0, .012))
    ) +
    labs(title = title, subtitle = subtitle, x = NULL,
         y = if (show_y) "Non-additivity, Q" else NULL) +
    theme_rq2(base_size = 5.25) +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(colour = "#ECEFF0", linewidth = .18),
      axis.text.x = element_text(size = 3.80, lineheight = .82, margin = margin(t = 1)),
      axis.text.y = if (show_y) element_text(size = 3.85) else element_blank(),
      axis.title.y = if (show_y) element_text(size = 4.20, margin = margin(r = 2)) else element_blank(),
      axis.ticks.y = if (show_y) element_line(colour = "#505457", linewidth = .20) else element_blank(),
      axis.line.y = element_blank(), axis.line.x = element_line(colour = "#505457", linewidth = .27),
      axis.ticks.x = element_line(colour = "#505457", linewidth = .22),
      plot.title = element_text(size = 5.05, face = "bold", hjust = 0, margin = margin(b = 1)),
      plot.subtitle = element_text(size = 3.70, colour = "#666A6D", hjust = 0,
                                   margin = margin(t = -1, b = 1)),
      plot.margin = margin(0, 2.5, 0, 2.5)
    )
}

p3b_po <- make_fig3_backbone_panel(
  PAIR_CODES[[1]], "Placement × optical", type = "categorical", show_y = TRUE
)
p3b_ot <- make_fig3_backbone_panel(
  PAIR_CODES[[2]], "Optical × temporal", type = "ordered", show_y = FALSE
)
p3b_pt <- make_fig3_backbone_panel(
  PAIR_CODES[[3]], "Placement × temporal", type = "ordered", show_y = FALSE,
  subtitle = "solid = chest · dashed = wrist"
)

p3b_body <- cowplot::plot_grid(
  p3b_po, p3b_ot, p3b_pt, ncol = 3,
  rel_widths = c(.72, 1.20, 1.36), align = "hv", axis = "tblr", greedy = TRUE
)
p3b <- cowplot::ggdraw() +
  cowplot::draw_plot(p3b_body, x = 0, y = 0, width = 1, height = .90) +
  cowplot::draw_label(
    "b  Transition-specific non-additivity",
    x = .002, y = .995, hjust = 0, vjust = 1,
    size = 6.25, fontface = "bold", colour = "#151515", fontfamily = MS_FONT
  )

metric_legend_main <- cowplot::get_legend(
  ggplot(
    tibble(metric_class = factor(FIG3_DISPLAY_CLASSES, levels = FIG3_DISPLAY_CLASSES),
           x = 1, y = 1),
    aes(x, y, colour = metric_class)
  ) +
    geom_point(size = 1.10) +
    scale_colour_manual(values = MS_METRIC_COLORS, limits = FIG3_DISPLAY_CLASSES,
                        labels = str_to_sentence(FIG3_DISPLAY_CLASSES)) +
    guides(colour = guide_legend(title = NULL, nrow = 1, byrow = TRUE,
                                 override.aes = list(size = 1.10, alpha = 1))) +
    theme_void(base_family = MS_FONT) +
    theme(
      legend.position = "bottom", legend.text = element_text(size = 4.35),
      legend.key.width = grid::unit(2.5, "mm"), legend.spacing.x = grid::unit(.55, "mm"),
      legend.margin = margin(0, 0, 0, 0)
    )
)

# The global panels sit together on the first row; the ordered transition
# backbone spans the full lower width so its rise/fall profile remains legible.
p3_top <- cowplot::plot_grid(
  p3a, p3c, ncol = 2, rel_widths = c(.66, .34),
  align = "hv", axis = "tblr", greedy = TRUE
)
p3_body <- cowplot::plot_grid(
  p3_top, p3b, ncol = 1, rel_heights = c(.94, 1.06),
  align = "v", axis = "lr", greedy = TRUE
)
p3 <- cowplot::plot_grid(
  metric_legend_main, p3_body, ncol = 1, rel_heights = c(.045, 1),
  align = "v", axis = "l", greedy = TRUE
)

ms_plot_save(p3, file.path(OUT_DIR, "Fig3_RQ2.png"), 7.40, 6.70)

ms_plot_write_manifest(
  file.path(OUT_DIR, "figure_artifact_manifest.csv"),
  tibble(
    figure = "Fig3_RQ2",
    input_artifact = "rq2_gamma_summary",
    core_artifact_version = CORE_VERSION,
    rq1_analysis_version = RQ1_VERSION,
    rq2_analysis_version = RQ2_VERSION,
    rq3_analysis_version = NA_character_
  )
)

message("Fig. 3 complete: ranked class distributions, ordered non-additivity backbones, and class-resolved directional coherence.")
