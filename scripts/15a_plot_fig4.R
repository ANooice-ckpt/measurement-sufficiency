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

RQ1_SUMMARY_CSV <- file.path("results", "rq1", "rq1_pairwise_summary.csv")
OBSERVED_RDS <- file.path("results", "rq3", "rq3_sufficiency_long.rds")
SUFFICIENCY_CSV <- file.path("results", "rq3", "rq3_sufficiency_long.csv")
REQUIREMENT_CSV <- file.path("results", "rq3", "rq3_single_dimension_requirement.csv")
UNORDERED_CSV <- file.path("results", "rq3", "rq3_unordered_substitutability.csv")
COVERAGE_CSV <- file.path("results", "rq3", "rq3_unordered_coverage_curves.csv")
CONVERGENCE_CSV <- file.path("results", "rq3", "rq3_convergence_profile.csv")
JOINT_CSV <- file.path("results", "rq3", "rq3_joint_summary.csv")
PARETO_OCCUPANCY_CSV <- file.path("results", "rq3", "rq3_pareto_occupancy.csv")
OUT_DIR <- file.path("results", "rq3", "figures")
ms_plot_require_files(c(RQ1_SUMMARY_CSV, OBSERVED_RDS, SUFFICIENCY_CSV, REQUIREMENT_CSV,
                        UNORDERED_CSV, COVERAGE_CSV, CONVERGENCE_CSV, JOINT_CSV,
                        PARETO_OCCUPANCY_CSV),
                        "RQ3 v5 plotting inputs")
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

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
pareto_occupancy <- readr::read_csv(PARETO_OCCUPANCY_CSV, show_col_types = FALSE, progress = FALSE)

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
ms_plot_require_columns(pareto_occupancy,
  c("support_id", "placement", "optical", "resolution_s", "n_days", "metric",
    "epsilon_interval_start", "epsilon_interval_end", "pareto"),
  "rq3_pareto_occupancy.csv")

RQ1_VERSION <- ms_plot_one_version(c(observed$rq1_analysis_version, joint$rq1_analysis_version),
                                   "rq1_analysis_version")
RQ3_VERSION <- ms_plot_one_version(c(observed$rq3_analysis_version, joint$rq3_analysis_version),
                                   "rq3_analysis_version")
CORE_VERSION <- ms_plot_assert_core(c(observed$core_artifact_version, joint$core_artifact_version))
ms_plot_assert_prefix(RQ1_VERSION, "rq1_v5_", "rq1_analysis_version")
ms_plot_assert_prefix(RQ3_VERSION, "rq3_v5_", "rq3_analysis_version")
if (!all(sort(unique(joint$resolution_s)) %in% sort(ms_primary_temporal_s()))) {
  stop("RQ3 joint artifact contains temporal states outside the frozen primary design", call. = FALSE)
}
if (!all(sort(unique(joint$n_days)) %in% DURATION_LEVELS)) {
  stop("RQ3 joint artifact contains duration states outside the frozen primary design", call. = FALSE)
}
if (!grepl(ms_analysis_design_id(), RQ3_VERSION, fixed = TRUE)) {
  stop("RQ3 plotting inputs do not match the current frozen analysis design", call. = FALSE)
}
metric_order <- ms_metric_order(rq1_summary)

safe_median <- function(x) {
  x <- x[is.finite(x)]
  if (length(x)) median(x) else NA_real_
}
safe_q <- function(x, p) {
  x <- x[is.finite(x)]
  if (length(x)) unname(quantile(x, p, names = FALSE)) else NA_real_
}
safe_mean <- function(x) {
  x <- x[is.finite(x)]
  if (length(x)) mean(x) else NA_real_
}

theme_rq3 <- function(base_size = 6.7, legend_position = "none") {
  theme_ms_axes(base_size = base_size, legend_position = legend_position)
}

metric_legend <- ms_metric_legend(text_size = 5.35, point_size = 1.5, key_width_mm = 3.5)

# =============================================================================
# Fig. 4 — tolerance determines the minimum sufficient burden
# =============================================================================

observed_display <- observed |>
  filter(dimension %in% ORDERED_DIMS, status == "resolved", is.finite(R_obs), is.finite(requirement_rank)) |>
  mutate(
    metric_class = factor(metric_class, levels = METRIC_CLASSES),
    dimension = factor(dimension, levels = ORDERED_DIMS, labels = unname(ORDERED_TITLES[ORDERED_DIMS])),
    class_offset = ms_class_offset(metric_class, span = .50, classes = METRIC_CLASSES),
    x_pos = requirement_rank + class_offset
  )

# a. Re-evaluate each metric on the pooled observed epsilon breakpoints within
# dimension. This is a display inversion of the existing R_obs thresholds only;
# it does not create a new sufficiency estimand.
requirement_grid <- bind_rows(lapply(ORDERED_DIMS, function(dim) {
  d <- observed |>
    filter(dimension == dim, status == "resolved", is.finite(R_obs), is.finite(requirement_rank))
  eps <- sort(unique(c(0, d$R_obs[is.finite(d$R_obs)])))
  metrics <- d |> distinct(metric, metric_class)
  bind_rows(lapply(seq_len(nrow(metrics)), function(i) {
    m <- metrics$metric[[i]]
    mc <- metrics$metric_class[[i]]
    g <- d |> filter(metric == m)
    rank_at <- vapply(eps, function(e) {
      ok <- g$requirement_rank[g$R_obs <= e + NUMERIC_TOL]
      if (length(ok)) min(ok) else NA_real_
    }, numeric(1))
    tibble(
      dimension = dim, metric = m, metric_class = mc,
      epsilon = eps, least_rank = rank_at
    )
  }))
})) |>
  mutate(metric_class = factor(metric_class, levels = METRIC_CLASSES))

requirement_summary <- requirement_grid |>
  group_by(dimension, metric_class, epsilon) |>
  summarise(
    n_metrics = n_distinct(metric),
    coverage = mean(is.finite(least_rank)),
    rank_median = safe_median(least_rank),
    rank_q25 = safe_q(least_rank, .25),
    rank_q75 = safe_q(least_rank, .75),
    .groups = "drop"
  ) |>
  mutate(
    dimension = factor(dimension, levels = ORDERED_DIMS,
                       labels = unname(ORDERED_TITLES[ORDERED_DIMS]))
  )

epsilon_values <- c(requirement_summary$epsilon, unordered$epsilon_entry)
epsilon_values <- epsilon_values[is.finite(epsilon_values) & epsilon_values >= 0]
epsilon_limit <- if (length(epsilon_values)) max(epsilon_values) * 1.02 else 1
if (!is.finite(epsilon_limit) || epsilon_limit <= 0) epsilon_limit <- 1

resolved_coverage <- requirement_grid |>
  group_by(dimension, epsilon) |>
  summarise(
    n_metrics = n_distinct(metric),
    coverage = mean(is.finite(least_rank)),
    .groups = "drop"
  ) |>
  mutate(
    dimension = factor(dimension, levels = ORDERED_DIMS,
                       labels = unname(ORDERED_TITLES[ORDERED_DIMS]))
  )

# b. Distribution of the empirical entry threshold R_obs at each ordered state.
observed_summary <- observed_display |>
  group_by(dimension, requirement_rank, metric_class) |>
  summarise(
    n_metrics = n_distinct(metric),
    R_median = median(R_obs, na.rm = TRUE),
    R_q25 = quantile(R_obs, .25, na.rm = TRUE, names = FALSE),
    R_q75 = quantile(R_obs, .75, na.rm = TRUE, names = FALSE),
    .groups = "drop"
  ) |>
  mutate(
    metric_class = factor(metric_class, levels = METRIC_CLASSES),
    class_offset = ms_class_offset(metric_class, span = .50, classes = METRIC_CLASSES),
    x_pos = requirement_rank + class_offset
  )
FIG4B_MAX_RESOLVED_RANK <- max(observed_summary$requirement_rank, na.rm = TRUE)

# c. Unordered dimensions are empirical substitutability curves. Reconstruct the
# all-metric ECDF directly from epsilon_entry so metric-class-specific source
# curves do not get accidentally connected together.
pair_ecdf <- unordered |>
  filter(dimension %in% c("placement", "optical"), is.finite(epsilon_entry)) |>
  mutate(pair = paste(config_a_label, "→", config_b_label)) |>
  group_by(dimension, comparison_pair_id, pair) |>
  group_modify(function(g, key) {
    eps <- sort(unique(c(0, g$epsilon_entry[is.finite(g$epsilon_entry)])))
    tibble(
      epsilon = eps,
      fraction_metrics_substitutable = vapply(
        eps, function(e) mean(g$epsilon_entry <= e + NUMERIC_TOL, na.rm = TRUE), numeric(1)
      )
    )
  }) |>
  ungroup() |>
  mutate(dimension = factor(dimension, levels = c("placement", "optical"),
                            labels = c("Placement", "Optical representation")))

pair_levels <- unique(pair_ecdf$pair)
pair_palette <- if (length(pair_levels) <= length(MS_THREE_COLORS)) {
  setNames(MS_THREE_COLORS[seq_along(pair_levels)], pair_levels)
} else {
  setNames(grDevices::hcl.colors(length(pair_levels), palette = "Dark 3"), pair_levels)
}
readr::write_csv(
  resolved_coverage |>
    mutate(dimension = as.character(dimension)),
  file.path("results", "rq3", "fig4_sufficient_metric_coverage.csv"), na = ""
)

readr::write_csv(
  requirement_summary |>
    mutate(metric_class = as.character(metric_class), dimension = as.character(dimension)),
  file.path("results", "rq3", "fig4_minimum_requirement_summary.csv"), na = ""
)
readr::write_csv(observed_display,
                 file.path("results", "rq3", "fig4_observed_stability.csv"), na = "")
readr::write_csv(pair_ecdf,
                 file.path("results", "rq3", "fig4_unordered_substitutability_ecdf.csv"), na = "")


FIG5_TOLERANCE_SLICES <- c(.10, .25, .50, .75)

# -----------------------------------------------------------------------------
# Main-text display refinement for Fig. 4.
# Tolerance spans close to an order of magnitude and includes zero. A log1p axis
# preserves zero and uses familiar logarithmic compression. Each tolerance facet
# uses its own observed x-range so the shorter placement/optical and duration
# domains do not inherit the temporal long tail. Breaks remain explicit so the
# decision-relevant low-tolerance region is not left with only a few automatic
# ticks.
# -----------------------------------------------------------------------------
epsilon_log1p <- scales::trans_new(
  name = "log1p",
  transform = base::log1p,
  inverse = base::expm1,
  domain = c(0, Inf)
)
epsilon_tick_candidates <- c(0, .1, .2, .5, 1, 2, 3, 5, 7, 10)
epsilon_tick_labels <- c("0", "0.1", "0.2", "0.5", "1", "2", "3", "5", "7", "10")
epsilon_tick_keep <- epsilon_tick_candidates <= epsilon_limit + NUMERIC_TOL
epsilon_ticks <- epsilon_tick_candidates[epsilon_tick_keep]
epsilon_labels <- epsilon_tick_labels[epsilon_tick_keep]

FIG4_DIM_LABELS <- c(
  "Temporal resolution" = paste0("Temporal resolution · ", RES_LABELS[[1]], " → ", tail(RES_LABELS, 1)),
  "Monitoring duration" = paste0("Monitoring duration · ", min(DURATION_LEVELS), " d → ", max(DURATION_LEVELS), " d")
)
fig5_slice_guides <- FIG5_TOLERANCE_SLICES[FIG5_TOLERANCE_SLICES <= epsilon_limit + NUMERIC_TOL]

# a. Class-wise distribution ribbons. Each metric contributes one minimum
# sufficient state at each pooled observed R_obs breakpoint. The threshold-like
# check is retained from the frozen RQ3 requirement rule: a non-threshold-like
# sufficient set is displayed as unresolved rather than being forced into a
# least-demanding state. This is a display inversion of frozen R_obs values only;
# it does not create a new sufficiency estimand.
p4a_class_order <- c(
  "timing", "duration", "level", "temporal dynamics", "exposure history", "spectrum"
)
p4a_metric_inventory <- rq1_summary |>
  filter(dimension %in% ORDERED_DIMS) |>
  distinct(dimension, metric, metric_class) |>
  mutate(metric_class = as.character(metric_class))

p4a_class_counts <- p4a_metric_inventory |>
  group_by(metric_class) |>
  summarise(n_metrics = n_distinct(metric), .groups = "drop")
if (!setequal(p4a_class_counts$metric_class, p4a_class_order)) {
  stop("Panel a metric classes do not match the frozen six-class inventory", call. = FALSE)
}

p4a_row_map <- tibble(
  row_key = c("overall", p4a_class_order),
  row_y = c(7.20, 6:1),
  row_label = c(
    "Overall",
    paste0(
      str_to_sentence(p4a_class_order),
      " (n=",
      p4a_class_counts$n_metrics[match(p4a_class_order, p4a_class_counts$metric_class)],
      ")"
    )
  )
)

p4a_state_labels <- list(
  temporal = RES_LABELS,
  duration = paste0(DURATION_LEVELS, " d")
)
p4a_state_key <- function(dimension, rank) {
  if (!is.finite(rank)) return("unresolved / no resolved sufficient state")
  prefix <- if (identical(dimension, "temporal")) "Temporal: " else "Duration: "
  paste0(prefix, p4a_state_labels[[dimension]][[as.integer(rank)]])
}

p4a_min_rank <- function(g, epsilon) {
  z <- g |>
    filter(status == "resolved", is.finite(R_obs), is.finite(requirement_rank)) |>
    arrange(requirement_rank)
  if (!nrow(z)) return(NA_real_)

  is_sufficient <- z$R_obs <= epsilon + NUMERIC_TOL
  # The RQ3 estimand reports a least-demanding state only when the resolved
  # sufficient set is threshold-like in increasing burden order.
  if (!any(is_sufficient) || any(diff(as.integer(is_sufficient)) < 0L)) {
    return(NA_real_)
  }
  min(z$requirement_rank[is_sufficient])
}

p4a_epsilon_limit <- max(
  c(.75, observed$R_obs[observed$dimension %in% ORDERED_DIMS & is.finite(observed$R_obs)]),
  na.rm = TRUE
)
if (!is.finite(p4a_epsilon_limit) || p4a_epsilon_limit <= 0) p4a_epsilon_limit <- 1
p4a_epsilon_limit <- p4a_epsilon_limit * 1.02

p4a_epsilon_intervals <- bind_rows(lapply(ORDERED_DIMS, function(dim) {
  d <- observed |>
    filter(dimension == dim, status == "resolved", is.finite(R_obs))
  tibble(
    dimension = dim,
    epsilon = sort(unique(c(0, d$R_obs, p4a_epsilon_limit)))
  )
})) |>
  group_by(dimension) |>
  arrange(epsilon, .by_group = TRUE) |>
  mutate(epsilon_end = lead(epsilon)) |>
  ungroup() |>
  filter(is.finite(epsilon_end), epsilon_end > epsilon + NUMERIC_TOL)

p4a_metric_states <- bind_rows(lapply(ORDERED_DIMS, function(dim) {
  metrics <- p4a_metric_inventory |>
    filter(dimension == dim) |>
    arrange(match(metric_class, p4a_class_order), metric)
  eps <- p4a_epsilon_intervals |>
    filter(dimension == dim) |>
    pull(epsilon)
  bind_rows(lapply(seq_len(nrow(metrics)), function(i) {
    m <- metrics$metric[[i]]
    g <- observed |>
      filter(dimension == dim, metric == m)
    tibble(
      dimension = dim,
      metric = m,
      metric_class = metrics$metric_class[[i]],
      epsilon = eps,
      minimum_rank = vapply(eps, function(e) p4a_min_rank(g, e), numeric(1))
    )
  }))
})) |>
  mutate(row_key = metric_class)
p4a_metric_states$state_key <- vapply(
  seq_len(nrow(p4a_metric_states)),
  function(i) p4a_state_key(
    p4a_metric_states$dimension[[i]], p4a_metric_states$minimum_rank[[i]]
  ),
  character(1)
)

# Reuse the same per-metric state assignments for the pooled Overall row. This
# preserves equal metric weighting while keeping the six class rows separate.
p4a_metric_states <- bind_rows(
  p4a_metric_states,
  p4a_metric_states |>
    mutate(metric_class = NA_character_, row_key = "overall")
)

p4a_state_order <- c(
  paste0("Temporal: ", p4a_state_labels$temporal),
  paste0("Duration: ", p4a_state_labels$duration),
  "unresolved / no resolved sufficient state"
)

p4a_ribbon_data <- p4a_metric_states |>
  left_join(p4a_row_map |> select(row_key, row_y), by = "row_key") |>
  left_join(p4a_epsilon_intervals, by = c("dimension", "epsilon")) |>
  mutate(state_key = factor(state_key, levels = p4a_state_order)) |>
  group_by(dimension, row_key, row_y, epsilon, epsilon_end, state_key) |>
  summarise(n_metrics = n_distinct(metric), .groups = "drop") |>
  group_by(dimension, row_key, row_y, epsilon, epsilon_end) |>
  mutate(
    fraction = n_metrics / sum(n_metrics),
    ribbon_ymin = row_y - .39 + .78 * lag(cumsum(fraction), default = 0),
    ribbon_ymax = ribbon_ymin + .78 * fraction
  ) |>
  ungroup() |>
  mutate(
    dimension = factor(
      dimension, levels = ORDERED_DIMS,
      labels = unname(c(temporal = "Temporal resolution", duration = "Monitoring duration"))
    )
  )

p4a_burden_palette <- c("#E8F0F3", "#C8DCE4", "#A5C5D1", "#7FA8B9", "#568BA1", "#2F5D7E")
p4a_fill_values <- c(
  setNames(p4a_burden_palette, paste0("Temporal: ", p4a_state_labels$temporal)),
  setNames(p4a_burden_palette, paste0("Duration: ", p4a_state_labels$duration)),
  "unresolved / no resolved sufficient state" = "#BFC5C8"
)
p4a_fill_breaks <- c(
  paste0("Temporal: ", p4a_state_labels$temporal),
  paste0("Duration: ", p4a_state_labels$duration),
  "unresolved / no resolved sufficient state"
)

p4a_ticks <- c(0, .10, .25, .50, .75, 1, 2, 3, 5)
p4a_tick_keep <- p4a_ticks <= p4a_epsilon_limit + NUMERIC_TOL
p4a_ticks <- p4a_ticks[p4a_tick_keep]
p4a_tick_labels <- c("0", "0.10", "0.25", "0.50", "0.75", "1", "2", "3", "5")[p4a_tick_keep]

p4a <- ggplot() +
  geom_rect(
    data = p4a_ribbon_data,
    aes(
      xmin = epsilon, xmax = epsilon_end,
      ymin = ribbon_ymin, ymax = ribbon_ymax, fill = state_key
    ),
    colour = NA
  ) +
  geom_vline(
    xintercept = c(.10, .25, .50, .75),
    linewidth = .20, linetype = 3, colour = "#BFC5C8", alpha = .72
  ) +
  facet_grid(. ~ dimension, scales = "fixed", space = "fixed") +
  scale_fill_manual(
    values = p4a_fill_values, breaks = p4a_fill_breaks,
    drop = FALSE, name = "requirement state\n(darker = higher burden)"
  ) +
  scale_x_continuous(
    trans = epsilon_log1p,
    limits = c(0, p4a_epsilon_limit),
    breaks = p4a_ticks, labels = p4a_tick_labels,
    expand = expansion(mult = c(0, 0))
  ) +
  scale_y_continuous(
    breaks = p4a_row_map$row_y, labels = p4a_row_map$row_label,
    limits = c(.48, 7.62), expand = expansion(mult = c(0, 0))
  ) +
  labs(
    title = "a  Tolerance sets the minimum sufficient measurement burden",
    subtitle = "100% stacked distribution of minimum sufficient states within each metric group; grey = unresolved / no resolved sufficient state",
    x = "tolerance epsilon", y = NULL
  ) +
  theme_rq3(base_size = 6.0, legend_position = "bottom") +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(colour = "#F0F1F2", linewidth = .18),
    panel.spacing = grid::unit(4.5, "mm"),
    axis.text.x = element_text(size = 4.65),
    axis.text.y = element_text(size = 4.75, hjust = 1),
    axis.ticks.x = element_line(colour = "#505457", linewidth = .25),
    strip.text = element_text(size = 6.0),
    plot.subtitle = element_text(size = 4.45, colour = "#666A6D", margin = margin(t = -1, b = 2)),
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.text = element_text(size = 4.15),
    legend.title = element_text(size = 4.25),
    legend.key.width = grid::unit(6.3, "mm"),
    legend.key.height = grid::unit(2.7, "mm"),
    legend.spacing.x = grid::unit(2.3, "mm"),
    plot.margin = margin(2, 3, 0, 3)
  ) +
  guides(fill = guide_legend(nrow = 2, byrow = TRUE, override.aes = list(colour = NA)))

# R_obs stays on its original linear scale; only the background raw points carry
# the long tail. The highest observed boundary is unresolved and omitted, so the
# rank axis ends at the highest resolved observed requirement.
p4b <- ggplot() +
  geom_point(
    data = observed_display,
    aes(x_pos, R_obs, color = metric_class),
    position = position_jitter(width = .018, height = 0, seed = 91),
    size = .52, alpha = .16
  ) +
  geom_linerange(
    data = observed_summary,
    aes(x_pos, ymin = R_q25, ymax = R_q75, color = metric_class),
    linewidth = .42, alpha = .46
  ) +
  geom_point(
    data = observed_summary,
    aes(x_pos, R_median, color = metric_class),
    shape = 18, size = 1.6
  ) +
  facet_wrap(~dimension, nrow = 1, labeller = as_labeller(FIG4_DIM_LABELS)) +
  scale_color_ms_metric(guide = "none") +
  scale_x_continuous(
    breaks = seq_len(FIG4B_MAX_RESOLVED_RANK),
    limits = c(.65, FIG4B_MAX_RESOLVED_RANK + .35),
    labels = as.character(seq_len(FIG4B_MAX_RESOLVED_RANK))
  ) +
  scale_y_continuous(breaks = scales::breaks_extended(n = 5)) +
  labs(
    title = "b  Residual instability contracts as measurement burden increases",
    subtitle = "highest observed boundary is unresolved and omitted",
    x = "requirement rank (low → high burden)", y = "R_obs = max A to higher observed states"
  ) +
  theme_rq3(base_size = 6.35) +
  theme(
    panel.grid.major.x = element_blank(), strip.text = element_text(size = 5.9),
    plot.subtitle = element_text(size = 4.8, colour = "#666A6D", margin = margin(t = -1, b = 2))
  )

pair_e50 <- pair_ecdf |>
  group_by(dimension, comparison_pair_id, pair) |>
  summarise(
    epsilon50 = if (any(fraction_metrics_substitutable >= .5)) {
      min(epsilon[fraction_metrics_substitutable >= .5], na.rm = TRUE)
    } else NA_real_,
    .groups = "drop"
  ) |>
  filter(is.finite(epsilon50))

p4c <- ggplot(
  pair_ecdf,
  aes(
    epsilon, fraction_metrics_substitutable,
    color = pair,
    group = interaction(dimension, comparison_pair_id, drop = TRUE)
  )
) +
  geom_vline(xintercept = fig5_slice_guides, linewidth = .22, linetype = 3, color = "#C5C9CC") +
  geom_hline(yintercept = .5, linewidth = .24, linetype = 3, color = "#B4B8BB") +
  geom_step(linewidth = .76, alpha = .94, lineend = "butt", linejoin = "mitre") +
  geom_point(
    data = pair_e50, aes(epsilon50, .5, color = pair),
    inherit.aes = FALSE, shape = 21, fill = "white", size = 1.35, stroke = .45
  ) +
  facet_wrap(~dimension, nrow = 1, scales = "free_x") +
  scale_color_manual(values = pair_palette, breaks = pair_levels, name = NULL) +
  scale_x_continuous(
    trans = epsilon_log1p,
    breaks = epsilon_ticks,
    labels = epsilon_labels,
    expand = expansion(mult = c(0, .01))
  ) +
  scale_y_continuous(limits = c(0, 1), labels = scales::label_percent(accuracy = 25)) +
  labs(
    title = "c  Target-aligned alternatives become substitutable as tolerance relaxes",
    subtitle = "open points = ε50; faint vertical guides = Fig. 5 tolerance slices",
    x = "tolerance ε", y = "fraction of metrics substitutable"
  ) +
  theme_rq3(base_size = 6.3, legend_position = "bottom") +
  theme(
    panel.grid.major.x = element_line(colour = "#ECEFF0", linewidth = .20),
    strip.text = element_text(size = 5.8),
    plot.subtitle = element_text(size = 4.45, colour = "#666A6D", margin = margin(t = -1, b = 1.5)),
    legend.text = element_text(size = 5.0),
    legend.key.width = grid::unit(5.0, "mm")
  )

fig4_bottom <- cowplot::plot_grid(
  p4b, p4c, ncol = 2, rel_widths = c(1.08, .92),
  align = "hv", axis = "tblr", greedy = TRUE
)
fig4_body <- cowplot::plot_grid(
  p4a, fig4_bottom, ncol = 1, rel_heights = c(1.20, .80),
  align = "v", axis = "l", greedy = TRUE
)
fig4 <- cowplot::plot_grid(
  metric_legend, fig4_body, ncol = 1,
  rel_heights = c(.042, 1), align = "v", axis = "l", greedy = TRUE
)
ms_plot_save(fig4, file.path(OUT_DIR, "Fig4_RQ3.pdf"), 9.0, 6.2)
ms_plot_save(fig4, file.path(OUT_DIR, "Fig4_RQ3.png"), 9.0, 6.2)
readr::write_csv(pair_e50, file.path("results", "rq3", "fig4_unordered_epsilon50.csv"), na = "")

message("RQ3 v5 figures complete: single-dimension sufficiency and joint tolerance landscapes")

ms_plot_write_manifest(
  file.path(OUT_DIR, "figure_artifact_manifest.csv"),
  tibble(
    figure = 'Fig4_RQ3',
    input_artifact = 'rq3_observed_stability+sufficiency+unordered_substitutability',
    core_artifact_version = CORE_VERSION,
    rq1_analysis_version = RQ1_VERSION,
    rq2_analysis_version = NA_character_,
    rq3_analysis_version = RQ3_VERSION
  )
)
