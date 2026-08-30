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
CONVERGENCE_CSV <- file.path("results", "rq3", "rq3_convergence_profile.csv")
JOINT_CSV <- file.path("results", "rq3", "rq3_joint_summary.csv")
OUT_DIR <- file.path("results", "rq3", "figures")
ms_plot_require_files(c(RQ1_SUMMARY_CSV, OBSERVED_RDS, SUFFICIENCY_CSV, REQUIREMENT_CSV,
                        UNORDERED_CSV, CONVERGENCE_CSV, JOINT_CSV),
                        "RQ3 v5 plotting inputs")
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

METRIC_CLASSES <- MS_METRIC_CLASSES
ORDERED_DIMS <- c("temporal", "duration")
ORDERED_TITLES <- c(temporal = "Temporal resolution", duration = "Monitoring duration")
RES_LEVELS <- rev(ms_primary_temporal_s())
RES_LABELS <- ms_temporal_label(RES_LEVELS)
DURATION_LEVELS <- ms_primary_duration_days()
ORDERED_MAX_RANK <- max(length(RES_LEVELS), length(DURATION_LEVELS))
FIG5_TOLERANCE_SLICES <- c(.10, .25, .50, .75)
NUMERIC_TOL <- 1e-12

rq1_summary <- readr::read_csv(RQ1_SUMMARY_CSV, show_col_types = FALSE, progress = FALSE)
observed <- readRDS(OBSERVED_RDS)
sufficiency <- readr::read_csv(SUFFICIENCY_CSV, show_col_types = FALSE, progress = FALSE)
requirement <- readr::read_csv(REQUIREMENT_CSV, show_col_types = FALSE, progress = FALSE)
unordered <- readr::read_csv(UNORDERED_CSV, show_col_types = FALSE, progress = FALSE)
convergence <- readr::read_csv(CONVERGENCE_CSV, show_col_types = FALSE, progress = FALSE)
joint <- readr::read_csv(JOINT_CSV, show_col_types = FALSE, progress = FALSE)

ms_plot_require_columns(rq1_summary, c("core_artifact_version", "rq1_analysis_version", "metric", "metric_class", "dimension", "A_mean_absolute"),
                        "rq1_pairwise_summary.csv")
ms_plot_require_columns(observed,
  c("core_artifact_version", "rq1_analysis_version", "rq3_analysis_version", "dimension",
    "metric", "metric_class", "state_id", "state_label", "requirement_rank", "R_obs", "status"),
  "rq3_sufficiency_long.rds")
ms_plot_require_columns(sufficiency,
  c("core_artifact_version", "rq1_analysis_version", "rq3_analysis_version",
    "dimension", "metric", "metric_class", "epsilon", "sufficient", "status"),
  "rq3_sufficiency_long.csv")
ms_plot_require_columns(requirement,
  c("dimension", "metric", "epsilon", "sufficient_states", "sufficient_set_threshold_like",
    "least_demanding_sufficient_state"),
  "rq3_single_dimension_requirement.csv")
ms_plot_require_columns(unordered,
  c("core_artifact_version", "rq1_analysis_version", "rq3_analysis_version",
    "dimension", "comparison_pair_id", "config_a_label", "config_b_label", "metric", "metric_class",
    "orientation_type", "epsilon_entry", "A", "B"),
  "rq3_unordered_substitutability.csv")
ms_plot_require_columns(convergence,
  c("core_artifact_version", "rq1_analysis_version", "rq3_analysis_version",
    "dimension", "metric", "metric_class", "G", "requirement_position", "boundary_proximity"),
  "rq3_convergence_profile.csv")
ms_plot_require_columns(joint,
  c("core_artifact_version", "rq1_analysis_version", "rq3_analysis_version", "support_id", "placement",
    "optical", "resolution_s", "n_days", "metric", "status", "epsilon_entry"),
  "rq3_joint_summary.csv")

RQ1_VERSION <- ms_plot_one_version(c(
  rq1_summary$rq1_analysis_version, observed$rq1_analysis_version,
  sufficiency$rq1_analysis_version, unordered$rq1_analysis_version,
  convergence$rq1_analysis_version, joint$rq1_analysis_version
), "rq1_analysis_version")
RQ3_VERSION <- ms_plot_one_version(c(
  observed$rq3_analysis_version, sufficiency$rq3_analysis_version,
  unordered$rq3_analysis_version, convergence$rq3_analysis_version,
  joint$rq3_analysis_version
), "rq3_analysis_version")
CORE_VERSION <- ms_plot_assert_core(c(
  rq1_summary$core_artifact_version, observed$core_artifact_version,
  sufficiency$core_artifact_version, unordered$core_artifact_version,
  convergence$core_artifact_version, joint$core_artifact_version
))
ms_plot_assert_prefix(RQ1_VERSION, "rq1_v5_", "rq1_analysis_version")
ms_plot_assert_prefix(RQ3_VERSION, "rq3_v5_", "rq3_analysis_version")
if (!grepl(ms_analysis_design_id(), RQ3_VERSION, fixed = TRUE)) {
  stop("RQ3 plotting inputs do not match the current frozen analysis design", call. = FALSE)
}
joint_resolutions <- sort(unique(as.integer(joint$resolution_s[is.finite(joint$resolution_s)])))
joint_days <- sort(unique(as.integer(joint$n_days[is.finite(joint$n_days)])))
if (!identical(joint_resolutions, sort(as.integer(ms_primary_temporal_s())))) {
  stop("RQ3 joint artifact does not contain exactly the frozen primary temporal states", call. = FALSE)
}
if (!identical(joint_days, sort(as.integer(DURATION_LEVELS)))) {
  stop("RQ3 joint artifact does not contain exactly the frozen primary duration states", call. = FALSE)
}
joint_configuration_states <- joint |>
  distinct(resolution_s, n_days) |>
  mutate(resolution_s = as.integer(resolution_s), n_days = as.integer(n_days))
expected_joint_configuration_states <- tidyr::crossing(
  resolution_s = as.integer(ms_primary_temporal_s()),
  n_days = as.integer(DURATION_LEVELS)
)
missing_joint_configuration_states <- dplyr::anti_join(
  expected_joint_configuration_states, joint_configuration_states,
  by = c("resolution_s", "n_days")
)
if (nrow(missing_joint_configuration_states)) {
  stop("RQ3 joint artifact is missing one or more frozen temporal-duration configurations", call. = FALSE)
}
temporal_ranks <- sort(unique(as.integer(observed$requirement_rank[
  observed$dimension == "temporal" & is.finite(observed$requirement_rank)
])))
duration_ranks <- sort(unique(as.integer(observed$requirement_rank[
  observed$dimension == "duration" & is.finite(observed$requirement_rank)
])))
if (!identical(temporal_ranks, seq_along(RES_LEVELS)) ||
    !identical(duration_ranks, seq_along(DURATION_LEVELS))) {
  stop("RQ3 ordered artifact does not contain exactly the frozen requirement ranks", call. = FALSE)
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
  metrics <- observed |> filter(dimension == dim) |> distinct(metric, metric_class)
  bind_rows(lapply(seq_len(nrow(metrics)), function(i) {
    m <- metrics$metric[[i]]
    mc <- metrics$metric_class[[i]]
    g <- d |> filter(metric == m)
    rank_at <- vapply(eps, function(e) {
      resolved <- g |> arrange(requirement_rank)
      ok <- resolved$R_obs <= e + NUMERIC_TOL
      threshold_like <- length(ok) < 2L || all(diff(as.integer(ok)) >= 0L)
      if (threshold_like && any(ok)) min(resolved$requirement_rank[ok]) else NA_real_
    }, numeric(1))
    threshold_like_at <- vapply(eps, function(e) {
      resolved <- g |> arrange(requirement_rank)
      ok <- resolved$R_obs <= e + NUMERIC_TOL
      length(ok) < 2L || all(diff(as.integer(ok)) >= 0L)
    }, logical(1))
    tibble(
      dimension = dim, metric = m, metric_class = mc,
      epsilon = eps, display_threshold_like = threshold_like_at, least_rank = rank_at
    )
  }))
})) |>
  mutate(metric_class = factor(metric_class, levels = METRIC_CLASSES))

canonical_requirement_check <- requirement |>
  filter(dimension %in% ORDERED_DIMS) |>
  left_join(
    observed |>
      filter(dimension %in% ORDERED_DIMS) |>
      distinct(dimension, metric, state_id, requirement_rank),
    by = c("dimension", "metric", "least_demanding_sufficient_state" = "state_id")
  ) |>
  transmute(
    dimension, metric, epsilon,
    canonical_threshold_like = sufficient_set_threshold_like,
    canonical_least_rank = requirement_rank
  )

requirement_native_check <- requirement_grid |>
  inner_join(canonical_requirement_check, by = c("dimension", "metric", "epsilon")) |>
  mutate(
    threshold_agrees = display_threshold_like == canonical_threshold_like,
    rank_agrees = (is.na(least_rank) & is.na(canonical_least_rank)) |
      (is.finite(least_rank) & is.finite(canonical_least_rank) &
         abs(least_rank - canonical_least_rank) <= NUMERIC_TOL)
  )
if (nrow(requirement_native_check) != nrow(canonical_requirement_check) ||
    any(is.na(requirement_native_check$threshold_agrees)) ||
    any(is.na(requirement_native_check$rank_agrees)) ||
    any(!requirement_native_check$threshold_agrees | !requirement_native_check$rank_agrees)) {
  stop("Fig. 4 display inversion disagrees with the canonical RQ3 threshold-like requirement", call. = FALSE)
}

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
# -----------------------------------------------------------------------------
# Main-text display refinement for Fig. 4.
# Tolerance spans close to an order of magnitude and includes zero. A log1p axis
# preserves zero, uses familiar logarithmic compression, and keeps all panels on
# exactly the same tolerance scale. Breaks are explicit so the decision-relevant
# low-tolerance region is not left with only a few automatic ticks.
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

p4a_main <- ggplot(requirement_summary, aes(epsilon, rank_median, color = metric_class)) +
  geom_vline(xintercept = fig5_slice_guides, linewidth = .22, linetype = 3, color = "#C5C9CC") +
  geom_step(aes(y = rank_q25, group = metric_class), linewidth = .34, alpha = .24) +
  geom_step(aes(y = rank_q75, group = metric_class), linewidth = .34, alpha = .24) +
  geom_step(aes(group = metric_class), linewidth = .82, alpha = .96) +
  facet_wrap(~dimension, nrow = 1, labeller = as_labeller(FIG4_DIM_LABELS)) +
  scale_color_ms_metric(guide = "none") +
  scale_x_continuous(
    trans = epsilon_log1p,
    breaks = epsilon_ticks,
    labels = epsilon_labels,
    expand = expansion(mult = c(0, .01))
  ) +
  coord_cartesian(xlim = c(0, epsilon_limit), clip = "on") +
  scale_y_continuous(
    breaks = seq_len(ORDERED_MAX_RANK),
    limits = c(.8, ORDERED_MAX_RANK + .2)
  ) +
  labs(
    title = "a  Tolerance sets the minimum sufficient measurement burden",
    subtitle = "thick = class median; thin = IQR · non-threshold-like sufficient sets remain unresolved",
    x = NULL, y = "minimum sufficient requirement rank\n(low → high burden)"
  ) +
  theme_rq3(base_size = 6.6) +
  theme(
    panel.grid.major.x = element_line(colour = "#ECEFF0", linewidth = .20),
    axis.text.x = element_blank(), axis.ticks.x = element_blank(),
    strip.text = element_text(size = 6.0),
    plot.subtitle = element_text(size = 4.8, colour = "#666A6D", margin = margin(t = -1, b = 2)),
    plot.margin = margin(2, 3, 0, 3)
  )

p4a_coverage_refined <- ggplot(resolved_coverage, aes(epsilon, coverage)) +
  geom_vline(xintercept = fig5_slice_guides, linewidth = .20, linetype = 3, color = "#D0D3D5") +
  geom_step(linewidth = .48, color = "#5D6265") +
  facet_wrap(~dimension, nrow = 1, labeller = as_labeller(FIG4_DIM_LABELS)) +
  scale_x_continuous(
    trans = epsilon_log1p,
    breaks = epsilon_ticks,
    labels = epsilon_labels,
    expand = expansion(mult = c(0, .01))
  ) +
  coord_cartesian(xlim = c(0, epsilon_limit), clip = "on") +
  scale_y_continuous(
    limits = c(0, 1), breaks = c(0, .5, 1),
    labels = scales::label_percent(accuracy = 50), expand = expansion(mult = c(0, .02))
  ) +
  labs(x = "tolerance ε", y = "metrics with a resolved\nthreshold-like requirement") +
  theme_rq3(base_size = 5.65) +
  theme(
    panel.grid.major.x = element_line(colour = "#F0F1F2", linewidth = .18),
    panel.grid.minor = element_blank(),
    strip.text = element_blank(), strip.background = element_blank(),
    axis.text.x = element_text(size = 4.9), axis.text.y = element_text(size = 4.55),
    axis.title.x = element_text(size = 5.3), axis.title.y = element_text(size = 4.8),
    plot.margin = margin(0, 3, 1, 3)
  )

p4a <- cowplot::plot_grid(
  p4a_main, p4a_coverage_refined,
  ncol = 1, rel_heights = c(1, .22),
  align = "v", axis = "lr", greedy = TRUE
)

# R_obs stays on its original linear scale; only the background raw points carry
# the long tail. Rank limits are derived from the frozen design rather than a
# historical seven-state temporal lattice.
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
    breaks = seq_len(ORDERED_MAX_RANK),
    limits = c(.65, ORDERED_MAX_RANK + .35),
    labels = as.character(seq_len(ORDERED_MAX_RANK))
  ) +
  scale_y_continuous(breaks = scales::breaks_extended(n = 5)) +
  labs(
    title = "b  Residual instability across increasing measurement burden",
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
  geom_step(linewidth = .76, alpha = .94) +
  geom_point(
    data = pair_e50, aes(epsilon50, .5, color = pair),
    inherit.aes = FALSE, shape = 21, fill = "white", size = 1.35, stroke = .45
  ) +
  facet_wrap(~dimension, nrow = 1) +
  scale_color_manual(values = pair_palette, breaks = pair_levels, name = NULL) +
  scale_x_continuous(
    trans = epsilon_log1p,
    breaks = epsilon_ticks,
    labels = epsilon_labels,
    expand = expansion(mult = c(0, .01))
  ) +
  coord_cartesian(xlim = c(0, epsilon_limit), clip = "on") +
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

# -----------------------------------------------------------------------------

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

# Supplement: retain the original detailed ordered-axis trajectories as audit views.
convergence_display <- convergence |>
  filter(dimension %in% ORDERED_DIMS, is.finite(G), is.finite(requirement_position)) |>
  group_by(dimension, metric, metric_class, requirement_position) |>
  summarise(G_display = median(G, na.rm = TRUE), .groups = "drop") |>
  mutate(metric_class = factor(metric_class, levels = METRIC_CLASSES))

p4s_convergence <- ggplot(
  convergence_display,
  aes(requirement_position, G_display, group = metric, color = metric_class)
) +
  geom_line(alpha = .46, linewidth = .36) + geom_point(size = .45, alpha = .58) +
  facet_wrap(~dimension, nrow = 1, scales = "free_x") +
  scale_color_ms_metric() +
  scale_y_continuous(trans = scales::transform_asinh(), breaks = scales::breaks_extended(n = 4)) +
  labs(title = "Adjacent-transition change by metric",
       x = "requirement position", y = "G = mean |z|") +
  theme_ms(base_size = 6.1, legend_position = "bottom")

sufficiency_plot <- sufficiency |>
  filter(dimension %in% ORDERED_DIMS) |>
  group_by(dimension, metric, metric_class, epsilon) |>
  summarise(
    resolved_fraction = mean(status == "resolved", na.rm = TRUE),
    fraction_sufficient = if (any(status == "resolved")) {
      mean(sufficient[status == "resolved"], na.rm = TRUE)
    } else NA_real_,
    .groups = "drop"
  ) |>
  mutate(metric_class = factor(metric_class, levels = METRIC_CLASSES))

p4s_sufficiency <- ggplot(
  sufficiency_plot,
  aes(epsilon, fraction_sufficient, group = metric, color = metric_class)
) +
  geom_line(alpha = .46, linewidth = .36) +
  facet_wrap(~dimension, nrow = 1, scales = "free_x") +
  scale_color_ms_metric() +
  scale_x_continuous(trans = scales::transform_asinh(), breaks = scales::breaks_extended(n = 4)) +
  scale_y_continuous(limits = c(0, 1), labels = scales::label_percent(accuracy = 25)) +
  labs(title = "Observed sufficiency projection by metric",
       x = "tolerance ε", y = "fraction of resolved states sufficient") +
  theme_ms(base_size = 6.1, legend_position = "bottom")

fig4s <- cowplot::plot_grid(p4s_convergence, p4s_sufficiency, ncol = 1, rel_heights = c(1, 1))
ms_plot_save(fig4s, file.path(OUT_DIR, "FigS_RQ3_single_dimension_detail.pdf"), 12.5, 8.0)
ms_plot_save(fig4s, file.path(OUT_DIR, "FigS_RQ3_single_dimension_detail.png"), 12.5, 8.0)


# =============================================================================
# Fig. 5 — joint temporal × duration sufficiency geometry
# =============================================================================

# Main-text Fig. 5 maps the joint temporal-resolution × duration design space.
# Fig. 4 owns tolerance-response curves. Here entry tolerance is treated as an
# intrinsic property of each joint configuration; class-specific heatmaps show
# heterogeneity around the common landscape, and the lower panel shows local
# substitution/complementarity between the two burden axes.
metric_class_lookup5 <- rq1_summary |> distinct(metric, metric_class)
joint_plot_base <- joint
if (!"metric_class" %in% names(joint_plot_base)) {
  joint_plot_base <- joint_plot_base |> left_join(metric_class_lookup5, by = "metric")
}
joint_plot_base <- joint_plot_base |>
  mutate(metric_class = factor(metric_class, levels = METRIC_CLASSES)) |>
  filter(is.finite(resolution_s), is.finite(n_days))

fig5_res_levels <- sort(unique(joint_plot_base$resolution_s), decreasing = TRUE)
fig5_days <- sort(unique(joint_plot_base$n_days))
format_resolution5 <- function(x) {
  x <- as.numeric(x)
  ifelse(
    x >= 60 & abs(x / 60 - round(x / 60)) < 1e-9,
    paste0(format(round(x / 60), trim = TRUE), " min"),
    paste0(format(x, trim = TRUE), " s")
  )
}
fig5_res_labels <- format_resolution5(fig5_res_levels)

joint_plot_base <- joint_plot_base |>
  mutate(resolution_rank = match(resolution_s, fig5_res_levels))

# Equal-weight metrics: first collapse support/placement/optical facets within a
# metric, then summarize across metrics. This prevents metrics represented by
# more fixed-facet combinations from dominating the display.
joint_target_metrics <- joint_plot_base |>
  filter(!is.na(metric_class)) |>
  distinct(metric, metric_class)
if (anyDuplicated(joint_target_metrics$metric)) {
  stop("A joint-analysis metric maps to more than one representation class", call. = FALSE)
}

entry_metric_observed <- joint_plot_base |>
  filter(!is.na(metric_class)) |>
  group_by(metric, metric_class, resolution_s, resolution_rank, n_days) |>
  summarise(
    n_facets = n(),
    n_resolved_facets = sum(status == "resolved" & is.finite(epsilon_entry)),
    epsilon_metric = if (any(status == "resolved" & is.finite(epsilon_entry))) {
      median(epsilon_entry[status == "resolved" & is.finite(epsilon_entry)], na.rm = TRUE)
    } else NA_real_,
    .groups = "drop"
  )

entry_metric_surface <- tidyr::crossing(
  metric = joint_target_metrics$metric,
  resolution_rank = seq_along(fig5_res_levels),
  n_days = fig5_days
) |>
  left_join(joint_target_metrics, by = "metric") |>
  mutate(resolution_s = fig5_res_levels[resolution_rank]) |>
  left_join(
    entry_metric_observed,
    by = c("metric", "metric_class", "resolution_s", "resolution_rank", "n_days")
  )

entry_surface <- entry_metric_surface |>
  group_by(resolution_s, resolution_rank, n_days) |>
  summarise(
    n_metrics = n_distinct(metric),
    n_resolved = sum(is.finite(epsilon_metric)),
    resolved_fraction = mean(is.finite(epsilon_metric)),
    epsilon_entry_median = safe_median(epsilon_metric),
    epsilon_entry_q25 = safe_q(epsilon_metric, .25),
    epsilon_entry_q75 = safe_q(epsilon_metric, .75),
    .groups = "drop"
  )

entry_class_surface <- entry_metric_surface |>
  group_by(metric_class, resolution_s, resolution_rank, n_days) |>
  summarise(
    n_metrics = n_distinct(metric),
    n_resolved = sum(is.finite(epsilon_metric)),
    resolved_fraction = mean(is.finite(epsilon_metric)),
    epsilon_entry_median = safe_median(epsilon_metric),
    .groups = "drop"
  ) |>
  left_join(
    entry_surface |>
      select(resolution_s, resolution_rank, n_days,
             overall_entry_tolerance = epsilon_entry_median),
    by = c("resolution_s", "resolution_rank", "n_days")
  ) |>
  mutate(entry_tolerance_difference = epsilon_entry_median - overall_entry_tolerance)

entry_grid <- tidyr::crossing(
  resolution_rank = seq_along(fig5_res_levels),
  n_days = fig5_days
) |>
  left_join(
    entry_surface |>
      select(resolution_rank, n_days, n_metrics,
             epsilon_entry_median, epsilon_entry_q25, epsilon_entry_q75),
    by = c("resolution_rank", "n_days")
  )

entry_class_grid <- tidyr::crossing(
  metric_class = factor(METRIC_CLASSES, levels = METRIC_CLASSES),
  resolution_rank = seq_along(fig5_res_levels),
  n_days = fig5_days
) |>
  left_join(
    entry_class_surface |>
      select(metric_class, resolution_rank, n_days, n_metrics,
             epsilon_entry_median, overall_entry_tolerance,
             entry_tolerance_difference),
    by = c("metric_class", "resolution_rank", "n_days")
  )

entry_class_summary <- entry_class_surface |>
  group_by(metric_class) |>
  summarise(
    n_cells = sum(is.finite(entry_tolerance_difference)),
    median_delta = safe_median(entry_tolerance_difference),
    fraction_positive = if (any(is.finite(entry_tolerance_difference))) {
      mean(entry_tolerance_difference > 0, na.rm = TRUE)
    } else NA_real_,
    .groups = "drop"
  ) |>
  mutate(
    strip_label = paste0(
      as.character(metric_class), "\nmed Δε ",
      if_else(is.finite(median_delta), sprintf("%+.2f", median_delta), "NA"),
      " · ",
      if_else(is.finite(fraction_positive), paste0(round(100 * fraction_positive), "% > 0"), "NA")
    )
  )
class_strip_labels <- setNames(entry_class_summary$strip_label, as.character(entry_class_summary$metric_class))

contrast_limit <- max(abs(entry_class_grid$entry_tolerance_difference), na.rm = TRUE)
if (!is.finite(contrast_limit) || contrast_limit <= 0) contrast_limit <- .01

# a. Overall joint entry-tolerance surface. Small numeric labels make the compact
# heatmap quantitative without adding another visual channel.
p5a <- ggplot(entry_grid, aes(resolution_rank, n_days, fill = epsilon_entry_median)) +
  geom_tile(width = .92, height = .92, color = "white", linewidth = .34) +
  geom_text(
    aes(label = if_else(
      is.finite(epsilon_entry_median),
      formatC(epsilon_entry_median, format = "f", digits = 2), ""
    )),
    size = 1.75, color = "#2F3437", na.rm = TRUE
  ) +
  scale_fill_ms_sequential(
    trans = scales::transform_asinh(),
    na.value = "#ECEEEF",
    name = "entry tolerance ε"
  ) +
  scale_x_continuous(
    breaks = seq_along(fig5_res_levels), labels = fig5_res_labels,
    expand = expansion(add = .30)
  ) +
  scale_y_continuous(
    breaks = fig5_days, labels = paste0(fig5_days, " d"),
    expand = expansion(add = .30)
  ) +
  coord_fixed(ratio = .86, clip = "off") +
  labs(
    title = "a  Joint configurations differ in the tolerance required for sufficiency",
    subtitle = "fill = median entry tolerance among resolved targets; grey = no resolved target",
    x = "temporal resolution  (low → high burden)", y = "monitoring duration"
  ) +
  theme_rq3(base_size = 6.25, legend_position = "bottom") +
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_text(angle = 30, hjust = 1, size = 4.95),
    plot.subtitle = element_text(size = 4.55, colour = "#666A6D", margin = margin(t = -1, b = 2)),
    legend.text = element_text(size = 4.55),
    legend.title = element_text(size = 4.7),
    legend.key.width = grid::unit(6.8, "mm")
  )

# b. Representation-class deviations from the common entry-tolerance landscape.
# Warm cells need more permissive tolerance than overall; cool cells need less.
p5b <- ggplot(
  entry_class_grid,
  aes(resolution_rank, n_days, fill = entry_tolerance_difference)
) +
  geom_tile(width = .92, height = .92, color = "white", linewidth = .23) +
  facet_wrap(~metric_class, ncol = 3, labeller = as_labeller(class_strip_labels)) +
  scale_x_continuous(
    breaks = seq_along(fig5_res_levels), labels = fig5_res_labels,
    expand = expansion(add = .25)
  ) +
  scale_y_continuous(
    breaks = fig5_days, labels = paste0(fig5_days, " d"),
    expand = expansion(add = .25)
  ) +
  scale_fill_ms_diverging(
    max_abs = contrast_limit,
    na.value = "#F1F2F2",
    name = "class − overall\nentry tolerance ε"
  ) +
  coord_fixed(ratio = .86, clip = "off") +
  labs(
    title = "b  Representation classes deviate from the common joint landscape",
    subtitle = "warm/cool = class median relative to pooled median; grey = unresolved",
    x = "temporal burden", y = "duration"
  ) +
  theme_rq3(base_size = 5.7, legend_position = "bottom") +
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_text(angle = 36, hjust = 1, size = 4.25),
    axis.text.y = element_text(size = 4.35),
    strip.text = element_text(size = 4.85),
    plot.subtitle = element_text(size = 4.15, colour = "#666A6D", margin = margin(t = -1, b = 2)),
    panel.spacing = grid::unit(1.6, "mm"),
    legend.text = element_text(size = 4.4),
    legend.title = element_text(size = 4.45),
    legend.key.width = grid::unit(6.5, "mm")
  )

# c. Tolerance-resolved feasible regions. Each tile is the fraction of target
# metrics for which the joint configuration is confirmed sufficient at the stated
# tolerance. Unresolved metric/configuration cells therefore do not count as
# sufficient. The dark outline marks configurations that are non-dominated in
# temporal burden, duration burden, and confirmed metric coverage at that slice.

joint_metric_status <- entry_metric_surface |>
  select(metric, metric_class, resolution_s, resolution_rank, n_days, epsilon_metric)

slice_coverage <- tidyr::crossing(
  epsilon = FIG5_TOLERANCE_SLICES,
  resolution_rank = seq_along(fig5_res_levels),
  n_days = fig5_days
) |>
  left_join(
    tidyr::crossing(epsilon = FIG5_TOLERANCE_SLICES, joint_metric_status) |>
      group_by(epsilon, resolution_rank, n_days) |>
      summarise(
        n_metrics = n_distinct(metric),
        n_resolved = sum(is.finite(epsilon_metric)),
        resolved_fraction = mean(is.finite(epsilon_metric)),
        fraction_confirmed_sufficient = mean(
          is.finite(epsilon_metric) & epsilon_metric <= epsilon + NUMERIC_TOL
        ),
        .groups = "drop"
      ),
    by = c("epsilon", "resolution_rank", "n_days")
  )

slice_frontier <- slice_coverage |>
  filter(is.finite(fraction_confirmed_sufficient)) |>
  group_by(epsilon) |>
  group_modify(function(g, key) {
    keep <- vapply(seq_len(nrow(g)), function(i) {
      dominated <-
        g$resolution_rank <= g$resolution_rank[[i]] &
        g$n_days <= g$n_days[[i]] &
        g$fraction_confirmed_sufficient >=
          g$fraction_confirmed_sufficient[[i]] - NUMERIC_TOL &
        (
          g$resolution_rank < g$resolution_rank[[i]] |
          g$n_days < g$n_days[[i]] |
          g$fraction_confirmed_sufficient >
            g$fraction_confirmed_sufficient[[i]] + NUMERIC_TOL
        )
      !any(dominated, na.rm = TRUE)
    }, logical(1))
    mutate(g, frontier = keep)
  }) |>
  ungroup()

slice_plot <- slice_coverage |>
  left_join(
    slice_frontier |>
      select(epsilon, resolution_rank, n_days, frontier),
    by = c("epsilon", "resolution_rank", "n_days")
  ) |>
  mutate(
    frontier = replace_na(frontier, FALSE),
    epsilon_label = factor(
      epsilon,
      levels = FIG5_TOLERANCE_SLICES,
      labels = paste0("ε = ", format(FIG5_TOLERANCE_SLICES, trim = TRUE))
    )
  )

p5c <- ggplot(
  slice_plot,
  aes(resolution_rank, n_days, fill = fraction_confirmed_sufficient)
) +
  geom_tile(width = .92, height = .92, color = "white", linewidth = .22) +
  geom_tile(
    data = slice_plot |> filter(frontier),
    fill = NA, color = "#272B2D", linewidth = .50,
    width = .92, height = .92
  ) +
  facet_wrap(~epsilon_label, nrow = 1) +
  scale_fill_ms_sequential(
    limits = c(0, 1),
    labels = scales::label_percent(accuracy = 25),
    na.value = "#ECEEEF",
    name = "metrics confirmed\nsufficient"
  ) +
  scale_x_continuous(
    breaks = seq_along(fig5_res_levels), labels = fig5_res_labels,
    expand = expansion(add = .24)
  ) +
  scale_y_continuous(
    breaks = fig5_days, labels = paste0(fig5_days, " d"),
    expand = expansion(add = .24)
  ) +
  coord_fixed(ratio = .86, clip = "off") +
  labs(
    title = "c  Jointly sufficient regions expand as tolerance relaxes",
    subtitle = "fill = confirmed coverage among joint-analysis targets (unresolved ≠ sufficient); dark outline = coverage-efficient frontier",
    x = "temporal resolution  (low → high burden)", y = "monitoring duration"
  ) +
  theme_rq3(base_size = 5.55, legend_position = "bottom") +
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_text(angle = 38, hjust = 1, size = 4.05),
    axis.text.y = element_text(size = 4.15),
    strip.text = element_text(size = 5.0),
    plot.subtitle = element_text(size = 4.15, colour = "#666A6D", margin = margin(t = -1, b = 2)),
    panel.spacing = grid::unit(1.7, "mm"),
    legend.text = element_text(size = 4.35),
    legend.title = element_text(size = 4.4),
    legend.key.width = grid::unit(7.0, "mm")
  )

fig5_top <- cowplot::plot_grid(
  p5a, p5b, ncol = 2, rel_widths = c(.40, .60),
  align = "hv", axis = "tblr", greedy = TRUE
)
fig5_body <- cowplot::plot_grid(
  fig5_top, p5c, ncol = 1, rel_heights = c(1.10, .90),
  align = "v", axis = "l", greedy = TRUE
)
ms_plot_save(fig5_body, file.path(OUT_DIR, "Fig5_RQ3.pdf"), 9.0, 6.5)
ms_plot_save(fig5_body, file.path(OUT_DIR, "Fig5_RQ3.png"), 9.0, 6.5)

readr::write_csv(
  entry_surface,
  file.path("results", "rq3", "fig5_joint_entry_tolerance_surface.csv"), na = ""
)
readr::write_csv(
  entry_class_surface |>
    mutate(metric_class = as.character(metric_class)),
  file.path("results", "rq3", "fig5_entry_tolerance_by_class.csv"), na = ""
)
readr::write_csv(
  entry_class_grid |>
    mutate(metric_class = as.character(metric_class)),
  file.path("results", "rq3", "fig5_entry_tolerance_class_contrast.csv"), na = ""
)
readr::write_csv(
  entry_class_summary |> mutate(metric_class = as.character(metric_class)),
  file.path("results", "rq3", "fig5_entry_tolerance_class_summary.csv"), na = ""
)
readr::write_csv(
  slice_plot |>
    mutate(epsilon_label = as.character(epsilon_label)),
  file.path("results", "rq3", "fig5_tolerance_slice_coverage.csv"), na = ""
)

ms_plot_write_manifest(
  file.path(OUT_DIR, "figure_artifact_manifest.csv"),
  tibble(
    figure = c(
      "Fig4_RQ3", "Fig5_RQ3",
      "FigS_RQ3_single_dimension_detail"
    ),
    input_artifact = c(
      "rq3_observed_stability+sufficiency+unordered_substitutability",
      "rq3_joint_summary",
      "rq3_convergence_profile+sufficiency"
    ),
    core_artifact_version = CORE_VERSION,
    rq1_analysis_version = RQ1_VERSION,
    rq2_analysis_version = NA_character_,
    rq3_analysis_version = RQ3_VERSION
  )
)
message("RQ3 v5 figures complete: single-dimension sufficiency and joint tolerance landscapes")
