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

RQ1_SUMMARY_CSV <- file.path("results", "rq1", "rq1_pairwise_summary.csv")
OBSERVED_RDS <- file.path("results", "rq3", "rq3_sufficiency_long.rds")
SUFFICIENCY_CSV <- file.path("results", "rq3", "rq3_sufficiency_long.csv")
REQUIREMENT_CSV <- file.path("results", "rq3", "rq3_single_dimension_requirement.csv")
UNORDERED_CSV <- file.path("results", "rq3", "rq3_unordered_substitutability.csv")
COVERAGE_CSV <- file.path("results", "rq3", "rq3_unordered_coverage_curves.csv")
CONVERGENCE_CSV <- file.path("results", "rq3", "rq3_convergence_profile.csv")
JOINT_CSV <- file.path("results", "rq3", "rq3_joint_summary.csv")
PARETO_CSV <- file.path("results", "rq3", "rq3_pareto_frontiers.csv")
FREQUENCY_CSV <- file.path("results", "rq3", "rq3_pareto_frequency.csv")
OUT_DIR <- file.path("results", "rq3", "figures")
ms_plot_require_files(c(RQ1_SUMMARY_CSV, OBSERVED_RDS, SUFFICIENCY_CSV, REQUIREMENT_CSV,
                        UNORDERED_CSV, COVERAGE_CSV, CONVERGENCE_CSV, JOINT_CSV,
                        PARETO_CSV, FREQUENCY_CSV), "RQ3 v5 plotting inputs")
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

METRIC_CLASSES <- MS_METRIC_CLASSES
ORDERED_DIMS <- c("temporal", "duration")
ORDERED_TITLES <- c(temporal = "Temporal resolution", duration = "Monitoring duration")
RES_LEVELS <- c(1800, 900, 300, 60, 30, 20, 10)
RES_LABELS <- c("30 min", "15 min", "5 min", "1 min", "30 s", "20 s", "10 s")
NUMERIC_TOL <- 1e-12

rq1_summary <- readr::read_csv(RQ1_SUMMARY_CSV, show_col_types = FALSE, progress = FALSE)
observed <- readRDS(OBSERVED_RDS)
sufficiency <- readr::read_csv(SUFFICIENCY_CSV, show_col_types = FALSE, progress = FALSE)
requirement <- readr::read_csv(REQUIREMENT_CSV, show_col_types = FALSE, progress = FALSE)
unordered <- readr::read_csv(UNORDERED_CSV, show_col_types = FALSE, progress = FALSE)
coverage <- readr::read_csv(COVERAGE_CSV, show_col_types = FALSE, progress = FALSE)
convergence <- readr::read_csv(CONVERGENCE_CSV, show_col_types = FALSE, progress = FALSE)
joint <- readr::read_csv(JOINT_CSV, show_col_types = FALSE, progress = FALSE)
pareto <- readr::read_csv(PARETO_CSV, show_col_types = FALSE, progress = FALSE)
frequency <- readr::read_csv(FREQUENCY_CSV, show_col_types = FALSE, progress = FALSE)

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
    "optical", "resolution_s", "n_days", "metric", "status", "epsilon_entry"),
  "rq3_joint_summary.csv")
ms_plot_require_columns(pareto,
  c("support_id", "placement", "optical", "metric", "metric_class", "resolution_s", "n_days",
    "ever_pareto", "pareto_persistence"),
  "rq3_pareto_frontiers.csv")
ms_plot_require_columns(frequency,
  c("support_id", "placement", "optical", "resolution_s", "n_days",
    "fraction_metrics_ever_pareto", "mean_pareto_persistence"),
  "rq3_pareto_frequency.csv")

RQ1_VERSION <- ms_plot_one_version(c(observed$rq1_analysis_version, joint$rq1_analysis_version),
                                   "rq1_analysis_version")
RQ3_VERSION <- ms_plot_one_version(c(observed$rq3_analysis_version, joint$rq3_analysis_version),
                                   "rq3_analysis_version")
CORE_VERSION <- ms_plot_assert_core(c(observed$core_artifact_version, joint$core_artifact_version))
ms_plot_assert_prefix(RQ1_VERSION, "rq1_v5_", "rq1_analysis_version")
ms_plot_assert_prefix(RQ3_VERSION, "rq3_v5_", "rq3_analysis_version")
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

p4a_main <- ggplot(requirement_summary, aes(epsilon, rank_median, color = metric_class)) +
  geom_step(aes(y = rank_q25, group = metric_class), linewidth = .34, alpha = .24) +
  geom_step(aes(y = rank_q75, group = metric_class), linewidth = .34, alpha = .24) +
  geom_step(aes(group = metric_class), linewidth = .82, alpha = .96) +
  facet_wrap(~dimension, nrow = 1) +
  scale_color_ms_metric(guide = "none") +
  scale_x_continuous(
    trans = scales::transform_asinh(), limits = c(0, epsilon_limit),
    breaks = scales::breaks_extended(n = 4), expand = expansion(mult = c(0, .01))
  ) +
  scale_y_continuous(breaks = 1:7, limits = c(.8, 7.2)) +
  labs(
    title = "a  Tolerance sets the minimum sufficient measurement burden",
    subtitle = "thick line = class median; thin lines = interquartile range",
    x = NULL, y = "minimum sufficient requirement rank\n(low → high burden)"
  ) +
  theme_rq3(base_size = 6.6) +
  theme(
    panel.grid.major.x = element_blank(), strip.text = element_text(size = 6.2),
    axis.text.x = element_blank(), axis.ticks.x = element_blank(),
    plot.subtitle = element_text(size = 5.0, colour = "#666A6D", margin = margin(t = -1, b = 2)),
    plot.margin = margin(2, 3, 0, 3)
  )

p4a_coverage <- ggplot(resolved_coverage, aes(epsilon, coverage)) +
  geom_step(linewidth = .50, color = "#5D6265") +
  facet_wrap(~dimension, nrow = 1) +
  scale_x_continuous(
    trans = scales::transform_asinh(), limits = c(0, epsilon_limit),
    breaks = scales::breaks_extended(n = 4), expand = expansion(mult = c(0, .01))
  ) +
  scale_y_continuous(
    limits = c(0, 1), breaks = c(0, .5, 1),
    labels = scales::label_percent(accuracy = 50), expand = expansion(mult = c(0, .02))
  ) +
  labs(x = "tolerance ε", y = "sufficient\ncoverage") +
  theme_rq3(base_size = 5.75) +
  theme(
    panel.grid.major.x = element_blank(), panel.grid.minor = element_blank(),
    strip.text = element_blank(), strip.background = element_blank(),
    axis.text.x = element_text(size = 5.0), axis.text.y = element_text(size = 4.7),
    axis.title.x = element_text(size = 5.5), axis.title.y = element_text(size = 5.0),
    plot.margin = margin(0, 3, 1, 3)
  )

p4a <- cowplot::plot_grid(
  p4a_main, p4a_coverage, ncol = 1, rel_heights = c(1, .22),
  align = "v", axis = "lr", greedy = TRUE
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
  facet_wrap(~dimension, nrow = 1) +
  scale_color_ms_metric(guide = "none") +
  scale_x_continuous(
    breaks = 1:7,
    limits = c(.65, 7.35),
    labels = as.character(1:7)
  ) +
  scale_y_continuous(trans = scales::transform_asinh(), breaks = scales::breaks_extended(n = 4)) +
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
p4c <- ggplot(pair_ecdf,
              aes(epsilon, fraction_metrics_substitutable,
                  color = pair, group = interaction(dimension, comparison_pair_id, drop = TRUE))) +
  geom_step(linewidth = .76, alpha = .94) +
  facet_wrap(~dimension, nrow = 1) +
  scale_color_manual(values = pair_palette, breaks = pair_levels, name = NULL) +
  scale_x_continuous(
    trans = scales::transform_asinh(), limits = c(0, epsilon_limit),
    breaks = scales::breaks_extended(n = 4), expand = expansion(mult = c(0, .01))
  ) +
  scale_y_continuous(limits = c(0, 1), labels = scales::label_percent(accuracy = 25)) +
  labs(
    title = "c  Target-aligned alternatives become substitutable as tolerance relaxes",
    x = "tolerance ε", y = "fraction of metrics substitutable"
  ) +
  theme_rq3(base_size = 6.3, legend_position = "bottom") +
  theme(
    panel.grid.major.x = element_blank(), strip.text = element_text(size = 5.8),
    legend.text = element_text(size = 5.0), legend.key.width = grid::unit(5.0, "mm")
  )

fig4_bottom <- cowplot::plot_grid(
  p4b, p4c, ncol = 2, rel_widths = c(1.08, .92),
  align = "hv", axis = "tblr", greedy = TRUE
)
fig4_body <- cowplot::plot_grid(
  p4a, fig4_bottom, ncol = 1, rel_heights = c(1.14, .86),
  align = "v", axis = "l", greedy = TRUE
)
fig4 <- cowplot::plot_grid(metric_legend, fig4_body, ncol = 1,
                           rel_heights = c(.042, 1), align = "v", greedy = TRUE)

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

# Retain the frozen Pareto summaries for the supplementary audit view below.
pareto_base <- pareto |>
  filter(is.finite(resolution_s), is.finite(n_days)) |>
  mutate(
    ever_pareto = as.logical(ever_pareto),
    pareto_persistence = pmax(0, pmin(1, pareto_persistence)),
    metric_class = factor(metric_class, levels = METRIC_CLASSES),
    resolution_rank = match(resolution_s, RES_LEVELS)
  ) |>
  filter(is.finite(resolution_rank))

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

entry_class_surface <- entry_metric_surface |>
  group_by(metric_class, resolution_s, resolution_rank, n_days) |>
  summarise(
    n_metrics = n_distinct(metric),
    epsilon_entry_median = median(epsilon_metric, na.rm = TRUE),
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
    subtitle = "darker = more permissive tolerance required; grey = unresolved",
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
  facet_wrap(~metric_class, ncol = 3) +
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
    subtitle = "warm = more permissive tolerance required; cool = stricter tolerance sufficient",
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

# c. Conditional marginal returns. Positive gain means that adding burden along
# one axis lowers the entry tolerance required for sufficiency while holding the
# other axis fixed.
resolved_cells <- entry_metric_surface |>
  transmute(
    metric, metric_class, resolution_s, resolution_rank, n_days,
    epsilon_entry = epsilon_metric
  ) |>
  distinct()

duration_from <- resolved_cells |>
  transmute(
    metric, metric_class, resolution_s, resolution_rank,
    n_days_from = n_days, n_days_to = n_days + 1,
    epsilon_from = epsilon_entry
  )
duration_to <- resolved_cells |>
  transmute(
    metric, metric_class, resolution_s, resolution_rank,
    n_days_to = n_days, epsilon_to = epsilon_entry
  )
duration_gain_raw <- inner_join(
  duration_from, duration_to,
  by = c("metric", "metric_class", "resolution_s", "resolution_rank", "n_days_to")
) |>
  mutate(gain = epsilon_from - epsilon_to) |>
  filter(is.finite(gain))

temporal_from <- resolved_cells |>
  transmute(
    metric, metric_class, n_days,
    resolution_rank_from = resolution_rank,
    resolution_rank_to = resolution_rank + 1,
    resolution_s_from = resolution_s,
    epsilon_from = epsilon_entry
  )
temporal_to <- resolved_cells |>
  transmute(
    metric, metric_class, n_days,
    resolution_rank_to = resolution_rank,
    resolution_s_to = resolution_s,
    epsilon_to = epsilon_entry
  )
temporal_gain_raw <- inner_join(
  temporal_from, temporal_to,
  by = c("metric", "metric_class", "n_days", "resolution_rank_to")
) |>
  mutate(gain = epsilon_from - epsilon_to) |>
  filter(is.finite(gain))

duration_metric_gain <- duration_gain_raw |>
  group_by(resolution_s, resolution_rank, metric, metric_class) |>
  summarise(gain = median(gain, na.rm = TRUE), .groups = "drop")
temporal_metric_gain <- temporal_gain_raw |>
  group_by(n_days, metric, metric_class) |>
  summarise(gain = median(gain, na.rm = TRUE), .groups = "drop")

duration_gain_summary <- duration_metric_gain |>
  group_by(resolution_s, resolution_rank) |>
  summarise(
    n_metrics = n_distinct(metric),
    gain_median = median(gain, na.rm = TRUE),
    gain_q25 = quantile(gain, .25, na.rm = TRUE, names = FALSE),
    gain_q75 = quantile(gain, .75, na.rm = TRUE, names = FALSE),
    .groups = "drop"
  )
temporal_gain_summary <- temporal_metric_gain |>
  group_by(n_days) |>
  summarise(
    n_metrics = n_distinct(metric),
    gain_median = median(gain, na.rm = TRUE),
    gain_q25 = quantile(gain, .25, na.rm = TRUE, names = FALSE),
    gain_q75 = quantile(gain, .75, na.rm = TRUE, names = FALSE),
    .groups = "drop"
  )

gain_limit <- ms_symmetric_limit(
  duration_metric_gain$gain, temporal_metric_gain$gain,
  duration_gain_summary$gain_q25, duration_gain_summary$gain_q75,
  temporal_gain_summary$gain_q25, temporal_gain_summary$gain_q75,
  pad = 1.05, fallback = 1
)

p5c_duration <- ggplot(duration_metric_gain, aes(resolution_rank, gain)) +
  geom_hline(yintercept = 0, linewidth = .28, color = "#9DA2A5") +
  geom_point(
    position = position_jitter(width = .07, height = 0, seed = 81),
    size = .43, color = "#A7B0B5", alpha = .23
  ) +
  geom_segment(
    data = duration_gain_summary,
    aes(x = resolution_rank, xend = resolution_rank, y = gain_q25, yend = gain_q75),
    inherit.aes = FALSE, linewidth = .92, color = MS_PRIMARY, alpha = .58, lineend = "round"
  ) +
  geom_line(
    data = duration_gain_summary,
    aes(resolution_rank, gain_median),
    inherit.aes = FALSE, linewidth = .70, color = MS_PRIMARY
  ) +
  geom_point(
    data = duration_gain_summary,
    aes(resolution_rank, gain_median),
    inherit.aes = FALSE, shape = 18, size = 1.50, color = MS_PRIMARY
  ) +
  scale_x_continuous(
    breaks = seq_along(fig5_res_levels), labels = fig5_res_labels,
    expand = expansion(add = .20)
  ) +
  scale_y_continuous(
    limits = c(-gain_limit, gain_limit),
    breaks = scales::breaks_extended(n = 4)
  ) +
  labs(
    title = "Added duration | temporal state fixed",
    x = "temporal resolution", y = "reduction in entry tolerance"
  ) +
  theme_rq3(base_size = 5.75) +
  theme(
    panel.grid.major.x = element_blank(),
    axis.text.x = element_text(angle = 30, hjust = 1, size = 4.45),
    plot.title = element_text(size = 5.35, hjust = .5),
    plot.margin = margin(2, 2, 2, 2)
  )

p5c_temporal <- ggplot(temporal_metric_gain, aes(n_days, gain)) +
  geom_hline(yintercept = 0, linewidth = .28, color = "#9DA2A5") +
  geom_point(
    position = position_jitter(width = .07, height = 0, seed = 83),
    size = .43, color = "#A7B0B5", alpha = .23
  ) +
  geom_segment(
    data = temporal_gain_summary,
    aes(x = n_days, xend = n_days, y = gain_q25, yend = gain_q75),
    inherit.aes = FALSE, linewidth = .92, color = MS_SECONDARY, alpha = .58, lineend = "round"
  ) +
  geom_line(
    data = temporal_gain_summary,
    aes(n_days, gain_median),
    inherit.aes = FALSE, linewidth = .70, color = MS_SECONDARY
  ) +
  geom_point(
    data = temporal_gain_summary,
    aes(n_days, gain_median),
    inherit.aes = FALSE, shape = 18, size = 1.50, color = MS_SECONDARY
  ) +
  scale_x_continuous(
    breaks = fig5_days, labels = paste0(fig5_days, " d"),
    expand = expansion(add = .20)
  ) +
  scale_y_continuous(
    limits = c(-gain_limit, gain_limit),
    breaks = scales::breaks_extended(n = 4)
  ) +
  labs(
    title = "Temporal refinement | duration fixed",
    x = "monitoring duration", y = NULL
  ) +
  theme_rq3(base_size = 5.75) +
  theme(
    panel.grid.major.x = element_blank(),
    axis.text.x = element_text(size = 4.65),
    axis.text.y = element_blank(), axis.ticks.y = element_blank(),
    axis.line.y = element_blank(),
    plot.title = element_text(size = 5.35, hjust = .5),
    plot.margin = margin(2, 2, 2, 2)
  )

p5c_core <- cowplot::plot_grid(
  p5c_duration, p5c_temporal, ncol = 2, rel_widths = c(1.05, .95),
  align = "hv", axis = "tblr", greedy = TRUE
)
p5c <- cowplot::ggdraw() +
  cowplot::draw_plot(p5c_core, x = 0, y = 0, width = 1, height = .92) +
  cowplot::draw_label(
    "c  Conditional marginal returns reveal joint trade-offs",
    x = .002, y = .998, hjust = 0, vjust = 1,
    fontface = "bold", size = 6.2
  )

fig5_top <- cowplot::plot_grid(
  p5a, p5b, ncol = 2, rel_widths = c(.40, .60),
  align = "hv", axis = "tblr", greedy = TRUE
)
fig5_body <- cowplot::plot_grid(
  fig5_top, p5c, ncol = 1, rel_heights = c(1.22, .78),
  align = "v", axis = "l", greedy = TRUE
)
ms_plot_save(fig5_body, file.path(OUT_DIR, "Fig5_RQ3.pdf"), 9.0, 6.3)
ms_plot_save(fig5_body, file.path(OUT_DIR, "Fig5_RQ3.png"), 9.0, 6.3)

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
  duration_metric_gain |>
    mutate(metric_class = as.character(metric_class)),
  file.path("results", "rq3", "fig5_duration_marginal_gain.csv"), na = ""
)
readr::write_csv(
  temporal_metric_gain |>
    mutate(metric_class = as.character(metric_class)),
  file.path("results", "rq3", "fig5_temporal_marginal_gain.csv"), na = ""
)

# Supplement: preserve support × placement × optical explicitly as an audit view.
pareto_full <- pareto_base |>
  filter(ever_pareto %in% TRUE) |>
  mutate(facet_row = paste(support_id, placement, sep = " · "))

p5s_a <- ggplot(
  pareto_full,
  aes(resolution_rank, n_days, color = metric_class, size = pareto_persistence)
) +
  geom_point(alpha = .78) +
  facet_grid(facet_row ~ optical, scales = "free", space = "free") +
  scale_color_ms_metric() +
  scale_size_continuous(range = c(.45, 2.8), limits = c(0, 1), name = "Pareto persistence") +
  scale_x_continuous(breaks = 1:7, labels = RES_LABELS) +
  scale_y_continuous(breaks = 1:6, labels = paste0(1:6, " d")) +
  labs(title = "Support-explicit Pareto persistence",
       x = "temporal resolution (low → high burden)", y = "monitoring duration") +
  theme_ms(base_size = 6.0, legend_position = "bottom") +
  theme(axis.text.x = element_text(angle = 42, hjust = 1))

frequency_full <- frequency |>
  mutate(
    resolution_rank = match(resolution_s, RES_LEVELS),
    mean_pareto_persistence = pmax(0, pmin(1, mean_pareto_persistence)),
    facet_row = paste(support_id, placement, sep = " · ")
  ) |>
  filter(is.finite(resolution_rank))

p5s_b <- ggplot(frequency_full,
                 aes(resolution_rank, n_days, fill = mean_pareto_persistence)) +
  geom_tile(color = "white", linewidth = .14) +
  facet_grid(facet_row ~ optical, scales = "free", space = "free") +
  scale_fill_ms_sequential(
    name = "mean Pareto\npersistence", limits = c(0, 1),
    labels = scales::label_percent(accuracy = 25)
  ) +
  scale_x_continuous(breaks = 1:7, labels = RES_LABELS) +
  scale_y_continuous(breaks = 1:6, labels = paste0(1:6, " d")) +
  labs(title = "Support-explicit mean Pareto persistence",
       x = "temporal resolution (low → high burden)", y = "monitoring duration") +
  theme_ms(base_size = 6.0, legend_position = "right") +
  theme(axis.text.x = element_text(angle = 42, hjust = 1))

fig5s <- cowplot::plot_grid(p5s_a, p5s_b, ncol = 2, rel_widths = c(1.05, .95))
ms_plot_save(fig5s, file.path(OUT_DIR, "FigS_RQ3_pareto_facets.pdf"), 15.5, 10.0)
ms_plot_save(fig5s, file.path(OUT_DIR, "FigS_RQ3_pareto_facets.png"), 15.5, 10.0)

ms_plot_write_manifest(
  file.path(OUT_DIR, "figure_artifact_manifest.csv"),
  tibble(
    figure = c(
      "Fig4_RQ3", "Fig5_RQ3",
      "FigS_RQ3_single_dimension_detail", "FigS_RQ3_pareto_facets"
    ),
    input_artifact = c(
      "rq3_observed_stability+sufficiency+unordered_substitutability",
      "rq3_joint_summary+pareto_frontiers",
      "rq3_convergence_profile+sufficiency",
      "rq3_pareto_frontiers+frequency"
    ),
    core_artifact_version = CORE_VERSION,
    rq1_analysis_version = RQ1_VERSION,
    rq2_analysis_version = NA_character_,
    rq3_analysis_version = RQ3_VERSION
  )
)
message("RQ3 v5 figures complete: compact single-dimension sufficiency and joint Pareto landscapes")

source(file.path("scripts", "utils", "analysis_design.R"), local = .GlobalEnv)

# Rebind every display-level ordered axis to the single frozen design definition.
# The legacy v5 source is retained for reproducibility, but canonical figures must
# never depend on its historical hard-coded 7-state temporal lattice.
RES_LEVELS <- rev(ms_primary_temporal_s())
RES_LABELS <- ms_temporal_label(RES_LEVELS)
DURATION_LEVELS <- ms_primary_duration_days()
ORDERED_MAX_RANK <- max(length(RES_LEVELS), length(DURATION_LEVELS))

if (!all(sort(unique(joint$resolution_s)) %in% sort(ms_primary_temporal_s()))) {
  stop("RQ3 joint artifact contains temporal states outside the frozen primary design", call. = FALSE)
}
if (!all(sort(unique(joint$n_days)) %in% DURATION_LEVELS)) {
  stop("RQ3 joint artifact contains duration states outside the frozen primary design", call. = FALSE)
}
if (!grepl(ms_analysis_design_id(), RQ3_VERSION, fixed = TRUE)) {
  stop("RQ3 plotting inputs do not match the current frozen analysis design", call. = FALSE)
}

# Recompute Pareto display summaries from the frozen RQ3 artifact using the
# current primary lattice. This prevents old plotting constants from silently
# dropping newly introduced states such as 40 s or 120 s.
pareto_base <- pareto |>
  filter(
    resolution_s %in% RES_LEVELS,
    n_days %in% DURATION_LEVELS,
    is.finite(resolution_s), is.finite(n_days)
  ) |>
  mutate(
    ever_pareto = as.logical(ever_pareto),
    pareto_persistence = pmax(0, pmin(1, pareto_persistence)),
    metric_class = factor(metric_class, levels = METRIC_CLASSES),
    resolution_rank = match(resolution_s, RES_LEVELS)
  ) |>
  filter(is.finite(resolution_rank))

pareto_global <- pareto_base |>
  group_by(resolution_s, resolution_rank, n_days) |>
  summarise(
    n_records = n(),
    fraction_ever_pareto = mean(ever_pareto, na.rm = TRUE),
    persistence_when_pareto = if (any(ever_pareto %in% TRUE, na.rm = TRUE)) {
      safe_mean(pareto_persistence[ever_pareto %in% TRUE])
    } else 0,
    .groups = "drop"
  )

pareto_class <- pareto_base |>
  group_by(metric_class, resolution_s, resolution_rank, n_days) |>
  summarise(
    n_records = n(),
    fraction_ever_pareto = mean(ever_pareto, na.rm = TRUE),
    persistence_when_pareto = if (any(ever_pareto %in% TRUE, na.rm = TRUE)) {
      safe_mean(pareto_persistence[ever_pareto %in% TRUE])
    } else 0,
    .groups = "drop"
  )

landscape_bg <- tidyr::expand_grid(
  resolution_rank = seq_along(RES_LEVELS),
  n_days = DURATION_LEVELS
)

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

p4a <- ggplot(requirement_summary, aes(epsilon, rank_median, color = metric_class)) +
  geom_step(aes(y = rank_q25, group = metric_class), linewidth = .34, alpha = .24) +
  geom_step(aes(y = rank_q75, group = metric_class), linewidth = .34, alpha = .24) +
  geom_step(aes(group = metric_class), linewidth = .82, alpha = .96) +
  facet_wrap(~dimension, nrow = 1) +
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
    subtitle = "thick line = class median; thin lines = interquartile range",
    x = "tolerance ε", y = "minimum sufficient requirement rank\n(low → high burden)"
  ) +
  theme_rq3(base_size = 6.6) +
  theme(
    panel.grid.major.x = element_line(colour = "#ECEFF0", linewidth = .20),
    strip.text = element_text(size = 6.2),
    plot.subtitle = element_text(size = 5.0, colour = "#666A6D", margin = margin(t = -1, b = 2))
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
  facet_wrap(~dimension, nrow = 1) +
  scale_color_ms_metric(guide = "none") +
  scale_x_continuous(
    breaks = seq_len(ORDERED_MAX_RANK),
    limits = c(.65, ORDERED_MAX_RANK + .35),
    labels = as.character(seq_len(ORDERED_MAX_RANK))
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

p4c <- ggplot(
  pair_ecdf,
  aes(
    epsilon, fraction_metrics_substitutable,
    color = pair,
    group = interaction(dimension, comparison_pair_id, drop = TRUE)
  )
) +
  geom_step(linewidth = .76, alpha = .94) +
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
    x = "tolerance ε", y = "fraction of metrics substitutable"
  ) +
  theme_rq3(base_size = 6.3, legend_position = "bottom") +
  theme(
    panel.grid.major.x = element_line(colour = "#ECEFF0", linewidth = .20),
    strip.text = element_text(size = 5.8),
    legend.text = element_text(size = 5.0),
    legend.key.width = grid::unit(5.0, "mm")
  )

fig4_bottom <- cowplot::plot_grid(
  p4b, p4c, ncol = 2, rel_widths = c(1.08, .92),
  align = "hv", axis = "tblr", greedy = TRUE
)
fig4_body <- cowplot::plot_grid(
  p4a, fig4_bottom, ncol = 1, rel_heights = c(1.14, .86),
  align = "v", axis = "l", greedy = TRUE
)
fig4 <- cowplot::plot_grid(
  metric_legend, fig4_body, ncol = 1,
  rel_heights = c(.042, 1), align = "v", axis = "l", greedy = TRUE
)
ms_plot_save(fig4, file.path(OUT_DIR, "Fig4_RQ3.pdf"), 9.0, 6.1)
ms_plot_save(fig4, file.path(OUT_DIR, "Fig4_RQ3.png"), 9.0, 6.1)

# -----------------------------------------------------------------------------
