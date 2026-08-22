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
  theme_ms(base_size = base_size, legend_position = legend_position) +
    theme(
      panel.border = element_blank(),
      axis.line.x = element_line(colour = "#505457", linewidth = .34),
      axis.line.y = element_line(colour = "#505457", linewidth = .34),
      panel.grid.major = element_line(colour = "#ECEFF0", linewidth = .22),
      panel.grid.minor = element_blank(),
      axis.ticks = element_line(colour = "#505457", linewidth = .28),
      strip.background = element_blank(),
      strip.text = element_text(face = "bold", colour = "#25282A", margin = margin(1, 2, 2, 2)),
      plot.title = element_text(size = base_size + .8, face = "bold", margin = margin(b = 3)),
      plot.margin = margin(3, 4, 3, 4)
    )
}

metric_legend_source <- ggplot(
  tibble(metric_class = factor(METRIC_CLASSES, levels = METRIC_CLASSES),
         x = seq_along(METRIC_CLASSES), y = 1),
  aes(x, y, color = metric_class)
) +
  geom_point(size = 1.7) +
  scale_color_ms_metric() +
  guides(color = guide_legend(title = NULL, nrow = 1, byrow = TRUE,
                              override.aes = list(size = 1.5))) +
  theme_void(base_family = MS_FONT, base_size = 7) +
  theme(
    legend.position = "bottom", legend.direction = "horizontal",
    legend.margin = margin(0, 0, 0, 0), legend.box.margin = margin(0, 0, 0, 0),
    legend.text = element_text(size = 5.35), legend.key.width = grid::unit(3.5, "mm")
  )
metric_legend <- cowplot::get_legend(metric_legend_source)

# =============================================================================
# Fig. 4 — tolerance determines the minimum sufficient burden
# =============================================================================

observed_display <- observed |>
  filter(dimension %in% ORDERED_DIMS, status == "resolved", is.finite(R_obs), is.finite(requirement_rank)) |>
  mutate(
    metric_class = factor(metric_class, levels = METRIC_CLASSES),
    dimension = factor(dimension, levels = ORDERED_DIMS, labels = unname(ORDERED_TITLES[ORDERED_DIMS]))
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

p4a <- ggplot(requirement_summary, aes(epsilon, rank_median, color = metric_class)) +
  geom_step(aes(y = rank_q25, group = metric_class), linewidth = .34, alpha = .24) +
  geom_step(aes(y = rank_q75, group = metric_class), linewidth = .34, alpha = .24) +
  geom_step(aes(group = metric_class), linewidth = .82, alpha = .96) +
  facet_wrap(~dimension, nrow = 1, scales = "free_x") +
  scale_color_ms_metric(guide = "none") +
  scale_x_continuous(trans = scales::transform_asinh(), breaks = scales::breaks_extended(n = 4)) +
  scale_y_continuous(breaks = 1:7, limits = c(.8, 7.2)) +
  labs(
    title = "a  Tolerance sets the minimum sufficient measurement burden",
    subtitle = "thick line = class median; thin lines = interquartile range",
    x = "tolerance ε", y = "minimum sufficient requirement rank\n(low → high burden)"
  ) +
  theme_rq3(base_size = 6.6) +
  theme(
    panel.grid.major.x = element_blank(), strip.text = element_text(size = 6.2),
    plot.subtitle = element_text(size = 5.0, colour = "#666A6D", margin = margin(t = -1, b = 2))
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
  )

p4b <- ggplot() +
  geom_point(
    data = observed_display,
    aes(requirement_rank, R_obs, color = metric_class),
    position = position_jitter(width = .10, height = 0, seed = 91),
    size = .52, alpha = .16
  ) +
  geom_linerange(
    data = observed_summary,
    aes(requirement_rank, ymin = R_q25, ymax = R_q75, color = metric_class),
    position = position_dodge(width = .58), linewidth = .42, alpha = .46
  ) +
  geom_point(
    data = observed_summary,
    aes(requirement_rank, R_median, color = metric_class),
    position = position_dodge(width = .58), shape = 18, size = 1.6
  ) +
  facet_wrap(~dimension, nrow = 1, scales = "free_x") +
  scale_color_ms_metric(guide = "none") +
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
pair_palette <- setNames(rep(MS_THREE_COLORS, length.out = length(pair_levels)), pair_levels)
p4c <- ggplot(pair_ecdf,
              aes(epsilon, fraction_metrics_substitutable, color = pair, group = pair)) +
  geom_step(linewidth = .76, alpha = .94) +
  facet_wrap(~dimension, nrow = 1, scales = "free_x") +
  scale_color_manual(values = pair_palette, breaks = pair_levels, name = NULL) +
  scale_x_continuous(trans = scales::transform_asinh(), breaks = scales::breaks_extended(n = 4)) +
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
  p4a, fig4_bottom, ncol = 1, rel_heights = c(1.08, .92),
  align = "v", axis = "l", greedy = TRUE
)
fig4 <- cowplot::plot_grid(metric_legend, fig4_body, ncol = 1,
                           rel_heights = c(.042, 1), align = "v", greedy = TRUE)
ms_plot_save(fig4, file.path(OUT_DIR, "Fig4_RQ3.pdf"), 9.0, 6.1)
ms_plot_save(fig4, file.path(OUT_DIR, "Fig4_RQ3.png"), 9.0, 6.1)

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
# Fig. 5 — joint temporal × duration Pareto landscape
# =============================================================================

pareto_base <- pareto |>
  filter(is.finite(resolution_s), is.finite(n_days)) |>
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

landscape_bg <- tidyr::expand_grid(resolution_rank = 1:7, n_days = 1:6)

p5a <- ggplot() +
  geom_point(
    data = landscape_bg, aes(resolution_rank, n_days),
    shape = 21, size = 1.55, fill = "white", color = "#D9DDDF", stroke = .24
  ) +
  geom_point(
    data = pareto_global,
    aes(resolution_rank, n_days, size = fraction_ever_pareto,
        fill = persistence_when_pareto),
    shape = 21, color = "#3F4447", stroke = .26, alpha = .98
  ) +
  scale_x_continuous(breaks = 1:7, labels = RES_LABELS, expand = expansion(add = .35)) +
  scale_y_continuous(breaks = 1:6, labels = paste0(1:6, " d"), expand = expansion(add = .35)) +
  scale_size_continuous(
    range = c(.8, 5.3), limits = c(0, 1),
    labels = scales::label_percent(accuracy = 25), name = "ever Pareto"
  ) +
  scale_fill_ms_sequential(
    limits = c(0, 1), labels = scales::label_percent(accuracy = 25),
    name = "persistence\nwhen Pareto"
  ) +
  coord_fixed(ratio = .88, clip = "off") +
  labs(
    title = "a  Joint measurement burden forms a compact Pareto landscape",
    subtitle = "point size = how often a configuration is ever Pareto; fill = its persistence when Pareto",
    x = "temporal resolution  (low → high burden)", y = "monitoring duration"
  ) +
  theme_rq3(base_size = 6.5, legend_position = "bottom") +
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_text(angle = 28, hjust = 1, size = 5.3),
    plot.subtitle = element_text(size = 4.9, colour = "#666A6D", margin = margin(t = -1, b = 2)),
    legend.text = element_text(size = 4.9), legend.title = element_text(size = 5.0),
    legend.key.width = grid::unit(4.2, "mm")
  )

p5b <- ggplot() +
  geom_point(
    data = tidyr::crossing(
      metric_class = factor(METRIC_CLASSES, levels = METRIC_CLASSES),
      landscape_bg
    ),
    aes(resolution_rank, n_days),
    shape = 21, size = .82, fill = "white", color = "#E0E3E5", stroke = .18
  ) +
  geom_point(
    data = pareto_class,
    aes(resolution_rank, n_days, size = fraction_ever_pareto,
        fill = persistence_when_pareto),
    shape = 21, color = "#474C4F", stroke = .18, alpha = .98
  ) +
  facet_wrap(~metric_class, ncol = 3) +
  scale_x_continuous(breaks = 1:7, labels = RES_LABELS, expand = expansion(add = .28)) +
  scale_y_continuous(breaks = 1:6, labels = paste0(1:6, " d"), expand = expansion(add = .28)) +
  scale_size_continuous(range = c(.35, 2.8), limits = c(0, 1), guide = "none") +
  scale_fill_ms_sequential(limits = c(0, 1), guide = "none") +
  coord_fixed(ratio = .88, clip = "off") +
  labs(
    title = "b  Representation classes occupy different Pareto regions",
    x = "temporal burden", y = "duration"
  ) +
  theme_rq3(base_size = 5.8) +
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_text(angle = 36, hjust = 1, size = 3.7),
    axis.text.y = element_text(size = 4.0),
    strip.text = element_text(size = 5.0),
    panel.spacing = grid::unit(1.8, "mm")
  )

fig5 <- cowplot::plot_grid(
  p5a, p5b, ncol = 2, rel_widths = c(.43, .57),
  align = "hv", axis = "tblr", greedy = TRUE
)
ms_plot_save(fig5, file.path(OUT_DIR, "Fig5_RQ3.pdf"), 9.0, 5.6)
ms_plot_save(fig5, file.path(OUT_DIR, "Fig5_RQ3.png"), 9.0, 5.6)

readr::write_csv(pareto_global,
                 file.path("results", "rq3", "fig5_pareto_landscape_overall.csv"), na = "")
readr::write_csv(
  pareto_class |> mutate(metric_class = as.character(metric_class)),
  file.path("results", "rq3", "fig5_pareto_landscape_by_class.csv"), na = ""
)
readr::write_csv(
  pareto_base |> mutate(metric_class = as.character(metric_class)),
  file.path("results", "rq3", "fig5_pareto_plot.csv"), na = ""
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
      "rq3_pareto_frontiers",
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
