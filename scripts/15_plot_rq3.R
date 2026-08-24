# Canonical RQ3 plotting entrypoint. Plotting consumes frozen v5 outputs only;
# unlike the analysis runtime, the plot source requires no deployment-time patch.
.ms_file <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
if (length(.ms_file)) {
  .ms_script <- normalizePath(sub("^--file=", "", .ms_file[[1]]), winslash = "/", mustWork = TRUE)
  .ms_root <- normalizePath(file.path(dirname(.ms_script), ".."), winslash = "/", mustWork = TRUE)
  if (!file.exists(file.path(.ms_root, "scripts", "15_plot_rq3_v5.R"))) {
    stop("Could not resolve measurement-sufficiency repository root from ", .ms_script, call. = FALSE)
  }
  setwd(.ms_root)
}
rm(.ms_file)
if (exists(".ms_script")) rm(.ms_script)
if (exists(".ms_root")) rm(.ms_root)
source(file.path("scripts", "15_plot_rq3_v5.R"), local = .GlobalEnv)

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
    limits = c(0, epsilon_limit),
    breaks = epsilon_ticks,
    labels = epsilon_labels,
    expand = expansion(mult = c(0, .01))
  ) +
  scale_y_continuous(breaks = 1:7, limits = c(.8, 7.2)) +
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
    limits = c(0, epsilon_limit),
    breaks = epsilon_ticks,
    labels = epsilon_labels,
    expand = expansion(mult = c(0, .01))
  ) +
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
  p4a, fig4_bottom, ncol = 1, rel_heights = c(1.08, .92),
  align = "v", axis = "l", greedy = TRUE
)
fig4 <- cowplot::plot_grid(
  metric_legend, fig4_body, ncol = 1,
  rel_heights = c(.042, 1), align = "v", greedy = TRUE
)
ms_plot_save(fig4, file.path(OUT_DIR, "Fig4_RQ3.png"), 9.0, 6.1)

# -----------------------------------------------------------------------------
# Main-text redesign for Fig. 5.
# The previous bubble display encoded ever-Pareto frequency and conditional
# persistence separately; both visual channels saturated across much of the grid.
# Their product is the unconditional Pareto occupancy of a configuration. Panel a
# shows that common landscape directly; panel b removes it and shows only each
# representation class's deviation from the overall landscape.
# -----------------------------------------------------------------------------
pareto_global_display <- pareto_global |>
  mutate(
    pareto_occupancy = fraction_ever_pareto * persistence_when_pareto
  )

pareto_class_display <- pareto_class |>
  mutate(
    pareto_occupancy = fraction_ever_pareto * persistence_when_pareto
  ) |>
  left_join(
    pareto_global_display |>
      select(resolution_s, resolution_rank, n_days, overall_pareto_occupancy = pareto_occupancy),
    by = c("resolution_s", "resolution_rank", "n_days")
  ) |>
  mutate(
    occupancy_difference = pareto_occupancy - overall_pareto_occupancy
  )

pareto_global_grid <- landscape_bg |>
  left_join(
    pareto_global_display |>
      select(resolution_rank, n_days, pareto_occupancy),
    by = c("resolution_rank", "n_days")
  )

pareto_class_grid <- tidyr::crossing(
  metric_class = factor(METRIC_CLASSES, levels = METRIC_CLASSES),
  landscape_bg
) |>
  left_join(
    pareto_class_display |>
      select(metric_class, resolution_rank, n_days, pareto_occupancy, occupancy_difference),
    by = c("metric_class", "resolution_rank", "n_days")
  )

occupancy_max <- max(pareto_global_grid$pareto_occupancy, na.rm = TRUE)
if (!is.finite(occupancy_max) || occupancy_max <= 0) occupancy_max <- 1
contrast_limit <- max(abs(pareto_class_grid$occupancy_difference), na.rm = TRUE)
if (!is.finite(contrast_limit) || contrast_limit <= 0) contrast_limit <- .01

p5a <- ggplot(
  pareto_global_grid,
  aes(resolution_rank, n_days, fill = pareto_occupancy)
) +
  geom_tile(width = .92, height = .92, color = "white", linewidth = .34) +
  scale_x_continuous(
    breaks = 1:7, labels = RES_LABELS,
    expand = expansion(add = .35)
  ) +
  scale_y_continuous(
    breaks = 1:6, labels = paste0(1:6, " d"),
    expand = expansion(add = .35)
  ) +
  scale_fill_ms_sequential(
    limits = c(0, occupancy_max),
    labels = scales::label_percent(accuracy = 1),
    na.value = "#F1F2F2",
    name = "Pareto occupancy"
  ) +
  coord_fixed(ratio = .88, clip = "off") +
  labs(
    title = "a  Persistent Pareto occupancy across joint measurement burden",
    subtitle = "ever-Pareto frequency × persistence when Pareto",
    x = "temporal resolution  (low → high burden)",
    y = "monitoring duration"
  ) +
  theme_rq3(base_size = 6.5, legend_position = "bottom") +
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_text(angle = 28, hjust = 1, size = 5.3),
    plot.subtitle = element_text(size = 4.9, colour = "#666A6D", margin = margin(t = -1, b = 2)),
    legend.text = element_text(size = 4.9),
    legend.title = element_text(size = 5.0),
    legend.key.width = grid::unit(8.0, "mm")
  )

p5b <- ggplot(
  pareto_class_grid,
  aes(resolution_rank, n_days, fill = occupancy_difference)
) +
  geom_tile(width = .92, height = .92, color = "white", linewidth = .24) +
  facet_wrap(~metric_class, ncol = 3) +
  scale_x_continuous(
    breaks = 1:7, labels = RES_LABELS,
    expand = expansion(add = .28)
  ) +
  scale_y_continuous(
    breaks = 1:6, labels = paste0(1:6, " d"),
    expand = expansion(add = .28)
  ) +
  scale_fill_ms_diverging(
    max_abs = contrast_limit,
    labels = scales::label_percent(accuracy = 1),
    na.value = "#F1F2F2",
    name = "class − overall\nPareto occupancy"
  ) +
  coord_fixed(ratio = .88, clip = "off") +
  labs(
    title = "b  Representation classes deviate from the common Pareto landscape",
    subtitle = "positive values indicate greater Pareto occupancy than the overall landscape",
    x = "temporal burden", y = "duration"
  ) +
  theme_rq3(base_size = 5.8, legend_position = "bottom") +
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_text(angle = 36, hjust = 1, size = 4.5),
    axis.text.y = element_text(size = 4.6),
    strip.text = element_text(size = 5.0),
    plot.subtitle = element_text(size = 4.35, colour = "#666A6D", margin = margin(t = -1, b = 2)),
    panel.spacing = grid::unit(1.8, "mm"),
    legend.text = element_text(size = 4.7),
    legend.title = element_text(size = 4.8),
    legend.key.width = grid::unit(7.0, "mm")
  )

fig5 <- cowplot::plot_grid(
  p5a, p5b, ncol = 2, rel_widths = c(.40, .60),
  align = "hv", axis = "tblr", greedy = TRUE
)
ms_plot_save(fig5, file.path(OUT_DIR, "Fig5_RQ3.png"), 9.0, 5.6)

readr::write_csv(
  pareto_global_display,
  file.path("results", "rq3", "fig5_pareto_occupancy_overall.csv"),
  na = ""
)
readr::write_csv(
  pareto_class_display |>
    mutate(metric_class = as.character(metric_class)),
  file.path("results", "rq3", "fig5_pareto_occupancy_by_class.csv"),
  na = ""
)
readr::write_csv(
  pareto_class_grid |>
    mutate(metric_class = as.character(metric_class)),
  file.path("results", "rq3", "fig5_pareto_occupancy_class_contrast.csv"),
  na = ""
)

message(
  "Fig. 4 display refinement: shared log1p tolerance axis with explicit dense breaks. ",
  "Fig. 5 redesign: occupancy heatmap plus class-minus-overall contrast heatmaps."
)
