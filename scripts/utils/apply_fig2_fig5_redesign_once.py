from pathlib import Path


def replace_between(text, start, end, replacement):
    i = text.find(start)
    if i < 0:
        raise SystemExit(f"start anchor not found: {start[:80]}")
    j = text.find(end, i)
    if j < 0:
        raise SystemExit(f"end anchor not found: {end[:80]}")
    return text[:i] + replacement + text[j:]

fig2_path = Path("scripts/13_plot_rq2.R")
fig2 = fig2_path.read_text(encoding="utf-8")

fig2_a = r'''
# a. Conditional distortion geometry combines magnitude and directional coherence
# on the same exposure-state axis. Transition-level trajectories retain the paired
# state response, while metric-class summaries remain the visual foreground.
conditional_trajectory_state <- conditional |>
  mutate(
    metric = as.character(metric),
    metric_class = factor(as.character(metric_class), levels = METRIC_CLASSES),
    state_num = as.integer(state_bin_label),
    class_num = as.integer(metric_class),
    class_offset = (class_num - (length(METRIC_CLASSES) + 1) / 2) * .055,
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

  p_mag <- ggplot() +
    geom_line(
      data = tr,
      aes(x_pos, A_state, group = interaction(metric, comparison_pair_id), color = metric_class),
      linewidth = .25, alpha = .10
    ) +
    geom_point(
      data = tr,
      aes(x_pos, A_state, color = metric_class),
      size = .34, alpha = .12
    ) +
    geom_linerange(
      data = sm,
      aes(x_pos, ymin = A_q25, ymax = A_q75, color = metric_class),
      linewidth = .72, alpha = .48
    ) +
    geom_line(
      data = sm,
      aes(x_pos, A_median, group = metric_class, color = metric_class),
      linewidth = .68, alpha = .90
    ) +
    geom_point(
      data = sm,
      aes(x_pos, A_median, color = metric_class),
      shape = 18, size = 1.45
    ) +
    scale_color_ms_metric(guide = "none") +
    scale_x_continuous(
      breaks = 1:3, labels = c("Low", "Middle", "High"),
      limits = c(.70, 3.30), expand = expansion(mult = c(0, 0))
    ) +
    scale_y_continuous(
      trans = scales::transform_asinh(),
      breaks = scales::breaks_extended(n = 4)
    ) +
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
      linewidth = .24, alpha = .09
    ) +
    geom_point(
      data = tr,
      aes(x_pos, direction_state, color = metric_class),
      size = .31, alpha = .11
    ) +
    geom_linerange(
      data = sm,
      aes(x_pos, ymin = direction_q25, ymax = direction_q75, color = metric_class),
      linewidth = .66, alpha = .46
    ) +
    geom_line(
      data = sm,
      aes(x_pos, direction_median, group = metric_class, color = metric_class),
      linewidth = .62, alpha = .90
    ) +
    geom_point(
      data = sm,
      aes(x_pos, direction_median, color = metric_class),
      shape = 18, size = 1.30
    ) +
    scale_color_ms_metric(guide = "none") +
    scale_x_continuous(
      breaks = 1:3, labels = c("Low", "Middle", "High"),
      limits = c(.70, 3.30), expand = expansion(mult = c(0, 0))
    ) +
    scale_y_continuous(
      limits = c(-1.03, 1.03), breaks = c(-1, 0, 1),
      expand = expansion(mult = c(0, 0))
    ) +
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
    p_mag, p_dir, ncol = 1, rel_heights = c(.77, .23),
    align = "v", axis = "lr", greedy = TRUE
  )
}

state_blocks <- lapply(DIMENSIONS, make_conditional_state_block)
p2a_core <- cowplot::plot_grid(
  plotlist = state_blocks, ncol = 2,
  align = "hv", axis = "tblr", greedy = TRUE
)
p2a <- cowplot::ggdraw() +
  cowplot::draw_plot(p2a_core, x = 0, y = 0, width = 1, height = .965) +
  cowplot::draw_label(
    "a  Conditional distortion geometry across exposure state",
    x = .002, y = .998, hjust = 0, vjust = 1,
    fontface = "bold", size = 7.0
  )

'''

fig2 = replace_between(
    fig2,
    "# a. Exposure-state dependence is shown as distributions rather than spaghetti\n",
    "# Transition-resolved state geometry.",
    fig2_a,
)

fig2_c = r'''
# c. Independent contextual information from participant-grouped CV. The two
# increments are evaluated only on matched test sets and therefore isolate the
# held-out information added by external context beyond state, and vice versa.
metric_direction_shift <- transition_state |>
  filter(is.finite(delta_direction_HL)) |>
  group_by(dimension, metric, metric_class) |>
  summarise(delta_direction_HL = median(delta_direction_HL, na.rm = TRUE), .groups = "drop") |>
  mutate(metric_class = factor(metric_class, levels = METRIC_CLASSES))

direction_shift_summary <- metric_direction_shift |>
  group_by(dimension, metric_class) |>
  summarise(
    n_metrics = n_distinct(metric),
    shift_median = median(delta_direction_HL, na.rm = TRUE),
    shift_q25 = quantile(delta_direction_HL, .25, na.rm = TRUE, names = FALSE),
    shift_q75 = quantile(delta_direction_HL, .75, na.rm = TRUE, names = FALSE),
    .groups = "drop"
  )

CONTEXT_COLORS <- c("External beyond state" = MS_PRIMARY, "State beyond external" = MS_SECONDARY)
CONTEXT_SHAPES <- c("External beyond state" = 16, "State beyond external" = 17)

if (nrow(context_task)) {
  context_plot <- context_task |>
    mutate(
      dimension_num = as.integer(dimension),
      y_pos = dimension_num + if_else(information == "External beyond state", -.11, .11)
    )
  context_plot_summary <- context_summary |>
    mutate(
      dimension_num = as.integer(dimension),
      y_pos = dimension_num + if_else(information == "External beyond state", -.11, .11)
    )

  p2c <- ggplot(context_plot, aes(delta_r2, y_pos, color = information, shape = information)) +
    geom_vline(xintercept = 0, linewidth = .30, color = "#9DA2A5") +
    geom_point(
      position = position_jitter(width = 0, height = .035, seed = 56),
      size = .56, alpha = .18
    ) +
    geom_segment(
      data = context_plot_summary,
      aes(x = delta_q25, xend = delta_q75, y = y_pos, yend = y_pos, color = information),
      inherit.aes = FALSE, linewidth = .92, alpha = .58, lineend = "round"
    ) +
    geom_point(
      data = context_plot_summary,
      aes(delta_median, y_pos, color = information, shape = information),
      inherit.aes = FALSE, size = 1.50
    ) +
    facet_wrap(~outcome_label, nrow = 1) +
    scale_color_manual(values = CONTEXT_COLORS, drop = FALSE) +
    scale_shape_manual(values = CONTEXT_SHAPES, drop = FALSE) +
    scale_x_continuous(
      trans = scales::transform_asinh(),
      breaks = scales::breaks_extended(n = 4)
    ) +
    scale_y_continuous(
      breaks = seq_along(DIMENSIONS),
      labels = unname(DIM_TITLES[DIMENSIONS]),
      limits = c(.55, length(DIMENSIONS) + .45)
    ) +
    guides(
      color = guide_legend(title = NULL, nrow = 2, byrow = TRUE,
                           override.aes = list(alpha = 1, size = 1.1)),
      shape = guide_legend(title = NULL, nrow = 2, byrow = TRUE,
                           override.aes = list(alpha = 1, size = 1.1))
    ) +
    labs(
      title = "c  Independent contextual information",
      x = "incremental participant-grouped CV R²", y = NULL
    ) +
    theme_rq2(base_size = 6.05, legend_position = "bottom") +
    theme(
      panel.grid.major.y = element_blank(),
      axis.line.y = element_blank(), axis.ticks.y = element_blank(),
      axis.text.y = element_text(size = 4.9),
      strip.text = element_text(size = 5.45),
      legend.text = element_text(size = 4.35),
      legend.key.width = grid::unit(2.7, "mm"),
      legend.spacing.x = grid::unit(.6, "mm"),
      panel.spacing = grid::unit(1.8, "mm")
    )
} else {
  p2c <- ggplot() + theme_void(base_family = MS_FONT) +
    annotate(
      "text", x = 0, y = 0,
      label = "c  Independent contextual information\nNo matched grouped-CV results",
      size = 2.2, colour = "#55595C"
    )
}

'''

fig2 = replace_between(
    fig2,
    "# c. Exposure-state dependence of distortion direction.\n",
    "# Keep the accepted asymmetric composition:",
    fig2_c,
)

fig2_comp = r'''
# Keep one visually dominant conditional-geometry panel above two orthogonal
# explanatory panels. The upper panel carries the empirical state response; the
# lower panels separate coefficient structure from held-out information.
p2bottom <- cowplot::plot_grid(
  p2b, p2c, ncol = 2, rel_widths = c(.60, .40),
  align = "hv", axis = "tblr", greedy = TRUE
)
p2body <- cowplot::plot_grid(
  p2a, p2bottom, ncol = 1, rel_heights = c(1.22, .78),
  align = "v", axis = "l", greedy = TRUE
)
p2 <- cowplot::plot_grid(
  metric_legend, p2body, ncol = 1, rel_heights = c(.040, 1),
  align = "v", greedy = TRUE
)
ms_plot_save(p2, file.path(OUT_DIR, "Fig2_RQ2.pdf"), 9.0, 6.9)
ms_plot_save(p2, file.path(OUT_DIR, "Fig2_RQ2.png"), 9.0, 6.9)

'''

fig2 = replace_between(
    fig2,
    "# Keep the accepted asymmetric composition:",
    "readr::write_csv(conditional_profile_summary |>",
    fig2_comp,
)

fig2_path.write_text(fig2, encoding="utf-8")

fig5_path = Path("scripts/15_plot_rq3.R")
fig5 = fig5_path.read_text(encoding="utf-8")

fig5_block = r'''
# =============================================================================
# Fig. 5 — joint temporal × duration sufficiency geometry
# =============================================================================

# Retain the frozen Pareto summaries for the supplementary audit view below.
pareto_base <- pareto |>
  filter(is.finite(resolution_s), is.finite(n_days)) |>
  mutate(
    ever_pareto = as.logical(ever_pareto),
    pareto_persistence = pmax(0, pmin(1, pareto_persistence)),
    metric_class = factor(metric_class, levels = METRIC_CLASSES)
  )

# Main-text Fig. 5 is deliberately not tolerance-on-the-x-axis: Fig. 4 already
# owns tolerance-response. Here tolerance is an intrinsic property of each joint
# configuration (the entry tolerance), and the other panels describe the local
# geometry of the resulting temporal × duration surface.
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

# a. Joint entry-tolerance surface: darker cells require a more permissive
# fidelity tolerance before the configuration becomes sufficient. Unresolved
# upper boundaries remain NA and therefore visually distinct from poor fidelity.
entry_surface <- joint_plot_base |>
  group_by(resolution_s, resolution_rank, n_days) |>
  summarise(
    n_records = n(),
    n_resolved = sum(status == "resolved" & is.finite(epsilon_entry)),
    resolved_fraction = n_resolved / n_records,
    epsilon_entry_median = safe_median(epsilon_entry[status == "resolved"]),
    epsilon_entry_q25 = safe_q(epsilon_entry[status == "resolved"], .25),
    epsilon_entry_q75 = safe_q(epsilon_entry[status == "resolved"], .75),
    .groups = "drop"
  )

entry_grid <- tidyr::crossing(
  resolution_rank = seq_along(fig5_res_levels),
  n_days = fig5_days
) |>
  left_join(
    entry_surface |>
      select(
        resolution_rank, n_days, n_records, n_resolved, resolved_fraction,
        epsilon_entry_median, epsilon_entry_q25, epsilon_entry_q75
      ),
    by = c("resolution_rank", "n_days")
  )

p5a <- ggplot(entry_grid, aes(resolution_rank, n_days, fill = epsilon_entry_median)) +
  geom_tile(color = "white", linewidth = .42) +
  scale_fill_ms_sequential(
    trans = scales::transform_asinh(),
    na.value = "#ECEEEF",
    name = "entry tolerance ε"
  ) +
  scale_x_continuous(
    breaks = seq_along(fig5_res_levels), labels = fig5_res_labels,
    expand = expansion(add = .02)
  ) +
  scale_y_continuous(
    breaks = fig5_days, labels = paste0(fig5_days, " d"),
    expand = expansion(add = .02)
  ) +
  coord_fixed(ratio = .58, clip = "off") +
  labs(
    title = "a  Joint configurations differ in the tolerance required for sufficiency",
    subtitle = "darker = more permissive tolerance required; grey = unresolved within the observed higher-state domain",
    x = "temporal resolution  (low → high burden)", y = "monitoring duration"
  ) +
  theme_rq3(base_size = 6.5, legend_position = "bottom") +
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_text(angle = 28, hjust = 1, size = 5.25),
    plot.subtitle = element_text(size = 4.85, colour = "#666A6D", margin = margin(t = -1, b = 2)),
    legend.text = element_text(size = 4.8),
    legend.title = element_text(size = 4.9),
    legend.key.width = grid::unit(7.0, "mm")
  )

# b. Conditional marginal returns. Within each fixed support × placement ×
# optical × metric facet, compare adjacent joint states along one burden axis
# while holding the other axis fixed. Positive values mean the added burden
# reduces the tolerance required for sufficiency.
resolved_cells <- joint_plot_base |>
  filter(status == "resolved", is.finite(epsilon_entry)) |>
  transmute(
    support_id, placement, optical, metric, metric_class,
    resolution_s, resolution_rank, n_days, epsilon_entry
  ) |>
  distinct()

duration_from <- resolved_cells |>
  transmute(
    support_id, placement, optical, metric, metric_class, resolution_s, resolution_rank,
    n_days_from = n_days, n_days_to = n_days + 1,
    epsilon_from = epsilon_entry
  )
duration_to <- resolved_cells |>
  transmute(
    support_id, placement, optical, metric, metric_class, resolution_s, resolution_rank,
    n_days_to = n_days, epsilon_to = epsilon_entry
  )
duration_gain_raw <- inner_join(
  duration_from, duration_to,
  by = c(
    "support_id", "placement", "optical", "metric", "metric_class",
    "resolution_s", "resolution_rank", "n_days_to"
  )
) |>
  mutate(
    gain = epsilon_from - epsilon_to,
    transition = paste0(n_days_from, "→", n_days_to, " d")
  ) |>
  filter(is.finite(gain))

temporal_from <- resolved_cells |>
  transmute(
    support_id, placement, optical, metric, metric_class, n_days,
    resolution_rank_from = resolution_rank,
    resolution_rank_to = resolution_rank + 1,
    resolution_s_from = resolution_s,
    epsilon_from = epsilon_entry
  )
temporal_to <- resolved_cells |>
  transmute(
    support_id, placement, optical, metric, metric_class, n_days,
    resolution_rank_to = resolution_rank,
    resolution_s_to = resolution_s,
    epsilon_to = epsilon_entry
  )
temporal_gain_raw <- inner_join(
  temporal_from, temporal_to,
  by = c(
    "support_id", "placement", "optical", "metric", "metric_class",
    "n_days", "resolution_rank_to"
  )
) |>
  mutate(
    gain = epsilon_from - epsilon_to,
    transition = paste0(format_resolution5(resolution_s_from), "→", format_resolution5(resolution_s_to))
  ) |>
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

p5b_duration <- ggplot(duration_metric_gain, aes(resolution_rank, gain)) +
  geom_hline(yintercept = 0, linewidth = .28, color = "#9DA2A5") +
  geom_point(
    position = position_jitter(width = .07, height = 0, seed = 81),
    size = .45, color = "#A7B0B5", alpha = .24
  ) +
  geom_segment(
    data = duration_gain_summary,
    aes(x = resolution_rank, xend = resolution_rank, y = gain_q25, yend = gain_q75),
    inherit.aes = FALSE, linewidth = .95, color = MS_PRIMARY, alpha = .58, lineend = "round"
  ) +
  geom_line(
    data = duration_gain_summary,
    aes(resolution_rank, gain_median),
    inherit.aes = FALSE, linewidth = .72, color = MS_PRIMARY
  ) +
  geom_point(
    data = duration_gain_summary,
    aes(resolution_rank, gain_median),
    inherit.aes = FALSE, shape = 18, size = 1.55, color = MS_PRIMARY
  ) +
  scale_x_continuous(
    breaks = seq_along(fig5_res_levels), labels = fig5_res_labels,
    expand = expansion(add = .22)
  ) +
  scale_y_continuous(
    limits = c(-gain_limit, gain_limit),
    breaks = scales::breaks_extended(n = 4)
  ) +
  labs(
    title = "Added duration | temporal state fixed",
    x = "temporal resolution", y = "reduction in entry tolerance"
  ) +
  theme_rq3(base_size = 5.8) +
  theme(
    panel.grid.major.x = element_blank(),
    axis.text.x = element_text(angle = 32, hjust = 1, size = 4.55),
    plot.title = element_text(size = 5.45, hjust = .5),
    plot.margin = margin(2, 2, 2, 2)
  )

p5b_temporal <- ggplot(temporal_metric_gain, aes(n_days, gain)) +
  geom_hline(yintercept = 0, linewidth = .28, color = "#9DA2A5") +
  geom_point(
    position = position_jitter(width = .07, height = 0, seed = 83),
    size = .45, color = "#A7B0B5", alpha = .24
  ) +
  geom_segment(
    data = temporal_gain_summary,
    aes(x = n_days, xend = n_days, y = gain_q25, yend = gain_q75),
    inherit.aes = FALSE, linewidth = .95, color = MS_SECONDARY, alpha = .58, lineend = "round"
  ) +
  geom_line(
    data = temporal_gain_summary,
    aes(n_days, gain_median),
    inherit.aes = FALSE, linewidth = .72, color = MS_SECONDARY
  ) +
  geom_point(
    data = temporal_gain_summary,
    aes(n_days, gain_median),
    inherit.aes = FALSE, shape = 18, size = 1.55, color = MS_SECONDARY
  ) +
  scale_x_continuous(
    breaks = fig5_days, labels = paste0(fig5_days, " d"),
    expand = expansion(add = .22)
  ) +
  scale_y_continuous(
    limits = c(-gain_limit, gain_limit),
    breaks = scales::breaks_extended(n = 4)
  ) +
  labs(
    title = "Temporal refinement | duration fixed",
    x = "monitoring duration", y = NULL
  ) +
  theme_rq3(base_size = 5.8) +
  theme(
    panel.grid.major.x = element_blank(),
    axis.text.x = element_text(size = 4.7),
    axis.text.y = element_blank(), axis.ticks.y = element_blank(),
    axis.line.y = element_blank(),
    plot.title = element_text(size = 5.45, hjust = .5),
    plot.margin = margin(2, 2, 2, 2)
  )

p5b_core <- cowplot::plot_grid(
  p5b_duration, p5b_temporal, ncol = 2, rel_widths = c(1.05, .95),
  align = "hv", axis = "tblr", greedy = TRUE
)
p5b <- cowplot::ggdraw() +
  cowplot::draw_plot(p5b_core, x = 0, y = 0, width = 1, height = .93) +
  cowplot::draw_label(
    "b  Conditional marginal returns reveal joint trade-offs",
    x = .002, y = .998, hjust = 0, vjust = 1,
    fontface = "bold", size = 6.3
  )

# c. Representation-class burden orientation. Each metric contributes one
# temporal-refinement benefit and one added-duration benefit; class medians and
# IQRs show whether efficient fidelity depends more on one burden axis.
duration_metric_overall <- duration_metric_gain |>
  group_by(metric, metric_class) |>
  summarise(duration_gain = median(gain, na.rm = TRUE), .groups = "drop")
temporal_metric_overall <- temporal_metric_gain |>
  group_by(metric, metric_class) |>
  summarise(temporal_gain = median(gain, na.rm = TRUE), .groups = "drop")

class_orientation_metric <- inner_join(
  temporal_metric_overall, duration_metric_overall,
  by = c("metric", "metric_class")
) |>
  filter(is.finite(temporal_gain), is.finite(duration_gain))

class_orientation_summary <- class_orientation_metric |>
  group_by(metric_class) |>
  summarise(
    n_metrics = n_distinct(metric),
    temporal_median = median(temporal_gain, na.rm = TRUE),
    temporal_q25 = quantile(temporal_gain, .25, na.rm = TRUE, names = FALSE),
    temporal_q75 = quantile(temporal_gain, .75, na.rm = TRUE, names = FALSE),
    duration_median = median(duration_gain, na.rm = TRUE),
    duration_q25 = quantile(duration_gain, .25, na.rm = TRUE, names = FALSE),
    duration_q75 = quantile(duration_gain, .75, na.rm = TRUE, names = FALSE),
    .groups = "drop"
  )

overall_orientation <- class_orientation_metric |>
  summarise(
    temporal_gain = median(temporal_gain, na.rm = TRUE),
    duration_gain = median(duration_gain, na.rm = TRUE)
  )

orientation_limit <- ms_symmetric_limit(
  class_orientation_metric$temporal_gain, class_orientation_metric$duration_gain,
  class_orientation_summary$temporal_q25, class_orientation_summary$temporal_q75,
  class_orientation_summary$duration_q25, class_orientation_summary$duration_q75,
  pad = 1.08, fallback = 1
)

p5c <- ggplot(
  class_orientation_metric,
  aes(temporal_gain, duration_gain, color = metric_class)
) +
  geom_hline(yintercept = 0, linewidth = .28, color = "#A7ABAE") +
  geom_vline(xintercept = 0, linewidth = .28, color = "#A7ABAE") +
  geom_point(size = .58, alpha = .20) +
  geom_segment(
    data = class_orientation_summary,
    aes(
      x = temporal_q25, xend = temporal_q75,
      y = duration_median, yend = duration_median,
      color = metric_class
    ),
    inherit.aes = FALSE, linewidth = .95, alpha = .50, lineend = "round"
  ) +
  geom_segment(
    data = class_orientation_summary,
    aes(
      x = temporal_median, xend = temporal_median,
      y = duration_q25, yend = duration_q75,
      color = metric_class
    ),
    inherit.aes = FALSE, linewidth = .95, alpha = .50, lineend = "round"
  ) +
  geom_point(
    data = class_orientation_summary,
    aes(temporal_median, duration_median, color = metric_class),
    inherit.aes = FALSE, shape = 18, size = 2.05
  ) +
  geom_point(
    data = overall_orientation,
    aes(temporal_gain, duration_gain),
    inherit.aes = FALSE, shape = 4, stroke = .70, size = 2.0, color = "#4C5053"
  ) +
  scale_color_ms_metric() +
  scale_x_continuous(
    limits = c(-orientation_limit, orientation_limit),
    breaks = scales::breaks_extended(n = 4)
  ) +
  scale_y_continuous(
    limits = c(-orientation_limit, orientation_limit),
    breaks = scales::breaks_extended(n = 4)
  ) +
  coord_equal() +
  labs(
    title = "c  Representation classes differ in burden orientation",
    x = "benefit of temporal refinement",
    y = "benefit of added duration"
  ) +
  theme_rq3(base_size = 5.85, legend_position = "bottom") +
  theme(
    panel.grid = element_blank(),
    legend.title = element_blank(),
    legend.text = element_text(size = 4.35),
    legend.key.width = grid::unit(2.9, "mm"),
    plot.title = element_text(size = 6.0)
  ) +
  guides(color = guide_legend(nrow = 2, byrow = TRUE, override.aes = list(alpha = 1, size = 1.2)))

fig5_bottom <- cowplot::plot_grid(
  p5b, p5c, ncol = 2, rel_widths = c(.60, .40),
  align = "hv", axis = "tblr", greedy = TRUE
)
fig5_body <- cowplot::plot_grid(
  p5a, fig5_bottom, ncol = 1, rel_heights = c(1.15, .85),
  align = "v", axis = "l", greedy = TRUE
)
ms_plot_save(fig5_body, file.path(OUT_DIR, "Fig5_RQ3.pdf"), 9.0, 6.2)
ms_plot_save(fig5_body, file.path(OUT_DIR, "Fig5_RQ3.png"), 9.0, 6.2)

readr::write_csv(
  entry_surface,
  file.path("results", "rq3", "fig5_joint_entry_tolerance_surface.csv"), na = ""
)
readr::write_csv(
  bind_rows(
    duration_metric_gain |>
      transmute(
        axis = "duration_gain_by_temporal_state", condition = format_resolution5(resolution_s),
        metric, metric_class = as.character(metric_class), gain
      ),
    temporal_metric_gain |>
      transmute(
        axis = "temporal_gain_by_duration", condition = paste0(n_days, " d"),
        metric, metric_class = as.character(metric_class), gain
      )
  ),
  file.path("results", "rq3", "fig5_conditional_marginal_returns.csv"), na = ""
)
readr::write_csv(
  class_orientation_summary |>
    mutate(metric_class = as.character(metric_class)),
  file.path("results", "rq3", "fig5_class_burden_orientation.csv"), na = ""
)

'''

fig5 = replace_between(
    fig5,
    "# =============================================================================\n# Fig. 5 — joint temporal × duration Pareto landscape\n# =============================================================================\n",
    "# Supplement: preserve support × placement × optical explicitly as an audit view.",
    fig5_block,
)
fig5 = fig5.replace(
    '      "rq3_pareto_frontiers",\n',
    '      "rq3_joint_summary+pareto_frontiers",\n',
    1,
)
fig5 = fig5.replace(
    'message("RQ3 v5 figures complete: compact single-dimension sufficiency and joint Pareto landscapes.")',
    'message("RQ3 v5 figures complete: compact single-dimension sufficiency, joint sufficiency geometry, and Pareto audit views.")',
)
fig5_path.write_text(fig5, encoding="utf-8")
