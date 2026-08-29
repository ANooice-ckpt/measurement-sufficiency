from pathlib import Path


def replace_between(text: str, start: str, end: str, replacement: str) -> str:
    i = text.find(start)
    if i < 0:
        raise RuntimeError(f"start marker not found: {start[:80]!r}")
    j = text.find(end, i + len(start))
    if j < 0:
        raise RuntimeError(f"end marker not found: {end[:80]!r}")
    return text[:i] + replacement.rstrip() + "\n\n" + text[j:]


def truncate_from(text: str, marker: str) -> str:
    i = text.find(marker)
    if i < 0:
        raise RuntimeError(f"truncate marker not found: {marker[:80]!r}")
    return text[:i].rstrip() + "\n"


# -----------------------------------------------------------------------------
# Fig. 2: keep the new conditional-geometry hero panel, replace the invalid
# matched-delta-R2 panel with directly estimable joint-model grouped-CV R2, and
# delete the historical second main-figure writer entirely.
# -----------------------------------------------------------------------------
p = Path("scripts/13_plot_rq2.R")
text = p.read_text(encoding="utf-8")

c_start = "# c. Independent contextual information from participant-grouped CV. The two\n"
c_end = "# Keep one visually dominant conditional-geometry panel above two orthogonal\n"
new_c = r'''# c. Out-of-sample contextual predictability from the joint contextual model.
# This uses the participant-grouped CV R2 already produced by RQ2. It does not
# subtract model-family R2 values evaluated on different complete-case samples.
joint_cv_metric <- performance |>
  filter(
    str_detect(validation_scheme, "^participant_grouped"),
    model_family == "joint", is.finite(r2)
  ) |>
  group_by(dimension, comparison_pair_id, metric, outcome) |>
  summarise(
    r2 = median(r2, na.rm = TRUE),
    n_test = max(n_test, na.rm = TRUE),
    .groups = "drop"
  ) |>
  group_by(dimension, metric, outcome) |>
  summarise(
    r2 = median(r2, na.rm = TRUE),
    n_test = max(n_test, na.rm = TRUE),
    .groups = "drop"
  ) |>
  left_join(metric_class_lookup, by = "metric") |>
  mutate(
    metric_class = factor(metric_class, levels = METRIC_CLASSES),
    dimension = factor(
      dimension, levels = DIMENSIONS,
      labels = unname(DIM_TITLES[DIMENSIONS])
    ),
    outcome_label = recode(
      outcome,
      signed = "Signed distortion",
      magnitude = "Absolute distortion",
      .default = outcome
    )
  ) |>
  filter(!is.na(dimension), !is.na(metric_class), is.finite(r2))

joint_cv_summary <- joint_cv_metric |>
  group_by(dimension, outcome_label, metric_class) |>
  summarise(
    n_metrics = n_distinct(metric),
    r2_median = median(r2, na.rm = TRUE),
    r2_q25 = quantile(r2, .25, na.rm = TRUE, names = FALSE),
    r2_q75 = quantile(r2, .75, na.rm = TRUE, names = FALSE),
    .groups = "drop"
  )

if (nrow(joint_cv_metric)) {
  joint_cv_cutoff <- as.numeric(
    quantile(abs(joint_cv_metric$r2), .99, na.rm = TRUE, names = FALSE, type = 8)
  )
  summary_extent <- max(abs(c(
    joint_cv_summary$r2_median,
    joint_cv_summary$r2_q25,
    joint_cv_summary$r2_q75
  )), na.rm = TRUE)
  joint_cv_limit <- max(joint_cv_cutoff, summary_extent, na.rm = TRUE) * 1.04
  if (!is.finite(joint_cv_limit) || joint_cv_limit <= 0) joint_cv_limit <- 1

  joint_cv_plot <- joint_cv_metric |>
    mutate(
      dimension_num = as.integer(dimension),
      class_num = as.integer(metric_class),
      class_offset = (class_num - (length(METRIC_CLASSES) + 1) / 2) * .060,
      y_pos = dimension_num + class_offset,
      displayed = abs(r2) <= joint_cv_limit + 1e-12
    ) |>
    filter(displayed)

  joint_cv_plot_summary <- joint_cv_summary |>
    mutate(
      dimension_num = as.integer(dimension),
      class_num = as.integer(metric_class),
      class_offset = (class_num - (length(METRIC_CLASSES) + 1) / 2) * .060,
      y_pos = dimension_num + class_offset
    )

  p2c <- ggplot(joint_cv_plot, aes(r2, y_pos, color = metric_class)) +
    geom_vline(xintercept = 0, linewidth = .30, color = "#9DA2A5") +
    geom_point(
      position = position_jitter(width = 0, height = .018, seed = 56),
      size = .54, alpha = .20
    ) +
    geom_segment(
      data = joint_cv_plot_summary,
      aes(x = r2_q25, xend = r2_q75, y = y_pos, yend = y_pos, color = metric_class),
      inherit.aes = FALSE, linewidth = .86, alpha = .52, lineend = "round"
    ) +
    geom_point(
      data = joint_cv_plot_summary,
      aes(r2_median, y_pos, color = metric_class),
      inherit.aes = FALSE, shape = 18, size = 1.42
    ) +
    facet_wrap(~outcome_label, nrow = 1) +
    scale_color_ms_metric(guide = "none") +
    scale_x_continuous(
      limits = c(-joint_cv_limit, joint_cv_limit),
      breaks = scales::breaks_extended(n = 4)
    ) +
    scale_y_continuous(
      breaks = seq_along(DIMENSIONS),
      labels = unname(DIM_TITLES[DIMENSIONS]),
      limits = c(.55, length(DIMENSIONS) + .45)
    ) +
    labs(
      title = "c  Out-of-sample contextual predictability",
      subtitle = "joint contextual model; participant-grouped cross-validation",
      x = "participant-grouped CV R²", y = NULL
    ) +
    theme_rq2(base_size = 6.05) +
    theme(
      panel.grid.major.y = element_blank(),
      axis.line.y = element_blank(), axis.ticks.y = element_blank(),
      axis.text.y = element_text(size = 4.9),
      strip.text = element_text(size = 5.35),
      plot.subtitle = element_text(
        size = 4.35, colour = "#666A6D", margin = margin(t = -1, b = 2)
      ),
      panel.spacing = grid::unit(1.8, "mm")
    )
} else {
  p2c <- ggplot() + theme_void(base_family = MS_FONT) +
    annotate(
      "text", x = 0, y = 0,
      label = "c  Out-of-sample contextual predictability\nNo joint grouped-CV results",
      size = 2.2, colour = "#55595C"
    )
}

readr::write_csv(
  joint_cv_metric |>
    mutate(
      dimension = as.character(dimension),
      metric_class = as.character(metric_class)
    ),
  file.path("results", "rq2", "fig2_joint_context_cv.csv"), na = ""
)'''
text = replace_between(text, c_start, c_end, new_c)

legacy_fig2 = "# Main Fig. 2a deliberately keeps a linear y scale. Facets retain their own\n"
text = truncate_from(text, legacy_fig2)

# Single-writer and stale-title invariants.
if text.count('file.path(OUT_DIR, "Fig2_RQ2.png")') != 1:
    raise RuntimeError("Fig2_RQ2.png must have exactly one writer")
if text.count('file.path(OUT_DIR, "Fig2_RQ2.pdf")') != 1:
    raise RuntimeError("Fig2_RQ2.pdf must have exactly one writer")
if "a  Conditional distortion magnitude across exposure state" in text:
    raise RuntimeError("legacy Fig. 2a title still present")
if "No matched grouped-CV results" in text:
    raise RuntimeError("invalid matched grouped-CV fallback still present")

p.write_text(text, encoding="utf-8")


# -----------------------------------------------------------------------------
# Fig. 5: replace the first/main Fig. 5 block with one definitive implementation
# and remove the historical occupancy-based second writer at the file tail.
# Main layout: overall entry-tolerance heatmap + class-deviation heatmap array,
# then a compact orthogonal marginal-return panel.
# -----------------------------------------------------------------------------
p = Path("scripts/15_plot_rq3.R")
text = p.read_text(encoding="utf-8")

f5_start = "# Main-text Fig. 5 is deliberately not tolerance-on-the-x-axis: Fig. 4 already\n"
f5_end = "# Supplement: preserve support × placement × optical explicitly as an audit view.\n"
new_f5 = r'''# Main-text Fig. 5 maps the joint temporal-resolution × duration design space.
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
)'''
text = replace_between(text, f5_start, f5_end, new_f5)

legacy_fig5 = "# Main-text redesign for Fig. 5.\n"
text = truncate_from(text, legacy_fig5)

if text.count('file.path(OUT_DIR, "Fig5_RQ3.png")') != 1:
    raise RuntimeError("Fig5_RQ3.png must have exactly one writer")
if text.count('file.path(OUT_DIR, "Fig5_RQ3.pdf")') != 1:
    raise RuntimeError("Fig5_RQ3.pdf must have exactly one writer")
if "Persistent Pareto occupancy across joint measurement burden" in text:
    raise RuntimeError("legacy Fig. 5 occupancy title still present")
if "pareto_global_display" in text:
    raise RuntimeError("legacy main-text Pareto display block still present")

p.write_text(text, encoding="utf-8")

print("Repaired Fig. 2 and Fig. 5 with single canonical writers.")
