from pathlib import Path


def replace_once(text: str, old: str, new: str, label: str) -> str:
    n = text.count(old)
    if n != 1:
        raise RuntimeError(f"{label}: expected exactly one match, found {n}")
    return text.replace(old, new, 1)


def replace_between(text: str, start: str, end: str, replacement: str, label: str) -> str:
    i = text.find(start)
    if i < 0:
        raise RuntimeError(f"{label}: start marker not found")
    j = text.find(end, i + len(start))
    if j < 0:
        raise RuntimeError(f"{label}: end marker not found")
    return text[:i] + replacement.rstrip() + "\n\n" + text[j:]


# =============================================================================
# RQ2 / Fig. 2 and Fig. 3
# =============================================================================
p = Path("scripts/13_plot_rq2.R")
text = p.read_text(encoding="utf-8")

anchor = 'metric_legend <- ms_metric_legend(text_size = 5.35, point_size = 1.5, key_width_mm = 3.5)\n'
helpers = r'''
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
'''
text = replace_once(text, anchor, helpers, "insert robust display helpers")

start = 'make_conditional_state_block <- function(dim_name) {\n'
end = 'state_blocks <- lapply(DIMENSIONS, make_conditional_state_block)\n'
new_fun = r'''make_conditional_state_block <- function(dim_name) {
  tr <- conditional_trajectory_state |> filter(dimension == dim_name)
  sm <- conditional_profile_summary |> filter(dimension == dim_name)

  mag_limit <- robust_upper_display_limit(
    tr$A_state,
    c(sm$A_median, sm$A_q25, sm$A_q75),
    prob = .95, pad = 1.08, fallback = .25
  )
  dir_range <- robust_bounded_display_range(
    tr$direction_state,
    c(sm$direction_median, sm$direction_q25, sm$direction_q75),
    probs = c(.05, .95), lower = -1, upper = 1, min_span = .30,
    pad_fraction = .08
  )

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
      linewidth = .72, alpha = .94
    ) +
    geom_point(
      data = sm,
      aes(x_pos, A_median, color = metric_class),
      shape = 18, size = 1.50
    ) +
    scale_color_ms_metric(guide = "none") +
    scale_x_continuous(
      breaks = 1:3, labels = c("Low", "Middle", "High"),
      limits = c(.70, 3.30), expand = expansion(mult = c(0, 0))
    ) +
    scale_y_continuous(breaks = scales::breaks_extended(n = 4)) +
    coord_cartesian(ylim = c(0, mag_limit), clip = "on") +
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
      linewidth = .26, alpha = .11
    ) +
    geom_point(
      data = tr,
      aes(x_pos, direction_state, color = metric_class),
      size = .32, alpha = .12
    ) +
    geom_linerange(
      data = sm,
      aes(x_pos, ymin = direction_q25, ymax = direction_q75, color = metric_class),
      linewidth = .68, alpha = .48
    ) +
    geom_line(
      data = sm,
      aes(x_pos, direction_median, group = metric_class, color = metric_class),
      linewidth = .68, alpha = .94
    ) +
    geom_point(
      data = sm,
      aes(x_pos, direction_median, color = metric_class),
      shape = 18, size = 1.34
    ) +
    scale_color_ms_metric(guide = "none") +
    scale_x_continuous(
      breaks = 1:3, labels = c("Low", "Middle", "High"),
      limits = c(.70, 3.30), expand = expansion(mult = c(0, 0))
    ) +
    scale_y_continuous(breaks = scales::breaks_extended(n = 3)) +
    coord_cartesian(ylim = dir_range, clip = "on") +
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
    p_mag, p_dir, ncol = 1, rel_heights = c(.76, .24),
    align = "v", axis = "lr", greedy = TRUE
  )
}
'''
text = replace_between(text, start, end, new_fun, "replace Fig2a block function")

start = 'PREDICTOR_COLORS <- c("External context" = MS_PRIMARY, "Exposure state" = MS_SECONDARY)\n'
end = '# Retain predictor-family grouped-CV increments as a supplementary validation of\n'
new_coef = r'''PREDICTOR_COLORS <- c("External context" = MS_PRIMARY, "Exposure state" = MS_SECONDARY)
OUTCOME_SHAPES <- c("Signed" = 16, "Absolute" = 17)
coef_limit <- robust_symmetric_display_limit(
  coef_metric$estimate,
  c(coef_summary$estimate_median, coef_summary$estimate_q25, coef_summary$estimate_q75),
  prob = .95, pad = 1.08, fallback = .25
)
coef_metric_display <- coef_metric |>
  mutate(displayed_in_fig2b = is.finite(estimate) & abs(estimate) <= coef_limit)

if (nrow(coef_metric)) {
  p2b <- ggplot(
    coef_metric_display |> filter(displayed_in_fig2b),
    aes(estimate, y_pos, color = predictor_family, shape = outcome_label)
  ) +
    geom_vline(xintercept = 0, linewidth = .30, color = "#9DA2A5") +
    geom_point(position = position_jitter(width = 0, height = .035, seed = 54),
               size = .52, alpha = .20) +
    geom_segment(
      data = coef_summary,
      aes(x = estimate_q25, xend = estimate_q75, y = y_pos, yend = y_pos,
          color = predictor_family),
      inherit.aes = FALSE, linewidth = .92, alpha = .60, lineend = "round"
    ) +
    geom_point(
      data = coef_summary,
      aes(estimate_median, y_pos, color = predictor_family, shape = outcome_label),
      inherit.aes = FALSE, size = 1.50
    ) +
    facet_wrap(~dimension, ncol = 2) +
    scale_color_manual(values = PREDICTOR_COLORS, drop = FALSE) +
    scale_shape_manual(values = OUTCOME_SHAPES, drop = FALSE) +
    scale_y_continuous(
      breaks = seq_along(levels(coef_metric$predictor)),
      labels = levels(coef_metric$predictor),
      limits = c(.55, length(levels(coef_metric$predictor)) + .45)
    ) +
    scale_x_continuous(
      limits = c(-coef_limit, coef_limit),
      breaks = scales::breaks_extended(n = 5)
    ) +
    guides(
      color = guide_legend(title = NULL, nrow = 1, order = 1,
                           override.aes = list(alpha = 1, size = 1.15)),
      shape = guide_legend(title = NULL, nrow = 1, order = 2,
                           override.aes = list(alpha = 1, size = 1.15))
    ) +
    labs(
      title = "b  Contextual predictors of distortion",
      subtitle = "raw points show central 95%; medians and IQRs use all estimates",
      x = "standardized joint-model coefficient", y = NULL
    ) +
    theme_rq2(base_size = 6.1, legend_position = "bottom") +
    theme(
      panel.grid.major.y = element_blank(), axis.line.y = element_blank(), axis.ticks.y = element_blank(),
      axis.text.y = element_text(size = 4.5), strip.text = element_text(size = 5.35),
      plot.subtitle = element_text(size = 4.35, colour = "#666A6D", margin = margin(t = -1, b = 1.5)),
      legend.text = element_text(size = 4.45), legend.key.width = grid::unit(2.8, "mm"),
      legend.spacing.x = grid::unit(.8, "mm"), panel.spacing = grid::unit(1.8, "mm")
    )
} else {
  p2b <- ggplot() + theme_void(base_family = MS_FONT) +
    annotate("text", x = 0, y = 0,
             label = "b  Contextual predictors of distortion\nNo joint-model coefficients",
             size = 2.2, colour = "#55595C")
}

readr::write_csv(
  coef_metric_display |>
    mutate(
      dimension = as.character(dimension), predictor = as.character(predictor),
      predictor_family = as.character(predictor_family), outcome_label = as.character(outcome_label)
    ),
  file.path("results", "rq2", "fig2_context_predictor_display_diagnostics.csv"), na = ""
)
'''
text = replace_between(text, start, end, new_coef, "replace Fig2b robust display")

start = 'if (nrow(joint_cv_metric)) {\n'
end = 'readr::write_csv(\n  joint_cv_metric |>\n'
new_cv = r'''if (nrow(joint_cv_metric)) {
  joint_cv_limit <- robust_symmetric_display_limit(
    joint_cv_metric$r2,
    c(joint_cv_summary$r2_median, joint_cv_summary$r2_q25, joint_cv_summary$r2_q75),
    prob = .95, pad = 1.08, fallback = .50
  )

  joint_cv_plot <- joint_cv_metric |>
    mutate(
      dimension_num = as.integer(dimension),
      class_num = as.integer(metric_class),
      class_offset = (class_num - (length(METRIC_CLASSES) + 1) / 2) * .060,
      y_pos = dimension_num + class_offset,
      displayed_in_fig2c = is.finite(r2) & abs(r2) <= joint_cv_limit
    ) |>
    filter(displayed_in_fig2c)

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
      size = .54, alpha = .22
    ) +
    geom_segment(
      data = joint_cv_plot_summary,
      aes(x = r2_q25, xend = r2_q75, y = y_pos, yend = y_pos, color = metric_class),
      inherit.aes = FALSE, linewidth = .88, alpha = .56, lineend = "round"
    ) +
    geom_point(
      data = joint_cv_plot_summary,
      aes(r2_median, y_pos, color = metric_class),
      inherit.aes = FALSE, shape = 18, size = 1.45
    ) +
    facet_wrap(~outcome_label, nrow = 1) +
    scale_color_ms_metric(guide = "none") +
    scale_x_continuous(
      limits = c(-joint_cv_limit, joint_cv_limit),
      breaks = scales::breaks_extended(n = 5)
    ) +
    scale_y_continuous(
      breaks = seq_along(DIMENSIONS),
      labels = unname(DIM_TITLES[DIMENSIONS]),
      limits = c(.55, length(DIMENSIONS) + .45)
    ) +
    labs(
      title = "c  Out-of-sample contextual predictability",
      subtitle = "raw points show central 95%; class summaries use all joint-model CV results",
      x = "participant-grouped CV R²", y = NULL
    ) +
    theme_rq2(base_size = 6.05) +
    theme(
      panel.grid.major.y = element_blank(),
      axis.line.y = element_blank(), axis.ticks.y = element_blank(),
      axis.text.y = element_text(size = 4.9),
      strip.text = element_text(size = 5.35),
      plot.subtitle = element_text(
        size = 4.25, colour = "#666A6D", margin = margin(t = -1, b = 2)
      ),
      panel.spacing = grid::unit(1.8, "mm")
    )
} else {
  joint_cv_limit <- NA_real_
  p2c <- ggplot() + theme_void(base_family = MS_FONT) +
    annotate(
      "text", x = 0, y = 0,
      label = "c  Out-of-sample contextual predictability\nNo joint grouped-CV results",
      size = 2.2, colour = "#55595C"
    )
}

'''
text = replace_between(text, start, end, new_cv, "replace Fig2c robust display")

old = '''readr::write_csv(direction_shift_summary |>
  mutate(metric_class = as.character(metric_class)),
  file.path("results", "rq2", "fig2_direction_shift.csv"), na = "")
'''
text = replace_once(text, old, '', "remove stale direction_shift_summary write")

old = '''p2 <- cowplot::plot_grid(
  metric_legend, p2body, ncol = 1, rel_heights = c(.040, 1),
  align = "v", greedy = TRUE
)
'''
new = '''p2 <- cowplot::plot_grid(
  metric_legend, p2body, ncol = 1, rel_heights = c(.040, 1),
  align = "v", axis = "l", greedy = TRUE
)
'''
text = replace_once(text, old, new, "fix Fig2 outer alignment")

old = '''r_limit <- max(abs(gamma_metric$R_metric), na.rm = TRUE)
if (!is.finite(r_limit) || r_limit <= 0) r_limit <- 1
q_limit <- max(gamma_metric$Q_metric, na.rm = TRUE)
if (!is.finite(q_limit) || q_limit <= 0) q_limit <- 1

p3a <- ggplot(gamma_metric, aes(R_metric, metric_class, color = metric_class)) +
'''
new = '''r_limit <- robust_symmetric_display_limit(
  gamma_metric$R_metric,
  c(gamma_r_summary$R_median, gamma_r_summary$R_q25, gamma_r_summary$R_q75),
  prob = .95, pad = 1.08, fallback = .05
)
q_limit <- robust_upper_display_limit(
  gamma_metric$Q_metric,
  c(gamma_q_summary$Q_median, gamma_q_summary$Q_q25, gamma_q_summary$Q_q75),
  prob = .95, pad = 1.08, fallback = .10
)
gamma_metric_r_display <- gamma_metric |> filter(abs(R_metric) <= r_limit)
gamma_metric_q_display <- gamma_metric |> filter(Q_metric <= q_limit)

p3a <- ggplot(gamma_metric_r_display, aes(R_metric, metric_class, color = metric_class)) +
'''
text = replace_once(text, old, new, "robust Fig3 R/Q limits")
text = replace_once(
    text,
    'p3b <- ggplot(gamma_metric, aes(Q_metric, metric_class, color = metric_class)) +\n',
    'p3b <- ggplot(gamma_metric_q_display, aes(Q_metric, metric_class, color = metric_class)) +\n',
    "robust Fig3 Q raw points"
)
text = replace_once(
    text,
    '  labs(title = "a  Signed cross-dimensional interaction",\n       x = "R = median signed γ across local transitions", y = NULL) +\n',
    '  labs(title = "a  Signed cross-dimensional interaction",\n       subtitle = "raw metric points show central 95%; summaries use all metrics",\n       x = "R = median signed γ across local transitions", y = NULL) +\n',
    "Fig3a subtitle"
)
text = replace_once(
    text,
    '  labs(title = "b  Magnitude of cross-dimensional interaction",\n       x = "Q = median |γ| across local transitions", y = NULL) +\n',
    '  labs(title = "b  Magnitude of cross-dimensional interaction",\n       subtitle = "raw metric points show central 95%; summaries use all metrics",\n       x = "Q = median |γ| across local transitions", y = NULL) +\n',
    "Fig3b subtitle"
)
text = replace_once(
    text,
    '        axis.text.y = element_text(size = 5.4), strip.text = element_text(size = 6.0),\n        panel.spacing.x = grid::unit(2.3, "mm"))\n\np3b <-',
    '        axis.text.y = element_text(size = 5.4), strip.text = element_text(size = 6.0),\n        plot.subtitle = element_text(size = 4.45, colour = "#666A6D", margin = margin(t = -1, b = 1.5)),\n        panel.spacing.x = grid::unit(2.3, "mm"))\n\np3b <-',
    "Fig3a subtitle theme"
)
text = replace_once(
    text,
    '        axis.text.y = element_text(size = 5.4), strip.text = element_text(size = 6.0),\n        panel.spacing.x = grid::unit(2.3, "mm"))\n\ngamma_transition <-',
    '        axis.text.y = element_text(size = 5.4), strip.text = element_text(size = 6.0),\n        plot.subtitle = element_text(size = 4.45, colour = "#666A6D", margin = margin(t = -1, b = 1.5)),\n        panel.spacing.x = grid::unit(2.3, "mm"))\n\ngamma_transition <-',
    "Fig3b subtitle theme"
)

old = '''p3 <- cowplot::plot_grid(metric_legend, p3body, ncol = 1, rel_heights = c(.045, 1),
                         align = "v", greedy = TRUE)
'''
new = '''p3 <- cowplot::plot_grid(metric_legend, p3body, ncol = 1, rel_heights = c(.045, 1),
                         align = "v", axis = "l", greedy = TRUE)
'''
text = replace_once(text, old, new, "fix Fig3 outer alignment")

# Static safety checks for the exact bugs reported by the user.
if 'direction_shift_summary' in text:
    raise RuntimeError('stale direction_shift_summary reference remains')
if 'r_limit <- max(abs(gamma_metric$R_metric)' in text:
    raise RuntimeError('Fig3 still uses full maximum for R axis')
if 'q_limit <- max(gamma_metric$Q_metric' in text:
    raise RuntimeError('Fig3 still uses full maximum for Q axis')
if 'coef_limit <- ms_symmetric_limit(' in text:
    raise RuntimeError('Fig2b still uses non-robust full-range display limit')

p.write_text(text, encoding="utf-8")


# =============================================================================
# RQ3 plotting hygiene: stop duplicate Fig4 rendering and scale-limit warnings.
# Fig5 is intentionally untouched.
# =============================================================================
p = Path("scripts/15_plot_rq3.R")
text = p.read_text(encoding="utf-8")

# The legacy Fig4 object is retained as source history but must not write the
# canonical file before the dynamic frozen-design refinement later in the script.
old = '''ms_plot_save(fig4, file.path(OUT_DIR, "Fig4_RQ3.pdf"), 9.0, 6.1)
ms_plot_save(fig4, file.path(OUT_DIR, "Fig4_RQ3.png"), 9.0, 6.1)
'''
text = replace_once(text, old, '', "remove legacy Fig4 writer")

# In the canonical dynamic Fig4 block, coordinate zoom must not delete step rows.
old = '''  scale_x_continuous(
    trans = epsilon_log1p,
    limits = c(0, epsilon_limit),
    breaks = epsilon_ticks,
    labels = epsilon_labels,
    expand = expansion(mult = c(0, .01))
  ) +
  scale_y_continuous(
'''
new = '''  scale_x_continuous(
    trans = epsilon_log1p,
    breaks = epsilon_ticks,
    labels = epsilon_labels,
    expand = expansion(mult = c(0, .01))
  ) +
  coord_cartesian(xlim = c(0, epsilon_limit), clip = "on") +
  scale_y_continuous(
'''
text = replace_once(text, old, new, "fix canonical Fig4a scale warning")

# Same policy for the substitutability ECDF in the canonical dynamic block.
old = '''  scale_x_continuous(
    trans = epsilon_log1p,
    limits = c(0, epsilon_limit),
    breaks = epsilon_ticks,
    labels = epsilon_labels,
    expand = expansion(mult = c(0, .01))
  ) +
  scale_y_continuous(limits = c(0, 1), labels = scales::label_percent(accuracy = 25)) +
'''
new = '''  scale_x_continuous(
    trans = epsilon_log1p,
    breaks = epsilon_ticks,
    labels = epsilon_labels,
    expand = expansion(mult = c(0, .01))
  ) +
  coord_cartesian(xlim = c(0, epsilon_limit), clip = "on") +
  scale_y_continuous(limits = c(0, 1), labels = scales::label_percent(accuracy = 25)) +
'''
text = replace_once(text, old, new, "fix canonical Fig4c scale warning")

old = '''fig4 <- cowplot::plot_grid(
  metric_legend, fig4_body, ncol = 1,
  rel_heights = c(.042, 1), align = "v", greedy = TRUE
)
ms_plot_save(fig4, file.path(OUT_DIR, "Fig4_RQ3.png"), 9.0, 6.1)
'''
new = '''fig4 <- cowplot::plot_grid(
  metric_legend, fig4_body, ncol = 1,
  rel_heights = c(.042, 1), align = "v", axis = "l", greedy = TRUE
)
ms_plot_save(fig4, file.path(OUT_DIR, "Fig4_RQ3.pdf"), 9.0, 6.1)
ms_plot_save(fig4, file.path(OUT_DIR, "Fig4_RQ3.png"), 9.0, 6.1)
'''
text = replace_once(text, old, new, "make canonical Fig4 single writer")

if text.count('file.path(OUT_DIR, "Fig4_RQ3.png")') != 1:
    raise RuntimeError('Fig4 PNG must have exactly one writer')
if text.count('file.path(OUT_DIR, "Fig4_RQ3.pdf")') != 1:
    raise RuntimeError('Fig4 PDF must have exactly one writer')
if text.count('file.path(OUT_DIR, "Fig5_RQ3.png")') != 1:
    raise RuntimeError('Fig5 PNG writer count changed unexpectedly')
if text.count('file.path(OUT_DIR, "Fig5_RQ3.pdf")') != 1:
    raise RuntimeError('Fig5 PDF writer count changed unexpectedly')

p.write_text(text, encoding="utf-8")
