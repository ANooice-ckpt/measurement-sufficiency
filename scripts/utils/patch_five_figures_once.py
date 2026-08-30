from pathlib import Path
import re


def replace_once(text, old, new, label):
    n = text.count(old)
    if n != 1:
        raise SystemExit(f'{label}: expected exactly one match, found {n}')
    return text.replace(old, new, 1)

# -----------------------------------------------------------------------------
# Fig. 1
# -----------------------------------------------------------------------------
p = Path('scripts/11_plot_fig1.R')
t = p.read_text(encoding='utf-8')

# Reuse a version-matched rank-preservation cache when present. This makes local
# re-rendering independent of the large partitioned pairwise artifact after the
# derived diagnostic has been computed once.
start = t.index('rank_part_paths <- rq1_pairwise_part_paths(pairwise_artifact)')
end = t.index('# -----------------------------------------------------------------------------\n# a. Absolute versus relational preservation', start)
block = t[start:end]
cache_prefix = '''rank_cache <- if (file.exists(RANK_CSV)) {
  readr::read_csv(RANK_CSV, show_col_types = FALSE, progress = FALSE)
} else tibble()
rank_cache_required <- c(
  "core_artifact_version", "rq1_analysis_version", "dimension", "metric",
  "A_mean_absolute", "rank_loss", "rank_preservation_available"
)
rank_cache_valid <- nrow(rank_cache) > 0 && all(rank_cache_required %in% names(rank_cache)) &&
  identical(unique(na.omit(rank_cache$core_artifact_version)), CORE_VERSION) &&
  identical(unique(na.omit(rank_cache$rq1_analysis_version)), RQ1_VERSION)

if (rank_cache_valid) {
  rank_base <- rank_cache
} else {
'''
indented = ''.join('  ' + line if line.strip() else line for line in block.splitlines(True))
t = t[:start] + cache_prefix + indented + '}\n\n' + t[end:]

old = '''dimension_summary <- dimension_metric |>
  group_by(dimension, metric_class) |>
  summarise(
    n_metrics = n_distinct(metric),
    A_median = median(A_typical, na.rm = TRUE),
    A_q25 = quantile(A_typical, .25, na.rm = TRUE, names = FALSE),
    A_q75 = quantile(A_typical, .75, na.rm = TRUE, names = FALSE),
    rank_loss_median = median(rank_loss_typical, na.rm = TRUE),
    rank_loss_q25 = quantile(rank_loss_typical, .25, na.rm = TRUE, names = FALSE),
    rank_loss_q75 = quantile(rank_loss_typical, .75, na.rm = TRUE, names = FALSE),
    .groups = "drop"
  )
'''
new = old + '''
dimension_assoc <- dimension_metric |>
  group_by(dimension) |>
  summarise(
    n_metrics = n_distinct(metric),
    rho_A_rank = if (n_metrics >= 3L && n_distinct(A_typical) >= 2L &&
                     n_distinct(rank_loss_typical) >= 2L) {
      suppressWarnings(cor(A_typical, rank_loss_typical, method = "spearman", use = "complete.obs"))
    } else NA_real_,
    .groups = "drop"
  ) |>
  mutate(label = if_else(is.finite(rho_A_rank), sprintf("ρ = %.2f", rho_A_rank), "ρ = NA"))
'''
t = replace_once(t, old, new, 'Fig1 dimension association insertion')

old = '''  geom_point(
    data = dimension_summary,
    aes(A_median, rank_loss_median, color = metric_class),
    inherit.aes = FALSE, shape = 18, size = 1.85, alpha = .98
  ) +
  facet_grid(. ~ dimension) +
'''
new = '''  geom_point(
    data = dimension_summary,
    aes(A_median, rank_loss_median, color = metric_class),
    inherit.aes = FALSE, shape = 18, size = 1.85, alpha = .98
  ) +
  geom_text(
    data = dimension_assoc,
    aes(x = Inf, y = Inf, label = label),
    inherit.aes = FALSE, hjust = 1.08, vjust = 1.25,
    size = 1.75, colour = "#666A6D"
  ) +
  facet_grid(. ~ dimension) +
'''
t = replace_once(t, old, new, 'Fig1 association label')
t = replace_once(
    t,
    'subtitle = "metric points; diamonds = class medians; bars = IQR · lower left = stronger preservation",',
    'subtitle = "metric points; diamonds = class medians; bars = IQR · facet ρ = metric-level A–rank-loss association",',
    'Fig1a subtitle'
)

old = '''target_labels <- target_geometry |>
  group_by(facet_label, metric) |>
  slice_max(A_mean_absolute, n = 1, with_ties = FALSE) |>
  ungroup() |>
  group_by(facet_label) |>
  slice_max(A_mean_absolute, n = 2, with_ties = FALSE) |>
  ungroup()
'''
new = '''target_label_max_a <- target_geometry |>
  group_by(facet_label, metric) |>
  slice_max(A_mean_absolute, n = 1, with_ties = FALSE) |>
  ungroup() |>
  group_by(facet_label) |>
  slice_max(A_mean_absolute, n = 1, with_ties = FALSE) |>
  ungroup()
target_label_coherent <- target_geometry |>
  mutate(direction_coherence = if_else(
    is.finite(A_mean_absolute) & A_mean_absolute > 0,
    abs(B_mean_signed) / A_mean_absolute, NA_real_
  )) |>
  filter(is.finite(direction_coherence)) |>
  group_by(facet_label, metric) |>
  slice_max(direction_coherence, n = 1, with_ties = FALSE) |>
  ungroup() |>
  group_by(facet_label) |>
  slice_max(direction_coherence, n = 1, with_ties = FALSE) |>
  ungroup()
target_labels <- bind_rows(target_label_max_a, target_label_coherent) |>
  distinct(facet_label, metric, .keep_all = TRUE)
'''
t = replace_once(t, old, new, 'Fig1b informative labels')
t = replace_once(
    t,
    'title = "b  Directionality and magnitude of target-aligned distortion",\n    x = "B: mean signed change", y = "A: mean absolute change"',
    'title = "b  Directionality and magnitude of target-aligned distortion",\n    subtitle = "labels mark maximum A and maximum |B|/A · toward |B| = A means more coherent directional change",\n    x = "B: mean signed change", y = "A: mean absolute change"',
    'Fig1b subtitle'
)

old = '''  labels <- d |> distinct(step_order, transition) |> arrange(step_order)
  xmax <- max(ds$share_q75, ds$share_median, na.rm = TRUE) * 1.18
'''
new = '''  labels <- d |> distinct(step_order, transition) |> arrange(step_order)
  equal_share <- 1 / n_distinct(labels$transition)
  xmax <- max(ds$share_q75, ds$share_median, na.rm = TRUE) * 1.18
'''
t = replace_once(t, old, new, 'Fig1 equal share value')
old = '''  ggplot() +
    geom_point(
'''
new = '''  ggplot() +
    geom_vline(
      xintercept = equal_share, linetype = 3,
      linewidth = .28, color = "#A9AEB1"
    ) +
    geom_point(
'''
# This exact grammar occurs in the local panel block only once after the previous insertion.
pos = t.index('local_distribution_panel <- function')
sub = t[pos:]
if sub.count(old) < 1:
    raise SystemExit('Fig1 equal share line anchor missing')
sub = sub.replace(old, new, 1)
t = t[:pos] + sub

t = replace_once(
    t,
    '"c  Ordered-axis local response",',
    '"c  Ordered-axis local response · dotted line = equal share across adjacent steps",',
    'Fig1 local-response title'
)
p.write_text(t, encoding='utf-8')

# -----------------------------------------------------------------------------
# Figs. 2–3
# -----------------------------------------------------------------------------
p = Path('scripts/13_plot_rq2.R')
t = p.read_text(encoding='utf-8')

old = '''PREDICTOR_LEVELS <- c(
  "external_radiation", "external_direct_fraction", "external_cloud",
  "solar_noon_elevation_deg", "primary_state_raw", "duration_day_variability"
)
PREDICTOR_LABELS <- c(
  external_radiation = "Solar radiation",
  external_direct_fraction = "Direct fraction",
  external_cloud = "Cloud cover",
  solar_noon_elevation_deg = "Solar-noon elevation",
  primary_state_raw = "Primary exposure state",
  duration_day_variability = "Day-to-day variability"
)
'''
new = '''PREDICTOR_LEVELS <- c(
  "external_radiation", "external_cloud", "solar_noon_elevation_deg",
  "micro_outdoor_fraction", "micro_daylight_indoor_fraction",
  "behaviour_work_fraction", "behaviour_exercise_level",
  "primary_state_raw", "duration_day_variability"
)
PREDICTOR_LABELS <- c(
  external_radiation = "Solar radiation",
  external_cloud = "Cloud cover",
  solar_noon_elevation_deg = "Solar-noon elevation",
  micro_outdoor_fraction = "Outdoor fraction",
  micro_daylight_indoor_fraction = "Indoor daylight fraction",
  behaviour_work_fraction = "Work fraction",
  behaviour_exercise_level = "Exercise level",
  primary_state_raw = "Primary exposure state",
  duration_day_variability = "Day-to-day variability"
)
PREDICTOR_FAMILIES <- c(
  external_radiation = "External opportunity",
  external_cloud = "External opportunity",
  solar_noon_elevation_deg = "External opportunity",
  micro_outdoor_fraction = "Micro-environment",
  micro_daylight_indoor_fraction = "Micro-environment",
  behaviour_work_fraction = "Behaviour",
  behaviour_exercise_level = "Behaviour",
  primary_state_raw = "Exposure state",
  duration_day_variability = "Exposure state"
)
'''
t = replace_once(t, old, new, 'Fig2 layered predictor set')

old = '''    predictor_family = if_else(term %in% c("primary_state_raw", "duration_day_variability"),
                               "Exposure state", "External context"),
'''
new = '''    predictor_family = factor(
      unname(PREDICTOR_FAMILIES[term]),
      levels = c("External opportunity", "Micro-environment", "Behaviour", "Exposure state")
    ),
'''
t = replace_once(t, old, new, 'Fig2 predictor family mapping')
t = replace_once(
    t,
    'PREDICTOR_COLORS <- c("External context" = MS_PRIMARY, "Exposure state" = MS_SECONDARY)',
    'PREDICTOR_COLORS <- c(\n  "External opportunity" = MS_PRIMARY,\n  "Micro-environment" = "#5F8F84",\n  "Behaviour" = MS_NEUTRAL,\n  "Exposure state" = MS_SECONDARY\n)',
    'Fig2 four-family colors'
)
t = replace_once(
    t,
    'subtitle = "raw points show central 95%; medians and IQRs use all estimates",',
    'subtitle = "prespecified representatives span external opportunity, micro-environment, behaviour and exposure state",',
    'Fig2b subtitle'
)

anchor = '''joint_cv_summary <- joint_cv_metric |>
  group_by(dimension, outcome_label, metric_class) |>
  summarise(
    n_metrics = n_distinct(metric),
    r2_median = median(r2, na.rm = TRUE),
    r2_q25 = quantile(r2, .25, na.rm = TRUE, names = FALSE),
    r2_q75 = quantile(r2, .75, na.rm = TRUE, names = FALSE),
    .groups = "drop"
  )
'''
insert = anchor + '''
joint_cv_positive <- joint_cv_metric |>
  group_by(dimension, outcome_label) |>
  summarise(
    n_metrics = n_distinct(metric),
    fraction_positive = mean(r2 > 0, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    dimension_num = as.integer(dimension),
    y_label = dimension_num + .25,
    label = paste0(round(100 * fraction_positive), "% > 0")
  )
'''
t = replace_once(t, anchor, insert, 'Fig2c positive CV fraction')

old = '''    geom_point(
      data = joint_cv_plot_summary,
      aes(r2_median, y_pos, color = metric_class),
      inherit.aes = FALSE, shape = 18, size = 1.45
    ) +
    facet_wrap(~outcome_label, nrow = 1) +
'''
new = '''    geom_point(
      data = joint_cv_plot_summary,
      aes(r2_median, y_pos, color = metric_class),
      inherit.aes = FALSE, shape = 18, size = 1.45
    ) +
    geom_text(
      data = joint_cv_positive,
      aes(x = Inf, y = y_label, label = label),
      inherit.aes = FALSE, hjust = 1.08, vjust = .5,
      size = 1.55, color = MS_NEUTRAL
    ) +
    facet_wrap(~outcome_label, nrow = 1) +
'''
t = replace_once(t, old, new, 'Fig2c positive fraction labels')
t = replace_once(
    t,
    'subtitle = "raw points show central 95%; class summaries use all joint-model CV results",',
    'subtitle = "class summaries use all joint-model CV results; right labels = fraction of metrics with CV R² > 0",',
    'Fig2c subtitle'
)

old = '''    Q_median = median(Q, na.rm = TRUE),
    Q_q25 = quantile(Q, .25, na.rm = TRUE, names = FALSE),
    Q_q75 = quantile(Q, .75, na.rm = TRUE, names = FALSE),
    R_median = median(R, na.rm = TRUE),
    .groups = "drop"
'''
new = '''    Q_median = median(Q, na.rm = TRUE),
    Q_q25 = quantile(Q, .25, na.rm = TRUE, names = FALSE),
    Q_q75 = quantile(Q, .75, na.rm = TRUE, names = FALSE),
    R_median = median(R, na.rm = TRUE),
    coherence_median = median(if_else(Q > 1e-12, R / Q, NA_real_), na.rm = TRUE),
    .groups = "drop"
'''
t = replace_once(t, old, new, 'Fig3 interaction coherence')

# Remove data-dependent R fill scaling and use the theoretical [-1, 1] coherence scale.
t = re.sub(
    r'fill_limit <- max\(abs\(gamma_transition\$R_median\), na\.rm = TRUE\)\nif \(!is\.finite\(fill_limit\) \|\| fill_limit <= 0\) fill_limit <- 1e-6\n\n',
    '', t, count=1
)
t = replace_once(
    t,
    'p3c <- ggplot(gamma_transition, aes(Q_median, transition_key)) +',
    'p3c <- ggplot(gamma_transition, aes(Q_median, transition_key)) +',
    'Fig3c anchor check'
)
t = replace_once(
    t,
    'geom_point(aes(fill = R_median), shape = 21, size = 2.15, color = "#3E4245", stroke = .22) +',
    'geom_point(aes(fill = coherence_median), shape = 21, size = 2.15, color = "#3E4245", stroke = .22) +',
    'Fig3c coherence fill'
)
t = replace_once(
    t,
    'scale_fill_ms_diverging(fill_limit, name = "median R") +',
    'scale_fill_ms_diverging(1, name = "median R / Q") +',
    'Fig3c coherence scale'
)
t = replace_once(
    t,
    'labs(title = "c  Localization of the strongest cross-dimensional interactions",\n       x = "median Q across metrics", y = NULL) +',
    'labs(\n    title = "c  Strong interactions differ in directional coherence",\n    subtitle = "x = interaction magnitude Q; fill = signed coherence R/Q (0 = cancellation)",\n    x = "median Q across metrics", y = NULL\n  ) +',
    'Fig3c labels'
)
# Add small subtitle styling without changing the rest of the grammar.
t = replace_once(
    t,
    'axis.text.y = element_text(size = 4.9), strip.text = element_text(size = 5.8),\n        legend.title = element_text(size = 5.3), legend.text = element_text(size = 5.0),',
    'axis.text.y = element_text(size = 4.9), strip.text = element_text(size = 5.8),\n        plot.subtitle = element_text(size = 4.35, colour = "#666A6D", margin = margin(t = -1, b = 1.5)),\n        legend.title = element_text(size = 5.3), legend.text = element_text(size = 5.0),',
    'Fig3c subtitle theme'
)
p.write_text(t, encoding='utf-8')

# -----------------------------------------------------------------------------
# Figs. 4–5
# -----------------------------------------------------------------------------
p = Path('scripts/15_plot_rq3.R')
t = p.read_text(encoding='utf-8')

# Class-level strip summaries for the six deviation heatmaps.
anchor = '''entry_class_grid <- tidyr::crossing(
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
'''
insert = anchor + '''
entry_class_summary <- entry_class_surface |>
  group_by(metric_class) |>
  summarise(
    n_cells = sum(is.finite(entry_tolerance_difference)),
    median_delta = median(entry_tolerance_difference, na.rm = TRUE),
    fraction_positive = mean(entry_tolerance_difference > 0, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    strip_label = paste0(
      as.character(metric_class), "\\nmed Δε ", sprintf("%+.2f", median_delta),
      " · ", round(100 * fraction_positive), "% > 0"
    )
  )
class_strip_labels <- setNames(entry_class_summary$strip_label, as.character(entry_class_summary$metric_class))
'''
t = replace_once(t, anchor, insert, 'Fig5 class summaries')
t = replace_once(
    t,
    'facet_wrap(~metric_class, ncol = 3) +',
    'facet_wrap(~metric_class, ncol = 3, labeller = as_labeller(class_strip_labels)) +',
    'Fig5 class strip labels'
)
t = replace_once(
    t,
    'subtitle = "fill = confirmed metric coverage; dark outline = coverage-efficient frontier",',
    'subtitle = "fill = confirmed coverage among all targets (unresolved ≠ sufficient); dark outline = coverage-efficient frontier",',
    'Fig5c unresolved semantics'
)

# Replace the final Fig. 4 display refinement with a denser version that restores
# the sufficiency-coverage strip and explicitly links the continuous tolerance
# response to the four slices used in Fig. 5.
start = t.index('p4a <- ggplot(requirement_summary, aes(epsilon, rank_median, color = metric_class)) +', t.index('epsilon_labels <- epsilon_tick_labels[epsilon_tick_keep]'))
end = t.index('# R_obs stays on its original linear scale;', start)
new_p4a = '''FIG4_DIM_LABELS <- c(
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
    subtitle = "thick = class median; thin = IQR · faint vertical guides = Fig. 5 tolerance slices",
    x = NULL, y = "minimum sufficient requirement rank\\n(low → high burden)"
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
  labs(x = "tolerance ε", y = "metrics with a\\nresolved sufficient state") +
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

'''
t = t[:start] + new_p4a + t[end:]

# Add actual endpoint labels to panel b facets too.
t = replace_once(
    t,
    'facet_wrap(~dimension, nrow = 1) +\n  scale_color_ms_metric(guide = "none") +\n  scale_x_continuous(\n    breaks = seq_len(ORDERED_MAX_RANK),',
    'facet_wrap(~dimension, nrow = 1, labeller = as_labeller(FIG4_DIM_LABELS)) +\n  scale_color_ms_metric(guide = "none") +\n  scale_x_continuous(\n    breaks = seq_len(ORDERED_MAX_RANK),',
    'Fig4b endpoint labels'
)

# ECDF median tolerance (epsilon50) plus the same vertical slice guides.
anchor = 'p4c <- ggplot(\n  pair_ecdf,\n'
if anchor not in t:
    raise SystemExit('Fig4c anchor missing')
pair_e50 = '''pair_e50 <- pair_ecdf |>
  group_by(dimension, comparison_pair_id, pair) |>
  summarise(
    epsilon50 = if (any(fraction_metrics_substitutable >= .5)) {
      min(epsilon[fraction_metrics_substitutable >= .5], na.rm = TRUE)
    } else NA_real_,
    .groups = "drop"
  ) |>
  filter(is.finite(epsilon50))

'''
t = t.replace(anchor, pair_e50 + anchor, 1)
t = replace_once(
    t,
    ') +\n  geom_step(linewidth = .76, alpha = .94) +\n  facet_wrap(~dimension, nrow = 1) +',
    ') +\n  geom_vline(xintercept = fig5_slice_guides, linewidth = .22, linetype = 3, color = "#C5C9CC") +\n  geom_hline(yintercept = .5, linewidth = .24, linetype = 3, color = "#B4B8BB") +\n  geom_step(linewidth = .76, alpha = .94) +\n  geom_point(\n    data = pair_e50, aes(epsilon50, .5, color = pair),\n    inherit.aes = FALSE, shape = 21, fill = "white", size = 1.35, stroke = .45\n  ) +\n  facet_wrap(~dimension, nrow = 1) +',
    'Fig4c epsilon50 and guides'
)
t = replace_once(
    t,
    'title = "c  Target-aligned alternatives become substitutable as tolerance relaxes",\n    x = "tolerance ε", y = "fraction of metrics substitutable"',
    'title = "c  Target-aligned alternatives become substitutable as tolerance relaxes",\n    subtitle = "open points = ε50; faint vertical guides = Fig. 5 tolerance slices",\n    x = "tolerance ε", y = "fraction of metrics substitutable"',
    'Fig4c subtitle'
)
t = replace_once(
    t,
    'panel.grid.major.x = element_line(colour = "#ECEFF0", linewidth = .20),\n    strip.text = element_text(size = 5.8),',
    'panel.grid.major.x = element_line(colour = "#ECEFF0", linewidth = .20),\n    strip.text = element_text(size = 5.8),\n    plot.subtitle = element_text(size = 4.45, colour = "#666A6D", margin = margin(t = -1, b = 1.5)),',
    'Fig4c subtitle theme'
)

t = replace_once(
    t,
    'fig4_body <- cowplot::plot_grid(\n  p4a, fig4_bottom, ncol = 1, rel_heights = c(1.14, .86),',
    'fig4_body <- cowplot::plot_grid(\n  p4a, fig4_bottom, ncol = 1, rel_heights = c(1.20, .80),',
    'Fig4 layout weights'
)
t = replace_once(
    t,
    'ms_plot_save(fig4, file.path(OUT_DIR, "Fig4_RQ3.pdf"), 9.0, 6.1)\nms_plot_save(fig4, file.path(OUT_DIR, "Fig4_RQ3.png"), 9.0, 6.1)',
    'ms_plot_save(fig4, file.path(OUT_DIR, "Fig4_RQ3.pdf"), 9.0, 6.2)\nms_plot_save(fig4, file.path(OUT_DIR, "Fig4_RQ3.png"), 9.0, 6.2)\nreadr::write_csv(pair_e50, file.path("results", "rq3", "fig4_unordered_epsilon50.csv"), na = "")',
    'Fig4 epsilon50 output'
)

# Persist the compact class-level summary used in Fig. 5b.
t = replace_once(
    t,
    'readr::write_csv(\n  entry_class_grid |>\n    mutate(metric_class = as.character(metric_class)),\n  file.path("results", "rq3", "fig5_entry_tolerance_class_contrast.csv"), na = ""\n)\n',
    'readr::write_csv(\n  entry_class_grid |>\n    mutate(metric_class = as.character(metric_class)),\n  file.path("results", "rq3", "fig5_entry_tolerance_class_contrast.csv"), na = ""\n)\nreadr::write_csv(\n  entry_class_summary |> mutate(metric_class = as.character(metric_class)),\n  file.path("results", "rq3", "fig5_entry_tolerance_class_summary.csv"), na = ""\n)\n',
    'Fig5 class summary output'
)
p.write_text(t, encoding='utf-8')

# Hard invariants for this one-shot patch.
checks = {
  'scripts/11_plot_fig1.R': [
    'rank_cache_valid', 'rho_A_rank', 'equal share across adjacent steps',
    'maximum |B|/A'
  ],
  'scripts/13_plot_rq2.R': [
    'Micro-environment', 'Behaviour', 'fraction of metrics with CV R² > 0',
    'coherence_median', 'median R / Q'
  ],
  'scripts/15_plot_rq3.R': [
    'metrics with a\\nresolved sufficient state', 'fig5_slice_guides',
    'epsilon50', 'class_strip_labels', 'unresolved ≠ sufficient'
  ]
}
for path, tokens in checks.items():
    txt = Path(path).read_text(encoding='utf-8')
    for token in tokens:
        if token not in txt:
            raise SystemExit(f'{path}: missing required token {token!r}')
