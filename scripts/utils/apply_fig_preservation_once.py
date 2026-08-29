from pathlib import Path


def replace_between(text: str, start: str, end: str, replacement: str) -> str:
    i = text.find(start)
    if i < 0:
        raise RuntimeError(f"start marker not found: {start!r}")
    j = text.find(end, i)
    if j < 0:
        raise RuntimeError(f"end marker not found: {end!r}")
    return text[:i] + replacement.rstrip() + "\n\n" + text[j:]


fig1_path = Path("scripts/11_plot_fig1.R")
fig1 = fig1_path.read_text(encoding="utf-8")

if "fig1_rank_preservation.csv" not in fig1:
    insert_marker = "# -----------------------------------------------------------------------------\n# a. Configuration sensitivity as four parallel distribution strips"
    insert_at = fig1.find(insert_marker)
    if insert_at < 0:
        raise RuntimeError("Fig. 1 panel-a marker not found")

    rank_block = r'''# -----------------------------------------------------------------------------
# Fig. 1a derived relational-preservation diagnostic
# -----------------------------------------------------------------------------
# This is intentionally a figure-level derived summary rather than a change to
# the canonical RQ1 estimand. It consumes the frozen paired RQ1 artifact, uses
# exactly the rows on which standardized A is defined, and writes a separate
# audit CSV. Spearman rank preservation is not defined for circular-time
# representations because ordinary ranks do not respect circular geometry.
RANK_CSV <- file.path("results", "rq1", "fig1_rank_preservation.csv")
rank_part_paths <- rq1_pairwise_part_paths(pairwise_artifact)
if (!length(rank_part_paths) || any(!file.exists(rank_part_paths))) {
  stop("Fig. 1 rank-preservation diagnostic requires all frozen RQ1 pairwise parts", call. = FALSE)
}

rank_group_vars <- c(
  "dimension", "comparison_lattice", "comparison_pair_id", "config_a_id", "config_b_id",
  "config_a_label", "config_b_label", "orientation_type", "orientation_basis",
  "metric", "metric_class", "metric_geometry"
)

rank_fragment <- function(part_path) {
  x <- readRDS(part_path) |>
    filter(
      available, is.finite(z), is.finite(value_a), is.finite(value_b),
      dimension %in% DIMENSIONS,
      dimension %in% c("placement", "optical") | coalesce(anchor_projection, FALSE)
    ) |>
    mutate(
      dimension = as.character(dimension),
      comparison_pair_id = if_else(
        dimension == "duration", paste0(n_days_a, "d_vs_", n_days_b, "d"),
        as.character(comparison_pair_id)
      ),
      config_a_id = if_else(
        dimension == "duration", paste0("duration_", n_days_a, "d"), as.character(config_a_id)
      ),
      config_b_id = if_else(
        dimension == "duration", paste0("duration_", n_days_b, "d"), as.character(config_b_id)
      ),
      config_a_label = if_else(
        dimension == "duration", paste0(n_days_a, " d"), as.character(config_a_label)
      ),
      config_b_label = if_else(
        dimension == "duration", paste0(n_days_b, " d"), as.character(config_b_label)
      )
    )
  if (!nrow(x)) return(tibble())
  x |>
    group_by(across(all_of(rank_group_vars))) |>
    summarise(
      n_units = n(),
      A_sum = sum(abs(z)),
      participant_keys = list(unique(paste(site, Id, sep = "|"))),
      value_a_values = list(as.numeric(value_a)),
      value_b_values = list(as.numeric(value_b)),
      .groups = "drop"
    )
}

rank_fragments <- map(rank_part_paths, rank_fragment)
rank_rows <- bind_rows(rank_fragments)
if (!nrow(rank_rows)) stop("No rows available for Fig. 1 rank-preservation diagnostic", call. = FALSE)

rank_base <- rank_rows |>
  group_by(across(all_of(rank_group_vars))) |>
  summarise(
    n_units = sum(n_units),
    A_sum = sum(A_sum),
    participant_keys = list(unique(unlist(participant_keys, use.names = FALSE))),
    value_a_values = list(unlist(value_a_values, use.names = FALSE)),
    value_b_values = list(unlist(value_b_values, use.names = FALSE)),
    .groups = "drop"
  ) |>
  mutate(
    n_participants = map_int(participant_keys, length),
    n_unique_a = map_int(value_a_values, ~n_distinct(.x[is.finite(.x)])),
    n_unique_b = map_int(value_b_values, ~n_distinct(.x[is.finite(.x)])),
    rho_spearman = pmap_dbl(
      list(value_a_values, value_b_values, metric_geometry, n_units, n_unique_a, n_unique_b),
      function(a, b, geometry, n, ua, ub) {
        if (identical(geometry, "circular_time") || n < 3L || ua < 2L || ub < 2L) return(NA_real_)
        r <- suppressWarnings(stats::cor(a, b, method = "spearman", use = "complete.obs"))
        if (is.finite(r)) max(-1, min(1, r)) else NA_real_
      }
    ),
    A_mean_absolute = if_else(n_units > 0, A_sum / n_units, NA_real_),
    rank_loss = if_else(is.finite(rho_spearman), 1 - rho_spearman, NA_real_),
    rank_preservation_available = is.finite(rho_spearman),
    rank_unavailable_reason = case_when(
      metric_geometry == "circular_time" ~ "not applicable to circular-time geometry",
      n_units < 3L ~ "fewer than 3 paired analysis units",
      n_unique_a < 2L | n_unique_b < 2L ~ "constant or degenerate values",
      !is.finite(rho_spearman) ~ "Spearman correlation undefined",
      TRUE ~ NA_character_
    ),
    pair_label = paste(config_a_label, "to", config_b_label),
    core_artifact_version = CORE_VERSION,
    rq1_analysis_version = RQ1_VERSION,
    rank_estimand = "Spearman correlation across paired RQ1 analysis units"
  ) |>
  select(
    all_of(rank_group_vars), pair_label, n_participants, n_units, n_unique_a, n_unique_b,
    A_mean_absolute, rho_spearman, rank_loss, rank_preservation_available,
    rank_unavailable_reason, rank_estimand, core_artifact_version, rq1_analysis_version
  )
readr::write_csv(rank_base, RANK_CSV, na = "")
rm(rank_fragments, rank_rows)
invisible(gc(FALSE))
'''
    fig1 = fig1[:insert_at] + rank_block + "\n" + fig1[insert_at:]

    panel_a_start = "# -----------------------------------------------------------------------------\n# a. Configuration sensitivity as four parallel distribution strips"
    panel_a_end = "# -----------------------------------------------------------------------------\n# Supplementary complete metric-level atlas retained unchanged in meaning"
    panel_a_new = r'''# -----------------------------------------------------------------------------
# a. Absolute versus relational preservation across measurement dimensions
# -----------------------------------------------------------------------------
# Each metric is collapsed within a dimension over the target-aligned contrasts
# (placement/optical) or refinement-to-anchor contrasts (temporal/duration).
# A and rank loss remain separate estimands: the first quantifies standardized
# level distortion and the second quantifies loss of ordering across paired
# analysis units. Circular-time representations are omitted from the rank axis.
dimension_metric <- rank_base |>
  filter(rank_preservation_available, is.finite(A_mean_absolute), is.finite(rank_loss)) |>
  group_by(dimension, metric, metric_class) |>
  summarise(
    n_oriented_pairs = n(),
    A_typical = median(A_mean_absolute, na.rm = TRUE),
    rank_loss_typical = median(rank_loss, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    dimension = factor(dimension, levels = DIMENSIONS, labels = unname(DIM_TITLES[DIMENSIONS])),
    metric_class = factor(metric_class, levels = METRIC_CLASSES)
  )

dimension_summary <- dimension_metric |>
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
if (!nrow(dimension_summary)) stop("No non-circular RQ1 rows available for Fig. 1a")

readr::write_csv(
  dimension_summary |>
    mutate(dimension = as.character(dimension), metric_class = as.character(metric_class)),
  file.path("results", "rq1", "fig1_panel_a_aggregated.csv"), na = ""
)

rank_loss_limit <- max(.05, max(dimension_metric$rank_loss_typical, na.rm = TRUE) * 1.06)
p1a <- ggplot(dimension_metric, aes(A_typical, rank_loss_typical, color = metric_class)) +
  geom_hline(yintercept = 0, linewidth = .24, color = "#D7DADD") +
  geom_point(size = .68, alpha = .30) +
  geom_segment(
    data = dimension_summary,
    aes(x = A_q25, xend = A_q75, y = rank_loss_median, yend = rank_loss_median, color = metric_class),
    inherit.aes = FALSE, linewidth = .78, alpha = .45, lineend = "round"
  ) +
  geom_segment(
    data = dimension_summary,
    aes(x = A_median, xend = A_median, y = rank_loss_q25, yend = rank_loss_q75, color = metric_class),
    inherit.aes = FALSE, linewidth = .78, alpha = .45, lineend = "round"
  ) +
  geom_point(
    data = dimension_summary,
    aes(A_median, rank_loss_median, color = metric_class),
    inherit.aes = FALSE, shape = 18, size = 1.85, alpha = .98
  ) +
  facet_grid(. ~ dimension) +
  scale_color_ms_metric(guide = "none") +
  scale_x_continuous(
    trans = scales::transform_asinh(), breaks = scales::breaks_extended(n = 4),
    expand = expansion(mult = c(.02, .03))
  ) +
  scale_y_continuous(
    limits = c(0, rank_loss_limit), breaks = scales::breaks_extended(n = 4),
    expand = expansion(mult = c(0, .03))
  ) +
  labs(
    title = "a  Absolute and relational preservation under configuration change",
    subtitle = "metric points; diamonds = class medians; bars = IQR · lower left = stronger preservation",
    x = "typical absolute distortion A", y = "typical rank loss  1 − Spearman ρ"
  ) +
  theme_fig1(base_size = 6.65) +
  theme(
    panel.grid.major = element_blank(),
    strip.text.x = element_text(size = FIG1_SUBPANEL_TITLE_SIZE, hjust = .5),
    panel.spacing.x = grid::unit(2.4, "mm"),
    plot.subtitle = element_text(size = 4.85, colour = "#666A6D", margin = margin(t = -1, b = 1)),
    plot.margin = margin(2, 3, 2, 5)
  )
'''
    fig1 = replace_between(fig1, panel_a_start, panel_a_end, panel_a_new)

    fig1 = fig1.replace(
        '  p1a, lower, ncol = 1, rel_heights = c(.72, 1.58),',
        '  p1a, lower, ncol = 1, rel_heights = c(.82, 1.52),'
    )
    fig1 = fig1.replace(
        '      "rq1_pairwise_summary + rq1_local_transition_summary",',
        '      "rq1_pairwise_change_long (derived Spearman) + rq1_pairwise_summary + rq1_local_transition_summary",'
    )
    fig1 = fig1.replace(
        'message("Fig. 1 complete: compact distribution-led overview, combined A/B geometry, and local-response distribution strips; full metric atlas retained as supplement.")',
        'message("Fig. 1 complete: level-versus-rank preservation, A/B geometry, and local-response distribution strips; full metric atlas retained as supplement.")'
    )
    fig1_path.write_text(fig1, encoding="utf-8")


fig4_path = Path("scripts/15_plot_rq3.R")
fig4 = fig4_path.read_text(encoding="utf-8")
if "fig4_resolved_coverage.csv" not in fig4:
    p4a_start = "p4a <- ggplot(requirement_summary, aes(epsilon, rank_median, color = metric_class)) +"
    p4a_end = "# b. Distribution of the empirical entry threshold R_obs at each ordered state."
    p4a_new = r'''resolved_coverage <- requirement_grid |>
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
  geom_area(fill = "#D9DDE0", alpha = .62, position = "identity") +
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
  labs(x = "tolerance ε", y = "resolved\ncoverage") +
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
'''
    fig4 = replace_between(fig4, p4a_start, p4a_end, p4a_new)
    fig4 = fig4.replace(
        '  p4a, fig4_bottom, ncol = 1, rel_heights = c(1.08, .92),',
        '  p4a, fig4_bottom, ncol = 1, rel_heights = c(1.14, .86),'
    )
    write_marker = 'readr::write_csv(\n  requirement_summary |>'
    idx = fig4.find(write_marker)
    if idx < 0:
        raise RuntimeError("Fig. 4 output-write marker not found")
    coverage_write = r'''readr::write_csv(
  resolved_coverage |>
    mutate(dimension = as.character(dimension)),
  file.path("results", "rq3", "fig4_resolved_coverage.csv"), na = ""
)

'''
    fig4 = fig4[:idx] + coverage_write + fig4[idx:]
    fig4_path.write_text(fig4, encoding="utf-8")


doc_path = Path("docs/FIGURE_ARCHITECTURE.md")
doc = doc_path.read_text(encoding="utf-8")
old = "The current plot scripts read only frozen RQ outputs. Main figures are written as\nPNG files to the centralized `results/figures` directory. RQ-specific artifact\nmanifests remain under `results/rq1`, `results/rq2`, and `results/rq3`."
new = "The current plot scripts read only frozen RQ outputs. Fig. 1 additionally derives a\nlightweight Spearman rank-preservation diagnostic directly from the frozen RQ1\npairwise artifact and writes it as a separate figure-level audit CSV; it does not\nchange the canonical RQ1 estimand or any downstream sufficiency calculation. Main\nfigures are written as PNG files to the centralized `results/figures` directory.\nRQ-specific artifact manifests remain under `results/rq1`, `results/rq2`, and\n`results/rq3`."
if old in doc:
    doc = doc.replace(old, new)
doc = doc.replace(
    "- Fig. 1a: normalized metric-level sensitivity across placement, optical,\n  temporal resolution, and monitoring duration;",
    "- Fig. 1a: absolute standardized distortion versus Spearman rank loss across\n  placement, optical, temporal-resolution and monitoring-duration contrasts;\n  ordinary rank preservation is not assigned to circular-time representations;"
)
doc = doc.replace(
    "- Fig. 4a: tolerance-dependent minimum sufficient requirement rank for the two\n  ordered dimensions;",
    "- Fig. 4a: tolerance-dependent minimum sufficient requirement rank for the two\n  ordered dimensions, with a thin strip showing the fraction of metrics for which\n  a resolved sufficient state is observed within the candidate domain;"
)
doc_path.write_text(doc, encoding="utf-8")

print("One-shot figure preservation/coverage update applied.")
