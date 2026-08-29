from pathlib import Path
import re

path = Path('scripts/15_plot_rq3.R')
text = path.read_text(encoding='utf-8')
original = text

# Remove Pareto-only input declarations, reads, file requirements, and schema checks.
text = text.replace('PARETO_CSV <- file.path("results", "rq3", "rq3_pareto_frontiers.csv")\n', '')
text = text.replace('FREQUENCY_CSV <- file.path("results", "rq3", "rq3_pareto_frequency.csv")\n', '')
text = text.replace('                        UNORDERED_CSV, COVERAGE_CSV, CONVERGENCE_CSV, JOINT_CSV,\n                        PARETO_CSV, FREQUENCY_CSV), "RQ3 v5 plotting inputs")',
                    '                        UNORDERED_CSV, COVERAGE_CSV, CONVERGENCE_CSV, JOINT_CSV),\n                        "RQ3 v5 plotting inputs")')
text = text.replace('pareto <- readr::read_csv(PARETO_CSV, show_col_types = FALSE, progress = FALSE)\n', '')
text = text.replace('frequency <- readr::read_csv(FREQUENCY_CSV, show_col_types = FALSE, progress = FALSE)\n', '')
text = re.sub(r'ms_plot_require_columns\(pareto,\n.*?"rq3_pareto_frontiers\.csv"\)\n', '', text, flags=re.S)
text = re.sub(r'ms_plot_require_columns\(frequency,\n.*?"rq3_pareto_frequency\.csv"\)\n', '', text, flags=re.S)

# Remove the obsolete Pareto pre-computation at the start of Fig. 5.
text = re.sub(
    r'# Retain the frozen Pareto summaries for the supplementary audit view below\.\n.*?\n# Main-text Fig\. 5 maps the joint temporal-resolution × duration design space\.',
    '# Main-text Fig. 5 maps the joint temporal-resolution × duration design space.',
    text, flags=re.S
)

# Replace low-information marginal-return panel with tolerance-resolved feasible-region slices.
start = text.index('# c. Conditional marginal returns.')
end = text.index('fig5_top <- cowplot::plot_grid(', start)
new_c = r'''# c. Tolerance-resolved feasible regions. Each tile is the fraction of target
# metrics for which the joint configuration is confirmed sufficient at the stated
# tolerance. Unresolved metric/configuration cells therefore do not count as
# sufficient. The dark outline marks configurations that are non-dominated in
# temporal burden, duration burden, and confirmed metric coverage at that slice.
FIG5_TOLERANCE_SLICES <- c(.10, .25, .50, .75)

joint_metric_status <- joint_plot_base |>
  filter(!is.na(metric_class)) |>
  group_by(metric, metric_class, resolution_s, resolution_rank, n_days) |>
  summarise(
    epsilon_metric = if (any(status == "resolved" & is.finite(epsilon_entry))) {
      median(epsilon_entry[status == "resolved" & is.finite(epsilon_entry)], na.rm = TRUE)
    } else NA_real_,
    .groups = "drop"
  )

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
    subtitle = "fill = confirmed metric coverage; dark outline = coverage-efficient frontier",
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

'''
text = text[:start] + new_c + text[end:]

# Give the tolerance-slice row enough room without making the figure taller than necessary.
text = text.replace(
    'fig5_top, p5c, ncol = 1, rel_heights = c(1.22, .78),',
    'fig5_top, p5c, ncol = 1, rel_heights = c(1.10, .90),'
)
text = text.replace(
    'ms_plot_save(fig5_body, file.path(OUT_DIR, "Fig5_RQ3.pdf"), 9.0, 6.3)\nms_plot_save(fig5_body, file.path(OUT_DIR, "Fig5_RQ3.png"), 9.0, 6.3)',
    'ms_plot_save(fig5_body, file.path(OUT_DIR, "Fig5_RQ3.pdf"), 9.0, 6.5)\nms_plot_save(fig5_body, file.path(OUT_DIR, "Fig5_RQ3.png"), 9.0, 6.5)'
)

# Replace obsolete marginal-gain outputs with the new tolerance-slice audit table.
text = re.sub(
    r'readr::write_csv\(\n  duration_metric_gain.*?fig5_temporal_marginal_gain\.csv"\), na = ""\n\)\n',
    'readr::write_csv(\n  slice_plot |>\n    mutate(epsilon_label = as.character(epsilon_label)),\n  file.path("results", "rq3", "fig5_tolerance_slice_coverage.csv"), na = ""\n)\n',
    text, flags=re.S
)

# Remove the Pareto-only supplementary figure block.
text = re.sub(
    r'# Supplement: preserve support × placement × optical explicitly as an audit view\.\n.*?\nms_plot_write_manifest\(',
    'ms_plot_write_manifest(',
    text, flags=re.S
)

# Remove later legacy Pareto display summaries; they no longer feed a figure.
text = re.sub(
    r'# Recompute Pareto display summaries from the frozen RQ3 artifact using the\n.*?\n# -----------------------------------------------------------------------------\n# Main-text display refinement for Fig\. 4\.',
    '# -----------------------------------------------------------------------------\n# Main-text display refinement for Fig. 4.',
    text, flags=re.S
)

# Update the manifest to list only figures that are still generated.
text = text.replace(
    '      "Fig4_RQ3", "Fig5_RQ3",\n      "FigS_RQ3_single_dimension_detail", "FigS_RQ3_pareto_facets"\n',
    '      "Fig4_RQ3", "Fig5_RQ3",\n      "FigS_RQ3_single_dimension_detail"\n'
)
text = text.replace(
    '      "rq3_observed_stability+sufficiency+unordered_substitutability",\n      "rq3_joint_summary+pareto_frontiers",\n      "rq3_convergence_profile+sufficiency",\n      "rq3_pareto_frontiers+frequency"\n',
    '      "rq3_observed_stability+sufficiency+unordered_substitutability",\n      "rq3_joint_summary",\n      "rq3_convergence_profile+sufficiency"\n'
)
text = text.replace(
    'message("RQ3 v5 figures complete: compact single-dimension sufficiency and joint Pareto landscapes")',
    'message("RQ3 v5 figures complete: single-dimension sufficiency and joint tolerance landscapes")'
)

# Hard invariants: no old marginal-return/Pareto figure code and exactly one Fig.5 writer.
for forbidden in [
    'Conditional marginal returns reveal joint trade-offs',
    'FigS_RQ3_pareto_facets',
    'pareto_persistence',
    'fraction_ever_pareto',
    'fig5_duration_marginal_gain.csv',
    'fig5_temporal_marginal_gain.csv',
]:
    if forbidden in text:
        raise SystemExit(f'Forbidden legacy Fig5 token remains: {forbidden}')

if text.count('"Fig5_RQ3.png"') != 1 or text.count('"Fig5_RQ3.pdf"') != 1:
    raise SystemExit('Fig5 canonical outputs must each have exactly one writer')
if 'FIG5_TOLERANCE_SLICES <- c(.10, .25, .50, .75)' not in text:
    raise SystemExit('Tolerance-slice block missing')
if 'fraction_confirmed_sufficient' not in text or 'coverage-efficient frontier' not in text:
    raise SystemExit('Tolerance-slice coverage/frontier logic missing')

if text == original:
    raise SystemExit('Patch made no changes')
path.write_text(text, encoding='utf-8')
