suppressPackageStartupMessages({
  library(tidyverse)
  library(cowplot)
})
source("scripts/utils/figure_style.R")
source("scripts/utils/figure_atlas.R")
source("scripts/utils/plot_contracts.R")

# RQ3 follows the inferential sequence: observed residual instability -> local
# convergence -> tolerance-based sufficiency. Placement/optical are shown as
# target-aligned substitutability, while Pareto dominance remains restricted to
# temporal-resolution and monitoring-duration burden within each facet.
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
                        PARETO_CSV, FREQUENCY_CSV), "RQ3 plotting inputs")
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

METRIC_CLASSES <- MS_METRIC_CLASSES
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

ms_plot_require_columns(rq1_summary, c("metric", "metric_class", "dimension", "A_mean_absolute"), "rq1_pairwise_summary.csv")
ms_plot_require_columns(
  observed,
  c("core_artifact_version", "rq1_analysis_version", "rq3_analysis_version", "dimension",
    "site", "Id", "metric", "metric_class", "state_label", "requirement_rank", "R_obs", "status"),
  "rq3_sufficiency_long.rds"
)
ms_plot_require_columns(sufficiency, c("dimension", "metric", "metric_class", "epsilon", "sufficient", "status"), "rq3_sufficiency_long.csv")
ms_plot_require_columns(requirement, c("dimension", "metric", "epsilon", "sufficient_states", "sufficient_set_threshold_like"), "rq3_single_dimension_requirement.csv")
ms_plot_require_columns(
  unordered,
  c("dimension", "comparison_pair_id", "config_a_label", "config_b_label", "metric", "metric_class",
    "orientation_type", "epsilon_entry", "A", "B"),
  "rq3_unordered_substitutability.csv"
)
ms_plot_require_columns(coverage, c("dimension", "comparison_pair_id", "epsilon", "fraction_metrics_substitutable"), "rq3_unordered_coverage_curves.csv")
ms_plot_require_columns(convergence, c("dimension", "metric", "metric_class", "G", "requirement_position", "boundary_proximity"), "rq3_convergence_profile.csv")
ms_plot_require_columns(joint, c("core_artifact_version", "rq1_analysis_version", "rq3_analysis_version", "placement", "optical", "resolution_s", "n_days", "metric", "status", "epsilon_entry"), "rq3_joint_summary.csv")
ms_plot_require_columns(pareto, c("placement", "optical", "metric", "metric_class", "resolution_s", "n_days", "ever_pareto", "pareto_persistence"), "rq3_pareto_frontiers.csv")
ms_plot_require_columns(frequency, c("placement", "optical", "resolution_s", "n_days", "fraction_metrics_ever_pareto", "mean_pareto_persistence"), "rq3_pareto_frequency.csv")

RQ1_VERSION <- ms_plot_one_version(c(observed$rq1_analysis_version, joint$rq1_analysis_version), "rq1_analysis_version")
RQ3_VERSION <- ms_plot_one_version(c(observed$rq3_analysis_version, joint$rq3_analysis_version), "rq3_analysis_version")
CORE_VERSION <- ms_plot_assert_core(c(observed$core_artifact_version, joint$core_artifact_version))
ms_plot_assert_prefix(RQ1_VERSION, "rq1_v5_", "rq1_analysis_version")
ms_plot_assert_prefix(RQ3_VERSION, "rq3_v4_", "rq3_analysis_version")

metric_order <- ms_metric_order(rq1_summary)

# Fig. 4a: residual instability is the primary ordered-dimension quantity.
# Display medians collapse repeated participant/window realizations only for
# visual clarity; R_obs itself is already frozen upstream.
observed_display <- observed |>
  filter(dimension %in% c("temporal", "duration"), status == "resolved", is.finite(R_obs)) |>
  group_by(dimension, metric, metric_class, requirement_rank) |>
  summarise(R_display = median(R_obs, na.rm = TRUE), .groups = "drop") |>
  mutate(
    metric_class = factor(metric_class, levels = METRIC_CLASSES),
    dimension = factor(dimension, levels = c("temporal", "duration"))
  )
boundary_marks <- tibble(
  dimension = factor(c("temporal", "duration"), levels = c("temporal", "duration")),
  requirement_rank = c(7, 6), label = "observed boundary\nunresolved"
)
p4a <- ggplot(observed_display, aes(requirement_rank, R_display, group = metric, color = metric_class)) +
  geom_line(alpha = .55, linewidth = .40) +
  geom_point(size = .55, alpha = .72) +
  geom_vline(data = boundary_marks, aes(xintercept = requirement_rank), inherit.aes = FALSE,
             linetype = 3, linewidth = .35, color = "#777777") +
  geom_text(data = boundary_marks, aes(x = requirement_rank, y = Inf, label = label), inherit.aes = FALSE,
            vjust = 1.15, hjust = 1.05, size = 1.8, color = "#666666") +
  facet_wrap(~dimension, nrow = 1, scales = "free_x") +
  scale_color_ms_metric() +
  scale_y_continuous(trans = scales::transform_asinh(), breaks = scales::breaks_extended(n = 4)) +
  labs(title = "a  Observed residual instability", x = "measurement-requirement position",
       y = "R_obs = max A to higher observed states") +
  theme_ms(base_size = 6.4, legend_position = "bottom")

# Fig. 4b: adjacent G describes the marginal representational change at each
# observed step and therefore the empirical convergence profile.
convergence_display <- convergence |>
  filter(dimension %in% c("temporal", "duration"), is.finite(G), is.finite(requirement_position)) |>
  group_by(dimension, metric, metric_class, requirement_position) |>
  summarise(G_display = median(G, na.rm = TRUE), .groups = "drop") |>
  mutate(metric_class = factor(metric_class, levels = METRIC_CLASSES))
p4b <- ggplot(convergence_display, aes(requirement_position, G_display, group = metric, color = metric_class)) +
  geom_line(alpha = .58, linewidth = .42) +
  geom_point(size = .55, alpha = .72) +
  facet_wrap(~dimension, nrow = 1, scales = "free_x") +
  scale_color_ms_metric() +
  scale_y_continuous(trans = scales::transform_asinh(), breaks = scales::breaks_extended(n = 4)) +
  labs(title = "b  Local adjacent-transition change", x = "measurement-requirement position",
       y = "G = mean |z|") +
  theme_ms(base_size = 6.4, legend_position = "bottom")

# Fig. 4c: tolerance converts observed residual instability into a decision
# projection. It is intentionally downstream of panels a-b.
sufficiency_plot <- sufficiency |>
  filter(dimension %in% c("temporal", "duration")) |>
  group_by(dimension, metric, metric_class, epsilon) |>
  summarise(
    resolved_fraction = mean(status == "resolved", na.rm = TRUE),
    fraction_sufficient = if (any(status == "resolved")) mean(sufficient[status == "resolved"], na.rm = TRUE) else NA_real_,
    .groups = "drop"
  ) |>
  mutate(dimension = factor(dimension, levels = c("temporal", "duration")))
p4c <- ggplot(sufficiency_plot, aes(epsilon, fraction_sufficient, group = metric, color = metric_class)) +
  geom_hline(yintercept = c(0, 1), linewidth = .25, color = "#C5C5C5") +
  geom_line(aes(y = resolved_fraction), color = "#777777", linetype = 3, linewidth = .28, alpha = .72) +
  geom_line(alpha = .62, linewidth = .42) +
  facet_wrap(~dimension, nrow = 1, scales = "free_x") +
  scale_color_ms_metric() +
  scale_x_continuous(trans = scales::transform_asinh(), breaks = scales::breaks_extended(n = 4)) +
  scale_y_continuous(limits = c(0, 1), labels = scales::label_percent(accuracy = 25)) +
  labs(title = "c  Tolerance projection of observed sufficiency", x = "tolerance ε",
       y = "sufficient among resolved states; dashed = resolved coverage") +
  theme_ms(base_size = 6.4, legend_position = "bottom")

# Fig. 4d: placement/optical alternatives are compared with their task-aligned
# states rather than placed on the temporal/duration burden order.
pair_labels <- unordered |>
  distinct(dimension, comparison_pair_id, config_a_label, config_b_label) |>
  mutate(pair = paste(config_a_label, "→", config_b_label))
coverage_plot <- coverage |>
  left_join(pair_labels, by = c("dimension", "comparison_pair_id")) |>
  mutate(
    pair = coalesce(pair, comparison_pair_id),
    dimension = factor(dimension, levels = c("placement", "optical"))
  )
p4d <- ggplot(coverage_plot, aes(epsilon, fraction_metrics_substitutable, color = pair, group = pair)) +
  geom_line(linewidth = .55, alpha = .82) +
  facet_wrap(~dimension, nrow = 1, scales = "free_x") +
  scale_y_continuous(limits = c(0, 1), labels = scales::label_percent(accuracy = 25)) +
  labs(title = "d  Target-aligned substitutability", x = "tolerance ε",
       y = "fraction of metrics substitutable", color = "oriented comparison") +
  theme_ms(base_size = 6.4, legend_position = "bottom")

fig4_top <- cowplot::plot_grid(p4a, p4b, ncol = 2, rel_widths = c(1, 1))
fig4_bottom <- cowplot::plot_grid(p4c, p4d, ncol = 2, rel_widths = c(1, 1))
fig4 <- cowplot::plot_grid(fig4_top, fig4_bottom, ncol = 1, rel_heights = c(1, 1))
ms_plot_save(fig4, file.path(OUT_DIR, "Fig4_RQ3.pdf"), 14.5, 10.5)
ms_plot_save(fig4, file.path(OUT_DIR, "Fig4_RQ3.png"), 14.5, 10.5)

# Fig. 5: Pareto efficiency is evaluated only over temporal resolution and
# monitoring duration. Facet labels identify the target-aligned placement /
# optical states without inserting target alignment into Pareto dominance.
placement_labels <- c(
  eye = "Eye\n(target-aligned)", chest = "Chest", wrist = "Wrist"
)
optical_labels <- c(
  MEDI = "MEDI\n(target-aligned)", LIGHT = "LIGHT"
)
facet_labeller <- labeller(placement = placement_labels, optical = optical_labels)

pareto_plot <- pareto |>
  filter(ever_pareto, is.finite(resolution_s), is.finite(n_days)) |>
  mutate(metric_class = factor(metric_class, levels = METRIC_CLASSES))
p5a <- ggplot(pareto_plot, aes(resolution_s, n_days, color = metric_class, size = pareto_persistence)) +
  geom_point(alpha = .78) +
  facet_grid(placement ~ optical, scales = "free", space = "free", labeller = facet_labeller) +
  scale_color_ms_metric() +
  scale_size_continuous(range = c(.45, 3.0), limits = c(0, 1), name = "Pareto persistence") +
  scale_x_continuous(breaks = c(10, 20, 30, 60, 300, 900, 1800),
                     labels = c("10 s", "20 s", "30 s", "1 m", "5 m", "15 m", "30 m")) +
  scale_y_continuous(breaks = 1:6, labels = paste0(1:6, " d")) +
  labs(title = "a  Pareto persistence within target-alignment facets",
       x = "temporal resolution", y = "monitoring duration") +
  theme_ms(base_size = 6.5, legend_position = "bottom") +
  theme(axis.text.x = element_text(angle = 42, hjust = 1))

frequency_plot <- frequency |>
  mutate(mean_pareto_persistence = pmax(0, pmin(1, mean_pareto_persistence)))
p5b <- ggplot(frequency_plot, aes(resolution_s, n_days, fill = mean_pareto_persistence)) +
  geom_tile(color = "white", linewidth = .14) +
  facet_grid(placement ~ optical, scales = "free", space = "free", labeller = facet_labeller) +
  scale_fill_ms_sequential(name = "mean Pareto\npersistence", limits = c(0, 1),
                           labels = scales::label_percent(accuracy = 25)) +
  scale_x_continuous(breaks = c(10, 20, 30, 60, 300, 900, 1800),
                     labels = c("10 s", "20 s", "30 s", "1 m", "5 m", "15 m", "30 m")) +
  scale_y_continuous(breaks = 1:6, labels = paste0(1:6, " d")) +
  labs(title = "b  Mean Pareto persistence across metrics",
       x = "temporal resolution", y = "monitoring duration") +
  theme_ms(base_size = 6.5, legend_position = "right") +
  theme(axis.text.x = element_text(angle = 42, hjust = 1))

fig5 <- cowplot::plot_grid(p5a, p5b, ncol = 2, rel_widths = c(1.05, .95), labels = NULL)
ms_plot_save(fig5, file.path(OUT_DIR, "Fig5_RQ3.pdf"), 14.2, 8.5)
ms_plot_save(fig5, file.path(OUT_DIR, "Fig5_RQ3.png"), 14.2, 8.5)

p_sub <- ggplot(coverage_plot, aes(epsilon, fraction_metrics_substitutable, color = pair, group = pair)) +
  geom_line(linewidth = .55) +
  facet_wrap(~dimension, scales = "free_x") +
  scale_y_continuous(limits = c(0, 1), labels = scales::label_percent(accuracy = 25)) +
  labs(title = "RQ3 target-aligned substitutability coverage", x = "tolerance ε",
       y = "fraction of metrics substitutable", color = "oriented comparison") +
  theme_ms(base_size = 6.5, legend_position = "bottom")
ms_plot_save(p_sub, file.path(OUT_DIR, "FigS_RQ3_unordered_substitutability.pdf"), 12.5, 6.8)
ms_plot_save(p_sub, file.path(OUT_DIR, "FigS_RQ3_unordered_substitutability.png"), 12.5, 6.8)

ms_plot_write_manifest(
  file.path(OUT_DIR, "figure_artifact_manifest.csv"),
  tibble(
    figure = c("Fig4_RQ3", "Fig5_RQ3", "FigS_RQ3_unordered_substitutability"),
    input_artifact = c(
      "rq3_observed_stability + rq3_convergence_profile + rq3_sufficiency_long + rq3_unordered_coverage_curves",
      "rq3_pareto_frontiers + rq3_pareto_frequency",
      "rq3_unordered_coverage_curves"
    ),
    core_artifact_version = CORE_VERSION, rq1_analysis_version = RQ1_VERSION,
    rq2_analysis_version = NA_character_, rq3_analysis_version = RQ3_VERSION
  )
)
message("RQ3 figures complete: residual instability, local convergence, sufficiency projection, target-aligned substitutability, and Pareto persistence.")
