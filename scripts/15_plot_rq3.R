suppressPackageStartupMessages({
  library(tidyverse)
  library(cowplot)
})
source("scripts/utils/figure_style.R")
source("scripts/utils/figure_atlas.R")
source("scripts/utils/plot_contracts.R")

# RQ3 is plotted as observed stability, tolerance projection, unordered
# substitutability, and facet-specific Pareto occupancy. No universal burden
# order is imposed on placement or optical states.
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
ms_plot_require_columns(observed, c("core_artifact_version", "rq1_analysis_version", "rq3_analysis_version", "dimension", "metric", "R_obs", "status"), "rq3_sufficiency_long.rds")
ms_plot_require_columns(sufficiency, c("dimension", "metric", "metric_class", "epsilon", "sufficient", "status"), "rq3_sufficiency_long.csv")
ms_plot_require_columns(requirement, c("dimension", "metric", "epsilon", "sufficient_states", "sufficient_set_threshold_like"), "rq3_single_dimension_requirement.csv")
ms_plot_require_columns(unordered, c("dimension", "comparison_pair_id", "metric", "metric_class", "epsilon_entry", "A", "B"), "rq3_unordered_substitutability.csv")
ms_plot_require_columns(coverage, c("dimension", "comparison_pair_id", "epsilon", "fraction_metrics_substitutable"), "rq3_unordered_coverage_curves.csv")
ms_plot_require_columns(convergence, c("dimension", "metric", "G", "requirement_position", "boundary_proximity"), "rq3_convergence_profile.csv")
ms_plot_require_columns(joint, c("core_artifact_version", "rq1_analysis_version", "rq3_analysis_version", "placement", "optical", "resolution_s", "n_days", "metric", "status", "epsilon_entry"), "rq3_joint_summary.csv")
ms_plot_require_columns(pareto, c("placement", "optical", "metric", "metric_class", "resolution_s", "n_days", "ever_pareto", "pareto_persistence"), "rq3_pareto_frontiers.csv")
ms_plot_require_columns(frequency, c("placement", "optical", "resolution_s", "n_days", "fraction_metrics_ever_pareto"), "rq3_pareto_frequency.csv")

RQ1_VERSION <- ms_plot_one_version(c(observed$rq1_analysis_version, joint$rq1_analysis_version), "rq1_analysis_version")
RQ3_VERSION <- ms_plot_one_version(c(observed$rq3_analysis_version, joint$rq3_analysis_version), "rq3_analysis_version")
CORE_VERSION <- ms_plot_assert_core(c(observed$core_artifact_version, joint$core_artifact_version))
ms_plot_assert_prefix(RQ1_VERSION, "rq1_v5_", "rq1_analysis_version")
ms_plot_assert_prefix(RQ3_VERSION, "rq3_v4_", "rq3_analysis_version")

metric_order <- ms_metric_order(rq1_summary)
metric_classes <- METRIC_CLASSES

# Fig. 4a: tolerance projection of observed residual instability. This is a
# metric-level summary of sufficient states, not a universal threshold claim.
sufficiency_plot <- sufficiency |>
  filter(dimension %in% c("temporal", "duration")) |>
  group_by(dimension, metric, metric_class, epsilon) |>
  summarise(
    resolved_fraction = mean(status == "resolved", na.rm = TRUE),
    fraction_sufficient = if (any(status == "resolved")) mean(sufficient[status == "resolved"], na.rm = TRUE) else NA_real_,
    .groups = "drop"
  ) |>
  mutate(dimension = factor(dimension, levels = c("temporal", "duration"))) |>
  mutate(dimension = as.character(dimension)) |>
  ms_add_metric_order(metric_order)
p4a <- ggplot(sufficiency_plot, aes(epsilon, fraction_sufficient, group = metric, color = metric_class)) +
  geom_hline(yintercept = c(0, 1), linewidth = .25, color = "#C5C5C5") +
  geom_line(aes(y = resolved_fraction), color = "#777777", linetype = 3, linewidth = .28, alpha = .72) +
  geom_line(alpha = .62, linewidth = .42) +
  facet_wrap(~dimension, nrow = 1, scales = "free_x") +
  scale_color_ms_metric() +
  scale_x_continuous(trans = scales::transform_asinh(), breaks = scales::breaks_extended(n = 4)) +
  scale_y_continuous(limits = c(0, 1), labels = scales::label_percent(accuracy = 25)) +
  labs(title = "a  Ordered-dimension sufficiency projection", x = "tolerance ε", y = "sufficient among resolved states; dashed = resolved coverage") +
  theme_ms(base_size = 6.6, legend_position = "bottom")

# Fig. 4b: local convergence profile, retaining boundary proximity rather than
# inventing a continuous burden order for unordered dimensions.
convergence_plot <- convergence |>
  filter(dimension %in% c("temporal", "duration")) |>
  left_join(metric_order |> select(metric, metric_class_order = metric_class), by = "metric") |>
  mutate(metric_class = factor(coalesce(as.character(metric_class), as.character(metric_class_order)), levels = METRIC_CLASSES))
p4b <- ggplot(convergence_plot, aes(requirement_position, G, group = metric, color = metric_class)) +
  geom_hline(yintercept = 0, linewidth = .25, color = "#C5C5C5") +
  geom_line(alpha = .64, linewidth = .45) +
  geom_point(size = .55, alpha = .72) +
  facet_grid(metric_class ~ dimension, scales = "free", space = "free", switch = "y") +
  scale_color_ms_metric() +
  labs(title = "b  Local adjacent-transition stability", x = "observed requirement position", y = "G = mean |z|") +
  theme_ms(base_size = 6.1, legend_position = "none") +
  theme(axis.text.y = element_text(size = 5.0), strip.text.y.left = element_text(size = 5.2))

# Fig. 4c: placement/optical substitutability is displayed as pair-specific
# coverage, with no ordering between the incomparable facets.
coverage_plot <- coverage |>
  mutate(pair = comparison_pair_id, dimension = factor(dimension, levels = c("placement", "optical")))
p4c <- ggplot(coverage_plot, aes(epsilon, fraction_metrics_substitutable, color = pair, group = pair)) +
  geom_line(linewidth = .55, alpha = .82) +
  facet_wrap(~dimension, nrow = 1, scales = "free_x") +
  scale_y_continuous(limits = c(0, 1), labels = scales::label_percent(accuracy = 25)) +
  labs(title = "c  Unordered placement/optical substitutability", x = "entry tolerance ε", y = "fraction of metrics substitutable", color = "comparison pair") +
  theme_ms(base_size = 6.4, legend_position = "bottom")

fig4 <- cowplot::plot_grid(p4a, p4b, p4c, ncol = 1, rel_heights = c(1.05, 1.15, .95), labels = NULL)
ms_plot_save(fig4, file.path(OUT_DIR, "Fig4_RQ3.pdf"), 14, 12.2)
ms_plot_save(fig4, file.path(OUT_DIR, "Fig4_RQ3.png"), 14, 12.2)

# Fig. 5: Pareto occupancy is shown within fixed placement x optical facets.
# Dominance is only over temporal resolution and monitoring duration.
pareto_plot <- pareto |>
  filter(ever_pareto, is.finite(resolution_s), is.finite(n_days)) |>
  mutate(metric_class = factor(metric_class, levels = METRIC_CLASSES))
p5a <- ggplot(pareto_plot, aes(resolution_s, n_days, color = metric_class, size = pareto_persistence)) +
  geom_point(alpha = .78) +
  facet_grid(placement ~ optical, scales = "free", space = "free") +
  scale_color_ms_metric() +
  scale_size_continuous(range = c(.45, 3.0), limits = c(0, 1), name = "Pareto persistence") +
  scale_x_continuous(breaks = c(10, 20, 30, 60, 300, 900, 1800), labels = c("10 s", "20 s", "30 s", "1 m", "5 m", "15 m", "30 m")) +
  scale_y_continuous(breaks = 1:6, labels = paste0(1:6, " d")) +
  labs(title = "a  Facet-specific Pareto occupancy", x = "temporal resolution", y = "monitoring duration") +
  theme_ms(base_size = 6.5, legend_position = "bottom") +
  theme(axis.text.x = element_text(angle = 42, hjust = 1))

frequency_plot <- frequency |>
  mutate(fraction_metrics_ever_pareto = pmax(0, pmin(1, fraction_metrics_ever_pareto)))
p5b <- ggplot(frequency_plot, aes(resolution_s, n_days, fill = fraction_metrics_ever_pareto)) +
  geom_tile(color = "white", linewidth = .14) +
  facet_grid(placement ~ optical, scales = "free", space = "free") +
  scale_fill_ms_sequential(name = "fraction of metrics\never Pareto", limits = c(0, 1), labels = scales::label_percent(accuracy = 25)) +
  scale_x_continuous(breaks = c(10, 20, 30, 60, 300, 900, 1800), labels = c("10 s", "20 s", "30 s", "1 m", "5 m", "15 m", "30 m")) +
  scale_y_continuous(breaks = 1:6, labels = paste0(1:6, " d")) +
  labs(title = "b  Pareto frequency across metrics", x = "temporal resolution", y = "monitoring duration") +
  theme_ms(base_size = 6.5, legend_position = "right") +
  theme(axis.text.x = element_text(angle = 42, hjust = 1))

fig5 <- cowplot::plot_grid(p5a, p5b, ncol = 2, rel_widths = c(1.05, .95), labels = NULL)
ms_plot_save(fig5, file.path(OUT_DIR, "Fig5_RQ3.pdf"), 14.2, 8.5)
ms_plot_save(fig5, file.path(OUT_DIR, "Fig5_RQ3.png"), 14.2, 8.5)

ms_plot_save(
  ggplot(coverage_plot, aes(epsilon, fraction_metrics_substitutable, color = pair, group = pair)) +
    geom_line(linewidth = .55) + facet_wrap(~dimension, scales = "free_x") +
    scale_y_continuous(limits = c(0, 1), labels = scales::label_percent(accuracy = 25)) +
    labs(title = "RQ3 unordered substitutability coverage", x = "entry tolerance ε", y = "fraction of metrics substitutable", color = "comparison pair") +
    theme_ms(base_size = 6.5, legend_position = "bottom"),
  file.path(OUT_DIR, "FigS_RQ3_unordered_substitutability.pdf"), 12.5, 6.8
)
ms_plot_save(
  ggplot(coverage_plot, aes(epsilon, fraction_metrics_substitutable, color = pair, group = pair)) +
    geom_line(linewidth = .55) + facet_wrap(~dimension, scales = "free_x") +
    scale_y_continuous(limits = c(0, 1), labels = scales::label_percent(accuracy = 25)) +
    labs(title = "RQ3 unordered substitutability coverage", x = "entry tolerance ε", y = "fraction of metrics substitutable", color = "comparison pair") +
    theme_ms(base_size = 6.5, legend_position = "bottom"),
  file.path(OUT_DIR, "FigS_RQ3_unordered_substitutability.png"), 12.5, 6.8
)

ms_plot_write_manifest(
  file.path(OUT_DIR, "figure_artifact_manifest.csv"),
  tibble(
    figure = c("Fig4_RQ3", "Fig5_RQ3", "FigS_RQ3_unordered_substitutability"),
    input_artifact = c("rq3_sufficiency_long + rq3_convergence_profile + rq3_unordered_coverage_curves", "rq3_pareto_frontiers + rq3_pareto_frequency", "rq3_unordered_coverage_curves"),
    core_artifact_version = CORE_VERSION, rq1_analysis_version = RQ1_VERSION,
    rq2_analysis_version = NA_character_, rq3_analysis_version = RQ3_VERSION
  )
)
message("RQ3 figures complete: tolerance projection, local stability, unordered substitutability, and facet-specific Pareto occupancy.")
