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
ms_plot_require_columns(observed,
  c("core_artifact_version", "rq1_analysis_version", "rq3_analysis_version", "dimension",
    "metric", "metric_class", "state_label", "requirement_rank", "R_obs", "status"),
  "rq3_sufficiency_long.rds")
ms_plot_require_columns(sufficiency, c("dimension", "metric", "metric_class", "epsilon", "sufficient", "status"), "rq3_sufficiency_long.csv")
ms_plot_require_columns(requirement, c("dimension", "metric", "epsilon", "sufficient_states", "sufficient_set_threshold_like"), "rq3_single_dimension_requirement.csv")
ms_plot_require_columns(unordered,
  c("dimension", "comparison_pair_id", "config_a_label", "config_b_label", "metric", "metric_class",
    "orientation_type", "epsilon_entry", "A", "B"), "rq3_unordered_substitutability.csv")
ms_plot_require_columns(coverage, c("dimension", "comparison_pair_id", "epsilon", "fraction_metrics_substitutable"), "rq3_unordered_coverage_curves.csv")
ms_plot_require_columns(convergence, c("dimension", "metric", "metric_class", "G", "requirement_position", "boundary_proximity"), "rq3_convergence_profile.csv")
ms_plot_require_columns(joint,
  c("core_artifact_version", "rq1_analysis_version", "rq3_analysis_version", "support_id", "placement",
    "optical", "resolution_s", "n_days", "metric", "status", "epsilon_entry"), "rq3_joint_summary.csv")
ms_plot_require_columns(pareto,
  c("support_id", "placement", "optical", "metric", "metric_class", "resolution_s", "n_days", "ever_pareto", "pareto_persistence"),
  "rq3_pareto_frontiers.csv")
ms_plot_require_columns(frequency,
  c("support_id", "placement", "optical", "resolution_s", "n_days", "fraction_metrics_ever_pareto", "mean_pareto_persistence"),
  "rq3_pareto_frequency.csv")

RQ1_VERSION <- ms_plot_one_version(c(observed$rq1_analysis_version, joint$rq1_analysis_version), "rq1_analysis_version")
RQ3_VERSION <- ms_plot_one_version(c(observed$rq3_analysis_version, joint$rq3_analysis_version), "rq3_analysis_version")
CORE_VERSION <- ms_plot_assert_core(c(observed$core_artifact_version, joint$core_artifact_version))
ms_plot_assert_prefix(RQ1_VERSION, "rq1_v5_", "rq1_analysis_version")
ms_plot_assert_prefix(RQ3_VERSION, "rq3_v5_", "rq3_analysis_version")
metric_order <- ms_metric_order(rq1_summary)

# Fig. 4a: R_obs is already a type-level estimand; no participant/window median
# is needed downstream.
observed_display <- observed |>
  filter(dimension %in% c("temporal", "duration"), status == "resolved", is.finite(R_obs)) |>
  mutate(metric_class = factor(metric_class, levels = METRIC_CLASSES),
         dimension = factor(dimension, levels = c("temporal", "duration")))
boundary_marks <- tibble(
  dimension = factor(c("temporal", "duration"), levels = c("temporal", "duration")),
  requirement_rank = c(7, 6), label = "observed boundary\nunresolved")
p4a <- ggplot(observed_display, aes(requirement_rank, R_obs, group = metric, color = metric_class)) +
  geom_line(alpha = .55, linewidth = .40) + geom_point(size = .55, alpha = .72) +
  geom_vline(data = boundary_marks, aes(xintercept = requirement_rank), inherit.aes = FALSE,
             linetype = 3, linewidth = .35, color = "#777777") +
  geom_text(data = boundary_marks, aes(x = requirement_rank, y = Inf, label = label), inherit.aes = FALSE,
            vjust = 1.15, hjust = 1.05, size = 1.8, color = "#666666") +
  facet_wrap(~dimension, nrow = 1, scales = "free_x") + scale_color_ms_metric() +
  scale_y_continuous(trans = scales::transform_asinh(), breaks = scales::breaks_extended(n = 4)) +
  labs(title = "a  Observed residual instability", x = "measurement-requirement position (low → high burden)",
       y = "R_obs = max A to higher observed states") + theme_ms(base_size = 6.4, legend_position = "bottom")

convergence_display <- convergence |>
  filter(dimension %in% c("temporal", "duration"), is.finite(G), is.finite(requirement_position)) |>
  group_by(dimension, metric, metric_class, requirement_position) |>
  summarise(G_display = median(G, na.rm = TRUE), .groups = "drop") |>
  mutate(metric_class = factor(metric_class, levels = METRIC_CLASSES))
p4b <- ggplot(convergence_display, aes(requirement_position, G_display, group = metric, color = metric_class)) +
  geom_line(alpha = .58, linewidth = .42) + geom_point(size = .55, alpha = .72) +
  facet_wrap(~dimension, nrow = 1, scales = "free_x") + scale_color_ms_metric() +
  scale_y_continuous(trans = scales::transform_asinh(), breaks = scales::breaks_extended(n = 4)) +
  labs(title = "b  Local adjacent-transition change", x = "measurement-requirement position (low → high burden)",
       y = "G = mean |z|") + theme_ms(base_size = 6.4, legend_position = "bottom")

sufficiency_plot <- sufficiency |>
  filter(dimension %in% c("temporal", "duration")) |>
  group_by(dimension, metric, metric_class, epsilon) |>
  summarise(
    resolved_fraction = mean(status == "resolved", na.rm = TRUE),
    fraction_sufficient = if (any(status == "resolved")) mean(sufficient[status == "resolved"], na.rm = TRUE) else NA_real_,
    .groups = "drop") |>
  mutate(dimension = factor(dimension, levels = c("temporal", "duration")))
p4c <- ggplot(sufficiency_plot, aes(epsilon, fraction_sufficient, group = metric, color = metric_class)) +
  geom_hline(yintercept = c(0, 1), linewidth = .25, color = "#C5C5C5") +
  geom_line(aes(y = resolved_fraction), color = "#777777", linetype = 3, linewidth = .28, alpha = .72) +
  geom_line(alpha = .62, linewidth = .42) + facet_wrap(~dimension, nrow = 1, scales = "free_x") +
  scale_color_ms_metric() + scale_x_continuous(trans = scales::transform_asinh(), breaks = scales::breaks_extended(n = 4)) +
  scale_y_continuous(limits = c(0, 1), labels = scales::label_percent(accuracy = 25)) +
  labs(title = "c  Tolerance projection of observed sufficiency", x = "tolerance ε",
       y = "sufficient among resolved states; dashed = resolved coverage") +
  theme_ms(base_size = 6.4, legend_position = "bottom")

pair_labels <- unordered |> distinct(dimension, comparison_pair_id, config_a_label, config_b_label) |>
  mutate(pair = paste(config_a_label, "→", config_b_label))
coverage_plot <- coverage |>
  left_join(pair_labels, by = c("dimension", "comparison_pair_id")) |>
  mutate(pair = coalesce(pair, comparison_pair_id), dimension = factor(dimension, levels = c("placement", "optical")))
p4d <- ggplot(coverage_plot, aes(epsilon, fraction_metrics_substitutable, color = pair, group = pair)) +
  geom_line(linewidth = .55, alpha = .82) + facet_wrap(~dimension, nrow = 1, scales = "free_x") +
  scale_y_continuous(limits = c(0, 1), labels = scales::label_percent(accuracy = 25)) +
  labs(title = "d  Target-aligned substitutability", x = "tolerance ε",
       y = "fraction of metrics substitutable", color = "oriented comparison") +
  theme_ms(base_size = 6.4, legend_position = "bottom")

fig4 <- cowplot::plot_grid(
  cowplot::plot_grid(p4a, p4b, ncol = 2),
  cowplot::plot_grid(p4c, p4d, ncol = 2),
  ncol = 1, rel_heights = c(1, 1))
ms_plot_save(fig4, file.path(OUT_DIR, "Fig4_RQ3.pdf"), 14.5, 10.5)
ms_plot_save(fig4, file.path(OUT_DIR, "Fig4_RQ3.png"), 14.5, 10.5)

# Fig. 5 keeps support explicit because support is part of the estimand.
pareto_plot <- pareto |>
  filter(ever_pareto, is.finite(resolution_s), is.finite(n_days)) |>
  mutate(metric_class = factor(metric_class, levels = METRIC_CLASSES),
         facet_row = paste(support_id, placement, sep = " · "))
p5a <- ggplot(pareto_plot, aes(resolution_s, n_days, color = metric_class, size = pareto_persistence)) +
  geom_point(alpha = .78) + facet_grid(facet_row ~ optical, scales = "free", space = "free") +
  scale_color_ms_metric() + scale_size_continuous(range = c(.45, 3.0), limits = c(0, 1), name = "Pareto persistence") +
  scale_x_reverse(breaks = c(1800, 900, 300, 60, 30, 20, 10),
                  labels = c("30 m", "15 m", "5 m", "1 m", "30 s", "20 s", "10 s")) +
  scale_y_continuous(breaks = 1:6, labels = paste0(1:6, " d")) +
  labs(title = "a  Pareto persistence within support × placement × optical facets",
       x = "temporal resolution (low → high burden)", y = "monitoring duration") +
  theme_ms(base_size = 6.3, legend_position = "bottom") + theme(axis.text.x = element_text(angle = 42, hjust = 1))

frequency_plot <- frequency |>
  mutate(mean_pareto_persistence = pmax(0, pmin(1, mean_pareto_persistence)),
         facet_row = paste(support_id, placement, sep = " · "))
p5b <- ggplot(frequency_plot, aes(resolution_s, n_days, fill = mean_pareto_persistence)) +
  geom_tile(color = "white", linewidth = .14) + facet_grid(facet_row ~ optical, scales = "free", space = "free") +
  scale_fill_ms_sequential(name = "mean Pareto\npersistence", limits = c(0, 1), labels = scales::label_percent(accuracy = 25)) +
  scale_x_reverse(breaks = c(1800, 900, 300, 60, 30, 20, 10),
                  labels = c("30 m", "15 m", "5 m", "1 m", "30 s", "20 s", "10 s")) +
  scale_y_continuous(breaks = 1:6, labels = paste0(1:6, " d")) +
  labs(title = "b  Mean Pareto persistence across metrics",
       x = "temporal resolution (low → high burden)", y = "monitoring duration") +
  theme_ms(base_size = 6.3, legend_position = "right") + theme(axis.text.x = element_text(angle = 42, hjust = 1))
fig5 <- cowplot::plot_grid(p5a, p5b, ncol = 2, rel_widths = c(1.05, .95))
ms_plot_save(fig5, file.path(OUT_DIR, "Fig5_RQ3.pdf"), 15.5, 10.0)
ms_plot_save(fig5, file.path(OUT_DIR, "Fig5_RQ3.png"), 15.5, 10.0)

readr::write_csv(observed_display, file.path("results", "rq3", "fig4_observed_stability.csv"), na = "")
readr::write_csv(pareto_plot |> mutate(metric_class = as.character(metric_class)),
                 file.path("results", "rq3", "fig5_pareto_plot.csv"), na = "")
ms_plot_write_manifest(file.path(OUT_DIR, "figure_artifact_manifest.csv"),
  tibble(
    figure = c("Fig4_RQ3", "Fig5_RQ3"),
    input_artifact = c("rq3_observed_stability+sufficiency+coverage", "rq3_pareto_frontiers+frequency"),
    core_artifact_version = CORE_VERSION, rq1_analysis_version = RQ1_VERSION,
    rq2_analysis_version = NA_character_, rq3_analysis_version = RQ3_VERSION
  ))
message("RQ3 v5 figures complete")
