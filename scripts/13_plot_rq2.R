suppressPackageStartupMessages({
  library(tidyverse)
  library(cowplot)
})
source("scripts/utils/figure_style.R")
source("scripts/utils/figure_atlas.R")
source("scripts/utils/rq1_pairwise_artifacts.R")
source("scripts/utils/plot_contracts.R")

# RQ2 figures consume conditional geometry, local model performance, and gamma
# summaries from scripts/12_rq2_analysis.R. Context-conditioned legacy atlases
# are deliberately not read here.
RQ1_SUMMARY_CSV <- file.path("results", "rq1", "rq1_pairwise_summary.csv")
CONDITION_RDS <- file.path("results", "rq2", "rq2_condition_long.rds")
COND_GEOM_CSV <- file.path("results", "rq2", "rq2_conditional_geometry.csv")
MODEL_PERF_CSV <- file.path("results", "rq2", "rq2_model_performance.csv")
MODEL_MANIFEST_CSV <- file.path("results", "rq2", "rq2_model_artifact_manifest.csv")
GAMMA_RDS <- file.path("results", "rq2", "rq2_gamma_long.rds")
GAMMA_SUMMARY_CSV <- file.path("results", "rq2", "rq2_gamma_summary.csv")
SCOPE_CSV <- file.path("results", "rq2", "rq2_interaction_scope.csv")
OUT_DIR <- file.path("results", "rq2", "figures")
ms_plot_require_files(c(RQ1_SUMMARY_CSV, CONDITION_RDS, COND_GEOM_CSV, MODEL_PERF_CSV,
                        MODEL_MANIFEST_CSV, GAMMA_RDS, GAMMA_SUMMARY_CSV, SCOPE_CSV),
                      "RQ2 plotting inputs")
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

METRIC_CLASSES <- MS_METRIC_CLASSES
rq1_summary <- readr::read_csv(RQ1_SUMMARY_CSV, show_col_types = FALSE, progress = FALSE)
condition <- readRDS(CONDITION_RDS)
conditional <- readr::read_csv(COND_GEOM_CSV, show_col_types = FALSE, progress = FALSE)
performance <- readr::read_csv(MODEL_PERF_CSV, show_col_types = FALSE, progress = FALSE)
model_manifest <- readr::read_csv(MODEL_MANIFEST_CSV, show_col_types = FALSE, progress = FALSE)
gamma_long <- readRDS(GAMMA_RDS)
gamma_summary <- readr::read_csv(GAMMA_SUMMARY_CSV, show_col_types = FALSE, progress = FALSE)
scope <- readr::read_csv(SCOPE_CSV, show_col_types = FALSE, progress = FALSE)

ms_plot_require_columns(rq1_summary, c("metric", "metric_class", "dimension", "A_mean_absolute"), "rq1_pairwise_summary.csv")
ms_plot_require_columns(
  conditional,
  c("core_artifact_version", "rq1_analysis_version", "rq2_analysis_version", "dimension",
    "comparison_pair_id", "config_a_label", "config_b_label", "metric", "metric_class",
    "state_bin_label", "A_conditional", "B_conditional"),
  "rq2_conditional_geometry.csv"
)
ms_plot_require_columns(
  performance,
  c("dimension", "comparison_pair_id", "metric", "outcome", "model_family", "validation_scheme",
    "rmse", "mae", "r2"),
  "rq2_model_performance.csv"
)
ms_plot_require_columns(
  model_manifest,
  c("core_artifact_version", "rq1_analysis_version", "rq2_analysis_version"),
  "rq2_model_artifact_manifest.csv"
)
ms_plot_require_columns(
  gamma_summary,
  c("dimension_a", "dimension_b", "comparison_lattice", "transition", "metric", "metric_class", "R", "Q"),
  "rq2_gamma_summary.csv"
)
ms_plot_require_columns(scope, c("dimension_pair", "primary_scope"), "rq2_interaction_scope.csv")

condition_core <- if (is.list(condition)) condition$core_artifact_version else NULL
condition_rq1 <- if (is.list(condition)) condition$rq1_analysis_version else NULL
condition_rq2 <- if (is.list(condition)) condition$rq2_analysis_version else NULL
RQ1_VERSION <- ms_plot_one_version(c(condition_rq1, conditional$rq1_analysis_version, model_manifest$rq1_analysis_version, gamma_long$rq1_analysis_version), "rq1_analysis_version")
RQ2_VERSION <- ms_plot_one_version(c(condition_rq2, conditional$rq2_analysis_version, model_manifest$rq2_analysis_version, gamma_long$rq2_analysis_version), "rq2_analysis_version")
CORE_VERSION <- ms_plot_assert_core(c(condition_core, conditional$core_artifact_version, model_manifest$core_artifact_version, gamma_long$core_artifact_version))
ms_plot_assert_prefix(RQ1_VERSION, "rq1_v5_", "rq1_analysis_version")
ms_plot_assert_prefix(RQ2_VERSION, "rq2_v4_", "rq2_analysis_version")

metric_order <- ms_metric_order(rq1_summary)
conditional <- conditional |>
  mutate(
    metric_class = factor(metric_class, levels = METRIC_CLASSES),
    dimension = factor(dimension, levels = c("placement", "optical", "temporal", "duration")),
    pair_label = paste(config_a_label, "→", config_b_label),
    state_bin_label = factor(state_bin_label, levels = c("Low", "Middle", "High")),
    direction_ratio = ms_direction_ratio(B_conditional, A_conditional)
  ) |>
  mutate(dimension = as.character(dimension)) |>
  ms_add_metric_order(metric_order)

# Fig. 2: exposure-process conditioning changes the observed pairwise geometry.
# The x axis is a transition-local state bin; bins are frozen upstream and are
# not recomputed or ranked by metric in this plot.
p2 <- ggplot(conditional, aes(interaction(pair_label, state_bin_label, sep = "\n"), metric)) +
  geom_point(aes(size = A_conditional, fill = direction_ratio), shape = 21, color = "#3B3B3B", stroke = .14, alpha = .92) +
  facet_grid(metric_class ~ dimension, scales = "free", space = "free", switch = "y") +
  ms_direction_scale(name = "B / A") +
  ms_magnitude_size_scale(name = "A = conditional mean |z|", range = c(.25, 3.0)) +
  labs(title = "Fig. 2  Conditional RQ2 geometry", x = "oriented pair × transition-local exposure state", y = NULL) +
  ms_atlas_theme(base_size = 6.0, x_angle = 52) +
  theme(axis.text.x = element_text(size = 4.5))
ms_plot_save(p2, file.path(OUT_DIR, "Fig2_RQ2.pdf"), 16, 11.5)
ms_plot_save(p2, file.path(OUT_DIR, "Fig2_RQ2.png"), 16, 11.5)

conditional_export <- conditional |>
  mutate(metric = as.character(metric), metric_class = as.character(metric_class), dimension = as.character(dimension))
readr::write_csv(conditional_export, file.path("results", "rq2", "fig2_conditional_geometry_atlas.csv"), na = "")

# Fig. 3: interaction gamma is shown as signed R and absolute Q. R and Q are
# the frozen gamma summaries; no cross-dimensional model is fitted here.
gamma_plot <- gamma_summary |>
  mutate(
    dimension_pair = paste(dimension_a, "×", dimension_b),
    metric_class = factor(metric_class, levels = METRIC_CLASSES),
    dimension = dimension_a,
    transition = factor(transition, levels = unique(transition)),
    Q = abs(Q)
  ) |>
  mutate(dimension = as.character(dimension)) |>
  ms_add_metric_order(metric_order)
gamma_limit <- max(abs(c(gamma_plot$R, gamma_plot$Q)), na.rm = TRUE)
if (!is.finite(gamma_limit) || gamma_limit <= 0) gamma_limit <- 1
p3 <- ggplot(gamma_plot, aes(transition, R, color = metric_class)) +
  geom_hline(yintercept = 0, linewidth = .28, color = "#8A8A8A") +
  geom_segment(aes(x = transition, xend = transition, y = 0, yend = R), alpha = .34, linewidth = .40) +
  geom_point(aes(size = Q), alpha = .90) +
  facet_grid(metric_class ~ dimension_pair, scales = "free_x", space = "free", switch = "y") +
  scale_color_ms_metric() +
  scale_size_continuous(range = c(.35, 2.8), name = "Q = mean |gamma|") +
  scale_y_continuous(limits = c(-gamma_limit * 1.05, gamma_limit * 1.05), breaks = scales::breaks_extended(n = 5)) +
  labs(title = "Fig. 3  Cross-dimension interaction geometry", x = "local interaction transition", y = "R = mean gamma") +
  ms_atlas_theme(base_size = 6.2, x_angle = 48)
ms_plot_save(p3, file.path(OUT_DIR, "Fig3_RQ2.pdf"), 14.5, 10.5)
ms_plot_save(p3, file.path(OUT_DIR, "Fig3_RQ2.png"), 14.5, 10.5)
readr::write_csv(gamma_plot |> mutate(metric = as.character(metric), metric_class = as.character(metric_class)),
                 file.path("results", "rq2", "fig3_gamma_atlas.csv"), na = "")

# Supplementary model-performance view. A structural smoke run may produce an
# empty performance table; in that case emit a clear placeholder rather than
# silently reading an old model artifact.
if (nrow(performance)) {
  perf_plot <- performance |>
    filter(is.finite(rmse) | is.finite(mae) | is.finite(r2)) |>
    group_by(dimension, model_family, outcome, validation_scheme) |>
    summarise(rmse = median(rmse, na.rm = TRUE), mae = median(mae, na.rm = TRUE), r2 = median(r2, na.rm = TRUE), .groups = "drop") |>
    pivot_longer(c(rmse, mae, r2), names_to = "measure", values_to = "value") |>
    mutate(dimension = factor(dimension, levels = c("placement", "optical", "temporal", "duration")))
  p_perf <- ggplot(perf_plot, aes(interaction(model_family, validation_scheme, sep = "\n"), outcome, fill = value)) +
    geom_tile(color = "white", linewidth = .12) +
    facet_grid(measure ~ dimension, scales = "free", space = "free", switch = "y") +
    scale_fill_ms_sequential(name = "median value") +
    labs(title = "RQ2 model validation diagnostics", x = "model family × validation scheme", y = NULL) +
    ms_atlas_theme(base_size = 6.1, x_angle = 48)
} else {
  p_perf <- ggplot() + theme_void() + annotate("text", x = 0, y = 0, label = "No model-performance rows; RQ2_RUN_MODELS=0 or no eligible tasks.")
}
ms_plot_save(p_perf, file.path(OUT_DIR, "FigS_RQ2_model_performance.pdf"), 13, 8.5)
ms_plot_save(p_perf, file.path(OUT_DIR, "FigS_RQ2_model_performance.png"), 13, 8.5)

ms_plot_write_manifest(
  file.path(OUT_DIR, "figure_artifact_manifest.csv"),
  tibble(
    figure = c("Fig2_RQ2", "Fig3_RQ2", "FigS_RQ2_model_performance"),
    input_artifact = c("rq2_conditional_geometry", "rq2_gamma_summary", "rq2_model_performance"),
    core_artifact_version = CORE_VERSION, rq1_analysis_version = RQ1_VERSION,
    rq2_analysis_version = RQ2_VERSION, rq3_analysis_version = NA_character_
  )
)
message("RQ2 figures complete: conditional geometry, interaction gamma, and model diagnostics.")
