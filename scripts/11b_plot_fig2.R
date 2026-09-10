.ms_file <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
if (length(.ms_file)) {
  .ms_script <- normalizePath(sub("^--file=", "", .ms_file[[1]]), winslash = "/", mustWork = TRUE)
  .ms_candidates <- unique(c(file.path(dirname(.ms_script), ".."), getwd()))
  .ms_ok <- vapply(
    .ms_candidates,
    function(x) file.exists(file.path(x, "scripts", "utils", "figure_style.R")),
    logical(1)
  )
  if (!any(.ms_ok)) stop("Could not resolve measurement-sufficiency repository root", call. = FALSE)
  setwd(normalizePath(.ms_candidates[which(.ms_ok)[1]], winslash = "/", mustWork = TRUE))
}
rm(.ms_file)
if (exists(".ms_script")) rm(.ms_script)
if (exists(".ms_candidates")) rm(.ms_candidates)
if (exists(".ms_ok")) rm(.ms_ok)

suppressPackageStartupMessages({
  library(tidyverse)
  library(cowplot)
})
source("scripts/utils/analysis_design.R")
source("scripts/utils/core_artifacts.R")
source("scripts/utils/figure_style.R")
source("scripts/utils/figure_atlas.R")
source("scripts/utils/plot_contracts.R")
source("scripts/utils/artifact_validation.R")

INFERENCE_RDS <- file.path("results", "rq1", "inference", "rq1_inferential_preservation.rds")
RQ1_SUMMARY_CSV <- file.path("results", "rq1", "rq1_pairwise_summary.csv")
OUT_DIR <- file.path("results", "rq1", "figures")
FIG2_WIDTH_IN <- 7.6
FIG2_HEIGHT_IN <- 7.6
ms_plot_require_files(c(INFERENCE_RDS, RQ1_SUMMARY_CSV), "Fig. 2 plotting inputs")
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

a <- readRDS(INFERENCE_RDS)
if (!identical(a$artifact_type, "rq1_inferential_preservation")) {
  stop("Unexpected inferential-preservation artifact type", call. = FALSE)
}
ms_assert_version(a, "analysis_design_id", ms_analysis_design_id())
CORE_VERSION <- ms_plot_assert_core(a$core_artifact_version)
RQ1_VERSION <- ms_plot_one_version(a$rq1_analysis_version, "rq1_analysis_version")
INFERENCE_VERSION <- ms_plot_one_version(a$rq1_inference_version, "rq1_inference_version")
ms_plot_assert_prefix(RQ1_VERSION, "rq1_v5_", "rq1_analysis_version")
ms_plot_assert_prefix(INFERENCE_VERSION, "rq1_inference_v2_anchor8__", "rq1_inference_version")

rq1_summary <- readr::read_csv(RQ1_SUMMARY_CSV, show_col_types = FALSE, progress = FALSE)
ms_plot_assert_core(rq1_summary$core_artifact_version, CORE_VERSION)
if (!identical(ms_plot_one_version(rq1_summary$rq1_analysis_version, "rq1 summary version"), RQ1_VERSION)) {
  stop("Fig. 2 RQ1 summary version mismatch", call. = FALSE)
}

reference <- tibble::as_tibble(a$reference_summary)
contrast <- tibble::as_tibble(a$contrast_summary)
anchor_map <- tibble::as_tibble(a$anchor_map)
ms_plot_require_columns(
  reference,
  c("metric", "metric_class", "outcome", "reference_association_strength", "status"),
  "reference association summary"
)
ms_plot_require_columns(
  contrast,
  c("candidate_config", "contrast_label", "contrast_order", "dimension", "metric", "metric_class",
    "outcome", "inference_deviation", "rq1_distortion_A", "status"),
  "inferential preservation summary"
)
if (nrow(anchor_map) != 8L || n_distinct(anchor_map$candidate_config) != 8L) {
  stop("Fig. 2 requires exactly eight frozen RQ1 anchor contrasts", call. = FALSE)
}

OUTCOME_LEVELS <- c("sleep_quality", "awakenings", "awake_duration")
OUTCOME_LABELS <- c(
  sleep_quality = "Sleep quality",
  awakenings = "Awakenings",
  awake_duration = "Awake duration"
)
metric_order <- ms_metric_order(rq1_summary)

reference_plot <- reference |>
  mutate(
    outcome = factor(outcome, levels = OUTCOME_LEVELS, labels = unname(OUTCOME_LABELS[OUTCOME_LEVELS])),
    reference_association_strength = if_else(
      is.finite(reference_association_strength), reference_association_strength, NA_real_
    )
  ) |>
  ms_add_metric_order(metric_order)

contrast_plot <- contrast |>
  mutate(
    outcome = factor(outcome, levels = OUTCOME_LEVELS, labels = unname(OUTCOME_LABELS[OUTCOME_LEVELS])),
    contrast_label = factor(contrast_label, levels = anchor_map$contrast_label[order(anchor_map$contrast_order)]),
    metric_class = factor(metric_class, levels = MS_METRIC_CLASSES)
  ) |>
  filter(is.finite(inference_deviation), is.finite(rq1_distortion_A))
if (!nrow(contrast_plot)) stop("No finite inferential-preservation results for Fig. 2", call. = FALSE)

# -----------------------------------------------------------------------------
# a. Canonical high-information reference association landscape
# -----------------------------------------------------------------------------
p2a <- ggplot(reference_plot, aes(outcome, metric, fill = reference_association_strength)) +
  geom_tile(color = "white", linewidth = .12) +
  facet_grid(metric_class ~ ., scales = "free_y", space = "free_y", switch = "y") +
  scale_fill_ms_sequential(
    trans = scales::transform_asinh(), na.value = "#ECEFF0",
    name = "Reference association\nstrength"
  ) +
  labs(
    title = "a  Reference association landscape",
    subtitle = "Eye · MEDI · 10 s; bootstrap-standardized within-participant association magnitude",
    x = NULL, y = NULL
  ) +
  ms_atlas_theme(base_size = 5.9, x_angle = 28) +
  theme(
    axis.text.y = element_text(size = 4.25),
    axis.text.x = element_text(size = 5.0, angle = 28, hjust = 1),
    strip.text.y.left = element_text(size = 4.65),
    legend.position = "bottom",
    legend.title = element_text(size = 4.6), legend.text = element_text(size = 4.3),
    plot.title = element_text(size = 6.8),
    plot.subtitle = element_text(size = 4.4, color = "#666A6D")
  )

# -----------------------------------------------------------------------------
# b. Distribution of downstream inferential degradation across 52 metrics
# -----------------------------------------------------------------------------
contrast_summary <- contrast_plot |>
  group_by(outcome, contrast_label, contrast_order, dimension) |>
  summarise(
    n_metrics = n_distinct(metric),
    deviation_q25 = quantile(inference_deviation, .25, na.rm = TRUE, names = FALSE, type = 8),
    deviation_median = median(inference_deviation, na.rm = TRUE),
    deviation_q75 = quantile(inference_deviation, .75, na.rm = TRUE, names = FALSE, type = 8),
    .groups = "drop"
  )

p2b <- ggplot(contrast_plot, aes(contrast_label, inference_deviation, color = metric_class)) +
  geom_point(
    position = position_jitter(width = .16, height = 0, seed = 211),
    size = .52, alpha = .18
  ) +
  geom_linerange(
    data = contrast_summary,
    aes(ymin = deviation_q25, ymax = deviation_q75),
    inherit.aes = FALSE, linewidth = .72, color = "#3D4347", alpha = .72
  ) +
  geom_point(
    data = contrast_summary,
    aes(y = deviation_median),
    inherit.aes = FALSE, shape = 18, size = 1.65, color = "#202426"
  ) +
  facet_wrap(~outcome, nrow = 1) +
  scale_color_ms_metric(guide = "none") +
  scale_y_continuous(trans = scales::transform_asinh(), breaks = scales::breaks_extended(n = 4)) +
  labs(
    title = "b  Inferential degradation by measurement contrast",
    subtitle = "Faint points = metrics; black diamonds/IQRs = cross-metric median and interquartile range",
    x = NULL,
    y = "Inferential deviation\n(reference-bootstrap uncertainty units)"
  ) +
  theme_ms_axes(base_size = 5.9, legend_position = "none", plot_title_size = 6.8) +
  theme(
    panel.grid.major.x = element_blank(),
    axis.text.x = element_text(size = 4.0, angle = 47, hjust = 1),
    axis.text.y = element_text(size = 4.5),
    strip.text = element_text(size = 5.0),
    plot.subtitle = element_text(size = 4.35, color = "#666A6D"),
    plot.margin = margin(2, 3, 2, 3)
  )

# -----------------------------------------------------------------------------
# c. Frozen RQ1 representation distortion -> downstream inferential deviation
# -----------------------------------------------------------------------------
link_bins <- contrast_plot |>
  group_by(outcome) |>
  mutate(distortion_bin = ntile(rq1_distortion_A, 5L)) |>
  group_by(outcome, distortion_bin) |>
  summarise(
    rq1_distortion_A = median(rq1_distortion_A, na.rm = TRUE),
    inference_deviation = median(inference_deviation, na.rm = TRUE),
    n = n(), .groups = "drop"
  )

link_assoc <- contrast_plot |>
  group_by(outcome) |>
  summarise(
    n = n(),
    rho = suppressWarnings(cor(rq1_distortion_A, inference_deviation,
                               method = "spearman", use = "complete.obs")),
    .groups = "drop"
  ) |>
  mutate(label = if_else(is.finite(rho), sprintf("Spearman r[s] = %.2f", rho), "Spearman r[s] = NA"))

p2c <- ggplot(contrast_plot, aes(rq1_distortion_A, inference_deviation, color = metric_class)) +
  geom_point(size = .54, alpha = .18) +
  geom_line(
    data = link_bins,
    aes(rq1_distortion_A, inference_deviation, group = outcome),
    inherit.aes = FALSE, linewidth = .78, color = "#202426"
  ) +
  geom_point(
    data = link_bins,
    aes(rq1_distortion_A, inference_deviation),
    inherit.aes = FALSE, shape = 18, size = 1.38, color = "#202426"
  ) +
  geom_text(
    data = link_assoc,
    aes(x = -Inf, y = Inf, label = label),
    inherit.aes = FALSE, parse = TRUE, hjust = -.04, vjust = 1.12,
    size = 2.05, color = "#303437"
  ) +
  facet_wrap(~outcome, nrow = 1) +
  scale_color_ms_metric(guide = "none") +
  scale_x_continuous(trans = scales::transform_asinh(), breaks = scales::breaks_extended(n = 4)) +
  scale_y_continuous(trans = scales::transform_asinh(), breaks = scales::breaks_extended(n = 4)) +
  labs(
    title = "c  Representation distortion propagates to downstream inference",
    subtitle = "x-axis uses the frozen RQ1 distortion estimate; black line connects distortion-quintile medians",
    x = "Frozen RQ1 representation distortion, A",
    y = "Inferential deviation\n(reference-bootstrap uncertainty units)"
  ) +
  theme_ms_axes(base_size = 5.9, legend_position = "none", plot_title_size = 6.8) +
  theme(
    axis.text = element_text(size = 4.5),
    strip.text = element_text(size = 5.0),
    plot.subtitle = element_text(size = 4.35, color = "#666A6D"),
    plot.margin = margin(2, 3, 2, 3)
  )

metric_legend <- ms_metric_legend(text_size = 5.35, point_size = 1.55, key_width_mm = 3.5)
right <- cowplot::plot_grid(
  p2b, p2c, ncol = 1, rel_heights = c(.93, 1.07),
  align = "v", axis = "lr", greedy = TRUE
)
body <- cowplot::plot_grid(
  p2a, right, ncol = 2, rel_widths = c(.40, .60),
  align = "hv", axis = "tblr", greedy = TRUE
)
fig2 <- cowplot::plot_grid(
  metric_legend, body, ncol = 1, rel_heights = c(.035, 1),
  align = "v", axis = "l", greedy = TRUE
)

readr::write_csv(
  reference_plot |>
    mutate(metric = as.character(metric), metric_class = as.character(metric_class), outcome = as.character(outcome)),
  file.path("results", "rq1", "inference", "fig2_reference_association_landscape.csv"), na = ""
)
readr::write_csv(
  contrast_summary |> mutate(outcome = as.character(outcome), contrast_label = as.character(contrast_label)),
  file.path("results", "rq1", "inference", "fig2_inferential_degradation.csv"), na = ""
)
readr::write_csv(
  contrast_plot |>
    mutate(metric_class = as.character(metric_class), outcome = as.character(outcome),
           contrast_label = as.character(contrast_label)),
  file.path("results", "rq1", "inference", "fig2_distortion_inference_link.csv"), na = ""
)

ms_plot_save(fig2, file.path(OUT_DIR, "Fig2_RQ1_inferential_preservation.png"), FIG2_WIDTH_IN, FIG2_HEIGHT_IN)
ms_plot_write_manifest(
  file.path(OUT_DIR, "figure_artifact_manifest.csv"),
  tibble(
    figure = c("Fig1_RQ1", "Fig2_RQ1_inferential_preservation"),
    input_artifact = c(
      "rq1_pairwise_change_long + rq1_pairwise_summary + rq1_local_transition_summary",
      "rq1_inferential_preservation + rq1_pairwise_summary"
    ),
    core_artifact_version = CORE_VERSION,
    rq1_analysis_version = RQ1_VERSION,
    rq1_inference_version = c(NA_character_, INFERENCE_VERSION),
    rq2_analysis_version = NA_character_,
    rq3_analysis_version = NA_character_
  )
)
message("Fig. 2 complete: reference association landscape, anchor-contrast inferential degradation, and RQ1 distortion-to-inference propagation.")
