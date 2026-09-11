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
source("scripts/utils/rq1_inference_contract.R")

contract <- rq1_inference_contract()
INFERENCE_RDS <- file.path("results", "rq1", "inference", contract$artifact_filename)
RQ1_SUMMARY_CSV <- file.path("results", "rq1", "rq1_pairwise_summary.csv")
OUT_DIR <- file.path("results", "rq1", "figures")
FIG2_WIDTH_IN <- 8.2
FIG2_HEIGHT_IN <- 6.8
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
ms_plot_assert_prefix(INFERENCE_VERSION, "rq1_inference_v3_domains_anchor8__", "rq1_inference_version")

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
  c("metric", "metric_class", "outcome", "outcome_domain", "reference_association_strength", "status"),
  "reference association summary"
)
ms_plot_require_columns(
  contrast,
  c("candidate_config", "contrast_label", "contrast_order", "dimension", "metric", "metric_class",
    "outcome", "outcome_domain", "inference_deviation", "rq1_distortion_A", "status"),
  "inferential preservation summary"
)
if (nrow(anchor_map) != contract$anchor_count || n_distinct(anchor_map$candidate_config) != contract$anchor_count) {
  stop("Fig. 2 anchor map does not match the frozen inferential-preservation contract", call. = FALSE)
}
if (n_distinct(reference$metric) != contract$daily_metric_count ||
    !setequal(unique(reference$outcome), contract$outcomes)) {
  stop("Fig. 2 reference association landscape does not match the frozen daily metric/outcome contract", call. = FALSE)
}

OUTCOME_LEVELS <- contract$outcomes
OUTCOME_LABELS <- contract$outcome_label
DOMAIN_LEVELS <- c("Sleep", "Alertness", "Affect")
metric_order <- ms_metric_order(rq1_summary)

reference_plot <- reference |>
  mutate(
    outcome_domain = factor(outcome_domain, levels = DOMAIN_LEVELS),
    outcome = factor(outcome, levels = OUTCOME_LEVELS, labels = unname(OUTCOME_LABELS[OUTCOME_LEVELS])),
    reference_association_strength = if_else(
      is.finite(reference_association_strength), reference_association_strength, NA_real_
    )
  ) |>
  ms_add_metric_order(metric_order)

contrast_plot <- contrast |>
  mutate(
    outcome_domain = factor(outcome_domain, levels = DOMAIN_LEVELS),
    outcome = factor(outcome, levels = OUTCOME_LEVELS, labels = unname(OUTCOME_LABELS[OUTCOME_LEVELS])),
    contrast_label = factor(contrast_label, levels = anchor_map$contrast_label[order(anchor_map$contrast_order)]),
    metric_class = factor(metric_class, levels = MS_METRIC_CLASSES)
  ) |>
  filter(is.finite(inference_deviation), is.finite(rq1_distortion_A))
if (!nrow(contrast_plot)) stop("No finite inferential-preservation results for Fig. 2", call. = FALSE)

# -----------------------------------------------------------------------------
# a. Canonical high-information reference association landscape across domains
# -----------------------------------------------------------------------------
# Display labels only; retain full metric IDs and frozen row order in audit CSVs.
metric_labels <- function(x) {
  x <- gsub("_", " ", as.character(x), fixed = TRUE)
  x <- gsub("total duration pulses above ", "Total pulse dur. > ", x, fixed = TRUE)
  x <- gsub("mean duration pulses above ", "Mean pulse dur. > ", x, fixed = TRUE)
  x <- gsub("mean level pulses above ", "Pulse level > ", x, fixed = TRUE)
  x <- gsub("mean midpoint pulses above ", "Pulse midpoint > ", x, fixed = TRUE)
  x <- gsub("mean onset pulses above ", "Pulse onset > ", x, fixed = TRUE)
  x <- gsub("mean offset pulses above ", "Pulse offset > ", x, fixed = TRUE)
  x <- gsub("frequency crossing ", "Crossing freq. ", x, fixed = TRUE)
  x <- gsub("above ", "> ", x, fixed = TRUE)
  x
}
stopifnot(!anyDuplicated(metric_labels(unique(reference_plot$metric))))
p2a <- ggplot(reference_plot, aes(outcome, metric, fill = reference_association_strength)) +
  geom_tile(color = "white", linewidth = .12) +
  facet_grid(metric_class ~ outcome_domain, scales = "free", space = "free", switch = "y",
    labeller = labeller(metric_class = c(duration = "Duration", `exposure history` = "History",
      level = "Level", spectrum = "Spec.", `temporal dynamics` = "Dynamics", timing = "Timing"))) +
  scale_fill_ms_sequential(
    trans = scales::transform_asinh(), na.value = "#ECEFF0",
    name = "Reference association strength", breaks = 0:5,
    limits = c(0, max(reference_plot$reference_association_strength, na.rm = TRUE))
  ) +
  scale_y_discrete(labels = metric_labels) +
  labs(
    title = "a  Reference association landscape",
    subtitle = "Eye / MEDI / 10 s · bootstrap-uncertainty units",
    x = NULL, y = NULL
  ) +
  ms_atlas_theme(base_size = 5.9, x_angle = 28) +
  theme(
    axis.text.y = element_text(size = 4.5),
    axis.text.x = element_text(size = 4.7, angle = 50, hjust = 1),
    strip.text.y.left = element_text(size = 4.55, angle = 90),
    strip.clip = "off",
    strip.text.x = element_text(size = 4.9, face = "bold"),
    legend.position = "bottom",
    legend.title = element_text(size = 4.6), legend.text = element_text(size = 4.3),
    plot.title = element_text(size = 6.8),
    plot.subtitle = element_text(size = 4.4, color = "#666A6D"),
    legend.key.width = grid::unit(6, "mm"),
    legend.key.height = grid::unit(2, "mm"),
    legend.direction = "horizontal",
    legend.title.position = "top"
  )

# -----------------------------------------------------------------------------
# b. Domain-level degradation distributions across metric-outcome combinations
# -----------------------------------------------------------------------------
contrast_summary <- contrast_plot |>
  group_by(outcome_domain, contrast_label, contrast_order, dimension) |>
  summarise(
    n_outcomes = n_distinct(outcome),
    n_metric_outcomes = n(),
    deviation_q25 = quantile(inference_deviation, .25, na.rm = TRUE, names = FALSE, type = 8),
    deviation_median = median(inference_deviation, na.rm = TRUE),
    deviation_q75 = quantile(inference_deviation, .75, na.rm = TRUE, names = FALSE, type = 8),
    .groups = "drop"
  )

# Both consequence panels use the same continuous deviation scale. This is a
# display transform, not an inferential threshold or a change to the estimates.
deviation_limits <- c(0, max(contrast_plot$inference_deviation) * 1.04)
deviation_breaks <- c(0, .1, .3, 1, 3, 5)
contrast_short <- setNames(c("Chest", "Wrist", "LIGHT", "20 s", "30 s", "40 s", "60 s", "120 s"),
                          anchor_map$contrast_label[order(anchor_map$contrast_order)])
p2b <- ggplot(contrast_plot, aes(inference_deviation, contrast_label, color = metric_class)) +
  geom_hline(yintercept = c(5.5, 6.5), linewidth = .23, colour = "#DEE3E5") +
  geom_point(
    position = position_jitter(width = 0, height = .15, seed = 211),
    size = .65, alpha = .32
  ) +
  geom_linerange(
    data = contrast_summary,
    aes(y = contrast_label, xmin = deviation_q25, xmax = deviation_q75),
    orientation = "y", inherit.aes = FALSE, linewidth = .65, color = "#3D4347", alpha = .85
  ) +
  geom_point(
    data = contrast_summary,
    aes(y = contrast_label, x = deviation_median),
    inherit.aes = FALSE, shape = 18, size = 1.65, color = "#202426"
  ) +
  facet_wrap(~outcome_domain, nrow = 1) +
  scale_color_ms_metric(guide = "none") +
  scale_y_discrete(limits = rev(levels(contrast_plot$contrast_label)), labels = contrast_short) +
  scale_x_continuous(trans = scales::pseudo_log_trans(sigma = .08),
                     limits = deviation_limits, breaks = deviation_breaks) +
  labs(
    title = "b  Which configurations shift inference?",
    subtitle = paste0("Points: metric–outcome pairs · diamonds / bars: domain median / IQR\n",
                      "All vs eye / MEDI / 10 s · pseudo-log x · LIGHT excludes MDER/nvRD"),
    x = "Inferential deviation (reference-bootstrap uncertainty units)", y = NULL
  ) +
  theme_ms_axes(base_size = 5.9, legend_position = "none", plot_title_size = 6.8) +
  theme(
    panel.grid.major.y = element_blank(),
    axis.text.x = element_text(size = 4.3),
    axis.text.y = element_text(size = 4.8),
    axis.title.x = element_text(size = 4.7),
    strip.text = element_text(size = 5.0),
    plot.subtitle = element_text(size = 4.35, color = "#666A6D"),
    plot.margin = margin(2, 3, 2, 3)
  )

# -----------------------------------------------------------------------------
# c. Frozen RQ1 representation distortion -> inferential deviation by domain
# -----------------------------------------------------------------------------
link_bins <- contrast_plot |>
  group_by(outcome_domain) |>
  mutate(distortion_bin = ntile(rq1_distortion_A, 5L)) |>
  group_by(outcome_domain, distortion_bin) |>
  summarise(
    rq1_distortion_A = median(rq1_distortion_A, na.rm = TRUE),
    inference_deviation = median(inference_deviation, na.rm = TRUE),
    n = n(), .groups = "drop"
  )

link_assoc <- contrast_plot |>
  group_by(outcome_domain) |>
  summarise(
    n = n(),
    rho = suppressWarnings(cor(rq1_distortion_A, inference_deviation,
                               method = "spearman", use = "complete.obs")),
    .groups = "drop"
  ) |>
  mutate(label = if_else(is.finite(rho), sprintf("Spearman rₛ = %.2f", rho), "Spearman rₛ = NA"))

p2c <- ggplot(contrast_plot, aes(rq1_distortion_A, inference_deviation, color = metric_class)) +
  geom_point(size = .70, alpha = .32) +
  geom_line(
    data = link_bins,
    aes(rq1_distortion_A, inference_deviation, group = outcome_domain),
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
    inherit.aes = FALSE, hjust = -.04, vjust = 1.12,
    size = 1.65, color = "#303437"
  ) +
  facet_wrap(~outcome_domain, nrow = 1) +
  scale_color_ms_metric(guide = "none") +
  scale_x_continuous(trans = scales::pseudo_log_trans(sigma = .03), breaks = c(0, .1, .5, 2)) +
  scale_y_continuous(trans = scales::pseudo_log_trans(sigma = .08),
                     limits = deviation_limits, breaks = deviation_breaks) +
  labs(
    title = "c  Distortion and inferential displacement",
    subtitle = "Frozen RQ1 A · black: quintile medians · axes expand near zero",
    x = "Frozen RQ1 representation distortion, A",
    y = "Inferential deviation"
  ) +
  theme_ms_axes(base_size = 5.9, legend_position = "none", plot_title_size = 6.8) +
  theme(
    axis.text = element_text(size = 4.5),
    strip.text = element_text(size = 5.0),
    axis.title = element_text(size = 4.7),
    plot.subtitle = element_text(size = 4.35, color = "#666A6D"),
    plot.margin = margin(2, 3, 2, 3)
  )

metric_legend <- ms_metric_legend(text_size = 5.35, point_size = 1.55, key_width_mm = 3.5)
right <- cowplot::plot_grid(
  p2b, p2c, ncol = 1, rel_heights = c(.50, .50),
  align = "v", axis = "lr", greedy = FALSE
)
body <- cowplot::plot_grid(
  p2a, right, ncol = 2, rel_widths = c(.44, .56)
)
fig2 <- cowplot::plot_grid(
  metric_legend, body, ncol = 1, rel_heights = c(.035, 1),
  align = "v", axis = "l", greedy = TRUE
)

readr::write_csv(
  reference_plot |>
    mutate(metric = as.character(metric), metric_class = as.character(metric_class),
           outcome = as.character(outcome), outcome_domain = as.character(outcome_domain)),
  file.path("results", "rq1", "inference", "fig2_reference_association_landscape.csv"), na = ""
)
readr::write_csv(
  contrast_summary |>
    mutate(outcome_domain = as.character(outcome_domain), contrast_label = as.character(contrast_label)),
  file.path("results", "rq1", "inference", "fig2_inferential_degradation.csv"), na = ""
)
readr::write_csv(
  contrast_plot |>
    mutate(metric_class = as.character(metric_class), outcome = as.character(outcome),
           outcome_domain = as.character(outcome_domain), contrast_label = as.character(contrast_label)),
  file.path("results", "rq1", "inference", "fig2_distortion_inference_link.csv"), na = ""
)

ms_plot_save(fig2, file.path(OUT_DIR, "Fig2_RQ1_inferential_preservation.png"), FIG2_WIDTH_IN, FIG2_HEIGHT_IN)
ms_plot_write_manifest(
  file.path(OUT_DIR, "figure_artifact_manifest.csv"),
  tibble(
    figure = c("Fig1_RQ1", "Fig2_RQ1_inferential_preservation"),
    input_artifact = c(
      "rq1_pairwise_change_long + rq1_pairwise_summary + rq1_local_transition_summary",
      "rq1_inferential_preservation_domains_anchor8 + rq1_pairwise_summary"
    ),
    core_artifact_version = CORE_VERSION,
    rq1_analysis_version = RQ1_VERSION,
    rq1_inference_version = c(NA_character_, INFERENCE_VERSION),
    rq2_analysis_version = NA_character_,
    rq3_analysis_version = NA_character_
  )
)
message("Fig. 2 complete: three-domain reference landscape, inferential degradation, and RQ1 distortion-to-inference propagation.")
