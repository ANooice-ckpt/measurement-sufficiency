# Recovery information-ablation atlas for the redesigned RQ2 Fig. 4.
# No model is fitted here. Existing held-out recovery estimates are summarized by
# scripts/12e_rq2_recovery_ablation.R, then visualized as stage-specific gains.
#
# Usage:
#   Rscript scripts/13c_plot_fig4_recovery.R
#   Rscript scripts/13c_plot_fig4_recovery.R <recovery_run_dir>
# or set RQ2_RECOVERY_RUN_DIR.

.ms_file <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
if (length(.ms_file)) {
  .ms_script <- normalizePath(sub("^--file=", "", .ms_file[[1]]), winslash = "/", mustWork = TRUE)
  .ms_root <- normalizePath(file.path(dirname(.ms_script), ".."), winslash = "/", mustWork = TRUE)
  setwd(.ms_root)
}
rm(.ms_file)
if (exists(".ms_script")) rm(.ms_script)
if (exists(".ms_root")) rm(.ms_root)

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
  library(ggplot2)
  library(cowplot)
})
source("scripts/utils/figure_style.R")
source("scripts/12e_rq2_recovery_ablation.R")

PAIR_ORDER <- c(
  "chest_vs_eye", "wrist_vs_eye", "LIGHT_vs_MEDI",
  "20s_vs_10s", "30s_vs_10s", "40s_vs_10s", "60s_vs_10s", "120s_vs_10s"
)
PAIR_LABELS <- c(
  chest_vs_eye = "Chest → eye", wrist_vs_eye = "Wrist → eye",
  LIGHT_vs_MEDI = "LIGHT → mEDI", `20s_vs_10s` = "20 → 10 s",
  `30s_vs_10s` = "30 → 10 s", `40s_vs_10s` = "40 → 10 s",
  `60s_vs_10s` = "60 → 10 s", `120s_vs_10s` = "120 → 10 s"
)
CLASS_ORDER <- MS_METRIC_CLASSES
CLASS_LABELS <- c(
  duration = "Duration", `exposure history` = "Exposure history", level = "Level",
  spectrum = "Spectrum", `temporal dynamics` = "Temporal dynamics", timing = "Timing"
)
STAGE_TITLES <- c(
  `self-calibration` = "a  Self-calibration from the degraded representation",
  `+ measurement signature` = "b  Added information retained within the low-burden measurement",
  `+ context` = "c  Added external, microenvironmental and behavioural context"
)

fig4_stage_plot <- function(atlas_long, stage_name) {
  d <- atlas_long |>
    filter(learner == "xgboost", stage == stage_name, n_unique_metrics >= 3L,
      comparison_pair_id %in% PAIR_ORDER, metric_class %in% CLASS_ORDER) |>
    mutate(
      pair = factor(comparison_pair_id, levels = PAIR_ORDER, labels = unname(PAIR_LABELS[PAIR_ORDER])),
      metric_class_plot = factor(metric_class, levels = rev(CLASS_ORDER),
        labels = unname(CLASS_LABELS[rev(CLASS_ORDER)])),
      win_label = if_else(is.finite(fraction_improved), sprintf("%.0f%%", 100 * fraction_improved), "")
    )
  if (!nrow(d)) stop("No plot-ready XGBoost cells for stage: ", stage_name, call. = FALSE)
  lim <- max(abs(d$mean_increment), na.rm = TRUE)
  if (!is.finite(lim) || lim <= 0) lim <- 1e-3
  d <- d |> mutate(text_colour = if_else(abs(mean_increment) >= .58 * lim, "white", "#25282A"))

  ggplot(d, aes(pair, metric_class_plot, fill = mean_increment)) +
    geom_tile(colour = "white", linewidth = .55) +
    geom_text(aes(label = win_label, colour = text_colour), size = 2.15,
      family = MS_FONT, fontface = "bold") +
    scale_colour_identity() +
    scale_fill_ms_diverging(lim, oob = scales::squish,
      name = expression("Mean incremental recovery  " * Delta * A)) +
    geom_vline(xintercept = c(2.5, 3.5), colour = "#7B8084", linewidth = .32) +
    labs(x = NULL, y = NULL, title = unname(STAGE_TITLES[[stage_name]])) +
    theme_ms(base_size = 7.0, legend_position = "right") +
    theme(
      panel.grid = element_blank(),
      panel.border = element_blank(),
      axis.ticks = element_blank(),
      axis.text.x = element_text(angle = 0, hjust = .5, size = 6.3),
      axis.text.y = element_text(size = 6.6),
      plot.title = element_text(size = 7.7, face = "bold", margin = margin(b = 3)),
      legend.title = element_text(size = 6.2), legend.text = element_text(size = 5.8),
      legend.key.height = grid::unit(9, "mm"),
      plot.margin = margin(2, 3, 2, 3)
    )
}

fig4_decoder_plot <- function(by_dimension) {
  long <- by_dimension |>
    filter(learner %in% c("ridge", "xgboost"), dimension %in% c("placement", "optical", "temporal")) |>
    select(learner, dimension, mean_calibration_increment, mean_signature_increment, mean_context_increment) |>
    pivot_longer(starts_with("mean_"), names_to = "stage", values_to = "mean_increment") |>
    mutate(
      stage = recode(stage,
        mean_calibration_increment = "Self-calibration",
        mean_signature_increment = "+ signature",
        mean_context_increment = "+ context"),
      stage = factor(stage, levels = c("Self-calibration", "+ signature", "+ context")),
      dimension = factor(dimension, levels = c("temporal", "optical", "placement"),
        labels = c("Temporal sampling", "Optical representation", "Placement")),
      learner = factor(learner, levels = c("ridge", "xgboost"), labels = c("Ridge", "XGBoost"))
    )

  ggplot(long, aes(mean_increment, dimension, colour = learner, shape = learner)) +
    geom_vline(xintercept = 0, colour = "#8A8E91", linewidth = .35) +
    geom_point(size = 2.0, position = position_dodge(width = .30)) +
    facet_wrap(~stage, scales = "free_x", nrow = 1) +
    scale_colour_manual(values = c(Ridge = MS_SECONDARY, XGBoost = MS_PRIMARY)) +
    scale_shape_manual(values = c(Ridge = 1, XGBoost = 16)) +
    labs(x = expression("Mean incremental recovery  " * Delta * A), y = NULL,
      colour = NULL, shape = NULL,
      title = "Linear and nonlinear decoders separate information availability from model flexibility") +
    theme_ms_axes(base_size = 7.0, legend_position = "bottom") +
    theme(
      strip.text = element_text(size = 6.8),
      axis.text = element_text(size = 6.2),
      plot.title = element_text(size = 7.6),
      legend.text = element_text(size = 6.2),
      panel.spacing.x = grid::unit(5, "mm")
    )
}

plot_recovery_ablation <- function(run_dir = NULL) {
  ablation_dir <- recovery_ablation_summarize(run_dir)
  run_dir <- dirname(ablation_dir)
  atlas_long <- read_csv(file.path(ablation_dir, "recovery_information_atlas_long.csv"),
    show_col_types = FALSE, progress = FALSE)
  by_dimension <- read_csv(file.path(ablation_dir, "recovery_ablation_by_dimension.csv"),
    show_col_types = FALSE, progress = FALSE)

  panels <- lapply(names(STAGE_TITLES), function(stage) fig4_stage_plot(atlas_long, stage))
  main <- plot_grid(plotlist = panels, ncol = 1, align = "v", axis = "lr", rel_heights = c(1, 1, 1))
  caption <- ggdraw() + draw_label(
    "Cell text = percentage of metrics improved at that information step. Cells with <3 metrics are omitted.\nColour bars are stage-specific because self-calibration and auxiliary-information gains differ strongly in scale; compare numeric ΔA scales rather than hue intensity across panels.",
    x = 0, hjust = 0, vjust = .5, size = 6.0, fontfamily = MS_FONT, colour = "#404447"
  )
  main <- plot_grid(main, caption, ncol = 1, rel_heights = c(1, .085))

  decoder <- fig4_decoder_plot(by_dimension)
  fig_dir <- file.path(ablation_dir, "figures")
  dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)
  ggsave(file.path(fig_dir, "Fig4_recovery_information_ablation.pdf"), main,
    width = 174, height = 190, units = "mm")
  ggsave(file.path(fig_dir, "Fig4_recovery_information_ablation.png"), main,
    width = 174, height = 190, units = "mm", dpi = MS_RASTER_DPI)
  ggsave(file.path(fig_dir, "FigS_recovery_decoder_capacity.pdf"), decoder,
    width = 174, height = 62, units = "mm")
  ggsave(file.path(fig_dir, "FigS_recovery_decoder_capacity.png"), decoder,
    width = 174, height = 62, units = "mm", dpi = MS_RASTER_DPI)
  write_csv(atlas_long |> filter(learner == "xgboost"),
    file.path(fig_dir, "Fig4_recovery_information_ablation_source.csv"))
  write_csv(by_dimension, file.path(fig_dir, "FigS_recovery_decoder_capacity_source.csv"))
  message("Recovery ablation figures: ", fig_dir)
  invisible(fig_dir)
}

if (sys.nframe() == 0L) {
  args <- commandArgs(trailingOnly = TRUE)
  if (length(args) > 1L) stop("Usage: Rscript scripts/13c_plot_fig4_recovery.R [recovery_run_dir]", call. = FALSE)
  plot_recovery_ablation(if (length(args)) args[[1]] else NULL)
}
