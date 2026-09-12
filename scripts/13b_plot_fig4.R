# Canonical Fig. 4: recovery from low-configuration measurements.
# Frozen held-out errors only: no analysis entrypoint, refitting or raw inputs.
options(encoding = "UTF-8")
if (.Platform$OS.type == "windows") invisible(suppressWarnings(Sys.setlocale("LC_CTYPE", "English_United States.utf8")))
.ms_file <- grep("^--file=", commandArgs(FALSE), value = TRUE)
if (length(.ms_file)) setwd(dirname(dirname(normalizePath(sub("^--file=", "", .ms_file[1]), winslash = "/"))))
suppressPackageStartupMessages({library(tidyverse); library(cowplot)})
source("scripts/utils/figure_style.R")
source("scripts/utils/figure_atlas.R")
source("scripts/utils/plot_contracts.R")
source("scripts/utils/analysis_design.R")

args <- commandArgs(trailingOnly = TRUE)
if (length(args) > 1L) stop("Usage: Rscript scripts/13b_plot_fig4.R [recovery_run_dir]")
run_dir <- if (length(args)) args[1] else Sys.getenv("RQ2_RECOVERY_RUN_DIR", "")
if (!nzchar(run_dir)) {
  candidates <- Sys.glob("results/rq2/recovery/*/*/recovery_manifest.rds")
  candidates <- candidates[vapply(candidates, function(p) {
    z <- readRDS(p)
    isTRUE(z$complete) && identical(z$provenance$analysis_design_id, ms_analysis_design_id()) &&
      identical(z$provenance$recovery_version, "rq2_recovery_v3_low_signature_temporal_context")
  }, logical(1))]
  if (length(candidates) != 1L) stop("Set RQ2_RECOVERY_RUN_DIR: expected exactly one compatible completed run")
  run_dir <- dirname(candidates)
}
manifest_path <- file.path(run_dir, "recovery_manifest.rds")
ms_plot_require_files(c(manifest_path, "results/rq1/rq1_pairwise_summary.csv"), "Recovery figure")
frozen <- readRDS(manifest_path); prov <- frozen$provenance
if (!isTRUE(frozen$complete) || any(frozen$statuses$status == "failed")) stop("Incomplete recovery run")
if (!identical(prov$analysis_design_id, ms_analysis_design_id()) ||
    !identical(prov$recovery_version, "rq2_recovery_v3_low_signature_temporal_context") ||
    length(prov$signature_predictors) != 16L || length(prov$predictors) != 18L || length(prov$temporal_predictors) != 12L)
  stop("Recovery version/design/information contract mismatch")
CORE_VERSION <- ms_plot_assert_core(prov$core_artifact_version)
RQ1_VERSION <- ms_plot_one_version(prov$rq1_analysis_version, "rq1_analysis_version")
rq1 <- read_csv("results/rq1/rq1_pairwise_summary.csv", show_col_types = FALSE)
if (!identical(ms_plot_one_version(rq1$rq1_analysis_version, "rq1_analysis_version"), RQ1_VERSION))
  stop("Recovery and RQ1 versions differ")
for (p in intersect(names(prov$input_md5), c("results/rq1/rq1_pairwise_summary.csv", "results/rq1/rq1_pairwise_change_long.rds")))
  if (!identical(unname(tools::md5sum(p)), unname(prov$input_md5[p]))) stop("Frozen recovery input MD5 mismatch: ", p)

PAIR_ORDER <- c("chest_vs_eye", "wrist_vs_eye", "LIGHT_vs_MEDI", "20s_vs_10s", "30s_vs_10s", "40s_vs_10s", "60s_vs_10s", "120s_vs_10s")
PAIR_LABELS <- c("Chest \u2192 eye", "Wrist \u2192 eye", "LIGHT \u2192 MEDI", "20 \u2192 10 s", "30 \u2192 10 s", "40 \u2192 10 s", "60 \u2192 10 s", "120 \u2192 10 s")
STATES <- c("raw", "calibration", "signature", "context")
STAGE_COLORS <- c(raw = "#737B82", calibration = "#405F80", signature = "#4D9085", context = "#C57A32")
errors <- as_tibble(frozen$heldout_errors)
ms_plot_require_columns(errors, c("learner", "task_index", "comparison_pair_id", "metric", "metric_class", "state", "A", "n_test", "n_test_participants", "status", "support_id"), "Recovery errors")
if (!setequal(unique(errors$comparison_pair_id), PAIR_ORDER) || !setequal(unique(errors$state), STATES) ||
    anyDuplicated(errors[c("learner", "task_index", "state")])) stop("Recovery anchor/state/key mismatch")
support_audit <- errors |> group_by(learner, task_index) |>
  summarise(n_states = n_distinct(state), supports = n_distinct(support_id),
    n_supports = n_distinct(n_test), n_participant_supports = n_distinct(n_test_participants), .groups = "drop")
if (any(support_audit$n_states != 4L | support_audit$supports != 1L | support_audit$n_supports != 1L | support_audit$n_participant_supports != 1L))
  stop("Recovery layers do not share held-out support")
available <- errors |> filter(status == "complete")
if (any(!is.finite(available$A) | available$A < 0)) stop("Invalid held-out standardized loss")
if (!setequal(unique(available$learner), c("xgboost", "ridge"))) stop("Both learners required")
wide <- available |> select(learner, task_index, dimension, comparison_pair_id, metric, metric_class, support_id, state, A) |>
  pivot_wider(names_from = state, values_from = A) |>
  mutate(calibration_gain = raw - calibration, signature_gain = calibration - signature,
    context_gain = signature - context, total_gain = raw - context,
    pair = factor(comparison_pair_id, levels = PAIR_ORDER, labels = PAIR_LABELS))
if (anyNA(wide[STATES])) stop("Incomplete four-state estimates")
primary <- wide |> filter(learner == "xgboost")
if (nrow(primary) != 414L || n_distinct(primary$metric) != 52L) stop("Expected 414 available tasks and 52 daily metrics")

theme_recovery <- function() theme_ms_axes(base_size = 6.5, legend_position = "none") +
  theme(panel.grid.major = element_line(colour = "#E9EDEF", linewidth = .18), panel.grid.minor = element_blank(),
    axis.text = element_text(size = 5.6), strip.text = element_text(size = 6.1, face = "bold"),
    plot.title = element_text(size = 7.4, face = "bold"), plot.subtitle = element_text(size = 5.4, colour = "#666D72"),
    plot.margin = margin(3, 4, 3, 4))
panel_header <- function(p, title, subtitle, header = .14) ggdraw() +
  draw_plot(p, 0, 0, 1, 1 - header) +
  draw_label(title, x = .012, y = .995, hjust = 0, vjust = 1, size = 7.4, fontface = "bold", fontfamily = MS_FONT) +
  draw_label(subtitle, x = .012, y = 1 - header * .50, hjust = 0, vjust = 1, size = 5.2, colour = "#626A70", fontfamily = MS_FONT)

# Relative stage gains use ratios of metric-equal means, not means of
# potentially unstable per-metric ratios. Signed gains are never truncated.
gain_names <- c("calibration_gain", "signature_gain", "context_gain")
STAGE_LABELS <- c(calibration_gain = "Calibrate YL", signature_gain = "+ signature", context_gain = "+ context")
ratio_floor <- max(as.numeric(prov$G_floor), 1e-6)
relative_gain <- function(gain, baseline) ifelse(is.finite(baseline) & baseline > ratio_floor, 100 * gain / baseline, NA_real_)
stage_rows <- wide |> pivot_longer(all_of(gain_names), names_to = "stage", values_to = "gain") |>
  mutate(baseline = case_when(stage == "calibration_gain" ~ raw, stage == "signature_gain" ~ calibration, TRUE ~ signature),
    stage = factor(stage, levels = gain_names, labels = unname(STAGE_LABELS)),
    metric_class = factor(metric_class, levels = MS_METRIC_CLASSES))
contrast_summary <- stage_rows |> group_by(learner, pair, stage) |>
  summarise(mean_gain = mean(gain), mean_baseline = mean(baseline), .groups = "drop") |>
  mutate(relative = relative_gain(mean_gain, mean_baseline), pair = factor(pair, levels = rev(PAIR_LABELS)))
class_summary <- stage_rows |> group_by(learner, comparison_pair_id, pair, metric_class, stage) |>
  summarise(n_metrics = n(), mean_gain = mean(gain), mean_baseline = mean(baseline),
    fraction_improved = mean(gain > 0), .groups = "drop") |>
  mutate(relative = relative_gain(mean_gain, mean_baseline), denominator_small = mean_baseline <= ratio_floor)
# Class summaries with fewer than three metrics remain in the audit, but do not
# masquerade as class-wide patterns in the main figure (same rule as the draft).
class_main <- class_summary |> filter(learner == "xgboost", n_metrics >= 3L)
display_classes <- MS_METRIC_CLASSES[MS_METRIC_CLASSES %in% as.character(class_main$metric_class)]

p_stage <- ggplot(contrast_summary |> filter(learner == "xgboost"), aes(relative, pair, colour = stage)) +
  geom_vline(xintercept = 0, colour = "#909A9F", linewidth = .35) +
  geom_segment(data = contrast_summary |> filter(learner == "xgboost"),
    aes(x = 0, xend = relative, yend = pair), linewidth = .75, alpha = .55) +
  geom_point(size = 1.8, stroke = .6) +
  facet_wrap(~stage, nrow = 1, scales = "free_x") +
  scale_colour_manual(values = c("Calibrate YL" = "#405F80", "+ signature" = "#4D9085", "+ context" = "#C57A32")) +
  scale_x_continuous(breaks = scales::breaks_pretty(n = 3), labels = function(x) paste0(x, "%"), expand = expansion(mult = .16)) +
  labs(x = "Reduction of preceding-stage loss", y = NULL) + theme_recovery() +
  theme(panel.grid.major.y = element_blank(), panel.spacing.x = unit(2, "mm"), axis.text.x = element_text(size = 5.2))
pa <- panel_header(p_stage, "a  What does each information layer add?",
  "XGBoost; relative reduction at each step; separate x-scales", .15)

# Plot context's incremental effect directly instead of near-diagonal final A.
context_points <- class_main |> filter(stage == "+ context") |>
  mutate(dimension = case_when(comparison_pair_id %in% PAIR_ORDER[1:2] ~ "Placement",
    comparison_pair_id == PAIR_ORDER[3] ~ "Optical", TRUE ~ "Temporal"))
p_context <- ggplot(context_points, aes(mean_baseline, relative, colour = metric_class, shape = dimension)) +
  geom_hline(yintercept = 0, colour = "#7C878D", linewidth = .4) +
  geom_point(size = 2.1, stroke = .7, alpha = .90) +
  geom_text(data = context_points |> filter(metric_class == "level", dimension != "Temporal") |>
      mutate(label = case_when(comparison_pair_id == "LIGHT_vs_MEDI" ~ "LIGHT", comparison_pair_id == "chest_vs_eye" ~ "Chest", TRUE ~ "Wrist")),
    aes(label = label), nudge_y = .22, size = 2.0, show.legend = FALSE, family = MS_FONT) +
  scale_colour_manual(values = MS_METRIC_COLORS) +
  scale_shape_manual(values = c(Placement = 16, Optical = 17, Temporal = 1)) +
  scale_x_continuous(trans = scales::pseudo_log_trans(sigma = .01), breaks = c(.01, .1, .5), labels = scales::label_number()) +
  scale_y_continuous(labels = function(x) paste0(x, "%"), breaks = scales::breaks_pretty(4), expand = expansion(mult = .12)) +
  labs(x = expression("Loss before context, "*A[S]), y = "Additional reduction from context") + theme_recovery()
pb <- panel_header(p_context, "b  Where does context help?",
  "Each point = class \u00d7 contrast; above zero = added recovery", .15)

# Draft-inspired class-level ablation: absolute magnitude and the proportion of
# improved metrics are complementary. Independent linear colourbars expose the
# much smaller signature/context increments without changing numerical values.
atlas_long <- class_summary
atlas_panel <- function(stage_name, title) {
  z <- class_main |> filter(stage == stage_name) |>
    mutate(class_plot = factor(metric_class, levels = rev(display_classes),
      labels = stringr::str_to_sentence(rev(display_classes))),
      pair = factor(pair, levels = PAIR_LABELS), label = sprintf("%.0f%%", 100 * fraction_improved))
  lim <- max(abs(z$mean_gain))
  z$text_colour <- ifelse(abs(z$mean_gain) > .6 * lim, "white", "#30373B")
  ggplot(z, aes(pair, class_plot, fill = mean_gain)) +
    geom_tile(colour = "white", linewidth = .65) +
    geom_text(aes(label = label, colour = text_colour), size = 2.4, fontface = "bold", family = MS_FONT) +
    scale_colour_identity() +
    geom_vline(xintercept = c(2.5, 3.5), colour = "#7E888E", linewidth = .3) +
    scale_fill_gradient2(low = "#31678C", mid = "#F5F5F0", high = "#B66A30", midpoint = 0,
      limits = c(-lim, lim), name = "Mean gain, \u0394A", breaks = c(-lim, 0, lim), labels = function(x) signif(x, 2),
      guide = guide_colourbar(barwidth = unit(2.7, "mm"), barheight = unit(16, "mm"), title.position = "top")) +
    labs(x = NULL, y = NULL, title = title) + theme_recovery() +
    theme(panel.grid = element_blank(), axis.ticks = element_blank(), axis.line = element_blank(),
      axis.text.x = element_text(size = 6), axis.text.y = element_text(size = 6.5),
      legend.position = "right", legend.title = element_text(size = 5.4), legend.text = element_text(size = 5.5),
      plot.title = element_text(size = 7.5), plot.margin = margin(4, 4, 4, 4))
}
pc <- atlas_panel("Calibrate YL", "c  Self-calibration from the low-configuration metric")
pd <- atlas_panel("+ signature", "d  Additional information retained in the low measurement")
pe <- atlas_panel("+ context", "e  Additional external, microenvironmental and behavioural context")
atlas_body <- plot_grid(pc, pd, pe, ncol = 1, align = "v", axis = "lr")
legend <- ms_metric_legend(text_size = 5.8, point_size = 1.2)
top <- plot_grid(pa, pb, nrow = 1, rel_widths = c(.61, .39))
foot <- ggdraw() + draw_label(paste0(
  "a,b: 100 \u00d7 mean(stage gain) / mean(preceding-stage loss); denominators \u2264 ", format(ratio_floor, scientific = TRUE), " are unavailable.\n",
  "c\u2013e: colour = signed mean \u0394A on the frozen RQ1 scale; text = % of metrics improved. Colour scales differ by stage.\n",
  "Class displays require \u22653 metrics; singleton exposure-history/spectrum summaries remain in the audit. Classes are descriptive.\n",
  "b: filled circle = placement; triangle = optical; open circle = temporal. YL = low metric; S = 16 signature features; C = 18 daily + 12 daypart features.\n",
  "All gains use identical participant-grouped held-out support; negative gains are retained. Ridge sensitivity is retained in the display audit."),
  x = .01, hjust = 0, size = 5.2, colour = "#626A70", fontfamily = MS_FONT)
figure <- plot_grid(top, legend, atlas_body, foot, ncol = 1, rel_heights = c(.33, .04, .54, .09))
write_csv(contrast_summary, "results/rq2/fig4_recovery_relative_display.csv")

# Keep registry numbering and PNG export, but bypass the retired composition
# inside this entrypoint only. Shared helpers and other figures are unchanged.
ms_fig3_atlas_refine_main <- function(...) NULL
ms_fig3_refine_main <- function(...) NULL
ms_polish_main_figure <- function(plot, path, caller_env, width, height) list(plot = plot, width = width, height = height)
ms_plot_save(figure, "results/rq2/Fig3_RQ2.png", 7.40, 8.20)
write_csv(atlas_long, "results/rq2/fig4_recovery_increment_display.csv")
write_csv(wide, "results/rq2/fig4_recovery_loss_display.csv")
ms_plot_write_manifest("results/rq2/figure_artifact_manifest.csv", tibble(
  figure = "Fig3_RQ2", input_artifact = manifest_path, core_artifact_version = CORE_VERSION,
  rq1_analysis_version = RQ1_VERSION, rq2_analysis_version = prov$recovery_version,
  rq3_analysis_version = NA_character_, recovery_run_id = frozen$run_id,
  source_md5 = unname(tools::md5sum(manifest_path))))
message("Fig. 4 complete: 414 matched-support tasks, 52 metrics, eight anchors; recovery run ", frozen$run_id)
