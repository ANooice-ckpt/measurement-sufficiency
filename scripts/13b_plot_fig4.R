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
      identical(z$provenance$recovery_version, "rq2_recovery_v4_factorial_context")
  }, logical(1))]
  if (length(candidates) != 1L) stop("Set RQ2_RECOVERY_RUN_DIR: expected exactly one compatible completed run")
  run_dir <- dirname(candidates)
}
manifest_path <- file.path(run_dir, "recovery_manifest.rds")
ms_plot_require_files(c(manifest_path, "results/rq1/rq1_pairwise_summary.csv"), "Recovery figure")
frozen <- readRDS(manifest_path); prov <- frozen$provenance
if (!isTRUE(frozen$complete) || any(frozen$statuses$status == "failed")) stop("Incomplete recovery run")
if (!identical(prov$analysis_design_id, ms_analysis_design_id()) ||
    !identical(prov$recovery_version, "rq2_recovery_v4_factorial_context") ||
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
STATES <- c("raw", "calibration", "signature", "context_only", "context")
STAGE_COLORS <- c(raw = "#737B82", calibration = "#405F80", signature = "#4D9085", context = "#C57A32")
errors <- as_tibble(frozen$heldout_errors)
ms_plot_require_columns(errors, c("learner", "task_index", "comparison_pair_id", "metric", "metric_class", "state", "A", "n_test", "n_test_participants", "status", "support_id"), "Recovery errors")
if (!setequal(unique(errors$comparison_pair_id), PAIR_ORDER) || !setequal(unique(errors$state), STATES) ||
    anyDuplicated(errors[c("learner", "task_index", "state")])) stop("Recovery anchor/state/key mismatch")
support_audit <- errors |> group_by(learner, task_index) |>
  summarise(n_states = n_distinct(state), supports = n_distinct(support_id),
    n_supports = n_distinct(n_test), n_participant_supports = n_distinct(n_test_participants), .groups = "drop")
if (any(support_audit$n_states != 5L | support_audit$supports != 1L | support_audit$n_supports != 1L | support_audit$n_participant_supports != 1L))
  stop("Recovery layers do not share held-out support")
available <- errors |> filter(status == "complete")
if (any(!is.finite(available$A) | available$A < 0)) stop("Invalid held-out standardized loss")
if (!setequal(unique(available$learner), c("xgboost", "ridge"))) stop("Both learners required")
wide <- available |> select(learner, task_index, dimension, comparison_pair_id, metric, metric_class, support_id, state, A) |>
  pivot_wider(names_from = state, values_from = A) |>
  mutate(calibration_gain = raw - calibration, signature_gain = calibration - signature,
    context_gain = signature - context, context_total_gain = calibration - context_only,
    joint_gain = calibration - context, signature_after_context_gain = context_only - context,
    shared_or_interaction = (calibration - context_only) - (signature - context), total_gain = raw - context,
    context_shapley_gain = .5 * (context_total_gain + context_gain),
    signature_shapley_gain = .5 * (signature_gain + signature_after_context_gain),
    attribution_balance = context_shapley_gain - signature_shapley_gain,
    pair = factor(comparison_pair_id, levels = PAIR_ORDER, labels = PAIR_LABELS))
if (anyNA(wide[STATES])) stop("Incomplete factorial estimates")
if (max(abs((wide$context_shapley_gain + wide$signature_shapley_gain) - wide$joint_gain)) > 1e-10)
  stop("Shapley attribution does not reconstruct the joint auxiliary gain")
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

# Factorial branch gains share the calibrated YL baseline. Ratios of metric-equal
# means avoid unstable per-metric ratios; signed gains are never truncated.
gain_names <- c("signature_gain", "context_total_gain", "joint_gain", "context_gain")
STAGE_LABELS <- c(signature_gain = "+ S", context_total_gain = "+ C", joint_gain = "+ S + C", context_gain = "C after S")
ratio_floor <- max(as.numeric(prov$G_floor), 1e-6)
relative_gain <- function(gain, baseline) ifelse(is.finite(baseline) & baseline > ratio_floor, 100 * gain / baseline, NA_real_)
stage_rows <- wide |> pivot_longer(all_of(gain_names), names_to = "stage", values_to = "gain") |>
  mutate(baseline = calibration,
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

# Two-player Shapley attribution averages the two possible entry orders for S and C.
# It is order-independent and exactly partitions the observed joint auxiliary gain.
class_attribution <- wide |>
  mutate(metric_class = factor(metric_class, levels = MS_METRIC_CLASSES)) |>
  group_by(learner, comparison_pair_id, pair, metric_class) |>
  summarise(n_metrics = n(), mean_baseline = mean(calibration),
    mean_signature_shapley_gain = mean(signature_shapley_gain),
    mean_context_shapley_gain = mean(context_shapley_gain),
    mean_full_auxiliary_gain = mean(joint_gain),
    mean_attribution_balance = mean(attribution_balance),
    fraction_signature_shapley_positive = mean(signature_shapley_gain > 0),
    fraction_context_shapley_positive = mean(context_shapley_gain > 0), .groups = "drop") |>
  mutate(relative_signature_shapley = relative_gain(mean_signature_shapley_gain, mean_baseline),
    relative_context_shapley = relative_gain(mean_context_shapley_gain, mean_baseline),
    context_share = if_else(mean_full_auxiliary_gain > ratio_floor &
      mean_signature_shapley_gain >= 0 & mean_context_shapley_gain >= 0,
      mean_context_shapley_gain / mean_full_auxiliary_gain, NA_real_))
class_attribution_main <- class_attribution |> filter(learner == "xgboost", n_metrics >= 3L)
if (any(class_attribution_main$context_share < -1e-10 | class_attribution_main$context_share > 1 + 1e-10, na.rm = TRUE))
  stop("Context share falls outside [0,1] despite a positive non-negative Shapley partition")

p_stage <- ggplot(contrast_summary |> filter(learner == "xgboost", stage != "C after S"), aes(relative, pair, colour = stage)) +
  geom_vline(xintercept = 0, colour = "#909A9F", linewidth = .35) +
  geom_segment(data = contrast_summary |> filter(learner == "xgboost", stage != "C after S"),
    aes(x = 0, xend = relative, yend = pair), linewidth = .75, alpha = .55) +
  geom_point(size = 1.8, stroke = .6) +
  facet_wrap(~stage, nrow = 1, scales = "free_x") +
  scale_colour_manual(values = c("+ S" = "#4D9085", "+ C" = "#C57A32", "+ S + C" = "#405F80")) +
  scale_x_continuous(breaks = scales::breaks_pretty(n = 3), labels = function(x) paste0(x, "%"), expand = expansion(mult = .16)) +
  labs(x = "Reduction relative to YL-only loss", y = NULL) + theme_recovery() +
  theme(panel.grid.major.y = element_blank(), panel.spacing.x = unit(2, "mm"), axis.text.x = element_text(size = 5.2))
pa <- panel_header(p_stage, "a  Independent and joint information gains",
  "XGBoost; all branches share YL-only baseline; separate x-scales", .15)

# Compare order-independent information attribution on a common relative scale.
attribution_points <- class_attribution_main |>
  mutate(dimension = case_when(comparison_pair_id %in% PAIR_ORDER[1:2] ~ "Placement",
    comparison_pair_id == PAIR_ORDER[3] ~ "Optical", TRUE ~ "Temporal"))
attribution_range <- range(c(0, attribution_points$relative_signature_shapley,
  attribution_points$relative_context_shapley), na.rm = TRUE)
attribution_span <- diff(attribution_range)
if (!is.finite(attribution_span) || attribution_span <= 0) attribution_span <- 1
attribution_limits <- attribution_range + c(-1, 1) * .06 * attribution_span
p_context <- ggplot(attribution_points,
    aes(relative_signature_shapley, relative_context_shapley, colour = metric_class, shape = dimension)) +
  geom_abline(slope = 1, intercept = 0, colour = "#9BA4A9", linewidth = .35) +
  geom_hline(yintercept = 0, colour = "#7C878D", linewidth = .3) +
  geom_vline(xintercept = 0, colour = "#7C878D", linewidth = .3) +
  geom_point(size = 2.1, stroke = .7, alpha = .90) +
  scale_colour_manual(values = MS_METRIC_COLORS) +
  scale_shape_manual(values = c(Placement = 16, Optical = 17, Temporal = 1)) +
  scale_x_continuous(limits = attribution_limits, labels = function(x) paste0(x, "%"), breaks = scales::breaks_pretty(3)) +
  scale_y_continuous(limits = attribution_limits, labels = function(x) paste0(x, "%"), breaks = scales::breaks_pretty(3)) +
  coord_fixed() + labs(x = "Signature-attributed gain", y = "Context-attributed gain") + theme_recovery()
pb <- panel_header(p_context, "b  Order-independent attribution of recoverable information",
  "Class \u00d7 contrast; above diagonal = context-dominant", .15)

# Main lower panels are intentionally orthogonal to a/b: c shows the metric-level
# composition hidden by class means, while d translates that heterogeneity into a
# descriptive acquisition/action map. Signed estimates remain in the audit CSVs.
COMPONENT_LEVELS <- c("Context-dominant", "Signature-dominant", "Mixed / unstable", "No usable recovery")
COMPONENT_COLORS <- c("Context-dominant" = "#C57A32", "Signature-dominant" = "#4D9085",
  "Mixed / unstable" = "#AEB6BA", "No usable recovery" = "#E2E6E8")
composition <- wide |> filter(learner == "xgboost", metric_class %in% display_classes) |>
  mutate(recovery_component = case_when(
    joint_gain <= 0 ~ "No usable recovery",
    signature_shapley_gain < 0 | context_shapley_gain < 0 ~ "Mixed / unstable",
    context_shapley_gain > signature_shapley_gain ~ "Context-dominant",
    TRUE ~ "Signature-dominant"),
    recovery_component = factor(recovery_component, levels = COMPONENT_LEVELS),
    class_plot = factor(as.character(metric_class), levels = display_classes,
      labels = stringr::str_to_sentence(display_classes)),
    pair_plot = factor(as.character(pair), levels = rev(PAIR_LABELS))) |>
  count(comparison_pair_id, pair_plot, class_plot, recovery_component, name = "n_metrics") |>
  group_by(comparison_pair_id, pair_plot, class_plot) |>
  mutate(total_metrics = sum(n_metrics), fraction = n_metrics / total_metrics,
    label = if_else(fraction >= .14, sprintf("%.0f%%", 100 * fraction), ""),
    text_colour = if_else(recovery_component %in% c("Context-dominant", "Signature-dominant"),
      "white", "#30373B")) |> ungroup() |>
  filter(total_metrics >= 3L)

p_composition <- ggplot(composition, aes(fraction, pair_plot, fill = recovery_component)) +
  geom_col(width = .72, colour = "white", linewidth = .25) +
  geom_text(aes(label = label, colour = text_colour), position = position_stack(vjust = .5),
    size = 2.05, fontface = "bold", family = MS_FONT) +
  scale_colour_identity() +
  scale_fill_manual(values = COMPONENT_COLORS, limits = COMPONENT_LEVELS, drop = FALSE, name = NULL) +
  scale_x_continuous(limits = c(0, 1), breaks = c(0, .5, 1), labels = scales::label_percent(accuracy = 1),
    expand = expansion(mult = c(0, .01))) +
  facet_wrap(~class_plot, ncol = 2) +
  labs(x = "Share of metrics", y = NULL) + theme_recovery() +
  theme(panel.grid.major.y = element_blank(), panel.grid.minor = element_blank(),
    strip.text = element_text(size = 6.2, face = "bold"), axis.text = element_text(size = 5.4),
    legend.position = "bottom", legend.text = element_text(size = 5.3), legend.key.width = unit(5.5, "mm"),
    legend.margin = margin(t = 1), panel.spacing = unit(2.3, "mm"), plot.margin = margin(3, 3, 3, 3))
pc <- panel_header(p_composition, "c  Metric-level composition of full recovery",
  "Each bar partitions all metrics: usable S/C dominance, mixed attribution, or no gain", .11)

# Practical action coordinates are frequencies rather than mean gains. The x-axis
# is how often S+C improves over YL-only; the y-axis is how often context dominates
# among recovered metrics with a stable non-negative Shapley partition.
action_summary <- wide |> filter(learner == "xgboost", metric_class %in% display_classes) |>
  group_by(comparison_pair_id, pair, metric_class) |>
  summarise(n_metrics = n(), p_recovery = mean(joint_gain > 0),
    n_stable_positive = sum(joint_gain > 0 & signature_shapley_gain >= 0 & context_shapley_gain >= 0),
    n_context_dominant = sum(joint_gain > 0 & signature_shapley_gain >= 0 & context_shapley_gain >= 0 &
      context_shapley_gain > signature_shapley_gain), .groups = "drop") |>
  mutate(p_context_dominant = if_else(n_stable_positive > 0,
      n_context_dominant / n_stable_positive, NA_real_),
    dimension = case_when(comparison_pair_id %in% PAIR_ORDER[1:2] ~ "Placement",
      comparison_pair_id == PAIR_ORDER[3] ~ "Optical", TRUE ~ "Temporal"),
    action_class = case_when(
      p_recovery < .50 ~ "No deployable benefit",
      !is.finite(p_context_dominant) ~ "Mixed evidence",
      p_context_dominant < .33 ~ "Signature-led",
      p_context_dominant < .67 ~ "Context-useful",
      TRUE ~ "Context-important")) |>
  filter(n_metrics >= 3L)
action_finite <- action_summary |> filter(is.finite(p_context_dominant))
action_unresolved <- action_summary |> filter(!is.finite(p_context_dominant)) |> mutate(p_context_dominant = 0)

p_action <- ggplot() +
  annotate("rect", xmin = 0, xmax = .5, ymin = 0, ymax = 1, fill = "#AEB6BA", alpha = .10) +
  annotate("rect", xmin = .5, xmax = 1, ymin = 0, ymax = .33, fill = "#4D9085", alpha = .08) +
  annotate("rect", xmin = .5, xmax = 1, ymin = .33, ymax = .67, fill = "#D9B07C", alpha = .10) +
  annotate("rect", xmin = .5, xmax = 1, ymin = .67, ymax = 1, fill = "#C57A32", alpha = .10) +
  geom_vline(xintercept = .5, linetype = 2, colour = "#8A9398", linewidth = .3) +
  geom_hline(yintercept = c(.33, .67), linetype = 2, colour = "#A0A8AC", linewidth = .28) +
  geom_point(data = action_finite,
    aes(p_recovery, p_context_dominant, colour = metric_class, shape = dimension),
    size = 2.15, stroke = .7, alpha = .92) +
  geom_point(data = action_unresolved, aes(p_recovery, p_context_dominant),
    shape = 4, size = 2.0, stroke = .7, colour = "#858D92") +
  annotate("text", x = .24, y = .93, label = "No deployable\nbenefit", size = 2.15,
    family = MS_FONT, colour = "#70787D") +
  annotate("text", x = .76, y = .16, label = "Signature-led", size = 2.15,
    family = MS_FONT, colour = "#667176") +
  annotate("text", x = .76, y = .50, label = "Context-useful", size = 2.15,
    family = MS_FONT, colour = "#756B60") +
  annotate("text", x = .76, y = .84, label = "Context-important", size = 2.15,
    family = MS_FONT, colour = "#75685F") +
  scale_colour_manual(values = MS_METRIC_COLORS) +
  scale_shape_manual(values = c(Placement = 16, Optical = 17, Temporal = 1)) +
  scale_x_continuous(limits = c(0, 1), breaks = c(0, .5, 1), labels = scales::label_percent(accuracy = 1),
    expand = expansion(mult = .01)) +
  scale_y_continuous(limits = c(0, 1), breaks = c(0, .33, .67, 1), labels = scales::label_percent(accuracy = 1),
    expand = expansion(mult = .01)) +
  labs(x = "Metrics with positive S + C recovery", y = "Context-dominant among stable recovered metrics") +
  theme_recovery() +
  theme(panel.grid = element_blank(), axis.text = element_text(size = 5.2), axis.title = element_text(size = 5.5),
    plot.margin = margin(3, 4, 3, 4))
pd <- panel_header(p_action, "d  Practical recovery action map",
  "Frequency-based deployment view; shaded regions are descriptive heuristics", .11)

# Preserve all signed and sequential summaries in audit outputs; only the main
# lower panels switch from mean signed heatmaps to heterogeneity/actionability.
atlas_long <- class_summary
legend <- ms_metric_legend(text_size = 5.8, point_size = 1.2)
top <- plot_grid(pa, pb, nrow = 1, rel_widths = c(.61, .39))
lower <- plot_grid(pc, pd, nrow = 1, rel_widths = c(.64, .36), align = "h", axis = "tb")
foot <- ggdraw() + draw_label(paste0(
  "a: 100 \u00d7 mean(branch gain) / mean(YL-only loss); denominators \u2264 ", format(ratio_floor, scientific = TRUE), " are unavailable.\n",
  "b: axes = 100 \u00d7 mean(two-player Shapley gain) / mean(YL-only loss); diagonal = equal order-independent attribution.\n",
  "c: metric-level categories use full S+C gain and two-player Shapley components; mixed = positive S+C recovery with either attributed component < 0.\n",
  "d: x = P(S+C improves YL-only); y = P(context Shapley > signature Shapley | positive S+C recovery and both Shapley components \u2265 0).\n",
  "d thresholds (50% recovery; 33/67% context dominance) are descriptive deployment heuristics, not inferential cutoffs; crosses at y=0 indicate no stable non-negative partition.\n",
  "Shapley values average the two S/C entry orders and exactly partition observed S+C recovery; they are model-dependent attribution, not causal effects or information-theoretic necessity.\n",
  "Class displays require \u22653 metrics; singleton exposure-history/spectrum summaries remain in the audit. b/d: filled circle = placement; triangle = optical; open circle = temporal.\n",
  "YL = low metric; S = 16 signature features; C = 18 daily + 12 daypart features. All gains use identical participant-grouped held-out support; full signed estimates are retained in exported audits."),
  x = .01, hjust = 0, size = 4.85, colour = "#626A70", fontfamily = MS_FONT)
figure <- plot_grid(top, legend, lower, foot, ncol = 1, rel_heights = c(.34, .04, .52, .10))
write_csv(contrast_summary, "results/rq2/fig4_recovery_relative_display.csv")

# Keep registry numbering and PNG export, but bypass the retired composition
# inside this entrypoint only. Shared helpers and other figures are unchanged.
ms_fig3_atlas_refine_main <- function(...) NULL
ms_fig3_refine_main <- function(...) NULL
ms_polish_main_figure <- function(plot, path, caller_env, width, height) list(plot = plot, width = width, height = height)
ms_plot_save(figure, "results/rq2/Fig3_RQ2.png", 7.40, 8.20)
write_csv(atlas_long, "results/rq2/fig4_recovery_increment_display.csv")
write_csv(class_attribution, "results/rq2/fig4_recovery_attribution_display.csv")
write_csv(composition, "results/rq2/fig4_recovery_composition_display.csv")
write_csv(action_summary, "results/rq2/fig4_recovery_action_display.csv")
write_csv(wide, "results/rq2/fig4_recovery_loss_display.csv")
ms_plot_write_manifest("results/rq2/figure_artifact_manifest.csv", tibble(
  figure = "Fig3_RQ2", input_artifact = manifest_path, core_artifact_version = CORE_VERSION,
  rq1_analysis_version = RQ1_VERSION, rq2_analysis_version = prov$recovery_version,
  rq3_analysis_version = NA_character_, recovery_run_id = frozen$run_id,
  source_md5 = unname(tools::md5sum(manifest_path))))
message("Fig. 4 complete: 414 matched-support tasks, 52 metrics, eight anchors; orthogonal recovery composition/actionability panels; recovery run ", frozen$run_id)
