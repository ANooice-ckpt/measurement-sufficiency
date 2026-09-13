# Canonical Fig. 4: post-acquisition information recoverability.
# Frozen held-out results only: no refitting or raw-input access.
options(encoding = "UTF-8")
if (.Platform$OS.type == "windows") invisible(suppressWarnings(Sys.setlocale("LC_CTYPE", "English_United States.utf8")))
.ms_file <- grep("^--file=", commandArgs(FALSE), value = TRUE)
if (length(.ms_file)) setwd(dirname(dirname(normalizePath(sub("^--file=", "", .ms_file[1]), winslash = "/"))))
suppressPackageStartupMessages({library(tidyverse); library(cowplot)})
source("scripts/utils/figure_style.R")
source("scripts/utils/figure_atlas.R")
source("scripts/utils/plot_contracts.R")

args <- commandArgs(trailingOnly = TRUE)
if (length(args) > 1L) stop("Usage: Rscript scripts/13b_plot_fig4.R [recovery_run_dir]")
run_dir <- if (length(args)) args[1] else Sys.getenv("RQ2_RECOVERY_RUN_DIR", "")
if (!nzchar(run_dir)) {
  candidates <- Sys.glob("results/rq2/recovery/*/*/recovery_manifest.rds")
  candidates <- candidates[vapply(candidates, function(p) {
    z <- tryCatch(readRDS(p), error = function(e) NULL)
    !is.null(z) && isTRUE(z$complete) && !any(z$statuses$status == "failed") &&
      identical(z$provenance$recovery_version, "information_recoverability_v1")
  }, logical(1))]
  if (!length(candidates)) stop("No completed information-recoverability run found")
  if (length(candidates) > 1L) candidates <- candidates[[which.max(file.info(candidates)$mtime)]]
  run_dir <- dirname(candidates[[1]])
}
manifest_path <- file.path(run_dir, "recovery_manifest.rds")
ms_plot_require_files(manifest_path, "Recoverability figure")
frozen <- readRDS(manifest_path); prov <- frozen$provenance
if (!isTRUE(frozen$complete) || any(frozen$statuses$status == "failed")) stop("Incomplete recoverability run")
if (!identical(prov$recovery_version, "information_recoverability_v1")) stop("Fig. 4 requires information_recoverability_v1")
CORE_VERSION <- if (is.null(prov$core_artifact_version)) NA_character_ else as.character(prov$core_artifact_version)[1]
RQ1_VERSION <- if (is.null(prov$rq1_analysis_version)) NA_character_ else as.character(prov$rq1_analysis_version)[1]
RQ2_VERSION <- as.character(prov$recovery_version)[1]
N_SIGNATURE <- length(prov$signature_predictors)
N_DAILY_CONTEXT <- length(prov$predictors)
N_DAYPART_CONTEXT <- length(prov$temporal_predictors)
ratio_floor <- max(as.numeric(prov$G_floor), 1e-6)

PAIR_ORDER <- c("chest_vs_eye", "wrist_vs_eye", "LIGHT_vs_MEDI", "20s_vs_10s", "30s_vs_10s", "40s_vs_10s", "60s_vs_10s", "120s_vs_10s")
PAIR_LABELS <- c("Chest \u2192 eye", "Wrist \u2192 eye", "LIGHT \u2192 MEDI", "20 \u2192 10 s", "30 \u2192 10 s", "40 \u2192 10 s", "60 \u2192 10 s", "120 \u2192 10 s")
RECON_STATES <- c("raw", "calibration", "prior_only", "signature", "context_only", "context")
OBS_STATES <- c("null", "prior_only", "signature", "context_only", "context")

errors <- as_tibble(frozen$heldout_errors)
ms_plot_require_columns(errors, c("learner", "task_index", "dimension", "comparison_pair_id", "metric", "metric_class",
  "estimand", "state", "loss", "n_test", "n_test_participants", "status", "support_id"), "Recoverability errors")
if (anyDuplicated(errors[c("learner", "task_index", "estimand", "state")])) stop("Recoverability state/key mismatch")
available <- errors |> filter(status == "complete", is.finite(loss), loss >= 0)
if (!nrow(available) || !"xgboost" %in% unique(available$learner)) stop("Primary XGBoost results are absent")
state_contract <- available |> distinct(estimand, state)
if (!setequal(state_contract$state[state_contract$estimand == "reconstructability"], RECON_STATES) ||
    !setequal(state_contract$state[state_contract$estimand == "observability"], OBS_STATES)) stop("Estimand state contract mismatch")

keys <- c("learner", "task_index", "dimension", "comparison_pair_id", "metric", "metric_class", "support_id")
recon <- available |> filter(estimand == "reconstructability") |>
  select(all_of(keys), state, loss) |> pivot_wider(names_from = state, values_from = loss) |>
  mutate(pair = factor(comparison_pair_id, levels = PAIR_ORDER, labels = PAIR_LABELS),
    calibration_recovery = if_else(raw > ratio_floor, 100 * (raw - calibration) / raw, NA_real_),
    prior_recovery = if_else(raw > ratio_floor, 100 * (raw - prior_only) / raw, NA_real_),
    signature_recovery = if_else(raw > ratio_floor, 100 * (raw - signature) / raw, NA_real_),
    context_recovery = if_else(raw > ratio_floor, 100 * (raw - context_only) / raw, NA_real_),
    joint_recovery = if_else(raw > ratio_floor, 100 * (raw - context) / raw, NA_real_),
    signature_shapley = if_else(prior_only > ratio_floor,
      100 * .5 * ((prior_only - signature) + (context_only - context)) / prior_only, NA_real_),
    context_shapley = if_else(prior_only > ratio_floor,
      100 * .5 * ((prior_only - context_only) + (signature - context)) / prior_only, NA_real_))
if (anyNA(recon[RECON_STATES])) stop("Incomplete reconstructability estimates")

obs <- available |> filter(estimand == "observability") |>
  select(all_of(keys), state, loss) |> pivot_wider(names_from = state, values_from = loss) |>
  mutate(pair = factor(comparison_pair_id, levels = PAIR_ORDER, labels = PAIR_LABELS),
    prior_skill = if_else(null > ratio_floor, 100 * (null - prior_only) / null, NA_real_),
    signature_skill = if_else(null > ratio_floor, 100 * (null - signature) / null, NA_real_),
    context_skill = if_else(null > ratio_floor, 100 * (null - context_only) / null, NA_real_),
    joint_skill = if_else(null > ratio_floor, 100 * (null - context) / null, NA_real_),
    signature_shapley = if_else(prior_only > ratio_floor,
      100 * .5 * ((prior_only - signature) + (context_only - context)) / prior_only, NA_real_),
    context_shapley = if_else(prior_only > ratio_floor,
      100 * .5 * ((prior_only - context_only) + (signature - context)) / prior_only, NA_real_))
if (anyNA(obs[OBS_STATES])) stop("Incomplete distortion-observability estimates")

# Ratio-of-mean losses preserves the task weighting used elsewhere in the manuscript.
ratio_gain <- function(base, state) ifelse(is.finite(base) & base > ratio_floor, 100 * (base - state) / base, NA_real_)
recon_pair <- recon |> filter(learner == "xgboost") |> group_by(pair) |>
  summarise(across(all_of(RECON_STATES), mean), .groups = "drop") |>
  transmute(pair,
    `Calibration` = ratio_gain(raw, calibration), `P only` = ratio_gain(raw, prior_only),
    `P + S` = ratio_gain(raw, signature), `P + C` = ratio_gain(raw, context_only),
    `P + S + C` = ratio_gain(raw, context)) |>
  pivot_longer(-pair, names_to = "information", values_to = "gain") |>
  mutate(information = factor(information, levels = c("Calibration", "P only", "P + S", "P + C", "P + S + C")),
    pair = factor(pair, levels = rev(PAIR_LABELS)))

obs_pair <- obs |> filter(learner == "xgboost") |> group_by(pair) |>
  summarise(across(all_of(OBS_STATES), mean), .groups = "drop") |>
  transmute(pair,
    `P only` = ratio_gain(null, prior_only), `P + S` = ratio_gain(null, signature),
    `P + C` = ratio_gain(null, context_only), `P + S + C` = ratio_gain(null, context)) |>
  pivot_longer(-pair, names_to = "information", values_to = "skill") |>
  mutate(information = factor(information, levels = c("P only", "P + S", "P + C", "P + S + C")),
    pair = factor(pair, levels = rev(PAIR_LABELS)))

theme_recovery <- function() theme_ms_axes(base_size = 6.5, legend_position = "none") +
  theme(panel.grid.major = element_line(colour = "#E9EDEF", linewidth = .18), panel.grid.minor = element_blank(),
    axis.text = element_text(size = 5.5), strip.text = element_text(size = 6.0, face = "bold"),
    plot.margin = margin(3, 4, 3, 4))
panel_header <- function(p, title, subtitle, header = .13) ggdraw() +
  draw_plot(p, 0, 0, 1, 1 - header) +
  draw_label(title, x = .012, y = .995, hjust = 0, vjust = 1, size = 7.4, fontface = "bold", fontfamily = MS_FONT) +
  draw_label(subtitle, x = .012, y = 1 - header * .50, hjust = 0, vjust = 1, size = 5.15, colour = "#626A70", fontfamily = MS_FONT)

# a: practical reconstructability, anchored directly to RQ1 A.
p_a <- ggplot(recon_pair, aes(information, pair, fill = gain)) +
  geom_tile(colour = "white", linewidth = .35) +
  geom_text(aes(label = if_else(is.finite(gain), sprintf("%.0f", gain), "")), size = 2.0, family = MS_FONT, colour = "#30373B") +
  scale_fill_gradient2(low = "#D7B8AD", mid = "#FAFAFA", high = "#6687A5", midpoint = 0, name = "%") +
  labs(x = NULL, y = NULL) + theme_recovery() +
  theme(panel.grid = element_blank(), axis.text.x = element_text(angle = 28, hjust = 1, size = 5.2),
    axis.text.y = element_text(size = 5.3), legend.position = "right", legend.key.height = unit(7, "mm"), legend.text = element_text(size = 5))
pa <- panel_header(p_a, "a  Target-representation reconstructability",
  "Percent of original RQ1 distortion removed; raw low-configuration loss = A", .13)

# b: order-independent auxiliary attribution conditional on flexible P-only decoding.
attr <- recon |> filter(learner == "xgboost") |>
  mutate(metric_class = factor(metric_class, levels = MS_METRIC_CLASSES),
    dimension_plot = case_when(dimension == "placement" ~ "Placement", dimension == "optical" ~ "Optical", TRUE ~ "Temporal")) |>
  group_by(comparison_pair_id, pair, metric_class, dimension_plot) |>
  summarise(n_metrics = n(), signature_shapley = mean(signature_shapley, na.rm = TRUE),
    context_shapley = mean(context_shapley, na.rm = TRUE), .groups = "drop") |> filter(n_metrics >= 3L)
lim <- range(c(0, attr$signature_shapley, attr$context_shapley), na.rm = TRUE); span <- diff(lim)
if (!is.finite(span) || span <= 0) span <- 1
lim <- lim + c(-1, 1) * .07 * span
p_b <- ggplot(attr, aes(signature_shapley, context_shapley, colour = metric_class, shape = dimension_plot)) +
  geom_abline(slope = 1, intercept = 0, colour = "#9BA4A9", linewidth = .35) +
  geom_hline(yintercept = 0, colour = "#858F94", linewidth = .3) + geom_vline(xintercept = 0, colour = "#858F94", linewidth = .3) +
  geom_point(size = 2.05, stroke = .65, alpha = .9) +
  scale_colour_manual(values = MS_METRIC_COLORS) + scale_shape_manual(values = c(Placement = 16, Optical = 17, Temporal = 1)) +
  scale_x_continuous(limits = lim, labels = function(x) paste0(round(x), "%"), breaks = scales::breaks_pretty(3)) +
  scale_y_continuous(limits = lim, labels = function(x) paste0(round(x), "%"), breaks = scales::breaks_pretty(3)) +
  coord_fixed() + labs(x = "Signature-attributed reduction", y = "Context-attributed reduction") + theme_recovery()
pb <- panel_header(p_b, "b  Sources of reconstructable information",
  "Two-player Shapley attribution conditional on flexible P-only decoding", .13)

# c: ability to diagnose the magnitude of configuration-induced distortion D=|z|.
p_c <- ggplot(obs_pair, aes(information, pair, fill = skill)) +
  geom_tile(colour = "white", linewidth = .35) +
  geom_text(aes(label = if_else(is.finite(skill), sprintf("%.0f", skill), "")), size = 2.0, family = MS_FONT, colour = "#30373B") +
  scale_fill_gradient2(low = "#D7B8AD", mid = "#FAFAFA", high = "#B58A55", midpoint = 0, name = "%") +
  labs(x = NULL, y = NULL) + theme_recovery() +
  theme(panel.grid = element_blank(), axis.text.x = element_text(angle = 28, hjust = 1, size = 5.2),
    axis.text.y = element_text(size = 5.3), legend.position = "right", legend.key.height = unit(7, "mm"), legend.text = element_text(size = 5))
pc <- panel_header(p_c, "c  Distortion observability",
  "Held-out skill for predicting D = |z| relative to a training-median null model", .13)

# d: class-level synthesis of correction value and reliability information.
joint <- inner_join(
  recon |> filter(learner == "xgboost") |> select(task_index, dimension, comparison_pair_id, metric, metric_class, raw, context),
  obs |> filter(learner == "xgboost") |> select(task_index, null, context),
  by = "task_index", suffix = c("_recon", "_obs")) |>
  mutate(pair = factor(comparison_pair_id, levels = PAIR_ORDER, labels = PAIR_LABELS),
    metric_class = factor(metric_class, levels = MS_METRIC_CLASSES),
    dimension_plot = case_when(dimension == "placement" ~ "Placement", dimension == "optical" ~ "Optical", TRUE ~ "Temporal")) |>
  group_by(comparison_pair_id, pair, metric_class, dimension_plot) |>
  summarise(n_metrics = n(), raw = mean(raw), recon = mean(context_recon), null = mean(null), obs = mean(context_obs), .groups = "drop") |>
  filter(n_metrics >= 3L) |>
  mutate(reconstructability = ratio_gain(raw, recon), observability = ratio_gain(null, obs))

p_d <- ggplot(joint, aes(reconstructability, observability, colour = metric_class, shape = dimension_plot)) +
  geom_hline(yintercept = 0, colour = "#858F94", linewidth = .3) + geom_vline(xintercept = 0, colour = "#858F94", linewidth = .3) +
  geom_point(size = 2.15, stroke = .7, alpha = .9) +
  scale_colour_manual(values = MS_METRIC_COLORS) + scale_shape_manual(values = c(Placement = 16, Optical = 17, Temporal = 1)) +
  scale_x_continuous(labels = function(x) paste0(round(x), "%"), breaks = scales::breaks_pretty(4), expand = expansion(mult = .08)) +
  scale_y_continuous(labels = function(x) paste0(round(x), "%"), breaks = scales::breaks_pretty(4), expand = expansion(mult = .08)) +
  labs(x = "Target representation reconstructed", y = "Distortion magnitude made observable") + theme_recovery()
pd <- panel_header(p_d, "d  Recovery–observability landscape",
  "Each point is a metric class × configuration transition; full information = P + S + C", .13)

legend <- ms_metric_legend(text_size = 5.8, point_size = 1.2)
top <- plot_grid(pa, pb, nrow = 1, rel_widths = c(.57, .43))
bottom <- plot_grid(pc, pd, nrow = 1, rel_widths = c(.57, .43))
foot <- ggdraw() + draw_label(paste0(
  "a: reconstructability = 100 × [L_Y(raw) − L_Y(I)] / L_Y(raw), where L_Y is held-out standardized absolute geometric error and L_Y(raw)=RQ1 A.\n",
  "b: S/C Shapley components partition the auxiliary reduction beyond P-only decoding; finite-sample estimates may be negative; point shapes encode measurement dimension.\n",
  "c: observability = 100 × [L_D(null) − L_D(I)] / L_D(null), with D=|z| and fold-specific training-median null prediction.\n",
  "All information states use the same participant-grouped outer/inner procedure and the same candidate model-complexity grid, selected independently within training data.\n",
  "P = conventional calibration of Y_L; S = ", N_SIGNATURE, " low-configuration signature features; C = ", N_DAILY_CONTEXT,
  " daily + ", N_DAYPART_CONTEXT, " daypart context features. XGBoost is shown; ridge is retained as sensitivity analysis."),
  x = .01, hjust = 0, size = 4.65, colour = "#626A70", fontfamily = MS_FONT)
figure <- plot_grid(top, legend, bottom, foot, ncol = 1, rel_heights = c(.42, .045, .42, .115))

write_csv(recon_pair, "results/rq2/fig4_reconstructability_display.csv")
write_csv(attr, "results/rq2/fig4_reconstructability_attribution_display.csv")
write_csv(obs_pair, "results/rq2/fig4_observability_display.csv")
write_csv(joint, "results/rq2/fig4_recovery_observability_landscape.csv")
write_csv(recon, "results/rq2/fig4_reconstructability_task_display.csv")
write_csv(obs, "results/rq2/fig4_observability_task_display.csv")

ms_fig3_atlas_refine_main <- function(...) NULL
ms_fig3_refine_main <- function(...) NULL
ms_polish_main_figure <- function(plot, path, caller_env, width, height) list(plot = plot, width = width, height = height)
ms_plot_save(figure, "results/rq2/Fig4_RQ2.png", 7.40, 8.20)
ms_plot_write_manifest("results/rq2/figure_artifact_manifest.csv", tibble(
  figure = "Fig4_RQ2", input_artifact = manifest_path, core_artifact_version = CORE_VERSION,
  rq1_analysis_version = RQ1_VERSION, rq2_analysis_version = RQ2_VERSION,
  rq3_analysis_version = NA_character_, recovery_run_id = frozen$run_id,
  source_md5 = unname(tools::md5sum(manifest_path))))
message("Fig. 4 complete: ", nrow(recon |> filter(learner == "xgboost")), " matched-support tasks, ",
  n_distinct(recon$metric), " metrics, ", n_distinct(recon$comparison_pair_id), " anchors; recoverability run ", frozen$run_id)
