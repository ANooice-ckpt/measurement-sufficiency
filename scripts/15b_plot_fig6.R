options(encoding = "UTF-8")
if (.Platform$OS.type == "windows") invisible(suppressWarnings(Sys.setlocale("LC_CTYPE", "English_United States.utf8")))
# Canonical RQ3 plotting source. All accepted display refinements are consolidated here.
.ms_file <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
if (length(.ms_file)) {
  .ms_script <- normalizePath(sub("^--file=", "", .ms_file[[1]]), winslash = "/", mustWork = TRUE)
  .ms_root <- normalizePath(file.path(dirname(.ms_script), ".."), winslash = "/", mustWork = TRUE)
  if (!file.exists(file.path(.ms_root, "scripts", "utils", "figure_style.R"))) {
    stop("Could not resolve measurement-sufficiency repository root from ", .ms_script, call. = FALSE)
  }
  setwd(.ms_root)
}
rm(.ms_file)
if (exists(".ms_script")) rm(.ms_script)
if (exists(".ms_root")) rm(.ms_root)
suppressPackageStartupMessages({library(tidyverse); library(cowplot)})
source("scripts/utils/figure_style.R")
source("scripts/utils/figure_atlas.R")
source("scripts/utils/plot_contracts.R")
source("scripts/utils/analysis_design.R")
source("scripts/utils/artifact_validation.R")

# Fig. 6 reads joint RQ3 results only; Fig. 5 owns single-axis requirements.
JOINT_CSV <- file.path("results", "rq3", "rq3_joint_summary.csv")
JOINT_RDS <- file.path("results", "rq3", "rq3_joint_stability.rds")
COMPOSITION_CSV <- file.path("results", "rq3", "rq3_composition_failure_summary.csv")
OUT_DIR <- file.path("results", "rq3", "figures")
ms_plot_require_files(c(JOINT_CSV, JOINT_RDS, COMPOSITION_CSV),
                      "Frozen RQ3 joint plotting inputs")
joint <- readr::read_csv(JOINT_CSV, show_col_types = FALSE, progress = FALSE)
task_projection <- attr(readRDS(JOINT_RDS), "task_projection", exact = TRUE)
if (!identical(task_projection$task_projection_version, "rq3_task_projection_v2_min2day_is")) {
  stop("Frozen RQ3 task projection is missing; run RQ3 analysis before plotting", call. = FALSE)
}
composition_summary <- readr::read_csv(COMPOSITION_CSV, show_col_types = FALSE, progress = FALSE)
ms_plot_require_columns(joint,
  c("core_artifact_version", "rq1_analysis_version", "rq3_analysis_version",
    "placement", "optical", "resolution_s", "n_days", "metric", "metric_class",
    "R_obs", "status", "epsilon_entry"),
  "rq3_joint_summary.csv")
ms_plot_require_columns(composition_summary,
  c("summary_scope", "epsilon", "n_axis_pass", "n_failure"), "composition summary")
RQ1_VERSION <- ms_plot_one_version(joint$rq1_analysis_version, "rq1_analysis_version")
RQ3_VERSION <- ms_plot_one_version(joint$rq3_analysis_version, "rq3_analysis_version")
CORE_VERSION <- ms_plot_assert_core(joint$core_artifact_version)
ms_plot_assert_prefix(RQ1_VERSION, c("rq1_v6_", "rq1_v7_"), "rq1_analysis_version")
ms_plot_assert_prefix(RQ3_VERSION, "rq3_v10_", "rq3_analysis_version")
for (artifact in list(task_projection, composition_summary)) {
  ms_assert_version(artifact, "rq3_analysis_version", RQ3_VERSION)
  ms_assert_version(artifact, "rq1_analysis_version", RQ1_VERSION)
  ms_assert_version(artifact, "core_artifact_version", CORE_VERSION)
}
if (!grepl(ms_analysis_design_id(), RQ3_VERSION, fixed = TRUE)) {
  stop("RQ3 plotting inputs do not match the frozen analysis design", call. = FALSE)
}
stopifnot(setequal(unique(joint$resolution_s), ms_primary_temporal_s()),
          setequal(unique(joint$n_days), ms_primary_duration_days()))
NUMERIC_TOL <- 1e-12
fig5_res_levels <- rev(ms_primary_temporal_s())
source("scripts/utils/fig6_redesign.R")

# Retain the complete previously exported eye/MEDI decision slice for audit.
task_slice5 <- task_projection$frontiers |>
  filter(task_id %in% c("level", "temporal dynamics", "all_targets"),
         placement == "eye", optical == "MEDI",
         epsilon_interval_start <= .50 + NUMERIC_TOL,
         epsilon_interval_end > .50 + NUMERIC_TOL | terminal_endpoint) |>
  group_by(task_id, placement, optical, config_id) |>
  slice_max(epsilon_interval_start, n = 1, with_ties = FALSE) |> ungroup() |>
  mutate(resolution_rank = match(resolution_s, fig5_res_levels),
         task_id = factor(task_id, levels = c("level", "temporal dynamics", "all_targets")),
         # Display only: scheduled sample count relative to 10 s x 6 d.
         nominal_sample_reduction = (6 / 10) / (n_days / resolution_s))
task_slice5 <- ms_fig6_task_explanation(task_slice5, task_projection$inventory |>
                                        distinct(metric, metric_class))
if (!ms_plot_prep_only()) {
  readr::write_csv(
    task_slice5 |>
      select(task_id, n_required, resolution_s, n_days, status, sufficient, pareto,
             nominal_sample_reduction, R_task, limiting_targets, limiting_target_classes,
             frontier_limiting_targets, comparison_task, comparison_relation,
             required_days_at_interval, required_interval_at_days_s,
             extra_days_vs_comparison_at_interval, sampling_rate_ratio_vs_comparison_at_days,
             shorter_n_days, shorter_status, shorter_sufficient, shorter_R_task,
             shorter_blocking_targets, shorter_blocker_key,
             coarser_resolution_s, coarser_status, coarser_sufficient, coarser_R_task,
             coarser_blocking_targets, coarser_blocker_key),
    file.path("results", "rq3", "fig6b_eye_medi_epsilon_0.50_display_slice.csv"), na = ""
  )
}
portfolio <- ms_fig6_frontier_display(task_projection$frontiers, epsilon = .50)
fig6_display <- ms_fig6_redesign(task_projection$frontiers, composition_summary, joint)
ms_plot_save(fig6_display$plot, file.path(OUT_DIR, "Fig5_RQ3.png"), MS_FIG_WIDTH_IN, 8.1)
if (!ms_plot_prep_only()) {
  readr::write_csv(portfolio, "results/rq3/fig6_joint_design_states_display.csv", na = "")
  readr::write_csv(portfolio |> filter(pareto %in% TRUE), "results/rq3/fig6_joint_design_portfolio.csv", na = "")
  readr::write_csv(fig6_display$savings, "results/rq3/fig6_pareto_savings_by_tolerance.csv", na = "")
  readr::write_csv(fig6_display$allocations, "results/rq3/fig6_all_target_design_intervals.csv", na = "")
  readr::write_csv(fig6_display$failure, "results/rq3/fig6_composition_failure_profiles.csv", na = "")
  readr::write_csv(fig6_display$state_detail, "results/rq3/fig6_constraint_states_display.csv", na = "")
  readr::write_csv(fig6_display$metric_detail, "results/rq3/fig6_constraint_targets_display.csv", na = "")
}
ms_plot_write_manifest(file.path(OUT_DIR, "figure_artifact_manifest.csv"),
  tibble(figure = "Fig5_RQ3",
    input_artifact = "rq3_joint_summary+rq3_joint_stability.task_projection+rq3_composition_failure_summary",
    core_artifact_version = CORE_VERSION, rq1_analysis_version = RQ1_VERSION,
    rq2_analysis_version = NA_character_, rq3_analysis_version = RQ3_VERSION))
message("Fig. 6 complete: joint designs, tolerance profiles, target-level constraints and composition failures.")
