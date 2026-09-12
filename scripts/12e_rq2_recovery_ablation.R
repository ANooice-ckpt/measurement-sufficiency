# Post-hoc information-ablation summaries for the frozen RQ2 recovery run.
#
# This script NEVER refits recovery models. It reads recovery_comparison.csv from
# an already completed 12d run and reorganizes the same held-out estimates as a
# nested information ablation:
#   raw -> self-calibration -> + low-measurement signature -> + context.
#
# Usage:
#   Rscript scripts/12e_rq2_recovery_ablation.R
#   Rscript scripts/12e_rq2_recovery_ablation.R <recovery_run_dir>
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
  library(tibble)
})

ablation_require <- function(x, columns, label) {
  missing <- setdiff(columns, names(x))
  if (length(missing)) stop(label, " missing columns: ", paste(missing, collapse = ", "), call. = FALSE)
  invisible(x)
}

ablation_safe_mean <- function(x) {
  x <- as.numeric(x); x <- x[is.finite(x)]
  if (length(x)) mean(x) else NA_real_
}
ablation_safe_median <- function(x) {
  x <- as.numeric(x); x <- x[is.finite(x)]
  if (length(x)) median(x) else NA_real_
}
ablation_safe_quantile <- function(x, p) {
  x <- as.numeric(x); x <- x[is.finite(x)]
  if (length(x)) unname(quantile(x, p, names = FALSE, type = 8)) else NA_real_
}
ablation_fraction_positive <- function(x) {
  x <- as.numeric(x); x <- x[is.finite(x)]
  if (length(x)) mean(x > 0) else NA_real_
}

ablation_resolve_run <- function(run_dir = NULL) {
  if (is.null(run_dir) || !nzchar(run_dir)) run_dir <- Sys.getenv("RQ2_RECOVERY_RUN_DIR", "")
  if (nzchar(run_dir)) {
    run_dir <- normalizePath(run_dir, winslash = "/", mustWork = TRUE)
    if (!file.exists(file.path(run_dir, "recovery_comparison.csv")))
      stop("Recovery run lacks recovery_comparison.csv: ", run_dir, call. = FALSE)
    return(run_dir)
  }

  manifests <- Sys.glob(file.path("results", "rq2", "recovery", "*", "*", "recovery_manifest.rds"))
  if (!length(manifests)) stop("No completed recovery run found; set RQ2_RECOVERY_RUN_DIR", call. = FALSE)
  complete <- vapply(manifests, function(path) {
    tryCatch(isTRUE(readRDS(path)$complete), error = function(e) FALSE)
  }, logical(1))
  manifests <- manifests[complete]
  if (!length(manifests)) stop("No complete recovery manifest found; set RQ2_RECOVERY_RUN_DIR", call. = FALSE)
  info <- file.info(manifests)
  dirname(manifests[[which.max(info$mtime)]])
}

ablation_metric_decomposition <- function(comparison) {
  ablation_require(comparison,
    c("learner", "task_index", "dimension", "comparison_pair_id", "candidate_config",
      "support_id", "metric", "metric_class", "metric_geometry", "state", "A",
      "A_raw", "A_calibration", "A_signature"),
    "recovery_comparison.csv")

  out <- comparison |>
    filter(state == "context") |>
    transmute(
      learner, task_index, dimension, comparison_pair_id, candidate_config, support_id,
      metric, metric_class, metric_geometry,
      A_raw = as.numeric(A_raw),
      A_calibration = as.numeric(A_calibration),
      A_signature = as.numeric(A_signature),
      A_context = as.numeric(A),
      calibration_increment = A_raw - A_calibration,
      signature_increment = A_calibration - A_signature,
      context_increment = A_signature - A_context,
      total_increment = A_raw - A_context,
      unrecovered_residual = A_context
    )

  if (!nrow(out)) stop("No context-state rows in recovery_comparison.csv", call. = FALSE)
  if (any(!is.finite(out$A_raw) | !is.finite(out$A_calibration) |
          !is.finite(out$A_signature) | !is.finite(out$A_context)))
    stop("Non-finite stage A in completed recovery comparison", call. = FALSE)
  key <- c("learner", "task_index")
  if (nrow(out) != nrow(distinct(out, across(all_of(key)))))
    stop("Context rows are not unique by learner/task_index", call. = FALSE)
  reconstruction <- with(out,
    A_raw - (calibration_increment + signature_increment + context_increment + unrecovered_residual))
  if (max(abs(reconstruction)) > 1e-10)
    stop("Stage decomposition does not reconstruct raw A", call. = FALSE)
  out
}

ablation_dominant_layer <- function(calibration, signature, context) {
  value <- c(calibration = calibration, signature = signature, context = context)
  value[!is.finite(value)] <- -Inf
  if (!length(value) || max(value) <= 0) return("none")
  names(which.max(value))
}

ablation_group_summary <- function(metric_level, groups) {
  metric_level |>
    group_by(across(all_of(groups))) |>
    summarise(
      n_tasks = n(), n_unique_metrics = n_distinct(metric),
      mean_A_raw = ablation_safe_mean(A_raw),
      mean_A_calibration = ablation_safe_mean(A_calibration),
      mean_A_signature = ablation_safe_mean(A_signature),
      mean_A_context = ablation_safe_mean(A_context),
      median_A_raw = ablation_safe_median(A_raw),
      median_A_calibration = ablation_safe_median(A_calibration),
      median_A_signature = ablation_safe_median(A_signature),
      median_A_context = ablation_safe_median(A_context),
      mean_calibration_increment = ablation_safe_mean(calibration_increment),
      mean_signature_increment = ablation_safe_mean(signature_increment),
      mean_context_increment = ablation_safe_mean(context_increment),
      mean_total_increment = ablation_safe_mean(total_increment),
      median_calibration_increment = ablation_safe_median(calibration_increment),
      median_signature_increment = ablation_safe_median(signature_increment),
      median_context_increment = ablation_safe_median(context_increment),
      median_total_increment = ablation_safe_median(total_increment),
      q25_calibration_increment = ablation_safe_quantile(calibration_increment, .25),
      q75_calibration_increment = ablation_safe_quantile(calibration_increment, .75),
      q25_signature_increment = ablation_safe_quantile(signature_increment, .25),
      q75_signature_increment = ablation_safe_quantile(signature_increment, .75),
      q25_context_increment = ablation_safe_quantile(context_increment, .25),
      q75_context_increment = ablation_safe_quantile(context_increment, .75),
      fraction_calibration_improved = ablation_fraction_positive(calibration_increment),
      fraction_signature_improved = ablation_fraction_positive(signature_increment),
      fraction_context_improved = ablation_fraction_positive(context_increment),
      fraction_final_improved = ablation_fraction_positive(total_increment),
      .groups = "drop"
    ) |>
    rowwise() |>
    mutate(
      dominant_recovery_layer = ablation_dominant_layer(
        mean_calibration_increment, mean_signature_increment, mean_context_increment),
      mean_unrecovered_fraction = if_else(
        is.finite(mean_A_raw) & mean_A_raw > 0, mean_A_context / mean_A_raw, NA_real_)
    ) |>
    ungroup()
}

ablation_atlas_long <- function(atlas) {
  bind_rows(
    atlas |>
      transmute(across(c(learner, dimension, comparison_pair_id, metric_class, n_tasks, n_unique_metrics)),
        stage = "self-calibration", stage_order = 1L,
        mean_increment = mean_calibration_increment,
        median_increment = median_calibration_increment,
        q25_increment = q25_calibration_increment, q75_increment = q75_calibration_increment,
        fraction_improved = fraction_calibration_improved),
    atlas |>
      transmute(across(c(learner, dimension, comparison_pair_id, metric_class, n_tasks, n_unique_metrics)),
        stage = "+ measurement signature", stage_order = 2L,
        mean_increment = mean_signature_increment,
        median_increment = median_signature_increment,
        q25_increment = q25_signature_increment, q75_increment = q75_signature_increment,
        fraction_improved = fraction_signature_improved),
    atlas |>
      transmute(across(c(learner, dimension, comparison_pair_id, metric_class, n_tasks, n_unique_metrics)),
        stage = "+ context", stage_order = 3L,
        mean_increment = mean_context_increment,
        median_increment = median_context_increment,
        q25_increment = q25_context_increment, q75_increment = q75_context_increment,
        fraction_improved = fraction_context_improved)
  ) |>
    arrange(learner, stage_order, dimension, comparison_pair_id, metric_class)
}

ablation_decoder_capacity <- function(atlas) {
  keys <- c("dimension", "comparison_pair_id", "metric_class")
  keep <- c(keys, "n_tasks", "n_unique_metrics", "dominant_recovery_layer",
    "mean_calibration_increment", "mean_signature_increment", "mean_context_increment",
    "fraction_calibration_improved", "fraction_signature_improved", "fraction_context_improved")
  xgb <- atlas |> filter(learner == "xgboost") |> select(all_of(keep))
  ridge <- atlas |> filter(learner == "ridge") |> select(all_of(keep))
  names(xgb)[!names(xgb) %in% keys] <- paste0("xgb_", names(xgb)[!names(xgb) %in% keys])
  names(ridge)[!names(ridge) %in% keys] <- paste0("ridge_", names(ridge)[!names(ridge) %in% keys])
  full_join(xgb, ridge, by = keys) |>
    mutate(
      xgb_minus_ridge_calibration_increment = xgb_mean_calibration_increment - ridge_mean_calibration_increment,
      xgb_minus_ridge_signature_increment = xgb_mean_signature_increment - ridge_mean_signature_increment,
      xgb_minus_ridge_context_increment = xgb_mean_context_increment - ridge_mean_context_increment,
      dominant_layer_concordant = xgb_dominant_recovery_layer == ridge_dominant_recovery_layer
    )
}

recovery_ablation_summarize <- function(run_dir = NULL) {
  run_dir <- ablation_resolve_run(run_dir)
  comparison_path <- file.path(run_dir, "recovery_comparison.csv")
  comparison <- read_csv(comparison_path, show_col_types = FALSE, progress = FALSE)
  metric_level <- ablation_metric_decomposition(comparison)

  atlas <- ablation_group_summary(metric_level,
    c("learner", "dimension", "comparison_pair_id", "metric_class"))
  by_dimension <- ablation_group_summary(metric_level, c("learner", "dimension"))
  by_metric_class <- ablation_group_summary(metric_level, c("learner", "metric_class"))
  overall <- ablation_group_summary(metric_level, c("learner"))
  atlas_long <- ablation_atlas_long(atlas)
  decoder <- ablation_decoder_capacity(atlas)

  out <- file.path(run_dir, "ablation")
  dir.create(out, recursive = TRUE, showWarnings = FALSE)
  write_csv(metric_level |> arrange(learner, dimension, comparison_pair_id, metric_class, metric),
    file.path(out, "recovery_metric_decomposition.csv"))
  write_csv(atlas, file.path(out, "recovery_information_atlas.csv"))
  write_csv(atlas_long, file.path(out, "recovery_information_atlas_long.csv"))
  write_csv(by_dimension, file.path(out, "recovery_ablation_by_dimension.csv"))
  write_csv(by_metric_class, file.path(out, "recovery_ablation_by_metric_class.csv"))
  write_csv(overall, file.path(out, "recovery_ablation_overall.csv"))
  write_csv(decoder, file.path(out, "recovery_decoder_capacity_atlas.csv"))
  write_csv(metric_level |> filter(learner == "xgboost") |> arrange(desc(context_increment)),
    file.path(out, "context_gain_ranked.csv"))

  message("Recovery information-ablation outputs: ", out)
  message("Primary XGBoost summary by degradation dimension:")
  print(by_dimension |> filter(learner == "xgboost") |>
    select(dimension, n_tasks, mean_calibration_increment, mean_signature_increment,
      mean_context_increment, fraction_context_improved, mean_A_context))
  message("Largest XGBoost context-assisted cells (descriptive; no selection was used in fitting):")
  print(atlas |> filter(learner == "xgboost") |>
    arrange(desc(mean_context_increment)) |>
    select(dimension, comparison_pair_id, metric_class, n_tasks,
      mean_context_increment, fraction_context_improved) |> slice_head(n = 12L))
  invisible(out)
}

if (sys.nframe() == 0L) {
  args <- commandArgs(trailingOnly = TRUE)
  if (length(args) > 1L) stop("Usage: Rscript scripts/12e_rq2_recovery_ablation.R [recovery_run_dir]", call. = FALSE)
  recovery_ablation_summarize(if (length(args)) args[[1]] else NULL)
}
