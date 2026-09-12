# Synthetic checks for scripts/12e_rq2_recovery_ablation.R.
# No formal recovery artifact is read.
source("scripts/12e_rq2_recovery_ablation.R")

states <- c("calibration", "signature", "context")
meta <- tibble::tibble(
  learner = rep(c("xgboost", "ridge"), each = 6L),
  task_index = seq_len(12L),
  dimension = rep(c("placement", "temporal"), 6L),
  comparison_pair_id = rep(c("wrist_vs_eye", "120s_vs_10s"), 6L),
  candidate_config = "synthetic_low", support_id = "synthetic_support",
  metric = paste0("m", rep(1:6, 2)),
  metric_class = rep(c("level", "duration", "timing"), 4L),
  metric_geometry = "linear"
)

comparison <- tidyr::crossing(meta, state = states) |>
  dplyr::mutate(
    A_raw = 1,
    A_calibration = if_else(learner == "xgboost", .70, .75),
    A_signature = if_else(learner == "xgboost", .60, .68),
    A = dplyr::case_when(
      state == "calibration" ~ A_calibration,
      state == "signature" ~ A_signature,
      state == "context" & metric_class == "level" ~ A_signature - .10,
      state == "context" ~ A_signature + .02
    )
  )

metric <- ablation_metric_decomposition(comparison)
stopifnot(nrow(metric) == nrow(meta))
stopifnot(all(abs(metric$calibration_increment - ifelse(metric$learner == "xgboost", .30, .25)) < 1e-12))
stopifnot(all(metric$context_increment[metric$metric_class == "level"] > 0))
stopifnot(all(metric$context_increment[metric$metric_class != "level"] < 0))
stopifnot(max(abs(with(metric,
  A_raw - (calibration_increment + signature_increment + context_increment + unrecovered_residual)))) < 1e-12)

atlas <- ablation_group_summary(metric,
  c("learner", "dimension", "comparison_pair_id", "metric_class"))
stopifnot(all(atlas$n_tasks > 0), all(atlas$n_unique_metrics > 0))
stopifnot(all(atlas$fraction_context_improved[atlas$metric_class == "level"] == 1))
stopifnot(all(atlas$fraction_context_improved[atlas$metric_class != "level"] == 0))

long <- ablation_atlas_long(atlas)
stopifnot(setequal(unique(long$stage), c("self-calibration", "+ measurement signature", "+ context")))
stopifnot(nrow(long) == 3L * nrow(atlas))

decoder <- ablation_decoder_capacity(atlas)
stopifnot(all(c("xgb_minus_ridge_calibration_increment",
  "xgb_minus_ridge_signature_increment", "xgb_minus_ridge_context_increment") %in% names(decoder)))
stopifnot(all(decoder$xgb_minus_ridge_calibration_increment > 0))

cat("PASS: recovery information-ablation synthetic checks.\n")
