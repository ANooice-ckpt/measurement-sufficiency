# Explicit lightweight export from frozen daily inference and reliability fits.
# Does not refit models, resample participants, or modify any canonical artifact.
options(encoding = "UTF-8")
if (.Platform$OS.type == "windows") invisible(suppressWarnings(Sys.setlocale("LC_CTYPE", "English_United States.utf8")))
suppressPackageStartupMessages(library(data.table))
source("scripts/utils/analysis_design.R")
source("scripts/utils/core_artifacts.R", encoding = "UTF-8")
source("scripts/utils/artifact_validation.R")
source("scripts/utils/rq1_inference_contract.R")
source("scripts/utils/rq2_reliability_contract.R")
source("scripts/utils/continuous_diagnostics.R")

out <- "results/continuous_diagnostics"
inference_path <- file.path("results/rq1/inference", rq1_inference_contract()$artifact_filename)
summary_path <- "results/rq1/inference/rq1_inferential_preservation_summary.csv"
reliability_path <- "results/rq2/rq2_conditional_reliability.rds"
rq1_path <- "results/rq1/rq1_pairwise_change_long.rds"
required <- c(inference_path, summary_path, reliability_path, rq1_path)
if (!all(file.exists(required))) stop("Missing frozen inputs: ", paste(required[!file.exists(required)], collapse = ", "))
message("Read frozen daily inference and conditional reliability; no refitting")
a <- readRDS(inference_path)
ms_assert_version(a, "artifact_type", "rq1_inferential_preservation")
ms_assert_version(a, "core_artifact_version", core_artifact_version())
ms_assert_version(a, "analysis_design_id", ms_analysis_design_id())
if (!startsWith(a$rq1_inference_version, "rq1_inference_v8_geometry_anchor8__")) stop("Unsupported frozen inference geometry version")
upstream <- readRDS(rq1_path)
# Daily exports retain their actual original scientific version. A new duration
# producer version is not a reason to relabel existing daily fits as newly run.
ms_assert_version(a, "rq1_analysis_version", upstream$rq1_analysis_version)
z <- readRDS(reliability_path)
rq2_reliability_assert_artifact(z, upstream, core_artifact_version())
ms_assert_version(a, "rq1_analysis_version", z$provenance$rq1_analysis_version)
versions <- c("core_artifact_version", "rq1_analysis_version", "rq1_inference_version")
for (field in versions) {
  ms_assert_version(a$contrast_summary, field, a[[field]])
  ms_assert_version(a$term_summary, field, a[[field]])
  ms_assert_version(a$component_blocks, field, a[[field]])
}
csv <- fread(summary_path, na.strings = c("", "NA"))
frozen <- as.data.table(a$contrast_summary)
if (!isTRUE(all.equal(as.data.frame(csv), as.data.frame(frozen), check.attributes = FALSE, tolerance = 1e-12))) {
  stop("Inference summary CSV differs from the active frozen artifact")
}
# Resolve the CSV through the validated active artifact, never by latest-file or
# wildcard selection. Linux absolute run_dir provenance need not exist locally.
score_path <- file.path("results/rq2/reliability", z$provenance$rq1_analysis_version, z$run_id, "task_scores.csv")
if (!file.exists(score_path)) stop("Active reliability task_scores.csv missing: ", score_path)
scores <- fread(score_path, na.strings = c("", "NA")) # literal state 'null' is data
if (!isTRUE(all.equal(as.data.frame(scores), as.data.frame(z$task_scores), check.attributes = FALSE, tolerance = 1e-12))) {
  stop("Task score CSV differs from validated conditional-reliability artifact")
}

message("Derive paired/refixed signals from stored sufficient statistics")
signal <- continuous_signal_export(a$contrast_summary, a$term_summary, a$component_blocks)
risk <- continuous_risk_export(scores)
normalized <- c("refitted_difference_over_outcome_rms", "refitted_difference_over_reference_rms",
  "fixed_beta_within_difference_over_outcome_rms", "fixed_beta_within_difference_over_reference_rms", "signal_reference_r2")
native <- c("signal_reference_rms", "signal_candidate_rms", "signal_difference_rms", "outcome_within_rms",
  "fixed_beta_within_difference_rms")
outputs <- list(
  rq1_signal_tasks = signal,
  rq1_signal_by_outcome = continuous_signal_summary(signal, c("outcome", "outcome_domain", "outcome_unit"), c(native, normalized)),
  rq1_signal_by_domain = continuous_signal_summary(signal, "outcome_domain", normalized),
  rq1_signal_by_domain_geometry = continuous_signal_summary(signal, c("outcome_domain", "metric_geometry"), normalized),
  rq1_signal_by_contrast = continuous_signal_summary(signal, c("comparison_pair_id", "contrast_label"), normalized),
  rq2_continuous_risk_tasks = risk$task,
  rq2_continuous_risk_by_contrast = risk$contrast
)
for (name in names(outputs)) {
  d <- outputs[[name]]
  d[, core_artifact_version := a$core_artifact_version]
  d[, rq1_analysis_version := a$rq1_analysis_version]
  if (startsWith(name, "rq1_")) d[, rq1_inference_version := a$rq1_inference_version]
  else d[, `:=`(rq2_analysis_version = z$provenance$rq2_analysis_version, reliability_run_id = z$run_id)]
  d[, continuous_export_version := "continuous_diagnostics_v1_frozen_matched_within"]
  outputs[[name]] <- d
}
dir.create(out, recursive = TRUE, showWarnings = FALSE)
for (name in names(outputs)) fwrite(outputs[[name]], file.path(out, paste0(name, ".csv")), na = "NA")
source_paths <- c(required, score_path, "scripts/10c_export_continuous_diagnostics.R", "scripts/utils/continuous_diagnostics.R")
fwrite(data.table(path = source_paths, md5 = unname(tools::md5sum(source_paths))), file.path(out, "input_provenance.csv"))
fwrite(data.table(artifact = paste0(names(outputs), ".csv"), rows = vapply(outputs, nrow, integer(1))),
  file.path(out, "export_manifest.csv"))
writeLines(c(
  "Frozen continuous diagnostics; no model refit, participant bootstrap, or raw-source computation.",
  "Signal RMS uses the exact matched participant-demeaned design and original day weights.",
  "Refitted: W_c beta_c - W_r beta_r. Fixed beta: W_c beta_r - W_r beta_r.",
  "Fixed beta removes coefficient re-estimation, while participant demeaning still removes stable offsets.",
  "It is not total fixed-intercept prediction displacement, held-out prediction, or causal health bias.",
  "Circular targets use their complete sin/cos coefficient vector and cross-product matrices.",
  "Reference ratios are displacement ratios, not proportions of association signal lost.",
  "Zero reference RMS gives NA. Weak reference R2 < 0.001 is labelled descriptively and is not excluded.",
  "Native units are summarized within outcome only; normalized summaries retain optional geometry strata.",
  "Task quantiles describe heterogeneity, not confidence intervals or independent task replication.",
  "RQ2 target mean_risk is continuous |z|, separate from exceedance Brier probabilities.",
  "Losses average held-out records inside each target, then targets equally before computing baseline skill.",
  "All validation partitions and six information states are retained; repeat_id=1 is primary.",
  "The literal state null is the training-fold mean baseline. Zero baseline loss gives undefined skill.",
  "MSE skill is relative to training means, not conventional held-out R2 about the overall observed mean.",
  "Full-day context supports retrospective reliability; no prospective/adaptive-logging validation is implied.",
  "Original frozen scientific versions are retained. This export does not relabel daily fits after duration revisions."
), file.path(out, "README.txt"), useBytes = TRUE)
message("Wrote ", length(outputs), " CSVs to ", out, "; estimated signal tasks=", sum(signal$status == "estimated"),
  "; max reconstruction error=", format(max(signal$reconstruction_max_absolute_error, na.rm = TRUE), digits = 4))
