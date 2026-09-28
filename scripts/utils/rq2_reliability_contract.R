# Scientific compatibility for frozen conditional-reliability fits. Source-file
# hashes remain provenance; harmless code edits do not invalidate a frozen plot.
if (!exists("ms_analysis_design_id", mode = "function")) {
  source("scripts/utils/analysis_design.R")
}

rq2_reliability_version <- function() {
  # v2 requires unpenalized context projection and isotonic-before-clipping
  # probability projection. The numerical implementations live in risk_models.
  paste0("conditional_reliability_v2_orthogonal_bounded_projection__", ms_analysis_design_id())
}

rq2_reliability_assert_provenance <- function(provenance, upstream, expected_core) {
  assert_one <- function(value, expected, label) {
    if (!is.character(value) || length(value) != 1L || is.na(value) ||
        !nzchar(value) || !identical(value, expected)) {
      stop("Conditional reliability ", label, " is missing or incompatible; expected ",
        expected, ". Rebuild current upstream artifacts as needed, then run ",
        "Rscript scripts/12d_rq2_recovery.R --run.", call. = FALSE)
    }
  }
  assert_one(provenance$rq2_analysis_version, rq2_reliability_version(), "estimator version")
  assert_one(upstream$core_artifact_version, expected_core, "upstream core version")
  assert_one(provenance$core_artifact_version, expected_core, "core version")
  rq1 <- upstream$rq1_analysis_version
  if (!is.character(rq1) || length(rq1) != 1L || is.na(rq1) || !nzchar(rq1)) {
    stop("Current RQ1 manifest lacks one analysis version", call. = FALSE)
  }
  assert_one(provenance$rq1_analysis_version, rq1, "RQ1 version")
  invisible(provenance)
}

rq2_reliability_assert_artifact <- function(artifact, upstream, expected_core) {
  if (!is.list(artifact) || !isTRUE(artifact$complete) || !is.list(artifact$provenance)) {
    stop("Conditional reliability artifact is incomplete", call. = FALSE)
  }
  rq2_reliability_assert_provenance(artifact$provenance, upstream, expected_core)
  invisible(artifact)
}
