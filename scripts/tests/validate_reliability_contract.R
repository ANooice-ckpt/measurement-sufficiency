# Pure compatibility checks: no production fitting, plotting, or output writes.
source("scripts/utils/rq2_reliability_contract.R")

upstream <- list(rq1_analysis_version = "rq1_fixture", core_artifact_version = "core_fixture")
current <- list(complete = TRUE, provenance = list(
  rq2_analysis_version = rq2_reliability_version(),
  rq1_analysis_version = upstream$rq1_analysis_version,
  core_artifact_version = upstream$core_artifact_version,
  code_md5 = c("scripts/utils/rq2_risk_models.R" = "historical_source_hash")
))
check <- function(x, source = upstream, core = "core_fixture") {
  rq2_reliability_assert_artifact(x, source, core)
}
reject <- function(expr, text) {
  result <- tryCatch({force(expr); NULL}, error = conditionMessage)
  stopifnot(is.character(result), length(result) == 1L, grepl(text, result, fixed = TRUE))
}

stopifnot(identical(check(current), current),
  endsWith(rq2_reliability_version(), ms_analysis_design_id()))
# A source-only edit must not invalidate semantically compatible frozen results.
comment_edit <- current
comment_edit$provenance$code_md5[] <- "different_source_hash"
stopifnot(identical(check(comment_edit), comment_edit))

old <- current
old$provenance$rq2_analysis_version <- "conditional_reliability_v1"
reject(check(old), "estimator version")
# --summarize uses the same provenance guard, so old fits cannot be relabelled.
reject(rq2_reliability_assert_provenance(old$provenance, upstream, "core_fixture"), "estimator version")
missing <- current
missing$provenance$rq2_analysis_version <- NULL
reject(check(missing), "estimator version")
wrong_rq1 <- current
wrong_rq1$provenance$rq1_analysis_version <- "rq1_retired"
reject(check(wrong_rq1), "RQ1 version")
wrong_core <- current
wrong_core$provenance$core_artifact_version <- "core_retired"
reject(check(wrong_core), "core version")
reject(check(current, core = "core_new_semantics"), "upstream core version")
unfinished <- current
unfinished$complete <- FALSE
reject(check(unfinished), "incomplete")
cat("Conditional reliability estimator/upstream compatibility contracts passed\n")
