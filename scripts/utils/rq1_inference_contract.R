# Frozen design constants shared by RQ1 inferential-preservation analysis/plotting.
# This file contains no data access, fitting or plotting code.

rq1_inference_contract <- function() {
  list(
    reference_config = "eye__MEDI__10s",
    outcomes = c("sleep_quality", "awakenings", "awake_duration"),
    daily_metric_count = 52L,
    anchor_count = 8L,
    dual_channel_metrics = c("MDER", "nvRD"),
    artifact_filename = "rq1_inferential_preservation_anchor8.rds",
    legacy_artifact_filename = "rq1_inferential_preservation.rds"
  )
}
