# Frozen design constants shared by RQ1 inferential-preservation analysis/plotting.
# This file contains no data access, fitting or plotting code.

rq1_inference_contract <- function() {
  outcomes <- c(
    "sleep_quality", "awakenings", "awake_duration",
    "kss", "positive_affect", "negative_affect"
  )
  list(
    reference_config = "eye__MEDI__10s",
    outcomes = outcomes,
    outcome_domain = c(
      sleep_quality = "Sleep", awakenings = "Sleep", awake_duration = "Sleep",
      kss = "Alertness",
      positive_affect = "Affect", negative_affect = "Affect"
    )[outcomes],
    outcome_label = c(
      sleep_quality = "Sleep quality", awakenings = "Awakenings", awake_duration = "Awake duration",
      kss = "Sleepiness (KSS)", positive_affect = "Positive affect", negative_affect = "Negative affect"
    )[outcomes],
    daily_metric_count = 52L,
    anchor_count = 8L,
    dual_channel_metrics = c("MDER", "nvRD"),
    ema_slots_h = c(11, 14, 17, 20),
    ema_slot_tolerance_min = 120L,
    ema_min_slots = 2L,
    artifact_filename = "rq1_inferential_preservation_domains_anchor8.rds",
    retired_artifact_filenames = c(
      "rq1_inferential_preservation_anchor8.rds",
      "rq1_inferential_preservation.rds"
    )
  )
}
