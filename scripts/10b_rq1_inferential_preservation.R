# Independent downstream extension; never sources/reruns 10_rq1_analysis.R.
# Run: RQ1_INFERENCE_BOOT=1000 Rscript scripts/10b_rq1_inferential_preservation.R
# Plot: Rscript scripts/11b_plot_fig2.R
#
# Analysis contract (additive to STUDY_SPEC; existing RQ1-RQ3 estimands remain unchanged):
# - 52 participant-day metrics and exactly eight single-axis anchor contrasts:
#   chest/wrist vs eye, LIGHT vs MEDI, and 20/30/40/60/120 s vs 10 s.
#   No multiday metrics, duration windows, reserve 300-s state, or multi-axis
#   configuration combinations enter this downstream consequence layer.
# - Candidate/reference exposure values and pair-specific support are read directly
#   from the frozen RQ1 pairwise artifact; Core is not reopened or recomputed.
#   The exposure distortion used as the upstream explanatory quantity is likewise
#   joined from the frozen RQ1 pairwise summary for the same metric/contrast.
#   Matched-support distortion is retained only as an audit quantity.
# - Each outcome gets identical candidate/reference complete cases, retaining
#   participants with >=2 matched days. Site is absorbed by participant fixed
#   effects; participants are keyed by site + Id, not treated as iid days.
# - Calendar exposure D -> local wake date D+1. This is a descriptive association
#   comparison, not a causal health effect or a reconstruction of sleep exposure.
# - Quality labels map explicitly to 1=Very poor ... 5=Very good; awakenings are
#   counts; awake duration is minutes. Linear mean projections for all outcomes
#   deliberately avoid outcome-specific likelihoods in this preservation test.
# - Linear exposures share the matched reference SD. Circular exposures enter
#   jointly as sin/cos. Inferential deviation is expressed in reference-bootstrap
#   uncertainty units: |delta beta|/SE_ref for linear metrics and the equivalent
#   two-parameter Mahalanobis norm for circular-time metrics.
# - Coefficient intervals use the SAME participant bootstrap draw for reference
#   and candidate fits, stratified by site. Intervals are pointwise and do not
#   establish equivalence or adjust for multiplicity.
# - A canonical eye/MEDI/10-s reference association landscape is recovered from
#   the frozen 20-s-vs-10-s RQ1 anchor, which already uses the canonical eye-only
#   metric-specific support.
suppressPackageStartupMessages(library(tidyverse))
source("scripts/utils/paths.R")
source("scripts/utils/melidos_io.R")
source("scripts/utils/core_artifacts.R")
source("scripts/utils/rq1_pairwise_artifacts.R")
source("scripts/utils/rq1_inference.R")

rq1_expand_sleep_support <- function(pairs, outcomes) {
  outcome_names <- rq1_inference_contract()$outcomes
  joined <- pairs |>
    left_join(outcomes, by = c("site", "Id", "Date"), relationship = "many-to-many") |>
    mutate(outcome_reason = if_else(is.na(outcome), "no_next_morning_diary", outcome_reason))
  missing <- joined |> filter(is.na(outcome)) |> select(-outcome)
  bind_rows(
    joined |> filter(!is.na(outcome)),
    tidyr::crossing(missing, outcome = outcome_names)
  )
}

rq1_fit_inference_groups <- function(pairs, keys, B, seed_base) {
  groups <- pairs |> group_by(across(all_of(keys))) |> group_split(.keep = TRUE)
  if (!length(groups)) stop("No daily exposure/reference comparisons")
  results <- vector("list", length(groups))
  for (i in seq_along(groups)) {
    g <- groups[[i]]
    meta <- g |> select(all_of(keys)) |> distinct()
    if (nrow(meta) != 1L) stop("Inference grouping metadata is not unique")
    fit <- rq1_inference_fit(g, B = B, seed = seed_base + i)
    results[[i]] <- lapply(
      fit,
      function(x) if (nrow(x)) bind_cols(meta[rep(1L, nrow(x)), , drop = FALSE], x) else x
    )
    if (i %% 100L == 0L) message("RQ1 inference tasks: ", i, "/", length(groups))
  }
  results
}

rq1_run_inference <- function() {
  contract <- rq1_inference_contract()
  rq1_path <- file.path(rq_root("rq1"), "rq1_pairwise_change_long.rds")
  rq1_summary_path <- file.path(rq_root("rq1"), "rq1_pairwise_summary.csv")
  for (p in c(rq1_path, rq1_summary_path)) {
    if (!file.exists(p)) stop("Missing frozen input: ", p)
  }

  upstream <- readRDS(rq1_path)
  rq1_version <- rq1_pairwise_version(upstream)
  ms_assert_version(upstream, "core_artifact_version", core_artifact_version())
  ms_assert_version(upstream, "analysis_design_id", ms_analysis_design_id())
  pair_part_paths <- rq1_pairwise_part_paths(upstream)
  if (rq1_pairwise_is_partitioned(upstream) &&
      (!length(pair_part_paths) || any(!file.exists(pair_part_paths)))) {
    stop("Frozen RQ1 pairwise parts are missing; cannot recover participant-day anchor values")
  }

  version <- rq1_inference_version(rq1_version)
  B <- suppressWarnings(as.integer(Sys.getenv("RQ1_INFERENCE_BOOT", "1000")))
  if (length(B) != 1L || !is.finite(B) || B < 0L) {
    stop("RQ1_INFERENCE_BOOT must be a nonnegative integer")
  }

  rq1_summary <- readr::read_csv(rq1_summary_path, show_col_types = FALSE, progress = FALSE)
  ms_assert_version(rq1_summary, "core_artifact_version", core_artifact_version())
  ms_assert_version(rq1_summary, "rq1_analysis_version", rq1_version)
  required_rq1 <- c("dimension", "comparison_pair_id", "metric", "A_mean_absolute", "B_mean_signed")
  if (!all(required_rq1 %in% names(rq1_summary))) {
    stop("rq1_pairwise_summary.csv lacks required inferential-link columns")
  }

  anchor_map <- rq1_inference_anchor_map()
  ms_assert_unique(anchor_map, "candidate_config", "RQ1 inference anchor map")
  if (nrow(anchor_map) != contract$anchor_count) {
    stop("RQ1 inferential preservation must contain exactly ", contract$anchor_count, " anchor contrasts")
  }

  base_pairs <- rq1_inference_pairs(upstream)
  if (n_distinct(base_pairs$metric) != contract$daily_metric_count) {
    stop("Frozen RQ1 anchor pairs do not span all ", contract$daily_metric_count, " participant-day metrics")
  }
  if (n_distinct(base_pairs$candidate_config) != contract$anchor_count) {
    stop("Frozen RQ1 anchor pairs do not contain all ", contract$anchor_count, " inferential contrasts")
  }

  sites <- sort(unique(base_pairs$site))
  diary_paths <- vapply(sites, raw_data_path, character(1), modality = "sleepdiaries")
  if (any(!file.exists(diary_paths))) stop("Missing harmonized sleep diaries")
  outcomes <- map_dfr(
    sites,
    function(s) rq1_sleep_outcomes(
      load_raw_file(raw_data_path(s, "sleepdiaries"), "sleepdiaries"), s
    )
  )
  if (!setequal(unique(outcomes$outcome), contract$outcomes)) {
    stop("Harmonized sleep outcomes do not match the frozen inferential-preservation contract")
  }

  pairs <- base_pairs |> rq1_expand_sleep_support(outcomes)
  contrast_keys <- c(
    "candidate_config", "support_id", "placement", "optical", "resolution_s",
    "dimension", "comparison_pair_id", "contrast_label", "contrast_order",
    "metric", "metric_class", "metric_geometry", "outcome"
  )
  contrast_results <- rq1_fit_inference_groups(pairs, contrast_keys, B, 20260911L)

  reference_pairs <- rq1_inference_reference_pairs(base_pairs) |> rq1_expand_sleep_support(outcomes)
  if (n_distinct(reference_pairs$metric) != contract$daily_metric_count) {
    stop("Reference association pairing does not span all participant-day metrics")
  }
  reference_keys <- c(
    "candidate_config", "support_id", "metric", "metric_class", "metric_geometry", "outcome"
  )
  reference_results <- rq1_fit_inference_groups(reference_pairs, reference_keys, B, 20270911L)

  stamp <- function(x) {
    if (!nrow(x)) return(x)
    mutate(
      x,
      core_artifact_version = core_artifact_version(),
      rq1_analysis_version = rq1_version,
      rq1_inference_version = version
    )
  }
  collect <- function(results, component) stamp(bind_rows(lapply(results, `[[`, component)))

  term_summary <- collect(contrast_results, "summary")
  contrast_summary <- collect(contrast_results, "task_summary")
  support <- collect(contrast_results, "support")
  bootstrap <- collect(contrast_results, "bootstrap")
  reference_term_summary <- collect(reference_results, "summary")
  reference_summary <- collect(reference_results, "task_summary")
  reference_support <- collect(reference_results, "support")
  reference_bootstrap <- collect(reference_results, "bootstrap")

  expected_contrast_tasks <- contract$anchor_count * contract$daily_metric_count * length(contract$outcomes)
  expected_reference_tasks <- contract$daily_metric_count * length(contract$outcomes)
  if (nrow(contrast_summary) != expected_contrast_tasks) {
    stop("Expected ", expected_contrast_tasks, " metric-outcome-contrast tasks; found ", nrow(contrast_summary))
  }
  if (nrow(reference_summary) != expected_reference_tasks) {
    stop("Expected ", expected_reference_tasks, " reference metric-outcome tasks; found ", nrow(reference_summary))
  }

  rq1_lookup <- anchor_map |>
    select(candidate_config, dimension, comparison_pair_id, contrast_label, contrast_order) |>
    inner_join(
      rq1_summary |>
        select(dimension, comparison_pair_id, metric,
               rq1_distortion_A = A_mean_absolute, rq1_distortion_B = B_mean_signed),
      by = c("dimension", "comparison_pair_id"), relationship = "many-to-many"
    ) |>
    mutate(
      rq1_direction_ratio = if_else(
        is.finite(rq1_distortion_A) & rq1_distortion_A > sqrt(.Machine$double.eps),
        rq1_distortion_B / rq1_distortion_A,
        if_else(is.finite(rq1_distortion_A) & abs(rq1_distortion_A) <= sqrt(.Machine$double.eps), 0, NA_real_)
      )
    )
  ms_assert_unique(rq1_lookup, c("candidate_config", "metric"), "RQ1 inference distortion lookup")

  contrast_summary <- contrast_summary |>
    left_join(
      rq1_lookup |>
        select(candidate_config, metric, rq1_distortion_A, rq1_distortion_B, rq1_direction_ratio),
      by = c("candidate_config", "metric"), relationship = "many-to-one"
    )
  estimable_missing_rq1 <- contrast_summary |>
    filter(is.finite(reference_association_strength), is.finite(candidate_association_strength),
           !is.finite(rq1_distortion_A))
  if (nrow(estimable_missing_rq1)) {
    stop("Estimable downstream tasks are missing their frozen RQ1 distortion: ", nrow(estimable_missing_rq1))
  }

  out <- file.path(rq_root("rq1"), "inference")
  ensure_result_dirs(out)
  provenance_paths <- unique(c(rq1_path, pair_part_paths, rq1_summary_path, diary_paths))
  artifact <- list(
    artifact_type = "rq1_inferential_preservation",
    core_artifact_version = core_artifact_version(),
    rq1_analysis_version = rq1_version,
    rq1_inference_version = version,
    analysis_design_id = ms_analysis_design_id(),
    bootstrap_replicates = B,
    bootstrap_seed_base = 20260911L,
    reference_bootstrap_seed_base = 20270911L,
    date_alignment = "exposure Date D -> local wake date D+1",
    model = "participant fixed effects; native outcome linear projection; paired site-stratified participant bootstrap",
    inference_deviation = "reference-bootstrap uncertainty norm: absolute/SE for linear; Mahalanobis norm for circular sin/cos",
    analysis_scope = "eight single-axis frozen RQ1 anchor contrasts; duration and multi-axis combinations excluded",
    exposure_input = "frozen RQ1 participant-day pair values (state_a candidate; state_b eye/MEDI/10-s reference)",
    scale = "reference SD on matched repeated-measures support, fixed across paired bootstrap draws; circular sin/cos unscaled",
    input_provenance = tibble(
      path = provenance_paths,
      md5 = unname(tools::md5sum(provenance_paths))
    ),
    anchor_map = anchor_map,
    rq1_distortion_lookup = stamp(rq1_lookup),
    summary = term_summary,
    term_summary = term_summary,
    contrast_summary = contrast_summary,
    reference_term_summary = reference_term_summary,
    reference_summary = reference_summary,
    support = support,
    reference_support = reference_support,
    bootstrap = bootstrap,
    reference_bootstrap = reference_bootstrap,
    pair_audit = stamp(pairs),
    reference_pair_audit = stamp(reference_pairs),
    diary_audit = outcomes
  )

  legacy_path <- file.path(out, contract$legacy_artifact_filename)
  unlink(c(legacy_path, paste0(legacy_path, ".ok")), force = TRUE)
  path <- file.path(out, contract$artifact_filename)
  rq1_write_part_atomic(artifact, path)
  readr::write_csv(artifact$contrast_summary,
                   file.path(out, "rq1_inferential_preservation_summary.csv"), na = "")
  readr::write_csv(artifact$term_summary,
                   file.path(out, "rq1_inferential_preservation_term_summary.csv"), na = "")
  readr::write_csv(artifact$reference_summary,
                   file.path(out, "rq1_reference_association_summary.csv"), na = "")
  message("RQ1 inferential preservation frozen: ", path)
  invisible(artifact)
}

if (sys.nframe() == 0L) rq1_run_inference()
