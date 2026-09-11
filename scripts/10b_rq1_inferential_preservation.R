# Independent downstream extension; never sources/reruns 10_rq1_analysis.R.
# Run: RQ1_INFERENCE_WORKERS=40 RQ1_INFERENCE_BOOT=1000 Rscript scripts/10b_rq1_inferential_preservation.R
# Plot: Rscript scripts/11b_plot_fig2.R
#
# Analysis contract (additive to STUDY_SPEC; existing RQ1-RQ3 estimands remain unchanged):
# - 52 participant-day metrics and exactly eight single-axis anchor contrasts:
#   chest/wrist vs eye, LIGHT vs MEDI, and 20/30/40/60/120 s vs 10 s.
#   No multiday metrics, duration windows, reserve 300-s state, or multi-axis
#   configuration combinations enter this downstream consequence layer.
# - Candidate/reference exposure values and pair-specific support are read directly
#   from the frozen RQ1 pairwise artifact; Core is not reopened or recomputed.
#   Frozen RQ1 representation distortion is joined for the same metric/contrast;
#   matched-support distortion is retained only as an audit quantity.
# - Downstream outcomes span three day-level human-state domains: Sleep
#   (quality, awakenings, awake duration), Alertness (daily KSS), and Affect
#   (daily positive and negative MoodZoom composites).
# - Sleep pairs exposure day D with the following morning diary. EMA responses are
#   reduced to the protocol's nominal 11/14/17/20 h slots, retaining the nearest
#   response within the frozen tolerance and requiring >=2 valid slots/day.
# - Each outcome gets identical candidate/reference complete cases, retaining
#   participants with >=2 matched days. Site is absorbed by participant fixed
#   effects; participants are keyed by site + Id, not treated as iid days.
# - All models are descriptive association-preservation tests, not causal health
#   effects or temporally resolved acute-response models.
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
source("scripts/utils/parallel_runtime.R")
source("scripts/utils/rq1_pairwise_artifacts.R")
source("scripts/utils/rq1_inference.R")

rq1_expand_outcome_support <- function(pairs, outcomes) {
  contract <- rq1_inference_contract()
  ms_assert_unique(outcomes, c("site", "Id", "Date", "outcome"), "RQ1 downstream outcomes")
  pairs |>
    tidyr::crossing(outcome = contract$outcomes) |>
    left_join(
      outcomes |>
        select(site, Id, Date, outcome, outcome_value, outcome_reason,
               outcome_n_observations, outcome_source),
      by = c("site", "Id", "Date", "outcome"), relationship = "many-to-one"
    ) |>
    mutate(
      outcome_reason = if_else(is.na(outcome_source), "outcome_not_observed", outcome_reason),
      outcome_domain = unname(contract$outcome_domain[outcome]),
      outcome_label = unname(contract$outcome_label[outcome]),
      outcome_n_observations = coalesce(outcome_n_observations, 0L)
    )
}

# One group is one independent metric x outcome x contrast/reference task. Keep
# the worker payload self-contained so PSOCK workers do not receive the complete
# grouped object or depend on scheduling order. Each task keeps its deterministic
# seed, so serial and parallel execution have identical bootstrap RNG semantics.
rq1_fit_inference_group_task <- function(task) {
  g <- task$group
  meta <- dplyr::distinct(dplyr::select(g, dplyr::all_of(task$keys)))
  if (nrow(meta) != 1L) stop("Inference grouping metadata is not unique")
  fit <- rq1_inference_fit(g, B = task$B, seed = task$seed)
  lapply(
    fit,
    function(x) {
      if (!nrow(x)) return(x)
      dplyr::bind_cols(meta[rep(1L, nrow(x)), , drop = FALSE], x)
    }
  )
}

rq1_fit_inference_groups <- function(pairs, keys, B, seed_base, workers) {
  groups <- pairs |> group_by(across(all_of(keys))) |> group_split(.keep = TRUE)
  if (!length(groups)) stop("No daily exposure/reference comparisons")
  tasks <- Map(
    function(g, i) list(group = g, keys = keys, B = B, seed = seed_base + i),
    groups, seq_along(groups)
  )
  rm(groups)
  invisible(gc(FALSE))
  active_workers <- min(as.integer(workers), length(tasks))
  message("RQ1 inference: ", length(tasks), " tasks across ", active_workers, " PSOCK workers")
  results <- ms_parallel_map(
    tasks,
    rq1_fit_inference_group_task,
    workers = active_workers,
    packages = c("dplyr", "tibble"),
    exports = c(
      "rq1_fit_inference_group_task", "rq1_inference_fit", "rq1_inference_stats",
      "rq1_inference_solve", "rq1_inference_solve_draws", "rq1_inference_quadnorm"
    )
  )
  message("RQ1 inference: completed ", length(results), " tasks")
  results
}

rq1_resolve_inference_workers <- function() {
  requested <- suppressWarnings(as.integer(Sys.getenv("RQ1_INFERENCE_WORKERS", unset = "40")))
  if (length(requested) != 1L || !is.finite(requested) || requested < 1L) requested <- 40L
  logical_cores <- suppressWarnings(parallel::detectCores(logical = TRUE))
  if (!is.finite(logical_cores) || logical_cores < 1L) logical_cores <- requested
  max(1L, min(requested, as.integer(logical_cores), 48L))
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

  # The downstream layer uses placement/optical/temporal participant-day pairs
  # only. When the canonical RQ1 artifact is partitioned, load and hash exactly
  # that immutable non-duration part instead of decompressing unrelated duration
  # parts. Older manifests without dimension metadata retain the safe all-part
  # fallback.
  anchor_upstream <- upstream
  if (rq1_pairwise_is_partitioned(upstream) && is.data.frame(upstream$part_manifest) &&
      all(c("part", "dimension") %in% names(upstream$part_manifest))) {
    anchor_records <- upstream$part_manifest |>
      filter(dimension == "placement_optical_temporal")
    if (nrow(anchor_records) != 1L) {
      stop("Expected exactly one frozen placement/optical/temporal RQ1 part; found ", nrow(anchor_records))
    }
    anchor_upstream$parts <- as.character(anchor_records$part)
    anchor_upstream$part_manifest <- anchor_records
  }
  pair_part_paths <- rq1_pairwise_part_paths(anchor_upstream)
  if (rq1_pairwise_is_partitioned(anchor_upstream) &&
      (!length(pair_part_paths) || any(!file.exists(pair_part_paths)))) {
    stop("Frozen RQ1 anchor pairwise part is missing; cannot recover participant-day anchor values")
  }

  version <- rq1_inference_version(rq1_version)
  B <- suppressWarnings(as.integer(Sys.getenv("RQ1_INFERENCE_BOOT", "1000")))
  if (length(B) != 1L || !is.finite(B) || B < 0L) {
    stop("RQ1_INFERENCE_BOOT must be a nonnegative integer")
  }
  workers <- rq1_resolve_inference_workers()
  message("RQ1 inference runtime: bootstrap=", B, "; workers=", workers)

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

  message("RQ1 inference: load frozen participant-day anchor pairs")
  base_pairs <- rq1_inference_pairs(anchor_upstream)
  if (n_distinct(base_pairs$metric) != contract$daily_metric_count) {
    stop("Frozen RQ1 anchor pairs do not span all ", contract$daily_metric_count, " participant-day metrics")
  }
  if (n_distinct(base_pairs$candidate_config) != contract$anchor_count) {
    stop("Frozen RQ1 anchor pairs do not contain all ", contract$anchor_count, " inferential contrasts")
  }

  sites <- sort(unique(base_pairs$site))
  sleep_paths <- vapply(sites, raw_data_path, character(1), modality = "sleepdiaries")
  ema_paths <- vapply(sites, raw_data_path, character(1), modality = "currentconditions")
  missing_outcome_inputs <- c(sleep_paths[!file.exists(sleep_paths)], ema_paths[!file.exists(ema_paths)])
  if (length(missing_outcome_inputs)) {
    stop(
      "Missing downstream outcome inputs: ", paste(missing_outcome_inputs, collapse = ", "),
      ". Run scripts/01_download_melidos.R with MELIDOS_MODALITIES=sleepdiaries,currentconditions."
    )
  }

  message("RQ1 inference: harmonize Sleep/Alertness/Affect outcomes")
  outcomes <- map_dfr(sites, function(s) {
    bind_rows(
      rq1_sleep_outcomes(load_raw_file(raw_data_path(s, "sleepdiaries"), "sleepdiaries"), s),
      rq1_ema_daily_outcomes(load_raw_file(raw_data_path(s, "currentconditions"), "currentconditions"), s)
    )
  })
  if (!setequal(unique(outcomes$outcome), contract$outcomes)) {
    stop("Downstream outcomes do not match the frozen Sleep/Alertness/Affect contract")
  }
  ms_assert_unique(outcomes, c("site", "Id", "Date", "outcome"), "combined downstream outcomes")

  pairs <- base_pairs |> rq1_expand_outcome_support(outcomes)
  contrast_keys <- c(
    "candidate_config", "support_id", "placement", "optical", "resolution_s",
    "dimension", "comparison_pair_id", "contrast_label", "contrast_order",
    "metric", "metric_class", "metric_geometry", "outcome", "outcome_domain", "outcome_label"
  )
  contrast_results <- rq1_fit_inference_groups(pairs, contrast_keys, B, 20260911L, workers)

  reference_pairs <- rq1_inference_reference_pairs(base_pairs) |> rq1_expand_outcome_support(outcomes)
  if (n_distinct(reference_pairs$metric) != contract$daily_metric_count) {
    stop("Reference association pairing does not span all participant-day metrics")
  }
  reference_keys <- c(
    "candidate_config", "support_id", "metric", "metric_class", "metric_geometry",
    "outcome", "outcome_domain", "outcome_label"
  )
  reference_results <- rq1_fit_inference_groups(reference_pairs, reference_keys, B, 20270911L, workers)

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
  provenance_paths <- unique(c(rq1_path, pair_part_paths, rq1_summary_path, sleep_paths, ema_paths))
  artifact <- list(
    artifact_type = "rq1_inferential_preservation",
    core_artifact_version = core_artifact_version(),
    rq1_analysis_version = rq1_version,
    rq1_inference_version = version,
    analysis_design_id = ms_analysis_design_id(),
    bootstrap_replicates = B,
    parallel_workers = workers,
    bootstrap_seed_base = 20260911L,
    reference_bootstrap_seed_base = 20270911L,
    outcome_domains = c("Sleep", "Alertness", "Affect"),
    outcome_alignment = paste(
      "Sleep: exposure Date D -> following-morning diary;",
      "Alertness/Affect: exposure Date D <-> same-day slot-harmonized EMA phenotype"
    ),
    ema_rule = paste0(
      "nearest response to ", paste(contract$ema_slots_h, collapse = "/"), " h; tolerance <= ",
      contract$ema_slot_tolerance_min, " min; >=", contract$ema_min_slots, " valid slots/day"
    ),
    model = "participant fixed effects; native outcome linear projection; paired site-stratified participant bootstrap",
    inference_deviation = "reference-bootstrap uncertainty norm: absolute/SE for linear; Mahalanobis norm for circular sin/cos",
    analysis_scope = "three human-state domains across eight single-axis frozen RQ1 anchor contrasts; duration and multi-axis combinations excluded",
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
    outcome_audit = outcomes
  )

  for (old_name in contract$retired_artifact_filenames) {
    old_path <- file.path(out, old_name)
    unlink(c(old_path, paste0(old_path, ".ok")), force = TRUE)
  }
  path <- file.path(out, contract$artifact_filename)
  rq1_write_part_atomic(artifact, path)
  readr::write_csv(artifact$contrast_summary,
                   file.path(out, "rq1_inferential_preservation_summary.csv"), na = "")
  readr::write_csv(artifact$term_summary,
                   file.path(out, "rq1_inferential_preservation_term_summary.csv"), na = "")
  readr::write_csv(artifact$reference_summary,
                   file.path(out, "rq1_reference_association_summary.csv"), na = "")
  readr::write_csv(artifact$outcome_audit,
                   file.path(out, "rq1_downstream_outcome_audit.csv"), na = "")
  message("RQ1 inferential preservation frozen: ", path)
  invisible(artifact)
}

if (sys.nframe() == 0L) rq1_run_inference()
