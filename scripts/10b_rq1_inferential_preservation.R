# Independent downstream extension; never sources/reruns 10_rq1_analysis.R.
# Run: RQ1_INFERENCE_BOOT=1000 Rscript scripts/10b_rq1_inferential_preservation.R
# Plot: MS_SUPPLEMENTARY_ONLY=inference Rscript scripts/16_plot_supplementary.R
#
# Analysis contract (additive to STUDY_SPEC; existing RQ1-RQ3 remain unchanged):
# - 52 daily metrics, all 35 alternative placement/optical/primary-cadence states;
#   no multiday metrics, duration windows, or 300-s reserve state.
# - Pair-specific maximal support, with eye/MEDI/10-s values from that SAME
#   support. Each outcome gets identical candidate/reference complete cases,
#   retaining participants with >=2 matched days. Site is absorbed by participant
#   fixed effects; participants are keyed by site + Id, not treated as iid days.
# - Calendar exposure D -> local wake date D+1. This is a descriptive association
#   comparison, not a causal health effect or a reconstruction of sleep exposure.
# - Quality labels map explicitly to 1=Very poor ... 5=Very good; awakenings are
#   counts; awake duration is minutes. Linear mean projections for all outcomes
#   deliberately avoid distributional likelihood assumptions; quality scoring is
#   an operational ordinal-score convention, not a latent ordinal-logit effect.
# - Linear exposures share the matched reference SD. Circular exposures enter
#   jointly as sin/cos (seconds modulo 86400), never a discontinuous clock slope.
# - Coefficient differences and pointwise percentile intervals use the SAME
#   participant draw for both fits, stratified by site, with a fixed reference
#   scale. Intervals do not establish equivalence or adjust for multiplicity.
# - Smallest-unit values, outcome exclusion reasons, actual model support,
#   bootstrap draws and source checksums are frozen in one independent RDS.
#   No existing RQ1 version, downstream checkpoint or Fig.1-5 is changed.
suppressPackageStartupMessages(library(tidyverse))
source("scripts/utils/paths.R")
source("scripts/utils/melidos_io.R")
source("scripts/utils/core_artifacts.R")
source("scripts/utils/rq1_pairwise_artifacts.R")
source("scripts/utils/rq1_inference.R")

rq1_run_inference <- function() {
  metric_path <- file.path(core_root(), "metric_cube.csv.gz")
  rq1_path <- file.path(rq_root("rq1"), "rq1_pairwise_change_long.rds")
  for (p in c(metric_path, rq1_path)) if (!file.exists(p)) stop("Missing frozen input: ", p)
  upstream <- readRDS(rq1_path)
  rq1_version <- rq1_pairwise_version(upstream)
  ms_assert_version(upstream, "core_artifact_version", core_artifact_version())
  ms_assert_version(upstream, "analysis_design_id", ms_analysis_design_id())
  version <- rq1_inference_version(rq1_version)
  B <- suppressWarnings(as.integer(Sys.getenv("RQ1_INFERENCE_BOOT", "1000")))
  if (length(B) != 1L || !is.finite(B) || B < 0L) stop("RQ1_INFERENCE_BOOT must be a nonnegative integer")
  cube <- readr::read_csv(metric_path, show_col_types = FALSE, progress = FALSE,
                         col_types = cols(Id = col_character()))
  ms_assert_version(cube, "core_artifact_version", core_artifact_version())
  sites <- sort(unique(cube$site))
  diary_paths <- vapply(sites, raw_data_path, character(1), modality = "sleepdiaries")
  if (any(!file.exists(diary_paths))) stop("Missing harmonized sleep diaries")
  outcomes <- map_dfr(sites, function(s) rq1_sleep_outcomes(load_raw_file(raw_data_path(s, "sleepdiaries"), "sleepdiaries"), s))
  pairs <- rq1_inference_pairs(cube) |>
    left_join(outcomes, by = c("site", "Id", "Date"), relationship = "many-to-many") |>
    mutate(outcome_reason = if_else(is.na(outcome), "no_next_morning_diary", outcome_reason))
  # Expand missing diaries into all three outcome-specific audits.
  missing <- pairs |> filter(is.na(outcome)) |> select(-outcome)
  pairs <- bind_rows(pairs |> filter(!is.na(outcome)),
                     tidyr::crossing(missing, outcome = c("sleep_quality", "awakenings", "awake_duration")))
  keys <- c("candidate_config", "support_id", "placement", "optical", "resolution_s", "metric", "metric_class", "metric_geometry", "outcome")
  groups <- pairs |> group_by(across(all_of(keys))) |> group_split(.keep = TRUE)
  if (!length(groups)) stop("No daily exposure/reference comparisons")
  results <- vector("list", length(groups))
  for (i in seq_along(groups)) {
    g <- groups[[i]]; meta <- g |> select(all_of(keys)) |> distinct()
    fit <- rq1_inference_fit(g, B = B)
    results[[i]] <- lapply(fit, function(x) if (nrow(x)) bind_cols(meta[rep(1L, nrow(x)), ], x) else x)
    if (i %% 100L == 0L) message("RQ1 inference tasks: ", i, "/", length(groups))
  }
  stamp <- function(x) mutate(x, core_artifact_version = core_artifact_version(),
                              rq1_analysis_version = rq1_version, rq1_inference_version = version)
  out <- file.path(rq_root("rq1"), "inference")
  ensure_result_dirs(out)
  artifact <- list(
    artifact_type = "rq1_inferential_preservation", core_artifact_version = core_artifact_version(),
    rq1_analysis_version = rq1_version, rq1_inference_version = version,
    analysis_design_id = ms_analysis_design_id(), bootstrap_replicates = B, bootstrap_seed = 20260911L,
    date_alignment = "exposure Date D -> local wake date D+1",
    model = "participant fixed effects; native outcome linear projection; paired site-stratified participant bootstrap",
    scale = "reference SD on matched repeated-measures support, fixed across paired bootstrap draws; circular sin/cos unscaled",
    input_provenance = tibble(path = c(metric_path, rq1_path, diary_paths),
                              md5 = unname(tools::md5sum(c(metric_path, rq1_path, diary_paths)))),
    summary = stamp(bind_rows(lapply(results, `[[`, "summary"))),
    support = stamp(bind_rows(lapply(results, `[[`, "support"))),
    bootstrap = bind_rows(lapply(results, `[[`, "bootstrap")),
    pair_audit = stamp(pairs), diary_audit = outcomes
  )
  # A single authoritative frozen result avoids mixed-generation plot inputs.
  path <- file.path(out, "rq1_inferential_preservation.rds")
  rq1_write_part_atomic(artifact, path)
  readr::write_csv(artifact$summary, file.path(out, "rq1_inferential_preservation_summary.csv"), na = "")
  message("RQ1 inferential preservation frozen: ", path)
  invisible(artifact)
}

if (sys.nframe() == 0L) rq1_run_inference()
