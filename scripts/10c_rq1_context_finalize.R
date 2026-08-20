suppressPackageStartupMessages({
  library(tidyverse)
})
source("scripts/utils/rq_context.R")

# Finalize RQ1 context inference from the canonical distortion artifact produced
# by scripts/10b_rq1_context_analysis.R. This script deliberately does not read
# cached 10-s support series, diaries, or rerun context extraction. Scientific
# estimands and bootstrap definitions are identical to the post-canonical stage
# in 10b; this file exists only to make the expensive extraction stage reusable.

RQ1_DISTORTION <- "data/derived/rq1/rq1_distortion_long.rds"
CANONICAL_PATH <- "data/derived/rq1/rq1_context_distortion_long.rds"
OUT_RESULTS <- "results/rq1"
OUT_DIAG <- "results/diagnostics"
B_BOOT <- suppressWarnings(as.integer(Sys.getenv("RQ1_BOOT", unset = "1000")))
if (!is.finite(B_BOOT) || B_BOOT < 0L) B_BOOT <- 1000L
CONTEXT_WORKERS <- suppressWarnings(as.integer(Sys.getenv("RQ1_CONTEXT_WORKERS", unset = "16")))
if (!is.finite(CONTEXT_WORKERS) || CONTEXT_WORKERS < 1L) CONTEXT_WORKERS <- 1L
BOOT_SEED <- 20260822L
Sys.setenv(
  OMP_NUM_THREADS = "1", OPENBLAS_NUM_THREADS = "1", MKL_NUM_THREADS = "1",
  VECLIB_MAXIMUM_THREADS = "1", NUMEXPR_NUM_THREADS = "1"
)

for (p in c(RQ1_DISTORTION, CANONICAL_PATH)) {
  if (!file.exists(p)) stop("Missing required artifact: ", p)
}
for (d in c(OUT_RESULTS, OUT_DIAG)) dir.create(d, recursive = TRUE, showWarnings = FALSE)

rq1 <- readRDS(RQ1_DISTORTION)
canonical <- readRDS(CANONICAL_PATH)

rq1_versions <- unique(rq1$rq1_analysis_version); rq1_versions <- rq1_versions[!is.na(rq1_versions)]
core_versions <- unique(rq1$core_artifact_version); core_versions <- core_versions[!is.na(core_versions)]
ctx_versions <- unique(canonical$rq1_context_analysis_version); ctx_versions <- ctx_versions[!is.na(ctx_versions)]
canon_rq1_versions <- unique(canonical$rq1_analysis_version); canon_rq1_versions <- canon_rq1_versions[!is.na(canon_rq1_versions)]
canon_core_versions <- unique(canonical$core_artifact_version); canon_core_versions <- canon_core_versions[!is.na(canon_core_versions)]
if (length(rq1_versions) != 1L || length(core_versions) != 1L ||
    length(ctx_versions) != 1L || length(canon_rq1_versions) != 1L || length(canon_core_versions) != 1L) {
  stop("RQ1 context finalization requires one unambiguous upstream version")
}
RQ1_VERSION <- rq1_versions[[1]]
CORE_VERSION <- core_versions[[1]]
RQ1_CONTEXT_VERSION <- paste0("rq1_context_v2__", RQ1_VERSION)
if (!identical(ctx_versions[[1]], RQ1_CONTEXT_VERSION) ||
    !identical(canon_rq1_versions[[1]], RQ1_VERSION) ||
    !identical(canon_core_versions[[1]], CORE_VERSION)) {
  stop("Canonical RQ1 context artifact does not match the current upstream RQ1/core versions")
}

required_cols <- c(
  "dimension", "configuration", "configuration_label", "configuration_order",
  "comparison_lattice", "context_family", "context_state",
  "metric", "metric_class", "metric_geometry", "site", "Id",
  "reference_n_context_days", "candidate_n_context_days",
  "reference_n_observations", "candidate_n_observations", "e", "abs_e", "available"
)
missing_cols <- setdiff(required_cols, names(canonical))
if (length(missing_cols)) stop("Canonical RQ1 context artifact missing columns: ", paste(missing_cols, collapse = ", "))

metric_meta <- rq1 |>
  distinct(metric, metric_class, metric_scope, metric_geometry)
metric_manifest <- rq_context_metric_manifest(metric_meta)

safe_q <- rq_context_safe_quantile
bootstrap_ci <- function(g, B = B_BOOT, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  clusters <- g |>
    group_by(site, Id) |>
    summarise(sum_e = sum(e), sum_abs_e = sum(abs_e), n = n(), .groups = "drop")
  site_counts <- clusters |> count(site, name = "n_participants")
  supported <- B > 0L && nrow(clusters) >= 2L && any(site_counts$n_participants > 1L)
  if (!supported) {
    return(tibble(
      bootstrap_supported = FALSE,
      B_ci_low = NA_real_, B_ci_high = NA_real_,
      A_ci_low = NA_real_, A_ci_high = NA_real_
    ))
  }
  by_site <- split(clusters, clusters$site)
  vals <- replicate(B, {
    sampled <- purrr::map_dfr(by_site, ~.x[sample.int(nrow(.x), nrow(.x), replace = TRUE), , drop = FALSE])
    c(
      B = sum(sampled$sum_e) / sum(sampled$n),
      A = sum(sampled$sum_abs_e) / sum(sampled$n)
    )
  })
  tibble(
    bootstrap_supported = TRUE,
    B_ci_low = safe_q(vals["B", ], .025), B_ci_high = safe_q(vals["B", ], .975),
    A_ci_low = safe_q(vals["A", ], .025), A_ci_high = safe_q(vals["A", ], .975)
  )
}

group_vars <- c(
  "dimension", "configuration", "configuration_label", "configuration_order",
  "comparison_lattice", "context_family", "context_state",
  "metric", "metric_class", "metric_geometry"
)
x <- canonical |> filter(available, is.finite(e))
summary_base <- x |>
  group_by(across(all_of(group_vars))) |>
  summarise(
    n_participants = n_distinct(paste(site, Id, sep = "|")),
    n_units = n(),
    median_e = median(e),
    q25_e = safe_q(e, .25), q75_e = safe_q(e, .75),
    p025_e = safe_q(e, .025), p975_e = safe_q(e, .975),
    B_mean_signed = mean(e), A_mean_absolute = mean(abs_e),
    .groups = "drop"
  )

bootstrap_groups <- x |>
  group_by(across(all_of(group_vars))) |>
  group_split(.keep = TRUE)
bootstrap_tasks <- Map(function(g, i) list(g = g, i = i), bootstrap_groups, seq_along(bootstrap_groups))
bootstrap_task <- function(task) {
  g <- task$g
  i <- task$i
  key <- g |> slice(1L) |> select(all_of(group_vars)) |> ungroup()
  bind_cols(key, bootstrap_ci(g, B_BOOT, seed = BOOT_SEED + i))
}

parallel_bootstrap <- function(tasks, workers = CONTEXT_WORKERS) {
  if (!length(tasks)) return(list())
  n_workers <- min(max(1L, as.integer(workers)), length(tasks))
  message("RQ1 context finalization: bootstrap=", B_BOOT,
          ", groups=", length(tasks), ", workers=", n_workers)
  if (n_workers <= 1L) return(lapply(tasks, bootstrap_task))
  if (.Platform$OS.type != "windows") {
    return(parallel::mclapply(
      tasks, bootstrap_task,
      mc.cores = n_workers, mc.preschedule = FALSE, mc.set.seed = TRUE
    ))
  }
  cl <- parallel::makePSOCKcluster(n_workers)
  on.exit(parallel::stopCluster(cl), add = TRUE)
  root <- normalizePath(".", winslash = "/", mustWork = TRUE)
  parallel::clusterCall(cl, function(root) {
    setwd(root)
    Sys.setenv(
      OMP_NUM_THREADS = "1", OPENBLAS_NUM_THREADS = "1", MKL_NUM_THREADS = "1",
      VECLIB_MAXIMUM_THREADS = "1", NUMEXPR_NUM_THREADS = "1"
    )
    suppressPackageStartupMessages({
      library(tidyverse)
    })
    NULL
  }, root)
  parallel::clusterExport(
    cl,
    c("B_BOOT", "BOOT_SEED", "group_vars", "safe_q", "bootstrap_ci", "bootstrap_task"),
    envir = .GlobalEnv
  )
  parallel::parLapplyLB(cl, tasks, bootstrap_task, chunk.size = 1L)
}

message("RQ1 context finalization: reusing canonical checkpoint ", CANONICAL_PATH)
cis <- bind_rows(parallel_bootstrap(bootstrap_tasks))
context_summary <- summary_base |>
  left_join(cis, by = group_vars) |>
  mutate(
    core_artifact_version = CORE_VERSION,
    rq1_analysis_version = RQ1_VERSION,
    rq1_context_analysis_version = RQ1_CONTEXT_VERSION
  )
readr::write_csv(context_summary, file.path(OUT_RESULTS, "rq1_context_summary.csv"), na = "")

coverage <- canonical |>
  group_by(dimension, configuration, context_family, context_state, metric, site) |>
  summarise(
    n_participants = n_distinct(Id),
    n_units = n(), n_available = sum(available),
    reference_context_days = sum(reference_n_context_days, na.rm = TRUE),
    candidate_context_days = sum(candidate_n_context_days, na.rm = TRUE),
    reference_observations = sum(reference_n_observations, na.rm = TRUE),
    candidate_observations = sum(candidate_n_observations, na.rm = TRUE),
    .groups = "drop"
  )
readr::write_csv(coverage, file.path(OUT_DIAG, "rq1_context_coverage.csv"), na = "")

geometry_audit <- context_summary |>
  transmute(
    dimension, configuration, context_family, context_state, metric,
    A_mean_absolute, B_mean_signed,
    pass = A_mean_absolute + 1e-12 >= abs(B_mean_signed)
  )
if (nrow(geometry_audit) && any(!geometry_audit$pass)) stop("Context A >= |B| invariant failed")
readr::write_csv(geometry_audit, file.path(OUT_DIAG, "rq1_context_geometry_invariant.csv"), na = "")

n_photo <- sum(metric_manifest$photoperiod_valid)
n_fragmented <- sum(metric_manifest$fragmented_context_valid)
writeLines(c(
  "# RQ1 context run report", "",
  paste0("Core artifact version: ", CORE_VERSION),
  paste0("RQ1 upstream version: ", RQ1_VERSION),
  paste0("RQ1 context version: ", RQ1_CONTEXT_VERSION),
  paste0("Workers: ", CONTEXT_WORKERS),
  paste0("Bootstrap replicates: ", B_BOOT),
  paste0("Whole-day target representations upstream: ", n_distinct(metric_meta$metric)),
  paste0("Photoperiod-valid context representations: ", n_photo),
  paste0("Indoor/outdoor and activity-valid representations: ", n_fragmented),
  paste0("Canonical context distortion rows: ", nrow(canonical)),
  paste0("Available standardized rows: ", nrow(x)),
  "Contexts: civil day/night; diary-derived indoor/outdoor; diary-derived home/working/vehicle/outdoors.",
  "Photoperiod keeps continuous-interval-valid metric families. Fragmented contexts keep only additive/distributional operators and calculate additive metrics within episodes before summing, never by stitching separated episodes.",
  "Reference standardization is fixed within comparison lattice x metric x context family and shared across context states.",
  "Finalization reused the canonical context-distortion checkpoint and did not rerun support-series extraction.",
  "Scientific operators and bootstrap estimands are unchanged from scripts/10b_rq1_context_analysis.R."
), file.path(OUT_RESULTS, "RQ1_CONTEXT_RUN_REPORT.md"))

message("RQ1 context finalization complete: ", RQ1_CONTEXT_VERSION)
message("  reused: ", CANONICAL_PATH)
message("  wrote: ", file.path(OUT_RESULTS, "rq1_context_summary.csv"))
message("  wrote: ", file.path(OUT_DIAG, "rq1_context_coverage.csv"))
