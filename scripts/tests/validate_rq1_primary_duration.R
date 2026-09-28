# Exercise production duration readers, task construction, canonicalization and
# summary projection on synthetic Core values. All writes stay in tempdir().
suppressPackageStartupMessages(library(tidyverse))
source("scripts/utils/analysis_design.R")
source("scripts/utils/rq1_pairwise_artifacts.R")

production <- parse("scripts/10_rq1_analysis.R")
assignment <- function(name) {
  found <- Filter(function(x) is.call(x) && identical(x[[1]], as.name("<-")) &&
    is.symbol(x[[2]]) && identical(as.character(x[[2]]), name), production)
  stopifnot(length(found) == 1L)
  found[[1]]
}

local({
  root <- tempfile("rq1_primary_duration_")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE))
  values <- tidyr::crossing(
    Id = c("P1", "P2"), resolution_s = ms_all_temporal_s(), n_days = c(1L, 6L)
  ) |>
    mutate(
      support_id = "eye_medi", site = "test", placement = "eye", optical = "MEDI",
      # Deliberately untrustworthy metadata: the frozen cadence vector is the rule.
      is_primary_resolution = TRUE,
      window_id = paste(Id, n_days, sep = "_"),
      config_id = paste0("eye__MEDI__", resolution_s, "s"),
      analysis_unit_id = paste(window_id, config_id, sep = "|"),
      metric = "mean_MEDI", metric_class = "magnitude", metric_scope = "daily",
      metric_geometry = "linear", available = TRUE, unavailable_reason = NA_character_,
      value = if_else(Id == "P1", 10, 20) + resolution_s / 10 -
        if_else(n_days == 1L, resolution_s / 100, 0),
      # A reserve cadence must affect neither the scale nor the duration changes.
      value = if_else(resolution_s == 300L,
                      if_else(n_days == 1L, -1e8, 1e8), value)
    )
  input_path <- file.path(root, "duration_core.rds")
  saveRDS(values, input_path)
  before <- tools::md5sum(input_path)
  windows <- tibble(
    support_id = "eye_medi", site = "test", Id = c("P1", "P2"),
    window_a = c("P1_1", "P2_1"), window_b = c("P1_6", "P2_6"),
    n_days_a = 1L, n_days_b = 6L,
    start_a = as.Date("2025-01-01"), end_a = as.Date("2025-01-01"),
    start_b = as.Date("2025-01-01"), end_b = as.Date("2025-01-06"),
    adjacent_transition = FALSE, pair_id = c("P1_1__to__P1_6", "P2_1__to__P2_6")
  )

  run_case <- function(partitioned, primary) {
    e <- new.env(parent = globalenv())
    e$PRIMARY_TEMPORAL_S <- primary
    e$MAX_DURATION_DAYS <- 6L
    e$CORE_VERSION <- "synthetic_core"
    e$ANALYSIS_DESIGN_ID <- ms_analysis_design_id()
    e$STARTUP_WORKERS <- 1L
    e$duration_artifact <- values
    e$duration_part_paths <- if (partitioned) input_path else character()
    e$duration_window_pairs <- windows
    for (name in c("RQ1_ANALYSIS_VERSION", "duration_columns", "summary_groups",
                   "circular_delta", "circular_mean", "scale_primary", "scale_robust",
                   "read_duration_anchor_part", "build_duration_pair_chunk",
                   "canonicalize_pairs", "rq1_marker_rows", "rq1_process_duration_part",
                   "rq1_summary_projection")) eval(assignment(name), envir = e)
    stopifnot(startsWith(e$RQ1_ANALYSIS_VERSION, "rq1_v5_primary_duration_type_canonical__"))
    e$pairwise_part_dir <- file.path(root, paste(partitioned, paste(primary, collapse = "_")),
                                   e$RQ1_ANALYSIS_VERSION)

    # Evaluate both real top-level input branches without executing the runner.
    eval(assignment("duration_anchor_values"), envir = e)
    expected_values <- values$value[values$n_days == 6L & values$resolution_s %in% primary]
    stopifnot(identical(sort(e$duration_anchor_values$value), sort(expected_values)))
    e$anchor_values <- e$duration_anchor_values |>
      transmute(comparison_lattice = "duration", metric, metric_geometry, value,
                scale_anchor_config = "longest_observed_window_in_run")
    eval(assignment("standardizers"), envir = e)
    expected_scale <- sd(expected_values)
    stopifnot(nrow(e$standardizers) == 1L,
              abs(e$standardizers$standardizer - expected_scale) < 1e-12)

    # The lower-level pair builder must also reject reserve values when called
    # directly, independently of the worker's input filtering.
    raw <- e$build_duration_pair_chunk(values, windows)
    stopifnot(nrow(raw) == 2L * length(primary),
              !any(grepl("__300s", raw$config_a_id, fixed = TRUE)))
    eval(assignment("duration_tasks"), envir = e)
    stopifnot(length(e$duration_tasks) == 1L)
    written <- e$rq1_process_duration_part(e$duration_tasks[[1]])
    output <- readRDS(e$duration_tasks[[1]]$part_path)
    stopifnot(written$status == "written", written$rows == nrow(raw),
              nrow(output) == nrow(raw), !anyDuplicated(output$pair_key),
              all(output$available), !any(grepl("__300s", output$config_a_id, fixed = TRUE)))

    # Retained source values stay exact. Each synthetic primary change equals
    # cadence / 100; this independent oracle also detects scale contamination.
    key <- function(id, config, window) paste(id, config, window, sep = "|")
    source_keys <- key(values$Id, values$config_id, values$window_id)
    config <- sub("__P[12]_[16]$", "", output$config_a_id)
    a <- match(key(output$Id, config, output$window_id_a), source_keys)
    b <- match(key(output$Id, config, output$window_id_b), source_keys)
    stopifnot(!anyNA(c(a, b)), identical(output$value_a, values$value[a]),
              identical(output$value_b, values$value[b]),
              abs(mean(abs(output$z)) - mean(primary) / 100 / expected_scale) < 1e-12)
    projected <- e$rq1_summary_projection(output)
    stopifnot(all(projected$comparison_pair_id == "1d_vs_6d"),
              all(projected$config_a_id == "duration_1d"),
              all(projected$config_b_id == "duration_6d"))
    output
  }

  partitioned <- run_case(TRUE, ms_primary_temporal_s())
  flat <- run_case(FALSE, ms_primary_temporal_s())
  stopifnot(identical(partitioned, flat))
  # A different frozen vector must propagate through all production paths.
  stopifnot(nrow(run_case(TRUE, c(20L, 60L))) == 4L,
            nrow(run_case(FALSE, c(20L, 60L))) == 4L,
            identical(tools::md5sum(input_path), before))
})

cat("PASS: RQ1 primary-only duration scales, partition/flat pairs and frozen summary projection\n")
