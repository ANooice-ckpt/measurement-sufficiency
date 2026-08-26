#!/usr/bin/env python3
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
OUT = ROOT / "results" / "runtime"
OUT.mkdir(parents=True, exist_ok=True)


def replace_once(text: str, old: str, new: str, label: str) -> str:
    n = text.count(old)
    if n != 1:
        raise RuntimeError(f"{label}: expected exactly one source anchor, found {n}")
    return text.replace(old, new, 1)


# ---------------------------------------------------------------------------
# Duration: on Linux/ECS, fork support x site blocks from the already-split
# in-memory metric/context objects. Each child writes its own checkpoint
# atomically and returns metadata only. This preserves the durable artifact
# format while avoiding both PSOCK copies and large result transfers.
# ---------------------------------------------------------------------------
duration_path = ROOT / "scripts" / "utils" / "duration_artifacts.R"
duration = duration_path.read_text(encoding="utf-8")

duration_old = '''    part_paths <- character(nrow(block_keys))
    part_rows <- integer(nrow(block_keys))
    part_support <- character(nrow(block_keys))
    part_site <- character(nrow(block_keys))
    for (i in seq_len(nrow(block_keys))) {
      part_name <- sprintf("duration_metric_cube_part_%03d.rds", i)
      part_path <- file.path(part_dir, part_name)
      part_marker <- paste0(part_path, ".ok")
      support_value <- as.character(block_keys$support_id[[i]])
      site_value <- as.character(block_keys$site[[i]])
      if (reuse_parts && file.exists(part_path)) {
        # A completed RDS is accompanied by a tiny marker written only after
        # the RDS has been closed.  This avoids reloading 100--500 MB parts
        # merely to resume a run after a process interruption.  Older v3
        # parts predate the marker; their versioned directory and substantial
        # size are sufficient evidence for this one-time migration, while
        # tiny/incomplete files are rebuilt.
        size_ok <- isTRUE(file.info(part_path)$size >= 5e6)
        marker_ok <- file.exists(part_marker)
        if (marker_ok || size_ok) {
          part_paths[[i]] <- part_path
          part_rows[[i]] <- NA_integer_
          part_support[[i]] <- support_value
          part_site[[i]] <- site_value
          next
        }
      }
      block <- build_block(block_keys[i, , drop = FALSE])
      part <- finalize_part(block)
      tmp_path <- paste0(part_path, ".tmp")
      if (file.exists(tmp_path)) unlink(tmp_path)
      saveRDS(part, tmp_path, compress = FALSE)
      if (!file.rename(tmp_path, part_path)) {
        stop("Could not atomically install duration checkpoint: ", part_path)
      }
      writeLines("duration_complete_analysis_days_v1", part_marker, useBytes = TRUE)
      part_paths[[i]] <- part_path
      part_rows[[i]] <- nrow(part)
      part_support[[i]] <- support_value
      part_site[[i]] <- site_value
      rm(block, part)
      invisible(gc())
    }
'''

duration_new = '''    part_paths <- character(nrow(block_keys))
    part_rows <- integer(nrow(block_keys))
    part_support <- character(nrow(block_keys))
    part_site <- character(nrow(block_keys))

    duration_workers <- suppressWarnings(as.integer(Sys.getenv("CORE_DURATION_WORKERS", unset = "1")))
    if (!is.finite(duration_workers) || duration_workers < 1L) duration_workers <- 1L
    detected_cores <- suppressWarnings(parallel::detectCores(logical = FALSE))
    if (!is.finite(detected_cores) || detected_cores < 1L) detected_cores <- parallel::detectCores(logical = TRUE)
    if (is.finite(detected_cores) && detected_cores > 0L) duration_workers <- min(duration_workers, detected_cores)
    duration_workers <- min(duration_workers, nrow(block_keys))

    build_part <- function(i) {
      part_name <- sprintf("duration_metric_cube_part_%03d.rds", i)
      part_path <- file.path(part_dir, part_name)
      part_marker <- paste0(part_path, ".ok")
      support_value <- as.character(block_keys$support_id[[i]])
      site_value <- as.character(block_keys$site[[i]])
      if (reuse_parts && file.exists(part_path)) {
        # A completed RDS is accompanied by a tiny marker written only after
        # the RDS has been closed.  This avoids reloading 100--500 MB parts
        # merely to resume a run after a process interruption.  Older v3
        # parts predate the marker; their versioned directory and substantial
        # size are sufficient evidence for this one-time migration, while
        # tiny/incomplete files are rebuilt.
        size_ok <- isTRUE(file.info(part_path)$size >= 5e6)
        marker_ok <- file.exists(part_marker)
        if (marker_ok || size_ok) {
          return(list(i = i, path = part_path, rows = NA_integer_, support = support_value,
                      site = site_value, status = "reused"))
        }
      }
      block <- build_block(block_keys[i, , drop = FALSE])
      part <- finalize_part(block)
      tmp_path <- paste0(part_path, ".tmp")
      if (file.exists(tmp_path)) unlink(tmp_path)
      saveRDS(part, tmp_path, compress = FALSE)
      if (!file.rename(tmp_path, part_path)) {
        stop("Could not atomically install duration checkpoint: ", part_path)
      }
      writeLines("duration_complete_analysis_days_v1", part_marker, useBytes = TRUE)
      rows <- nrow(part)
      rm(block, part)
      invisible(gc(FALSE))
      list(i = i, path = part_path, rows = rows, support = support_value,
           site = site_value, status = "written")
    }

    # Longest-processing-time-first proxy. The metric/context row counts capture
    # site/support scale, while duration membership rows capture how many window
    # memberships must participate in the many-to-many joins.
    block_cost <- vapply(seq_len(nrow(block_keys)), function(i) {
      support_value <- as.character(block_keys$support_id[[i]])
      site_value <- as.character(block_keys$site[[i]])
      key <- block_token(support_value, site_value)
      metric_n <- if (is.null(metric_blocks[[key]])) 0 else nrow(metric_blocks[[key]])
      context_n <- if (is.null(context_blocks[[key]])) 0 else nrow(context_blocks[[key]])
      membership_n <- sum(membership$support_id == support_value & membership$site == site_value)
      as.numeric(metric_n + context_n + 10 * membership_n)
    }, numeric(1))
    schedule_idx <- order(block_cost, decreasing = TRUE, na.last = TRUE)
    message("[duration] LPT schedule: ", nrow(block_keys), " blocks; workers=", duration_workers)

    if (.Platform$OS.type != "windows" && duration_workers > 1L) {
      part_records_scheduled <- parallel::mclapply(
        schedule_idx, build_part, mc.cores = duration_workers,
        mc.preschedule = FALSE, mc.set.seed = FALSE
      )
    } else {
      part_records_scheduled <- lapply(schedule_idx, build_part)
    }
    failed <- vapply(part_records_scheduled, inherits, logical(1), "try-error")
    if (any(failed)) {
      stop("Duration checkpoint worker failed: ", as.character(part_records_scheduled[[which(failed)[1L]]]))
    }
    part_records <- part_records_scheduled[order(schedule_idx)]
    for (rec in part_records) {
      i <- rec$i
      part_paths[[i]] <- rec$path
      part_rows[[i]] <- rec$rows
      part_support[[i]] <- rec$support
      part_site[[i]] <- rec$site
    }
'''
duration = replace_once(duration, duration_old, duration_new, "duration fork/LPT patch")
duration_out = OUT / "duration_artifacts.optimized.R"
duration_out.write_text(duration, encoding="utf-8")


# ---------------------------------------------------------------------------
# Core: preserve the existing dynamic scheduler, but feed longest estimated
# tasks first. Estimated cost = uncompressed support bytes x configuration count.
# Results are restored to the original support order before downstream binding,
# so this changes scheduling only, not scientific row ordering. The optimized
# core entry point also sources the runtime-only duration implementation above.
# ---------------------------------------------------------------------------
core_path = ROOT / "scripts" / "09_build_core_artifacts.R"
core = core_path.read_text(encoding="utf-8")
core = replace_once(
    core,
    'source("scripts/utils/duration_artifacts.R")\nsource("scripts/utils/weather_era5.R")\nsuppressPackageStartupMessages({',
    'source("results/runtime/duration_artifacts.optimized.R")\nsource("scripts/utils/weather_era5.R")\nsuppressPackageStartupMessages({',
    "core optimized duration source patch",
)

core_old = '''block_paths <- parallel_core_lapply(
  support_paths, compute_one,
  exports = c("METRIC_DIR", "CONTEXT_DIR", "force_rebuild")
)
metric_paths <- vapply(block_paths, `[[`, character(1), "metric")
context_paths <- vapply(block_paths, `[[`, character(1), "context")
'''

core_new = '''# Runtime-only LPT scheduling: dispatch the largest expected blocks first to
# reduce the long tail when the number of supports is only modestly larger than
# the worker count. File size is a direct proxy for support rows because support
# RDS files are stored uncompressed; configuration count captures support-grid
# multiplicity. Restore original order after execution so artifact row ordering
# remains unchanged.
support_id_from_path <- function(path) {
  sub("^[^_]+__", "", tools::file_path_sans_ext(basename(path)))
}
support_bytes <- as.numeric(file.info(support_paths)$size)
support_n_configs <- vapply(
  support_paths,
  function(path) nrow(core_config_grid(support_id_from_path(path))),
  integer(1)
)
support_estimated_cost <- support_bytes * support_n_configs
schedule_idx <- order(support_estimated_cost, decreasing = TRUE, na.last = TRUE)
message(
  "[metrics/context] LPT schedule: ", length(support_paths),
  " blocks; largest estimated cost first"
)
block_paths_scheduled <- parallel_core_lapply(
  support_paths[schedule_idx], compute_one,
  exports = c("METRIC_DIR", "CONTEXT_DIR", "force_rebuild")
)
block_paths <- block_paths_scheduled[order(schedule_idx)]
metric_paths <- vapply(block_paths, `[[`, character(1), "metric")
context_paths <- vapply(block_paths, `[[`, character(1), "context")
'''
core = replace_once(core, core_old, core_new, "core LPT patch")
core_out = OUT / "09_build_core_artifacts.optimized.R"
core_out.write_text(core, encoding="utf-8")


# ---------------------------------------------------------------------------
# RQ1: (1) optimize serial startup work; (2) longest duration parts first;
# (3) parallelize the formerly serial fragment-construction pass and order large
# fragments first. Every optimization preserves the original scientific order
# before downstream binding/aggregation.
# ---------------------------------------------------------------------------
rq1_path = ROOT / "scripts" / "10_rq1_analysis.R"
rq1 = rq1_path.read_text(encoding="utf-8")

rq1_workers_old = '''PART_WORKERS <- ms_resolve_workers("RQ1_PART_WORKERS", default = rq1_default_part_workers(), cap = 48L)
BOOT_SEED <- 20260820L
'''
rq1_workers_new = '''PART_WORKERS <- ms_resolve_workers("RQ1_PART_WORKERS", default = rq1_default_part_workers(), cap = 48L)
STARTUP_WORKERS <- ms_resolve_workers("RQ1_STARTUP_WORKERS", default = min(24L, PART_WORKERS), cap = 32L)
FRAGMENT_WORKERS <- ms_resolve_workers("RQ1_FRAGMENT_WORKERS", default = min(16L, PART_WORKERS), cap = 24L)
BOOT_SEED <- 20260820L
'''
rq1 = replace_once(rq1, rq1_workers_old, rq1_workers_new, "RQ1 startup/fragment worker patch")

rq1_read_old = '''cube <- readr::read_csv(CORE_METRICS, show_col_types = FALSE, progress = FALSE) |>
  mutate(Date = as.Date(Date))
duration_artifact <- readRDS(DURATION_CUBE)
duration_part_paths <- if (duration_cube_is_partitioned(duration_artifact)) {
  file.path(duration_artifact$part_dir, duration_artifact$parts)
} else {
  character()
}
duration_manifest <- readRDS(DURATION_MANIFEST) |>
  mutate(window_start = as.Date(window_start), window_end = as.Date(window_end))
'''
rq1_read_new = '''message("RQ1 startup: read core metric cube")
cube <- readr::read_csv(CORE_METRICS, show_col_types = FALSE, progress = FALSE) |>
  mutate(Date = as.Date(Date))
message("RQ1 startup: read duration manifests")
duration_artifact <- readRDS(DURATION_CUBE)
duration_part_paths <- if (duration_cube_is_partitioned(duration_artifact)) {
  file.path(duration_artifact$part_dir, duration_artifact$parts)
} else {
  character()
}
duration_manifest <- readRDS(DURATION_MANIFEST) |>
  mutate(window_start = as.Date(window_start), window_end = as.Date(window_end))
'''
rq1 = replace_once(rq1, rq1_read_old, rq1_read_new, "RQ1 startup progress patch")

rq1_window_old = '''duration_window_pairs <- duration_manifest |>
  rename(window_a = window_id, n_days_a = n_days, start_a = window_start, end_a = window_end,
         dates_a = member_dates) |>
  inner_join(
    duration_manifest |>
      rename(window_b = window_id, n_days_b = n_days, start_b = window_start, end_b = window_end,
             dates_b = member_dates),
    by = c("support_id", "site", "Id", "run_id"), relationship = "many-to-many"
  ) |>
  filter(n_days_a < n_days_b) |>
  rowwise() |>
  mutate(nested = all(dates_a %in% dates_b)) |>
  ungroup() |>
  filter(nested) |>
  mutate(
    adjacent_transition = n_days_b == n_days_a + 1L,
    pair_id = paste(window_a, window_b, sep = "__to__")
  ) |>
  select(support_id, site, Id, run_id, window_a, window_b, n_days_a, n_days_b,
         start_a, end_a, start_b, end_b, adjacent_transition, pair_id)
'''
rq1_window_new = '''message("RQ1 startup: build nested duration-window pairs (vectorized)")
# duration_window_manifest() constructs every window from consecutive dates
# within an already-consecutive complete-day run. Therefore A is a strict
# subset of longer B iff B starts no later than A and ends no earlier than A.
# Verify the invariant before replacing the old rowwise list-membership test.
window_span_days <- as.integer(duration_manifest$window_end - duration_manifest$window_start) + 1L
if (any(window_span_days != duration_manifest$n_days)) {
  stop("Duration-window contiguity invariant failed; cannot use vectorized nesting test")
}
duration_window_pairs <- duration_manifest |>
  select(-member_dates) |>
  rename(window_a = window_id, n_days_a = n_days, start_a = window_start, end_a = window_end) |>
  inner_join(
    duration_manifest |>
      select(-member_dates) |>
      rename(window_b = window_id, n_days_b = n_days, start_b = window_start, end_b = window_end),
    by = c("support_id", "site", "Id", "run_id"), relationship = "many-to-many"
  ) |>
  filter(n_days_a < n_days_b, start_b <= start_a, end_a <= end_b) |>
  mutate(
    adjacent_transition = n_days_b == n_days_a + 1L,
    pair_id = paste(window_a, window_b, sep = "__to__")
  ) |>
  select(support_id, site, Id, run_id, window_a, window_b, n_days_a, n_days_b,
         start_a, end_a, start_b, end_b, adjacent_transition, pair_id)
'''
rq1 = replace_once(rq1, rq1_window_old, rq1_window_new, "RQ1 duration-window vectorization patch")

rq1_anchor_old = '''duration_anchor_values <- if (length(duration_part_paths)) {
  map_dfr(duration_part_paths, function(part_path) {
    readRDS(part_path) |>
      filter(n_days == MAX_DURATION_DAYS) |>
      select(metric, metric_geometry, value)
  })
} else {
  duration_artifact |>
    filter(n_days == MAX_DURATION_DAYS) |>
    select(metric, metric_geometry, value)
}
'''
rq1_anchor_new = '''read_duration_anchor_part <- function(part_path) {
  readRDS(part_path) |>
    filter(n_days == MAX_DURATION_DAYS) |>
    select(metric, metric_geometry, value)
}
duration_anchor_values <- if (length(duration_part_paths)) {
  message(
    "RQ1 startup: scan ", length(duration_part_paths),
    " duration anchor parts; workers=", STARTUP_WORKERS
  )
  anchor_schedule_idx <- order(
    as.numeric(file.info(duration_part_paths)$size), decreasing = TRUE, na.last = TRUE
  )
  if (.Platform$OS.type != "windows" && STARTUP_WORKERS > 1L) {
    anchor_parts_scheduled <- parallel::mclapply(
      duration_part_paths[anchor_schedule_idx], read_duration_anchor_part,
      mc.cores = min(STARTUP_WORKERS, length(duration_part_paths)),
      mc.preschedule = FALSE, mc.set.seed = FALSE
    )
    anchor_parts <- anchor_parts_scheduled[order(anchor_schedule_idx)]
    bind_rows(anchor_parts)
  } else {
    map_dfr(duration_part_paths, read_duration_anchor_part)
  }
} else {
  duration_artifact |>
    filter(n_days == MAX_DURATION_DAYS) |>
    select(metric, metric_geometry, value)
}
'''
rq1 = replace_once(rq1, rq1_anchor_old, rq1_anchor_new, "RQ1 duration-anchor parallel patch")

rq1_duration_old = '''pending_duration <- duration_tasks[!vapply(duration_tasks, function(task) is.finite(rq1_marker_rows(task$part_path)), logical(1))]
if (length(pending_duration)) {
'''
rq1_duration_new = '''pending_duration <- duration_tasks[!vapply(duration_tasks, function(task) is.finite(rq1_marker_rows(task$part_path)), logical(1))]
if (length(pending_duration)) {
  # LPT scheduling by immutable duration-part size. Result assembly below is
  # keyed by part_index, so execution order cannot change scientific ordering.
  duration_cost <- vapply(
    pending_duration,
    function(task) as.numeric(file.info(task$duration_path)$size),
    numeric(1)
  )
  pending_duration <- pending_duration[order(duration_cost, decreasing = TRUE, na.last = TRUE)]
'''
rq1 = replace_once(rq1, rq1_duration_old, rq1_duration_new, "RQ1 duration LPT patch")

rq1_fragment_old = '''message("RQ1 streaming summaries over ", length(part_paths), " immutable parts")
fragments <- map(part_paths, make_rq1_fragments)
'''
rq1_fragment_new = '''message(
  "RQ1 streaming summaries over ", length(part_paths), " immutable parts; workers=", FRAGMENT_WORKERS
)
fragment_cost <- as.numeric(file.info(part_paths)$size)
fragment_schedule_idx <- order(fragment_cost, decreasing = TRUE, na.last = TRUE)
fragments_scheduled <- ms_parallel_map(
  part_paths[fragment_schedule_idx], make_rq1_fragments,
  workers = FRAGMENT_WORKERS,
  packages = c("tidyverse"),
  exports = c("summary_groups", "make_rq1_fragments")
)
fragments <- fragments_scheduled[order(fragment_schedule_idx)]
'''
rq1 = replace_once(rq1, rq1_fragment_old, rq1_fragment_new, "RQ1 fragment parallel patch")
rq1_out = OUT / "10_rq1_analysis.optimized.R"
rq1_out.write_text(rq1, encoding="utf-8")

print(duration_out.relative_to(ROOT))
print(core_out.relative_to(ROOT))
print(rq1_out.relative_to(ROOT))
