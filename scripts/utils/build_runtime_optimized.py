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
# Core: preserve the existing dynamic scheduler, but feed longest estimated
# tasks first. Estimated cost = uncompressed support bytes x configuration count.
# Results are restored to the original support order before downstream binding,
# so this changes scheduling only, not scientific row ordering.
# ---------------------------------------------------------------------------
core_path = ROOT / "scripts" / "09_build_core_artifacts.R"
core = core_path.read_text(encoding="utf-8")

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
# RQ1: (1) longest duration parts first; (2) parallelize the formerly serial
# fragment-construction pass with a deliberately bounded worker pool; (3) order
# fragment jobs by part size and restore original order afterward.
# ---------------------------------------------------------------------------
rq1_path = ROOT / "scripts" / "10_rq1_analysis.R"
rq1 = rq1_path.read_text(encoding="utf-8")

rq1_workers_old = '''PART_WORKERS <- ms_resolve_workers("RQ1_PART_WORKERS", default = rq1_default_part_workers(), cap = 48L)
BOOT_SEED <- 20260820L
'''
rq1_workers_new = '''PART_WORKERS <- ms_resolve_workers("RQ1_PART_WORKERS", default = rq1_default_part_workers(), cap = 48L)
FRAGMENT_WORKERS <- ms_resolve_workers("RQ1_FRAGMENT_WORKERS", default = min(16L, PART_WORKERS), cap = 24L)
BOOT_SEED <- 20260820L
'''
rq1 = replace_once(rq1, rq1_workers_old, rq1_workers_new, "RQ1 fragment worker patch")

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

print(core_out.relative_to(ROOT))
print(rq1_out.relative_to(ROOT))
