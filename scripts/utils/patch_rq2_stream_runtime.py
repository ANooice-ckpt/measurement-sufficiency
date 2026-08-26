#!/usr/bin/env python3
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
TARGET = ROOT / "results" / "runtime" / "12_rq2_analysis_v5.runtime.R"


def replace_once(text: str, old: str, new: str, label: str) -> str:
    n = text.count(old)
    if n != 1:
        raise RuntimeError(f"{label}: expected exactly one anchor, found {n}")
    return text.replace(old, new, 1)


text = TARGET.read_text(encoding="utf-8")

text = replace_once(
    text,
    'RQ2_WORKERS <- ms_resolve_workers("RQ2_WORKERS", default = 1L, cap = 48L)\n',
    'RQ2_WORKERS <- ms_resolve_workers("RQ2_WORKERS", default = 1L, cap = 48L)\n'
    'RQ2_STREAM_WORKERS <- ms_resolve_workers("RQ2_STREAM_WORKERS", default = min(RQ2_WORKERS, 12L), cap = 24L)\n',
    "RQ2 stream-worker setting",
)

text = replace_once(
    text,
    'SHARD_DIR <- file.path(SHARD_ROOT, RQ2_VERSION)\n'
    'dir.create(SHARD_DIR, recursive = TRUE, showWarnings = FALSE)\n',
    'SHARD_DIR <- file.path(SHARD_ROOT, RQ2_VERSION)\n'
    'UNIT_FEATURE_DIR <- file.path(OUT, "unit_feature_parts", RQ2_VERSION)\n'
    'dir.create(SHARD_DIR, recursive = TRUE, showWarnings = FALSE)\n'
    'dir.create(UNIT_FEATURE_DIR, recursive = TRUE, showWarnings = FALSE)\n',
    "RQ2 unit-feature checkpoint directory",
)

old_pass1 = '''message("RQ2 v5 pass 1/2: build metric-independent transition-unit features from ", length(part_paths), " canonical parts")
unit_feature_parts <- lapply(seq_along(part_paths), function(i) {
  if (i %% 8L == 0L || i == length(part_paths)) message("  unit features ", i, "/", length(part_paths))
  out <- unit_features_from_part(part_paths[[i]])
  invisible(gc(FALSE))
  out
})
unit_features <- bind_rows(unit_feature_parts) |>
  distinct(transition_unit_key, .keep_all = TRUE)
rm(unit_feature_parts)
invisible(gc())'''

new_pass1 = '''unit_feature_checkpoint <- function(task) {
  out_path <- file.path(UNIT_FEATURE_DIR, sprintf("part_%03d.rds", task$i - 1L))
  if (file.exists(out_path)) return(readRDS(out_path))
  out <- unit_features_from_part(task$path)
  tmp <- paste0(out_path, ".tmp.", Sys.getpid())
  saveRDS(out, tmp, compress = "gzip")
  if (file.exists(out_path)) unlink(out_path)
  if (!file.rename(tmp, out_path)) stop("Could not install RQ2 unit-feature checkpoint")
  out
}
message("RQ2 v5 pass 1/2: build/reuse metric-independent transition-unit features from ",
        length(part_paths), " canonical parts; workers=", RQ2_STREAM_WORKERS)
unit_feature_tasks <- Map(function(path, i) list(path = path, i = i), part_paths, seq_along(part_paths))
unit_feature_parts <- ms_parallel_map(
  unit_feature_tasks, unit_feature_checkpoint, workers = RQ2_STREAM_WORKERS,
  packages = c("tidyverse"),
  exports = c("unit_feature_checkpoint", "unit_features_from_part", "normalize_primary",
              "daily_features", "window_features", "UNIT_FEATURE_DIR")
)
unit_features <- bind_rows(unit_feature_parts) |>
  distinct(transition_unit_key, .keep_all = TRUE)
rm(unit_feature_parts, unit_feature_tasks)
invisible(gc())'''
text = replace_once(text, old_pass1, new_pass1, "RQ2 pass-1 parallel/checkpoint patch")

old_pass2 = '''message("RQ2 v5 pass 2/2: stream canonical parts into bounded model-input shards")
shard_manifests <- vector("list", length(part_paths))
for (i in seq_along(part_paths)) {
  shard_manifests[[i]] <- build_shards_for_part(part_paths[[i]], i)
  if (i %% 8L == 0L || i == length(part_paths)) message("  shards ", i, "/", length(part_paths))
}
shard_manifest <- bind_rows(shard_manifests)'''

new_pass2 = '''build_shard_task <- function(task) build_shards_for_part(task$path, task$i)
message("RQ2 v5 pass 2/2: build/reuse bounded model-input shards; workers=", RQ2_STREAM_WORKERS)
shard_tasks <- Map(function(path, i) list(path = path, i = i), part_paths, seq_along(part_paths))
shard_manifests <- ms_parallel_map(
  shard_tasks, build_shard_task, workers = RQ2_STREAM_WORKERS,
  packages = c("tidyverse"),
  exports = c("build_shard_task", "build_shards_for_part", "normalize_primary", "unit_features",
              "task_catalog", "SHARD_DIR", "RQ2_VERSION", "sanitize_task", "part_token", "EXTERNAL")
)
shard_manifest <- bind_rows(shard_manifests)
rm(shard_tasks)'''
text = replace_once(text, old_pass2, new_pass2, "RQ2 pass-2 parallel patch")

text = replace_once(
    text,
    '''if (!KEEP_MODEL_INPUTS && dir.exists(SHARD_DIR)) {
  unlink(SHARD_DIR, recursive = TRUE, force = TRUE)
}
message("RQ2 complete: ", RQ2_VERSION)''',
    '''# Base shards are deliberately retained until the layered context stage has
# completed successfully. The canonical stage must never delete its own resume
# state before downstream consumers finish.
message("RQ2 complete: ", RQ2_VERSION)''',
    "RQ2 deferred shard cleanup patch",
)

TARGET.write_text(text, encoding="utf-8")
print(TARGET.relative_to(ROOT))
print("RQ2 streaming runtime patched: parallel pass 1/2 + pass 2/2, durable pass-1 checkpoints, deferred shard cleanup")
