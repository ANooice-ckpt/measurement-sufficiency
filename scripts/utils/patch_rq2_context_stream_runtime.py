#!/usr/bin/env python3
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
SRC = ROOT / "scripts" / "12c_rq2_context_models.R"
OUT = ROOT / "results" / "runtime" / "12c_rq2_context_models.runtime.R"
OUT.parent.mkdir(parents=True, exist_ok=True)

text = SRC.read_text(encoding="utf-8")
old = '''  message("RQ2 layered context: build model-input shards from canonical RQ1 parts")
  context_shard_manifest <- bind_rows(lapply(seq_along(part_paths), function(i) {
    if (i %% 8L == 0L || i == length(part_paths)) message("  context shards ", i, "/", length(part_paths))
    build_context_shards(part_paths[[i]], i)
  }))'''
new = '''  CONTEXT_STREAM_WORKERS <- ms_resolve_workers("RQ2_STREAM_WORKERS", default = 12L, cap = 24L)
  context_shard_task <- function(task) build_context_shards(task$path, task$i)
  context_shard_tasks <- lapply(seq_along(part_paths), function(i) {
    list(path = part_paths[[i]], i = i)
  })
  message(
    "RQ2 layered context: build model-input shards from canonical RQ1 parts across ",
    CONTEXT_STREAM_WORKERS, " PSOCK workers"
  )
  context_shard_manifest <- bind_rows(ms_parallel_map(
    context_shard_tasks, context_shard_task,
    workers = CONTEXT_STREAM_WORKERS,
    packages = c("tidyverse"),
    exports = c(
      "context_shard_task", "build_context_shards", "CONTEXT_SHARD_ROOT",
      "context_part_token", "CONTEXT_VERSION", "normalize_primary",
      "unit_features_layered", "CONTEXT_ALL", "task_catalog", "sanitize_task"
    )
  ))'''

count = text.count(old)
if count != 1:
    raise RuntimeError(f"RQ2 layered-context stream patch: expected exactly one anchor, found {count}")

OUT.write_text(text.replace(old, new, 1), encoding="utf-8")
print(OUT.relative_to(ROOT))
