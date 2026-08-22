#!/usr/bin/env python3
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
TARGET = ROOT / "results" / "runtime" / "10_rq1_analysis.optimized.R"


def replace_once(text: str, old: str, new: str, label: str) -> str:
    n = text.count(old)
    if n != 1:
        raise RuntimeError(f"{label}: expected exactly one source anchor, found {n}")
    return text.replace(old, new, 1)


if not TARGET.exists():
    raise RuntimeError(
        "Missing generated optimized RQ1 script. Run "
        "python3 scripts/utils/build_runtime_optimized.py first."
    )

rq1 = TARGET.read_text(encoding="utf-8")

# Replace the old pseudo-streaming implementation with durable per-part
# checkpoints at the correct scientific summary level. Canonical duration rows
# remain concrete nested-window comparisons for traceability, but the summary
# projection collapses those concrete windows to the 15 monitoring-duration
# comparison types (1d->2d, ..., 5d->6d). Exact z values are retained only as
# compact numeric vectors so global quantiles remain exact without retaining
# millions of duplicated string/group columns.
fragment_old = '''message(
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

fragment_new = '''fragment_dir <- file.path(pairwise_part_dir, "summary_fragments_sufficient_v2_duration_type")
dir.create(fragment_dir, recursive = TRUE, showWarnings = FALSE)

fragment_path_for <- function(part_path) {
  file.path(
    fragment_dir,
    paste0(tools::file_path_sans_ext(basename(part_path)), "__summary_fragment.rds")
  )
}

rq1_summary_projection <- function(canonical) {
  canonical |>
    mutate(
      comparison_pair_id = if_else(
        dimension == "duration",
        paste0(n_days_a, "d_vs_", n_days_b, "d"),
        comparison_pair_id
      ),
      config_a_id = if_else(
        dimension == "duration",
        paste0("duration_", n_days_a, "d"),
        config_a_id
      ),
      config_b_id = if_else(
        dimension == "duration",
        paste0("duration_", n_days_b, "d"),
        config_b_id
      ),
      config_a_label = if_else(
        dimension == "duration",
        paste0(n_days_a, " d"),
        config_a_label
      ),
      config_b_label = if_else(
        dimension == "duration",
        paste0(n_days_b, " d"),
        config_b_label
      )
    )
}

make_rq1_fragment_checkpoint <- function(part_path) {
  fragment_path <- fragment_path_for(part_path)
  marker_path <- paste0(fragment_path, ".ok")
  if (file.exists(fragment_path) && file.exists(marker_path)) return(fragment_path)

  canonical <- rq1_summary_projection(readRDS(part_path))
  if (any(canonical$dimension == "duration" & grepl("__to__", canonical$comparison_pair_id, fixed = TRUE))) {
    stop("Concrete duration window id leaked into RQ1 summary projection")
  }
  x <- canonical |> filter(available, is.finite(z))

  # Compact exact-distribution chunks. Each duration part now contributes only
  # a small number of scientific groups, while z itself remains exact for the
  # final pooled empirical quantiles.
  summary_fragment <- x |>
    group_by(across(all_of(summary_groups))) |>
    summarise(z_values = list(as.numeric(z)), .groups = "drop")

  anchor_fragment <- canonical |> filter(anchor_projection) |>
    group_by(across(all_of(summary_groups))) |>
    summarise(
      n_total_units = n(), n_available_units = sum(available),
      participant_keys = list(unique(paste(site, Id, sep = "|"))),
      A_sum = sum(abs(z[available & is.finite(z)])),
      B_sum = sum(z[available & is.finite(z)]), .groups = "drop"
    )

  availability_fragment <- canonical |>
    group_by(dimension, comparison_lattice, comparison_pair_id, config_a_id, config_b_id, metric, metric_class) |>
    summarise(
      n_total_units = n(), n_available_units = sum(available),
      participant_keys = list(unique(paste(site, Id, sep = "|"))),
      available_participant_keys = list(unique(paste(site[available], Id[available], sep = "|"))),
      representation_available = any(available),
      unavailable_reasons = list(sort(unique(na.omit(unavailable_reason)))), .groups = "drop"
    )

  # Exact sufficient statistics for participant-cluster/site-stratified
  # bootstrap. If a participant has rows in more than one immutable part, the
  # master aggregation below sums these before resampling.
  participant_fragment <- x |>
    group_by(across(all_of(summary_groups)), site, Id) |>
    summarise(n_units = n(), sum_abs = sum(abs(z)), sum_signed = sum(z), .groups = "drop")

  robust_fragment <- canonical |> filter(pair_available, is.finite(robust_z)) |>
    group_by(across(all_of(summary_groups))) |>
    summarise(sum_abs = sum(abs(robust_z)), sum_signed = sum(robust_z), n_units = n(), .groups = "drop")

  out <- list(
    summary = summary_fragment,
    anchor = anchor_fragment,
    availability = availability_fragment,
    participant = participant_fragment,
    robust = robust_fragment
  )
  tmp_path <- paste0(fragment_path, ".tmp.", Sys.getpid())
  if (file.exists(tmp_path)) unlink(tmp_path)
  saveRDS(out, tmp_path, compress = "gzip")
  if (!file.rename(tmp_path, fragment_path)) {
    stop("Could not atomically install RQ1 summary fragment: ", fragment_path)
  }
  writeLines("rq1_summary_fragment_sufficient_v2_duration_type", marker_path, useBytes = TRUE)
  rm(canonical, x, summary_fragment, anchor_fragment, availability_fragment,
     participant_fragment, robust_fragment, out)
  invisible(gc(FALSE))
  fragment_path
}

message(
  "RQ1 memory-safe duration-type summary checkpoints over ", length(part_paths),
  " immutable parts; workers=", FRAGMENT_WORKERS
)
fragment_cost <- as.numeric(file.info(part_paths)$size)
fragment_schedule_idx <- order(fragment_cost, decreasing = TRUE, na.last = TRUE)
fragment_paths_scheduled <- ms_parallel_map(
  part_paths[fragment_schedule_idx], make_rq1_fragment_checkpoint,
  workers = FRAGMENT_WORKERS,
  packages = c("tidyverse"),
  exports = c("summary_groups", "fragment_dir", "fragment_path_for",
              "rq1_summary_projection", "make_rq1_fragment_checkpoint")
)
fragment_paths <- unlist(fragment_paths_scheduled[order(fragment_schedule_idx)], use.names = FALSE)

read_rq1_fragment_component <- function(component) {
  map_dfr(fragment_paths, function(path) {
    z <- readRDS(path)
    out <- z[[component]]
    rm(z)
    out
  })
}
'''
rq1 = replace_once(rq1, fragment_old, fragment_new, "memory-safe duration-type fragment checkpoint patch")

# Replace the original raw-unit summary/bootstrap block. Exact z distributions
# are pooled only as numeric vectors; participant resampling uses n, sum|z| and
# sum(z), which is algebraically identical to resampling all raw units belonging
# to each selected participant.
summary_old = '''summary_fragments <- map_dfr(fragments, "summary")
summary_base <- summary_fragments |>
  group_by(across(all_of(summary_groups))) |>
  summarise(z_values = list(unlist(z_values, use.names = FALSE)),
            site_values = list(unlist(site_values, use.names = FALSE)),
            id_values = list(unlist(id_values, use.names = FALSE)), .groups = "drop") |>
  mutate(
    n_participants = map2_int(site_values, id_values, ~n_distinct(paste(.x, .y, sep = "|"))),
    n_units = map_int(z_values, length),
    median_z = map_dbl(z_values, ~safe_q(.x, .5)), q25_z = map_dbl(z_values, ~safe_q(.x, .25)),
    q75_z = map_dbl(z_values, ~safe_q(.x, .75)), p025_z = map_dbl(z_values, ~safe_q(.x, .025)),
    p975_z = map_dbl(z_values, ~safe_q(.x, .975)), B_mean_signed = map_dbl(z_values, mean),
    A_mean_absolute = map_dbl(z_values, ~mean(abs(.x)))
  )
summary <- summary_base |>
  select(all_of(summary_groups), n_participants, n_units, median_z, q25_z, q75_z, p025_z, p975_z,
         B_mean_signed, A_mean_absolute) |>
  mutate(core_artifact_version = CORE_VERSION, rq1_analysis_version = RQ1_ANALYSIS_VERSION,
         uncertainty_method = if (B_BOOT > 0L) "participant-cluster/site-stratified bootstrap" else "point estimate; bootstrap disabled")

bootstrap_groups <- map(seq_len(nrow(summary_base)), function(i) tibble(
  site = summary_base$site_values[[i]], Id = summary_base$id_values[[i]], z = summary_base$z_values[[i]]
))
bootstrap_tasks <- map(seq_along(bootstrap_groups), function(i) list(group = bootstrap_groups[[i]], reps = B_BOOT, seed = BOOT_SEED + i))
bootstrap_results <- if (B_BOOT > 0L) {
  ms_parallel_map(bootstrap_tasks, bootstrap_pair_group, workers = BOOT_WORKERS, seed = BOOT_SEED,
                  packages = c("tidyverse"), exports = c("bootstrap_pair_group", "safe_q"))
} else lapply(bootstrap_tasks, bootstrap_pair_group)
bootstrap_summary <- bind_rows(map(seq_along(bootstrap_groups), function(i) bind_cols(
  summary_base[i, ] |> select(all_of(summary_groups)), bootstrap_results[[i]]
)))
summary <- summary |> left_join(bootstrap_summary, by = summary_groups)
'''

summary_new = '''summary_chunks <- read_rq1_fragment_component("summary")
summary_base <- summary_chunks |>
  group_by(across(all_of(summary_groups))) |>
  summarise(z_values = list(unlist(z_values, use.names = FALSE)), .groups = "drop") |>
  arrange(across(all_of(summary_groups))) |>
  mutate(
    .summary_index = row_number(),
    n_units = map_int(z_values, length),
    median_z = map_dbl(z_values, ~safe_q(.x, .5)),
    q25_z = map_dbl(z_values, ~safe_q(.x, .25)),
    q75_z = map_dbl(z_values, ~safe_q(.x, .75)),
    p025_z = map_dbl(z_values, ~safe_q(.x, .025)),
    p975_z = map_dbl(z_values, ~safe_q(.x, .975)),
    B_mean_signed = map_dbl(z_values, mean),
    A_mean_absolute = map_dbl(z_values, ~mean(abs(.x)))
  )
rm(summary_chunks)
invisible(gc(FALSE))

# Aggregate participant sufficient statistics across immutable parts before
# counting participants or bootstrapping scientific comparison groups.
participant_fragments <- read_rq1_fragment_component("participant") |>
  group_by(across(all_of(summary_groups)), site, Id) |>
  summarise(
    n_units = sum(n_units), sum_abs = sum(sum_abs), sum_signed = sum(sum_signed),
    .groups = "drop"
  )
participant_counts <- participant_fragments |>
  group_by(across(all_of(summary_groups))) |>
  summarise(n_participants = n(), .groups = "drop")
summary_base <- summary_base |>
  left_join(participant_counts, by = summary_groups)

# Duration is summarized by monitoring requirement, never by concrete window id.
duration_pair_types <- summary_base |>
  filter(dimension == "duration") |>
  distinct(comparison_pair_id)
if (nrow(duration_pair_types) > 15L || any(grepl("__to__", duration_pair_types$comparison_pair_id, fixed = TRUE))) {
  stop("RQ1 duration summary-level invariant failed: expected at most 15 n-day comparison types")
}

summary <- summary_base |>
  select(all_of(summary_groups), n_participants, n_units, median_z, q25_z, q75_z, p025_z, p975_z,
         B_mean_signed, A_mean_absolute) |>
  mutate(core_artifact_version = CORE_VERSION, rq1_analysis_version = RQ1_ANALYSIS_VERSION,
         uncertainty_method = if (B_BOOT > 0L) "participant-cluster/site-stratified bootstrap" else "point estimate; bootstrap disabled")

bootstrap_pair_group_sufficient <- function(task) {
  participants <- task$group |> select(site, Id, n_units, sum_abs, sum_signed)
  reps <- task$reps
  if (!reps || !nrow(participants)) {
    return(tibble(A_boot_q025 = NA_real_, A_boot_q975 = NA_real_,
                  B_boot_q025 = NA_real_, B_boot_q975 = NA_real_,
                  bootstrap_reps = 0L, bootstrap_sites = n_distinct(participants$site),
                  bootstrap_participants = nrow(participants)))
  }
  set.seed(task$seed)
  a <- b <- rep(NA_real_, reps)
  for (j in seq_len(reps)) {
    sampled <- split(participants$Id, participants$site) |>
      lapply(function(ids) sample(ids, length(ids), replace = TRUE))
    sampled <- tibble(
      site = rep(names(sampled), lengths(sampled)),
      Id = unlist(sampled, use.names = FALSE)
    )
    boot <- merge(sampled, participants, by = c("site", "Id"), all = FALSE, sort = FALSE)
    denom <- sum(boot$n_units)
    if (is.finite(denom) && denom > 0) {
      a[[j]] <- sum(boot$sum_abs) / denom
      b[[j]] <- sum(boot$sum_signed) / denom
    }
  }
  tibble(
    A_boot_q025 = safe_q(a, .025), A_boot_q975 = safe_q(a, .975),
    B_boot_q025 = safe_q(b, .025), B_boot_q975 = safe_q(b, .975),
    bootstrap_reps = sum(is.finite(a) & is.finite(b)),
    bootstrap_sites = n_distinct(participants$site),
    bootstrap_participants = nrow(participants)
  )
}

participant_fragments <- participant_fragments |>
  inner_join(summary_base |> select(all_of(summary_groups), .summary_index), by = summary_groups)
split_groups <- split(participant_fragments, participant_fragments$.summary_index)
bootstrap_tasks <- lapply(summary_base$.summary_index, function(i) {
  g <- split_groups[[as.character(i)]]
  if (is.null(g)) g <- participant_fragments[0, , drop = FALSE]
  list(group = g, reps = B_BOOT, seed = BOOT_SEED + i)
})
bootstrap_results <- if (B_BOOT > 0L) {
  ms_parallel_map(
    bootstrap_tasks, bootstrap_pair_group_sufficient,
    workers = BOOT_WORKERS, seed = BOOT_SEED,
    packages = c("tidyverse"),
    exports = c("bootstrap_pair_group_sufficient", "safe_q")
  )
} else {
  lapply(bootstrap_tasks, bootstrap_pair_group_sufficient)
}
bootstrap_summary <- bind_rows(map(seq_along(bootstrap_results), function(i) {
  bind_cols(
    summary_base[i, ] |> select(all_of(summary_groups)),
    bootstrap_results[[i]]
  )
}))
summary <- summary |> left_join(bootstrap_summary, by = summary_groups)
rm(split_groups, bootstrap_tasks, bootstrap_results, participant_counts)
invisible(gc(FALSE))
'''
rq1 = replace_once(rq1, summary_old, summary_new, "duration-type sufficient-statistic bootstrap patch")

# Remaining result tables consume only checkpointed components. These fragments
# are already projected to the scientific duration comparison level.
rq1 = replace_once(
    rq1,
    'anchor_projection <- map_dfr(fragments, "anchor") |>',
    'anchor_projection <- read_rq1_fragment_component("anchor") |>',
    "anchor component patch",
)
rq1 = replace_once(
    rq1,
    'availability <- map_dfr(fragments, "availability") |>',
    'availability <- read_rq1_fragment_component("availability") |>',
    "availability component patch",
)
rq1 = replace_once(
    rq1,
    'participant_balanced <- map_dfr(fragments, "participant") |>',
    'participant_balanced <- read_rq1_fragment_component("participant") |>',
    "participant component patch",
)
rq1 = replace_once(
    rq1,
    'robust <- map_dfr(fragments, "robust") |>',
    'robust <- read_rq1_fragment_component("robust") |>',
    "robust component patch",
)

# Ad-hoc optimized RQ1 invocations use the same tested server default as the
# wrapper scripts unless the environment explicitly overrides it.
rq1 = rq1.replace(
    'FRAGMENT_WORKERS <- ms_resolve_workers("RQ1_FRAGMENT_WORKERS", default = min(16L, PART_WORKERS), cap = 24L)',
    'FRAGMENT_WORKERS <- ms_resolve_workers("RQ1_FRAGMENT_WORKERS", default = min(10L, PART_WORKERS), cap = 16L)',
    1,
)

TARGET.write_text(rq1, encoding="utf-8")
print(TARGET.relative_to(ROOT))
print("RQ1 memory-safe duration-type summary patch applied")
