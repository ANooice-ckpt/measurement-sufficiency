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

# The old 'streaming' implementation returned large list-column fragments to
# the master process and retained all of them in RAM. Replace it with durable
# per-part sufficient-statistic checkpoints. Workers return only file paths.
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
fragment_new = '''fragment_dir <- file.path(pairwise_part_dir, "summary_fragments_sufficient_v1")
dir.create(fragment_dir, recursive = TRUE, showWarnings = FALSE)

fragment_path_for <- function(part_path) {
  file.path(
    fragment_dir,
    paste0(tools::file_path_sans_ext(basename(part_path)), "__summary_fragment.rds")
  )
}

make_rq1_fragment_checkpoint <- function(part_path) {
  fragment_path <- fragment_path_for(part_path)
  marker_path <- paste0(fragment_path, ".ok")
  if (file.exists(fragment_path) && file.exists(marker_path)) return(fragment_path)

  canonical <- readRDS(part_path)
  x <- canonical |> filter(available, is.finite(z))

  # Exact per-scientific-pair summaries. Unlike the previous implementation,
  # no raw z/site/Id vectors are retained in list columns.
  summary_fragment <- x |>
    group_by(across(all_of(summary_groups))) |>
    summarise(
      n_participants = n_distinct(paste(site, Id, sep = "|")),
      n_units = n(),
      median_z = safe_q(z, .5), q25_z = safe_q(z, .25), q75_z = safe_q(z, .75),
      p025_z = safe_q(z, .025), p975_z = safe_q(z, .975),
      B_mean_signed = mean(z), A_mean_absolute = mean(abs(z)),
      .groups = "drop"
    )

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

  # These are exact sufficient statistics for the existing participant-cluster
  # bootstrap: a resampled participant contributes its n, sum|z| and sum(z).
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
  writeLines("rq1_summary_fragment_sufficient_v1", marker_path, useBytes = TRUE)
  rm(canonical, x, summary_fragment, anchor_fragment, availability_fragment,
     participant_fragment, robust_fragment, out)
  invisible(gc(FALSE))
  fragment_path
}

message(
  "RQ1 memory-safe summary checkpoints over ", length(part_paths),
  " immutable parts; workers=", FRAGMENT_WORKERS
)
fragment_cost <- as.numeric(file.info(part_paths)$size)
fragment_schedule_idx <- order(fragment_cost, decreasing = TRUE, na.last = TRUE)
fragment_paths_scheduled <- ms_parallel_map(
  part_paths[fragment_schedule_idx], make_rq1_fragment_checkpoint,
  workers = FRAGMENT_WORKERS,
  packages = c("tidyverse"),
  exports = c("summary_groups", "fragment_dir", "fragment_path_for",
              "make_rq1_fragment_checkpoint", "safe_q")
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
rq1 = replace_once(rq1, fragment_old, fragment_new, "memory-safe fragment checkpoint patch")

# Replace raw-vector aggregation + raw-unit bootstrap with scalar summaries and
# an exactly equivalent participant sufficient-statistic bootstrap.
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
summary_new = '''summary_base <- read_rq1_fragment_component("summary") |>
  arrange(across(all_of(summary_groups))) |>
  mutate(.summary_index = row_number())

# With the frozen artifact construction, each scientific summary key is owned by
# exactly one immutable pairwise part (all non-duration rows are in part 000;
# duration comparison_pair_id/config ids contain their window identities).
# Fail loudly if that invariant ever changes instead of silently double-counting.
summary_key <- do.call(paste, c(summary_base[summary_groups], sep = "\\r"))
if (anyDuplicated(summary_key)) {
  stop("RQ1 summary key spans multiple immutable parts; memory-safe ownership invariant failed")
}
rm(summary_key)

summary <- summary_base |>
  select(all_of(summary_groups), n_participants, n_units, median_z, q25_z, q75_z, p025_z, p975_z,
         B_mean_signed, A_mean_absolute) |>
  mutate(core_artifact_version = CORE_VERSION, rq1_analysis_version = RQ1_ANALYSIS_VERSION,
         uncertainty_method = if (B_BOOT > 0L) "participant-cluster/site-stratified bootstrap" else "point estimate; bootstrap disabled")

bootstrap_pair_group_sufficient <- function(task) {
  group <- task$group
  reps <- task$reps
  participants <- group |> select(site, Id, n_units, sum_abs, sum_signed)
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
    sampled <- tibble(site = rep(names(sampled), lengths(sampled)),
                      Id = unlist(sampled, use.names = FALSE))
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

participant_fragments <- read_rq1_fragment_component("participant") |>
  inner_join(summary_base |> select(all_of(summary_groups), .summary_index), by = summary_groups)

if (B_BOOT > 0L) {
  # A duration summary key embeds one participant's concrete nested window pair.
  # For a one-participant cluster, resampling with replacement is deterministic;
  # the original bootstrap therefore returns the point estimate at every draw.
  duration_rows <- summary_base |> filter(dimension == "duration")
  if (nrow(duration_rows) && any(duration_rows$n_participants != 1L)) {
    stop("Duration bootstrap one-participant invariant failed")
  }
  duration_bootstrap <- duration_rows |>
    transmute(
      across(all_of(summary_groups)),
      A_boot_q025 = A_mean_absolute, A_boot_q975 = A_mean_absolute,
      B_boot_q025 = B_mean_signed, B_boot_q975 = B_mean_signed,
      bootstrap_reps = B_BOOT, bootstrap_sites = 1L, bootstrap_participants = 1L
    )

  non_duration_rows <- summary_base |> filter(dimension != "duration")
  non_duration_participants <- participant_fragments |> filter(dimension != "duration")
  split_groups <- split(non_duration_participants, non_duration_participants$.summary_index)
  bootstrap_tasks <- lapply(non_duration_rows$.summary_index, function(i) {
    g <- split_groups[[as.character(i)]]
    if (is.null(g)) g <- non_duration_participants[0, , drop = FALSE]
    list(group = g, reps = B_BOOT, seed = BOOT_SEED + i)
  })
  bootstrap_results <- ms_parallel_map(
    bootstrap_tasks, bootstrap_pair_group_sufficient,
    workers = BOOT_WORKERS, seed = BOOT_SEED,
    packages = c("tidyverse"),
    exports = c("bootstrap_pair_group_sufficient", "safe_q")
  )
  non_duration_bootstrap <- bind_rows(map(seq_along(bootstrap_results), function(i) {
    bind_cols(
      non_duration_rows[i, ] |> select(all_of(summary_groups)),
      bootstrap_results[[i]]
    )
  }))
  bootstrap_summary <- bind_rows(duration_bootstrap, non_duration_bootstrap)
} else {
  bootstrap_summary <- summary_base |>
    transmute(
      across(all_of(summary_groups)),
      A_boot_q025 = NA_real_, A_boot_q975 = NA_real_,
      B_boot_q025 = NA_real_, B_boot_q975 = NA_real_,
      bootstrap_reps = 0L, bootstrap_sites = n_participants,
      bootstrap_participants = n_participants
    )
}
summary <- summary |> left_join(bootstrap_summary, by = summary_groups)
rm(participant_fragments)
invisible(gc(FALSE))
'''
rq1 = replace_once(rq1, summary_old, summary_new, "sufficient-statistic bootstrap patch")

# Remaining result tables consume only checkpointed sufficient-statistic
# components, never the canonical pairwise rows or raw z vectors.
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

# The runtime default is intentionally memory-bound, not CPU-bound. The server
# runner also exports 6 explicitly; this protects ad-hoc invocations.
rq1 = rq1.replace(
    'FRAGMENT_WORKERS <- ms_resolve_workers("RQ1_FRAGMENT_WORKERS", default = min(16L, PART_WORKERS), cap = 24L)',
    'FRAGMENT_WORKERS <- ms_resolve_workers("RQ1_FRAGMENT_WORKERS", default = min(6L, PART_WORKERS), cap = 12L)',
    1,
)

TARGET.write_text(rq1, encoding="utf-8")
print(TARGET.relative_to(ROOT))
print("RQ1 memory-safe summary patch applied")
