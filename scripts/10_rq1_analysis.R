suppressPackageStartupMessages(library(tidyverse))
source("scripts/utils/protocol_windows.R")
source("scripts/utils/paths.R")
source("scripts/utils/duration_artifacts.R")
source("scripts/utils/parallel_runtime.R")
source("scripts/utils/rq1_pairwise_artifacts.R")

# RQ1 canonical object: pairwise representation change between two observed
# configuration states.  Orientation is unique: state_a is the less demanding
# state for ordered dimensions (coarser temporal sampling / shorter duration),
# state_b is the more demanding state; delta = value_a - value_b.  Placement and
# optical pairs use the explicit left-to-right facet orientation documented below.
CORE_ROOT <- file.path("results", "core")
CORE_METRICS <- file.path(CORE_ROOT, "metric_cube.csv.gz")
DURATION_CUBE <- file.path(CORE_ROOT, "duration_metric_cube.rds")
DURATION_MANIFEST <- file.path(CORE_ROOT, "duration_window_manifest.rds")
OUT <- file.path("results", "rq1")
DIAG <- file.path("results", "diagnostics")
B_BOOT <- suppressWarnings(as.integer(Sys.getenv("RQ1_BOOT", unset = "1000")))
if (!is.finite(B_BOOT) || B_BOOT < 0L) B_BOOT <- 1000L
BOOT_WORKERS <- ms_resolve_workers("RQ1_BOOT_WORKERS", default = 1L, cap = 48L)
rq1_default_part_workers <- function() {
  logical <- suppressWarnings(parallel::detectCores(logical = TRUE))
  physical <- suppressWarnings(parallel::detectCores(logical = FALSE))
  detected <- if (is.finite(physical) && physical > 0) physical else logical
  if (!is.finite(detected) || detected < 1) return(1L)
  max(1L, min(24L, as.integer(floor(detected / 2))))
}
PART_WORKERS <- ms_resolve_workers("RQ1_PART_WORKERS", default = rq1_default_part_workers(), cap = 48L)
BOOT_SEED <- 20260820L
PRIMARY_TEMPORAL_S <- c(10L, 20L, 30L, 60L, 300L, 900L, 1800L)
DUAL_CHANNEL_METRICS <- c("MDER", "nvRD")
ISIV_METRICS <- c("interdaily_stability", "intradaily_variability")
NUMERIC_TOL <- 1e-12

for (p in c(CORE_METRICS, DURATION_CUBE, DURATION_MANIFEST)) if (!file.exists(p)) stop("Missing required core artifact: ", p)
ensure_result_dirs(OUT, DIAG)

cube <- readr::read_csv(CORE_METRICS, show_col_types = FALSE, progress = FALSE) |>
  mutate(Date = as.Date(Date))
duration_artifact <- readRDS(DURATION_CUBE)
duration_part_paths <- if (duration_cube_is_partitioned(duration_artifact)) {
  file.path(duration_artifact$part_dir, duration_artifact$parts)
} else {
  character()
}
duration_manifest <- readRDS(DURATION_MANIFEST) |>
  mutate(window_start = as.Date(window_start), window_end = as.Date(window_end))
core_versions <- unique(na.omit(c(cube$core_artifact_version, duration_artifact$core_artifact_version)))
if (length(core_versions) != 1L) stop("Core artifact version mismatch")
CORE_VERSION <- core_versions[[1]]
if (any(cube$resolution_s == 15L)) stop("15-s state detected")
if (!all(PRIMARY_TEMPORAL_S %in% unique(cube$resolution_s))) stop("Primary temporal state missing")
metric_meta <- cube |> distinct(metric, metric_class, metric_scope, metric_geometry)
if (n_distinct(metric_meta$metric) != 54L) stop("Expected 54 target metrics")
RQ1_ANALYSIS_VERSION <- paste0("rq1_v5_adaptive_partitioned_pairwise_config_keyed__", CORE_VERSION)

temporal_label <- function(x) case_when(
  x < 60L ~ paste0(x, " s"), x %% 60L == 0L ~ paste0(x %/% 60L, " min"), TRUE ~ paste0(x, " s")
)
circular_delta <- function(a, b, period = 86400) ((a - b + period / 2) %% period) - period / 2
circular_mean <- function(x, period = 86400) {
  x <- x[is.finite(x)]; if (!length(x)) return(NA_real_)
  th <- 2 * pi * x / period
  (atan2(mean(sin(th)), mean(cos(th))) %% (2 * pi)) * period / (2 * pi)
}
scale_primary <- function(x, geometry) {
  x <- x[is.finite(x)]; if (length(x) < 2L) return(NA_real_)
  if (identical(geometry, "circular_time")) stats::sd(circular_delta(x, circular_mean(x))) else stats::sd(x)
}
scale_robust <- function(x, geometry) {
  x <- x[is.finite(x)]; if (length(x) < 2L) return(NA_real_)
  if (identical(geometry, "circular_time")) x <- circular_delta(x, circular_mean(x))
  s <- stats::IQR(x, na.rm = TRUE, type = 7) / 1.349
  if (is.finite(s) && s > sqrt(.Machine$double.eps)) s else NA_real_
}
safe_q <- function(x, p) { x <- x[is.finite(x)]; if (length(x)) unname(stats::quantile(x, p, names = FALSE)) else NA_real_ }

bootstrap_pair_group <- function(task) {
  group <- task$group
  reps <- task$reps
  participants <- unique(group[c("site", "Id")])
  if (!reps || !nrow(group) || !nrow(participants)) {
    return(tibble(A_boot_q025 = NA_real_, A_boot_q975 = NA_real_,
                  B_boot_q025 = NA_real_, B_boot_q975 = NA_real_,
                  bootstrap_reps = 0L, bootstrap_sites = n_distinct(group$site),
                  bootstrap_participants = nrow(participants)))
  }
  set.seed(task$seed)
  a <- b <- rep(NA_real_, reps)
  for (j in seq_len(reps)) {
    sampled <- split(participants$Id, participants$site) |>
      lapply(function(ids) sample(ids, length(ids), replace = TRUE))
    sampled <- tibble(site = rep(names(sampled), lengths(sampled)), Id = unlist(sampled, use.names = FALSE))
    boot <- merge(sampled, group, by = c("site", "Id"), all = FALSE, sort = FALSE)
    z <- boot$z[is.finite(boot$z)]
    if (length(z)) { a[[j]] <- mean(abs(z)); b[[j]] <- mean(z) }
  }
  tibble(
    A_boot_q025 = safe_q(a, .025), A_boot_q975 = safe_q(a, .975),
    B_boot_q025 = safe_q(b, .025), B_boot_q975 = safe_q(b, .975),
    bootstrap_reps = sum(is.finite(a) & is.finite(b)),
    bootstrap_sites = n_distinct(group$site), bootstrap_participants = nrow(participants)
  )
}

choose_metric_support <- function(df, medi_support, full_support) {
  df |> filter((metric %in% DUAL_CHANNEL_METRICS & support_id == full_support) |
                 (!metric %in% DUAL_CHANNEL_METRICS & support_id == medi_support))
}

pair_configuration_values <- function(state_a, state_b, dimension, lattice, pair_id,
                                      config_a_label, config_b_label,
                                      ordered_dimension = FALSE, adjacent = FALSE,
                                      anchor_projection = FALSE, relation = "unordered") {
  keys <- c("support_id", "site", "Id", "analysis_unit_type", "Date", "metric")
  a <- state_a |>
    select(all_of(keys), metric_class, metric_scope, metric_geometry,
           analysis_unit_id_a = analysis_unit_id, config_a_id = config_id, value_a = value, available_a = available,
           unavailable_reason_a = unavailable_reason)
  b <- state_b |>
    select(all_of(keys), analysis_unit_id_b = analysis_unit_id, config_b_id = config_id, value_b = value, available_b = available,
           unavailable_reason_b = unavailable_reason)
  inner_join(a, b, by = keys, relationship = "many-to-many") |>
    mutate(
      dimension = dimension, comparison_lattice = lattice, comparison_pair_id = pair_id,
      config_a_label = config_a_label, config_b_label = config_b_label,
      ordered_dimension = ordered_dimension, adjacent_transition = adjacent,
      anchor_projection = anchor_projection, requirement_relation = relation,
      window_id_a = NA_character_, window_id_b = NA_character_,
      n_days_a = NA_integer_, n_days_b = NA_integer_,
      pair_available = coalesce(available_a, FALSE) & coalesce(available_b, FALSE) &
        is.finite(value_a) & is.finite(value_b),
      pair_unavailable_reason = case_when(
        !coalesce(available_a, FALSE) | !is.finite(value_a) ~ coalesce(unavailable_reason_a, "state_a unavailable"),
        !coalesce(available_b, FALSE) | !is.finite(value_b) ~ coalesce(unavailable_reason_b, "state_b unavailable"),
        TRUE ~ NA_character_
      )
    ) |>
    select(
      dimension, comparison_lattice, comparison_pair_id, config_a_id, config_b_id,
      config_a_label, config_b_label, ordered_dimension, adjacent_transition,
      anchor_projection, requirement_relation, support_id, site, Id, analysis_unit_type,
      analysis_unit_id_a, analysis_unit_id_b, Date, window_id_a, window_id_b, n_days_a, n_days_b,
      metric, metric_class, metric_scope, metric_geometry, value_a, value_b,
      available_a, available_b, pair_available, pair_unavailable_reason
    )
}

placement_pairs <- map_dfr(c("chest", "wrist"), function(pos) {
  medi_support <- paste0("eye_", pos, "_medi"); full_support <- paste0("eye_", pos, "_full")
  z <- cube |>
    filter(support_id %in% c(medi_support, full_support), placement %in% c("eye", pos), optical == "MEDI", resolution_s == 10L) |>
    choose_metric_support(medi_support, full_support)
  pair_configuration_values(
    z |> filter(placement == pos), z |> filter(placement == "eye"), "placement",
    paste0("placement_", pos), paste0(pos, "_vs_eye"), stringr::str_to_title(pos), "Eye",
    relation = "unordered_facet"
  )
})

optical_base <- cube |> filter(support_id == "eye_full", placement == "eye", optical %in% c("MEDI", "LIGHT"), resolution_s == 10L)
optical_pairs <- pair_configuration_values(
  optical_base |> filter(optical == "LIGHT"), optical_base |> filter(optical == "MEDI"),
  "optical", "optical", "LIGHT_vs_MEDI", "LIGHT", "MEDI", relation = "unordered_facet"
)

temporal_pairs <- map_dfr(combn(PRIMARY_TEMPORAL_S, 2L, simplify = FALSE), function(x) {
  coarse <- max(x); fine <- min(x); adjacent <- abs(match(coarse, PRIMARY_TEMPORAL_S) - match(fine, PRIMARY_TEMPORAL_S)) == 1L
  z <- cube |>
    filter(support_id %in% c("eye_medi", "eye_full"), placement == "eye", optical == "MEDI", resolution_s %in% x) |>
    choose_metric_support("eye_medi", "eye_full")
  pair_configuration_values(
    z |> filter(resolution_s == coarse), z |> filter(resolution_s == fine), "temporal", "temporal",
    paste0(coarse, "s_vs_", fine, "s"), temporal_label(coarse), temporal_label(fine),
    ordered_dimension = TRUE, adjacent = adjacent, anchor_projection = fine == 10L,
    relation = "a_coarser_than_b"
  )
})

duration_window_pairs <- duration_manifest |>
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

build_duration_pair_chunk <- function(duration_values, window_pairs) {
  if (!nrow(duration_values) || !nrow(window_pairs)) return(tibble())
  duration_a <- window_pairs |>
    select(support_id, site, Id, window_a, window_b, n_days_a, n_days_b, start_a, end_a,
           start_b, end_b, adjacent_transition, pair_id) |>
    inner_join(duration_values |> rename(window_a = window_id),
               by = c("support_id", "site", "Id", "window_a"), relationship = "many-to-many") |>
    transmute(
      support_id, site, Id, window_a, window_b, n_days_a, n_days_b, start_a, end_a, start_b, end_b,
      adjacent_transition, pair_id, config_id, metric, metric_class, metric_scope, metric_geometry,
      placement, optical, resolution_s, analysis_unit_id_a = analysis_unit_id, value_a = value,
      available_a = available, unavailable_reason_a = unavailable_reason
    )
  duration_b <- window_pairs |>
    select(support_id, site, Id, window_a, window_b, pair_id) |>
    inner_join(duration_values |> rename(window_b = window_id),
               by = c("support_id", "site", "Id", "window_b"), relationship = "many-to-many") |>
    transmute(
      support_id, site, Id, window_a, window_b, pair_id, config_id, metric,
      analysis_unit_id_b = analysis_unit_id, value_b = value,
      available_b = available, unavailable_reason_b = unavailable_reason
    )
  inner_join(
    duration_a, duration_b,
    by = c("support_id", "site", "Id", "window_a", "window_b", "pair_id", "config_id", "metric"),
    relationship = "many-to-many"
  ) |>
    transmute(
      dimension = "duration", comparison_lattice = "duration", comparison_pair_id = pair_id,
      config_a_id = paste0(config_id, "__", window_a), config_b_id = paste0(config_id, "__", window_b),
      config_a_label = paste0(n_days_a, " d (", as.character(start_a), "–", as.character(end_a), ")"),
      config_b_label = paste0(n_days_b, " d (", as.character(start_b), "–", as.character(end_b), ")"),
      ordered_dimension = TRUE, adjacent_transition, anchor_projection = n_days_b == 6L,
      requirement_relation = "a_shorter_than_b", support_id, site, Id,
      analysis_unit_type = "participant_window", analysis_unit_id_a, analysis_unit_id_b, Date = as.Date(NA),
      window_id_a = window_a, window_id_b = window_b, n_days_a = as.integer(n_days_a), n_days_b = as.integer(n_days_b),
      metric, metric_class, metric_scope, metric_geometry, value_a, value_b,
      available_a, available_b,
      pair_available = coalesce(available_a, FALSE) & coalesce(available_b, FALSE) & is.finite(value_a) & is.finite(value_b),
      pair_unavailable_reason = case_when(
        !coalesce(available_a, FALSE) | !is.finite(value_a) ~ coalesce(unavailable_reason_a, "state_a unavailable"),
        !coalesce(available_b, FALSE) | !is.finite(value_b) ~ coalesce(unavailable_reason_b, "state_b unavailable"),
        TRUE ~ NA_character_
      )
    )
}

# Duration blocks are large.  Keep only one support/site block in each worker;
# never bind all duration pairwise rows in the master process.  The standardizer
# is computed once from the frozen anchor states and then applied independently
# to every immutable RQ1 part.
duration_columns <- c("support_id", "site", "Id", "window_id", "config_id", "metric", "metric_class",
                      "metric_scope", "metric_geometry", "placement", "optical", "resolution_s",
                      "analysis_unit_id", "value", "available", "unavailable_reason")
duration_anchor_values <- if (length(duration_part_paths)) {
  map_dfr(duration_part_paths, function(part_path) {
    readRDS(part_path) |>
      filter(n_days == 6L) |>
      select(metric, metric_geometry, value)
  })
} else {
  duration_artifact |>
    filter(n_days == 6L) |>
    select(metric, metric_geometry, value)
}

# One standardizer per comparison lattice x metric. The pair relation and scale
# anchor are distinct objects: every pair below joins the same denominator.
anchor_values <- bind_rows(
  map_dfr(c("chest", "wrist"), function(pos) {
    medi_support <- paste0("eye_", pos, "_medi")
    full_support <- paste0("eye_", pos, "_full")
    cube |>
      filter(support_id %in% c(medi_support, full_support), placement == "eye", optical == "MEDI", resolution_s == 10L) |>
      choose_metric_support(medi_support, full_support) |>
      transmute(comparison_lattice = paste0("placement_", pos), metric, metric_geometry, value, scale_anchor_config = "eye_state")
  }),
  cube |> filter(support_id == "eye_full", placement == "eye", optical == "MEDI", resolution_s == 10L) |>
    transmute(comparison_lattice = "optical", metric, metric_geometry, value, scale_anchor_config = "MEDI_state"),
  cube |> filter(support_id %in% c("eye_medi", "eye_full"), placement == "eye", optical == "MEDI", resolution_s == 10L) |>
    choose_metric_support("eye_medi", "eye_full") |>
    transmute(comparison_lattice = "temporal", metric, metric_geometry, value, scale_anchor_config = "eye__MEDI__10s"),
  duration_anchor_values |>
    transmute(comparison_lattice = "duration", metric, metric_geometry, value, scale_anchor_config = "longest_observed_window_in_run")
) |>
  filter(is.finite(value))
standardizers <- anchor_values |>
  group_by(comparison_lattice, metric, metric_geometry, scale_anchor_config) |>
  summarise(
    n_scale_units = n(), standardizer = scale_primary(value, first(metric_geometry)),
    robust_standardizer = scale_robust(value, first(metric_geometry)), .groups = "drop"
  ) |>
  mutate(zero_or_near_zero = !is.finite(standardizer) | standardizer <= sqrt(.Machine$double.eps))
readr::write_csv(standardizers, file.path(DIAG, "rq1_standardizer_audit.csv"), na = "")

summary_groups <- c("dimension", "comparison_lattice", "comparison_pair_id", "config_a_id", "config_b_id",
                    "config_a_label", "config_b_label", "ordered_dimension", "adjacent_transition",
                    "anchor_projection", "requirement_relation", "metric", "metric_class", "metric_geometry")

canonicalize_pairs <- function(raw) {
  if (!nrow(raw)) return(tibble())
  out <- raw |>
    mutate(
      pair_key = paste(dimension, comparison_lattice, comparison_pair_id, config_a_id, config_b_id,
                       support_id, site, Id, analysis_unit_id_a, analysis_unit_id_b, metric, sep = "|"),
      scale_anchor_config = case_when(
        dimension == "temporal" ~ "eye__MEDI__10s",
        dimension == "duration" ~ "longest_observed_window_in_run",
        dimension == "placement" ~ "eye_state",
        dimension == "optical" ~ "MEDI_state",
        TRUE ~ NA_character_
      )
    ) |>
    left_join(standardizers, by = c("comparison_lattice", "metric", "scale_anchor_config", "metric_geometry")) |>
    mutate(
      delta = if_else(metric_geometry == "circular_time", circular_delta(value_a, value_b), value_a - value_b),
      available = pair_available & !coalesce(zero_or_near_zero, TRUE) & is.finite(delta) & is.finite(standardizer),
      unavailable_reason = case_when(
        !pair_available ~ pair_unavailable_reason,
        coalesce(zero_or_near_zero, TRUE) ~ "scale anchor dispersion zero or undefined",
        !is.finite(delta) ~ "representation difference undefined", TRUE ~ NA_character_
      ),
      z = if_else(available, delta / standardizer, NA_real_),
      robust_z = if_else(pair_available & is.finite(delta) & is.finite(robust_standardizer) & robust_standardizer > 0,
                         delta / robust_standardizer, NA_real_),
      core_artifact_version = CORE_VERSION, rq1_analysis_version = RQ1_ANALYSIS_VERSION
    ) |>
    select(core_artifact_version, rq1_analysis_version, pair_key, dimension, comparison_lattice,
           comparison_pair_id, config_a_id, config_b_id, config_a_label, config_b_label,
           ordered_dimension, adjacent_transition, anchor_projection, requirement_relation,
           scale_anchor_config, support_id, site, Id, analysis_unit_type,
           analysis_unit_id_a, analysis_unit_id_b, Date, window_id_a, window_id_b, n_days_a, n_days_b,
           metric, metric_class, metric_scope, metric_geometry, value_a, value_b, delta, z, robust_z,
           available_a, available_b, pair_available, available, unavailable_reason)
  duplicate_rows <- which(duplicated(out$pair_key) | duplicated(out$pair_key, fromLast = TRUE))
  if (length(duplicate_rows)) {
    sample_keys <- unique(out$pair_key[duplicate_rows])[seq_len(min(5L, length(unique(out$pair_key[duplicate_rows]))))]
    stop("Duplicate RQ1 scientific pair keys within part: ", paste(sample_keys, collapse = " || "))
  }
  out
}

rq1_marker_rows <- function(path) {
  ok <- paste0(path, ".ok")
  if (!file.exists(path) || !file.exists(ok)) return(NA_integer_)
  lines <- readLines(ok, warn = FALSE)
  hit <- lines[grepl("^rows=", lines)]
  if (!length(hit)) return(NA_integer_)
  suppressWarnings(as.integer(sub("^rows=", "", hit[[1]])))
}

rq1_process_duration_part <- function(task) {
  existing <- rq1_marker_rows(task$part_path)
  if (is.finite(existing)) return(tibble(part_index = task$part_index, part = basename(task$part_path), dimension = "duration", rows = existing, status = "reused"))
  values <- readRDS(task$duration_path) |> select(all_of(duration_columns))
  keys <- values |> distinct(support_id, site)
  window_pairs <- duration_window_pairs |> semi_join(keys, by = c("support_id", "site"))
  raw <- build_duration_pair_chunk(values, window_pairs)
  rm(values, keys, window_pairs)
  invisible(gc(FALSE))
  canonical <- canonicalize_pairs(raw)
  rm(raw)
  rq1_write_part_atomic(canonical, task$part_path)
  rows <- nrow(canonical)
  rm(canonical)
  invisible(gc(FALSE))
  tibble(part_index = task$part_index, part = basename(task$part_path), dimension = "duration", rows = rows, status = "written")
}

pairwise_part_dir <- file.path(OUT, "pairwise_parts", RQ1_ANALYSIS_VERSION)
dir.create(pairwise_part_dir, recursive = TRUE, showWarnings = FALSE)
non_duration_raw <- tibble::as_tibble(data.table::rbindlist(
  list(placement_pairs, optical_pairs, temporal_pairs), use.names = TRUE, fill = TRUE
))
if (!nrow(non_duration_raw)) stop("No non-duration pairwise configuration changes were constructed")
non_duration_path <- file.path(pairwise_part_dir, "rq1_pairwise_part_000.rds")
non_duration_rows <- rq1_marker_rows(non_duration_path)
if (!is.finite(non_duration_rows)) {
  non_duration_canonical <- canonicalize_pairs(non_duration_raw)
  rq1_write_part_atomic(non_duration_canonical, non_duration_path)
  non_duration_rows <- nrow(non_duration_canonical)
  rm(non_duration_canonical)
}
rm(non_duration_raw, placement_pairs, optical_pairs, temporal_pairs)
invisible(gc())

duration_tasks <- if (length(duration_part_paths)) {
  map2(duration_part_paths, seq_along(duration_part_paths), function(path, i) {
    list(duration_path = path, part_path = file.path(pairwise_part_dir, sprintf("rq1_pairwise_part_%03d.rds", i)), part_index = i)
  })
} else list()
pending_duration <- duration_tasks[!vapply(duration_tasks, function(task) is.finite(rq1_marker_rows(task$part_path)), logical(1))]
if (length(pending_duration)) {
  message("RQ1 duration parts pending: ", length(pending_duration), "/", length(duration_tasks), "; workers=", PART_WORKERS)
  rq1_part_results <- ms_parallel_map(
    pending_duration, rq1_process_duration_part, workers = PART_WORKERS, seed = BOOT_SEED,
    packages = c("tidyverse", "data.table"),
    exports = c("duration_columns", "duration_window_pairs", "standardizers", "CORE_VERSION",
                "RQ1_ANALYSIS_VERSION", "build_duration_pair_chunk", "canonicalize_pairs",
                "rq1_write_part_atomic", "rq1_marker_rows", "rq1_process_duration_part",
                "circular_delta")
  )
} else rq1_part_results <- list()

all_part_records <- bind_rows(
  tibble(part_index = 0L, part = basename(non_duration_path), dimension = "placement_optical_temporal",
         rows = non_duration_rows, status = if (is.finite(non_duration_rows)) "complete" else "missing"),
  map_dfr(seq_along(duration_tasks), function(i) {
    task <- duration_tasks[[i]]
    hit <- rq1_part_results[vapply(rq1_part_results, function(z) identical(z$part_index, task$part_index), logical(1))]
    if (length(hit)) return(hit[[1]])
    tibble(part_index = task$part_index, part = basename(task$part_path), dimension = "duration",
           rows = rq1_marker_rows(task$part_path), status = "reused")
  })
) |>
  arrange(part_index)
if (any(!is.finite(all_part_records$rows))) stop("RQ1 part manifest contains incomplete parts")
readr::write_csv(all_part_records, file.path(pairwise_part_dir, "part_manifest.csv"), na = "")

pairwise_manifest <- list(
  artifact_type = "partitioned_rq1_pairwise_change",
  artifact_version = RQ1_ANALYSIS_VERSION,
  core_artifact_version = CORE_VERSION,
  rq1_analysis_version = RQ1_ANALYSIS_VERSION,
  part_dir = pairwise_part_dir,
  parts = all_part_records$part,
  part_manifest = all_part_records,
  generated_at = format(Sys.time(), tz = "UTC", usetz = TRUE)
)
saveRDS(pairwise_manifest, file.path(OUT, "rq1_pairwise_change_long.rds"), compress = "xz")

part_paths <- file.path(pairwise_part_dir, all_part_records$part)
make_rq1_fragments <- function(part_path) {
  canonical <- readRDS(part_path)
  x <- canonical |> filter(available, is.finite(z))
  summary_fragment <- x |>
    group_by(across(all_of(summary_groups))) |>
    summarise(z_values = list(as.numeric(z)), site_values = list(as.character(site)), id_values = list(as.character(Id)), .groups = "drop")
  anchor_fragment <- canonical |> filter(anchor_projection) |>
    group_by(across(all_of(summary_groups))) |>
    summarise(n_total_units = n(), n_available_units = sum(available),
              participant_keys = list(unique(paste(site, Id, sep = "|"))),
              A_sum = sum(abs(z[available & is.finite(z)])), B_sum = sum(z[available & is.finite(z)]), .groups = "drop")
  availability_fragment <- canonical |>
    group_by(dimension, comparison_lattice, comparison_pair_id, config_a_id, config_b_id, metric, metric_class) |>
    summarise(n_total_units = n(), n_available_units = sum(available),
              participant_keys = list(unique(paste(site, Id, sep = "|"))),
              available_participant_keys = list(unique(paste(site[available], Id[available], sep = "|"))),
              representation_available = any(available), unavailable_reasons = list(sort(unique(na.omit(unavailable_reason)))), .groups = "drop")
  participant_fragment <- x |>
    group_by(across(all_of(summary_groups)), site, Id) |>
    summarise(n_units = n(), sum_abs = sum(abs(z)), sum_signed = sum(z), .groups = "drop")
  robust_fragment <- canonical |> filter(pair_available, is.finite(robust_z)) |>
    group_by(across(all_of(summary_groups))) |>
    summarise(sum_abs = sum(abs(robust_z)), sum_signed = sum(robust_z), n_units = n(), .groups = "drop")
  rm(canonical, x)
  invisible(gc(FALSE))
  list(summary = summary_fragment, anchor = anchor_fragment, availability = availability_fragment,
       participant = participant_fragment, robust = robust_fragment)
}
message("RQ1 streaming summaries over ", length(part_paths), " immutable parts")
fragments <- map(part_paths, make_rq1_fragments)
summary_fragments <- map_dfr(fragments, "summary")
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
if (any(summary$A_mean_absolute + NUMERIC_TOL < abs(summary$B_mean_signed))) stop("RQ1 A >= |B| invariant failed")
readr::write_csv(summary, file.path(OUT, "rq1_pairwise_summary.csv"), na = "")
readr::write_csv(summary, file.path(OUT, "rq1_summary.csv"), na = "")
readr::write_csv(bootstrap_summary, file.path(OUT, "rq1_pairwise_bootstrap.csv"), na = "")

anchor_projection <- map_dfr(fragments, "anchor") |>
  group_by(across(all_of(summary_groups))) |>
  summarise(n_participants = n_distinct(unlist(participant_keys)), n_units = sum(n_available_units),
            A = if (sum(n_available_units) > 0) sum(A_sum) / sum(n_available_units) else NA_real_,
            B = if (sum(n_available_units) > 0) sum(B_sum) / sum(n_available_units) else NA_real_, .groups = "drop")
readr::write_csv(anchor_projection, file.path(OUT, "rq1_anchor_projection.csv"), na = "")
local_summary <- summary |> filter(adjacent_transition) |>
  transmute(metric, metric_class, dimension, comparison_lattice, lower_level = config_a_label,
            higher_level = config_b_label, config_a_id, config_b_id, adjacent_transition,
            G = A_mean_absolute, A = A_mean_absolute, B = B_mean_signed,
            n_participants, n_units, core_artifact_version, rq1_analysis_version)
readr::write_csv(local_summary, file.path(OUT, "rq1_local_transition_summary.csv"), na = "")

availability <- map_dfr(fragments, "availability") |>
  group_by(dimension, comparison_lattice, comparison_pair_id, config_a_id, config_b_id, metric, metric_class) |>
  summarise(n_total_units = sum(n_total_units), n_available_units = sum(n_available_units),
            n_participants_total = n_distinct(unlist(participant_keys)),
            n_participants_available = n_distinct(unlist(available_participant_keys)),
            representation_available = any(representation_available),
            unavailable_reason = paste(sort(unique(unlist(unavailable_reasons))), collapse = "; "), .groups = "drop") |>
  mutate(unavailable_reason = na_if(unavailable_reason, ""))
readr::write_csv(availability, file.path(OUT, "rq1_metric_availability.csv"), na = "")

participant_balanced <- map_dfr(fragments, "participant") |>
  group_by(across(all_of(summary_groups)), site, Id) |>
  summarise(n_units = sum(n_units), sum_abs = sum(sum_abs), sum_signed = sum(sum_signed), .groups = "drop") |>
  mutate(A_participant = sum_abs / n_units, B_participant = sum_signed / n_units) |>
  group_by(across(all_of(summary_groups))) |>
  summarise(A_participant_balanced = mean(A_participant), B_participant_balanced = mean(B_participant),
            n_participants = n(), .groups = "drop")
readr::write_csv(participant_balanced, file.path(OUT, "rq1_participant_balanced_sensitivity.csv"), na = "")
robust <- map_dfr(fragments, "robust") |>
  group_by(across(all_of(summary_groups))) |>
  summarise(A_robust = sum(sum_abs) / sum(n_units), B_robust = sum(sum_signed) / sum(n_units),
            n_units = sum(n_units), .groups = "drop")
readr::write_csv(robust, file.path(OUT, "rq1_robust_scale_sensitivity.csv"), na = "")

readr::write_csv(duration_cohort_audit(duration_manifest), file.path(DIAG, "duration_cohort_audit.csv"), na = "")
readr::write_csv(duration_manifest |>
  mutate(member_dates = map_chr(member_dates, ~paste(as.character(.x), collapse = ";"))),
  file.path(DIAG, "duration_window_manifest_audit.csv"), na = "")
pair_counts <- summary |>
  group_by(dimension, metric) |>
  summarise(n_pair_types = n_distinct(comparison_pair_id), n_available_pair_types = n_distinct(comparison_pair_id[A_mean_absolute >= 0]), .groups = "drop")
readr::write_csv(pair_counts, file.path(OUT, "rq1_pair_type_counts.csv"), na = "")
geom_audit <- summary |> transmute(dimension, comparison_pair_id, metric, A = A_mean_absolute, B = B_mean_signed,
                                   gap = A - abs(B), pass = A + NUMERIC_TOL >= abs(B))
readr::write_csv(geom_audit, file.path(DIAG, "rq1_geometry_invariant.csv"), na = "")

writeLines(c(
  "# RQ1 run report", "", paste0("Core artifact version: ", CORE_VERSION), paste0("RQ1 analysis version: ", RQ1_ANALYSIS_VERSION),
  paste0("Pairwise change rows: ", sum(all_part_records$rows)), paste0("Finite/available rows: ", sum(summary$n_units)),
  paste0("Canonical storage: ", normalizePath(file.path(OUT, "rq1_pairwise_change_long.rds"), winslash = "/", mustWork = FALSE)),
  paste0("Pairwise part count: ", nrow(all_part_records), "; duration part workers: ", PART_WORKERS, "; bootstrap workers: ", BOOT_WORKERS),
  "Canonical orientation: state_a is the less demanding state for ordered dimensions; delta = value_a - value_b.",
  "Temporal map: all 21 pair types among 10, 20, 30, 60, 300, 900, 1800 s; adjacent transitions are separately flagged.",
  "Duration map: all nested windows within consecutive complete-analysis-day runs, limited to 1-6 d; no protocol seven-day reference.",
  "The 10-s temporal and longest observed duration states are scale anchors only; they are not forced into the pair ontology.",
  "Placement and optical comparisons are unordered empirical facets, not burden-ordered sufficiency dimensions."
), file.path(OUT, "RQ1_RUN_REPORT.md"))
message("RQ1 complete: ", RQ1_ANALYSIS_VERSION, "; rows=", sum(all_part_records$rows), "; parts=", nrow(all_part_records))
