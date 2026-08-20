suppressPackageStartupMessages({
  library(tidyverse)
  library(LightLogR)
})
source("scripts/utils/melidos_io.R")
source("scripts/utils/core_artifacts.R")
source("scripts/utils/core_temporal_sampling.R")
source("scripts/utils/protocol_windows.R")
source("scripts/utils/rq_context.R")

# Post-core RQ1 context extension.
# Whole-day all-54-metric distortion remains the primary RQ1 object. Context
# analyses start from those same target representations and retain only metrics
# whose operators remain meaningful after context restriction:
#   * civil day/night: continuous-interval-valid metrics;
#   * indoor/outdoor and activity: additive/distributional metrics that can be
#     reconstructed without stitching separated episodes together.
# This script reads cached cleaned support series and never rebuilds core.

RQ1_DISTORTION <- "data/derived/rq1/rq1_distortion_long.rds"
CORE_CONTEXT <- "data/derived/core/unit_context.csv.gz"
OUT_DATA <- "data/derived/rq1"
OUT_RESULTS <- "results/rq1"
OUT_DIAG <- "results/diagnostics"
PRIMARY_TEMPORAL_S <- c(20L, 30L, 60L, 300L, 900L, 1800L)
DUAL_CHANNEL_METRICS <- c("MDER", "nvRD")
B_BOOT <- suppressWarnings(as.integer(Sys.getenv("RQ1_BOOT", unset = "1000")))
if (!is.finite(B_BOOT) || B_BOOT < 0L) B_BOOT <- 1000L
CONTEXT_WORKERS <- suppressWarnings(as.integer(Sys.getenv("RQ1_CONTEXT_WORKERS", unset = "16")))
if (!is.finite(CONTEXT_WORKERS) || CONTEXT_WORKERS < 1L) CONTEXT_WORKERS <- 1L
BOOT_SEED <- 20260822L
Sys.setenv(
  OMP_NUM_THREADS = "1", OPENBLAS_NUM_THREADS = "1", MKL_NUM_THREADS = "1",
  VECLIB_MAXIMUM_THREADS = "1", NUMEXPR_NUM_THREADS = "1"
)

# Cross-platform execution wrapper. Unix-like systems use fork workers; Windows
# uses a PSOCK cluster initialized from the same repository code. Each worker is
# single-threaded internally, so RQ1_CONTEXT_WORKERS controls process-level
# parallelism without nested BLAS/OpenMP oversubscription. Worker exceptions are
# returned explicitly with task identity instead of being allowed to surface as
# secondary bind_rows() failures.
parallel_lapply <- function(X, FUN, workers = CONTEXT_WORKERS,
                            exports = character(), label = "tasks") {
  if (!length(X)) return(list())
  n_workers <- min(max(1L, as.integer(workers)), length(X))
  message(sprintf("RQ1 context [%s]: dispatching %d tasks across %d worker%s",
                  label, length(X), n_workers, ifelse(n_workers == 1L, "", "s")))

  safe_runner <- function(x, fun) {
    tryCatch(
      list(ok = TRUE, value = fun(x), error = NA_character_),
      error = function(e) list(ok = FALSE, value = NULL, error = conditionMessage(e))
    )
  }

  if (n_workers <= 1L) {
    ans <- lapply(X, safe_runner, fun = FUN)
  } else if (.Platform$OS.type != "windows") {
    ans <- parallel::mclapply(
      X, safe_runner, fun = FUN,
      mc.cores = n_workers, mc.preschedule = FALSE, mc.set.seed = TRUE
    )
  } else {
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
        library(LightLogR)
      })
      source("scripts/utils/melidos_io.R")
      source("scripts/utils/core_artifacts.R")
      source("scripts/utils/core_temporal_sampling.R")
      source("scripts/utils/protocol_windows.R")
      source("scripts/utils/rq_context.R")
      assign("annotated_cache", new.env(parent = emptyenv()), envir = .GlobalEnv)
      assign("value_cache", new.env(parent = emptyenv()), envir = .GlobalEnv)
      NULL
    }, root)
    exports <- intersect(unique(exports), ls(envir = .GlobalEnv))
    if (length(exports)) parallel::clusterExport(cl, exports, envir = .GlobalEnv)
    ans <- parallel::parLapplyLB(cl, X, safe_runner, FUN, chunk.size = 1L)
  }

  failed <- which(!vapply(ans, function(z) isTRUE(z$ok), logical(1)))
  if (length(failed)) {
    describe_task <- function(i) {
      task <- X[[i]]
      if (is.list(task) && all(c("site", "support_id") %in% names(task))) {
        paste0(task$site, "__", task$support_id)
      } else {
        paste0("task ", i)
      }
    }
    details <- vapply(
      failed,
      function(i) paste0("  - ", describe_task(i), ": ", ans[[i]]$error),
      character(1)
    )
    stop(
      sprintf("RQ1 context [%s] failed for %d/%d tasks:\n%s",
              label, length(failed), length(X), paste(details, collapse = "\n")),
      call. = FALSE
    )
  }
  message(sprintf("RQ1 context [%s]: completed %d/%d tasks", label, length(ans), length(X)))
  lapply(ans, `[[`, "value")
}

for (p in c(RQ1_DISTORTION, CORE_CONTEXT)) if (!file.exists(p)) stop("Missing required upstream artifact: ", p)
for (d in c(OUT_DATA, OUT_RESULTS, OUT_DIAG)) dir.create(d, recursive = TRUE, showWarnings = FALSE)

rq1 <- readRDS(RQ1_DISTORTION)
rq1_versions <- unique(rq1$rq1_analysis_version); rq1_versions <- rq1_versions[!is.na(rq1_versions)]
core_versions <- unique(rq1$core_artifact_version); core_versions <- core_versions[!is.na(core_versions)]
if (length(rq1_versions) != 1L || length(core_versions) != 1L) stop("RQ1/core version mismatch")
RQ1_VERSION <- rq1_versions[[1]]
CORE_VERSION <- core_versions[[1]]
RQ1_CONTEXT_VERSION <- paste0("rq1_context_v2__", RQ1_VERSION)
SUPPORT_DIR <- file.path("data", "interim", "core", CORE_VERSION, "supports")
if (!dir.exists(SUPPORT_DIR)) stop("Missing cached core support directory: ", SUPPORT_DIR)
message("RQ1 context runtime: workers=", CONTEXT_WORKERS, ", boot=", B_BOOT)

metric_meta <- rq1 |>
  distinct(metric, metric_class, metric_scope, metric_geometry)
if (n_distinct(metric_meta$metric) != 54L) stop("Expected 54 upstream target representations")
metric_manifest <- rq_context_metric_manifest(metric_meta)
if (!any(metric_manifest$photoperiod_valid)) stop("No photoperiod-valid target representations identified")
if (!any(metric_manifest$fragmented_context_valid)) stop("No fragmented-context-valid target representations identified")
if (any(metric_manifest$photoperiod_valid & metric_manifest$metric_geometry != "linear")) {
  stop("Context manifest admitted a non-linear/circular target representation")
}
readr::write_csv(
  metric_manifest,
  file.path(OUT_DIAG, "rq1_context_metric_manifest.csv"),
  na = ""
)

context_core <- readr::read_csv(CORE_CONTEXT, show_col_types = FALSE, progress = FALSE) |>
  mutate(
    Date = as.Date(Date),
    protocol_start_date = as.Date(protocol_start_date),
    protocol_end_date = as.Date(protocol_end_date)
  )
site_meta <- core_site_metadata()

# lightexposurediary is intentionally a post-core input. Pulling it does not
# invalidate or rebuild the versioned core artifacts.
diary_paths <- setNames(
  vapply(melidos_sites(), function(s) raw_data_path(s, "lightexposurediary"), character(1)),
  melidos_sites()
)
missing_diaries <- diary_paths[!file.exists(diary_paths)]
if (length(missing_diaries)) {
  stop(
    "Missing lightexposurediary raw files for: ", paste(names(missing_diaries), collapse = ", "),
    ". Run Rscript scripts/01_download_melidos.R; no core rebuild is required."
  )
}

annotated_cache <- new.env(parent = emptyenv())
value_cache <- new.env(parent = emptyenv())

get_annotated_support <- function(site, support_id) {
  key <- paste(site, support_id, sep = "__")
  if (exists(key, envir = annotated_cache, inherits = FALSE)) {
    return(get(key, envir = annotated_cache, inherits = FALSE))
  }
  path <- file.path(SUPPORT_DIR, paste0(key, ".rds"))
  if (!file.exists(path)) return(NULL)
  meta <- site_meta |> filter(.data$site == .env$site)
  if (nrow(meta) != 1L) stop("Missing or non-unique site metadata for ", site)
  support <- readRDS(path)
  diary <- load_raw_file(diary_paths[[site]], "lightexposurediary")
  out <- rq_context_annotate_support(support, diary, c(meta$latitude, meta$longitude))
  assign(key, out, envir = annotated_cache)
  out
}

get_context_values <- function(site, support_id, placement, optical, resolution_s) {
  key <- paste(site, support_id, placement, optical, resolution_s, sep = "__")
  if (exists(key, envir = value_cache, inherits = FALSE)) {
    return(get(key, envir = value_cache, inherits = FALSE))
  }
  ann <- get_annotated_support(site, support_id)
  if (is.null(ann)) return(NULL)
  series <- rq_context_make_series(ann, placement, optical, resolution_s)
  include_spectral <- optical == "MEDI" && grepl("_full$", support_id)
  include_pulses <- as.integer(resolution_s) < 300L
  out <- rq_context_compute_values(
    series,
    metric_manifest = metric_manifest,
    include_spectral = include_spectral,
    include_pulses = include_pulses,
    resolution_s = resolution_s
  )
  assign(key, out, envir = value_cache)
  out
}

pair_support_values <- function(site, support_id,
                                ref_placement, ref_optical, ref_resolution,
                                can_placement, can_optical, can_resolution,
                                dimension, configuration, configuration_label,
                                configuration_order, comparison_lattice,
                                reference_configuration) {
  ref <- get_context_values(site, support_id, ref_placement, ref_optical, ref_resolution)
  can <- get_context_values(site, support_id, can_placement, can_optical, can_resolution)
  if (is.null(ref) || is.null(can) || !nrow(ref) || !nrow(can)) return(tibble())

  rq_context_pair_values(ref, can) |>
    mutate(
      dimension = dimension,
      configuration = configuration,
      configuration_label = configuration_label,
      configuration_order = configuration_order,
      comparison_lattice = comparison_lattice,
      reference_configuration = reference_configuration,
      support_id = support_id,
      analysis_unit_type = "participant_day_context",
      analysis_unit_id = paste(site, Id, as.character(Date), context_family, context_state, sep = "|"),
      reference_unit_id = analysis_unit_id,
      reference_id = NA_character_,
      window_id = NA_character_, window_index = NA_integer_, n_days = NA_integer_,
      reference_n_context_days = 1L,
      candidate_n_context_days = 1L
    ) |>
    select(
      dimension, configuration, configuration_label, configuration_order,
      comparison_lattice, reference_configuration, support_id,
      site, Id, Date, analysis_unit_type, analysis_unit_id, reference_unit_id,
      reference_id, window_id, window_index, n_days,
      context_family, context_state,
      metric, metric_class, metric_scope, metric_geometry,
      reference_n_context_days, candidate_n_context_days,
      reference_n_observations, candidate_n_observations,
      reference_value, candidate_value, delta_native
    )
}

# Parallelize at the site x support level. This preserves within-support caching
# while exposing dozens of independent work units on Linux/ECS and Windows.
message("RQ1 context: placement, optical, and temporal operator-valid representations")
pair_plan <- list(); pk <- 0L
for (site_i in melidos_sites()) {
  supports_i <- if (site_i == "MPI") {
    c("eye_medi", "eye_full")
  } else {
    c("eye_chest_medi", "eye_chest_full", "eye_wrist_medi", "eye_wrist_full", "eye_medi", "eye_full")
  }
  for (support_id in supports_i) {
    path <- file.path(SUPPORT_DIR, paste0(site_i, "__", support_id, ".rds"))
    if (!file.exists(path)) next
    pk <- pk + 1L
    pair_plan[[pk]] <- list(site = site_i, support_id = support_id)
  }
}

run_pair_support <- function(task) {
  site_i <- task$site
  support_id <- task$support_id
  out <- list(); k <- 0L

  if (support_id %in% c("eye_chest_medi", "eye_chest_full", "eye_wrist_medi", "eye_wrist_full")) {
    pos <- if (grepl("chest", support_id, fixed = TRUE)) "chest" else "wrist"
    branch_dual <- grepl("_full$", support_id)
    z <- pair_support_values(
      site_i, support_id,
      "eye", "MEDI", 10L, pos, "MEDI", 10L,
      "placement", pos, stringr::str_to_title(pos), match(pos, c("chest", "wrist")),
      paste0("placement_", pos), "Eye MEDI, 10 s"
    )
    if (nrow(z)) {
      z <- if (branch_dual) z |> filter(metric %in% DUAL_CHANNEL_METRICS) else z |> filter(!metric %in% DUAL_CHANNEL_METRICS)
      k <- k + 1L; out[[k]] <- z
    }
  }

  if (support_id == "eye_full") {
    z <- pair_support_values(
      site_i, support_id,
      "eye", "MEDI", 10L, "eye", "LIGHT", 10L,
      "optical", "LIGHT", "Photopic illuminance", 1L,
      "optical", "Eye MEDI, 10 s"
    )
    if (nrow(z)) { k <- k + 1L; out[[k]] <- z }

    for (j in seq_along(PRIMARY_TEMPORAL_S)) {
      r <- PRIMARY_TEMPORAL_S[[j]]
      label <- if (r < 60L) paste0(r, " s") else paste0(r %/% 60L, " min")
      code <- if (r < 60L) paste0(r, "s") else paste0(r %/% 60L, "min")
      z <- pair_support_values(
        site_i, support_id,
        "eye", "MEDI", 10L, "eye", "MEDI", r,
        "temporal", code, label, j, "temporal", "Eye MEDI, 10 s"
      ) |>
        filter(metric %in% DUAL_CHANNEL_METRICS)
      if (nrow(z)) { k <- k + 1L; out[[k]] <- z }
    }
  }

  if (support_id == "eye_medi") {
    for (j in seq_along(PRIMARY_TEMPORAL_S)) {
      r <- PRIMARY_TEMPORAL_S[[j]]
      label <- if (r < 60L) paste0(r, " s") else paste0(r %/% 60L, " min")
      code <- if (r < 60L) paste0(r, "s") else paste0(r %/% 60L, "min")
      z <- pair_support_values(
        site_i, support_id,
        "eye", "MEDI", 10L, "eye", "MEDI", r,
        "temporal", code, label, j, "temporal", "Eye MEDI, 10 s"
      ) |>
        filter(!metric %in% DUAL_CHANNEL_METRICS)
      if (nrow(z)) { k <- k + 1L; out[[k]] <- z }
    }
  }

  bind_rows(out)
}

message("RQ1 context: parallel extraction across ", length(pair_plan), " site-support tasks")
pair_blocks <- parallel_lapply(
  pair_plan, run_pair_support,
  exports = c(
    "SUPPORT_DIR", "site_meta", "diary_paths", "metric_manifest",
    "PRIMARY_TEMPORAL_S", "DUAL_CHANNEL_METRICS",
    "get_annotated_support", "get_context_values", "pair_support_values", "run_pair_support"
  ),
  label = "pair extraction"
)
raw_pairs <- bind_rows(pair_blocks)
if (!nrow(raw_pairs)) stop("No context-conditioned placement/optical/temporal rows were produced")

message("RQ1 context: protocol-anchored duration operator-valid representations")
duration_context <- context_core |>
  filter(
    support_id %in% c("eye_medi", "eye_full"),
    placement == "eye", optical == "MEDI", resolution_s == 10L
  ) |>
  distinct(support_id, site, Id, Date, .keep_all = TRUE)
duration_cohort <- protocol_reference_cohort(duration_context)
eligible_duration <- duration_cohort |> filter(eligible_protocol_7)
duration_windows <- make_protocol_duration_windows(eligible_duration, include_reference = FALSE)

# Daily context representations are calculated once at the reference measurement
# configuration. As in the whole-day duration analysis, each candidate window is
# compared with the participant's first-seven-valid-date protocol reference. A context
# that does not occur on a given day is structural absence, not imputed data;
# n_context_days is retained explicitly in the primitive artifact.
duration_support_plan <- tidyr::crossing(
  site = melidos_sites(),
  support_id = c("eye_medi", "eye_full")
) |>
  mutate(path = file.path(SUPPORT_DIR, paste0(site, "__", support_id, ".rds"))) |>
  filter(file.exists(path))

run_duration_support <- function(i) {
  row <- duration_support_plan[i, ]
  z <- get_context_values(row$site, row$support_id, "eye", "MEDI", 10L)
  if (is.null(z) || !nrow(z)) return(tibble())
  if (row$support_id == "eye_full") {
    z <- z |> filter(metric %in% DUAL_CHANNEL_METRICS)
  } else {
    z <- z |> filter(!metric %in% DUAL_CHANNEL_METRICS)
  }
  z |> mutate(support_id = row$support_id)
}
message("RQ1 context: parallel duration context extraction across ", nrow(duration_support_plan), " site-support tasks")
duration_daily_context <- bind_rows(parallel_lapply(
  seq_len(nrow(duration_support_plan)), run_duration_support,
  exports = c(
    "duration_support_plan", "SUPPORT_DIR", "site_meta", "diary_paths", "metric_manifest",
    "DUAL_CHANNEL_METRICS", "get_annotated_support", "get_context_values", "run_duration_support"
  ),
  label = "duration context extraction"
))

duration_blocks <- vector("list", nrow(duration_windows))
for (i in seq_len(nrow(duration_windows))) {
  w <- duration_windows[i, ]
  pdat <- duration_daily_context |>
    filter(support_id == w$support_id, site == w$site, Id == w$Id)
  if (!nrow(pdat)) next

  ref <- rq_context_aggregate_window(pdat, w$reference_dates[[1]], "reference")
  can <- rq_context_aggregate_window(pdat, w$selected_dates[[1]], "candidate")
  if (!nrow(ref) || !nrow(can)) next

  duration_blocks[[i]] <- inner_join(
    can, ref,
    by = c(
      "site", "Id", "context_family", "context_state",
      "metric", "metric_class", "metric_scope", "metric_geometry"
    )
  ) |>
    mutate(
      delta_native = candidate_value - reference_value,
      dimension = "duration",
      configuration = paste0(w$n_days, "d"),
      configuration_label = paste0(w$n_days, " d"),
      configuration_order = 7L - w$n_days,
      comparison_lattice = "duration",
      reference_configuration = "7 protocol-anchored days",
      support_id = w$support_id,
      Date = as.Date(NA),
      analysis_unit_type = "participant_window_context",
      analysis_unit_id = paste(w$window_id, context_family, context_state, metric, sep = "|"),
      reference_unit_id = paste(w$reference_id, context_family, context_state, metric, sep = "|"),
      reference_id = w$reference_id,
      window_id = w$window_id,
      window_index = w$window_index,
      n_days = w$n_days
    ) |>
    select(
      dimension, configuration, configuration_label, configuration_order,
      comparison_lattice, reference_configuration, support_id,
      site, Id, Date, analysis_unit_type, analysis_unit_id, reference_unit_id,
      reference_id, window_id, window_index, n_days,
      context_family, context_state,
      metric, metric_class, metric_scope, metric_geometry,
      reference_n_context_days, candidate_n_context_days,
      reference_n_observations, candidate_n_observations,
      reference_value, candidate_value, delta_native
    )
}
raw_pairs <- bind_rows(raw_pairs, bind_rows(duration_blocks)) |>
  mutate(
    context_family = as.character(context_family),
    context_state = tolower(as.character(context_state))
  ) |>
  filter(
    (context_family == "photoperiod" & context_state %in% c("day", "night")) |
      (context_family == "environment" & context_state %in% c("indoor", "outdoor")) |
      (context_family == "activity" & context_state %in% c("home", "working", "vehicle", "outdoors"))
  )

# One reference scale per comparison lattice x metric x context family. All
# states within a context family share the denominator, so conditional shifts
# cannot be manufactured by state-specific renormalisation.
reference_basis <- raw_pairs |>
  filter(is.finite(reference_value)) |>
  distinct(
    comparison_lattice, metric, metric_geometry, context_family,
    site, Id, reference_unit_id, context_state, reference_value
  )
standardizers <- reference_basis |>
  group_by(comparison_lattice, metric, metric_geometry, context_family) |>
  summarise(
    n_reference_units = n(),
    standardizer = sd(reference_value),
    .groups = "drop"
  ) |>
  mutate(zero_or_undefined = !is.finite(standardizer) | standardizer <= sqrt(.Machine$double.eps))
readr::write_csv(standardizers, file.path(OUT_DIAG, "rq1_context_standardizer_audit.csv"), na = "")

canonical <- raw_pairs |>
  left_join(
    standardizers,
    by = c("comparison_lattice", "metric", "metric_geometry", "context_family")
  ) |>
  mutate(
    available = is.finite(reference_value) & is.finite(candidate_value) & is.finite(delta_native) &
      !replace_na(zero_or_undefined, TRUE),
    e = if_else(available, delta_native / standardizer, NA_real_),
    abs_e = abs(e),
    core_artifact_version = CORE_VERSION,
    rq1_analysis_version = RQ1_VERSION,
    rq1_context_analysis_version = RQ1_CONTEXT_VERSION
  ) |>
  select(
    core_artifact_version, rq1_analysis_version, rq1_context_analysis_version,
    dimension, configuration, configuration_label, configuration_order,
    comparison_lattice, reference_configuration, support_id,
    site, Id, Date, analysis_unit_type, analysis_unit_id, reference_unit_id,
    reference_id, window_id, window_index, n_days,
    context_family, context_state,
    metric, metric_class, metric_scope, metric_geometry,
    reference_n_context_days, candidate_n_context_days,
    reference_n_observations, candidate_n_observations,
    reference_value, candidate_value, delta_native, standardizer,
    e, abs_e, available
  )
saveRDS(canonical, file.path(OUT_DATA, "rq1_context_distortion_long.rds"), compress = "xz")

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
    sampled <- map_dfr(by_site, ~.x[sample.int(nrow(.x), nrow(.x), replace = TRUE), , drop = FALSE])
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

message("RQ1 context: parallel bootstrap")
bootstrap_groups <- x |>
  group_by(across(all_of(group_vars))) |>
  group_split(.keep = TRUE)
bootstrap_one <- function(i) {
  g <- bootstrap_groups[[i]]
  key <- g |> slice(1L) |> select(all_of(group_vars)) |> ungroup()
  bind_cols(key, bootstrap_ci(g, B_BOOT, seed = BOOT_SEED + i))
}
cis <- bind_rows(parallel_lapply(
  seq_along(bootstrap_groups), bootstrap_one,
  exports = c(
    "bootstrap_groups", "group_vars", "B_BOOT", "BOOT_SEED",
    "safe_q", "bootstrap_ci", "bootstrap_one"
  ),
  label = "bootstrap"
))
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
  paste0("Whole-day target representations upstream: ", n_distinct(metric_meta$metric)),
  paste0("Photoperiod-valid context representations: ", n_photo),
  paste0("Indoor/outdoor and activity-valid representations: ", n_fragmented),
  paste0("Canonical context distortion rows: ", nrow(canonical)),
  paste0("Available standardized rows: ", nrow(x)),
  "Contexts: civil day/night; diary-derived indoor/outdoor; diary-derived home/working/vehicle/outdoors.",
  "Photoperiod keeps continuous-interval-valid metric families. Fragmented contexts keep only additive/distributional operators and calculate additive metrics within episodes before summing, never by stitching separated episodes.",
  "Reference standardization is fixed within comparison lattice x metric x context family and shared across context states.",
  "Parallel execution changes only scheduling; scientific operators and estimands are unchanged.",
  "This script reads cached support artifacts only; it does not rebuild core."
), file.path(OUT_RESULTS, "RQ1_CONTEXT_RUN_REPORT.md"))

message("RQ1 context complete: ", RQ1_CONTEXT_VERSION)
message("  ", file.path(OUT_DATA, "rq1_context_distortion_long.rds"))
message("  ", file.path(OUT_RESULTS, "rq1_context_summary.csv"))
message("  ", file.path(OUT_DIAG, "rq1_context_metric_manifest.csv"))
