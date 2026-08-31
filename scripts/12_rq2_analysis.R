# Canonical RQ2 analysis.
# The former v5 source and deployment-time patches have been consolidated into
# this file. The layered contextual model remains a separate scientific module
# and is sourced in the same R process so it reuses the validated transition
# objects without reconstructing RQ1/core data.

.requested_rq2_models <- tolower(Sys.getenv("RQ2_RUN_MODELS", unset = "1")) %in% c("1", "true", "yes")
.rq2_models_env_original <- Sys.getenv("RQ2_RUN_MODELS", unset = NA_character_)
Sys.setenv(RQ2_RUN_MODELS = "0")
suppressPackageStartupMessages({library(tidyverse); library(nlme)})
source("scripts/utils/analysis_design.R")
source("scripts/utils/paths.R")
source("scripts/utils/parallel_runtime.R")
source("scripts/utils/duration_artifacts.R")
source("scripts/utils/rq1_pairwise_artifacts.R")

# Corrected RQ2 implementation.
# Scientific invariants:
# - canonical RQ1 rows remain concrete observed pairs;
# - duration is projected to adjacent duration TYPES before conditioning/model grouping;
# - exposure-state bins are transition/support properties and are keyed by BOTH
#   sides of the concrete observation pair, never by metric;
# - large canonical parts are streamed; model workers receive file paths only;
# - participant-grouped CV retains every row belonging to each held-out participant.

RQ1_PAIRWISE <- file.path("results", "rq1", "rq1_pairwise_change_long.rds")
RQ1_SUMMARY <- file.path("results", "rq1", "rq1_pairwise_summary.csv")
CORE_METRICS <- file.path("results", "core", "metric_cube.csv.gz")
CORE_CONTEXT <- file.path("results", "core", "unit_context.csv.gz")
DURATION_MANIFEST <- file.path("results", "core", "duration_window_manifest.rds")
OUT <- file.path("results", "rq2")
DIAG <- file.path("results", "diagnostics")
CHECKPOINTS <- file.path(OUT, "checkpoints_v5")
SHARD_ROOT <- file.path(OUT, "model_input_shards")
RUN_MODELS <- !identical(Sys.getenv("RQ2_RUN_MODELS", unset = "1"), "0")
KEEP_MODEL_INPUTS <- identical(Sys.getenv("RQ2_KEEP_MODEL_INPUTS", unset = "0"), "1")
RQ2_CV_FOLDS <- suppressWarnings(as.integer(Sys.getenv("RQ2_CV_FOLDS", unset = "5")))
if (!is.finite(RQ2_CV_FOLDS) || RQ2_CV_FOLDS < 2L) RQ2_CV_FOLDS <- 5L
RQ2_WORKERS <- ms_resolve_workers("RQ2_WORKERS", default = 1L, cap = 48L)
RQ2_STREAM_WORKERS <- ms_resolve_workers("RQ2_STREAM_WORKERS", default = min(RQ2_WORKERS, 12L), cap = 24L)
MODEL_SEED <- 20260821L
PRIMARY_TEMPORAL_S <- ms_primary_temporal_s()
PRIMARY_DURATION_DAYS <- ms_primary_duration_days()
TEMPORAL_GAMMA_S <- PRIMARY_TEMPORAL_S
ANALYSIS_DESIGN_ID <- ms_analysis_design_id()
EXTERNAL <- c("external_radiation", "external_direct_fraction", "external_cloud", "solar_noon_elevation_deg")
OUTCOMES <- c(signed = "z", magnitude = "abs_z")
ensure_result_dirs(OUT, DIAG, CHECKPOINTS, SHARD_ROOT)
for (p in c(RQ1_PAIRWISE, RQ1_SUMMARY, CORE_METRICS, CORE_CONTEXT, DURATION_MANIFEST)) {
  if (!file.exists(p)) stop("Missing RQ2 input: ", p)
}

safe_mean <- function(x) { x <- x[is.finite(x)]; if (length(x)) mean(x) else NA_real_ }
safe_sd <- function(x) { x <- x[is.finite(x)]; if (length(x) >= 2L) sd(x) else NA_real_ }
safe_q <- function(x, p) { x <- x[is.finite(x)]; if (length(x)) unname(quantile(x, p, names = FALSE)) else NA_real_ }
circular_delta <- function(a, b, period = 86400) ((a - b + period / 2) %% period) - period / 2
circular_mean <- function(x, period = 86400) {
  x <- x[is.finite(x)]; if (!length(x)) return(NA_real_)
  th <- 2 * pi * x / period
  (atan2(mean(sin(th)), mean(cos(th))) %% (2 * pi)) * period / (2 * pi)
}
standardize <- function(x, geometry) {
  x <- x[is.finite(x)]; if (length(x) < 2L) return(NA_real_)
  if (identical(geometry, "circular_time")) sd(circular_delta(x, circular_mean(x))) else sd(x)
}

rq1_artifact <- readRDS(RQ1_PAIRWISE)
if (!rq1_pairwise_is_partitioned(rq1_artifact)) stop("RQ2 v5 requires partitioned RQ1 pairwise artifact")
part_paths <- rq1_pairwise_part_paths(rq1_artifact)
if (any(!file.exists(part_paths))) stop("Missing RQ1 canonical part")
RQ1_VERSION <- rq1_pairwise_version(rq1_artifact)
if (!is.null(rq1_artifact$analysis_design_id) && !identical(as.character(rq1_artifact$analysis_design_id[[1]]), ANALYSIS_DESIGN_ID)) {
  stop("RQ1 artifact analysis design does not match current frozen design")
}
pair_summary <- readr::read_csv(RQ1_SUMMARY, show_col_types = FALSE, progress = FALSE)
rq1_assert_summary_version(rq1_artifact, pair_summary)
CORE_VERSION <- unique(na.omit(c(rq1_artifact$core_artifact_version, pair_summary$core_artifact_version)))
if (length(CORE_VERSION) != 1L) stop("Core version mismatch")
CORE_VERSION <- CORE_VERSION[[1]]
RQ2_VERSION <- paste0("rq2_v5_duration_type_streamed_cv_fixed__", RQ1_VERSION, "__", ANALYSIS_DESIGN_ID)
SHARD_DIR <- file.path(SHARD_ROOT, RQ2_VERSION)
UNIT_FEATURE_DIR <- file.path(OUT, "unit_feature_parts", RQ2_VERSION)
dir.create(SHARD_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(UNIT_FEATURE_DIR, recursive = TRUE, showWarnings = FALSE)

cube <- readr::read_csv(CORE_METRICS, show_col_types = FALSE, progress = FALSE) |>
  mutate(Date = as.Date(Date))
context <- readr::read_csv(CORE_CONTEXT, show_col_types = FALSE, progress = FALSE) |>
  mutate(Date = as.Date(Date))
if (!all(PRIMARY_TEMPORAL_S %in% unique(cube$resolution_s))) stop("Primary temporal state missing from core cube")

# Daily transition-local state and measurement-independent external context.
state_cube <- cube |>
  filter(analysis_unit_type == "participant_day", support_id == "eye_medi", placement == "eye",
         optical == "MEDI", resolution_s == 10L,
         metric %in% c("mean_MEDI", "frequency_crossing_250"), available, is.finite(value)) |>
  select(site, Id, Date, metric, value) |>
  distinct() |>
  pivot_wider(names_from = metric, values_from = value)
external <- context |>
  filter(support_id == "eye_medi", placement == "eye", optical == "MEDI", resolution_s == 10L) |>
  select(site, Id, Date, era5_ssrd_daily_mean_w_m2, era5_direct_fraction,
         era5_total_cloud_cover_mean, latitude, day_of_year) |>
  distinct()
daily_features <- full_join(state_cube, external, by = c("site", "Id", "Date")) |>
  mutate(
    state_level = mean_MEDI,
    state_dynamic = if_else(is.finite(frequency_crossing_250) & frequency_crossing_250 >= 0,
                            log1p(frequency_crossing_250), NA_real_),
    external_radiation = if_else(is.finite(era5_ssrd_daily_mean_w_m2) & era5_ssrd_daily_mean_w_m2 >= 0,
                                 log1p(era5_ssrd_daily_mean_w_m2), NA_real_),
    external_direct_fraction = era5_direct_fraction,
    external_cloud = era5_total_cloud_cover_mean,
    solar_noon_elevation_deg = pmax(0, 90 - abs(latitude - 23.44 * sin(2 * pi * (284 + day_of_year) / 365.25)))
  ) |>
  select(site, Id, Date, state_level, state_dynamic, all_of(EXTERNAL))

# Duration predictors are computed from exact manifest member dates. The longer
# window supplies exposure level and external context; the shorter window
# supplies pre-addition day-to-day variability. One-day variability is left NA
# rather than invented as zero; the model engine drops an unusable predictor.
duration_manifest <- readRDS(DURATION_MANIFEST) |>
  mutate(window_start = as.Date(window_start), window_end = as.Date(window_end))
window_features <- duration_manifest |>
  select(support_id, site, Id, window_id, n_days, member_dates) |>
  unnest_longer(member_dates, values_to = "Date") |>
  mutate(Date = as.Date(Date)) |>
  left_join(daily_features, by = c("site", "Id", "Date")) |>
  group_by(support_id, site, Id, window_id, n_days) |>
  summarise(
    window_state_level = safe_mean(state_level),
    window_day_variability = safe_sd(state_level),
    across(all_of(EXTERNAL), safe_mean),
    .groups = "drop"
  )

normalize_primary <- function(z) {
  z |>
    filter(
      pair_available,
      dimension %in% c("placement", "optical", "temporal", "duration"),
      dimension %in% c("placement", "optical") | adjacent_transition
    ) |>
    mutate(
      Date = as.Date(Date),
      comparison_pair_id = if_else(dimension == "duration", paste0(n_days_a, "d_vs_", n_days_b, "d"), comparison_pair_id),
      config_a_id = if_else(dimension == "duration", paste0("duration_", n_days_a, "d"), config_a_id),
      config_b_id = if_else(dimension == "duration", paste0("duration_", n_days_b, "d"), config_b_id),
      config_a_label = if_else(dimension == "duration", paste0(n_days_a, " d"), config_a_label),
      config_b_label = if_else(dimension == "duration", paste0(n_days_b, " d"), config_b_label),
      transition_unit_key = paste(
        dimension, comparison_pair_id, support_id, site, Id,
        analysis_unit_id_a, analysis_unit_id_b, sep = "|"
      )
    )
}

unit_features_from_part <- function(path) {
  z <- normalize_primary(readRDS(path)) |>
    distinct(
      transition_unit_key, dimension, comparison_pair_id, config_a_label, config_b_label,
      support_id, site, Id, analysis_unit_id_a, analysis_unit_id_b, Date,
      window_id_a, window_id_b, n_days_a, n_days_b
    )
  nd <- z |> filter(dimension != "duration") |>
    left_join(daily_features, by = c("site", "Id", "Date")) |>
    mutate(
      primary_state_name = case_when(
        dimension %in% c("placement", "optical") ~ "target-aligned daily MEDI level",
        dimension == "temporal" ~ "higher-resolution short-term crossing dynamics",
        TRUE ~ "transition-local exposure state"
      ),
      primary_state_raw = if_else(dimension == "temporal", state_dynamic, state_level),
      duration_day_variability = NA_real_
    ) |>
    select(-state_level, -state_dynamic)
  du <- z |> filter(dimension == "duration") |>
    left_join(
      window_features |>
        rename(window_id_b = window_id, longer_window_level = window_state_level,
               longer_window_variability = window_day_variability,
               external_radiation_b = external_radiation,
               external_direct_fraction_b = external_direct_fraction,
               external_cloud_b = external_cloud,
               solar_noon_elevation_deg_b = solar_noon_elevation_deg) |>
        select(support_id, site, Id, window_id_b, longer_window_level, longer_window_variability,
               ends_with("_b")),
      by = c("support_id", "site", "Id", "window_id_b")
    ) |>
    left_join(
      window_features |>
        rename(window_id_a = window_id, shorter_window_variability = window_day_variability) |>
        select(support_id, site, Id, window_id_a, shorter_window_variability),
      by = c("support_id", "site", "Id", "window_id_a")
    ) |>
    mutate(
      primary_state_name = "longer-window MEDI level plus pre-addition day variability",
      primary_state_raw = if_else(is.finite(longer_window_level), log1p(abs(longer_window_level)), NA_real_),
      duration_day_variability = shorter_window_variability,
      external_radiation = external_radiation_b,
      external_direct_fraction = external_direct_fraction_b,
      external_cloud = external_cloud_b,
      solar_noon_elevation_deg = solar_noon_elevation_deg_b
    ) |>
    select(
      -longer_window_level, -longer_window_variability,
      -external_radiation_b, -external_direct_fraction_b, -external_cloud_b,
      -solar_noon_elevation_deg_b, -shorter_window_variability
    )
  bind_rows(nd, du) |>
    mutate(participant_key = paste(site, Id, sep = "|"))
}

unit_feature_checkpoint <- function(task) {
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
invisible(gc())
if (anyDuplicated(unit_features$transition_unit_key)) stop("RQ2 transition-unit key is not unique")
if (any(unit_features$dimension == "duration" & grepl("__to__", unit_features$comparison_pair_id, fixed = TRUE))) {
  stop("Concrete duration window id leaked into RQ2 transition type")
}

state_bins <- unit_features |>
  filter(is.finite(primary_state_raw)) |>
  group_by(dimension, comparison_pair_id, support_id) |>
  mutate(state_bin = if (n() >= 6L && n_distinct(primary_state_raw) >= 3L) ntile(primary_state_raw, 3L) else NA_integer_) |>
  ungroup() |>
  mutate(state_bin_label = recode(as.character(state_bin), `1` = "Low", `2` = "Middle", `3` = "High", .default = NA_character_)) |>
  select(transition_unit_key, dimension, comparison_pair_id, support_id, site, Id,
         analysis_unit_id_a, analysis_unit_id_b, primary_state_raw, state_bin, state_bin_label)
readr::write_csv(state_bins, file.path(DIAG, "rq2_reference_state_bins.csv"), na = "")
unit_features <- unit_features |>
  left_join(state_bins |> select(transition_unit_key, state_bin, state_bin_label), by = "transition_unit_key")

# Frozen model-task catalogue uses configuration TYPES. This is also an explicit
# invariant against accidental concrete duration-window task proliferation.
task_catalog <- pair_summary |>
  filter(dimension %in% c("placement", "optical") | adjacent_transition) |>
  distinct(dimension, comparison_pair_id, metric, metric_class) |>
  arrange(dimension, comparison_pair_id, metric) |>
  mutate(task_index = row_number())
if (any(task_catalog$dimension == "duration" & grepl("__to__", task_catalog$comparison_pair_id, fixed = TRUE))) {
  stop("RQ1 duration summary is not projected to duration comparison types")
}
expected_duration_transitions <- length(PRIMARY_DURATION_DAYS) - 1L
if (n_distinct(task_catalog$comparison_pair_id[task_catalog$dimension == "duration"]) > expected_duration_transitions) {
  stop("RQ2 duration local-transition catalogue exceeds the frozen adjacent duration types")
}
readr::write_csv(task_catalog, file.path(DIAG, "rq2_model_task_catalog.csv"), na = "")

sanitize_task <- function(i) sprintf("task_%04d", as.integer(i))
part_token <- function(i) sprintf("part_%03d", as.integer(i) - 1L)

build_shards_for_part <- function(path, i) {
  pdir <- file.path(SHARD_DIR, part_token(i))
  marker <- file.path(pdir, ".complete")
  manifest_path <- file.path(pdir, "manifest.csv")
  conditional_path <- file.path(pdir, "conditional_fragment.rds")
  if (file.exists(marker) && file.exists(manifest_path) && file.exists(conditional_path)) {
    return(readr::read_csv(manifest_path, show_col_types = FALSE, progress = FALSE))
  }
  if (dir.exists(pdir)) unlink(pdir, recursive = TRUE, force = TRUE)
  dir.create(pdir, recursive = TRUE, showWarnings = FALSE)

  z <- normalize_primary(readRDS(path)) |>
    filter(available, is.finite(z)) |>
    select(
      dimension, comparison_pair_id, config_a_label, config_b_label, metric, metric_class, metric_geometry,
      support_id, site, Id, analysis_unit_id_a, analysis_unit_id_b, transition_unit_key, z
    ) |>
    left_join(
      unit_features |>
        select(transition_unit_key, participant_key, primary_state_name, primary_state_raw,
               duration_day_variability, all_of(EXTERNAL), state_bin, state_bin_label),
      by = "transition_unit_key"
    ) |>
    mutate(abs_z = abs(z)) |>
    left_join(task_catalog, by = c("dimension", "comparison_pair_id", "metric", "metric_class"))
  if (any(is.na(z$task_index))) stop("RQ2 task catalogue join failed in ", basename(path))

  conditional_fragment <- z |>
    filter(!is.na(state_bin_label)) |>
    group_by(dimension, comparison_pair_id, config_a_label, config_b_label, metric, metric_class,
             metric_geometry, primary_state_name, state_bin, state_bin_label) |>
    summarise(
      z_values = list(as.numeric(z)), state_values = list(as.numeric(primary_state_raw)),
      participant_keys = list(unique(participant_key)), .groups = "drop"
    )
  saveRDS(conditional_fragment, conditional_path, compress = "gzip")

  model_z <- z |>
    filter(is.finite(primary_state_raw)) |>
    select(task_index, site, participant_key, z, abs_z, primary_state_raw,
           duration_day_variability, all_of(EXTERNAL))
  groups <- split(model_z, model_z$task_index)
  rows <- vector("list", length(groups)); k <- 0L
  for (nm in names(groups)) {
    k <- k + 1L
    g <- groups[[nm]] |> select(-task_index)
    spath <- file.path(pdir, paste0(sanitize_task(as.integer(nm)), ".rds"))
    saveRDS(g, spath, compress = "gzip")
    rows[[k]] <- tibble(
      part_index = i, task_index = as.integer(nm), shard_path = normalizePath(spath, winslash = "/", mustWork = TRUE),
      rows = nrow(g), bytes = as.numeric(file.info(spath)$size)
    )
  }
  manifest <- bind_rows(rows)
  readr::write_csv(manifest, manifest_path, na = "")
  writeLines(c(RQ2_VERSION, paste0("source=", basename(path))), marker)
  rm(z, model_z, groups, conditional_fragment)
  invisible(gc(FALSE))
  manifest
}

build_shard_task <- function(task) build_shards_for_part(task$path, task$i)
message("RQ2 v5 pass 2/2: build/reuse bounded model-input shards; workers=", RQ2_STREAM_WORKERS)
shard_tasks <- Map(function(path, i) list(path = path, i = i), part_paths, seq_along(part_paths))
shard_manifests <- ms_parallel_map(
  shard_tasks, build_shard_task, workers = RQ2_STREAM_WORKERS,
  packages = c("tidyverse"),
  exports = c("build_shard_task", "build_shards_for_part", "normalize_primary", "unit_features",
              "task_catalog", "SHARD_DIR", "RQ2_VERSION", "sanitize_task", "part_token", "EXTERNAL")
)
shard_manifest <- bind_rows(shard_manifests)
rm(shard_tasks)
readr::write_csv(shard_manifest, file.path(OUT, "rq2_model_input_shard_manifest.csv"), na = "")

# A small manifest replaces the legacy enormous condition_long table. Plotting
# only needs provenance; inferential summaries are written separately below.
condition_manifest <- list(
  artifact_type = "rq2_streamed_condition_manifest_v1",
  core_artifact_version = CORE_VERSION,
  rq1_analysis_version = RQ1_VERSION,
  rq2_analysis_version = RQ2_VERSION,
  analysis_design_id = ANALYSIS_DESIGN_ID,
  canonical_part_count = length(part_paths),
  task_count = nrow(task_catalog),
  shard_dir = normalizePath(SHARD_DIR, winslash = "/", mustWork = TRUE)
)
saveRDS(condition_manifest, file.path(OUT, "rq2_condition_long.rds"), compress = "gzip")

# Exact conditional A/B and quantiles are pooled from compact numeric chunks.
conditional_parts <- lapply(seq_along(part_paths), function(i) {
  readRDS(file.path(SHARD_DIR, part_token(i), "conditional_fragment.rds"))
})
conditional_geometry <- bind_rows(conditional_parts) |>
  group_by(dimension, comparison_pair_id, config_a_label, config_b_label, metric, metric_class,
           metric_geometry, primary_state_name, state_bin, state_bin_label) |>
  summarise(
    z_values = list(unlist(z_values, use.names = FALSE)),
    state_values = list(unlist(state_values, use.names = FALSE)),
    participant_keys = list(unique(unlist(participant_keys, use.names = FALSE))),
    .groups = "drop"
  ) |>
  transmute(
    dimension, comparison_pair_id, config_a_label, config_b_label, metric, metric_class, metric_geometry,
    primary_state_name, state_bin, state_bin_label,
    n_participants = lengths(participant_keys), n_units = lengths(z_values),
    state_median = map_dbl(state_values, ~safe_q(.x, .5)),
    state_q25 = map_dbl(state_values, ~safe_q(.x, .25)),
    state_q75 = map_dbl(state_values, ~safe_q(.x, .75)),
    B_conditional = map_dbl(z_values, mean), A_conditional = map_dbl(z_values, ~mean(abs(.x))),
    median_z = map_dbl(z_values, ~safe_q(.x, .5)),
    p025_z = map_dbl(z_values, ~safe_q(.x, .025)), p975_z = map_dbl(z_values, ~safe_q(.x, .975)),
    core_artifact_version = CORE_VERSION, rq1_analysis_version = RQ1_VERSION, rq2_analysis_version = RQ2_VERSION
  )
if (any(conditional_geometry$A_conditional + 1e-12 < abs(conditional_geometry$B_conditional))) {
  stop("RQ2 conditional A >= |B| invariant failed")
}
readr::write_csv(conditional_geometry, file.path(OUT, "rq2_conditional_geometry.csv"), na = "")
rm(conditional_parts)
invisible(gc())

family_predictors <- function(dimension, family) {
  state <- if (identical(dimension, "duration")) c("primary_state_raw", "duration_day_variability") else "primary_state_raw"
  if (identical(family, "external_context")) return(EXTERNAL)
  if (identical(family, "exposure_state")) return(state)
  if (identical(family, "joint")) return(c(state, EXTERNAL))
  character()
}

fit_task <- function(task) {
  dat <- bind_rows(lapply(task$shard_paths, readRDS))
  meta <- task$meta
  scale_train_test <- function(tr, te, predictors) {
    keep <- character()
    for (p in predictors) {
      finite <- is.finite(tr[[p]])
      if (sum(finite) < 3L) next
      mu <- mean(tr[[p]][finite]); s <- sd(tr[[p]][finite])
      if (!is.finite(mu) || !is.finite(s) || s <= sqrt(.Machine$double.eps)) next
      tr[[p]] <- (tr[[p]] - mu) / s
      te[[p]] <- (te[[p]] - mu) / s
      keep <- c(keep, p)
    }
    list(tr = tr, te = te, keep = keep)
  }
  fit_one <- function(d, outcome, predictors) {
    if (!length(predictors) || nrow(d) < 20L || n_distinct(d$participant_key) < 3L) {
      return(list(fit = NULL, random_structure = NA_character_))
    }
    d$site <- factor(d$site); d$participant_key <- factor(d$participant_key)
    f <- reformulate(predictors, response = outcome)
    ctrl <- nlme::lmeControl(opt = "optim", maxIter = 100L, msMaxIter = 100L, returnObject = TRUE)
    fit <- tryCatch(
      suppressWarnings(nlme::lme(fixed = f, random = ~1 | site/participant_key, data = d,
                                 method = "ML", na.action = na.omit, control = ctrl)),
      error = function(e) NULL
    )
    if (!is.null(fit)) return(list(fit = fit, random_structure = "site/participant"))
    fit <- tryCatch(
      suppressWarnings(nlme::lme(fixed = f, random = ~1 | participant_key, data = d,
                                 method = "ML", na.action = na.omit, control = ctrl)),
      error = function(e) NULL
    )
    list(fit = fit, random_structure = if (is.null(fit)) NA_character_ else "participant")
  }
  performance <- function(obs, pred) {
    ok <- is.finite(obs) & is.finite(pred); obs <- obs[ok]; pred <- pred[ok]
    if (length(obs) < 2L) return(tibble(n_test = length(obs), rmse = NA_real_, mae = NA_real_, r2 = NA_real_))
    sst <- sum((obs - mean(obs))^2)
    tibble(n_test = length(obs), rmse = sqrt(mean((obs - pred)^2)), mae = mean(abs(obs - pred)),
           r2 = if (sst > 0) 1 - sum((obs - pred)^2) / sst else NA_real_)
  }

  set.seed(task$seed)
  pm_base <- dat |>
    distinct(site, participant_key) |>
    group_by(site) |>
    mutate(fold = sample(rep(seq_len(task$folds), length.out = n()))) |>
    ungroup()
  coefs <- list(); perfs <- list(); ci <- 0L; pi <- 0L
  for (oname in names(OUTCOMES)) for (fname in c("external_context", "exposure_state", "joint")) {
    outcome <- OUTCOMES[[oname]]
    candidates <- family_predictors(meta$dimension[[1]], fname)
    usable <- candidates[vapply(candidates, function(p) {
      x <- dat[[p]]; sum(is.finite(x)) >= 3L && is.finite(sd(x[is.finite(x)])) && sd(x[is.finite(x)]) > sqrt(.Machine$double.eps)
    }, logical(1))]
    if (!length(usable)) next
    d <- dat |> filter(is.finite(.data[[outcome]]), if_all(all_of(usable), is.finite))
    if (nrow(d) < 20L || n_distinct(d$participant_key) < 3L) next

    sc <- scale_train_test(d, d, usable)
    full_fit <- fit_one(sc$tr, outcome, sc$keep)
    if (!is.null(full_fit$fit)) {
      tt <- summary(full_fit$fit)$tTable
      ci <- ci + 1L
      coefs[[ci]] <- tibble(
        dimension = meta$dimension, comparison_pair_id = meta$comparison_pair_id, metric = meta$metric,
        outcome = oname, model_family = fname, random_structure = full_fit$random_structure,
        term = rownames(tt), estimate = tt[, "Value"], std_error = tt[, "Std.Error"],
        df = tt[, "DF"], t_value = tt[, "t-value"], p_value = tt[, "p-value"]
      )
    }

    d_cv <- d |> left_join(pm_base, by = c("site", "participant_key"))
    obs <- pred <- numeric()
    for (sp in sort(unique(d_cv$fold))) {
      tr <- d_cv |> filter(fold != sp)
      te <- d_cv |> filter(fold == sp)
      if (nrow(tr) < 20L || nrow(te) < 2L || n_distinct(tr$participant_key) < 3L) next
      ss <- scale_train_test(tr, te, usable)
      ff <- fit_one(ss$tr, outcome, ss$keep)
      if (is.null(ff$fit)) next
      pr <- tryCatch(as.numeric(predict(ff$fit, newdata = ss$te, level = 0)),
                     error = function(e) rep(NA_real_, nrow(ss$te)))
      obs <- c(obs, ss$te[[outcome]]); pred <- c(pred, pr)
    }
    perf <- performance(obs, pred)
    pi <- pi + 1L
    perfs[[pi]] <- tibble(
      dimension = meta$dimension, comparison_pair_id = meta$comparison_pair_id, metric = meta$metric,
      outcome = oname, model_family = fname, validation_scheme = "participant_grouped",
      n_participants = n_distinct(d$participant_key), n_sites = n_distinct(d$site),
      n_test = perf$n_test, rmse = perf$rmse, mae = perf$mae, r2 = perf$r2
    )
  }
  list(checkpoint_version = task$checkpoint_version, complete = TRUE,
       coefficients = bind_rows(coefs), performance = bind_rows(perfs))
}

fit_task_checkpoint <- function(task) {
  log_progress <- function(status) {
    line <- paste(
      format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"), task$index, status,
      task$meta$dimension[[1]], task$meta$comparison_pair_id[[1]], task$meta$metric[[1]],
      sep = "\t"
    )
    cat(line, "\n", file = task$progress_path, append = TRUE, sep = "")
  }
  cached <- if (file.exists(task$checkpoint_path)) tryCatch(readRDS(task$checkpoint_path), error = function(e) NULL) else NULL
  if (!is.null(cached) && identical(cached$checkpoint_version, task$checkpoint_version) && isTRUE(cached$complete)) {
    log_progress("reused")
    return(list(index = task$index, path = task$checkpoint_path, reused = TRUE))
  }
  obj <- fit_task(task)
  tmp <- paste0(task$checkpoint_path, ".tmp.", Sys.getpid())
  saveRDS(obj, tmp, compress = FALSE)
  if (file.exists(task$checkpoint_path)) unlink(task$checkpoint_path)
  if (!file.rename(tmp, task$checkpoint_path)) stop("Could not install RQ2 model checkpoint")
  log_progress("completed")
  list(index = task$index, path = task$checkpoint_path, reused = FALSE)
}

PROGRESS_LOG <- file.path("results", "logs", "rq2_model_progress.tsv")
dir.create(dirname(PROGRESS_LOG), recursive = TRUE, showWarnings = FALSE)
if (RUN_MODELS) {
  writeLines("timestamp\ttask_index\tstatus\tdimension\tcomparison_pair_id\tmetric", PROGRESS_LOG)
}

model_tasks <- lapply(seq_len(nrow(task_catalog)), function(i) {
  meta <- task_catalog[i, ]
  paths <- shard_manifest |> filter(task_index == meta$task_index) |> pull(shard_path)
  checkpoint_path <- file.path(CHECKPOINTS, paste0(sanitize_task(meta$task_index), ".rds"))
  list(
    index = meta$task_index[[1]], meta = meta, shard_paths = paths,
    folds = RQ2_CV_FOLDS, seed = MODEL_SEED + meta$task_index[[1]],
    checkpoint_version = paste0(RQ2_VERSION, "__core__", CORE_VERSION, "__participant_cv__", RQ2_CV_FOLDS),
    checkpoint_path = checkpoint_path,
    progress_path = normalizePath(PROGRESS_LOG, winslash = "/", mustWork = FALSE),
    cost = sum(shard_manifest$bytes[shard_manifest$task_index == meta$task_index], na.rm = TRUE)
  )
})
model_tasks <- keep(model_tasks, ~length(.x$shard_paths) > 0L)
model_results <- vector("list", nrow(task_catalog))
if (RUN_MODELS && length(model_tasks)) {
  order_idx <- order(vapply(model_tasks, `[[`, numeric(1), "cost"), decreasing = TRUE)
  scheduled <- model_tasks[order_idx]
  message("RQ2 v5 models: ", length(scheduled), " tasks across ", RQ2_WORKERS, " PSOCK workers; participant-grouped ", RQ2_CV_FOLDS, "-fold CV only")
  message("RQ2 model progress: ", PROGRESS_LOG, " (one line per processed task; total = ", length(scheduled), ")")
  refs <- ms_parallel_map(
    scheduled, fit_task_checkpoint, workers = RQ2_WORKERS, seed = MODEL_SEED,
    packages = c("tidyverse", "nlme"),
    exports = c("fit_task_checkpoint", "fit_task", "family_predictors", "EXTERNAL", "OUTCOMES", "safe_q")
  )
  for (r in refs) model_results[[r$index]] <- readRDS(r$path)
} else if (!RUN_MODELS) {
  message("RQ2 v5 models disabled by RQ2_RUN_MODELS=0")
}
# Load valid cached checkpoints for tasks not populated above.
for (task in model_tasks) {
  if (is.null(model_results[[task$index]]) && file.exists(task$checkpoint_path)) {
    obj <- tryCatch(readRDS(task$checkpoint_path), error = function(e) NULL)
    if (!is.null(obj) && identical(obj$checkpoint_version, task$checkpoint_version) && isTRUE(obj$complete)) {
      model_results[[task$index]] <- obj
    }
  }
}
model_coefficients <- bind_rows(map(model_results, ~if (is.null(.x)) NULL else .x$coefficients))
model_performance <- bind_rows(map(model_results, ~if (is.null(.x)) NULL else .x$performance))
if (!ncol(model_coefficients)) model_coefficients <- tibble(
  dimension = character(), comparison_pair_id = character(), metric = character(), outcome = character(),
  model_family = character(), random_structure = character(), term = character(), estimate = double(),
  std_error = double(), df = double(), t_value = double(), p_value = double()
)
if (!ncol(model_performance)) model_performance <- tibble(
  dimension = character(), comparison_pair_id = character(), metric = character(), outcome = character(),
  model_family = character(), validation_scheme = character(), n_participants = integer(), n_sites = integer(),
  n_test = integer(), rmse = double(), mae = double(), r2 = double()
)
readr::write_csv(model_coefficients, file.path(OUT, "rq2_model_coefficients.csv"), na = "")
readr::write_csv(model_performance, file.path(OUT, "rq2_model_performance.csv"), na = "")

model_manifest <- bind_rows(lapply(seq_len(nrow(task_catalog)), function(i) {
  meta <- task_catalog[i, ]
  obj <- model_results[[meta$task_index]]
  checkpoint_path <- file.path(CHECKPOINTS, paste0(sanitize_task(meta$task_index), ".rds"))
  tibble(
    artifact_type = "rq2_model_checkpoint_manifest_v2",
    rq1_analysis_version = RQ1_VERSION, rq2_analysis_version = RQ2_VERSION,
    core_artifact_version = CORE_VERSION, task_index = meta$task_index,
    dimension = meta$dimension, comparison_pair_id = meta$comparison_pair_id,
    metric = meta$metric, metric_class = meta$metric_class,
    checkpoint_path = normalizePath(checkpoint_path, winslash = "/", mustWork = FALSE),
    checkpoint_version = paste0(RQ2_VERSION, "__core__", CORE_VERSION, "__participant_cv__", RQ2_CV_FOLDS),
    checkpoint_present = file.exists(checkpoint_path),
    checkpoint_complete = !is.null(obj) && isTRUE(obj$complete),
    coefficient_rows = if (!is.null(obj)) nrow(obj$coefficients) else 0L,
    performance_rows = if (!is.null(obj)) nrow(obj$performance) else 0L,
    run_models = RUN_MODELS
  )
}))
readr::write_csv(model_manifest, file.path(OUT, "rq2_model_artifact_manifest.csv"), na = "")

# -----------------------------------------------------------------------------
# Gamma: actual four-cell configuration values. This branch uses the daily core
# only and therefore does not require the large pairwise canonical object.
# -----------------------------------------------------------------------------
branch_cut_test <- tibble(
  period = 86400, marginal_from = 43200 - 2, marginal_to = -43200 + 2,
  ordinary_difference = marginal_to - marginal_from,
  circular_second_difference = circular_delta(marginal_to, marginal_from),
  pass = abs(circular_delta(marginal_to, marginal_from)) < 10
)
if (!branch_cut_test$pass) stop("Circular gamma branch-cut regression failed")
readr::write_csv(branch_cut_test, file.path(DIAG, "rq2_circular_gamma_test.csv"), na = "")

gamma_block <- function(cells, cell_names, dimension_a, dimension_b, transition, lattice, anchor_support) {
  key <- c("support_id", "site", "Id", "analysis_unit_type", "analysis_unit_id", "Date", "metric")
  z <- cells |> filter(cell %in% cell_names) |>
    select(all_of(key), metric_class, metric_geometry, cell, value, available)
  wide <- z |> select(-available) |> pivot_wider(names_from = cell, values_from = value)
  if (!all(cell_names %in% names(wide))) return(tibble())
  wide |>
    mutate(
      dimension_a = dimension_a, dimension_b = dimension_b, transition = transition,
      comparison_lattice = lattice,
      delta_at_b_from = if_else(metric_geometry == "circular_time",
                                circular_delta(.data[[cell_names[[2]]]], .data[[cell_names[[1]]]]),
                                .data[[cell_names[[2]]]] - .data[[cell_names[[1]]]]),
      delta_at_b_to = if_else(metric_geometry == "circular_time",
                              circular_delta(.data[[cell_names[[4]]]], .data[[cell_names[[3]]]]),
                              .data[[cell_names[[4]]]] - .data[[cell_names[[3]]]]),
      gamma_delta = if_else(metric_geometry == "circular_time",
                            circular_delta(delta_at_b_to, delta_at_b_from),
                            delta_at_b_to - delta_at_b_from),
      delta_at_b = delta_at_b_from, delta_at_ref = delta_at_b_to,
      anchor_support = anchor_support
    )
}

gamma_blocks <- list()
for (pos in c("chest", "wrist")) {
  sup <- paste0("eye_", pos, "_full")
  cells <- cube |> filter(support_id == sup, resolution_s == 10L, metric %in% unique(pair_summary$metric)) |>
    mutate(cell = paste(placement, optical, sep = "__"))
  gamma_blocks[[length(gamma_blocks) + 1L]] <- gamma_block(
    cells,
    c(paste(pos, "LIGHT", sep = "__"), paste("eye", "LIGHT", sep = "__"),
      paste(pos, "MEDI", sep = "__"), paste("eye", "MEDI", sep = "__")),
    "placement", "optical", paste0(pos, "_LIGHT_to_MEDI"), paste0("placement_", pos, "_x_optical"), sup
  )
}
for (pos in c("chest", "wrist")) for (j in seq_len(length(TEMPORAL_GAMMA_S) - 1L)) {
  fine <- TEMPORAL_GAMMA_S[j]; coarse <- TEMPORAL_GAMMA_S[j + 1L]
  sup <- paste0("eye_", pos, "_full")
  cells <- cube |> filter(support_id == sup, optical == "MEDI", resolution_s %in% c(fine, coarse)) |>
    mutate(cell = paste(placement, resolution_s, sep = "__"))
  gamma_blocks[[length(gamma_blocks) + 1L]] <- gamma_block(
    cells,
    c(paste(pos, coarse, sep = "__"), paste("eye", coarse, sep = "__"),
      paste(pos, fine, sep = "__"), paste("eye", fine, sep = "__")),
    "placement", "temporal", paste0(pos, "_", coarse, "to", fine), paste0("placement_", pos, "_x_temporal"), sup
  )
}
for (j in seq_len(length(TEMPORAL_GAMMA_S) - 1L)) {
  fine <- TEMPORAL_GAMMA_S[j]; coarse <- TEMPORAL_GAMMA_S[j + 1L]
  cells <- cube |> filter(support_id == "eye_full", placement == "eye", resolution_s %in% c(fine, coarse)) |>
    mutate(cell = paste(optical, resolution_s, sep = "__"))
  gamma_blocks[[length(gamma_blocks) + 1L]] <- gamma_block(
    cells,
    c(paste("LIGHT", coarse, sep = "__"), paste("MEDI", coarse, sep = "__"),
      paste("LIGHT", fine, sep = "__"), paste("MEDI", fine, sep = "__")),
    "optical", "temporal", paste0(coarse, "to", fine), "optical_x_temporal", "eye_full"
  )
}
gamma_raw <- bind_rows(gamma_blocks)
if (nrow(gamma_raw)) {
  anchor <- cube |> filter(placement == "eye", optical == "MEDI", resolution_s == 10L, is.finite(value)) |>
    group_by(support_id, metric, metric_geometry) |>
    summarise(standardizer = standardize(value, first(metric_geometry)), .groups = "drop")
  gamma_long <- gamma_raw |>
    left_join(anchor, by = c("anchor_support" = "support_id", "metric", "metric_geometry")) |>
    mutate(
      available = is.finite(gamma_delta) & is.finite(standardizer) & standardizer > 0,
      gamma = if_else(available, gamma_delta / standardizer, NA_real_),
      core_artifact_version = CORE_VERSION, rq1_analysis_version = RQ1_VERSION, rq2_analysis_version = RQ2_VERSION
    ) |>
    select(core_artifact_version, rq1_analysis_version, rq2_analysis_version, dimension_a, dimension_b,
           transition, comparison_lattice, anchor_support, site, Id, analysis_unit_type, analysis_unit_id,
           Date, metric, metric_class, metric_geometry, delta_at_b_from, delta_at_b_to, delta_at_b,
           delta_at_ref, gamma_delta, standardizer, gamma, available)
} else gamma_long <- tibble()
saveRDS(gamma_long, file.path(OUT, "rq2_gamma_long.rds"), compress = "xz")
gamma_summary <- if (nrow(gamma_long)) {
  gamma_long |> filter(available, is.finite(gamma)) |>
    group_by(dimension_a, dimension_b, comparison_lattice, transition, metric, metric_class, metric_geometry) |>
    summarise(n_participants = n_distinct(paste(site, Id, sep = "|")), n_units = n(),
              R = mean(gamma), Q = mean(abs(gamma)), .groups = "drop")
} else tibble()
if (nrow(gamma_summary) && any(gamma_summary$Q + 1e-12 < abs(gamma_summary$R))) stop("RQ2 Q >= |R| invariant failed")
readr::write_csv(gamma_summary, file.path(OUT, "rq2_gamma_summary.csv"), na = "")
readr::write_csv(gamma_summary, file.path(OUT, "rq2_conditional_geometry_gamma.csv"), na = "")
readr::write_csv(tibble(
  dimension_pair = c("placement x optical", "placement x temporal", "optical x temporal", "duration-containing"),
  primary_scope = c(TRUE, TRUE, TRUE, FALSE),
  note = c("target-aligned local full-support four-cell contrast", "adjacent primary temporal transitions",
           "adjacent primary temporal transitions", "duration enters RQ3 joint stability")
), file.path(OUT, "rq2_interaction_scope.csv"), na = "")

writeLines(c(
  "# RQ2 run report", "",
  paste0("RQ1 upstream: ", RQ1_VERSION), paste0("RQ2 analysis version: ", RQ2_VERSION),
  paste0("Analysis design: ", ANALYSIS_DESIGN_ID),
  paste0("Duration canonical rows are grouped by adjacent duration TYPES across ", min(PRIMARY_DURATION_DAYS), "-", max(PRIMARY_DURATION_DAYS), " d, not concrete window identity."),
  paste0("Temporal gamma uses adjacent primary cadences: ", paste(PRIMARY_TEMPORAL_S, collapse = ", "), " s."),
  "State-bin keys include both analysis-unit sides, preventing ambiguous nested-window assignments.",
  "Duration exposure state uses log absolute longer-window mean MEDI; shorter-window day variability is an additional state predictor when estimable.",
  "Duration external context uses measurement-independent daily context averaged over the longer observed window.",
  "Cross-validation is participant-grouped only and holds out all rows belonging to each selected participant; LOSO is not run.",
  paste0("Mixed models executed: ", RUN_MODELS),
  paste0("RQ2 workers: ", RQ2_WORKERS, "; CV folds: ", RQ2_CV_FOLDS),
  paste0("Model-input shards retained after success: ", KEEP_MODEL_INPUTS),
  paste0("Circular gamma branch-cut test passed: gamma_delta = ", format(branch_cut_test$circular_second_difference, digits = 8))
), file.path(OUT, "RQ2_RUN_REPORT.md"))

# Base shards are deliberately retained until the layered context stage has
# completed successfully. The canonical stage must never delete its own resume
# state before downstream consumers finish.
message("RQ2 complete: ", RQ2_VERSION)

RUN_MODELS <- .requested_rq2_models
Sys.setenv(RQ2_RUN_MODELS = if (.requested_rq2_models) "1" else "0")

# The layered contextual extension deliberately runs in the same R process: it
# reuses the validated canonical transition objects created above and never
# reconstructs RQ1/core objects under a second set of rules.
source(file.path("scripts", "12c_rq2_context_models.R"), local = .GlobalEnv)

if (isTRUE(RUN_MODELS)) {
  expected_context_files <- file.path(OUT, c(
    "rq2_layered_context_model_coefficients.csv",
    "rq2_layered_context_model_performance.csv",
    "rq2_layered_context_model_manifest.csv",
    "rq2_layered_context_run_report.txt"
  ))
  missing_context_files <- expected_context_files[!file.exists(expected_context_files)]
  if (length(missing_context_files)) {
    stop("Layered RQ2 context stage did not produce: ", paste(missing_context_files, collapse = ", "))
  }

  expected_families <- c(
    "external_context", "microenvironment", "behaviour", "exposure_state", "joint"
  )
  observed_families <- unique(as.character(context_coefficients$model_family))
  missing_families <- setdiff(expected_families, observed_families)
  if (length(missing_families)) {
    stop("Layered RQ2 model families missing from coefficients: ", paste(missing_families, collapse = ", "))
  }

  # Fig. 2b is allowed to show only a fixed representative subset, but the
  # corresponding joint-model layers must actually have been estimated.
  required_joint_terms <- c(
    "micro_outdoor_fraction", "micro_daylight_indoor_fraction",
    "behaviour_work_fraction", "behaviour_exercise_level"
  )
  observed_joint_terms <- unique(as.character(
    context_coefficients$term[context_coefficients$model_family == "joint"]
  ))
  missing_joint_terms <- setdiff(required_joint_terms, observed_joint_terms)
  if (length(missing_joint_terms)) {
    stop("Layered RQ2 joint-model terms required for Fig. 2b are missing: ",
         paste(missing_joint_terms, collapse = ", "))
  }

  cat(
    "\nLayered context extension: ", CONTEXT_VERSION,
    "\nBase legacy coefficient fits: intentionally skipped; canonical conditional/gamma products retained.",
    "\nModel families: external opportunity, micro-environment, behaviour, exposure state, joint.",
    "\nContext inputs reuse unit_context ERA5 fields plus harmonised MeLiDos diaries; no core or ERA5 ingestion rule is redefined.\n",
    file = file.path(OUT, "RQ2_RUN_REPORT.md"), append = TRUE, sep = ""
  )

  # Delete large row-level model inputs only after the entire layered RQ2 stage
  # has succeeded. Durable pass-1 unit-feature checkpoints are intentionally kept.
  if (!KEEP_MODEL_INPUTS) {
    if (exists("SHARD_DIR") && dir.exists(SHARD_DIR)) {
      unlink(SHARD_DIR, recursive = TRUE, force = TRUE)
    }
    if (exists("CONTEXT_SHARD_ROOT") && dir.exists(CONTEXT_SHARD_ROOT)) {
      unlink(CONTEXT_SHARD_ROOT, recursive = TRUE, force = TRUE)
    }
  }
}

if (is.na(.rq2_models_env_original)) {
  Sys.unsetenv("RQ2_RUN_MODELS")
} else {
  Sys.setenv(RQ2_RUN_MODELS = .rq2_models_env_original)
}
rm(.requested_rq2_models, .rq2_models_env_original)
