# Canonical RQ2 recovery entrypoint: rich deployable temporal context (v5).
# The tested v4 machinery is retained verbatim in 12d_rq2_recovery_core.R;
# this entrypoint loads it without dispatch, then overrides only the context
# representation, XGBoost capacity, provenance/version, and CLI dispatch.
source("scripts/12d_rq2_recovery_core.R")

# Preserve the full daily context used in Fig. 3, while retaining within-day
# structure for the context components that are observable from frozen weather
# and harmonised diary intervals without using any target light measurement.
recovery_temporal_bases <- function() c(
  radiation = "external_daypart",
  outdoor = "microenvironment_daypart",
  daylight_indoor = "microenvironment_daypart",
  daylight_outdoor = "microenvironment_daypart",
  display = "microenvironment_daypart",
  work = "behaviour_daypart",
  home = "behaviour_daypart",
  vehicle = "behaviour_daypart"
)
recovery_temporal_predictors <- function() unlist(lapply(names(recovery_temporal_bases()),
  function(f) paste0("ct_", f, "_", names(recovery_dayparts()))), use.names = FALSE)
recovery_temporal_families <- function() rep(unname(recovery_temporal_bases()),
  each = length(recovery_dayparts()))

# Four local dayparts x eight deployable contextual signals = 32 temporal context
# predictors, in addition to the existing 18 daily context predictors.
recovery_temporal_context <- function(obj, core_version, rq1_version) {
  ms_assert_version(obj, "core_artifact_version", core_version)
  ms_assert_version(obj, "rq1_analysis_version", rq1_version)
  if (!identical(obj$artifact_type, "recovery_deployable_dayparts_v2") ||
      !identical(obj$daypart_contract, "local_06_11_14_18_24_v1")) stop("Incompatible frozen daypart contract")
  recovery_require(obj$sources, c("role", "path", "md5"), "Daypart source provenance")
  if (!setequal(obj$sources$role, c("core_weather", "harmonized_diary")) ||
      anyNA(obj$sources) || any(!grepl("^[a-fA-F0-9]{32}$", obj$sources$md5)))
    stop("Daypart provenance requires frozen weather and harmonized diary source hashes")
  z <- obj$data
  fraction_fields <- c("outdoor_fraction", "daylight_indoor_fraction", "daylight_outdoor_fraction",
    "display_fraction", "work_fraction", "home_fraction", "vehicle_fraction")
  recovery_require(z, c("site", "Id", "Date", "timezone", "daypart", "radiation_mean_w_m2",
    fraction_fields, "weather_hours", "environment_hours", "source_hours", "activity_hours"),
    "Frozen rich daypart context")
  z <- z |> mutate(Id = as.character(Id), Date = as.Date(Date))
  ms_assert_unique(z, c("site", "Id", "Date", "daypart"), "Frozen rich daypart context")
  if (anyNA(z$timezone) || any(!z$timezone %in% OlsonNames()) ||
      any(!z$daypart %in% names(recovery_dayparts()))) stop("Invalid local timezone/daypart")
  counts <- z |> count(site, Id, Date)
  if (any(counts$n != 4L)) stop("Each context day needs four explicit daypart rows (NA values allowed)")
  if (nrow(distinct(z, site, Id, Date, timezone)) != nrow(counts)) stop("Inconsistent context timezone within day")
  nums <- c("radiation_mean_w_m2", fraction_fields, "weather_hours", "environment_hours",
    "source_hours", "activity_hours")
  for (p in nums) {
    if (!is.numeric(z[[p]]) && !all(is.na(z[[p]]))) stop("Non-numeric daypart field: ", p)
    z[[p]] <- as.numeric(z[[p]])
    if (any(is.infinite(z[[p]])) || any(z[[p]] < 0, na.rm = TRUE)) stop("Invalid daypart values: ", p)
  }
  for (p in fraction_fields) if (any(z[[p]] > 1, na.rm = TRUE)) stop("Invalid daypart fraction: ", p)
  z$radiation_mean_w_m2[!is.finite(z$weather_hours) | z$weather_hours <= 0] <- NA_real_
  z$outdoor_fraction[!is.finite(z$environment_hours) | z$environment_hours <= 0] <- NA_real_
  for (p in c("daylight_indoor_fraction", "daylight_outdoor_fraction", "display_fraction"))
    z[[p]][!is.finite(z$source_hours) | z$source_hours <= 0] <- NA_real_
  for (p in c("work_fraction", "home_fraction", "vehicle_fraction"))
    z[[p]][!is.finite(z$activity_hours) | z$activity_hours <= 0] <- NA_real_
  out <- distinct(select(z, site, Id, Date))
  for (part in names(recovery_dayparts())) {
    p <- z |> filter(daypart == part) |> transmute(site, Id, Date,
      radiation = log1p(radiation_mean_w_m2), outdoor = outdoor_fraction,
      daylight_indoor = daylight_indoor_fraction, daylight_outdoor = daylight_outdoor_fraction,
      display = display_fraction, work = work_fraction, home = home_fraction, vehicle = vehicle_fraction)
    names(p)[4:11] <- paste0("ct_", names(recovery_temporal_bases()), "_", part)
    out <- left_join(out, p, by = c("site", "Id", "Date"), relationship = "one-to-one")
  }
  recovery_require(out, c("site", "Id", "Date", recovery_temporal_predictors()), "Rich temporal context export")
  out
}

# Row-preserving interval view of deployable diary context. Source, environment
# and activity reporting domains remain separate, so missingness is not converted
# into absence. No light-series or target metric enters these features.
recovery_diary_intervals <- function(diary) {
  recovery_require(diary, c("Id", "start", "end", "lightsource_primary", rq_context_activity_columns()),
    "Light-exposure diary")
  if (!inherits(diary$start, "POSIXt") || !inherits(diary$end, "POSIXt"))
    stop("Diary must retain harmonized timezone-aware timestamps")
  classified <- rq_context_prepare_diary(diary)
  if (nrow(classified) != nrow(diary) || !identical(classified$Id, as.character(diary$Id)))
    stop("Diary classification changed row identity")
  source <- as.character(diary$lightsource_primary)
  source_reported <- !is.na(source) & nzchar(source)
  activity_reported <- !is.na(classified$activity)
  work <- coalesce(as.logical(diary$act_working_indoor), FALSE) |
    coalesce(as.logical(diary$act_working_outdoor), FALSE)
  home <- coalesce(as.logical(diary$act_home), FALSE)
  vehicle <- coalesce(as.logical(diary$act_road_vehicle), FALSE)
  tibble(Id = as.character(diary$Id), start = as.numeric(diary$start), end = as.numeric(diary$end),
    outdoor = ifelse(is.na(classified$environment), NA_real_, as.numeric(classified$environment == "outdoor")),
    daylight_indoor = ifelse(source_reported, as.numeric(source == "Daylight indoors"), NA_real_),
    daylight_outdoor = ifelse(source_reported, as.numeric(source == "Daylight outdoors (including shade)"), NA_real_),
    display = ifelse(source_reported, as.numeric(source == "Emissive display light"), NA_real_),
    work = ifelse(activity_reported, as.numeric(work), NA_real_),
    home = ifelse(activity_reported, as.numeric(home), NA_real_),
    vehicle = ifelse(activity_reported, as.numeric(vehicle), NA_real_))
}

# Integrate diary flags within a daypart by reporting domain. Identical duplicate
# intervals count once; conflicting overlapping reports are excluded only for the
# affected domain/segment and remain explicitly audited.
recovery_diary_part <- function(d, lower, upper) {
  d <- d[is.finite(d$start) & is.finite(d$end) & d$end > d$start &
    d$start < upper & d$end > lower, , drop = FALSE]
  breaks <- sort(unique(c(lower, upper, pmax(lower, d$start), pmin(upper, d$end))))
  domain_signals <- list(environment = "outdoor",
    source = c("daylight_indoor", "daylight_outdoor", "display"),
    activity = c("work", "home", "vehicle"))
  signal_names <- unlist(domain_signals, use.names = FALSE)
  totals <- setNames(rep(0, length(signal_names) + 1L + 2L * length(domain_signals)),
    c(signal_names, "overlap", names(domain_signals), paste0(names(domain_signals), "_conflict")))
  for (j in seq_len(length(breaks) - 1L)) {
    a <- breaks[j]; b <- breaks[j + 1L]; duration <- b - a
    active <- d[d$start < b & d$end > a, , drop = FALSE]
    if (nrow(active) > 1L) totals["overlap"] <- totals["overlap"] + duration
    for (domain in names(domain_signals)) {
      signals <- domain_signals[[domain]]
      if (!nrow(active)) next
      mat <- as.matrix(active[, signals, drop = FALSE])
      complete <- apply(is.finite(mat), 1L, all)
      profiles <- unique(mat[complete, , drop = FALSE])
      if (nrow(profiles) == 1L) {
        totals[domain] <- totals[domain] + duration
        totals[signals] <- totals[signals] + duration * as.numeric(profiles[1, ])
      } else if (nrow(profiles) > 1L) {
        totals[paste0(domain, "_conflict")] <- totals[paste0(domain, "_conflict")] + duration
      }
    }
  }
  fraction <- function(signal, domain) unname(if (totals[domain] > 0) totals[signal] / totals[domain] else NA_real_)
  c(outdoor_fraction = fraction("outdoor", "environment"),
    daylight_indoor_fraction = fraction("daylight_indoor", "source"),
    daylight_outdoor_fraction = fraction("daylight_outdoor", "source"),
    display_fraction = fraction("display", "source"),
    work_fraction = fraction("work", "activity"), home_fraction = fraction("home", "activity"),
    vehicle_fraction = fraction("vehicle", "activity"),
    environment_hours = unname(totals["environment"] / 3600), source_hours = unname(totals["source"] / 3600),
    activity_hours = unname(totals["activity"] / 3600), overlap_hours = unname(totals["overlap"] / 3600),
    environment_conflict_hours = unname(totals["environment_conflict"] / 3600),
    source_conflict_hours = unname(totals["source_conflict"] / 3600),
    activity_conflict_hours = unname(totals["activity_conflict"] / 3600))
}

recovery_daypart_provenance <- function(weather_path, unit_path, diary_paths, core_version, rq1_version) {
  paths <- c(weather_path, unit_path, unname(diary_paths))
  if (any(!file.exists(paths))) stop("Missing daypart source: ", paste(paths[!file.exists(paths)], collapse = ", "))
  funcs <- c("recovery_dayparts", "recovery_temporal_bases", "recovery_daypart_bounds", "recovery_diary_intervals",
    "recovery_diary_part", "recovery_weather_part", "recovery_build_dayparts", "recovery_temporal_context",
    "rq_context_prepare_diary", "rq_context_activity_columns", "load_raw_file")
  list(builder_version = "daypart_overlap_v2_rich_context", core_artifact_version = core_version,
    rq1_analysis_version = rq1_version, daypart_definition = recovery_dayparts(),
    source_md5 = tools::md5sum(paths), diary_sites = names(diary_paths),
    builder_hash = recovery_hash(lapply(funcs, function(nm) list(name = nm, body = body(get(nm))))),
    source_rules = paste("minute_left_interval_60s; diary_half_open_overlap_union;",
      "environment/source/activity domains kept separate; conflicts_missing_per_domain"))
}

recovery_ensure_dayparts <- function(path, weather_path, unit_path, diary_paths, core_version, rq1_version) {
  provenance <- recovery_daypart_provenance(weather_path, unit_path, diary_paths, core_version, rq1_version)
  old <- if (file.exists(path)) readRDS(path) else NULL
  if (!is.null(old) && identical(old$provenance, provenance)) {
    recovery_temporal_context(old, core_version, rq1_version)
    if (!identical(old$data_md5, recovery_hash(old$data))) stop("Daypart cache content hash mismatch")
    if (!identical(old$daypart_definition, recovery_dayparts()) || is.null(old$generated_at))
      stop("Daypart cache metadata mismatch")
    source_paths <- c(weather_path, unname(diary_paths))
    if (!identical(as.character(old$sources$path), source_paths) ||
        !identical(as.character(old$sources$md5), unname(provenance$source_md5[source_paths])))
      stop("Daypart cache source provenance mismatch")
    message("Daypart context: reused ", path)
    return(list(object = old, reused = TRUE))
  }
  message("Daypart context: building rich temporal representation from frozen weather/diary intervals")
  unit <- recovery_read_csv(unit_path)
  ms_assert_version(unit, "core_artifact_version", core_version)
  recovery_require(unit, c("site", "Id", "Date", "timezone", "analysis_unit_type"), "Core context calendar")
  calendar <- unit |> filter(analysis_unit_type == "participant_day") |>
    distinct(site, Id, Date, timezone) |> mutate(Id = as.character(Id), Date = as.Date(Date))
  ms_assert_unique(calendar, c("site", "Id", "Date"), "Core context calendar")
  if (anyNA(calendar$timezone) || any(!calendar$timezone %in% OlsonNames())) stop("Invalid core calendar timezone")
  if (!setequal(unique(calendar$site), names(diary_paths))) stop("Diary sites differ from frozen calendar")
  weather <- readr::read_csv(weather_path, col_types = readr::cols_only(core_artifact_version = readr::col_character(),
    site = readr::col_character(), time_utc = readr::col_datetime(), timezone = readr::col_character(),
    ssrd_w_m2 = readr::col_double()), progress = FALSE)
  ms_assert_version(weather, "core_artifact_version", core_version)
  diaries <- lapply(diary_paths, load_raw_file, modality = "lightexposurediary")
  built <- recovery_build_dayparts(calendar, weather, diaries)
  source_paths <- c(weather_path, unname(diary_paths))
  obj <- list(artifact_type = "recovery_deployable_dayparts_v2", daypart_contract = "local_06_11_14_18_24_v1",
    core_artifact_version = core_version, rq1_analysis_version = rq1_version,
    daypart_definition = recovery_dayparts(), generated_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%OS%z"),
    provenance = provenance, sources = tibble(role = c("core_weather", rep("harmonized_diary", length(diary_paths))),
      path = source_paths, md5 = unname(tools::md5sum(source_paths))), data = built$data, audit = built$audit,
    data_md5 = recovery_hash(built$data))
  recovery_temporal_context(obj, core_version, rq1_version)
  if (!identical(provenance, recovery_daypart_provenance(weather_path, unit_path, diary_paths, core_version, rq1_version)))
    stop("Daypart source changed during build; cache not installed")
  recovery_atomic(obj, path)
  message("Daypart context: installed ", nrow(obj$data), " rows at ", path)
  list(object = obj, reused = FALSE)
}

# With 18 daily + 32 daypart context predictors, depth 3/min-child 5 can become a
# practical under-capacity bottleneck. Keep one prespecified model for every branch,
# but modestly relax capacity; participant-grouped inner early stopping still picks
# the effective number of trees and all branches retain identical tuning protocol.
recovery_xgb_config <- function() list(nrounds = 800L, early_stopping_rounds = 40L,
  params = list(objective = "reg:squarederror", eval_metric = "rmse", booster = "gbtree",
    tree_method = "hist", max_depth = 4L, eta = .05, min_child_weight = 3,
    subsample = 1, colsample_bytree = 1, lambda = 1, alpha = 0, base_score = 0, nthread = 1L))

recovery_worker_exports <- function() c("recovery_task", "recovery_fit", "recovery_predict",
  "recovery_design", "recovery_delta", "recovery_predictors", "recovery_atomic", "rq2_model_helpers",
  "recovery_target", "recovery_inner", "recovery_boost", "recovery_xgb_config",
  "recovery_layer_predictors", "recovery_signature_predictors", "recovery_temporal_bases",
  "recovery_temporal_predictors", "recovery_context_predictors", "recovery_states", "recovery_dayparts",
  "rq2_context_external_predictors", "rq2_context_micro_predictors", "rq2_context_behaviour_predictors")

# v5 differs from v4 only in the richer deployable temporal context and the modest
# prespecified XGBoost capacity relaxation above. The factorial estimand, folds,
# support, learners, residual target and held-out scoring remain unchanged.
recovery_run <- function(inputs) {
  if (!requireNamespace("xgboost", quietly = TRUE)) stop("Formal recovery requires xgboost; no automatic installation")
  integer_env <- function(name, default, minimum) {
    v <- suppressWarnings(as.integer(Sys.getenv(name, as.character(default))))
    if (length(v) != 1L || !is.finite(v) || v < minimum) stop("Invalid ", name)
    v
  }
  seed <- integer_env("RQ2_RECOVERY_SEED", 20260912L, 1L)
  folds <- integer_env("RQ2_RECOVERY_FOLDS", 5L, 2L)
  lambda <- as.numeric(Sys.getenv("RQ2_RECOVERY_LAMBDA", "0.01"))
  floor <- as.numeric(Sys.getenv("RQ2_RECOVERY_G_FLOOR", "0.000001"))
  if (!is.finite(lambda) || lambda <= 0 || !is.finite(floor) || floor <= 0) stop("Invalid lambda/G floor")
  workers <- ms_resolve_workers("RQ2_RECOVERY_WORKERS", default = 36L, cap = 48L)
  ms_worker_init()
  code <- c("scripts/12d_rq2_recovery.R", "scripts/12d_rq2_recovery_core.R",
    "scripts/utils/rq1_inference.R", "scripts/utils/rq1_inference_contract.R", "scripts/utils/rq1_pairwise_artifacts.R",
    "scripts/utils/rq2_context_features.R", "scripts/utils/rq2_model_helpers.R",
    "scripts/utils/parallel_runtime.R", "scripts/utils/analysis_design.R", "scripts/utils/artifact_validation.R",
    "scripts/12c_rq2_context_models.R", "scripts/utils/rq_context.R", "scripts/utils/melidos_io.R",
    "scripts/utils/core_context.R", "scripts/utils/core_artifacts.R", "external/LightLogR/R/normalise.R")
  provenance <- list(recovery_version = "rq2_recovery_v5_rich_temporal_context", rq1_analysis_version = inputs$version,
    core_artifact_version = inputs$core, analysis_design_id = ms_analysis_design_id(),
    input_md5 = tools::md5sum(inputs$paths), code_md5 = tools::md5sum(code),
    seed = seed, folds = folds, lambda = lambda, G_floor = floor, predictors = recovery_predictors(),
    signature_predictors = recovery_signature_predictors(), temporal_predictors = recovery_temporal_predictors(),
    fitted_states = recovery_states(), factorial_context_state = "Y_L plus 18 daily + 32 daypart context predictors without low-measurement signature",
    temporal_source_manifest = inputs$temporal_sources, dayparts = recovery_dayparts(),
    learners = c(primary = "xgboost", sensitivity = "ridge"), xgb_config = recovery_xgb_config(),
    inner_validation = "20% of outer-training participants; shared split/protocol across predictor states; full-training refit",
    R = R.version.string, packages = sapply(c("dplyr", "tibble", "readr", "data.table", "xgboost"),
      function(p) as.character(utils::packageVersion(p))),
    context_provenance_limit = "Daily context is frozen; rich daypart context is rebuilt only from frozen weather and harmonized diary intervals with source hashes",
    scale_role = "Frozen RQ1 SD used ONLY for scoring; never for fitting/tuning",
    model = "Identity-anchored residual correction; factorial context-only branch; rich deployable temporal context; primary XGBoost with nested early stopping; ridge sensitivity; no target-state predictors")
  run_id <- recovery_hash(provenance)
  out <- file.path("results/rq2/recovery", inputs$version, run_id)
  dir.create(out, recursive = TRUE, showWarnings = FALSE)
  recovery_atomic(c(provenance, list(run_id = run_id, workers = workers,
    started = Sys.time(), session = capture.output(sessionInfo()))), file.path(out, "provenance.rds"))
  message("Recovery: validate and extract frozen non-duration anchors")
  x <- recovery_pairs(inputs)
  signature <- recovery_signature(recovery_read_csv(inputs$paths[["unit_context"]]), inputs$core, requested = x)
  x <- recovery_join_information(x, signature, inputs$temporal)
  readr::write_csv(signature, file.path(out, "low_signature_features.csv"))
  readr::write_csv(inputs$temporal, file.path(out, "temporal_context_features.csv"))
  readr::write_csv(x |> distinct(support_id, site, Id, Date, candidate_config,
    signature_valid_hours, signature_row_present, temporal_row_present), file.path(out, "information_support_audit.csv"))
  pm <- x |> distinct(site, participant_key) |> arrange(site, participant_key)
  if (folds > nrow(pm)) stop("More folds than participants")
  set.seed(seed, kind = "Mersenne-Twister", normal.kind = "Inversion", sample.kind = "Rejection")
  pm <- pm |> group_by(site) |> mutate(fold = sample(rep(seq_len(folds), length.out = n()))) |> ungroup()
  x <- left_join(x, pm, by = c("site", "participant_key"), relationship = "many-to-one")
  readr::write_csv(pm, file.path(out, "participant_folds.csv"))
  keys <- c("dimension", "comparison_pair_id", "candidate_config", "support_id",
    "metric", "metric_class", "metric_geometry")
  groups <- x |> group_by(across(all_of(keys))) |> group_split(.keep = TRUE)
  catalog <- bind_rows(lapply(groups, function(g) {
    meta <- distinct(select(g, all_of(keys)))
    bind_cols(meta, tibble(n_rows = nrow(g), n_eligible = sum(g$eligible),
      n_context_rows = sum(g$context_row_present & g$eligible),
      n_eligible_participants = n_distinct(g$participant_key[g$eligible]),
      A_raw_all_eligible = if (any(g$eligible)) mean(abs(g$z[g$eligible])) else NA_real_))
  })) |> mutate(task_index = row_number()) |>
    left_join(select(inputs$summary, dimension, comparison_pair_id, metric,
      A_frozen_RQ1 = A_mean_absolute), by = c("dimension", "comparison_pair_id", "metric"), relationship = "many-to-one")
  mismatch <- with(catalog, is.finite(A_raw_all_eligible) &
    (!is.finite(A_frozen_RQ1) | abs(A_raw_all_eligible - A_frozen_RQ1) > 1e-7 * (1 + abs(A_frozen_RQ1))))
  if (any(mismatch)) stop("Anchor raw A does not reproduce frozen RQ1 summary; inspect support/version")
  unavailable <- x |> filter(!eligible) |> count(dimension, comparison_pair_id, metric, support_id, pair_reason, available,
    name = "n_rows")
  readr::write_csv(unavailable, file.path(out, "unavailable_audit.csv"))
  readr::write_csv(bind_rows(
    tibble(predictor = recovery_signature_predictors(), family = "low_signature"),
    tibble(predictor = recovery_predictors(), family = c(rep("external", 8), rep("microenvironment", 4), rep("behaviour", 6))),
    tibble(predictor = recovery_temporal_predictors(), family = recovery_temporal_families())),
    file.path(out, "predictor_allowlist.csv"))
  input_paths <- vapply(seq_along(groups), function(i) {
    ip <- file.path(out, "inputs", sprintf("task_%04d.rds", i))
    recovery_atomic(groups[[i]], ip)
    ip
  }, character(1))
  catalog <- bind_rows(lapply(c("xgboost", "ridge"), function(learner) {
    catalog |> mutate(base_task_index = task_index, learner = learner)
  })) |> mutate(task_index = row_number())
  readr::write_csv(catalog, file.path(out, "task_catalog.csv"))
  tasks <- lapply(seq_len(nrow(catalog)), function(i) {
    list(index = i, input = input_paths[[catalog$base_task_index[i]]],
      output = file.path(out, "checkpoints", sprintf("task_%04d.rds", i)),
      learner = catalog$learner[i], seed = seed + catalog$base_task_index[i],
      xgb_config = recovery_xgb_config(), run_id = run_id, lambda = lambda, meta = catalog[i, ])
  })
  rm(x, groups); invisible(gc(FALSE))
  message("Recovery: ", length(tasks), " tasks; ", workers, " workers; ", folds, " grouped folds; ", out)
  refs <- ms_parallel_map(tasks, recovery_task, workers = workers, seed = seed,
    packages = c("dplyr", "tibble"), exports = recovery_worker_exports())
  state_rows <- list(); statuses <- list(); fold_rows <- list()
  for (r in refs) {
    obj <- readRDS(r$path)
    statuses[[r$index]] <- bind_cols(catalog[r$index, ], tibble(status = obj$status,
      error = if (is.null(obj$error)) NA_character_ else obj$error,
      reused = r$reused, checkpoint = r$path, elapsed_seconds = obj$elapsed_seconds))
    if (!nrow(obj$predictions)) {
      state_rows[[r$index]] <- bind_cols(catalog[rep(r$index, length(recovery_states())), ], tibble(
        state = recovery_states(), n_test = 0L, n_test_participants = 0L,
        MAE_native = NA_real_, RMSE_native = NA_real_, A = NA_real_, B = NA_real_,
        standardized_RMSE = NA_real_, n_circular_fallback = 0L, status = obj$status))
      next
    }
    s <- obj$predictions |> group_by(state) |> summarise(n_test = n(),
      n_test_participants = n_distinct(participant_key), MAE_native = mean(abs(error)),
      RMSE_native = sqrt(mean(error^2)), A = mean(abs(standardized_error)),
      B = mean(standardized_error), standardized_RMSE = sqrt(mean(standardized_error^2)),
      n_circular_fallback = sum(circular_fallback), status = obj$status, .groups = "drop")
    stopifnot(n_distinct(s$n_test) == 1L)
    state_rows[[r$index]] <- bind_cols(catalog[rep(r$index, nrow(s)), ], s)
    f <- obj$predictions |> group_by(fold, state) |> summarise(n_test = n(),
      A = mean(abs(standardized_error)), .groups = "drop")
    fold_rows[[r$index]] <- bind_cols(catalog[rep(r$index, nrow(f)), ], f)
  }
  status <- bind_rows(statuses); states <- bind_rows(state_rows)
  readr::write_csv(status, file.path(out, "task_status.csv"))
  readr::write_csv(states, file.path(out, "heldout_errors.csv"))
  readr::write_csv(bind_rows(fold_rows), file.path(out, "fold_errors.csv"))
  if (any(is.finite(states$A))) recovery_summaries(filter(states, is.finite(A)), out, floor)
  recovery_atomic(list(run_id = run_id, complete = !any(status$status == "failed"),
    statuses = status, heldout_errors = states, provenance = provenance), file.path(out, "recovery_manifest.rds"))
  message("Recovery outputs: ", out)
  if (any(status$status == "failed")) stop("Some recovery tasks failed; successful checkpoints retained. Restart same command.")
  invisible(out)
}

if (sys.nframe() == 0L) {
  args <- commandArgs(trailingOnly = TRUE)
  command <- if (length(args)) args[[1]] else ""
  allowed <- c("--check-inputs", "--build-context", "--smoke-test", "--run", "--ablation")
  if (!command %in% allowed || length(args) > if (command == "--ablation") 2L else 1L)
    stop("Use --check-inputs, --build-context, --smoke-test, --run, or --ablation [recovery_run_dir]")
  if (command == "--ablation") {
    recovery_ablation_summarize(if (length(args) == 2L) args[[2]] else NULL)
  } else {
    inputs <- recovery_inputs()
    if (length(inputs$problems)) stop(paste(c(inputs$problems,
      "No upstream process was invoked. Supply the existing frozen artifacts."), collapse = "\n"))
    if (command == "--check-inputs") message("Frozen paths and rich context schemas validated; daypart cache ready. Pair contents are checked by --smoke-test/--run.")
    else if (command == "--build-context") message("Rich daypart context ready; reused=", inputs$temporal_reused)
    else if (command == "--smoke-test") recovery_smoke(inputs)
    else recovery_run(inputs)
  }
}
