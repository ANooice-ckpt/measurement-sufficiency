# Independent, frozen-input RQ2 recovery prototype. No upstream entrypoint is sourced.
# Rscript scripts/12d_rq2_recovery.R --check-inputs  # may build ONLY the daypart cache
# RQ2_RECOVERY_WORKERS=36 Rscript scripts/12d_rq2_recovery.R --run
# See docs/RQ2_RECOVERY.md for the estimand, deployment boundary and outputs.
suppressPackageStartupMessages({ library(dplyr); library(tibble) })
source("scripts/utils/rq1_inference.R")
source("scripts/utils/rq2_context_features.R")
source("scripts/utils/rq2_model_helpers.R")
source("scripts/utils/parallel_runtime.R")

recovery_predictors <- function() c(rq2_context_external_predictors(),
  rq2_context_micro_predictors(), rq2_context_behaviour_predictors())
recovery_dayparts <- function() list(morning = 6:10, midday = 11:13,
  afternoon = 14:17, evening = 18:23)
recovery_signature_predictors <- function() paste0("sl_", c("mean", "median", "q10", "q90",
  "iqr", "sd", "min", "max", names(recovery_dayparts()), "hour_fraction_ge250",
  "hour_fraction_le10", "adjacent_abs_change", "lag1_correlation"))
recovery_temporal_predictors <- function() unlist(lapply(c("external", "micro", "behaviour"),
  function(f) paste0("ct_", f, "_", names(recovery_dayparts()))), use.names = FALSE)
recovery_layer_predictors <- function(state) {
  switch(state, calibration = character(), signature = recovery_signature_predictors(),
    context = c(recovery_signature_predictors(), recovery_predictors(), recovery_temporal_predictors()),
    stop("Unknown recovery information layer"))
}
recovery_states <- function() c("raw", "calibration", "signature", "context")

# Compact, configuration-local hourly basis ONLY; never the 52 target-metric vector.
# isiv_hXX is already mean(log10(observed low-channel light + 0.1)) in core.
recovery_signature <- function(unit, core_version, requested = NULL) {
  hours <- sprintf("isiv_h%02d", 0:23)
  keys <- c("support_id", "site", "Id", "Date", "candidate_config")
  recovery_require(unit, c("core_artifact_version", "config_id", "analysis_unit_type",
    "placement", "optical", "resolution_s", "support_id", "site", "Id", "Date", hours), "Frozen low-configuration hourly basis")
  ms_assert_version(unit, "core_artifact_version", core_version)
  if (anyNA(unit[c("config_id", "placement", "optical", "resolution_s")]) ||
      any(unit$config_id != paste0(unit$placement, "__", unit$optical, "__", unit$resolution_s, "s")))
    stop("Hourly basis configuration metadata mismatch")
  if (!is.null(requested)) unit <- semi_join(unit, requested,
    by = c("support_id", "site", "Id", "Date", config_id = "candidate_config"))
  unit <- unit |> filter(analysis_unit_type == "participant_day",
    config_id %in% rq1_inference_anchor_map()$candidate_config) |>
    transmute(support_id, site, Id = as.character(Id), Date = as.Date(Date),
      candidate_config = config_id, across(all_of(hours)))
  ms_assert_unique(unit, keys, "Low-configuration hourly basis")
  for (p in hours) {
    if (!is.numeric(unit[[p]]) && !all(is.na(unit[[p]]))) stop("Non-numeric hourly basis: ", p)
    unit[[p]] <- as.numeric(unit[[p]])
    if (any(is.infinite(unit[[p]]))) stop("Infinite hourly basis: ", p)
  }
  rows <- lapply(seq_len(nrow(unit)), function(i) {
    h <- as.numeric(unit[i, hours]); v <- h[is.finite(h)]
    avg <- function(x) if (any(is.finite(x))) mean(x[is.finite(x)]) else NA_real_
    stats <- if (length(v)) c(mean(v), median(v), quantile(v, c(.1, .9), names = FALSE),
      IQR(v), if (length(v) > 1L) sd(v) else NA_real_, min(v), max(v)) else rep(NA_real_, 8L)
    parts <- vapply(recovery_dayparts(), function(ix) avg(h[ix + 1L]), numeric(1))
    a <- h[1:23]; b <- h[2:24]; ok <- is.finite(a) & is.finite(b)
    lag <- if (sum(ok) >= 3L && sd(a[ok]) > 0 && sd(b[ok]) > 0) cor(a[ok], b[ok]) else NA_real_
    c(stats, parts, if (length(v)) mean(v >= log10(250.1)) else NA_real_,
      if (length(v)) mean(v <= log10(10.1)) else NA_real_, avg(abs(a - b)), lag)
  })
  features <- as_tibble(matrix(unlist(rows), nrow = nrow(unit), ncol = 16L, byrow = TRUE,
    dimnames = list(NULL, recovery_signature_predictors())))
  bind_cols(select(unit, all_of(keys)), features,
    tibble(signature_valid_hours = rowSums(is.finite(as.matrix(unit[hours])))))
}

# New compact frozen context export: four local dayparts, three source families.
# Existing daily context cannot be disaggregated into these values.
recovery_temporal_context <- function(obj, core_version, rq1_version) {
  ms_assert_version(obj, "core_artifact_version", core_version)
  ms_assert_version(obj, "rq1_analysis_version", rq1_version)
  if (!identical(obj$artifact_type, "recovery_deployable_dayparts_v1") ||
      !identical(obj$daypart_contract, "local_06_11_14_18_24_v1")) stop("Incompatible frozen daypart contract")
  recovery_require(obj$sources, c("role", "path", "md5"), "Daypart source provenance")
  if (!setequal(obj$sources$role, c("core_weather", "harmonized_diary")) ||
      anyNA(obj$sources) || any(!grepl("^[a-fA-F0-9]{32}$", obj$sources$md5)))
    stop("Daypart provenance requires frozen weather and harmonized diary source hashes")
  z <- obj$data
  recovery_require(z, c("site", "Id", "Date", "timezone", "daypart", "radiation_mean_w_m2",
    "outdoor_fraction", "work_fraction", "weather_hours", "environment_hours", "activity_hours"), "Frozen daypart context")
  z <- z |> mutate(Id = as.character(Id), Date = as.Date(Date))
  ms_assert_unique(z, c("site", "Id", "Date", "daypart"), "Frozen daypart context")
  if (anyNA(z$timezone) || any(!z$timezone %in% OlsonNames()) ||
      any(!z$daypart %in% names(recovery_dayparts()))) stop("Invalid local timezone/daypart")
  counts <- z |> count(site, Id, Date)
  if (any(counts$n != 4L)) stop("Each context day needs four explicit daypart rows (NA values allowed)")
  if (nrow(distinct(z, site, Id, Date, timezone)) != nrow(counts)) stop("Inconsistent context timezone within day")
  nums <- c("radiation_mean_w_m2", "outdoor_fraction", "work_fraction",
    "weather_hours", "environment_hours", "activity_hours")
  for (p in nums) {
    if (!is.numeric(z[[p]]) && !all(is.na(z[[p]]))) stop("Non-numeric daypart field: ", p)
    z[[p]] <- as.numeric(z[[p]])
    if (any(is.infinite(z[[p]])) || any(z[[p]] < 0, na.rm = TRUE)) stop("Invalid daypart values: ", p)
  }
  for (p in c("outdoor_fraction", "work_fraction"))
    if (any(z[[p]] > 1, na.rm = TRUE)) stop("Invalid daypart fraction: ", p)
  z$radiation_mean_w_m2[!is.finite(z$weather_hours) | z$weather_hours <= 0] <- NA_real_
  z$outdoor_fraction[!is.finite(z$environment_hours) | z$environment_hours <= 0] <- NA_real_
  z$work_fraction[!is.finite(z$activity_hours) | z$activity_hours <= 0] <- NA_real_
  out <- distinct(select(z, site, Id, Date))
  for (part in names(recovery_dayparts())) {
    p <- z |> filter(daypart == part) |> transmute(site, Id, Date,
      external = log1p(radiation_mean_w_m2), micro = outdoor_fraction, behaviour = work_fraction)
    names(p)[4:6] <- paste0("ct_", c("external", "micro", "behaviour"), "_", part)
    out <- left_join(out, p, by = c("site", "Id", "Date"), relationship = "one-to-one")
  }
  out
}

# These functions operate exclusively on frozen weather and harmonized diary
# intervals. No light-series, metric, exposure-state or outcome operator is used.
recovery_daypart_bounds <- function(date, timezone, part) {
  hours <- recovery_dayparts()[[part]]
  end_hour <- max(hours) + 1L
  start <- as.POSIXct(paste(as.Date(date), sprintf("%02d:00:00", min(hours))), tz = timezone)
  end <- as.POSIXct(paste(as.Date(date) + as.integer(end_hour == 24L),
    sprintf("%02d:00:00", end_hour %% 24L)), tz = timezone)
  if (!is.finite(as.numeric(start)) || !is.finite(as.numeric(end)) || end <= start)
    stop("Invalid local daypart boundary")
  c(start = as.numeric(start), end = as.numeric(end))
}
recovery_diary_intervals <- function(diary) {
  recovery_require(diary, c("Id", "start", "end", rq_context_activity_columns()), "Light-exposure diary")
  if (!inherits(diary$start, "POSIXt") || !inherits(diary$end, "POSIXt"))
    stop("Diary must retain harmonized timezone-aware timestamps")
  classified <- rq_context_prepare_diary(diary)
  if (nrow(classified) != nrow(diary) || !identical(classified$Id, as.character(diary$Id)))
    stop("Diary classification changed row identity")
  work <- coalesce(as.logical(diary$act_working_indoor), FALSE) |
    coalesce(as.logical(diary$act_working_outdoor), FALSE)
  tibble(Id = as.character(diary$Id), start = as.numeric(diary$start), end = as.numeric(diary$end),
    outdoor = ifelse(is.na(classified$environment), NA_real_, as.numeric(classified$environment == "outdoor")),
    work = ifelse(is.na(classified$activity), NA_real_, as.numeric(work)))
  # Keep the original exclusive end. The existing classification helper's -1 s
  # convention is for inclusive timestamp joins, not duration integration.
}
recovery_diary_part <- function(d, lower, upper) {
  d <- d[is.finite(d$start) & is.finite(d$end) & d$end > d$start &
    d$start < upper & d$end > lower, , drop = FALSE]
  breaks <- sort(unique(c(lower, upper, pmax(lower, d$start), pmin(upper, d$end))))
  totals <- c(outdoor = 0, work = 0, environment = 0, activity = 0,
    overlap = 0, environment_conflict = 0, activity_conflict = 0)
  for (j in seq_len(length(breaks) - 1L)) {
    a <- breaks[j]; b <- breaks[j + 1L]; duration <- b - a
    active <- d[d$start < b & d$end > a, , drop = FALSE]
    if (nrow(active) > 1L) totals["overlap"] <- totals["overlap"] + duration
    for (family in c("outdoor", "work")) {
      domain <- if (family == "outdoor") "environment" else "activity"
      flag <- unique(active[[family]][is.finite(active[[family]])])
      if (length(flag) == 1L) {
        totals[domain] <- totals[domain] + duration
        totals[family] <- totals[family] + duration * flag
      } else if (length(flag) > 1L) {
        nm <- paste0(domain, "_conflict"); totals[nm] <- totals[nm] + duration
      }
    }
  }
  # Identical duplicates count once. Conflicting reports are missing only for
  # the affected domain/segment; all conflicts and overlaps remain in RDS audit.
  c(outdoor_fraction = unname(if (totals["environment"] > 0) totals["outdoor"] / totals["environment"] else NA_real_),
    work_fraction = unname(if (totals["activity"] > 0) totals["work"] / totals["activity"] else NA_real_),
    environment_hours = unname(totals["environment"] / 3600), activity_hours = unname(totals["activity"] / 3600),
    overlap_hours = unname(totals["overlap"] / 3600),
    environment_conflict_hours = unname(totals["environment_conflict"] / 3600),
    activity_conflict_hours = unname(totals["activity_conflict"] / 3600))
}
recovery_weather_part <- function(w, lower, upper) {
  # Frozen minute value represents [time_utc,time_utc+60 s). Do not interpolate
  # missing minutes or bridge gaps; actual overlap seconds determine weights.
  lo <- findInterval(lower - 60, w$seconds) + 1L
  hi <- findInterval(upper, w$seconds)
  if (lo > hi) return(c(radiation_mean_w_m2 = NA_real_, weather_hours = 0))
  ix <- seq.int(lo, hi)
  weight <- pmax(0, pmin(w$seconds[ix] + 60, upper) - pmax(w$seconds[ix], lower))
  ok <- is.finite(w$ssrd_w_m2[ix]) & weight > 0
  duration <- sum(weight[ok])
  c(radiation_mean_w_m2 = if (duration > 0) sum(weight[ok] * w$ssrd_w_m2[ix][ok]) / duration else NA_real_,
    weather_hours = duration / 3600)
}
recovery_build_dayparts <- function(calendar, weather, diaries) {
  ms_assert_unique(calendar, c("site", "Id", "Date"), "Daypart calendar")
  recovery_require(weather, c("site", "time_utc", "timezone", "ssrd_w_m2"), "Frozen minute weather")
  if (!inherits(weather$time_utc, "POSIXt")) stop("Frozen weather requires parsed UTC timestamps")
  weather$seconds <- as.numeric(weather$time_utc)
  ms_assert_unique(weather, c("site", "seconds"), "Frozen minute weather")
  if (any(!is.finite(weather$seconds)) || any(weather$seconds %% 60 != 0) ||
      any(is.infinite(weather$ssrd_w_m2)) || any(weather$ssrd_w_m2 < 0, na.rm = TRUE))
    stop("Invalid frozen minute weather values/grid")
  rows <- list(); audits <- list(); k <- 0L
  for (site_name in sort(unique(calendar$site))) {
    cal <- calendar[calendar$site == site_name, ]
    w <- weather[weather$site == site_name, ] |> arrange(seconds)
    if (!nrow(w) || !setequal(unique(cal$timezone), unique(w$timezone)))
      stop("Missing weather site or timezone mismatch: ", site_name)
    d <- recovery_diary_intervals(diaries[[site_name]])
    invalid <- !is.finite(d$start) | !is.finite(d$end) | d$end <= d$start | is.na(d$Id)
    audits[[site_name]] <- tibble(site = site_name, diary_rows = nrow(d), invalid_intervals = sum(invalid))
    d <- d[!invalid, ]
    person <- split(d, d$Id)
    weather_days <- new.env(parent = emptyenv())
    for (i in seq_len(nrow(cal))) {
      dd <- person[[cal$Id[i]]]
      if (is.null(dd)) dd <- d[FALSE, ]
      for (part in names(recovery_dayparts())) {
        bounds <- recovery_daypart_bounds(cal$Date[i], cal$timezone[i], part)
        key <- paste(cal$Date[i], part)
        if (!exists(key, weather_days, inherits = FALSE))
          assign(key, recovery_weather_part(w, bounds[1], bounds[2]), weather_days)
        k <- k + 1L
        rows[[k]] <- bind_cols(cal[i, c("site", "Id", "Date", "timezone")], tibble(daypart = part),
          as_tibble_row(get(key, weather_days)), as_tibble_row(recovery_diary_part(dd, bounds[1], bounds[2])))
      }
    }
  }
  list(data = bind_rows(rows), audit = bind_rows(audits))
}
recovery_daypart_provenance <- function(weather_path, unit_path, diary_paths, core_version, rq1_version) {
  paths <- c(weather_path, unit_path, unname(diary_paths))
  if (any(!file.exists(paths))) stop("Missing daypart source: ", paste(paths[!file.exists(paths)], collapse = ", "))
  funcs <- c("recovery_dayparts", "recovery_daypart_bounds", "recovery_diary_intervals",
    "recovery_diary_part", "recovery_weather_part", "recovery_build_dayparts", "recovery_temporal_context",
    "rq_context_prepare_diary", "rq_context_activity_columns", "load_raw_file")
  list(builder_version = "daypart_overlap_v1", core_artifact_version = core_version,
    rq1_analysis_version = rq1_version, daypart_definition = recovery_dayparts(),
    source_md5 = tools::md5sum(paths), diary_sites = names(diary_paths),
    builder_hash = recovery_hash(lapply(funcs, function(nm) list(name = nm, body = body(get(nm))))),
    source_rules = "minute_left_interval_60s; diary_half_open_overlap_union; conflicts_missing_per_domain")
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
  message("Daypart context: building from frozen weather/diary intervals")
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
  obj <- list(artifact_type = "recovery_deployable_dayparts_v1", daypart_contract = "local_06_11_14_18_24_v1",
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

recovery_join_information <- function(x, signature, temporal) {
  keys <- c("support_id", "site", "Id", "Date", "candidate_config")
  ms_assert_unique(signature, keys, "Recovery signature")
  ms_assert_unique(temporal, c("site", "Id", "Date"), "Recovery temporal context")
  x <- left_join(x, mutate(signature, signature_row_present = TRUE), by = keys, relationship = "many-to-one")
  x <- left_join(x, mutate(temporal, temporal_row_present = TRUE),
    by = c("site", "Id", "Date"), relationship = "many-to-one")
  if (any(x$eligible & (is.na(x$signature_row_present) | is.na(x$temporal_row_present))))
    stop("Missing exact low-configuration/support signature or explicit context day; refusing to shrink matched support")
  x
}
recovery_delta <- function(a, b, circular) {
  if (circular) ((a - b + 43200) %% 86400) - 43200 else a - b
}
recovery_require <- function(x, fields, label) {
  missing <- setdiff(fields, names(x))
  if (length(missing)) stop(label, " missing fields: ", paste(missing, collapse = ", "))
}
recovery_hash <- function(x) {
  p <- tempfile(); on.exit(unlink(p)); saveRDS(x, p, version = 3)
  unname(tools::md5sum(p))
}
recovery_atomic <- function(x, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  tmp <- paste0(path, ".tmp.", Sys.getpid())
  on.exit(unlink(tmp), add = TRUE)
  saveRDS(x, tmp, compress = "gzip")
  if (file.exists(path)) unlink(path)
  if (!file.rename(tmp, path)) stop("Cannot install recovery checkpoint: ", path)
}
recovery_read_csv <- function(path) {
  header <- names(readr::read_csv(path, n_max = 0L, show_col_types = FALSE, progress = FALSE))
  types <- setNames(lapply(intersect(c("site", "Id"), header), function(p) readr::col_character()),
    intersect(c("site", "Id"), header))
  readr::read_csv(path, col_types = do.call(readr::cols, c(types, list(.default = readr::col_guess()))),
    show_col_types = FALSE, progress = FALSE)
}

# Resolve only the already-selected non-duration parts. Never rewrite the manifest.
recovery_resolve_parts <- function(upstream, repo_root = ".") {
  if (!dir.exists(upstream$part_dir)) {
    upstream$part_dir <- file.path(repo_root, "results", "rq1", "pairwise_parts",
      rq1_pairwise_version(upstream))
  }
  parts <- rq1_pairwise_part_paths(upstream)
  problems <- character()
  for (p in parts) {
    if (!file.exists(p)) problems <- c(problems, paste0("Missing frozen anchor part: ", p))
    if (!file.exists(paste0(p, ".ok"))) problems <- c(problems, paste0("Missing completion marker: ", p, ".ok"))
  }
  list(upstream = upstream, paths = parts, problems = problems)
}

# Validate small contracts even when local pair parts are absent. No reconstruction.
recovery_inputs <- function() {
  paths <- c(pairwise = "results/rq1/rq1_pairwise_change_long.rds",
    summary = "results/rq1/rq1_pairwise_summary.csv",
    scales = "results/diagnostics/rq1_standardizer_audit.csv",
    context = "results/diagnostics/rq2_layered_context_day_features.csv")
  core_root <- Sys.getenv("RQ2_RECOVERY_CORE_ROOT", "results/core")
  extra_paths <- c(unit_context = file.path(core_root, "unit_context.csv.gz"),
    temporal_context = "results/rq2/recovery_inputs/context_dayparts.rds")
  weather_path <- file.path(core_root, "weather_1min.csv.gz")
  problems <- paste0("Missing frozen input: ", paths[!file.exists(paths)])
  problems <- problems[nzchar(problems) & problems != "Missing frozen input: "]
  if (!requireNamespace("xgboost", quietly = TRUE))
    problems <- c(problems, "Missing required R package: xgboost (no automatic installation)")
  required_extra <- c(extra_paths["unit_context"], weather_path)
  missing_extra <- required_extra[!file.exists(required_extra)]
  if (length(missing_extra)) problems <- c(problems, paste0("Missing frozen input: ", missing_extra))
  if (any(!file.exists(paths))) return(list(problems = problems, paths = paths))
  upstream <- readRDS(paths[["pairwise"]])
  version <- rq1_pairwise_version(upstream)
  core <- paste0("v4_sparse_sampling_complete_days__", ms_core_design_id())
  ms_assert_version(upstream, "analysis_design_id", ms_analysis_design_id())
  ms_assert_version(upstream, "core_artifact_version", core)
  if (!rq1_pairwise_is_partitioned(upstream)) stop("Expected frozen partitioned RQ1 manifest")
  recovery_require(upstream$part_manifest, c("part", "dimension"), "RQ1 part manifest")
  records <- upstream$part_manifest |> filter(dimension == "placement_optical_temporal")
  if (!nrow(records)) stop("No declared non-duration RQ1 parts; refusing to scan duration")
  upstream$parts <- as.character(records$part)
  resolved <- recovery_resolve_parts(upstream)
  upstream <- resolved$upstream
  parts <- resolved$paths
  problems <- c(problems, resolved$problems)
  summary <- recovery_read_csv(paths[["summary"]])
  tryCatch(rq1_assert_summary_version(upstream, summary), error = function(e)
    problems <<- c(problems, conditionMessage(e)))
  tryCatch(ms_assert_version(summary, "core_artifact_version", core), error = function(e)
    problems <<- c(problems, conditionMessage(e)))
  recovery_require(summary, c("dimension", "comparison_pair_id", "metric", "metric_geometry",
    "metric_class", "A_mean_absolute", "comparison_lattice"), "RQ1 summary")
  summary <- semi_join(summary, rq1_inference_anchor_map(), by = c("dimension", "comparison_pair_id")) |>
    filter(!metric %in% c("interdaily_stability", "intradaily_variability"))
  ms_assert_unique(summary, c("dimension", "comparison_pair_id", "metric"), "Anchor summary")
  scales <- recovery_read_csv(paths[["scales"]])
  recovery_require(scales, c("comparison_lattice", "metric", "metric_geometry",
    "standardizer", "scale_anchor_config"), "Frozen scales")
  ms_assert_unique(scales, c("comparison_lattice", "metric", "metric_geometry"), "Frozen scales")
  context <- recovery_read_csv(paths[["context"]])
  recovery_require(context, c("site", "Id", "Date", recovery_predictors()), "Frozen context")
  context <- context |> mutate(Date = as.Date(Date)) |>
    select(site, Id, Date, all_of(recovery_predictors()))
  ms_assert_unique(context, c("site", "Id", "Date"), "Frozen context")
  # All-NA columns can be parsed as logical; permit those, never parse text silently.
  for (p in recovery_predictors()) {
    if (!is.numeric(context[[p]]) && !all(is.na(context[[p]]))) stop("Non-numeric context: ", p)
    context[[p]] <- as.numeric(context[[p]])
  }
  temporal <- NULL; temporal_sources <- NULL; temporal_reused <- NA
  diary_paths <- setNames(vapply(sort(unique(context$site)), raw_data_path, character(1),
    modality = "lightexposurediary"), sort(unique(context$site)))
  if (any(!file.exists(diary_paths))) problems <- c(problems,
    paste0("Missing daypart diary: ", diary_paths[!file.exists(diary_paths)]))
  if (!length(problems)) {
    cached <- recovery_ensure_dayparts(extra_paths[["temporal_context"]], weather_path,
      extra_paths[["unit_context"]], diary_paths, core, version)
    obj <- cached$object; temporal_reused <- cached$reused
    temporal <- recovery_temporal_context(obj, core, version)
    temporal_sources <- obj$sources
  }
  list(problems = problems, paths = c(paths, extra_paths, parts), upstream = upstream,
    version = version, core = core, summary = summary, scales = scales, context = context,
    temporal = temporal, temporal_sources = temporal_sources, temporal_reused = temporal_reused)
}

# Select through the same Fig.2 anchor contract, while preserving z/scale audit fields.
recovery_pairs <- function(inputs) {
  wanted <- rq1_inference_anchor_map()
  filter_anchor <- function(z) z |> filter(analysis_unit_type == "participant_day") |>
    semi_join(wanted, by = c("dimension", "comparison_pair_id"))
  pieces <- lapply(rq1_pairwise_part_paths(inputs$upstream), function(p) {
    z <- filter_anchor(readRDS(p))
    recovery_require(z, c("config_a_id", "config_b_id", "comparison_lattice", "z", "delta",
      "available", "scale_anchor_config", "rq1_analysis_version", "core_artifact_version"), p)
    ms_assert_version(z, "rq1_analysis_version", inputs$version, p)
    ms_assert_version(z, "core_artifact_version", inputs$core, p)
    a <- rq1_inference_pairs(as.data.frame(z))
    keys <- c("dimension", "comparison_pair_id", "support_id", "site", "Id", "Date", "metric")
    z <- z |> mutate(Id = as.character(Id), Date = as.Date(Date))
    a <- left_join(a, select(z, all_of(keys), config_a_id, config_b_id,
      comparison_lattice, z, delta, available, scale_anchor_config), by = keys, relationship = "one-to-one")
    if (any(a$config_a_id != a$candidate_config | a$config_b_id != a$reference_config))
      stop("Anchor configuration orientation mismatch")
    a
  })
  x <- bind_rows(pieces)
  ms_assert_unique(x, c("candidate_config", "site", "Id", "Date", "metric"), "Recovery pairs")
  if (n_distinct(x$metric) != 52L || n_distinct(x$candidate_config) != 8L)
    stop("Expected 52 daily metrics and eight anchors, including unavailable rows")
  dual <- x$metric %in% rq1_inference_contract()$dual_channel_metrics
  expected <- ifelse(x$dimension == "placement", paste0("eye_", x$placement,
    ifelse(dual, "_full", "_medi")), ifelse(x$dimension == "optical", "eye_full",
      ifelse(dual, "eye_full", "eye_medi")))
  if (any(x$support_id != expected)) stop("Non-maximal or unexpected anchor support")
  x <- left_join(x, inputs$scales, by = c("comparison_lattice", "metric", "metric_geometry", "scale_anchor_config"),
    relationship = "many-to-one")
  if (any(is.na(x$standardizer) & x$available, na.rm = TRUE)) stop("Missing frozen scale for available pair")
  x$eligible <- is.na(x$pair_reason) & coalesce(x$available, FALSE) &
    is.finite(x$standardizer) & x$standardizer > sqrt(.Machine$double.eps)
  if (any(x$eligible & x$optical == "LIGHT" & dual)) stop("LIGHT-only dual-channel metric marked available")
  err <- recovery_delta(x$reference_value, x$candidate_value, FALSE)
  circ <- x$metric_geometry == "circular_time"
  if (any(!x$metric_geometry %in% c("linear", "circular_time"))) stop("Unknown metric geometry")
  err[circ] <- recovery_delta(x$reference_value[circ], x$candidate_value[circ], TRUE)
  ok <- x$eligible
  if (any(!is.finite(x$delta[ok])) || any(abs(err[ok] - x$delta[ok]) >
      1e-8 * (1 + abs(x$delta[ok])))) stop("Frozen delta disagrees with paired-value geometry")
  if (any(!is.finite(x$z[ok])) || any(abs(err[ok] / x$standardizer[ok] - x$z[ok]) >
      1e-8 * (1 + abs(x$z[ok])))) stop("Frozen scale/orientation does not reproduce RQ1 z")
  x$participant_key <- paste(x$site, x$Id, sep = "::")
  x <- left_join(x, mutate(inputs$context, context_row_present = TRUE),
    by = c("site", "Id", "Date"), relationship = "many-to-one")
  x$context_row_present <- coalesce(x$context_row_present, FALSE)
  x
}

# Imputation, missingness indicators, variance screening and scaling use training only.
# The only measurement predictors are this task's Y_L (or its sin/cos).
recovery_design <- function(tr, te, context, circular, screen = TRUE) {
  allowed <- lapply(c("calibration", "signature", "context"), recovery_layer_predictors)
  if (!any(vapply(allowed, identical, logical(1), context)))
    stop("Predictors must match one of the three prespecified information layers")
  basis <- function(d) {
    if (circular) data.frame(low_sin = sin(d$candidate_value * 2*pi/86400),
      low_cos = cos(d$candidate_value * 2*pi/86400)) else data.frame(low = d$candidate_value)
  }
  a <- basis(tr); b <- basis(te); audit <- list()
  for (p in context) {
    good <- is.finite(tr[[p]])
    med <- if (any(good)) median(tr[[p]][good]) else 0
    a[[p]] <- ifelse(good, tr[[p]], med)
    b[[p]] <- ifelse(is.finite(te[[p]]), te[[p]], med)
    a[[paste0(p, "_missing")]] <- as.numeric(!good)
    b[[paste0(p, "_missing")]] <- as.numeric(!is.finite(te[[p]]))
    audit[[p]] <- tibble(predictor = p, train_observed = sum(good),
      test_observed = sum(is.finite(te[[p]])), train_median = med)
  }
  scaled <- rq2_model_helpers()$scale_train_test(a, b, names(a))
  if (!screen) {
    # Preserve the full prespecified XGBoost schema, including constant/missing fields.
    for (p in setdiff(names(a), scaled$keep)) {
      scaled$tr[[p]] <- 0; scaled$te[[p]] <- 0
    }
    scaled$keep <- names(a)
  }
  aa <- cbind(intercept = 1, as.matrix(scaled$tr[, scaled$keep, drop = FALSE]))
  bb <- cbind(intercept = 1, as.matrix(scaled$te[, scaled$keep, drop = FALSE]))
  list(tr = aa, te = bb, kept = scaled$keep, audit = bind_rows(audit),
    # Store all fitted preprocessing parameters for reproducible deployment audit.
    centers = vapply(a[scaled$keep], mean, numeric(1)),
    scales = vapply(a[scaled$keep], sd, numeric(1)))
}
recovery_target <- function(tr, circular) {
  delta <- recovery_delta(tr$reference_value, tr$candidate_value, circular)
  # Zero correction maps to (0,0), so regularization is anchored at identity.
  if (circular) cbind(sin_delta = sin(delta * 2*pi/86400),
    cos_delta_minus_one = cos(delta * 2*pi/86400) - 1) else cbind(delta = delta)
}
recovery_inner <- function(tr, seed) {
  people <- sort(unique(tr$participant_key))
  if (length(people) < 3L) stop("Insufficient participants for inner validation")
  set.seed(seed, kind = "Mersenne-Twister", normal.kind = "Inversion", sample.kind = "Rejection")
  validation <- sample(people, min(length(people) - 2L, max(1L, ceiling(.2 * length(people)))))
  data.frame(participant_key = people, inner_validation = people %in% validation)
}
recovery_xgb_config <- function() list(nrounds = 500L, early_stopping_rounds = 30L,
  params = list(objective = "reg:squarederror", eval_metric = "rmse", booster = "gbtree",
    tree_method = "hist", max_depth = 3L, eta = .05, min_child_weight = 5,
    subsample = 1, colsample_bytree = 1, lambda = 1, alpha = 0, base_score = 0, nthread = 1L))

# This boundary receives inner fit/validation matrices and UNLABELLED outer test X.
# Round selection is derived from the inner evaluation log (avoids version-specific
# best_iteration indexing). Refit on all outer-training rows for exactly those rounds.
recovery_boost <- function(inner, inner_y, full, full_y, config, seed) {
  if (!requireNamespace("xgboost", quietly = TRUE)) stop("Missing required R package: xgboost; no automatic installation")
  params <- config$params; params$seed <- as.integer(seed)
  make <- function(x, y = NULL) xgboost::xgb.DMatrix(data = x, label = y, nthread = 1L)
  # R APIs before 3.x call this argument watchlist; later APIs call it evals.
  eval_arg <- if ("evals" %in% names(formals(xgboost::xgb.train))) "evals" else "watchlist"
  args <- list(params = params, data = make(inner$tr, inner_y$tr), nrounds = config$nrounds,
    early_stopping_rounds = config$early_stopping_rounds, maximize = FALSE, verbose = 0)
  args[[eval_arg]] <- list(validation = make(inner$te, inner_y$te))
  tuned <- do.call(xgboost::xgb.train, args)
  log <- attr(tuned, "evaluation_log") # 3.x stores R-only metadata as attributes.
  if (is.null(log)) log <- tuned$evaluation_log # 1.x/2.x list representation.
  log <- as.data.frame(log)
  if (!"validation_rmse" %in% names(log) || !any(is.finite(log$validation_rmse)))
    stop("XGBoost lacks finite inner validation RMSE")
  scores <- log$validation_rmse; scores[!is.finite(scores)] <- Inf
  rounds <- which.min(scores)
  model <- xgboost::xgb.train(params = params, data = make(full$tr, full_y), nrounds = rounds, verbose = 0)
  list(prediction = as.numeric(predict(model, make(full$te))),
    model = list(booster_raw = xgboost::xgb.save.raw(model), selected_rounds = rounds,
      evaluation_log = log, params = params))
}
recovery_predict <- function(tr, te, context, circular, lambda, learner = "ridge",
                             seed = 20260912L, config = recovery_xgb_config()) {
  if (!learner %in% c("ridge", "xgboost")) stop("Unknown recovery learner")
  d <- recovery_design(tr, te, context, circular, screen = learner == "ridge")
  y <- recovery_target(tr, circular)
  details <- list()
  if (learner == "ridge") {
    penalty <- diag(ncol(d$tr)); penalty[1, 1] <- 0
    beta <- solve(crossprod(d$tr) / nrow(tr) + lambda * penalty, crossprod(d$tr, y) / nrow(tr))
    pr <- d$te %*% beta
    details$beta <- beta
  } else {
    split <- recovery_inner(tr, seed)
    v <- tr$participant_key %in% split$participant_key[split$inner_validation]
    inner <- recovery_design(tr[!v, ], tr[v, ], context, circular, screen = FALSE)
    # The intercept column is constant; XGBoost uses explicit zero base_score.
    trim <- function(design) list(tr = design$tr[, -1, drop = FALSE], te = design$te[, -1, drop = FALSE])
    fits <- lapply(seq_len(ncol(y)), function(j) recovery_boost(trim(inner),
      list(tr = y[!v, j], te = y[v, j]), trim(d), y[, j], config, seed))
    pr <- do.call(cbind, lapply(fits, `[[`, "prediction"))
    details <- list(target_terms = colnames(y), boosters = lapply(fits, `[[`, "model"),
      inner_participants = split, inner_centers = inner$centers, inner_scales = inner$scales,
      inner_imputation = inner$audit, inner_seed = seed, config = config)
  }
  fallback <- rep(FALSE, nrow(te))
  if (circular) {
    pr[, 2] <- pr[, 2] + 1
    fallback <- sqrt(rowSums(pr^2)) < 1e-10
    correction <- recovery_delta(atan2(pr[, 1], pr[, 2]) * 86400/(2*pi), 0, TRUE)
    correction[fallback] <- 0
    pr <- (te$candidate_value + correction) %% 86400
  } else { correction <- as.numeric(pr); pr <- te$candidate_value + correction }
  if (any(!is.finite(pr))) stop("Non-finite held-out predictions")
  list(prediction = pr, correction = correction, fallback = fallback,
    model = c(list(learner = learner, target = "identity_anchored_residual_correction",
      columns = colnames(d$tr), centers = d$centers,
      scales = d$scales, context_imputation = d$audit), details))
}
recovery_fit <- function(task) {
  x <- readRDS(task$input)
  x <- x[x$eligible, , drop = FALSE]
  if (nrow(x) < 20L || n_distinct(x$participant_key) < 4L)
    return(list(status = "unavailable_support", complete = TRUE, predictions = tibble(), models = list()))
  circular <- identical(unique(x$metric_geometry), "circular_time")
  for (state in recovery_states()[-1]) {
    x[[state]] <- NA_real_; x[[paste0(state, "_fallback")]] <- FALSE
  }
  models <- list()
  for (f in sort(unique(x$fold))) {
    ti <- which(x$fold != f); vi <- which(x$fold == f)
    tr <- x[ti, , drop = FALSE]; te <- x[vi, , drop = FALSE]
    if (nrow(tr) < 15L || n_distinct(tr$participant_key) < 3L)
      return(list(status = "unavailable_training_support", complete = TRUE, predictions = tibble(), models = list()))
    stopifnot(!any(tr$participant_key %in% te$participant_key))
    for (state in recovery_states()[-1]) {
      fit <- recovery_predict(tr, te, recovery_layer_predictors(state),
        circular, task$lambda, learner = task$learner, seed = task$seed + f, config = task$xgb_config)
      x[[state]][vi] <- fit$prediction
      x[[paste0(state, "_fallback")]][vi] <- fit$fallback
      models[[paste(f, state, sep = "_")]] <- c(list(fold = f, state = state,
        n_train = nrow(tr), n_test = nrow(te), n_train_participants = n_distinct(tr$participant_key)), fit$model)
    }
  }
  x$raw <- x$candidate_value
  pred <- bind_rows(lapply(recovery_states(), function(state) {
    fallback <- if (state == "raw") rep(FALSE, nrow(x)) else x[[paste0(state, "_fallback")]]
    x |> transmute(site, Id, Date, support_id, participant_key, fold, context_row_present,
      state = state, learner = task$learner, Y_L = candidate_value, Y_H = reference_value,
      prediction = .data[[state]], error = recovery_delta(.data[[state]], reference_value, circular),
      correction = recovery_delta(prediction, candidate_value, circular),
      standardized_error = error / standardizer,
      circular_fallback = .env$fallback)
  }))
  list(status = "complete", complete = TRUE, predictions = pred, models = models)
}
recovery_task <- function(task) {
  old <- if (file.exists(task$output)) tryCatch(readRDS(task$output), error = function(e) NULL) else NULL
  if (!is.null(old) && identical(old$run_id, task$run_id) && isTRUE(old$complete))
    return(list(index = task$index, path = task$output, status = old$status, reused = TRUE))
  started <- Sys.time()
  result <- tryCatch(recovery_fit(task), error = function(e)
    list(status = "failed", complete = FALSE, error = conditionMessage(e), predictions = tibble(), models = list()))
  result$run_id <- task$run_id; result$meta <- task$meta
  result$elapsed_seconds <- as.numeric(difftime(Sys.time(), started, units = "secs"))
  recovery_atomic(result, task$output)
  list(index = task$index, path = task$output, status = result$status, reused = FALSE)
}

recovery_worker_exports <- function() c("recovery_task", "recovery_fit", "recovery_predict",
  "recovery_design", "recovery_delta", "recovery_predictors", "recovery_atomic", "rq2_model_helpers",
  "recovery_target", "recovery_inner", "recovery_boost", "recovery_xgb_config",
  "recovery_layer_predictors", "recovery_signature_predictors", "recovery_temporal_predictors",
  "recovery_states", "recovery_dayparts", "rq2_context_external_predictors",
  "rq2_context_micro_predictors", "rq2_context_behaviour_predictors")

# Bounded real-data end-to-end smoke: one well-supported linear and circular
# task, both learners, all outer folds and information layers, unchanged settings.
# It writes only temporary task/checkpoint files; no second permanent artifact.
recovery_smoke <- function(inputs) {
  started <- Sys.time()
  cache_path <- inputs$paths[["temporal_context"]]
  cache_md5 <- tools::md5sum(cache_path)
  x <- recovery_pairs(inputs) # validates the complete eight-anchor input domain
  seed <- as.integer(Sys.getenv("RQ2_RECOVERY_SEED", "20260912"))
  folds <- as.integer(Sys.getenv("RQ2_RECOVERY_FOLDS", "5"))
  if (!is.finite(seed) || !is.finite(folds) || folds < 2L) stop("Invalid smoke seed/folds")
  pm <- x |> distinct(site, participant_key) |> arrange(site, participant_key)
  set.seed(seed, kind = "Mersenne-Twister", normal.kind = "Inversion", sample.kind = "Rejection")
  pm <- pm |> group_by(site) |> mutate(fold = sample(rep(seq_len(folds), length.out = n()))) |> ungroup()
  x <- left_join(x, pm, by = c("site", "participant_key"), relationship = "many-to-one")
  keys <- c("dimension", "comparison_pair_id", "candidate_config", "support_id", "metric", "metric_geometry")
  meta <- x |> filter(eligible) |> group_by(across(all_of(keys))) |>
    summarise(n_units = n(), n_participants = n_distinct(participant_key), .groups = "drop") |>
    filter(n_units >= 20L, n_participants >= 6L) |> arrange(metric, comparison_pair_id) |>
    group_by(metric_geometry) |> slice_max(n_units, n = 1L, with_ties = FALSE) |> ungroup()
  if (!setequal(meta$metric_geometry, c("linear", "circular_time"))) stop("Smoke needs both metric geometries")
  x <- semi_join(x, meta, by = keys)
  signature <- recovery_signature(recovery_read_csv(inputs$paths[["unit_context"]]), inputs$core, requested = x)
  x <- recovery_join_information(x, signature, inputs$temporal)
  root <- tempfile("recovery_real_smoke_"); dir.create(root)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  tasks <- list()
  for (i in seq_len(nrow(meta))) {
    g <- semi_join(x, meta[i, ], by = keys)
    frozen <- inputs$summary |> semi_join(meta[i, ], by = c("dimension", "comparison_pair_id", "metric"))
    if (nrow(frozen) != 1L || abs(mean(abs(g$z[g$eligible])) - frozen$A_mean_absolute) > 1e-7)
      stop("Smoke raw A disagrees with frozen summary")
    ip <- file.path(root, paste0("input_", i, ".rds")); saveRDS(g, ip)
    for (learner in c("xgboost", "ridge")) {
      k <- length(tasks) + 1L
      tasks[[k]] <- list(index = k, input = ip, output = file.path(root, paste0("task_", k, ".rds")),
        learner = learner, seed = seed + i, xgb_config = recovery_xgb_config(), run_id = "real_smoke",
        lambda = as.numeric(Sys.getenv("RQ2_RECOVERY_LAMBDA", "0.01")), meta = meta[i, ])
    }
  }
  workers <- ms_resolve_workers("RQ2_RECOVERY_SMOKE_WORKERS", default = 2L, cap = 4L)
  ms_worker_init()
  refs <- ms_parallel_map(tasks, recovery_task, workers = workers, seed = seed,
    packages = c("dplyr", "tibble"), exports = recovery_worker_exports())
  report <- bind_rows(lapply(refs, function(r) {
    obj <- readRDS(r$path)
    if (!identical(obj$status, "complete")) stop("Real smoke task failed: ", if (is.null(obj$error)) obj$status else obj$error)
    counts <- table(obj$predictions$state)
    stopifnot(length(counts) == 4L, length(unique(counts)) == 1L)
    models <- obj$models
    for (model in models) {
      allowed <- c("intercept", "low", "low_sin", "low_cos", recovery_layer_predictors(model$state),
        paste0(recovery_layer_predictors(model$state), "_missing"))
      stopifnot(all(model$columns %in% allowed))
      if (model$learner == "xgboost") {
        heldout <- unique(obj$predictions$participant_key[obj$predictions$fold == model$fold])
        stopifnot(!any(model$inner_participants$participant_key %in% heldout))
      }
    }
    stopifnot(recovery_task(tasks[[r$index]])$reused)
    tibble(learner = tasks[[r$index]]$learner, metric = obj$meta$metric,
      contrast = obj$meta$comparison_pair_id, geometry = obj$meta$metric_geometry,
      heldout_days = unname(counts["raw"]), states = length(counts), fitted_models = length(models))
  }))
  stopifnot(identical(cache_md5, tools::md5sum(cache_path)))
  print(report)
  message("PASS real smoke: ", nrow(report), " tasks; ", workers, " workers; all three layers, nested CV, XGBoost/ridge and checkpoint reuse; ",
    round(as.numeric(difftime(Sys.time(), started, units = "secs")), 1), " s")
  invisible(report)
}

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
  code <- c("scripts/12d_rq2_recovery.R", "scripts/utils/rq1_inference.R",
    "scripts/utils/rq1_inference_contract.R", "scripts/utils/rq1_pairwise_artifacts.R",
    "scripts/utils/rq2_context_features.R", "scripts/utils/rq2_model_helpers.R",
    "scripts/utils/parallel_runtime.R", "scripts/utils/analysis_design.R", "scripts/utils/artifact_validation.R",
    "scripts/12c_rq2_context_models.R", "scripts/utils/rq_context.R", "scripts/utils/melidos_io.R",
    "scripts/utils/core_context.R", "scripts/utils/core_artifacts.R", "external/LightLogR/R/normalise.R")
  provenance <- list(recovery_version = "rq2_recovery_v3_low_signature_temporal_context", rq1_analysis_version = inputs$version,
    core_artifact_version = inputs$core, analysis_design_id = ms_analysis_design_id(),
    input_md5 = tools::md5sum(inputs$paths), code_md5 = tools::md5sum(code),
    seed = seed, folds = folds, lambda = lambda, G_floor = floor, predictors = recovery_predictors(),
    signature_predictors = recovery_signature_predictors(), temporal_predictors = recovery_temporal_predictors(),
    temporal_source_manifest = inputs$temporal_sources, dayparts = recovery_dayparts(),
    learners = c(primary = "xgboost", sensitivity = "ridge"), xgb_config = recovery_xgb_config(),
    inner_validation = "20% of outer-training participants; shared split/protocol across predictor states; full-training refit",
    R = R.version.string, packages = sapply(c("dplyr", "tibble", "readr", "data.table", "xgboost"),
      function(p) as.character(utils::packageVersion(p))),
    context_provenance_limit = "Legacy frozen context CSV has no upstream version; content fingerprint and producer code audited; no regeneration",
    scale_role = "Frozen RQ1 SD used ONLY for scoring; never for fitting/tuning",
    model = "Identity-anchored residual correction; primary XGBoost with nested early stopping; ridge sensitivity; no target-state predictors")
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
  # One global site-stratified participant map shared by all metrics/contrasts/states.
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
    tibble(predictor = recovery_temporal_predictors(), family = rep(c("external_daypart", "microenvironment_daypart",
      "behaviour_daypart"), each = 4L))), file.path(out, "predictor_allowlist.csv"))
  input_paths <- vapply(seq_along(groups), function(i) {
    ip <- file.path(out, "inputs", sprintf("task_%04d.rds", i))
    recovery_atomic(groups[[i]], ip)
    ip
  }, character(1))
  # Checkpoint each learner independently so an XGBoost failure cannot discard ridge fits.
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
      state_rows[[r$index]] <- bind_cols(catalog[rep(r$index, 4L), ], tibble(
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

recovery_summaries <- function(states, out, floor) {
  raw <- states |> filter(state == "raw") |> select(task_index, A_raw = A)
  cal <- states |> filter(state == "calibration") |> select(task_index, A_calibration = A)
  sig <- states |> filter(state == "signature") |> select(task_index, A_signature = A)
  comparison <- states |> filter(state != "raw") |>
    left_join(raw, by = "task_index") |> left_join(cal, by = "task_index") |> left_join(sig, by = "task_index") |>
    mutate(delta_A = A_raw - A, G = if_else(A_raw > floor, 1 - A / A_raw, NA_real_),
      G_denominator_small = A_raw <= floor,
      signature_increment = if_else(state == "signature", A_calibration - A, NA_real_),
      context_increment = if_else(state == "context", A_signature - A, NA_real_),
      context_vs_calibration = if_else(state == "context", A_calibration - A, NA_real_))
  decomposition <- comparison |> filter(state == "context") |>
    transmute(task_index, learner, comparison_pair_id, metric, raw_loss = A_raw,
      calibration_gain = A_raw - A_calibration, signature_gain = A_calibration - A_signature,
      self_recoverable_loss = A_raw - A_signature, context_recoverable_loss = A_signature - A,
      unrecovered_residual = A,
      reconstruction_error = A_raw - ((A_raw - A_signature) + (A_signature - A) + A))
  readr::write_csv(decomposition, file.path(out, "loss_decomposition.csv"))
  # Descriptive raw-magnitude adjustment, not evidence of statistical independence.
  comparison <- comparison |> group_by(learner, comparison_pair_id, state) |> group_modify(function(d, key) {
    d$delta_A_raw_adjusted <- NA_real_
    if (nrow(d) >= 8L && n_distinct(d$A_raw) >= 4L) {
      fit <- lm(delta_A ~ log1p(A_raw) + I(log1p(A_raw)^2), data = d)
      d$delta_A_raw_adjusted <- residuals(fit)
    }
    d$raw_magnitude_bin <- dplyr::ntile(d$A_raw, min(4L, nrow(d)))
    d
  }) |> ungroup()
  readr::write_csv(comparison, file.path(out, "recovery_comparison.csv"))
  overview <- comparison |> group_by(learner, comparison_pair_id, state) |> summarise(n_metrics = n(),
    fraction_improved = mean(delta_A > 0), median_delta_A = median(delta_A),
    median_G = if (any(is.finite(G))) median(G[is.finite(G)]) else NA_real_,
    fraction_context_better_than_signature = if (first(state) == "context") mean(context_increment > 0) else NA_real_,
    fraction_signature_better_than_calibration = if (first(state) == "signature") mean(signature_increment > 0) else NA_real_,
    median_context_increment = if (first(state) == "context") median(context_increment) else NA_real_,
    raw_recovered_spearman = if (sd(A_raw) > 0 && sd(A) > 0) cor(A_raw, A, method = "spearman") else NA_real_,
    adjusted_delta_IQR = if (any(is.finite(delta_A_raw_adjusted))) IQR(delta_A_raw_adjusted, na.rm = TRUE) else NA_real_,
    .groups = "drop")
  readr::write_csv(overview, file.path(out, "recovery_overview.csv"))
  bins <- comparison |> group_by(learner, comparison_pair_id, state, raw_magnitude_bin) |>
    summarise(n_metrics = n(), raw_min = min(A_raw), raw_max = max(A_raw),
      delta_median = median(delta_A), delta_q25 = quantile(delta_A, .25), delta_q75 = quantile(delta_A, .75),
      .groups = "drop")
  readr::write_csv(bins, file.path(out, "raw_magnitude_structure.csv"))
}

# Post-hoc information-ablation summaries. These functions NEVER refit recovery
# models; they only reorganize already-held-out errors from recovery_comparison.csv.
ablation_require <- function(x, columns, label) {
  missing <- setdiff(columns, names(x))
  if (length(missing)) stop(label, " missing columns: ", paste(missing, collapse = ", "), call. = FALSE)
  invisible(x)
}
ablation_safe_mean <- function(x) {
  x <- as.numeric(x); x <- x[is.finite(x)]
  if (length(x)) mean(x) else NA_real_
}
ablation_safe_median <- function(x) {
  x <- as.numeric(x); x <- x[is.finite(x)]
  if (length(x)) median(x) else NA_real_
}
ablation_safe_quantile <- function(x, p) {
  x <- as.numeric(x); x <- x[is.finite(x)]
  if (length(x)) unname(quantile(x, p, names = FALSE, type = 8)) else NA_real_
}
ablation_fraction_positive <- function(x) {
  x <- as.numeric(x); x <- x[is.finite(x)]
  if (length(x)) mean(x > 0) else NA_real_
}
ablation_resolve_run <- function(run_dir = NULL) {
  if (is.null(run_dir) || !nzchar(run_dir)) run_dir <- Sys.getenv("RQ2_RECOVERY_RUN_DIR", "")
  if (nzchar(run_dir)) {
    run_dir <- normalizePath(run_dir, winslash = "/", mustWork = TRUE)
    if (!file.exists(file.path(run_dir, "recovery_comparison.csv")))
      stop("Recovery run lacks recovery_comparison.csv: ", run_dir, call. = FALSE)
    return(run_dir)
  }
  manifests <- Sys.glob(file.path("results", "rq2", "recovery", "*", "*", "recovery_manifest.rds"))
  if (!length(manifests)) stop("No completed recovery run found; set RQ2_RECOVERY_RUN_DIR", call. = FALSE)
  complete <- vapply(manifests, function(path) tryCatch(isTRUE(readRDS(path)$complete), error = function(e) FALSE), logical(1))
  manifests <- manifests[complete]
  if (!length(manifests)) stop("No complete recovery manifest found; set RQ2_RECOVERY_RUN_DIR", call. = FALSE)
  info <- file.info(manifests)
  dirname(manifests[[which.max(info$mtime)]])
}
ablation_metric_decomposition <- function(comparison) {
  ablation_require(comparison,
    c("learner", "task_index", "dimension", "comparison_pair_id", "candidate_config",
      "support_id", "metric", "metric_class", "metric_geometry", "state", "A",
      "A_raw", "A_calibration", "A_signature"), "recovery_comparison.csv")
  out <- comparison |> filter(state == "context") |>
    transmute(learner, task_index, dimension, comparison_pair_id, candidate_config, support_id,
      metric, metric_class, metric_geometry,
      A_raw = as.numeric(A_raw), A_calibration = as.numeric(A_calibration),
      A_signature = as.numeric(A_signature), A_context = as.numeric(A),
      calibration_increment = A_raw - A_calibration,
      signature_increment = A_calibration - A_signature,
      context_increment = A_signature - A_context,
      total_increment = A_raw - A_context, unrecovered_residual = A_context)
  if (!nrow(out)) stop("No context-state rows in recovery_comparison.csv", call. = FALSE)
  if (any(!is.finite(out$A_raw) | !is.finite(out$A_calibration) |
          !is.finite(out$A_signature) | !is.finite(out$A_context)))
    stop("Non-finite stage A in completed recovery comparison", call. = FALSE)
  key <- c("learner", "task_index")
  if (nrow(out) != nrow(distinct(out, across(all_of(key)))))
    stop("Context rows are not unique by learner/task_index", call. = FALSE)
  reconstruction <- with(out,
    A_raw - (calibration_increment + signature_increment + context_increment + unrecovered_residual))
  if (max(abs(reconstruction)) > 1e-10) stop("Stage decomposition does not reconstruct raw A", call. = FALSE)
  out
}
ablation_dominant_layer <- function(calibration, signature, context) {
  value <- c(calibration = calibration, signature = signature, context = context)
  value[!is.finite(value)] <- -Inf
  if (!length(value) || max(value) <= 0) return("none")
  names(which.max(value))
}
ablation_group_summary <- function(metric_level, groups) {
  metric_level |> group_by(across(all_of(groups))) |>
    summarise(n_tasks = n(), n_unique_metrics = n_distinct(metric),
      mean_A_raw = ablation_safe_mean(A_raw), mean_A_calibration = ablation_safe_mean(A_calibration),
      mean_A_signature = ablation_safe_mean(A_signature), mean_A_context = ablation_safe_mean(A_context),
      median_A_raw = ablation_safe_median(A_raw), median_A_calibration = ablation_safe_median(A_calibration),
      median_A_signature = ablation_safe_median(A_signature), median_A_context = ablation_safe_median(A_context),
      mean_calibration_increment = ablation_safe_mean(calibration_increment),
      mean_signature_increment = ablation_safe_mean(signature_increment),
      mean_context_increment = ablation_safe_mean(context_increment),
      mean_total_increment = ablation_safe_mean(total_increment),
      median_calibration_increment = ablation_safe_median(calibration_increment),
      median_signature_increment = ablation_safe_median(signature_increment),
      median_context_increment = ablation_safe_median(context_increment),
      median_total_increment = ablation_safe_median(total_increment),
      q25_calibration_increment = ablation_safe_quantile(calibration_increment, .25),
      q75_calibration_increment = ablation_safe_quantile(calibration_increment, .75),
      q25_signature_increment = ablation_safe_quantile(signature_increment, .25),
      q75_signature_increment = ablation_safe_quantile(signature_increment, .75),
      q25_context_increment = ablation_safe_quantile(context_increment, .25),
      q75_context_increment = ablation_safe_quantile(context_increment, .75),
      fraction_calibration_improved = ablation_fraction_positive(calibration_increment),
      fraction_signature_improved = ablation_fraction_positive(signature_increment),
      fraction_context_improved = ablation_fraction_positive(context_increment),
      fraction_final_improved = ablation_fraction_positive(total_increment), .groups = "drop") |>
    rowwise() |> mutate(dominant_recovery_layer = ablation_dominant_layer(
      mean_calibration_increment, mean_signature_increment, mean_context_increment),
      mean_unrecovered_fraction = if_else(is.finite(mean_A_raw) & mean_A_raw > 0,
        mean_A_context / mean_A_raw, NA_real_)) |> ungroup()
}
ablation_atlas_long <- function(atlas) {
  bind_rows(
    atlas |> transmute(across(c(learner, dimension, comparison_pair_id, metric_class, n_tasks, n_unique_metrics)),
      stage = "self-calibration", stage_order = 1L, mean_increment = mean_calibration_increment,
      median_increment = median_calibration_increment, q25_increment = q25_calibration_increment,
      q75_increment = q75_calibration_increment, fraction_improved = fraction_calibration_improved),
    atlas |> transmute(across(c(learner, dimension, comparison_pair_id, metric_class, n_tasks, n_unique_metrics)),
      stage = "+ measurement signature", stage_order = 2L, mean_increment = mean_signature_increment,
      median_increment = median_signature_increment, q25_increment = q25_signature_increment,
      q75_increment = q75_signature_increment, fraction_improved = fraction_signature_improved),
    atlas |> transmute(across(c(learner, dimension, comparison_pair_id, metric_class, n_tasks, n_unique_metrics)),
      stage = "+ context", stage_order = 3L, mean_increment = mean_context_increment,
      median_increment = median_context_increment, q25_increment = q25_context_increment,
      q75_increment = q75_context_increment, fraction_improved = fraction_context_improved)
  ) |> arrange(learner, stage_order, dimension, comparison_pair_id, metric_class)
}
ablation_decoder_capacity <- function(atlas) {
  keys <- c("dimension", "comparison_pair_id", "metric_class")
  keep <- c(keys, "n_tasks", "n_unique_metrics", "dominant_recovery_layer",
    "mean_calibration_increment", "mean_signature_increment", "mean_context_increment",
    "fraction_calibration_improved", "fraction_signature_improved", "fraction_context_improved")
  xgb <- atlas |> filter(learner == "xgboost") |> select(all_of(keep))
  ridge <- atlas |> filter(learner == "ridge") |> select(all_of(keep))
  names(xgb)[!names(xgb) %in% keys] <- paste0("xgb_", names(xgb)[!names(xgb) %in% keys])
  names(ridge)[!names(ridge) %in% keys] <- paste0("ridge_", names(ridge)[!names(ridge) %in% keys])
  full_join(xgb, ridge, by = keys) |>
    mutate(xgb_minus_ridge_calibration_increment = xgb_mean_calibration_increment - ridge_mean_calibration_increment,
      xgb_minus_ridge_signature_increment = xgb_mean_signature_increment - ridge_mean_signature_increment,
      xgb_minus_ridge_context_increment = xgb_mean_context_increment - ridge_mean_context_increment,
      dominant_layer_concordant = xgb_dominant_recovery_layer == ridge_dominant_recovery_layer)
}
recovery_ablation_summarize <- function(run_dir = NULL) {
  run_dir <- ablation_resolve_run(run_dir)
  comparison <- readr::read_csv(file.path(run_dir, "recovery_comparison.csv"),
    show_col_types = FALSE, progress = FALSE)
  metric_level <- ablation_metric_decomposition(comparison)
  atlas <- ablation_group_summary(metric_level,
    c("learner", "dimension", "comparison_pair_id", "metric_class"))
  by_dimension <- ablation_group_summary(metric_level, c("learner", "dimension"))
  by_metric_class <- ablation_group_summary(metric_level, c("learner", "metric_class"))
  overall <- ablation_group_summary(metric_level, c("learner"))
  atlas_long <- ablation_atlas_long(atlas)
  decoder <- ablation_decoder_capacity(atlas)
  out <- file.path(run_dir, "ablation")
  dir.create(out, recursive = TRUE, showWarnings = FALSE)
  readr::write_csv(metric_level |> arrange(learner, dimension, comparison_pair_id, metric_class, metric),
    file.path(out, "recovery_metric_decomposition.csv"))
  readr::write_csv(atlas, file.path(out, "recovery_information_atlas.csv"))
  readr::write_csv(atlas_long, file.path(out, "recovery_information_atlas_long.csv"))
  readr::write_csv(by_dimension, file.path(out, "recovery_ablation_by_dimension.csv"))
  readr::write_csv(by_metric_class, file.path(out, "recovery_ablation_by_metric_class.csv"))
  readr::write_csv(overall, file.path(out, "recovery_ablation_overall.csv"))
  readr::write_csv(decoder, file.path(out, "recovery_decoder_capacity_atlas.csv"))
  readr::write_csv(metric_level |> filter(learner == "xgboost") |> arrange(desc(context_increment)),
    file.path(out, "context_gain_ranked.csv"))
  message("Recovery information-ablation outputs: ", out)
  message("Primary XGBoost summary by degradation dimension:")
  print(by_dimension |> filter(learner == "xgboost") |>
    select(dimension, n_tasks, mean_calibration_increment, mean_signature_increment,
      mean_context_increment, fraction_context_improved, mean_A_context))
  message("Largest XGBoost context-assisted cells (descriptive; no selection was used in fitting):")
  print(atlas |> filter(learner == "xgboost") |> arrange(desc(mean_context_increment)) |>
    select(dimension, comparison_pair_id, metric_class, n_tasks,
      mean_context_increment, fraction_context_improved) |> slice_head(n = 12L))
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
    if (command == "--check-inputs") message("Frozen paths and context schemas validated; daypart cache ready. Pair contents are checked by --smoke-test/--run.")
    else if (command == "--build-context") message("Daypart context ready; reused=", inputs$temporal_reused)
    else if (command == "--smoke-test") recovery_smoke(inputs)
    else recovery_run(inputs)
  }
}
