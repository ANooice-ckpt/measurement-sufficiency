# Canonical RQ2 recovery: conventional calibration followed by shared-prior residual learning.
# Rscript scripts/12d_rq2_recovery.R --check-inputs
# RQ2_RECOVERY_WORKERS=36 Rscript scripts/12d_rq2_recovery.R --run
suppressPackageStartupMessages({ library(dplyr); library(tibble) })
source("scripts/utils/rq1_inference.R")
source("scripts/utils/rq2_context_features.R")
source("scripts/utils/rq2_model_helpers.R")
source("scripts/utils/parallel_runtime.R")

# -----------------------------------------------------------------------------
# Information contract
# -----------------------------------------------------------------------------
recovery_predictors <- function() c(rq2_context_external_predictors(),
  rq2_context_micro_predictors(), rq2_context_behaviour_predictors())
recovery_dayparts <- function() list(morning = 6:10, midday = 11:13,
  afternoon = 14:17, evening = 18:23)
recovery_signature_predictors <- function() paste0("sl_", c("mean", "median", "q10", "q90",
  "iqr", "sd", "min", "max", names(recovery_dayparts()), "hour_fraction_ge250",
  "hour_fraction_le10", "adjacent_abs_change", "lag1_correlation"))
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
recovery_temporal_families <- function() rep(unname(recovery_temporal_bases()), each = length(recovery_dayparts()))
recovery_context_predictors <- function() c(recovery_predictors(), recovery_temporal_predictors())
recovery_layer_predictors <- function(state) {
  switch(state,
    calibration = character(),
    prior_only = character(),
    signature = recovery_signature_predictors(),
    context_only = recovery_context_predictors(),
    context = c(recovery_signature_predictors(), recovery_context_predictors()),
    stop("Unknown recovery information layer"))
}
recovery_states <- function() c("raw", "calibration", "prior_only", "signature", "context_only", "context")

# Compact configuration-local signature from the observed low configuration only.
recovery_signature <- function(unit, core_version = NULL, requested = NULL) {
  hours <- sprintf("isiv_h%02d", 0:23)
  keys <- c("support_id", "site", "Id", "Date", "candidate_config")
  recovery_require(unit, c("config_id", "analysis_unit_type", "placement", "optical", "resolution_s",
    "support_id", "site", "Id", "Date", hours), "Frozen low-configuration hourly basis")
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

# -----------------------------------------------------------------------------
# Rich deployable temporal context (weather + harmonised diary only)
# -----------------------------------------------------------------------------
recovery_temporal_context <- function(obj, core_version = NULL, rq1_version = NULL) {
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
  nums <- c("radiation_mean_w_m2", fraction_fields, "weather_hours", "environment_hours", "source_hours", "activity_hours")
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

recovery_daypart_bounds <- function(date, timezone, part) {
  hours <- recovery_dayparts()[[part]]; end_hour <- max(hours) + 1L
  start <- as.POSIXct(paste(as.Date(date), sprintf("%02d:00:00", min(hours))), tz = timezone)
  end <- as.POSIXct(paste(as.Date(date) + as.integer(end_hour == 24L), sprintf("%02d:00:00", end_hour %% 24L)), tz = timezone)
  if (!is.finite(as.numeric(start)) || !is.finite(as.numeric(end)) || end <= start) stop("Invalid local daypart boundary")
  c(start = as.numeric(start), end = as.numeric(end))
}

recovery_diary_intervals <- function(diary) {
  recovery_require(diary, c("Id", "start", "end", "lightsource_primary", rq_context_activity_columns()), "Light-exposure diary")
  if (!inherits(diary$start, "POSIXt") || !inherits(diary$end, "POSIXt")) stop("Diary must retain harmonized timezone-aware timestamps")
  classified <- rq_context_prepare_diary(diary)
  if (nrow(classified) != nrow(diary) || !identical(classified$Id, as.character(diary$Id))) stop("Diary classification changed row identity")
  source <- as.character(diary$lightsource_primary); source_reported <- !is.na(source) & nzchar(source)
  activity_reported <- !is.na(classified$activity)
  work <- coalesce(as.logical(diary$act_working_indoor), FALSE) | coalesce(as.logical(diary$act_working_outdoor), FALSE)
  home <- coalesce(as.logical(diary$act_home), FALSE); vehicle <- coalesce(as.logical(diary$act_road_vehicle), FALSE)
  tibble(Id = as.character(diary$Id), start = as.numeric(diary$start), end = as.numeric(diary$end),
    outdoor = ifelse(is.na(classified$environment), NA_real_, as.numeric(classified$environment == "outdoor")),
    daylight_indoor = ifelse(source_reported, as.numeric(source == "Daylight indoors"), NA_real_),
    daylight_outdoor = ifelse(source_reported, as.numeric(source == "Daylight outdoors (including shade)"), NA_real_),
    display = ifelse(source_reported, as.numeric(source == "Emissive display light"), NA_real_),
    work = ifelse(activity_reported, as.numeric(work), NA_real_),
    home = ifelse(activity_reported, as.numeric(home), NA_real_),
    vehicle = ifelse(activity_reported, as.numeric(vehicle), NA_real_))
}

recovery_diary_part <- function(d, lower, upper) {
  d <- d[is.finite(d$start) & is.finite(d$end) & d$end > d$start & d$start < upper & d$end > lower, , drop = FALSE]
  breaks <- sort(unique(c(lower, upper, pmax(lower, d$start), pmin(upper, d$end))))
  domain_signals <- list(environment = "outdoor", source = c("daylight_indoor", "daylight_outdoor", "display"),
    activity = c("work", "home", "vehicle"))
  signal_names <- unlist(domain_signals, use.names = FALSE)
  totals <- setNames(rep(0, length(signal_names) + 1L + 2L * length(domain_signals)),
    c(signal_names, "overlap", names(domain_signals), paste0(names(domain_signals), "_conflict")))
  for (j in seq_len(length(breaks) - 1L)) {
    a <- breaks[j]; b <- breaks[j + 1L]; duration <- b - a
    active <- d[d$start < b & d$end > a, , drop = FALSE]
    if (nrow(active) > 1L) totals["overlap"] <- totals["overlap"] + duration
    for (domain in names(domain_signals)) {
      signals <- domain_signals[[domain]]; if (!nrow(active)) next
      mat <- as.matrix(active[, signals, drop = FALSE]); complete <- apply(is.finite(mat), 1L, all)
      profiles <- unique(mat[complete, , drop = FALSE])
      if (nrow(profiles) == 1L) {
        totals[domain] <- totals[domain] + duration
        totals[signals] <- totals[signals] + duration * as.numeric(profiles[1, ])
      } else if (nrow(profiles) > 1L) totals[paste0(domain, "_conflict")] <- totals[paste0(domain, "_conflict")] + duration
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

recovery_weather_part <- function(w, lower, upper) {
  lo <- findInterval(lower - 60, w$seconds) + 1L; hi <- findInterval(upper, w$seconds)
  if (lo > hi) return(c(radiation_mean_w_m2 = NA_real_, weather_hours = 0))
  ix <- seq.int(lo, hi); weight <- pmax(0, pmin(w$seconds[ix] + 60, upper) - pmax(w$seconds[ix], lower))
  ok <- is.finite(w$ssrd_w_m2[ix]) & weight > 0; duration <- sum(weight[ok])
  c(radiation_mean_w_m2 = if (duration > 0) sum(weight[ok] * w$ssrd_w_m2[ix][ok]) / duration else NA_real_, weather_hours = duration / 3600)
}

recovery_build_dayparts <- function(calendar, weather, diaries) {
  ms_assert_unique(calendar, c("site", "Id", "Date"), "Daypart calendar")
  recovery_require(weather, c("site", "time_utc", "timezone", "ssrd_w_m2"), "Frozen minute weather")
  if (!inherits(weather$time_utc, "POSIXt")) stop("Frozen weather requires parsed UTC timestamps")
  weather$seconds <- as.numeric(weather$time_utc); ms_assert_unique(weather, c("site", "seconds"), "Frozen minute weather")
  if (any(!is.finite(weather$seconds)) || any(weather$seconds %% 60 != 0) || any(is.infinite(weather$ssrd_w_m2)) || any(weather$ssrd_w_m2 < 0, na.rm = TRUE))
    stop("Invalid frozen minute weather values/grid")
  rows <- list(); audits <- list(); k <- 0L
  for (site_name in sort(unique(calendar$site))) {
    cal <- calendar[calendar$site == site_name, ]; w <- weather[weather$site == site_name, ] |> arrange(seconds)
    if (!nrow(w) || !setequal(unique(cal$timezone), unique(w$timezone))) stop("Missing weather site or timezone mismatch: ", site_name)
    d <- recovery_diary_intervals(diaries[[site_name]])
    invalid <- !is.finite(d$start) | !is.finite(d$end) | d$end <= d$start | is.na(d$Id)
    audits[[site_name]] <- tibble(site = site_name, diary_rows = nrow(d), invalid_intervals = sum(invalid))
    d <- d[!invalid, ]; person <- split(d, d$Id); weather_days <- new.env(parent = emptyenv())
    for (i in seq_len(nrow(cal))) {
      dd <- person[[cal$Id[i]]]; if (is.null(dd)) dd <- d[FALSE, ]
      for (part in names(recovery_dayparts())) {
        bounds <- recovery_daypart_bounds(cal$Date[i], cal$timezone[i], part); key <- paste(cal$Date[i], part)
        if (!exists(key, weather_days, inherits = FALSE)) assign(key, recovery_weather_part(w, bounds[1], bounds[2]), weather_days)
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
  funcs <- c("recovery_dayparts", "recovery_temporal_bases", "recovery_daypart_bounds", "recovery_diary_intervals",
    "recovery_diary_part", "recovery_weather_part", "recovery_build_dayparts", "recovery_temporal_context",
    "rq_context_prepare_diary", "rq_context_activity_columns", "load_raw_file")
  list(builder = "rich_daypart_overlap", core_artifact_version = core_version, rq1_analysis_version = rq1_version,
    daypart_definition = recovery_dayparts(), source_md5 = tools::md5sum(paths), diary_sites = names(diary_paths),
    builder_hash = recovery_hash(lapply(funcs, function(nm) list(name = nm, body = body(get(nm))))),
    source_rules = "minute_left_interval_60s; diary_half_open_overlap_union; reporting domains separate; conflicts missing per domain")
}

recovery_ensure_dayparts <- function(path, weather_path, unit_path, diary_paths, core_version, rq1_version) {
  provenance <- recovery_daypart_provenance(weather_path, unit_path, diary_paths, core_version, rq1_version)
  old <- if (file.exists(path)) tryCatch(readRDS(path), error = function(e) NULL) else NULL
  if (!is.null(old) && identical(old$provenance, provenance) && identical(old$data_md5, recovery_hash(old$data))) {
    recovery_temporal_context(old, core_version, rq1_version)
    message("Daypart context: reused ", path)
    return(list(object = old, reused = TRUE))
  }
  message("Daypart context: building rich temporal representation from current weather/diary inputs")
  unit <- recovery_read_csv(unit_path)
  recovery_require(unit, c("site", "Id", "Date", "timezone", "analysis_unit_type"), "Core context calendar")
  calendar <- unit |> filter(analysis_unit_type == "participant_day") |>
    distinct(site, Id, Date, timezone) |> mutate(Id = as.character(Id), Date = as.Date(Date))
  ms_assert_unique(calendar, c("site", "Id", "Date"), "Core context calendar")
  if (anyNA(calendar$timezone) || any(!calendar$timezone %in% OlsonNames())) stop("Invalid core calendar timezone")
  if (!setequal(unique(calendar$site), names(diary_paths))) stop("Diary sites differ from current calendar")
  weather <- readr::read_csv(weather_path, col_types = readr::cols_only(core_artifact_version = readr::col_character(),
    site = readr::col_character(), time_utc = readr::col_datetime(), timezone = readr::col_character(),
    ssrd_w_m2 = readr::col_double()), progress = FALSE)
  diaries <- lapply(diary_paths, load_raw_file, modality = "lightexposurediary")
  built <- recovery_build_dayparts(calendar, weather, diaries); source_paths <- c(weather_path, unname(diary_paths))
  obj <- list(artifact_type = "recovery_deployable_dayparts", daypart_contract = "local_06_11_14_18_24",
    core_artifact_version = core_version, rq1_analysis_version = rq1_version,
    daypart_definition = recovery_dayparts(), generated_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%OS%z"),
    provenance = provenance, sources = tibble(role = c("core_weather", rep("harmonized_diary", length(diary_paths))),
      path = source_paths, md5 = unname(tools::md5sum(source_paths))), data = built$data, audit = built$audit,
    data_md5 = recovery_hash(built$data))
  recovery_temporal_context(obj, core_version, rq1_version); recovery_atomic(obj, path)
  message("Daypart context: installed ", nrow(obj$data), " rows at ", path)
  list(object = obj, reused = FALSE)
}

# -----------------------------------------------------------------------------
# Input assembly
# -----------------------------------------------------------------------------
recovery_join_information <- function(x, signature, temporal) {
  keys <- c("support_id", "site", "Id", "Date", "candidate_config")
  ms_assert_unique(signature, keys, "Recovery signature"); ms_assert_unique(temporal, c("site", "Id", "Date"), "Recovery temporal context")
  x <- left_join(x, mutate(signature, signature_row_present = TRUE), by = keys, relationship = "many-to-one")
  x <- left_join(x, mutate(temporal, temporal_row_present = TRUE), by = c("site", "Id", "Date"), relationship = "many-to-one")
  if (any(x$eligible & (is.na(x$signature_row_present) | is.na(x$temporal_row_present)))) stop("Missing signature or context on eligible support")
  x
}
recovery_delta <- function(a, b, circular) if (circular) ((a - b + 43200) %% 86400) - 43200 else a - b
recovery_require <- function(x, fields, label) {
  missing <- setdiff(fields, names(x)); if (length(missing)) stop(label, " missing fields: ", paste(missing, collapse = ", "))
}
recovery_hash <- function(x) {
  p <- tempfile(); on.exit(unlink(p)); saveRDS(x, p, version = 3); unname(tools::md5sum(p))
}
recovery_atomic <- function(x, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE); tmp <- paste0(path, ".tmp.", Sys.getpid())
  on.exit(unlink(tmp), add = TRUE); saveRDS(x, tmp, compress = "gzip")
  if (file.exists(path)) unlink(path); if (!file.rename(tmp, path)) stop("Cannot install recovery checkpoint: ", path)
}
recovery_read_csv <- function(path) {
  header <- names(readr::read_csv(path, n_max = 0L, show_col_types = FALSE, progress = FALSE))
  types <- setNames(lapply(intersect(c("site", "Id"), header), function(p) readr::col_character()), intersect(c("site", "Id"), header))
  readr::read_csv(path, col_types = do.call(readr::cols, c(types, list(.default = readr::col_guess()))), show_col_types = FALSE, progress = FALSE)
}
recovery_resolve_parts <- function(upstream, repo_root = ".") {
  if (!dir.exists(upstream$part_dir)) upstream$part_dir <- file.path(repo_root, "results", "rq1", "pairwise_parts", rq1_pairwise_version(upstream))
  parts <- rq1_pairwise_part_paths(upstream); problems <- character()
  for (p in parts) {
    if (!file.exists(p)) problems <- c(problems, paste0("Missing anchor part: ", p))
    if (!file.exists(paste0(p, ".ok"))) problems <- c(problems, paste0("Missing completion marker: ", p, ".ok"))
  }
  list(upstream = upstream, paths = parts, problems = problems)
}

recovery_inputs <- function() {
  paths <- c(pairwise = "results/rq1/rq1_pairwise_change_long.rds", summary = "results/rq1/rq1_pairwise_summary.csv",
    scales = "results/diagnostics/rq1_standardizer_audit.csv", context = "results/diagnostics/rq2_layered_context_day_features.csv")
  core_root <- Sys.getenv("RQ2_RECOVERY_CORE_ROOT", "results/core")
  extra_paths <- c(unit_context = file.path(core_root, "unit_context.csv.gz"), temporal_context = "results/rq2/recovery_inputs/context_dayparts.rds")
  weather_path <- file.path(core_root, "weather_1min.csv.gz")
  problems <- paste0("Missing input: ", paths[!file.exists(paths)]); problems <- problems[nzchar(problems) & problems != "Missing input: "]
  if (!requireNamespace("xgboost", quietly = TRUE)) problems <- c(problems, "Missing required R package: xgboost")
  missing_extra <- c(extra_paths["unit_context"], weather_path)[!file.exists(c(extra_paths["unit_context"], weather_path))]
  if (length(missing_extra)) problems <- c(problems, paste0("Missing input: ", missing_extra))
  if (any(!file.exists(paths))) return(list(problems = problems, paths = paths))
  upstream <- readRDS(paths[["pairwise"]]); version <- rq1_pairwise_version(upstream)
  core <- if (!is.null(upstream$core_artifact_version) && length(upstream$core_artifact_version))
    as.character(upstream$core_artifact_version)[1] else "current"
  if (!rq1_pairwise_is_partitioned(upstream)) stop("Expected partitioned RQ1 manifest")
  recovery_require(upstream$part_manifest, c("part", "dimension"), "RQ1 part manifest")
  records <- upstream$part_manifest |> filter(dimension == "placement_optical_temporal")
  if (!nrow(records)) stop("No declared non-duration RQ1 parts")
  upstream$parts <- as.character(records$part)
  resolved <- recovery_resolve_parts(upstream); upstream <- resolved$upstream; parts <- resolved$paths; problems <- c(problems, resolved$problems)
  summary <- recovery_read_csv(paths[["summary"]])
  recovery_require(summary, c("dimension", "comparison_pair_id", "metric", "metric_geometry", "metric_class", "A_mean_absolute", "comparison_lattice"), "RQ1 summary")
  summary <- semi_join(summary, rq1_inference_anchor_map(), by = c("dimension", "comparison_pair_id")) |>
    filter(!metric %in% c("interdaily_stability", "intradaily_variability"))
  ms_assert_unique(summary, c("dimension", "comparison_pair_id", "metric"), "Anchor summary")
  scales <- recovery_read_csv(paths[["scales"]])
  recovery_require(scales, c("comparison_lattice", "metric", "metric_geometry", "standardizer", "scale_anchor_config"), "Current scales")
  ms_assert_unique(scales, c("comparison_lattice", "metric", "metric_geometry"), "Current scales")
  context <- recovery_read_csv(paths[["context"]]); recovery_require(context, c("site", "Id", "Date", recovery_predictors()), "Current context")
  context <- context |> mutate(Date = as.Date(Date)) |> select(site, Id, Date, all_of(recovery_predictors()))
  ms_assert_unique(context, c("site", "Id", "Date"), "Current context")
  for (p in recovery_predictors()) {
    if (!is.numeric(context[[p]]) && !all(is.na(context[[p]]))) stop("Non-numeric context: ", p)
    context[[p]] <- as.numeric(context[[p]])
  }
  temporal <- NULL; temporal_sources <- NULL; temporal_reused <- NA
  sites <- sort(unique(context$site)); diary_paths <- setNames(vapply(sites, raw_data_path, character(1), modality = "lightexposurediary"), sites)
  if (any(!file.exists(diary_paths))) problems <- c(problems, paste0("Missing daypart diary: ", diary_paths[!file.exists(diary_paths)]))
  if (!length(problems)) {
    cached <- recovery_ensure_dayparts(extra_paths[["temporal_context"]], weather_path, extra_paths[["unit_context"]], diary_paths, core, version)
    temporal_reused <- cached$reused; temporal <- recovery_temporal_context(cached$object, core, version); temporal_sources <- cached$object$sources
  }
  list(problems = problems, paths = c(paths, extra_paths, parts), upstream = upstream, version = version, core = core,
    summary = summary, scales = scales, context = context, temporal = temporal, temporal_sources = temporal_sources, temporal_reused = temporal_reused)
}

recovery_pairs <- function(inputs) {
  wanted <- rq1_inference_anchor_map(); filter_anchor <- function(z) z |> filter(analysis_unit_type == "participant_day") |>
    semi_join(wanted, by = c("dimension", "comparison_pair_id"))
  pieces <- lapply(rq1_pairwise_part_paths(inputs$upstream), function(p) {
    z <- filter_anchor(readRDS(p))
    recovery_require(z, c("config_a_id", "config_b_id", "comparison_lattice", "z", "delta", "available", "scale_anchor_config"), p)
    a <- rq1_inference_pairs(as.data.frame(z)); keys <- c("dimension", "comparison_pair_id", "support_id", "site", "Id", "Date", "metric")
    z <- z |> mutate(Id = as.character(Id), Date = as.Date(Date))
    a <- left_join(a, select(z, all_of(keys), config_a_id, config_b_id, comparison_lattice, z, delta, available, scale_anchor_config),
      by = keys, relationship = "one-to-one")
    if (any(a$config_a_id != a$candidate_config | a$config_b_id != a$reference_config)) stop("Anchor configuration orientation mismatch")
    a
  })
  x <- bind_rows(pieces); ms_assert_unique(x, c("candidate_config", "site", "Id", "Date", "metric"), "Recovery pairs")
  dual <- x$metric %in% rq1_inference_contract()$dual_channel_metrics
  expected <- ifelse(x$dimension == "placement", paste0("eye_", x$placement, ifelse(dual, "_full", "_medi")),
    ifelse(x$dimension == "optical", "eye_full", ifelse(dual, "eye_full", "eye_medi")))
  if (any(x$support_id != expected)) stop("Unexpected anchor support")
  x <- left_join(x, inputs$scales, by = c("comparison_lattice", "metric", "metric_geometry", "scale_anchor_config"), relationship = "many-to-one")
  if (any(is.na(x$standardizer) & x$available, na.rm = TRUE)) stop("Missing scale for available pair")
  x$eligible <- is.na(x$pair_reason) & coalesce(x$available, FALSE) & is.finite(x$standardizer) & x$standardizer > sqrt(.Machine$double.eps)
  if (any(x$eligible & x$optical == "LIGHT" & dual)) stop("LIGHT-only dual-channel metric marked available")
  err <- recovery_delta(x$reference_value, x$candidate_value, FALSE); circ <- x$metric_geometry == "circular_time"
  if (any(!x$metric_geometry %in% c("linear", "circular_time"))) stop("Unknown metric geometry")
  err[circ] <- recovery_delta(x$reference_value[circ], x$candidate_value[circ], TRUE); ok <- x$eligible
  if (any(!is.finite(x$delta[ok])) || any(abs(err[ok] - x$delta[ok]) > 1e-8 * (1 + abs(x$delta[ok])))) stop("Delta disagrees with paired-value geometry")
  if (any(!is.finite(x$z[ok])) || any(abs(err[ok] / x$standardizer[ok] - x$z[ok]) > 1e-8 * (1 + abs(x$z[ok])))) stop("Scale/orientation does not reproduce RQ1 z")
  x$participant_key <- paste(x$site, x$Id, sep = "::")
  x <- left_join(x, mutate(inputs$context, context_row_present = TRUE), by = c("site", "Id", "Date"), relationship = "many-to-one")
  x$context_row_present <- coalesce(x$context_row_present, FALSE); x
}

# -----------------------------------------------------------------------------
# Conventional calibration + shared-prior residual learning
# -----------------------------------------------------------------------------
recovery_calibration <- function(tr, te, circular) {
  if (!circular) {
    xtr <- cbind(intercept = 1, low = as.numeric(tr$candidate_value)); xte <- cbind(intercept = 1, low = as.numeric(te$candidate_value))
    y <- as.numeric(tr$reference_value); beta <- tryCatch(qr.solve(xtr, y, tol = 1e-10), error = function(e) rep(NA_real_, 2L))
    if (any(!is.finite(beta))) beta <- c(mean(y - tr$candidate_value), 1)
    return(list(train_prediction = as.numeric(xtr %*% beta), prediction = as.numeric(xte %*% beta),
      fallback = rep(FALSE, nrow(te)), model = list(model_type = "affine_calibration", coefficients = beta, columns = colnames(xtr), target = "Y_H_from_Y_L")))
  }
  angle <- function(x) x * 2*pi/86400; xmat <- function(x) cbind(intercept = 1, low_sin = sin(angle(x)), low_cos = cos(angle(x)))
  ymat <- function(x) cbind(high_sin = sin(angle(x)), high_cos = cos(angle(x)))
  xtr <- xmat(tr$candidate_value); xte <- xmat(te$candidate_value); y <- ymat(tr$reference_value)
  beta <- tryCatch(qr.solve(xtr, y, tol = 1e-10), error = function(e) matrix(NA_real_, 6L, nrow = 3L))
  if (any(!is.finite(beta))) {
    d <- angle(recovery_delta(tr$reference_value, tr$candidate_value, TRUE)); shift <- atan2(mean(sin(d)), mean(cos(d)))
    make_pred <- function(x) (x + shift * 86400/(2*pi)) %% 86400
    return(list(train_prediction = make_pred(tr$candidate_value), prediction = make_pred(te$candidate_value), fallback = rep(FALSE, nrow(te)),
      model = list(model_type = "circular_constant_offset", offset_seconds = shift * 86400/(2*pi), target = "Y_H_from_Y_L")))
  }
  decode <- function(v, fallback_value) {
    norm <- sqrt(rowSums(v^2)); bad <- !is.finite(norm) | norm < 1e-10
    out <- atan2(v[, 1], v[, 2]) * 86400/(2*pi); out <- out %% 86400; out[bad] <- fallback_value[bad] %% 86400
    list(value = out, fallback = bad)
  }
  trd <- decode(xtr %*% beta, tr$candidate_value); ted <- decode(xte %*% beta, te$candidate_value)
  list(train_prediction = trd$value, prediction = ted$value, fallback = ted$fallback,
    model = list(model_type = "circular_affine_calibration", coefficients = beta, columns = colnames(xtr), target = "sin_cos_Y_H_from_sin_cos_Y_L"))
}

# Every residual branch receives the same calibrated prior P. Auxiliary branches
# differ only by adding S, C, or S+C. No hand-built interaction terms are supplied;
# XGBoost may learn interactions with P from the shared feature set itself.
recovery_design <- function(tr, te, predictors, circular, prior_train, prior_test, screen = TRUE) {
  allowed <- lapply(c("prior_only", "signature", "context_only", "context"), recovery_layer_predictors)
  if (!any(vapply(allowed, identical, logical(1), predictors))) stop("Residual predictors must match an information state")
  a <- data.frame(row.names = seq_len(nrow(tr))); b <- data.frame(row.names = seq_len(nrow(te))); audit <- list()
  for (p in predictors) {
    good <- is.finite(tr[[p]]); med <- if (any(good)) median(tr[[p]][good]) else 0
    a[[p]] <- ifelse(good, tr[[p]], med); b[[p]] <- ifelse(is.finite(te[[p]]), te[[p]], med)
    a[[paste0(p, "_missing")]] <- as.numeric(!good); b[[paste0(p, "_missing")]] <- as.numeric(!is.finite(te[[p]]))
    audit[[p]] <- tibble(predictor = p, train_observed = sum(good), test_observed = sum(is.finite(te[[p]])), train_median = med)
  }
  if (ncol(a)) {
    scaled <- rq2_model_helpers()$scale_train_test(a, b, names(a))
    if (!screen) {
      for (p in setdiff(names(a), scaled$keep)) { scaled$tr[[p]] <- 0; scaled$te[[p]] <- 0 }
      scaled$keep <- names(a)
    }
    atr <- as.data.frame(scaled$tr[, scaled$keep, drop = FALSE]); ate <- as.data.frame(scaled$te[, scaled$keep, drop = FALSE])
    centers <- if (length(scaled$keep)) vapply(a[scaled$keep], mean, numeric(1)) else numeric()
    scales <- if (length(scaled$keep)) vapply(a[scaled$keep], sd, numeric(1)) else numeric()
  } else {
    atr <- data.frame(row.names = seq_len(nrow(tr))); ate <- data.frame(row.names = seq_len(nrow(te)))
    centers <- numeric(); scales <- numeric()
  }
  if (circular) {
    prior_tr <- data.frame(prior_sin = sin(prior_train * 2*pi/86400), prior_cos = cos(prior_train * 2*pi/86400))
    prior_te <- data.frame(prior_sin = sin(prior_test * 2*pi/86400), prior_cos = cos(prior_test * 2*pi/86400))
    prior_meta <- list(type = "circular_calibrated_prior_sin_cos", center = NA_real_, scale = NA_real_)
  } else {
    center <- mean(prior_train); scl <- sd(prior_train)
    if (!is.finite(scl) || scl <= sqrt(.Machine$double.eps)) scl <- 1
    prior_tr <- data.frame(prior_z = (prior_train - center) / scl); prior_te <- data.frame(prior_z = (prior_test - center) / scl)
    prior_meta <- list(type = "training_standardized_calibrated_prior", center = center, scale = scl)
  }
  atr <- cbind(prior_tr, atr); ate <- cbind(prior_te, ate)
  aa <- cbind(intercept = 1, as.matrix(atr)); bb <- cbind(intercept = 1, as.matrix(ate))
  list(tr = aa, te = bb, kept = names(atr), prior_columns = names(prior_tr), prior_basis = prior_meta,
    audit = bind_rows(audit), centers = centers, scales = scales)
}

recovery_residual_target <- function(reference, baseline, circular) {
  delta <- recovery_delta(reference, baseline, circular)
  if (circular) cbind(sin_delta = sin(delta * 2*pi/86400), cos_delta_minus_one = cos(delta * 2*pi/86400) - 1) else cbind(delta = delta)
}
recovery_apply_residual <- function(baseline, pr, circular) {
  fallback <- rep(FALSE, length(baseline))
  if (circular) {
    pr[, 2] <- pr[, 2] + 1; fallback <- sqrt(rowSums(pr^2)) < 1e-10
    correction <- recovery_delta(atan2(pr[, 1], pr[, 2]) * 86400/(2*pi), 0, TRUE); correction[fallback] <- 0
    prediction <- (baseline + correction) %% 86400
  } else { correction <- as.numeric(pr); prediction <- baseline + correction }
  list(prediction = prediction, correction = correction, fallback = fallback)
}
recovery_inner <- function(tr, seed) {
  people <- sort(unique(tr$participant_key)); if (length(people) < 3L) stop("Insufficient participants for inner validation")
  set.seed(seed, kind = "Mersenne-Twister", normal.kind = "Inversion", sample.kind = "Rejection")
  validation <- sample(people, min(length(people) - 2L, max(1L, ceiling(.2 * length(people)))))
  data.frame(participant_key = people, inner_validation = people %in% validation)
}
recovery_xgb_config <- function() list(nrounds = 800L, early_stopping_rounds = 40L,
  params = list(objective = "reg:squarederror", eval_metric = "rmse", booster = "gbtree", tree_method = "hist",
    max_depth = 4L, eta = .05, min_child_weight = 3, subsample = 1, colsample_bytree = 1,
    lambda = 1, alpha = 0, base_score = 0, nthread = 1L))

recovery_boost <- function(inner, inner_y, full, full_y, config, seed) {
  if (!requireNamespace("xgboost", quietly = TRUE)) stop("Missing required R package: xgboost")
  params <- config$params; params$seed <- as.integer(seed)
  make <- function(x, y = NULL) xgboost::xgb.DMatrix(data = x, label = y, nthread = 1L)
  eval_arg <- if ("evals" %in% names(formals(xgboost::xgb.train))) "evals" else "watchlist"
  args <- list(params = params, data = make(inner$tr, inner_y$tr), nrounds = config$nrounds,
    early_stopping_rounds = config$early_stopping_rounds, maximize = FALSE, verbose = 0)
  args[[eval_arg]] <- list(validation = make(inner$te, inner_y$te)); tuned <- do.call(xgboost::xgb.train, args)
  log <- attr(tuned, "evaluation_log"); if (is.null(log)) log <- tuned$evaluation_log; log <- as.data.frame(log)
  if (!"validation_rmse" %in% names(log) || !any(is.finite(log$validation_rmse))) stop("XGBoost lacks finite inner validation RMSE")
  scores <- log$validation_rmse; scores[!is.finite(scores)] <- Inf; rounds <- which.min(scores)
  model <- xgboost::xgb.train(params = params, data = make(full$tr, full_y), nrounds = rounds, verbose = 0)
  list(prediction = as.numeric(predict(model, make(full$te))), model = list(booster_raw = xgboost::xgb.save.raw(model),
    selected_rounds = rounds, evaluation_log = log, params = params))
}

recovery_predict <- function(tr, te, predictors, circular, lambda, learner = "ridge",
                             seed = 20260912L, config = recovery_xgb_config(), calibration = NULL) {
  if (!learner %in% c("ridge", "xgboost")) stop("Unknown recovery learner")
  if (is.null(calibration)) calibration <- recovery_calibration(tr, te, circular)
  d <- recovery_design(tr, te, predictors, circular, calibration$train_prediction, calibration$prediction,
    screen = learner == "ridge")
  y <- recovery_residual_target(tr$reference_value, calibration$train_prediction, circular); details <- list()
  if (learner == "ridge") {
    penalty <- diag(ncol(d$tr)); penalty[1, 1] <- 0
    beta <- solve(crossprod(d$tr) / nrow(tr) + lambda * penalty, crossprod(d$tr, y) / nrow(tr))
    pr <- d$te %*% beta; details$beta <- beta
  } else {
    split <- recovery_inner(tr, seed); v <- tr$participant_key %in% split$participant_key[split$inner_validation]
    inner_cal <- recovery_calibration(tr[!v, ], tr[v, ], circular)
    inner <- recovery_design(tr[!v, ], tr[v, ], predictors, circular,
      inner_cal$train_prediction, inner_cal$prediction, screen = FALSE)
    inner_y <- recovery_residual_target(tr$reference_value[!v], inner_cal$train_prediction, circular)
    inner_y_val <- recovery_residual_target(tr$reference_value[v], inner_cal$prediction, circular)
    trim <- function(design) list(tr = design$tr[, -1, drop = FALSE], te = design$te[, -1, drop = FALSE])
    fits <- lapply(seq_len(ncol(y)), function(j) recovery_boost(trim(inner),
      list(tr = inner_y[, j], te = inner_y_val[, j]), trim(d), y[, j], config, seed))
    pr <- do.call(cbind, lapply(fits, `[[`, "prediction"))
    details <- list(target_terms = colnames(y), boosters = lapply(fits, `[[`, "model"), inner_participants = split,
      inner_calibration = inner_cal$model, inner_prior_basis = inner$prior_basis, inner_centers = inner$centers,
      inner_scales = inner$scales, inner_imputation = inner$audit, inner_seed = seed, config = config)
  }
  applied <- recovery_apply_residual(calibration$prediction, pr, circular)
  if (any(!is.finite(applied$prediction))) stop("Non-finite held-out predictions")
  list(prediction = applied$prediction, correction = applied$correction, fallback = applied$fallback,
    model = c(list(learner = learner, target = "residual_after_conventional_calibration_with_shared_prior",
      baseline = calibration$model, circular = circular, columns = colnames(d$tr), prior_columns = d$prior_columns,
      prior_basis = d$prior_basis, centers = d$centers, scales = d$scales, context_imputation = d$audit), details))
}

recovery_fit <- function(task) {
  x <- readRDS(task$input); x <- x[x$eligible, , drop = FALSE]
  if (nrow(x) < 20L || n_distinct(x$participant_key) < 4L) return(list(status = "unavailable_support", complete = TRUE, predictions = tibble(), models = list()))
  circular <- identical(unique(x$metric_geometry), "circular_time")
  for (state in recovery_states()[-1]) { x[[state]] <- NA_real_; x[[paste0(state, "_fallback")]] <- FALSE }
  models <- list()
  for (f in sort(unique(x$fold))) {
    ti <- which(x$fold != f); vi <- which(x$fold == f); tr <- x[ti, , drop = FALSE]; te <- x[vi, , drop = FALSE]
    if (nrow(tr) < 15L || n_distinct(tr$participant_key) < 3L) return(list(status = "unavailable_training_support", complete = TRUE, predictions = tibble(), models = list()))
    stopifnot(!any(tr$participant_key %in% te$participant_key))
    cal <- recovery_calibration(tr, te, circular); x$calibration[vi] <- cal$prediction; x$calibration_fallback[vi] <- cal$fallback
    models[[paste(f, "calibration", sep = "_")]] <- c(list(fold = f, state = "calibration", learner = "conventional",
      n_train = nrow(tr), n_test = nrow(te), n_train_participants = n_distinct(tr$participant_key)), cal$model)
    for (state in c("prior_only", "signature", "context_only", "context")) {
      fit <- recovery_predict(tr, te, recovery_layer_predictors(state), circular, task$lambda,
        learner = task$learner, seed = task$seed + f, config = task$xgb_config, calibration = cal)
      x[[state]][vi] <- fit$prediction; x[[paste0(state, "_fallback")]][vi] <- fit$fallback
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
      correction = recovery_delta(prediction, candidate_value, circular), standardized_error = error / standardizer,
      circular_fallback = .env$fallback)
  }))
  list(status = "complete", complete = TRUE, predictions = pred, models = models)
}

recovery_task <- function(task) {
  old <- if (file.exists(task$output)) tryCatch(readRDS(task$output), error = function(e) NULL) else NULL
  if (!is.null(old) && identical(old$run_id, task$run_id) && isTRUE(old$complete)) return(list(index = task$index, path = task$output, status = old$status, reused = TRUE))
  started <- Sys.time(); result <- tryCatch(recovery_fit(task), error = function(e)
    list(status = "failed", complete = FALSE, error = conditionMessage(e), predictions = tibble(), models = list()))
  result$run_id <- task$run_id; result$meta <- task$meta; result$elapsed_seconds <- as.numeric(difftime(Sys.time(), started, units = "secs"))
  recovery_atomic(result, task$output); list(index = task$index, path = task$output, status = result$status, reused = FALSE)
}

recovery_worker_exports <- function() c("recovery_task", "recovery_fit", "recovery_predict", "recovery_calibration",
  "recovery_design", "recovery_residual_target", "recovery_apply_residual", "recovery_delta",
  "recovery_predictors", "recovery_atomic", "rq2_model_helpers", "recovery_inner", "recovery_boost", "recovery_xgb_config",
  "recovery_layer_predictors", "recovery_signature_predictors", "recovery_temporal_bases", "recovery_temporal_predictors",
  "recovery_context_predictors", "recovery_states", "recovery_dayparts", "rq2_context_external_predictors",
  "rq2_context_micro_predictors", "rq2_context_behaviour_predictors")

recovery_smoke <- function(inputs) {
  started <- Sys.time(); cache_path <- inputs$paths[["temporal_context"]]; cache_md5 <- tools::md5sum(cache_path)
  x <- recovery_pairs(inputs); seed <- as.integer(Sys.getenv("RQ2_RECOVERY_SEED", "20260912")); folds <- as.integer(Sys.getenv("RQ2_RECOVERY_FOLDS", "5"))
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
  x <- semi_join(x, meta, by = keys); signature <- recovery_signature(recovery_read_csv(inputs$paths[["unit_context"]]), inputs$core, requested = x)
  x <- recovery_join_information(x, signature, inputs$temporal); root <- tempfile("recovery_real_smoke_"); dir.create(root); on.exit(unlink(root, recursive = TRUE), add = TRUE)
  tasks <- list()
  for (i in seq_len(nrow(meta))) {
    g <- semi_join(x, meta[i, ], by = keys); frozen <- inputs$summary |> semi_join(meta[i, ], by = c("dimension", "comparison_pair_id", "metric"))
    if (nrow(frozen) != 1L || abs(mean(abs(g$z[g$eligible])) - frozen$A_mean_absolute) > 1e-7) stop("Smoke raw A disagrees with current summary")
    ip <- file.path(root, paste0("input_", i, ".rds")); saveRDS(g, ip)
    for (learner in c("xgboost", "ridge")) {
      k <- length(tasks) + 1L; tasks[[k]] <- list(index = k, input = ip, output = file.path(root, paste0("task_", k, ".rds")),
        learner = learner, seed = seed + i, xgb_config = recovery_xgb_config(), run_id = "real_smoke",
        lambda = as.numeric(Sys.getenv("RQ2_RECOVERY_LAMBDA", "0.01")), meta = meta[i, ])
    }
  }
  workers <- ms_resolve_workers("RQ2_RECOVERY_SMOKE_WORKERS", default = 2L, cap = 4L); ms_worker_init()
  refs <- ms_parallel_map(tasks, recovery_task, workers = workers, seed = seed,
    packages = c("dplyr", "tibble"), exports = recovery_worker_exports())
  report <- bind_rows(lapply(refs, function(r) {
    obj <- readRDS(r$path); if (!identical(obj$status, "complete")) stop("Real smoke task failed: ", if (is.null(obj$error)) obj$status else obj$error)
    counts <- table(obj$predictions$state); stopifnot(length(counts) == length(recovery_states()), length(unique(counts)) == 1L)
    for (model in obj$models) {
      if (identical(model$state, "calibration")) {
        stopifnot(model$model_type %in% c("affine_calibration", "circular_affine_calibration", "circular_constant_offset"))
      } else {
        base <- recovery_layer_predictors(model$state)
        prior <- if (model$circular) c("prior_sin", "prior_cos") else "prior_z"
        allowed <- c("intercept", prior, base, paste0(base, "_missing"))
        stopifnot(all(model$columns %in% allowed), all(prior %in% model$columns))
        if (model$learner == "xgboost") {
          heldout <- unique(obj$predictions$participant_key[obj$predictions$fold == model$fold]); stopifnot(!any(model$inner_participants$participant_key %in% heldout))
        }
      }
    }
    stopifnot(recovery_task(tasks[[r$index]])$reused)
    tibble(learner = tasks[[r$index]]$learner, metric = obj$meta$metric, contrast = obj$meta$comparison_pair_id,
      geometry = obj$meta$metric_geometry, heldout_days = unname(counts["raw"]), states = length(counts), fitted_models = length(obj$models))
  }))
  stopifnot(identical(cache_md5, tools::md5sum(cache_path))); print(report)
  message("PASS real smoke: ", nrow(report), " tasks; conventional calibration + shared-prior residual learning; ",
    round(as.numeric(difftime(Sys.time(), started, units = "secs")), 1), " s"); invisible(report)
}

recovery_run <- function(inputs) {
  if (!requireNamespace("xgboost", quietly = TRUE)) stop("Formal recovery requires xgboost")
  integer_env <- function(name, default, minimum) {
    v <- suppressWarnings(as.integer(Sys.getenv(name, as.character(default)))); if (length(v) != 1L || !is.finite(v) || v < minimum) stop("Invalid ", name); v
  }
  seed <- integer_env("RQ2_RECOVERY_SEED", 20260912L, 1L); folds <- integer_env("RQ2_RECOVERY_FOLDS", 5L, 2L)
  lambda <- as.numeric(Sys.getenv("RQ2_RECOVERY_LAMBDA", "0.01")); floor <- as.numeric(Sys.getenv("RQ2_RECOVERY_G_FLOOR", "0.000001"))
  if (!is.finite(lambda) || lambda <= 0 || !is.finite(floor) || floor <= 0) stop("Invalid lambda/G floor")
  workers <- ms_resolve_workers("RQ2_RECOVERY_WORKERS", default = 36L, cap = 48L); ms_worker_init()
  code <- c("scripts/12d_rq2_recovery.R", "scripts/utils/rq1_inference.R", "scripts/utils/rq1_inference_contract.R",
    "scripts/utils/rq1_pairwise_artifacts.R", "scripts/utils/rq2_context_features.R", "scripts/utils/rq2_model_helpers.R",
    "scripts/utils/parallel_runtime.R", "scripts/utils/analysis_design.R", "scripts/utils/artifact_validation.R",
    "scripts/12c_rq2_context_models.R", "scripts/utils/rq_context.R", "scripts/utils/melidos_io.R",
    "scripts/utils/core_context.R", "scripts/utils/core_artifacts.R", "external/LightLogR/R/normalise.R")
  provenance <- list(recovery_version = "shared_calibrated_prior_residual_learning",
    rq1_analysis_version = inputs$version, core_artifact_version = inputs$core, analysis_design_id = ms_analysis_design_id(),
    input_md5 = tools::md5sum(inputs$paths), code_md5 = tools::md5sum(code), seed = seed, folds = folds, lambda = lambda, G_floor = floor,
    predictors = recovery_predictors(), signature_predictors = recovery_signature_predictors(), temporal_predictors = recovery_temporal_predictors(),
    fitted_states = recovery_states(), calibration = "outer-training affine Y_H~Y_L; circular metrics use affine sin/cos mapping",
    residual_estimand = "same residual learner and calibrated prior P in every flexible branch; S/C are the only added information",
    factorial_context_state = "P-only, P+S, P+C, and P+S+C; no hand-built P-by-auxiliary interactions",
    temporal_source_manifest = inputs$temporal_sources, dayparts = recovery_dayparts(),
    learners = c(primary = "xgboost", sensitivity = "ridge"), xgb_config = recovery_xgb_config(),
    inner_validation = "participant-grouped inner split; affine calibration and prior basis refit inside inner training; residual early stopping on inner-held-out participants",
    R = R.version.string, packages = sapply(c("dplyr", "tibble", "readr", "data.table", "xgboost"), function(p) as.character(utils::packageVersion(p))),
    context_provenance_limit = "Context is built only from current weather/diary/context files; no target-state predictors",
    scale_role = "RQ1 SD used only for scoring",
    model = "Conventional affine prior plus common-capacity residual learner; compare P-only with P+S, P+C, and P+S+C")
  run_id <- recovery_hash(provenance); out <- file.path("results/rq2/recovery", inputs$version, run_id); dir.create(out, recursive = TRUE, showWarnings = FALSE)
  recovery_atomic(c(provenance, list(run_id = run_id, workers = workers, started = Sys.time(), session = capture.output(sessionInfo()))), file.path(out, "provenance.rds"))
  message("Recovery: extract current non-duration anchors")
  x <- recovery_pairs(inputs); signature <- recovery_signature(recovery_read_csv(inputs$paths[["unit_context"]]), inputs$core, requested = x)
  x <- recovery_join_information(x, signature, inputs$temporal)
  readr::write_csv(signature, file.path(out, "low_signature_features.csv")); readr::write_csv(inputs$temporal, file.path(out, "temporal_context_features.csv"))
  readr::write_csv(x |> distinct(support_id, site, Id, Date, candidate_config, signature_valid_hours, signature_row_present, temporal_row_present),
    file.path(out, "information_support_audit.csv"))
  pm <- x |> distinct(site, participant_key) |> arrange(site, participant_key); if (folds > nrow(pm)) stop("More folds than participants")
  set.seed(seed, kind = "Mersenne-Twister", normal.kind = "Inversion", sample.kind = "Rejection")
  pm <- pm |> group_by(site) |> mutate(fold = sample(rep(seq_len(folds), length.out = n()))) |> ungroup()
  x <- left_join(x, pm, by = c("site", "participant_key"), relationship = "many-to-one"); readr::write_csv(pm, file.path(out, "participant_folds.csv"))
  keys <- c("dimension", "comparison_pair_id", "candidate_config", "support_id", "metric", "metric_class", "metric_geometry")
  groups <- x |> group_by(across(all_of(keys))) |> group_split(.keep = TRUE)
  catalog <- bind_rows(lapply(groups, function(g) {
    meta <- distinct(select(g, all_of(keys)))
    bind_cols(meta, tibble(n_rows = nrow(g), n_eligible = sum(g$eligible), n_context_rows = sum(g$context_row_present & g$eligible),
      n_eligible_participants = n_distinct(g$participant_key[g$eligible]), A_raw_all_eligible = if (any(g$eligible)) mean(abs(g$z[g$eligible])) else NA_real_))
  })) |> mutate(task_index = row_number()) |>
    left_join(select(inputs$summary, dimension, comparison_pair_id, metric, A_frozen_RQ1 = A_mean_absolute),
      by = c("dimension", "comparison_pair_id", "metric"), relationship = "many-to-one")
  mismatch <- with(catalog, is.finite(A_raw_all_eligible) & (!is.finite(A_frozen_RQ1) | abs(A_raw_all_eligible - A_frozen_RQ1) > 1e-7 * (1 + abs(A_frozen_RQ1))))
  if (any(mismatch)) stop("Anchor raw A does not reproduce current RQ1 summary")
  readr::write_csv(x |> filter(!eligible) |> count(dimension, comparison_pair_id, metric, support_id, pair_reason, available, name = "n_rows"),
    file.path(out, "unavailable_audit.csv"))
  readr::write_csv(bind_rows(tibble(predictor = recovery_signature_predictors(), family = "low_signature"),
    tibble(predictor = recovery_predictors(), family = c(rep("external", 8), rep("microenvironment", 4), rep("behaviour", 6))),
    tibble(predictor = recovery_temporal_predictors(), family = recovery_temporal_families())), file.path(out, "predictor_allowlist.csv"))
  input_paths <- vapply(seq_along(groups), function(i) {
    ip <- file.path(out, "inputs", sprintf("task_%04d.rds", i)); recovery_atomic(groups[[i]], ip); ip
  }, character(1))
  catalog <- bind_rows(lapply(c("xgboost", "ridge"), function(learner) catalog |> mutate(base_task_index = task_index, learner = learner))) |> mutate(task_index = row_number())
  readr::write_csv(catalog, file.path(out, "task_catalog.csv"))
  tasks <- lapply(seq_len(nrow(catalog)), function(i) list(index = i, input = input_paths[[catalog$base_task_index[i]]],
    output = file.path(out, "checkpoints", sprintf("task_%04d.rds", i)), learner = catalog$learner[i],
    seed = seed + catalog$base_task_index[i], xgb_config = recovery_xgb_config(), run_id = run_id, lambda = lambda, meta = catalog[i, ]))
  rm(x, groups); invisible(gc(FALSE))
  message("Recovery: ", length(tasks), " tasks; ", workers, " workers; affine calibration + shared-prior residual learning; ", out)
  refs <- ms_parallel_map(tasks, recovery_task, workers = workers, seed = seed, packages = c("dplyr", "tibble"), exports = recovery_worker_exports())
  state_rows <- list(); statuses <- list(); fold_rows <- list()
  for (r in refs) {
    obj <- readRDS(r$path)
    statuses[[r$index]] <- bind_cols(catalog[r$index, ], tibble(status = obj$status,
      error = if (is.null(obj$error)) NA_character_ else obj$error, reused = r$reused, checkpoint = r$path, elapsed_seconds = obj$elapsed_seconds))
    if (!nrow(obj$predictions)) {
      state_rows[[r$index]] <- bind_cols(catalog[rep(r$index, length(recovery_states())), ], tibble(
        state = recovery_states(), n_test = 0L, n_test_participants = 0L, MAE_native = NA_real_, RMSE_native = NA_real_,
        A = NA_real_, B = NA_real_, standardized_RMSE = NA_real_, n_circular_fallback = 0L, status = obj$status)); next
    }
    s <- obj$predictions |> group_by(state) |> summarise(n_test = n(), n_test_participants = n_distinct(participant_key),
      MAE_native = mean(abs(error)), RMSE_native = sqrt(mean(error^2)), A = mean(abs(standardized_error)),
      B = mean(standardized_error), standardized_RMSE = sqrt(mean(standardized_error^2)),
      n_circular_fallback = sum(circular_fallback), status = obj$status, .groups = "drop")
    stopifnot(n_distinct(s$n_test) == 1L); state_rows[[r$index]] <- bind_cols(catalog[rep(r$index, nrow(s)), ], s)
    f <- obj$predictions |> group_by(fold, state) |> summarise(n_test = n(), A = mean(abs(standardized_error)), .groups = "drop")
    fold_rows[[r$index]] <- bind_cols(catalog[rep(r$index, nrow(f)), ], f)
  }
  status <- bind_rows(statuses); states <- bind_rows(state_rows)
  readr::write_csv(status, file.path(out, "task_status.csv")); readr::write_csv(states, file.path(out, "heldout_errors.csv"));
  readr::write_csv(bind_rows(fold_rows), file.path(out, "fold_errors.csv"))
  if (any(is.finite(states$A))) recovery_summaries(filter(states, is.finite(A)), out, floor)
  recovery_atomic(list(run_id = run_id, complete = !any(status$status == "failed"), statuses = status, heldout_errors = states, provenance = provenance),
    file.path(out, "recovery_manifest.rds"))
  message("Recovery outputs: ", out); if (any(status$status == "failed")) stop("Some recovery tasks failed; successful checkpoints retained")
  invisible(out)
}

recovery_summaries <- function(states, out, floor) {
  raw <- states |> filter(state == "raw") |> select(task_index, A_raw = A)
  cal <- states |> filter(state == "calibration") |> select(task_index, A_calibration = A)
  prior <- states |> filter(state == "prior_only") |> select(task_index, A_prior_only = A)
  sig <- states |> filter(state == "signature") |> select(task_index, A_signature = A)
  ctx0 <- states |> filter(state == "context_only") |> select(task_index, A_context_only = A)
  comparison <- states |> filter(state != "raw") |>
    left_join(raw, by = "task_index") |> left_join(cal, by = "task_index") |> left_join(prior, by = "task_index") |>
    left_join(sig, by = "task_index") |> left_join(ctx0, by = "task_index") |>
    mutate(delta_A = A_raw - A, G = if_else(A_raw > floor, 1 - A / A_raw, NA_real_), G_denominator_small = A_raw <= floor,
      prior_flexible_increment = if_else(state == "prior_only", A_calibration - A, NA_real_),
      signature_increment = if_else(state == "signature", A_prior_only - A, NA_real_),
      context_total_increment = if_else(state == "context_only", A_prior_only - A, NA_real_),
      context_increment = if_else(state == "context", A_signature - A, NA_real_),
      signature_after_context_increment = if_else(state == "context", A_context_only - A, NA_real_),
      practical_context_increment = if_else(state == "context_only", A_calibration - A, NA_real_),
      practical_joint_increment = if_else(state == "context", A_calibration - A, NA_real_),
      context_overlap_or_interaction = if_else(state == "context", (A_prior_only - A_context_only) - (A_signature - A), NA_real_))
  decomposition <- comparison |> filter(state == "context") |>
    transmute(task_index, learner, comparison_pair_id, metric, raw_loss = A_raw,
      affine_calibration_loss = A_calibration, prior_only_loss = A_prior_only,
      calibration_gain = A_raw - A_calibration, flexible_prior_gain = A_calibration - A_prior_only,
      signature_gain = A_prior_only - A_signature, context_total_gain = A_prior_only - A_context_only,
      context_unique_after_signature = A_signature - A, signature_unique_after_context = A_context_only - A,
      context_shared_or_interaction = (A_prior_only - A_context_only) - (A_signature - A),
      practical_context_gain = A_calibration - A_context_only, practical_joint_gain = A_calibration - A,
      full_auxiliary_gain = A_prior_only - A,
      context_shapley_gain = .5 * ((A_prior_only - A_context_only) + (A_signature - A)),
      signature_shapley_gain = .5 * ((A_prior_only - A_signature) + (A_context_only - A)),
      context_share_of_auxiliary_gain = if_else(full_auxiliary_gain > floor, context_shapley_gain / full_auxiliary_gain, NA_real_),
      signature_share_of_auxiliary_gain = if_else(full_auxiliary_gain > floor, signature_shapley_gain / full_auxiliary_gain, NA_real_),
      unrecovered_residual = A,
      reconstruction_error_signature_first = A_raw - ((A_raw - A_calibration) + (A_calibration - A_prior_only) +
        (A_prior_only - A_signature) + (A_signature - A) + A),
      reconstruction_error_context_first = A_raw - ((A_raw - A_calibration) + (A_calibration - A_prior_only) +
        (A_prior_only - A_context_only) + (A_context_only - A) + A),
      shapley_reconstruction_error = full_auxiliary_gain - (context_shapley_gain + signature_shapley_gain))
  readr::write_csv(decomposition, file.path(out, "loss_decomposition.csv"))
  comparison <- comparison |> group_by(learner, comparison_pair_id, state) |> group_modify(function(d, key) {
    d$delta_A_raw_adjusted <- NA_real_
    if (nrow(d) >= 8L && n_distinct(d$A_raw) >= 4L) {
      fit <- lm(delta_A ~ log1p(A_raw) + I(log1p(A_raw)^2), data = d); d$delta_A_raw_adjusted <- residuals(fit)
    }
    d$raw_magnitude_bin <- dplyr::ntile(d$A_raw, min(4L, nrow(d))); d
  }) |> ungroup()
  readr::write_csv(comparison, file.path(out, "recovery_comparison.csv"))
  overview <- comparison |> group_by(learner, comparison_pair_id, state) |> summarise(n_metrics = n(),
    fraction_improved = mean(delta_A > 0), median_delta_A = median(delta_A),
    median_G = if (any(is.finite(G))) median(G[is.finite(G)]) else NA_real_,
    fraction_context_better_than_signature = if (first(state) == "context") mean(context_increment > 0) else NA_real_,
    fraction_context_only_better_than_prior = if (first(state) == "context_only") mean(context_total_increment > 0) else NA_real_,
    fraction_signature_better_than_prior = if (first(state) == "signature") mean(signature_increment > 0) else NA_real_,
    fraction_prior_better_than_affine = if (first(state) == "prior_only") mean(prior_flexible_increment > 0) else NA_real_,
    median_context_increment = if (first(state) == "context") median(context_increment) else NA_real_,
    median_context_total_increment = if (first(state) == "context_only") median(context_total_increment) else NA_real_,
    median_practical_context_increment = if (first(state) == "context_only") median(practical_context_increment) else NA_real_,
    raw_recovered_spearman = if (sd(A_raw) > 0 && sd(A) > 0) cor(A_raw, A, method = "spearman") else NA_real_,
    adjusted_delta_IQR = if (any(is.finite(delta_A_raw_adjusted))) IQR(delta_A_raw_adjusted, na.rm = TRUE) else NA_real_, .groups = "drop")
  readr::write_csv(overview, file.path(out, "recovery_overview.csv"))
  bins <- comparison |> group_by(learner, comparison_pair_id, state, raw_magnitude_bin) |>
    summarise(n_metrics = n(), raw_min = min(A_raw), raw_max = max(A_raw), delta_median = median(delta_A),
      delta_q25 = quantile(delta_A, .25), delta_q75 = quantile(delta_A, .75), .groups = "drop")
  readr::write_csv(bins, file.path(out, "raw_magnitude_structure.csv"))
}

if (sys.nframe() == 0L) {
  args <- commandArgs(trailingOnly = TRUE); command <- if (length(args)) args[[1]] else ""
  allowed <- c("--check-inputs", "--build-context", "--smoke-test", "--run")
  if (!command %in% allowed || length(args) != 1L) stop("Use --check-inputs, --build-context, --smoke-test, or --run")
  inputs <- recovery_inputs(); if (length(inputs$problems)) stop(paste(c(inputs$problems, "Supply the current input artifacts."), collapse = "\n"))
  if (command == "--check-inputs") message("Current paths and context schemas validated; daypart cache ready.")
  else if (command == "--build-context") message("Rich daypart context ready; reused=", inputs$temporal_reused)
  else if (command == "--smoke-test") recovery_smoke(inputs)
  else recovery_run(inputs)
}
