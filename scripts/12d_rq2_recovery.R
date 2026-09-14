# Canonical RQ2 conditional reliability. Historical recovery helpers below are
# retained for audit/reproduction; --run now uses the risk/Brier analysis.
# Rscript scripts/12d_rq2_recovery.R --check-inputs
# RQ2_RELIABILITY_WORKERS=12 Rscript scripts/12d_rq2_recovery.R --run
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
    prior_only = character(),
    signature = recovery_signature_predictors(),
    context_only = recovery_context_predictors(),
    context = c(recovery_signature_predictors(), recovery_context_predictors()),
    stop("Unknown recovery information layer"))
}
recovery_reconstruction_states <- function() c("raw", "calibration", "prior_only", "signature", "context_only", "context")
recovery_observability_states <- function() c("null", "prior_only", "signature", "context_only", "context")

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
  if (anyNA(calendar$timezone) || any(!calendar$timezone %in% OlsonNames()) ||
      !setequal(unique(calendar$site), names(diary_paths))) stop("Invalid calendar/diary site-timezone contract")
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
  missing_extra <- c(extra_paths["unit_context"], weather_path)[!file.exists(c(extra_paths["unit_context"], weather_path))]
  if (length(missing_extra)) problems <- c(problems, paste0("Missing input: ", missing_extra))
  if (any(!file.exists(paths))) return(list(problems = problems, paths = paths))
  upstream <- readRDS(paths[["pairwise"]]); version <- rq1_pairwise_version(upstream)
  core <- if (!is.null(upstream$core_artifact_version) && length(upstream$core_artifact_version)) as.character(upstream$core_artifact_version)[1] else "current"
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
# Conventional calibration + anchored information-set learning
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

# Every information state is anchored at the same conventional calibration P.
# Flexible reconstruction learns only the discrepancy from P; observability learns D=|z|.
# Within an outer fold all states and both estimands use exactly the same inner participants.
recovery_design <- function(tr, te, predictors, circular, prior_train, prior_test, screen = TRUE) {
  allowed <- lapply(c("prior_only", "signature", "context_only", "context"), recovery_layer_predictors)
  if (!any(vapply(allowed, identical, logical(1), predictors))) stop("Predictors must match an information state")
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
  list(tr = cbind(intercept = 1, as.matrix(atr)), te = cbind(intercept = 1, as.matrix(ate)),
    kept = names(atr), prior_columns = names(prior_tr), prior_basis = prior_meta,
    audit = bind_rows(audit), centers = centers, scales = scales)
}

recovery_inner <- function(tr, seed) {
  people <- sort(unique(tr$participant_key)); if (length(people) < 3L) stop("Insufficient participants for inner validation")
  set.seed(seed, kind = "Mersenne-Twister", normal.kind = "Inversion", sample.kind = "Rejection")
  validation <- sample(people, min(length(people) - 2L, max(1L, ceiling(.2 * length(people)))))
  data.frame(participant_key = people, inner_validation = people %in% validation)
}

recovery_xgb_grid <- function() list(
  shallow = list(nrounds = 600L, early_stopping_rounds = 30L,
    params = list(objective = "reg:squarederror", eval_metric = "mae", booster = "gbtree", tree_method = "hist",
      max_depth = 1L, eta = .05, min_child_weight = 8, subsample = 1, colsample_bytree = 1,
      lambda = 2, alpha = 0, nthread = 1L)),
  moderate = list(nrounds = 600L, early_stopping_rounds = 30L,
    params = list(objective = "reg:squarederror", eval_metric = "mae", booster = "gbtree", tree_method = "hist",
      max_depth = 2L, eta = .05, min_child_weight = 5, subsample = 1, colsample_bytree = 1,
      lambda = 1, alpha = 0, nthread = 1L)),
  flexible = list(nrounds = 600L, early_stopping_rounds = 30L,
    params = list(objective = "reg:squarederror", eval_metric = "mae", booster = "gbtree", tree_method = "hist",
      max_depth = 4L, eta = .04, min_child_weight = 3, subsample = 1, colsample_bytree = 1,
      lambda = 1, alpha = 0, nthread = 1L))
)
recovery_ridge_grid <- function(lambda) sort(unique(pmax(c(lambda/10, lambda, lambda*10, lambda*100), 1e-8)))

recovery_residual_target <- function(reference, baseline, circular) {
  delta <- recovery_delta(reference, baseline, circular)
  if (circular) cbind(sin_delta = sin(delta * 2*pi/86400), cos_delta_minus_one = cos(delta * 2*pi/86400) - 1) else cbind(delta = delta)
}
recovery_apply_residual <- function(baseline, pr, circular) {
  fallback <- rep(FALSE, length(baseline))
  if (circular) {
    pr[, 2] <- pr[, 2] + 1
    fallback <- !is.finite(sqrt(rowSums(pr^2))) | sqrt(rowSums(pr^2)) < 1e-10
    correction <- recovery_delta(atan2(pr[, 1], pr[, 2]) * 86400/(2*pi), 0, TRUE)
    correction[fallback] <- 0
    prediction <- (baseline + correction) %% 86400
  } else {
    correction <- as.numeric(pr[, 1]); prediction <- baseline + correction
  }
  list(prediction = prediction, correction = correction, fallback = fallback)
}
recovery_prediction_loss <- function(prediction, data, estimand, circular) {
  if (estimand == "observability") return(mean(abs(prediction - abs(data$z))))
  mean(abs(recovery_delta(data$reference_value, prediction, circular) / data$standardizer))
}
recovery_ridge_fit <- function(x, y, lambda) {
  penalty <- diag(ncol(x)); penalty[1, 1] <- 0
  lhs <- crossprod(x) / nrow(x) + lambda * penalty + diag(1e-10, ncol(x))
  tryCatch(solve(lhs, crossprod(x, y) / nrow(x)), error = function(e) qr.solve(lhs, crossprod(x, y) / nrow(x), tol = 1e-10))
}

recovery_xgb_inner_channel <- function(xtr, ytr, xval, yval, config, seed) {
  if (!requireNamespace("xgboost", quietly = TRUE)) stop("Missing required R package: xgboost")
  params <- config$params; params$seed <- as.integer(seed); params$base_score <- mean(ytr)
  make <- function(x, y = NULL) xgboost::xgb.DMatrix(data = x, label = y, nthread = 1L)
  eval_arg <- if ("evals" %in% names(formals(xgboost::xgb.train))) "evals" else "watchlist"
  args <- list(params = params, data = make(xtr, ytr), nrounds = config$nrounds,
    early_stopping_rounds = config$early_stopping_rounds, maximize = FALSE, verbose = 0)
  args[[eval_arg]] <- list(validation = make(xval, yval)); fit <- do.call(xgboost::xgb.train, args)
  log <- attr(fit, "evaluation_log"); if (is.null(log)) log <- fit$evaluation_log; log <- as.data.frame(log)
  metric <- grep("^validation_", names(log), value = TRUE)[1]
  if (is.na(metric) || !any(is.finite(log[[metric]]))) stop("XGBoost lacks finite inner validation metric")
  score <- log[[metric]]; score[!is.finite(score)] <- Inf; rounds <- which.min(score)
  list(rounds = rounds, validation_prediction = as.numeric(predict(fit, make(xval))), evaluation_log = log, params = params)
}
recovery_xgb_refit_channel <- function(xtr, ytr, xte, selected) {
  params <- selected$params; params$base_score <- mean(ytr)
  make <- function(x, y = NULL) xgboost::xgb.DMatrix(data = x, label = y, nthread = 1L)
  model <- xgboost::xgb.train(params = params, data = make(xtr, ytr), nrounds = selected$rounds, verbose = 0)
  list(prediction = as.numeric(predict(model, make(xte))), booster_raw = xgboost::xgb.save.raw(model))
}

recovery_choose_candidate <- function(candidates, min_relative_gain) {
  scores <- vapply(candidates, `[[`, numeric(1), "score")
  if (!length(scores) || !is.finite(scores[1])) stop("Invalid baseline candidate")
  best <- which.min(scores)
  required <- scores[1] * (1 - min_relative_gain)
  if (best != 1L && is.finite(scores[best]) && scores[best] < required) best else 1L
}

# One estimator for one information set. The estimand is information value; the estimator
# conservatively selects among no update, ridge, and XGBoost on the shared inner participants.
recovery_predict_information <- function(tr, te, predictors, circular, estimand, inner_split,
                                         lambda = .01, seed = 20260912L,
                                         xgb_grid = recovery_xgb_grid(), calibration = NULL,
                                         min_relative_gain = .005) {
  if (!estimand %in% c("reconstructability", "observability")) stop("Unknown estimand")
  if (is.null(calibration)) calibration <- recovery_calibration(tr, te, circular)
  if (!is.data.frame(inner_split) || !all(c("participant_key", "inner_validation") %in% names(inner_split))) stop("Invalid shared inner split")
  if (!setequal(inner_split$participant_key, sort(unique(tr$participant_key)))) stop("Inner split does not match outer-training participants")
  v <- tr$participant_key %in% inner_split$participant_key[inner_split$inner_validation]
  if (!any(v) || !any(!v)) stop("Degenerate inner split")

  inner_cal <- recovery_calibration(tr[!v, ], tr[v, ], circular)
  d_inner_ridge <- recovery_design(tr[!v, ], tr[v, ], predictors, circular,
    inner_cal$train_prediction, inner_cal$prediction, screen = TRUE)
  d_full_ridge <- recovery_design(tr, te, predictors, circular,
    calibration$train_prediction, calibration$prediction, screen = TRUE)
  d_inner_xgb <- recovery_design(tr[!v, ], tr[v, ], predictors, circular,
    inner_cal$train_prediction, inner_cal$prediction, screen = FALSE)
  d_full_xgb <- recovery_design(tr, te, predictors, circular,
    calibration$train_prediction, calibration$prediction, screen = FALSE)

  if (estimand == "reconstructability") {
    y_inner <- recovery_residual_target(tr$reference_value[!v], inner_cal$train_prediction, circular)
    y_full <- recovery_residual_target(tr$reference_value, calibration$train_prediction, circular)
    baseline_inner <- inner_cal$prediction
    baseline_outer <- calibration$prediction
    candidates <- list(list(kind = "none", name = "no_correction",
      score = recovery_prediction_loss(baseline_inner, tr[v, ], estimand, circular)))

    for (lam in recovery_ridge_grid(lambda)) {
      beta <- recovery_ridge_fit(d_inner_ridge$tr, y_inner, lam)
      pr <- d_inner_ridge$te %*% beta
      applied <- recovery_apply_residual(inner_cal$prediction, pr, circular)
      candidates[[length(candidates) + 1L]] <- list(kind = "ridge", name = paste0("ridge_", format(lam, scientific = TRUE)),
        lambda = lam, score = recovery_prediction_loss(applied$prediction, tr[v, ], estimand, circular))
    }

    trim <- function(d) list(tr = d$tr[, -1, drop = FALSE], te = d$te[, -1, drop = FALSE])
    ii <- trim(d_inner_xgb); ff <- trim(d_full_xgb)
    for (k in seq_along(xgb_grid)) {
      channels <- lapply(seq_len(ncol(y_inner)), function(j)
        recovery_xgb_inner_channel(ii$tr, y_inner[, j], ii$te, recovery_residual_target(tr$reference_value[v], inner_cal$prediction, circular)[, j],
          xgb_grid[[k]], seed + 100L*k + j))
      pr <- do.call(cbind, lapply(channels, `[[`, "validation_prediction"))
      applied <- recovery_apply_residual(inner_cal$prediction, pr, circular)
      candidates[[length(candidates) + 1L]] <- list(kind = "xgboost", name = paste0("xgb_", names(xgb_grid)[k]),
        config = names(xgb_grid)[k], channels = channels,
        score = recovery_prediction_loss(applied$prediction, tr[v, ], estimand, circular))
    }

    selected_index <- recovery_choose_candidate(candidates, min_relative_gain); selected <- candidates[[selected_index]]
    if (selected$kind == "none") {
      prediction <- baseline_outer; fallback <- calibration$fallback; fit_details <- list()
    } else if (selected$kind == "ridge") {
      beta <- recovery_ridge_fit(d_full_ridge$tr, y_full, selected$lambda)
      applied <- recovery_apply_residual(calibration$prediction, d_full_ridge$te %*% beta, circular)
      prediction <- applied$prediction; fallback <- applied$fallback; fit_details <- list(beta = beta, selected_lambda = selected$lambda)
    } else {
      refits <- lapply(seq_len(ncol(y_full)), function(j)
        recovery_xgb_refit_channel(ff$tr, y_full[, j], ff$te, selected$channels[[j]]))
      applied <- recovery_apply_residual(calibration$prediction,
        do.call(cbind, lapply(refits, `[[`, "prediction")), circular)
      prediction <- applied$prediction; fallback <- applied$fallback
      fit_details <- list(selected_config = selected$config,
        selected_rounds = vapply(selected$channels, `[[`, integer(1), "rounds"),
        evaluation_logs = lapply(selected$channels, `[[`, "evaluation_log"),
        boosters = lapply(refits, `[[`, "booster_raw"))
    }
  } else {
    y_inner <- abs(tr$z[!v]); y_full <- abs(tr$z)
    baseline_inner <- rep(median(y_inner), sum(v)); baseline_outer <- rep(median(y_full), nrow(te))
    candidates <- list(list(kind = "none", name = "median_null",
      score = recovery_prediction_loss(baseline_inner, tr[v, ], estimand, circular)))

    for (lam in recovery_ridge_grid(lambda)) {
      beta <- recovery_ridge_fit(d_inner_ridge$tr, cbind(distortion = y_inner), lam)
      pr <- pmax(0, as.numeric(d_inner_ridge$te %*% beta))
      candidates[[length(candidates) + 1L]] <- list(kind = "ridge", name = paste0("ridge_", format(lam, scientific = TRUE)),
        lambda = lam, score = recovery_prediction_loss(pr, tr[v, ], estimand, circular))
    }

    trim <- function(d) list(tr = d$tr[, -1, drop = FALSE], te = d$te[, -1, drop = FALSE])
    ii <- trim(d_inner_xgb); ff <- trim(d_full_xgb)
    for (k in seq_along(xgb_grid)) {
      channel <- recovery_xgb_inner_channel(ii$tr, y_inner, ii$te, abs(tr$z[v]), xgb_grid[[k]], seed + 100L*k + 1L)
      pr <- pmax(0, channel$validation_prediction)
      candidates[[length(candidates) + 1L]] <- list(kind = "xgboost", name = paste0("xgb_", names(xgb_grid)[k]),
        config = names(xgb_grid)[k], channels = list(channel),
        score = recovery_prediction_loss(pr, tr[v, ], estimand, circular))
    }

    selected_index <- recovery_choose_candidate(candidates, min_relative_gain); selected <- candidates[[selected_index]]
    fallback <- rep(FALSE, nrow(te))
    if (selected$kind == "none") {
      prediction <- baseline_outer; fit_details <- list()
    } else if (selected$kind == "ridge") {
      beta <- recovery_ridge_fit(d_full_ridge$tr, cbind(distortion = y_full), selected$lambda)
      prediction <- pmax(0, as.numeric(d_full_ridge$te %*% beta)); fit_details <- list(beta = beta, selected_lambda = selected$lambda)
    } else {
      refit <- recovery_xgb_refit_channel(ff$tr, y_full, ff$te, selected$channels[[1]])
      prediction <- pmax(0, refit$prediction)
      fit_details <- list(selected_config = selected$config, selected_rounds = selected$channels[[1]]$rounds,
        evaluation_logs = list(selected$channels[[1]]$evaluation_log), boosters = list(refit$booster_raw))
    }
  }

  if (any(!is.finite(prediction))) stop("Non-finite held-out prediction")
  candidate_scores <- setNames(vapply(candidates, `[[`, numeric(1), "score"), vapply(candidates, `[[`, character(1), "name"))
  list(prediction = prediction, fallback = fallback,
    model = c(list(estimator = "anchored_adaptive_library", estimand = estimand, circular = circular,
      selected_candidate = selected$name, selected_kind = selected$kind,
      inner_baseline_loss = candidates[[1]]$score, inner_selected_loss = selected$score,
      candidate_scores = candidate_scores, min_relative_gain = min_relative_gain,
      inner_participants = inner_split,
      columns_ridge = colnames(d_full_ridge$tr), columns_xgb = colnames(d_full_xgb$tr),
      prior_columns = d_full_xgb$prior_columns, prior_basis = d_full_xgb$prior_basis,
      centers = d_full_xgb$centers, scales = d_full_xgb$scales, context_imputation = d_full_xgb$audit), fit_details))
}

recovery_fit <- function(task) {
  x <- readRDS(task$input); x <- x[x$eligible, , drop = FALSE]
  if (nrow(x) < 20L || n_distinct(x$participant_key) < 4L)
    return(list(status = "unavailable_support", complete = TRUE, predictions = tibble(), models = list()))
  circular <- identical(unique(x$metric_geometry), "circular_time")
  if (length(unique(x$metric_geometry)) != 1L) stop("Task mixes metric geometries")
  recon_states <- recovery_reconstruction_states(); obs_states <- recovery_observability_states()
  for (state in recon_states[-1]) { x[[paste0("recon_", state)]] <- NA_real_; x[[paste0("recon_", state, "_fallback")]] <- FALSE }
  for (state in obs_states) x[[paste0("obs_", state)]] <- NA_real_
  models <- list()

  for (f in sort(unique(x$fold))) {
    ti <- which(x$fold != f); vi <- which(x$fold == f); tr <- x[ti, , drop = FALSE]; te <- x[vi, , drop = FALSE]
    if (nrow(tr) < 15L || n_distinct(tr$participant_key) < 3L)
      return(list(status = "unavailable_training_support", complete = TRUE, predictions = tibble(), models = list()))
    stopifnot(!any(tr$participant_key %in% te$participant_key))
    cal <- recovery_calibration(tr, te, circular)
    x[["recon_calibration"]][vi] <- cal$prediction; x[["recon_calibration_fallback"]][vi] <- cal$fallback
    models[[paste(f, "calibration", sep = "_")]] <- c(list(fold = f, state = "calibration", estimand = "reconstructability",
      learner = "conventional", n_train = nrow(tr), n_test = nrow(te), n_train_participants = n_distinct(tr$participant_key)), cal$model)

    shared_inner <- recovery_inner(tr, task$seed + 1000L*f)
    for (state_index in seq_along(c("prior_only", "signature", "context_only", "context"))) {
      state <- c("prior_only", "signature", "context_only", "context")[[state_index]]
      predictors <- recovery_layer_predictors(state)
      fit_y <- recovery_predict_information(tr, te, predictors, circular, "reconstructability", shared_inner,
        lambda = task$lambda, seed = task$seed + 10000L*f + 100L*state_index,
        xgb_grid = task$xgb_grid, calibration = cal, min_relative_gain = task$min_relative_gain)
      x[[paste0("recon_", state)]][vi] <- fit_y$prediction
      x[[paste0("recon_", state, "_fallback")]][vi] <- fit_y$fallback
      models[[paste(f, "reconstructability", state, sep = "_")]] <- c(list(fold = f, state = state,
        learner = "xgboost", n_train = nrow(tr), n_test = nrow(te), n_train_participants = n_distinct(tr$participant_key)), fit_y$model)

      fit_d <- recovery_predict_information(tr, te, predictors, circular, "observability", shared_inner,
        lambda = task$lambda, seed = task$seed + 50000L + 10000L*f + 100L*state_index,
        xgb_grid = task$xgb_grid, calibration = cal, min_relative_gain = task$min_relative_gain)
      x[[paste0("obs_", state)]][vi] <- fit_d$prediction
      models[[paste(f, "observability", state, sep = "_")]] <- c(list(fold = f, state = state,
        learner = "xgboost", n_train = nrow(tr), n_test = nrow(te), n_train_participants = n_distinct(tr$participant_key)), fit_d$model)
    }
    x[["obs_null"]][vi] <- median(abs(tr$z))
  }
  x$recon_raw <- x$candidate_value

  recon <- bind_rows(lapply(recon_states, function(state) {
    pred <- x[[paste0("recon_", state)]]; fallback <- if (state == "raw") rep(FALSE, nrow(x)) else x[[paste0("recon_", state, "_fallback")]]
    err_native <- recovery_delta(x$reference_value, pred, circular); serr <- err_native / x$standardizer
    x |> transmute(site, Id, Date, support_id, participant_key, fold, context_row_present,
      estimand = "reconstructability", state = state, learner = "xgboost",
      Y_L = candidate_value, Y_H = reference_value, raw_distortion = abs(z), target_value = reference_value,
      prediction = .env$pred, native_error = .env$err_native, signed_error = .env$serr, loss = abs(.env$serr),
      circular_fallback = .env$fallback)
  }))
  observability <- bind_rows(lapply(obs_states, function(state) {
    pred <- x[[paste0("obs_", state)]]; target <- abs(x$z); serr <- pred - target
    x |> transmute(site, Id, Date, support_id, participant_key, fold, context_row_present,
      estimand = "observability", state = state, learner = "xgboost",
      Y_L = candidate_value, Y_H = reference_value, raw_distortion = abs(z), target_value = .env$target,
      prediction = .env$pred, native_error = .env$serr, signed_error = .env$serr, loss = abs(.env$serr),
      circular_fallback = FALSE)
  }))
  list(status = "complete", complete = TRUE, predictions = bind_rows(recon, observability), models = models)
}

recovery_task <- function(task) {
  old <- if (file.exists(task$output)) tryCatch(readRDS(task$output), error = function(e) NULL) else NULL
  if (!is.null(old) && identical(old$run_id, task$run_id) && isTRUE(old$complete))
    return(list(index = task$index, path = task$output, status = old$status, reused = TRUE))
  started <- Sys.time(); result <- tryCatch(recovery_fit(task), error = function(e)
    list(status = "failed", complete = FALSE, error = conditionMessage(e), predictions = tibble(), models = list()))
  result$run_id <- task$run_id; result$meta <- task$meta; result$elapsed_seconds <- as.numeric(difftime(Sys.time(), started, units = "secs"))
  recovery_atomic(result, task$output); list(index = task$index, path = task$output, status = result$status, reused = FALSE)
}

recovery_worker_exports <- function() c("recovery_task", "recovery_fit", "recovery_predict_information", "recovery_calibration",
  "recovery_design", "recovery_delta", "recovery_residual_target", "recovery_apply_residual", "recovery_prediction_loss",
  "recovery_xgb_inner_channel", "recovery_xgb_refit_channel", "recovery_ridge_fit", "recovery_choose_candidate", "recovery_inner",
  "recovery_xgb_grid", "recovery_ridge_grid", "recovery_predictors", "recovery_atomic", "rq2_model_helpers",
  "recovery_layer_predictors", "recovery_signature_predictors", "recovery_temporal_bases", "recovery_temporal_predictors",
  "recovery_context_predictors", "recovery_reconstruction_states", "recovery_observability_states", "recovery_dayparts",
  "rq2_context_external_predictors", "rq2_context_micro_predictors", "rq2_context_behaviour_predictors")

recovery_smoke <- function(inputs) {
  started <- Sys.time(); cache_path <- inputs$paths[["temporal_context"]]; cache_md5 <- tools::md5sum(cache_path)
  x <- recovery_pairs(inputs); seed <- as.integer(Sys.getenv("RQ2_RECOVERY_SEED", "20260912")); folds <- as.integer(Sys.getenv("RQ2_RECOVERY_FOLDS", "5"))
  min_gain <- as.numeric(Sys.getenv("RQ2_RECOVERY_MIN_INNER_GAIN", "0.005"))
  if (!is.finite(seed) || !is.finite(folds) || folds < 2L || !is.finite(min_gain) || min_gain < 0 || min_gain >= 1) stop("Invalid smoke settings")
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
  tasks <- vector("list", nrow(meta))
  for (i in seq_len(nrow(meta))) {
    g <- semi_join(x, meta[i, ], by = keys); frozen <- inputs$summary |> semi_join(meta[i, ], by = c("dimension", "comparison_pair_id", "metric"))
    if (nrow(frozen) != 1L || abs(mean(abs(g$z[g$eligible])) - frozen$A_mean_absolute) > 1e-7) stop("Smoke raw A disagrees with current summary")
    ip <- file.path(root, paste0("input_", i, ".rds")); saveRDS(g, ip)
    tasks[[i]] <- list(index = i, input = ip, output = file.path(root, paste0("task_", i, ".rds")),
      seed = seed + i, xgb_grid = recovery_xgb_grid(), run_id = "real_smoke",
      lambda = as.numeric(Sys.getenv("RQ2_RECOVERY_LAMBDA", "0.01")), min_relative_gain = min_gain, meta = meta[i, ])
  }
  workers <- ms_resolve_workers("RQ2_RECOVERY_SMOKE_WORKERS", default = 2L, cap = 4L); ms_worker_init()
  refs <- ms_parallel_map(tasks, recovery_task, workers = workers, seed = seed,
    packages = c("dplyr", "tibble"), exports = recovery_worker_exports())
  report <- bind_rows(lapply(refs, function(r) {
    obj <- readRDS(r$path); if (!identical(obj$status, "complete")) stop("Real smoke task failed: ", if (is.null(obj$error)) obj$status else obj$error)
    expected <- c(length(recovery_reconstruction_states()), length(recovery_observability_states()))
    counts <- obj$predictions |> count(estimand, state) |> count(estimand, name = "n_states")
    if (!setequal(counts$n_states, expected)) stop("Smoke estimand/state contract failed")
    raw <- obj$predictions |> filter(estimand == "reconstructability", state == "raw")
    if (max(abs(raw$loss - raw$raw_distortion)) > 1e-8) stop("Raw reconstruction loss no longer equals RQ1 |z|")
    flexible <- obj$models[vapply(obj$models, function(m) !identical(m$state, "calibration"), logical(1))]
    for (fold in sort(unique(vapply(flexible, `[[`, integer(1), "fold")))) {
      fm <- flexible[vapply(flexible, function(m) identical(m$fold, fold), logical(1))]
      signatures <- vapply(fm, function(m) recovery_hash(m$inner_participants), character(1))
      if (length(unique(signatures)) != 1L) stop("Information states do not share the same inner participants")
      if (any(!vapply(fm, function(m) m$selected_kind %in% c("none", "ridge", "xgboost"), logical(1)))) stop("Unknown selected candidate")
    }
    stopifnot(recovery_task(tasks[[r$index]])$reused)
    tibble(metric = obj$meta$metric, contrast = obj$meta$comparison_pair_id,
      geometry = obj$meta$metric_geometry, heldout_days = nrow(raw), fitted_models = length(obj$models))
  }))
  stopifnot(identical(cache_md5, tools::md5sum(cache_path))); print(report)
  message("PASS real smoke: ", nrow(report), " tasks; anchored shared-split information recovery; ",
    round(as.numeric(difftime(Sys.time(), started, units = "secs")), 1), " s"); invisible(report)
}

recovery_summaries <- function(states, out, floor) {
  recon <- states |> filter(estimand == "reconstructability") |>
    select(learner, task_index, dimension, comparison_pair_id, metric, metric_class, state, loss) |>
    tidyr::pivot_wider(names_from = state, values_from = loss) |>
    mutate(calibration_recovery = if_else(raw > floor, 1 - calibration/raw, NA_real_),
      prior_recovery = if_else(raw > floor, 1 - prior_only/raw, NA_real_),
      signature_recovery = if_else(raw > floor, 1 - signature/raw, NA_real_),
      context_recovery = if_else(raw > floor, 1 - context_only/raw, NA_real_),
      joint_recovery = if_else(raw > floor, 1 - context/raw, NA_real_),
      signature_aux_gain = prior_only - signature,
      context_aux_gain = prior_only - context_only,
      joint_aux_gain = prior_only - context,
      signature_after_context_gain = context_only - context,
      context_after_signature_gain = signature - context,
      signature_shapley_gain = .5 * (signature_aux_gain + signature_after_context_gain),
      context_shapley_gain = .5 * (context_aux_gain + context_after_signature_gain),
      signature_shapley_fraction = if_else(prior_only > floor, signature_shapley_gain/prior_only, NA_real_),
      context_shapley_fraction = if_else(prior_only > floor, context_shapley_gain/prior_only, NA_real_))
  obs <- states |> filter(estimand == "observability") |>
    select(learner, task_index, dimension, comparison_pair_id, metric, metric_class, state, loss) |>
    tidyr::pivot_wider(names_from = state, values_from = loss) |>
    mutate(prior_skill = if_else(null > floor, 1 - prior_only/null, NA_real_),
      signature_skill = if_else(null > floor, 1 - signature/null, NA_real_),
      context_skill = if_else(null > floor, 1 - context_only/null, NA_real_),
      joint_skill = if_else(null > floor, 1 - context/null, NA_real_),
      signature_aux_gain = prior_only - signature,
      context_aux_gain = prior_only - context_only,
      joint_aux_gain = prior_only - context,
      signature_after_context_gain = context_only - context,
      context_after_signature_gain = signature - context,
      signature_shapley_gain = .5 * (signature_aux_gain + signature_after_context_gain),
      context_shapley_gain = .5 * (context_aux_gain + context_after_signature_gain),
      signature_shapley_fraction = if_else(prior_only > floor, signature_shapley_gain/prior_only, NA_real_),
      context_shapley_fraction = if_else(prior_only > floor, context_shapley_gain/prior_only, NA_real_))
  readr::write_csv(recon, file.path(out, "reconstructability_summary.csv"))
  readr::write_csv(obs, file.path(out, "distortion_observability_summary.csv"))
  joined <- inner_join(recon, obs,
    by = c("learner", "task_index", "dimension", "comparison_pair_id", "metric", "metric_class"),
    suffix = c("_reconstruction", "_observability"))
  readr::write_csv(joined, file.path(out, "information_recoverability_joint.csv"))
  overview <- bind_rows(
    recon |> group_by(learner, comparison_pair_id) |> summarise(estimand = "reconstructability", n_metrics = n(),
      mean_full = mean(joint_recovery, na.rm = TRUE), mean_context = mean(context_recovery, na.rm = TRUE),
      fraction_context_positive = mean(context_aux_gain > 0, na.rm = TRUE), .groups = "drop"),
    obs |> group_by(learner, comparison_pair_id) |> summarise(estimand = "observability", n_metrics = n(),
      mean_full = mean(joint_skill, na.rm = TRUE), mean_context = mean(context_skill, na.rm = TRUE),
      fraction_context_positive = mean(context_aux_gain > 0, na.rm = TRUE), .groups = "drop"))
  readr::write_csv(overview, file.path(out, "recovery_overview.csv"))
}

recovery_run <- function(inputs) {
  if (!requireNamespace("xgboost", quietly = TRUE)) stop("Formal recovery requires xgboost")
  integer_env <- function(name, default, minimum) {
    v <- suppressWarnings(as.integer(Sys.getenv(name, as.character(default)))); if (length(v) != 1L || !is.finite(v) || v < minimum) stop("Invalid ", name); v
  }
  seed <- integer_env("RQ2_RECOVERY_SEED", 20260912L, 1L); folds <- integer_env("RQ2_RECOVERY_FOLDS", 5L, 2L)
  lambda <- as.numeric(Sys.getenv("RQ2_RECOVERY_LAMBDA", "0.01")); floor <- as.numeric(Sys.getenv("RQ2_RECOVERY_G_FLOOR", "0.000001"))
  min_gain <- as.numeric(Sys.getenv("RQ2_RECOVERY_MIN_INNER_GAIN", "0.005"))
  if (!is.finite(lambda) || lambda <= 0 || !is.finite(floor) || floor <= 0 || !is.finite(min_gain) || min_gain < 0 || min_gain >= 1) stop("Invalid recovery settings")
  workers <- ms_resolve_workers("RQ2_RECOVERY_WORKERS", default = 36L, cap = 48L); ms_worker_init()
  code <- c("scripts/12d_rq2_recovery.R", "scripts/utils/rq1_inference.R", "scripts/utils/rq1_inference_contract.R",
    "scripts/utils/rq1_pairwise_artifacts.R", "scripts/utils/rq2_context_features.R", "scripts/utils/rq2_model_helpers.R",
    "scripts/utils/parallel_runtime.R", "scripts/utils/analysis_design.R", "scripts/utils/artifact_validation.R",
    "scripts/12c_rq2_context_models.R", "scripts/utils/rq_context.R", "scripts/utils/melidos_io.R",
    "scripts/utils/core_context.R", "scripts/utils/core_artifacts.R", "external/LightLogR/R/normalise.R")
  provenance <- list(recovery_version = "information_recoverability_v1",
    estimator_version = "anchored_shared_split_candidate_library_v2",
    rq1_analysis_version = inputs$version, core_artifact_version = inputs$core, analysis_design_id = ms_analysis_design_id(),
    input_md5 = tools::md5sum(inputs$paths), code_md5 = tools::md5sum(code), seed = seed, folds = folds, lambda = lambda,
    min_inner_relative_gain = min_gain, G_floor = floor,
    predictors = recovery_predictors(), signature_predictors = recovery_signature_predictors(), temporal_predictors = recovery_temporal_predictors(),
    reconstructability_states = recovery_reconstruction_states(), observability_states = recovery_observability_states(),
    calibration = "outer-training affine Y_H~Y_L; circular metrics use affine sin/cos mapping",
    information_sets = "P-only, P+S, P+C, P+S+C; identical predictor dictionaries for every metric and transition",
    estimands = c(reconstructability = "standardized absolute geometric error after anchored discrepancy correction",
      observability = "absolute error in predicting D=|z|"),
    candidate_library = "no update/null, ridge, and XGBoost; conservative inner selection; same library for every information state",
    temporal_source_manifest = inputs$temporal_sources, dayparts = recovery_dayparts(),
    xgb_grid = recovery_xgb_grid(), ridge_grid = recovery_ridge_grid(lambda),
    inner_validation = "one participant-grouped inner split per outer fold, shared by every information state and both estimands; calibration refit inside inner training",
    R = R.version.string, packages = sapply(c("dplyr", "tibble", "readr", "data.table", "xgboost"), function(p) as.character(utils::packageVersion(p))),
    context_provenance_limit = "Context is built only from current weather/diary/context files; no target-state predictors",
    scale_role = "RQ1 standardizer defines reconstruction loss and D=|z|",
    model = "Conventional calibrated prior P plus conservatively selected discrepancy correction; D=|z| observability uses the same information sets")
  run_id <- recovery_hash(provenance); out <- file.path("results/rq2/recovery", inputs$version, run_id); dir.create(out, recursive = TRUE, showWarnings = FALSE)
  recovery_atomic(c(provenance, list(run_id = run_id, workers = workers, started = Sys.time(), session = capture.output(sessionInfo()))), file.path(out, "provenance.rds"))
  message("Recoverability: extract current non-duration anchors")
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
  })) |> mutate(task_index = row_number(), learner = "xgboost") |>
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
  readr::write_csv(catalog, file.path(out, "task_catalog.csv"))
  tasks <- lapply(seq_len(nrow(catalog)), function(i) list(index = i, input = input_paths[[i]],
    output = file.path(out, "checkpoints", sprintf("task_%04d.rds", i)),
    seed = seed + i, xgb_grid = recovery_xgb_grid(), run_id = run_id, lambda = lambda,
    min_relative_gain = min_gain, meta = catalog[i, ]))
  rm(x, groups); invisible(gc(FALSE))
  message("Recoverability: ", length(tasks), " tasks; ", workers, " workers; anchored adaptive recovery + D=|z| observability; ", out)
  refs <- ms_parallel_map(tasks, recovery_task, workers = workers, seed = seed, packages = c("dplyr", "tibble"), exports = recovery_worker_exports())
  state_rows <- list(); statuses <- list(); fold_rows <- list()
  for (r in refs) {
    obj <- readRDS(r$path)
    statuses[[r$index]] <- bind_cols(catalog[r$index, ], tibble(status = obj$status,
      error = if (is.null(obj$error)) NA_character_ else obj$error, reused = r$reused, checkpoint = r$path, elapsed_seconds = obj$elapsed_seconds))
    if (!nrow(obj$predictions)) {
      empty_contract <- bind_rows(tibble(estimand = "reconstructability", state = recovery_reconstruction_states()),
        tibble(estimand = "observability", state = recovery_observability_states()))
      state_rows[[r$index]] <- bind_cols(catalog[rep(r$index, nrow(empty_contract)), ], empty_contract,
        tibble(n_test = 0L, n_test_participants = 0L, loss = NA_real_, A = NA_real_, B = NA_real_,
          RMSE = NA_real_, n_circular_fallback = 0L, status = obj$status)); next
    }
    s <- obj$predictions |> group_by(estimand, state) |> summarise(n_test = n(), n_test_participants = n_distinct(participant_key),
      loss = mean(loss), A = mean(loss), B = mean(signed_error), RMSE = sqrt(mean(signed_error^2)),
      n_circular_fallback = sum(circular_fallback), status = obj$status, .groups = "drop")
    state_rows[[r$index]] <- bind_cols(catalog[rep(r$index, nrow(s)), ], s)
    f <- obj$predictions |> group_by(fold, estimand, state) |> summarise(n_test = n(), loss = mean(loss), .groups = "drop")
    fold_rows[[r$index]] <- bind_cols(catalog[rep(r$index, nrow(f)), ], f)
  }
  status <- bind_rows(statuses); states <- bind_rows(state_rows)
  readr::write_csv(status, file.path(out, "task_status.csv")); readr::write_csv(states, file.path(out, "heldout_errors.csv"))
  readr::write_csv(bind_rows(fold_rows), file.path(out, "fold_errors.csv"))
  if (any(is.finite(states$loss))) recovery_summaries(filter(states, is.finite(loss)), out, floor)
  recovery_atomic(list(run_id = run_id, complete = !any(status$status == "failed"), statuses = status, heldout_errors = states, provenance = provenance),
    file.path(out, "recovery_manifest.rds"))
  message("Recoverability outputs: ", out); if (any(status$status == "failed")) stop("Some recoverability tasks failed; successful checkpoints retained")
  invisible(out)
}

if (sys.nframe() == 0L) {
  args <- commandArgs(trailingOnly = TRUE); command <- if (length(args)) args[[1]] else ""
  allowed <- c("--check-inputs", "--build-context", "--smoke-test", "--run", "--legacy-run", "--summarize")
  if (!command %in% allowed || length(args) != 1L) stop("Use --run, --summarize, --check-inputs, --build-context, --smoke-test, or --legacy-run")
  if(command%in%c("--run","--summarize")) {
    suppressPackageStartupMessages(library(data.table))
    source("scripts/utils/rq2_conditional_reliability.R")
    if(command=="--run")reliability_run() else reliability_finalize()
  } else {
  inputs <- recovery_inputs(); if (length(inputs$problems)) stop(paste(c(inputs$problems, "Supply the current input artifacts."), collapse = "\n"))
  if (command == "--check-inputs") message("Current paths and context schemas validated; daypart cache ready.")
  else if (command == "--build-context") message("Rich daypart context ready; reused=", inputs$temporal_reused)
  else if (command == "--smoke-test") recovery_smoke(inputs)
  else recovery_run(inputs)
  }
}
