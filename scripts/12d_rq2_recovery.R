# Canonical RQ2 conditional reliability and its shared frozen-input builders.
# Historical recovery estimators are retired; --run uses the risk/Brier analysis.
# Rscript scripts/12d_rq2_recovery.R --check-inputs
# RQ2_RELIABILITY_WORKERS=12 Rscript scripts/12d_rq2_recovery.R --run
suppressPackageStartupMessages({ library(dplyr); library(tibble) })
source("scripts/utils/rq1_inference.R")
source("scripts/utils/rq2_context_features.R")
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

recovery_ensure_dayparts <- function(path, weather_path, unit_path, diary_paths, core_version, rq1_version,
                                    allow_build = TRUE) {
  provenance <- recovery_daypart_provenance(weather_path, unit_path, diary_paths, core_version, rq1_version)
  old <- if (file.exists(path)) tryCatch(readRDS(path), error = function(e) NULL) else NULL
  if (!is.null(old) && identical(old$provenance, provenance) && identical(old$data_md5, recovery_hash(old$data))) {
    recovery_temporal_context(old, core_version, rq1_version)
    message("Daypart context: reused ", path)
    return(list(object = old, reused = TRUE))
  }
  if (!allow_build) stop("Missing or stale daypart context; rebuild inputs before selecting a frozen export")
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
  upstream$part_dir <- rq1_pairwise_part_dir(upstream, repo_root)
  # Already resolved against repo_root; do not resolve a missing fallback again
  # against the process working directory when reporting incomplete copies.
  parts <- file.path(upstream$part_dir, upstream$parts); problems <- character()
  for (p in parts) {
    if (!file.exists(p)) problems <- c(problems, paste0("Missing anchor part: ", p))
    if (!file.exists(paste0(p, ".ok"))) problems <- c(problems, paste0("Missing completion marker: ", p, ".ok"))
  }
  list(upstream = upstream, paths = parts, problems = problems)
}

recovery_inputs <- function(allow_build = TRUE) {
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
    cached <- recovery_ensure_dayparts(extra_paths[["temporal_context"]], weather_path, extra_paths[["unit_context"]], diary_paths, core, version,
                                      allow_build = allow_build)
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

if (sys.nframe() == 0L) {
  args <- commandArgs(trailingOnly = TRUE)
  allowed <- c("--check-inputs", "--build-context", "--run", "--summarize")
  if (length(args) != 1L || !args[[1]] %in% allowed) {
    stop("Use --run, --summarize, --check-inputs, or --build-context; historical recovery fitting is retired")
  }
  command <- args[[1]]
  if (command %in% c("--run", "--summarize")) {
    suppressPackageStartupMessages(library(data.table))
    source("scripts/utils/rq2_conditional_reliability.R")
    if (command == "--run") reliability_run() else reliability_finalize()
  } else {
    inputs <- recovery_inputs()
    if (length(inputs$problems)) stop(paste(inputs$problems, collapse = "\n"))
    message("Current daily inputs and daypart context validated; reused=", inputs$temporal_reused)
  }
}
