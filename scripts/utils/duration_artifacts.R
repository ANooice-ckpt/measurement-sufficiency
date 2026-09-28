# Materialise duration state values once in core so RQ1/RQ2/RQ3 share the same
# actual window representations. Daily targets are aggregated from the durable
# participant-day metric cube; IS/IV are rebuilt from the stored hourly basis.

duration_circular_mean <- function(x, period = 86400) {
  x <- x[is.finite(x)]
  if (!length(x)) return(NA_real_)
  theta <- 2 * pi * x / period
  (atan2(mean(sin(theta)), mean(cos(theta))) %% (2 * pi)) * period / (2 * pi)
}

duration_aggregate <- function(x, geometry) {
  if (!length(x) || any(!is.finite(x))) return(NA_real_)
  if (identical(geometry, "circular_time")) duration_circular_mean(x) else mean(x)
}

duration_cube_is_partitioned <- function(x) {
  is.list(x) && identical(x$artifact_type, "partitioned_duration_metric_cube")
}

load_duration_metric_cube <- function(x, columns = NULL, filter_fn = NULL) {
  if (is.data.frame(x)) return(x)
  if (!duration_cube_is_partitioned(x)) stop("Unsupported duration metric cube artifact")
  paths <- file.path(x$part_dir, x$parts)
  missing <- paths[!file.exists(paths)]
  if (length(missing)) stop("Missing duration cube part: ", missing[[1]])
  read_part <- function(path) {
    z <- readRDS(path)
    if (!is.null(columns)) z <- z |> dplyr::select(dplyr::all_of(columns))
    if (!is.null(filter_fn)) z <- filter_fn(z)
    z
  }
  dplyr::bind_rows(lapply(paths, read_part))
}

duration_decode_hourly_basis <- function(encoded) {
  if (length(encoded) != 1L) stop("Expected one encoded hourly basis per analysis day")
  if (is.na(encoded) || !nzchar(encoded)) return(matrix(numeric(), ncol = 3L))
  records <- strsplit(encoded, ";", fixed = TRUE)[[1L]]
  fields <- strsplit(records, ":", fixed = TRUE)
  if (any(lengths(fields) != 3L)) stop("Malformed exact hourly basis")
  values <- suppressWarnings(as.numeric(unlist(fields, use.names = FALSE)))
  out <- matrix(values, ncol = 3L, byrow = TRUE)
  if (any(!is.finite(out)) || any(out[, 2L] < 0 | out[, 2L] > 23 | out[, 2L] != floor(out[, 2L])) ||
      any(diff(out[, 1L]) <= 0)) stop("Invalid exact hourly basis")
  out
}

duration_isiv_from_context <- function(x) {
  if (!"isiv_hourly_basis" %in% names(x)) {
    stop("Duration IS/IV requires the exact hourly basis; rebuild core artifacts")
  }
  if (!nrow(x)) {
    return(tibble::tibble(metric = c("interdaily_stability", "intradaily_variability"), value = NA_real_))
  }
  # Reproduce LightLogR's population-variance equations on actual hourly
  # instants. Fall-back hours remain distinct in total variance and IV while
  # their local clock labels are pooled only for IS's average daily pattern.
  x <- x[order(x$Date), , drop = FALSE]
  basis <- x$isiv_hourly_basis
  if (!is.list(basis)) basis <- lapply(basis, duration_decode_hourly_basis)
  hourly <- do.call(rbind, basis)
  if (nrow(hourly)) {
    hourly <- hourly[order(hourly[, 1L]), , drop = FALSE]
    if (anyDuplicated(hourly[, 1L])) stop("Duplicate hourly instants in duration window")
  }
  hourly_values <- hourly[, 3L]
  hourly_index <- hourly[, 2L]
  if (length(hourly_values) < 2L) {
    vals <- c(NA_real_, NA_real_)
  } else {
    overall_mean <- mean(hourly_values)
    var_total <- sum((hourly_values - overall_mean)^2) / length(hourly_values)
    by_hour <- tapply(hourly_values, hourly_index, mean)
    var_avg_day <- sum((by_hour - overall_mean)^2) / length(by_hour)
    var_hourly_diff <- sum(diff(hourly_values)^2) / (length(hourly_values) - 1L)
    vals <- c(var_avg_day / var_total, var_hourly_diff / var_total)
  }
  tibble::tibble(metric = c("interdaily_stability", "intradaily_variability"), value = as.numeric(vals))
}

build_duration_metric_cube <- function(metric_cube, unit_context, metric_types, max_days = 6L, part_dir = NULL, reuse_parts = TRUE) {
  ref_context <- unit_context |>
    dplyr::filter(analysis_unit_type == "participant_day", !is.na(Date), placement == "eye", optical == "MEDI", resolution_s == 10L) |>
    dplyr::distinct(support_id, site, Id, Date)
  runs <- complete_analysis_day_runs(ref_context)
  windows <- duration_window_manifest(ref_context, max_days = max_days)
  if (!nrow(windows)) {
    return(list(manifest = windows, runs = runs, audit = duration_cohort_audit(windows, runs), cube = tibble::tibble()))
  }

  membership <- windows |>
    dplyr::select(support_id, site, Id, run_id, window_id, window_index, n_days, window_start, window_end, member_dates) |>
    tidyr::unnest_longer(member_dates, values_to = "Date") |>
    dplyr::mutate(Date = as.Date(Date))

  if (!"isiv_hourly_basis" %in% names(unit_context)) {
    stop("Duration artifacts require the exact hourly basis; rebuild core artifacts")
  }
  isiv_meta <- metric_types |>
    dplyr::filter(metric %in% c("interdaily_stability", "intradaily_variability")) |>
    dplyr::mutate(metric_scope = "multiday", metric_geometry = "linear")

  # Duration parts are independent support x site blocks. Split the large inputs
  # once, then build/checkpoint each block separately to bound memory use.
  block_keys <- windows |>
    dplyr::distinct(support_id, site) |>
    dplyr::arrange(support_id, site)
  block_token <- function(support_id, site) paste(support_id, site, sep = "\r")
  metric_blocks <- split(metric_cube, block_token(metric_cube$support_id, metric_cube$site))
  context_blocks <- split(unit_context, block_token(unit_context$support_id, unit_context$site))
  rm(metric_cube, unit_context)
  invisible(gc())

  build_block <- function(block_key) {
    support_value <- block_key$support_id[[1L]]
    site_value <- block_key$site[[1L]]
    key <- block_token(support_value, site_value)
    block_membership <- membership |>
      dplyr::filter(support_id == support_value, site == site_value)
    if (!nrow(block_membership)) return(list(daily = tibble::tibble(), isiv = tibble::tibble()))

    metric_block <- metric_blocks[[key]]
    context_block <- context_blocks[[key]]
    if (is.null(metric_block) || is.null(context_block)) {
      return(list(daily = tibble::tibble(), isiv = tibble::tibble()))
    }
    # Decode once per configuration-day, rather than once per overlapping window.
    context_block$isiv_hourly_basis <- lapply(context_block$isiv_hourly_basis, duration_decode_hourly_basis)

    daily_block <- metric_block |>
      dplyr::filter(
        analysis_unit_type == "participant_day", support_id == support_value,
        site == site_value, !metric %in% c("interdaily_stability", "intradaily_variability")
      ) |>
      dplyr::select(
        support_id, site, Id, Date, placement, optical, resolution_s, is_primary_resolution, config_id,
        metric, metric_class, metric_scope, metric_geometry, value, available, unavailable_reason
      ) |>
      dplyr::inner_join(block_membership, by = c("support_id", "site", "Id", "Date"), relationship = "many-to-many") |>
      dplyr::group_by(
        support_id, site, Id, run_id, window_id, window_index, n_days, window_start, window_end,
        placement, optical, resolution_s, is_primary_resolution, config_id,
        metric, metric_class, metric_scope, metric_geometry
      ) |>
      dplyr::summarise(
        n_days_present = dplyr::n_distinct(Date),
        value = if (n_days_present == dplyr::first(n_days) && all(dplyr::coalesce(available, FALSE) & is.finite(value)))
          duration_aggregate(value, dplyr::first(metric_geometry)) else NA_real_,
        available = is.finite(value),
        unavailable_reason = if (is.finite(value)) NA_character_ else "one or more complete-day metric representations unavailable",
        .groups = "drop"
      )

    isiv_block <- tibble::tibble()
    if (nrow(isiv_meta)) {
      isiv_block <- context_block |>
        dplyr::filter(analysis_unit_type == "participant_day", support_id == support_value, site == site_value, !is.na(Date)) |>
        dplyr::select(support_id, site, Id, Date, placement, optical, resolution_s, is_primary_resolution, config_id, isiv_hourly_basis) |>
        dplyr::inner_join(block_membership, by = c("support_id", "site", "Id", "Date"), relationship = "many-to-many") |>
        dplyr::group_by(
          support_id, site, Id, run_id, window_id, window_index, n_days, window_start, window_end,
          placement, optical, resolution_s, is_primary_resolution, config_id
        ) |>
        dplyr::group_modify(~{
          vals <- duration_isiv_from_context(.x)
          if (dplyr::n_distinct(.x$Date) != dplyr::first(.y$n_days)) vals$value <- NA_real_
          vals
        }) |>
        dplyr::ungroup() |>
        dplyr::left_join(isiv_meta, by = "metric") |>
        dplyr::mutate(
          n_days_present = n_days, available = is.finite(value),
          unavailable_reason = dplyr::if_else(available, NA_character_, "IS/IV undefined on selected complete-day hourly basis")
        ) |>
        dplyr::select(
          support_id, site, Id, run_id, window_id, window_index, n_days, window_start, window_end,
          placement, optical, resolution_s, is_primary_resolution, config_id, metric, metric_class,
          metric_scope, metric_geometry, n_days_present, value, available, unavailable_reason
        )
    }
    list(daily = daily_block, isiv = isiv_block)
  }

  finalize_part <- function(block) {
    raw <- dplyr::bind_rows(block$daily, block$isiv)
    if (!nrow(raw)) return(raw)
    raw |>
      dplyr::mutate(
        analysis_unit_type = "participant_window", analysis_unit_id = window_id,
        duration_artifact_version = "duration_complete_analysis_days_v2_exact_hours"
      ) |>
      dplyr::select(
        support_id, site, Id, analysis_unit_type, analysis_unit_id, run_id, window_id, window_index,
        window_start, window_end, n_days, n_days_present, placement, optical, resolution_s,
        is_primary_resolution, config_id, metric, metric_class, metric_scope, metric_geometry,
        value, available, unavailable_reason, duration_artifact_version
      )
  }

  if (!is.null(part_dir)) {
    dir.create(part_dir, recursive = TRUE, showWarnings = FALSE)
    part_paths <- character(nrow(block_keys))
    part_rows <- integer(nrow(block_keys))
    part_support <- character(nrow(block_keys))
    part_site <- character(nrow(block_keys))

    duration_workers <- suppressWarnings(as.integer(Sys.getenv("CORE_DURATION_WORKERS", unset = "1")))
    if (!is.finite(duration_workers) || duration_workers < 1L) duration_workers <- 1L
    detected_cores <- suppressWarnings(parallel::detectCores(logical = FALSE))
    if (!is.finite(detected_cores) || detected_cores < 1L) detected_cores <- parallel::detectCores(logical = TRUE)
    if (is.finite(detected_cores) && detected_cores > 0L) duration_workers <- min(duration_workers, detected_cores)
    duration_workers <- min(duration_workers, nrow(block_keys))

    build_part <- function(i) {
      part_name <- sprintf("duration_metric_cube_part_%03d.rds", i)
      part_path <- file.path(part_dir, part_name)
      part_marker <- paste0(part_path, ".ok")
      support_value <- as.character(block_keys$support_id[[i]])
      site_value <- as.character(block_keys$site[[i]])
      if (reuse_parts && file.exists(part_path)) {
        marker_ok <- file.exists(part_marker)
        if (marker_ok) {
          marker <- readLines(part_marker, n = 2L, warn = FALSE)
          if (length(marker) && identical(marker[[1L]], "duration_complete_analysis_days_v2_exact_hours")) {
            rows <- if (length(marker) >= 2L) suppressWarnings(as.integer(marker[[2L]])) else NA_integer_
            if (!is.finite(rows) || rows < 0) rows <- nrow(readRDS(part_path))
            if (length(rows) != 1L || !is.finite(rows)) stop("Cannot recover duration checkpoint row count: ", part_path)
            return(list(i = i, path = part_path, rows = rows, support = support_value, site = site_value))
          }
          message("[duration] rebuilding checkpoint with incompatible hourly-basis marker: ", part_path)
        }
      }
      block <- build_block(block_keys[i, , drop = FALSE])
      part <- finalize_part(block)
      tmp_path <- paste0(part_path, ".tmp")
      if (file.exists(tmp_path)) unlink(tmp_path)
      saveRDS(part, tmp_path, compress = FALSE)
      if (file.exists(part_marker)) unlink(part_marker)
      if (file.exists(part_path)) unlink(part_path)
      if (!file.rename(tmp_path, part_path)) stop("Could not atomically install duration checkpoint: ", part_path)
      rows <- nrow(part)
      writeLines(c("duration_complete_analysis_days_v2_exact_hours", as.character(rows)), part_marker, useBytes = TRUE)
      rm(block, part)
      invisible(gc(FALSE))
      list(i = i, path = part_path, rows = rows, support = support_value, site = site_value)
    }

    # Largest blocks first reduces the long tail on high-core-count Linux hosts.
    block_cost <- vapply(seq_len(nrow(block_keys)), function(i) {
      support_value <- as.character(block_keys$support_id[[i]])
      site_value <- as.character(block_keys$site[[i]])
      key <- block_token(support_value, site_value)
      metric_n <- if (is.null(metric_blocks[[key]])) 0 else nrow(metric_blocks[[key]])
      context_n <- if (is.null(context_blocks[[key]])) 0 else nrow(context_blocks[[key]])
      membership_n <- sum(membership$support_id == support_value & membership$site == site_value)
      as.numeric(metric_n + context_n + 10 * membership_n)
    }, numeric(1))
    schedule_idx <- order(block_cost, decreasing = TRUE, na.last = TRUE)
    message("[duration] LPT schedule: ", nrow(block_keys), " blocks; workers=", duration_workers)

    if (.Platform$OS.type != "windows" && duration_workers > 1L) {
      part_records_scheduled <- parallel::mclapply(
        schedule_idx, build_part, mc.cores = duration_workers,
        mc.preschedule = FALSE, mc.set.seed = FALSE
      )
    } else {
      part_records_scheduled <- lapply(schedule_idx, build_part)
    }
    failed <- vapply(part_records_scheduled, inherits, logical(1), "try-error")
    if (any(failed)) stop("Duration checkpoint worker failed: ", as.character(part_records_scheduled[[which(failed)[1L]]]))
    part_records <- part_records_scheduled[order(schedule_idx)]
    for (rec in part_records) {
      i <- rec$i
      part_paths[[i]] <- rec$path
      part_rows[[i]] <- rec$rows
      part_support[[i]] <- rec$support
      part_site[[i]] <- rec$site
    }

    part_manifest <- tibble::tibble(
      part = basename(part_paths), part_dir = part_dir, rows = part_rows,
      support_id = part_support, site = part_site
    )
    rm(block_keys, membership, metric_blocks, context_blocks, block_token, build_block, finalize_part)
    invisible(gc())
    cube <- list(
      artifact_type = "partitioned_duration_metric_cube",
      duration_artifact_version = "duration_complete_analysis_days_v2_exact_hours",
      part_dir = part_dir, parts = part_manifest$part,
      part_manifest = part_manifest, rows = sum(part_manifest$rows, na.rm = TRUE)
    )
  } else {
    block_parts <- lapply(seq_len(nrow(block_keys)), function(i) {
      finalize_part(build_block(block_keys[i, , drop = FALSE]))
    })
    cube <- dplyr::bind_rows(block_parts)
    rm(block_parts, block_keys, membership, metric_blocks, context_blocks, block_token, build_block, finalize_part)
    invisible(gc())
  }
  list(manifest = windows, runs = runs, audit = duration_cohort_audit(windows, runs), cube = cube)
}
