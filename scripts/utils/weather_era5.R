# ERA5 ingestion, physical QC, daily summaries, and a reusable 1-minute
# continuous context artifact. Handles both plain CSV and ZIP payloads saved
# with a .csv extension by CDS.

era5_read_payload <- function(path) {
  con <- file(path, "rb")
  magic <- readBin(con, what = "raw", n = 4L)
  close(con)
  is_zip <- length(magic) >= 2L && identical(as.integer(magic[1:2]), c(80L, 75L))

  if (!is_zip) {
    return(list(
      data = readr::read_csv(path, show_col_types = FALSE, progress = FALSE),
      payload_format = "csv"
    ))
  }

  listing <- utils::unzip(path, list = TRUE)
  csv_names <- listing$Name[grepl("[.]csv$", listing$Name, ignore.case = TRUE)]
  if (length(csv_names) != 1L) {
    stop("Expected exactly one CSV inside ERA5 payload ", path, "; found ", length(csv_names))
  }
  td <- tempfile("era5_zip_")
  dir.create(td)
  on.exit(unlink(td, recursive = TRUE, force = TRUE), add = TRUE)
  utils::unzip(path, files = csv_names, exdir = td)
  extracted <- file.path(td, csv_names)
  list(
    data = readr::read_csv(extracted, show_col_types = FALSE, progress = FALSE),
    payload_format = "zip_csv"
  )
}

era5_pick_numeric <- function(x, aliases) {
  nm <- intersect(aliases, names(x))[1]
  if (is.na(nm)) return(rep(NA_real_, nrow(x)))
  as.numeric(x[[nm]])
}

era5_pick_time <- function(x) {
  nm <- intersect(c("valid_time", "time", "datetime", "Datetime", "date"), names(x))[1]
  if (is.na(nm)) stop("No recognizable ERA5 time column")

  raw <- x[[nm]]
  dt <- suppressWarnings(
    lubridate::ymd_hms(raw, tz = "UTC", quiet = TRUE, truncated = 3)
  )
  bad <- is.na(dt) & !is.na(raw)
  if (any(bad)) {
    fallback <- suppressWarnings(as.POSIXct(raw[bad], tz = "UTC"))
    dt[bad] <- fallback
  }
  if (all(is.na(dt))) stop("Could not parse ERA5 time column")
  dt
}

era5_validate_tz <- function(tz) {
  if (length(tz) != 1L || is.na(tz) || !nzchar(tz) || !(tz %in% OlsonNames())) {
    stop("Invalid ERA5 timezone: ", paste(tz, collapse = ", "))
  }
  invisible(tz)
}

era5_rh_pct <- function(t_c, td_c) {
  td_eff <- pmin(td_c, t_c)
  es_t <- 6.112 * exp((17.67 * t_c) / (t_c + 243.5))
  es_td <- 6.112 * exp((17.67 * td_eff) / (td_eff + 243.5))
  rh <- 100 * es_td / es_t
  pmin(100, pmax(0, rh))
}

era5_vpd_kpa <- function(t_c, rh_pct) {
  es_kpa <- 0.6112 * exp((17.67 * t_c) / (t_c + 243.5))
  pmax(0, es_kpa * (1 - rh_pct / 100))
}

era5_qc_hourly <- function(raw, site, tz, payload_format) {
  era5_validate_tz(tz)
  time_utc <- era5_pick_time(raw)
  x <- tibble::tibble(
    site = site,
    time_utc = time_utc,
    timezone = tz,
    u100_ms = era5_pick_numeric(raw, c("u100", "100m_u_component_of_wind")),
    v100_ms = era5_pick_numeric(raw, c("v100", "100m_v_component_of_wind")),
    u10_ms = era5_pick_numeric(raw, c("u10", "10m_u_component_of_wind")),
    v10_ms = era5_pick_numeric(raw, c("v10", "10m_v_component_of_wind")),
    dewpoint_c = era5_pick_numeric(raw, c("d2m", "2m_dewpoint_temperature")) - 273.15,
    t2m_c = era5_pick_numeric(raw, c("t2m", "2m_temperature")) - 273.15,
    boundary_layer_height_m = era5_pick_numeric(raw, c("blh", "boundary_layer_height")),
    cloud_base_height_m = era5_pick_numeric(raw, c("cbh", "cloud_base_height")),
    msl_pressure_hpa = era5_pick_numeric(raw, c("msl", "mean_sea_level_pressure")) / 100,
    skin_temperature_c = era5_pick_numeric(raw, c("skt", "skin_temperature")) - 273.15,
    surface_pressure_hpa = era5_pick_numeric(raw, c("sp", "surface_pressure")) / 100,
    ssrd_j_m2 = era5_pick_numeric(raw, c("ssrd", "surface_solar_radiation_downwards")),
    strd_j_m2 = era5_pick_numeric(raw, c("strd", "surface_thermal_radiation_downwards")),
    total_cloud_cover = era5_pick_numeric(raw, c("tcc", "total_cloud_cover")),
    precipitation_mm = era5_pick_numeric(raw, c("tp", "total_precipitation")) * 1000,
    fdir_j_m2 = era5_pick_numeric(raw, c("fdir", "total_sky_direct_solar_radiation_at_surface")),
    grid_latitude = era5_pick_numeric(raw, c("latitude")),
    grid_longitude = era5_pick_numeric(raw, c("longitude"))
  ) |>
    dplyr::arrange(time_utc) |>
    dplyr::distinct(time_utc, .keep_all = TRUE)

  # Tiny negative de-accumulation/rounding noise is treated as zero; larger or
  # physically impossible values are made missing and counted in the QC audit.
  for (nm in c("ssrd_j_m2", "strd_j_m2", "fdir_j_m2", "precipitation_mm")) {
    z <- x[[nm]]
    z[is.finite(z) & z < 0 & z >= -1e-6] <- 0
    x[[nm]] <- z
  }
  z <- x$total_cloud_cover
  z[is.finite(z) & z < 0 & z >= -1e-6] <- 0
  z[is.finite(z) & z > 1 & z <= 1 + 1e-6] <- 1
  x$total_cloud_cover <- z

  bounds <- tibble::tribble(
    ~variable, ~lower, ~upper,
    "u100_ms", -100, 100,
    "v100_ms", -100, 100,
    "u10_ms", -100, 100,
    "v10_ms", -100, 100,
    "dewpoint_c", -100, 70,
    "t2m_c", -100, 70,
    "boundary_layer_height_m", 0, 20000,
    "cloud_base_height_m", 0, 20000,
    "msl_pressure_hpa", 800, 1100,
    "skin_temperature_c", -100, 80,
    "surface_pressure_hpa", 500, 1100,
    "ssrd_j_m2", 0, 5.5e6,
    "strd_j_m2", 0, 3.0e6,
    "total_cloud_cover", 0, 1,
    "precipitation_mm", 0, 500,
    "fdir_j_m2", 0, 5.5e6,
    "grid_latitude", -90, 90,
    "grid_longitude", -180, 180
  )

  qc <- vector("list", nrow(bounds))
  for (i in seq_len(nrow(bounds))) {
    nm <- bounds$variable[i]
    before <- x[[nm]]
    bad <- is.finite(before) & (before < bounds$lower[i] | before > bounds$upper[i])
    qc[[i]] <- tibble::tibble(
      site = site,
      payload_format = payload_format,
      variable = nm,
      lower_bound = bounds$lower[i],
      upper_bound = bounds$upper[i],
      n_rows = length(before),
      n_finite_before = sum(is.finite(before)),
      n_out_of_range = sum(bad),
      raw_min = if (any(is.finite(before))) min(before[is.finite(before)]) else NA_real_,
      raw_max = if (any(is.finite(before))) max(before[is.finite(before)]) else NA_real_
    )
    x[[nm]][bad] <- NA_real_
  }

  bad_dewpoint <- is.finite(x$dewpoint_c) & is.finite(x$t2m_c) &
    x$dewpoint_c > x$t2m_c + 0.5
  if (any(bad_dewpoint)) x$dewpoint_c[bad_dewpoint] <- NA_real_

  bad_direct <- is.finite(x$fdir_j_m2) & is.finite(x$ssrd_j_m2) &
    x$fdir_j_m2 > x$ssrd_j_m2 + 1e-6

  x <- x |>
    dplyr::mutate(
      local_time = lubridate::with_tz(time_utc, tz),
      local_date = as.Date(time_utc, tz = tz),
      wind10_ms = sqrt(u10_ms^2 + v10_ms^2),
      wind100_ms = sqrt(u100_ms^2 + v100_ms^2),
      rh_pct = era5_rh_pct(t2m_c, dewpoint_c),
      vpd_kpa = era5_vpd_kpa(t2m_c, rh_pct),
      diffuse_j_m2 = dplyr::if_else(
        is.finite(ssrd_j_m2) & is.finite(fdir_j_m2) & !bad_direct,
        pmax(0, ssrd_j_m2 - fdir_j_m2), NA_real_
      ),
      direct_fraction = dplyr::if_else(
        is.finite(ssrd_j_m2) & ssrd_j_m2 > 0 &
          is.finite(fdir_j_m2) & !bad_direct,
        pmin(1, pmax(0, fdir_j_m2 / ssrd_j_m2)), NA_real_
      )
    )

  qc_extra <- tibble::tibble(
    site = site,
    payload_format = payload_format,
    variable = c("dewpoint_gt_t2m_plus_0.5C", "fdir_gt_ssrd"),
    lower_bound = NA_real_,
    upper_bound = NA_real_,
    n_rows = nrow(x),
    n_finite_before = NA_integer_,
    n_out_of_range = c(sum(bad_dewpoint), sum(bad_direct)),
    raw_min = NA_real_,
    raw_max = NA_real_
  )

  list(hourly = x, qc = dplyr::bind_rows(qc, qc_extra))
}

era5_safe_mean <- function(z) {
  z <- z[is.finite(z)]
  if (!length(z)) NA_real_ else mean(z)
}
era5_safe_sd <- function(z) {
  z <- z[is.finite(z)]
  if (length(z) < 2L) NA_real_ else stats::sd(z)
}
era5_safe_min <- function(z) {
  z <- z[is.finite(z)]
  if (!length(z)) NA_real_ else min(z)
}
era5_safe_max <- function(z) {
  z <- z[is.finite(z)]
  if (!length(z)) NA_real_ else max(z)
}
era5_safe_sum <- function(z) {
  z <- z[is.finite(z)]
  if (!length(z)) NA_real_ else sum(z)
}

era5_daily_summary <- function(hourly, tz) {
  era5_validate_tz(tz)
  inst <- hourly |>
    dplyr::mutate(Date = as.Date(time_utc, tz = tz)) |>
    dplyr::group_by(site, Date) |>
    dplyr::summarise(
      era5_hours_instantaneous = dplyr::n(),
      era5_grid_latitude = dplyr::first(grid_latitude),
      era5_grid_longitude = dplyr::first(grid_longitude),
      era5_t2m_mean_c = era5_safe_mean(t2m_c),
      era5_t2m_sd_c = era5_safe_sd(t2m_c),
      era5_t2m_min_c = era5_safe_min(t2m_c),
      era5_t2m_max_c = era5_safe_max(t2m_c),
      era5_t2m_range_c = era5_t2m_max_c - era5_t2m_min_c,
      era5_dewpoint_mean_c = era5_safe_mean(dewpoint_c),
      era5_rh_mean_pct = era5_safe_mean(rh_pct),
      era5_rh_min_pct = era5_safe_min(rh_pct),
      era5_rh_max_pct = era5_safe_max(rh_pct),
      era5_vpd_mean_kpa = era5_safe_mean(vpd_kpa),
      era5_vpd_max_kpa = era5_safe_max(vpd_kpa),
      era5_skin_temperature_mean_c = era5_safe_mean(skin_temperature_c),
      era5_total_cloud_cover_mean = era5_safe_mean(total_cloud_cover),
      era5_total_cloud_cover_sd = era5_safe_sd(total_cloud_cover),
      era5_boundary_layer_height_mean_m = era5_safe_mean(boundary_layer_height_m),
      era5_boundary_layer_height_max_m = era5_safe_max(boundary_layer_height_m),
      era5_cloud_base_height_mean_m = era5_safe_mean(cloud_base_height_m),
      era5_surface_pressure_mean_hpa = era5_safe_mean(surface_pressure_hpa),
      era5_msl_pressure_mean_hpa = era5_safe_mean(msl_pressure_hpa),
      era5_u10_mean_ms = era5_safe_mean(u10_ms),
      era5_v10_mean_ms = era5_safe_mean(v10_ms),
      era5_wind10_mean_ms = era5_safe_mean(wind10_ms),
      era5_wind10_max_ms = era5_safe_max(wind10_ms),
      era5_u100_mean_ms = era5_safe_mean(u100_ms),
      era5_v100_mean_ms = era5_safe_mean(v100_ms),
      era5_wind100_mean_ms = era5_safe_mean(wind100_ms),
      era5_wind100_max_ms = era5_safe_max(wind100_ms),
      .groups = "drop"
    )

  # ERA5 time-series accumulations are one-hour totals ending at valid_time.
  # Attribute each interval to the local date of its midpoint to avoid assigning
  # the midnight-ending hour to the following calendar day.
  accum <- hourly |>
    dplyr::mutate(
      interval_mid_utc = time_utc - 1800,
      Date = as.Date(interval_mid_utc, tz = tz)
    ) |>
    dplyr::group_by(site, Date) |>
    dplyr::summarise(
      era5_hours_accumulated = dplyr::n(),
      era5_ssrd_sum_mj_m2 = era5_safe_sum(ssrd_j_m2) / 1e6,
      era5_fdir_sum_mj_m2 = era5_safe_sum(fdir_j_m2) / 1e6,
      era5_diffuse_sum_mj_m2 = era5_safe_sum(diffuse_j_m2) / 1e6,
      era5_strd_sum_mj_m2 = era5_safe_sum(strd_j_m2) / 1e6,
      era5_precipitation_sum_mm = era5_safe_sum(precipitation_mm),
      era5_wet_hours_0.1mm = sum(is.finite(precipitation_mm) & precipitation_mm >= 0.1),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      era5_direct_fraction = dplyr::if_else(
        is.finite(era5_ssrd_sum_mj_m2) & era5_ssrd_sum_mj_m2 > 0 &
          is.finite(era5_fdir_sum_mj_m2),
        pmin(1, pmax(0, era5_fdir_sum_mj_m2 / era5_ssrd_sum_mj_m2)),
        NA_real_
      ),
      era5_ssrd_daily_mean_w_m2 = era5_ssrd_sum_mj_m2 * 1e6 / (24 * 3600),
      era5_fdir_daily_mean_w_m2 = era5_fdir_sum_mj_m2 * 1e6 / (24 * 3600)
    )

  dplyr::full_join(inst, accum, by = c("site", "Date")) |>
    dplyr::mutate(
      era5_complete_local_day =
        era5_hours_instantaneous >= 24L & era5_hours_accumulated >= 24L
    ) |>
    dplyr::arrange(site, Date)
}

era5_pchip_slopes <- function(x, y) {
  n <- length(x)
  if (n < 2L) return(rep(NA_real_, n))
  h <- diff(x)
  delta <- diff(y) / h
  if (n == 2L) return(c(delta[1], delta[1]))

  d <- numeric(n)
  for (k in 2:(n - 1L)) {
    if (!is.finite(delta[k - 1L]) || !is.finite(delta[k]) ||
        delta[k - 1L] == 0 || delta[k] == 0 ||
        sign(delta[k - 1L]) != sign(delta[k])) {
      d[k] <- 0
    } else {
      w1 <- 2 * h[k] + h[k - 1L]
      w2 <- h[k] + 2 * h[k - 1L]
      d[k] <- (w1 + w2) / (w1 / delta[k - 1L] + w2 / delta[k])
    }
  }

  left <- ((2 * h[1] + h[2]) * delta[1] - h[1] * delta[2]) / (h[1] + h[2])
  if (sign(left) != sign(delta[1])) left <- 0
  if (sign(delta[1]) != sign(delta[2]) && abs(left) > abs(3 * delta[1])) {
    left <- 3 * delta[1]
  }
  d[1] <- left

  hn <- h[n - 1L]
  hprev <- h[n - 2L]
  dn <- delta[n - 1L]
  dprev <- delta[n - 2L]
  right <- ((2 * hn + hprev) * dn - hn * dprev) / (hn + hprev)
  if (sign(right) != sign(dn)) right <- 0
  if (sign(dn) != sign(dprev) && abs(right) > abs(3 * dn)) right <- 3 * dn
  d[n] <- right
  d
}

era5_pchip_eval_run <- function(x, y, xout) {
  if (length(x) < 2L) return(rep(NA_real_, length(xout)))
  d <- era5_pchip_slopes(x, y)
  idx <- findInterval(xout, x, all.inside = TRUE)
  idx[idx >= length(x)] <- length(x) - 1L
  h <- x[idx + 1L] - x[idx]
  t <- (xout - x[idx]) / h
  h00 <- 2 * t^3 - 3 * t^2 + 1
  h10 <- t^3 - 2 * t^2 + t
  h01 <- -2 * t^3 + 3 * t^2
  h11 <- t^3 - t^2
  h00 * y[idx] + h10 * h * d[idx] +
    h01 * y[idx + 1L] + h11 * h * d[idx + 1L]
}

era5_pchip <- function(time, y, out_time, max_gap_s = 5400) {
  x <- as.numeric(time)
  xo <- as.numeric(out_time)
  keep <- is.finite(x) & is.finite(y)
  x <- x[keep]
  y <- y[keep]
  if (!length(x)) return(rep(NA_real_, length(xo)))
  ord <- order(x)
  x <- x[ord]
  y <- y[ord]
  keep_unique <- !duplicated(x)
  x <- x[keep_unique]
  y <- y[keep_unique]

  out <- rep(NA_real_, length(xo))
  if (length(x) == 1L) {
    out[abs(xo - x[1]) < 1e-6] <- y[1]
    return(out)
  }

  segment <- cumsum(c(1L, diff(x) > max_gap_s))
  for (g in unique(segment)) {
    xx <- x[segment == g]
    yy <- y[segment == g]
    if (length(xx) == 1L) {
      out[abs(xo - xx[1]) < 1e-6] <- yy[1]
      next
    }
    use <- xo >= min(xx) & xo <= max(xx)
    if (any(use)) out[use] <- era5_pchip_eval_run(xx, yy, xo[use])
  }
  out
}

era5_piecewise_hourly_rate <- function(end_time, hourly_value, out_time) {
  ends <- as.numeric(end_time)
  xo <- as.numeric(out_time)
  ord <- order(ends)
  ends <- ends[ord]
  values <- hourly_value[ord]

  previous <- findInterval(xo, ends)
  idx <- previous + 1L
  exact <- previous > 0L & abs(xo - ends[pmax(previous, 1L)]) < 1e-6
  idx[exact] <- previous[exact]

  out <- rep(NA_real_, length(xo))
  valid <- idx >= 1L & idx <= length(ends)
  valid[valid] <- xo[valid] > (ends[idx[valid]] - 3600) &
    xo[valid] <= ends[idx[valid]]
  out[valid] <- values[idx[valid]]
  out
}

era5_interpolate_1min <- function(hourly, tz) {
  era5_validate_tz(tz)
  out_time <- seq(
    from = min(hourly$time_utc, na.rm = TRUE),
    to = max(hourly$time_utc, na.rm = TRUE),
    by = 60
  )
  midpoint_time <- hourly$time_utc - 1800

  out <- tibble::tibble(
    site = dplyr::first(hourly$site),
    time_utc = out_time,
    timezone = tz,
    grid_latitude = dplyr::first(hourly$grid_latitude),
    grid_longitude = dplyr::first(hourly$grid_longitude),
    t2m_c = era5_pchip(hourly$time_utc, hourly$t2m_c, out_time),
    dewpoint_c = era5_pchip(hourly$time_utc, hourly$dewpoint_c, out_time),
    skin_temperature_c = era5_pchip(hourly$time_utc, hourly$skin_temperature_c, out_time),
    total_cloud_cover = era5_pchip(hourly$time_utc, hourly$total_cloud_cover, out_time),
    boundary_layer_height_m = era5_pchip(hourly$time_utc, hourly$boundary_layer_height_m, out_time),
    cloud_base_height_m = era5_pchip(hourly$time_utc, hourly$cloud_base_height_m, out_time),
    surface_pressure_hpa = era5_pchip(hourly$time_utc, hourly$surface_pressure_hpa, out_time),
    msl_pressure_hpa = era5_pchip(hourly$time_utc, hourly$msl_pressure_hpa, out_time),
    u10_ms = era5_pchip(hourly$time_utc, hourly$u10_ms, out_time),
    v10_ms = era5_pchip(hourly$time_utc, hourly$v10_ms, out_time),
    u100_ms = era5_pchip(hourly$time_utc, hourly$u100_ms, out_time),
    v100_ms = era5_pchip(hourly$time_utc, hourly$v100_ms, out_time),
    ssrd_w_m2 = era5_pchip(midpoint_time, hourly$ssrd_j_m2 / 3600, out_time),
    fdir_w_m2 = era5_pchip(midpoint_time, hourly$fdir_j_m2 / 3600, out_time),
    strd_w_m2 = era5_pchip(midpoint_time, hourly$strd_j_m2 / 3600, out_time),
    precipitation_rate_mm_h = era5_piecewise_hourly_rate(
      hourly$time_utc, hourly$precipitation_mm, out_time
    )
  ) |>
    dplyr::mutate(
      total_cloud_cover = pmin(1, pmax(0, total_cloud_cover)),
      boundary_layer_height_m = pmax(0, boundary_layer_height_m),
      cloud_base_height_m = pmax(0, cloud_base_height_m),
      dewpoint_c = pmin(dewpoint_c, t2m_c),
      ssrd_w_m2 = pmax(0, ssrd_w_m2),
      fdir_w_m2 = pmax(0, fdir_w_m2),
      strd_w_m2 = pmax(0, strd_w_m2),
      precipitation_rate_mm_h = pmax(0, precipitation_rate_mm_h),
      wind10_ms = sqrt(u10_ms^2 + v10_ms^2),
      wind100_ms = sqrt(u100_ms^2 + v100_ms^2),
      rh_pct = era5_rh_pct(t2m_c, dewpoint_c),
      vpd_kpa = era5_vpd_kpa(t2m_c, rh_pct),
      diffuse_w_m2 = dplyr::if_else(
        is.finite(ssrd_w_m2) & is.finite(fdir_w_m2) & fdir_w_m2 <= ssrd_w_m2 + 1e-6,
        pmax(0, ssrd_w_m2 - fdir_w_m2), NA_real_
      ),
      direct_fraction = dplyr::if_else(
        is.finite(ssrd_w_m2) & ssrd_w_m2 >= 1 &
          is.finite(fdir_w_m2) & fdir_w_m2 <= ssrd_w_m2 + 1e-6,
        pmin(1, pmax(0, fdir_w_m2 / ssrd_w_m2)), NA_real_
      ),
      local_datetime = format(time_utc, "%Y-%m-%dT%H:%M:%S%z", tz = tz),
      Date = as.Date(time_utc, tz = tz),
      source_exact_hour = lubridate::minute(time_utc) == 0L &
        lubridate::second(time_utc) == 0L
    ) |>
    dplyr::select(
      site, time_utc, local_datetime, Date, timezone,
      grid_latitude, grid_longitude, source_exact_hour,
      t2m_c, dewpoint_c, rh_pct, vpd_kpa, skin_temperature_c,
      total_cloud_cover, boundary_layer_height_m, cloud_base_height_m,
      surface_pressure_hpa, msl_pressure_hpa,
      u10_ms, v10_ms, wind10_ms, u100_ms, v100_ms, wind100_ms,
      ssrd_w_m2, fdir_w_m2, diffuse_w_m2, direct_fraction,
      strd_w_m2, precipitation_rate_mm_h
    )
  out
}

era5_build_site <- function(site, tz) {
  era5_validate_tz(tz)
  path <- file.path("data", "raw", "era5", paste0(site, ".csv"))
  if (!file.exists(path)) stop("Missing ERA5 site file: ", path)
  payload <- era5_read_payload(path)
  cleaned <- era5_qc_hourly(payload$data, site, tz, payload$payload_format)
  hourly <- cleaned$hourly
  if (nrow(hourly) < 48L) stop("ERA5 file too short for ", site, ": ", nrow(hourly), " rows")
  if (any(diff(hourly$time_utc) <= 0)) stop("ERA5 time is not strictly increasing for ", site)

  list(
    hourly = hourly,
    daily = era5_daily_summary(hourly, tz),
    minute = era5_interpolate_1min(hourly, tz),
    qc = cleaned$qc
  )
}
