# Configuration-level daily context used by RQ2 and by downstream duration
# reconstruction of IS/IV without returning to the high-resolution source.

core_config_daily_context <- function(support_path) {
  support <- readRDS(support_path)
  support_id <- unique(support$support_id)
  if (length(support_id) != 1L) stop("Expected one support_id in ", support_path)
  cfgs <- core_config_grid(support_id)
  blocks <- vector("list", nrow(cfgs))

  for (i in seq_len(nrow(cfgs))) {
    cfg <- cfgs[i, ]
    series <- core_make_series(support, cfg$placement, cfg$optical, cfg$resolution_s)

    # Exact basis required by LightLogR IS/IV: both functions first transform the
    # configured series and then operate on mean hourly values. Persisting these
    # 24 daily hourly means makes arbitrary later day-subset IS/IV calculations
    # possible without re-reading 10-s data.
    hourly <- series |>
      dplyr::transmute(
        site, Id, Date, Datetime,
        log_light = LightLogR::log_zero_inflated(MEDI)
      ) |>
      dplyr::filter(is.finite(log_light)) |>
      dplyr::mutate(hour = lubridate::hour(Datetime)) |>
      dplyr::group_by(site, Id, Date, hour) |>
      dplyr::summarise(hourly_log_light = mean(log_light), .groups = "drop") |>
      dplyr::group_by(site, Id, Date) |>
      tidyr::complete(hour = 0:23) |>
      dplyr::ungroup() |>
      dplyr::mutate(hour_label = sprintf("%02d", hour)) |>
      dplyr::select(-hour) |>
      tidyr::pivot_wider(
        names_from = hour_label, values_from = hourly_log_light,
        names_prefix = "isiv_h"
      )

    quality <- series |>
      dplyr::group_by(site, Id, Date) |>
      dplyr::group_modify(~{
        missing <- is.na(.x$MEDI)
        rr <- rle(missing)
        miss_lengths <- rr$lengths[rr$values]
        tibble::tibble(
          expected_values = nrow(.x),
          valid_values = sum(!missing),
          valid_fraction = mean(!missing),
          n_missing_blocks = length(miss_lengths),
          largest_missing_gap_s = if (length(miss_lengths)) max(miss_lengths) * cfg$resolution_s else 0,
          first_valid_time = if (all(missing)) as.POSIXct(NA) else min(.x$Datetime[!missing], na.rm = TRUE),
          last_valid_time = if (all(missing)) as.POSIXct(NA) else max(.x$Datetime[!missing], na.rm = TRUE)
        )
      }) |>
      dplyr::ungroup() |>
      dplyr::left_join(hourly, by = c("site", "Id", "Date")) |>
      dplyr::mutate(
        support_id = support_id,
        placement = cfg$placement,
        optical = cfg$optical,
        resolution_s = cfg$resolution_s,
        config_id = cfg$config_id,
        analysis_unit_type = "participant_day",
        analysis_unit_id = paste(support_id, site, Id, as.character(Date), sep = "|")
      ) |>
      dplyr::select(
        support_id, site, Id, analysis_unit_type, analysis_unit_id, Date,
        placement, optical, resolution_s, config_id,
        expected_values, valid_values, valid_fraction, n_missing_blocks,
        largest_missing_gap_s, first_valid_time, last_valid_time,
        dplyr::starts_with("isiv_h")
      )
    blocks[[i]] <- quality
  }
  dplyr::bind_rows(blocks)
}

core_finalize_metric_cube <- function(emitted, metric_types) {
  out <- core_expand_metric_availability(emitted, metric_types)

  # MDER and nvRD intrinsically use both optical channels. On pairwise *_medi
  # supports only MEDI was required to be common across placements, so these two
  # representations must use the corresponding *_full lattice instead.
  needs_full_pair <- grepl("^eye_(chest|wrist)_medi$", out$support_id) &
    out$metric %in% c("MDER", "nvRD")
  out$available[needs_full_pair] <- FALSE
  out$unavailable_reason[needs_full_pair] <-
    "dual-channel placement metric requires corresponding *_full support"
  out
}

# Safe ERA5 daily summaries. Missing variables/days remain NA; they are never
# converted to zero by sum(..., na.rm = TRUE).
core_read_era5_daily_safe <- function(site, timezone) {
  path <- file.path("data", "raw", "era5", paste0(site, ".csv"))
  if (!file.exists(path)) return(NULL)
  x <- readr::read_csv(path, show_col_types = FALSE, progress = FALSE)
  time_col <- intersect(c("time", "valid_time", "datetime", "Datetime", "date"), names(x))[1]
  if (is.na(time_col)) stop("No recognizable ERA5 time column in ", path)
  dt <- suppressWarnings(lubridate::ymd_hms(x[[time_col]], tz = "UTC", quiet = TRUE))
  if (all(is.na(dt))) dt <- suppressWarnings(as.POSIXct(x[[time_col]], tz = "UTC"))
  if (all(is.na(dt))) stop("Could not parse ERA5 time column in ", path)
  x$local_time <- lubridate::with_tz(dt, timezone)
  x$Date <- as.Date(x$local_time)

  getv <- function(df, name) if (name %in% names(df)) as.numeric(df[[name]]) else rep(NA_real_, nrow(df))
  safe_mean <- function(z) if (all(!is.finite(z))) NA_real_ else mean(z[is.finite(z)])
  safe_min <- function(z) if (all(!is.finite(z))) NA_real_ else min(z[is.finite(z)])
  safe_max <- function(z) if (all(!is.finite(z))) NA_real_ else max(z[is.finite(z)])
  safe_sum <- function(z) if (all(!is.finite(z))) NA_real_ else sum(z[is.finite(z)])

  x |>
    dplyr::group_by(Date) |>
    dplyr::group_modify(~{
      d <- .x
      u10 <- getv(d, "10m_u_component_of_wind"); v10 <- getv(d, "10m_v_component_of_wind")
      u100 <- getv(d, "100m_u_component_of_wind"); v100 <- getv(d, "100m_v_component_of_wind")
      t2m <- getv(d, "2m_temperature") - 273.15
      d2m <- getv(d, "2m_dewpoint_temperature") - 273.15
      skin <- getv(d, "skin_temperature") - 273.15
      tcc <- getv(d, "total_cloud_cover")
      blh <- getv(d, "boundary_layer_height")
      cbh <- getv(d, "cloud_base_height")
      sp <- getv(d, "surface_pressure") / 100
      msl <- getv(d, "mean_sea_level_pressure") / 100
      ssrd <- getv(d, "surface_solar_radiation_downwards")
      fdir <- getv(d, "total_sky_direct_solar_radiation_at_surface")
      strd <- getv(d, "surface_thermal_radiation_downwards")
      tp <- getv(d, "total_precipitation")
      ssrd_sum <- safe_sum(ssrd) / 1e6
      fdir_sum <- safe_sum(fdir) / 1e6
      tibble::tibble(
        era5_hours = sum(!is.na(d$local_time)),
        era5_grid_latitude = if ("latitude" %in% names(d)) dplyr::first(d$latitude) else NA_real_,
        era5_grid_longitude = if ("longitude" %in% names(d)) dplyr::first(d$longitude) else NA_real_,
        era5_t2m_mean_c = safe_mean(t2m),
        era5_t2m_min_c = safe_min(t2m),
        era5_t2m_max_c = safe_max(t2m),
        era5_dewpoint_mean_c = safe_mean(d2m),
        era5_skin_temperature_mean_c = safe_mean(skin),
        era5_total_cloud_cover_mean = safe_mean(tcc),
        era5_boundary_layer_height_mean_m = safe_mean(blh),
        era5_boundary_layer_height_max_m = safe_max(blh),
        era5_cloud_base_height_mean_m = safe_mean(cbh),
        era5_surface_pressure_mean_hpa = safe_mean(sp),
        era5_msl_pressure_mean_hpa = safe_mean(msl),
        era5_u10_mean_ms = safe_mean(u10),
        era5_v10_mean_ms = safe_mean(v10),
        era5_wind10_mean_ms = safe_mean(sqrt(u10^2 + v10^2)),
        era5_wind10_max_ms = safe_max(sqrt(u10^2 + v10^2)),
        era5_u100_mean_ms = safe_mean(u100),
        era5_v100_mean_ms = safe_mean(v100),
        era5_wind100_mean_ms = safe_mean(sqrt(u100^2 + v100^2)),
        era5_wind100_max_ms = safe_max(sqrt(u100^2 + v100^2)),
        era5_ssrd_sum_mj_m2 = ssrd_sum,
        era5_fdir_sum_mj_m2 = fdir_sum,
        era5_diffuse_sum_mj_m2 = if (is.finite(ssrd_sum) && is.finite(fdir_sum)) ssrd_sum - fdir_sum else NA_real_,
        era5_direct_fraction = if (is.finite(ssrd_sum) && ssrd_sum > 0 && is.finite(fdir_sum)) fdir_sum / ssrd_sum else NA_real_,
        era5_strd_sum_mj_m2 = safe_sum(strd) / 1e6,
        era5_precipitation_sum_mm = safe_sum(tp) * 1000
      )
    }) |>
    dplyr::ungroup() |>
    dplyr::mutate(site = site, .before = 1)
}
