# Helpers for building the configuration-level metric cube and daily context artifact.

core_site_metadata <- function() {
  tibble::tribble(
    ~site, ~city, ~country, ~timezone, ~latitude, ~longitude,
    "RISE", "Borås", "Sweden", "Europe/Stockholm", 57.715675, 12.890871,
    "FUSPCEU", "Madrid", "Spain", "Europe/Madrid", 40.4165, -3.70256,
    "BAUA", "Dortmund", "Germany", "Europe/Berlin", 51.498204, 7.416708,
    "TUM", "Munich", "Germany", "Europe/Berlin", 48.1333, 11.5667,
    "MPI", "Tübingen", "Germany", "Europe/Berlin", 48.5216, 9.0576,
    "THUAS", "Delft", "The Netherlands", "Europe/Amsterdam", 52.0116, 4.3571,
    "IZTECH", "Izmir", "Turkey", "Europe/Istanbul", 38.32, 26.63,
    "KNUST", "Kumasi", "Ghana", "Africa/Accra", 6.6750074282377385, -1.572643823555129,
    "UCR", "San Pedro, San José", "Costa Rica", "America/Costa_Rica", 9.9372, -84.0509
  )
}

core_build_state_intervals <- function(sleep, wear) {
  sleep_adj <- sleep |>
    dplyr::select(Id, sleepprep, wake) |>
    dplyr::group_by(Id) |>
    tidyr::pivot_longer(-Id, names_to = "sleep", values_to = "Datetime") |>
    LightLogR::sc2interval(Statechange.colname = sleep, starting.state = "wake") |>
    LightLogR::sleep_int2Brown(
      sleep.state = "sleepprep", Brown.day = "wake",
      Brown.evening = "pre-sleep", Brown.night = "sleep"
    ) |>
    dplyr::mutate(sleep = dplyr::case_when(
      is.na(sleep) & State.Brown == "pre-sleep" ~ "wake",
      .default = sleep
    ))
  wear_adj <- wear |> dplyr::select(Id, start, end, wear = state)
  list(sleep = sleep_adj, wear = wear_adj)
}

core_annotate_filter <- function(light, state_intervals, measurement_cols) {
  out <- light |>
    LightLogR::add_states(state_intervals$sleep, start = Interval, end = Interval) |>
    LightLogR::add_states(state_intervals$wear)
  for (nm in measurement_cols) {
    out[[nm]] <- dplyr::case_when(
      out$wear == "off" & (out$State.Brown != "sleep" | is.na(out$State.Brown)) ~ NA_real_,
      out[[nm]] >= 100000 ~ NA_real_,
      TRUE ~ as.numeric(out[[nm]])
    )
  }
  out
}

core_apply_common_mask <- function(x, required_cols, measurement_cols) {
  invalid <- Reduce(`|`, lapply(required_cols, function(nm) is.na(x[[nm]])))
  for (nm in measurement_cols) x[[nm]][invalid] <- NA_real_
  x
}

core_complete_days <- function(x) {
  x |>
    dplyr::group_by(site, Id) |>
    LightLogR::cut_Datetime(unit = "1 hour", group_by = TRUE, type = "floor") |>
    LightLogR::remove_partial_data(MEDI_eye, threshold.missing = .5) |>
    dplyr::ungroup(Datetime.rounded) |>
    dplyr::select(-Datetime.rounded) |>
    LightLogR::add_Date_col(group.by = TRUE) |>
    LightLogR::gap_handler(full.days = TRUE) |>
    LightLogR::remove_partial_data(MEDI_eye, threshold.missing = .2) |>
    dplyr::ungroup(Date)
}

core_align_pair <- function(eye0, candidate0, position) {
  base <- eye0 |> dplyr::select(Id, Datetime, MEDI, LIGHT)
  if (identical(position, "chest")) {
    return(base |>
      LightLogR::data2reference(candidate0, Reference.column = MEDI_chest) |>
      LightLogR::data2reference(candidate0, Data.column = LIGHT, Reference.column = LIGHT_chest) |>
      dplyr::rename(MEDI_eye = MEDI, LIGHT_eye = LIGHT))
  }
  base |>
    LightLogR::data2reference(candidate0, Reference.column = MEDI_wrist) |>
    LightLogR::data2reference(candidate0, Data.column = LIGHT, Reference.column = LIGHT_wrist) |>
    dplyr::rename(MEDI_eye = MEDI, LIGHT_eye = LIGHT)
}

core_prepare_support <- function(site, support_id) {
  parts <- strsplit(support_id, "_", fixed = TRUE)[[1]]
  is_full <- identical(tail(parts, 1), "full")
  if (support_id %in% c("eye_medi", "eye_full")) {
    eye0 <- load_raw_file(raw_data_path(site, "light_glasses"), "light_glasses")
    sleep <- load_raw_file(raw_data_path(site, "sleepdiaries"), "sleepdiaries")
    wear <- load_raw_file(raw_data_path(site, "wearlog"), "wearlog")
    states <- core_build_state_intervals(sleep, wear)
    x <- eye0 |>
      dplyr::transmute(site = site, Id, Datetime, MEDI_eye = MEDI, LIGHT_eye = LIGHT) |>
      core_annotate_filter(states, c("MEDI_eye", "LIGHT_eye"))
    if (is_full) x <- core_apply_common_mask(x, c("MEDI_eye", "LIGHT_eye"), c("MEDI_eye", "LIGHT_eye"))
    return(core_complete_days(x) |>
             dplyr::distinct(site, Id, Datetime, .keep_all = TRUE) |>
             dplyr::mutate(support_id = support_id, .before = 1))
  }

  position <- if (grepl("chest", support_id, fixed = TRUE)) "chest" else "wrist"
  if (identical(site, "MPI")) return(NULL)
  eye0 <- load_raw_file(raw_data_path(site, "light_glasses"), "light_glasses")
  cand0 <- load_raw_file(raw_data_path(site, paste0("light_", position)), paste0("light_", position))
  sleep <- load_raw_file(raw_data_path(site, "sleepdiaries"), "sleepdiaries")
  wear <- load_raw_file(raw_data_path(site, "wearlog"), "wearlog")
  states <- core_build_state_intervals(sleep, wear)
  x <- core_align_pair(eye0, cand0, position) |> dplyr::mutate(site = site, .before = 1)
  med_nm <- paste0("MEDI_", position)
  light_nm <- paste0("LIGHT_", position)
  measure_cols <- c("MEDI_eye", "LIGHT_eye", med_nm, light_nm)
  x <- core_annotate_filter(x, states, measure_cols)
  required <- if (is_full) measure_cols else c("MEDI_eye", med_nm)
  x <- core_apply_common_mask(x, required, measure_cols)
  core_complete_days(x) |>
    dplyr::distinct(site, Id, Datetime, .keep_all = TRUE) |>
    dplyr::mutate(support_id = support_id, .before = 1)
}

core_support_grid <- function() {
  tidyr::crossing(
    site = melidos_sites(),
    support_id = c("eye_medi", "eye_full", "eye_chest_medi", "eye_chest_full", "eye_wrist_medi", "eye_wrist_full")
  ) |>
    dplyr::filter(!(site == "MPI" & grepl("eye_(chest|wrist)", support_id)))
}

core_config_grid <- function(support_id) {
  is_pair <- grepl("eye_(chest|wrist)", support_id)
  position <- if (grepl("chest", support_id, fixed = TRUE)) "chest" else if (grepl("wrist", support_id, fixed = TRUE)) "wrist" else NA_character_
  placements <- if (is_pair) c("eye", position) else "eye"
  opticals <- if (grepl("_full$", support_id)) c("MEDI", "LIGHT") else "MEDI"
  tidyr::crossing(
    placement = placements,
    optical = opticals,
    resolution_s = c(10L, 30L, 60L, 300L, 900L, 1800L)
  ) |>
    dplyr::mutate(config_id = paste(placement, optical, paste0(resolution_s, "s"), sep = "__"))
}

core_make_series <- function(support, placement, optical, resolution_s) {
  med_nm <- paste0("MEDI_", placement)
  light_nm <- paste0("LIGHT_", placement)
  if (!med_nm %in% names(support)) stop("Missing channel: ", med_nm)
  if (!light_nm %in% names(support)) stop("Missing channel: ", light_nm)

  x <- support |>
    dplyr::transmute(
      site, Id, Date, Datetime,
      MEDI = if (optical == "MEDI") .data[[med_nm]] else .data[[light_nm]],
      LIGHT = if (optical == "MEDI") .data[[light_nm]] else NA_real_
    )
  if (resolution_s == 10L) return(x)

  tz <- lubridate::tz(x$Datetime)
  if (is.null(tz) || !length(tz) || is.na(tz) || !nzchar(tz)) tz <- "UTC"
  x |>
    dplyr::mutate(bin = as.POSIXct(
      floor(as.numeric(Datetime) / resolution_s) * resolution_s,
      origin = "1970-01-01", tz = tz
    )) |>
    dplyr::group_by(site, Id, Date, bin) |>
    dplyr::summarise(
      MEDI = if (all(is.na(MEDI))) NA_real_ else mean(MEDI, na.rm = TRUE),
      LIGHT = if (all(is.na(LIGHT))) NA_real_ else mean(LIGHT, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::rename(Datetime = bin)
}

core_compute_support_metrics <- function(support_path) {
  support <- readRDS(support_path)
  support_id <- unique(support$support_id)
  if (length(support_id) != 1L) stop("Expected one support_id in ", support_path)
  cfgs <- core_config_grid(support_id)
  n_days <- support |> dplyr::distinct(site, Id, Date) |> dplyr::count(site, Id, name = "n_days_observed")

  blocks <- vector("list", nrow(cfgs))
  for (i in seq_len(nrow(cfgs))) {
    cfg <- cfgs[i, ]
    series <- core_make_series(support, cfg$placement, cfg$optical, cfg$resolution_s)
    series <- series |> dplyr::mutate(configuration = cfg$config_id)
    m <- rq1_all_metrics(
      series,
      include_spectral = identical(cfg$optical, "MEDI"),
      include_pulses = cfg$resolution_s < 300L
    ) |>
      dplyr::left_join(n_days, by = c("site", "Id")) |>
      dplyr::mutate(
        support_id = support_id,
        placement = cfg$placement,
        optical = cfg$optical,
        resolution_s = cfg$resolution_s,
        config_id = cfg$config_id,
        analysis_unit_type = dplyr::if_else(is.na(Date), "participant_multiday", "participant_day"),
        analysis_unit_id = dplyr::if_else(
          is.na(Date),
          paste(support_id, site, Id, "multiday", sep = "|"),
          paste(support_id, site, Id, as.character(Date), sep = "|")
        ),
        n_days = dplyr::if_else(is.na(Date), n_days_observed, 1L)
      ) |>
      dplyr::select(
        support_id, site, Id, analysis_unit_type, analysis_unit_id, Date, n_days,
        placement, optical, resolution_s, config_id, metric, value
      )
    blocks[[i]] <- m
  }
  dplyr::bind_rows(blocks)
}

core_support_quality <- function(support) {
  support |>
    dplyr::group_by(support_id, site, Id, Date) |>
    dplyr::group_modify(~{
      missing <- is.na(.x$MEDI_eye)
      rr <- rle(missing)
      miss_lengths <- rr$lengths[rr$values]
      tibble::tibble(
        expected_epochs = nrow(.x),
        valid_epochs = sum(!missing),
        valid_fraction = mean(!missing),
        n_missing_blocks = length(miss_lengths),
        largest_missing_gap_s = if (length(miss_lengths)) max(miss_lengths) * 10 else 0,
        first_valid_time = if (all(missing)) as.POSIXct(NA) else min(.x$Datetime[!missing], na.rm = TRUE),
        last_valid_time = if (all(missing)) as.POSIXct(NA) else max(.x$Datetime[!missing], na.rm = TRUE)
      )
    }) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      analysis_unit_type = "participant_day",
      analysis_unit_id = paste(support_id, site, Id, as.character(Date), sep = "|")
    )
}

core_read_era5_daily <- function(site, timezone) {
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
      tibble::tibble(
        era5_hours = sum(!is.na(d$local_time)),
        era5_grid_latitude = if ("latitude" %in% names(d)) dplyr::first(d$latitude) else NA_real_,
        era5_grid_longitude = if ("longitude" %in% names(d)) dplyr::first(d$longitude) else NA_real_,
        era5_t2m_mean_c = mean(t2m, na.rm = TRUE),
        era5_t2m_min_c = min(t2m, na.rm = TRUE),
        era5_t2m_max_c = max(t2m, na.rm = TRUE),
        era5_dewpoint_mean_c = mean(d2m, na.rm = TRUE),
        era5_skin_temperature_mean_c = mean(skin, na.rm = TRUE),
        era5_total_cloud_cover_mean = mean(tcc, na.rm = TRUE),
        era5_boundary_layer_height_mean_m = mean(blh, na.rm = TRUE),
        era5_boundary_layer_height_max_m = max(blh, na.rm = TRUE),
        era5_cloud_base_height_mean_m = mean(cbh, na.rm = TRUE),
        era5_surface_pressure_mean_hpa = mean(sp, na.rm = TRUE),
        era5_msl_pressure_mean_hpa = mean(msl, na.rm = TRUE),
        era5_u10_mean_ms = mean(u10, na.rm = TRUE),
        era5_v10_mean_ms = mean(v10, na.rm = TRUE),
        era5_wind10_mean_ms = mean(sqrt(u10^2 + v10^2), na.rm = TRUE),
        era5_wind10_max_ms = max(sqrt(u10^2 + v10^2), na.rm = TRUE),
        era5_u100_mean_ms = mean(u100, na.rm = TRUE),
        era5_v100_mean_ms = mean(v100, na.rm = TRUE),
        era5_wind100_mean_ms = mean(sqrt(u100^2 + v100^2), na.rm = TRUE),
        era5_wind100_max_ms = max(sqrt(u100^2 + v100^2), na.rm = TRUE),
        era5_ssrd_sum_mj_m2 = sum(ssrd, na.rm = TRUE) / 1e6,
        era5_fdir_sum_mj_m2 = sum(fdir, na.rm = TRUE) / 1e6,
        era5_strd_sum_mj_m2 = sum(strd, na.rm = TRUE) / 1e6,
        era5_precipitation_sum_mm = sum(tp, na.rm = TRUE) * 1000
      )
    }) |>
    dplyr::ungroup() |>
    dplyr::mutate(site = site, .before = 1) |>
    dplyr::mutate(dplyr::across(where(is.numeric), ~ifelse(is.infinite(.x) | is.nan(.x), NA_real_, .x)))
}

core_expand_metric_availability <- function(emitted, metric_types) {
  daily_types <- metric_types |> dplyr::filter(!metric %in% c("interdaily_stability", "intradaily_variability"))
  multi_types <- metric_types |> dplyr::filter(metric %in% c("interdaily_stability", "intradaily_variability"))
  units <- emitted |>
    dplyr::distinct(
      support_id, site, Id, analysis_unit_type, analysis_unit_id, Date, n_days,
      placement, optical, resolution_s, config_id
    )
  daily_units <- units |> dplyr::filter(analysis_unit_type == "participant_day")
  multi_units <- units |> dplyr::filter(analysis_unit_type == "participant_multiday")
  full <- dplyr::bind_rows(
    tidyr::crossing(daily_units, daily_types),
    tidyr::crossing(multi_units, multi_types)
  ) |>
    dplyr::left_join(
      emitted,
      by = c(
        "support_id", "site", "Id", "analysis_unit_type", "analysis_unit_id", "Date", "n_days",
        "placement", "optical", "resolution_s", "config_id", "metric"
      )
    ) |>
    dplyr::mutate(
      representation_available = !(
        optical == "LIGHT" & metric %in% c("MDER", "nvRD")
      ) & !(
        resolution_s >= 300L & stringr::str_detect(metric, "pulses_above")
      ),
      available = representation_available & is.finite(value),
      unavailable_reason = dplyr::case_when(
        optical == "LIGHT" & metric %in% c("MDER", "nvRD") ~ "requires MEDI and LIGHT simultaneously",
        resolution_s >= 300L & stringr::str_detect(metric, "pulses_above") ~ "pulse operator unavailable at this epoch",
        !is.finite(value) ~ "metric undefined or missing on this analysis unit",
        TRUE ~ NA_character_
      ),
      is_reference_config = placement == "eye" & optical == "MEDI" & resolution_s == 10L
    ) |>
    dplyr::select(
      support_id, site, Id, analysis_unit_type, analysis_unit_id, Date, n_days,
      placement, optical, resolution_s, config_id,
      metric, metric_class, value, available, unavailable_reason, is_reference_config
    )
  full
}
