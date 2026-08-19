source("scripts/utils/melidos_io.R")
source("scripts/utils/core_artifacts.R")
source("scripts/utils/weather_era5.R")
suppressPackageStartupMessages({
  library(tidyverse)
})

site_meta <- core_site_metadata()
inv_path <- "logs/data_inventory.csv"
if (!file.exists(inv_path)) stop("Missing ", inv_path, "; run scripts/02_inventory.R first")
inv <- read.csv(inv_path, stringsAsFactors = FALSE)
eye_inv <- inv |>
  filter(modality == "light_glasses") |>
  transmute(
    site,
    light_date_min = as.Date(substr(date_min, 1, 10)),
    light_date_max = as.Date(substr(date_max, 1, 10))
  )

required_aliases <- list(
  u100 = c("u100", "100m_u_component_of_wind"),
  v100 = c("v100", "100m_v_component_of_wind"),
  u10 = c("u10", "10m_u_component_of_wind"),
  v10 = c("v10", "10m_v_component_of_wind"),
  d2m = c("d2m", "2m_dewpoint_temperature"),
  t2m = c("t2m", "2m_temperature"),
  blh = c("blh", "boundary_layer_height"),
  cbh = c("cbh", "cloud_base_height"),
  msl = c("msl", "mean_sea_level_pressure"),
  skt = c("skt", "skin_temperature"),
  sp = c("sp", "surface_pressure"),
  ssrd = c("ssrd", "surface_solar_radiation_downwards"),
  strd = c("strd", "surface_thermal_radiation_downwards"),
  tcc = c("tcc", "total_cloud_cover"),
  tp = c("tp", "total_precipitation"),
  fdir = c("fdir", "total_sky_direct_solar_radiation_at_surface"),
  latitude = c("latitude"),
  longitude = c("longitude")
)

rows <- vector("list", nrow(site_meta))
missing_fields <- list()
for (i in seq_len(nrow(site_meta))) {
  site_i <- site_meta$site[i]
  timezone_i <- site_meta$timezone[i]
  path <- file.path("data", "raw", "era5", paste0(site_i, ".csv"))
  if (!file.exists(path)) stop("Missing ERA5 file: ", path)

  payload <- era5_read_payload(path)
  x <- payload$data
  parsed_time <- era5_pick_time(x)
  if (any(is.na(parsed_time))) stop("Unparseable ERA5 timestamp(s) for ", site_i)
  dt <- sort(unique(parsed_time))
  if (length(dt) < 2L) stop("ERA5 has fewer than two valid timestamps for ", site_i)

  miss <- names(required_aliases)[!vapply(required_aliases, function(a) any(a %in% names(x)), logical(1))]
  if (length(miss)) missing_fields[[site_i]] <- miss

  diffs <- as.numeric(diff(dt), units = "secs")
  local_dates <- as.Date(lubridate::with_tz(dt, timezone_i))
  study <- eye_inv |> filter(.data$site == site_i)
  if (nrow(study) != 1L) stop("Could not resolve one light_glasses inventory row for ", site_i)

  grid_lat_vec <- era5_pick_numeric(x, c("latitude"))
  grid_lon_vec <- era5_pick_numeric(x, c("longitude"))
  grid_lat <- if (any(is.finite(grid_lat_vec))) grid_lat_vec[which(is.finite(grid_lat_vec))[1]] else NA_real_
  grid_lon <- if (any(is.finite(grid_lon_vec))) grid_lon_vec[which(is.finite(grid_lon_vec))[1]] else NA_real_
  expected_lat <- site_meta$latitude[i]
  expected_lon <- site_meta$longitude[i]
  lon_delta <- if (is.finite(grid_lon)) abs(((grid_lon - expected_lon + 180) %% 360) - 180) else Inf
  grid_matches_site <- is.finite(grid_lat) && is.finite(grid_lon) &&
    abs(grid_lat - expected_lat) <= 0.26 && lon_delta <= 0.26

  rows[[i]] <- tibble(
    site = site_i,
    payload_format = payload$payload_format,
    n_rows = nrow(x),
    n_columns = ncol(x),
    time_min_utc = min(dt),
    time_max_utc = max(dt),
    local_date_min = min(local_dates),
    local_date_max = max(local_dates),
    study_date_min = study$light_date_min,
    study_date_max = study$light_date_max,
    covers_study_dates = min(local_dates) <= study$light_date_min & max(local_dates) >= study$light_date_max,
    n_duplicate_times = nrow(x) - length(unique(parsed_time)),
    median_epoch_s = median(diffs),
    n_nonhourly_gaps = sum(diffs != 3600),
    n_missing_required_fields = length(miss),
    grid_latitude = grid_lat,
    grid_longitude = grid_lon,
    expected_latitude = expected_lat,
    expected_longitude = expected_lon,
    grid_matches_site = grid_matches_site
  )
}

audit <- bind_rows(rows)
dir.create("logs", recursive = TRUE, showWarnings = FALSE)
write.csv(audit, "logs/era5_input_inventory.csv", row.names = FALSE, na = "")

if (length(missing_fields)) {
  msg <- paste(vapply(names(missing_fields), function(s) {
    paste0(s, ": ", paste(missing_fields[[s]], collapse = ", "))
  }, character(1)), collapse = "; ")
  stop("ERA5 payload(s) missing requested variables: ", msg)
}
if (any(audit$n_duplicate_times > 0L)) {
  stop("Duplicate ERA5 timestamps detected; inspect logs/era5_input_inventory.csv")
}
if (any(audit$n_nonhourly_gaps > 0L)) {
  stop("ERA5 source is not a complete hourly series for one or more sites; inspect logs/era5_input_inventory.csv")
}
if (any(!audit$covers_study_dates)) {
  stop("ERA5 date coverage does not span all near-corneal study dates; inspect logs/era5_input_inventory.csv")
}
if (any(!audit$grid_matches_site)) {
  stop("ERA5 grid coordinate does not match the named MeLiDos site for one or more files; inspect logs/era5_input_inventory.csv")
}

message("ERA5 input preflight passed for all ", nrow(audit), " sites")
