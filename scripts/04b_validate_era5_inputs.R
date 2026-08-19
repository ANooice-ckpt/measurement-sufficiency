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
  site <- site_meta$site[i]
  timezone <- site_meta$timezone[i]
  path <- file.path("data", "raw", "era5", paste0(site, ".csv"))
  if (!file.exists(path)) stop("Missing ERA5 file: ", path)

  payload <- era5_read_payload(path)
  x <- payload$data
  dt <- era5_pick_time(x)
  dt <- sort(unique(dt[!is.na(dt)]))
  if (length(dt) < 2L) stop("ERA5 has fewer than two valid timestamps for ", site)

  miss <- names(required_aliases)[!vapply(required_aliases, function(a) any(a %in% names(x)), logical(1))]
  if (length(miss)) missing_fields[[site]] <- miss

  diffs <- as.numeric(diff(dt), units = "secs")
  local_dates <- as.Date(lubridate::with_tz(dt, timezone))
  study <- eye_inv |> filter(site == !!site)
  if (nrow(study) != 1L) stop("Could not resolve one light_glasses inventory row for ", site)

  rows[[i]] <- tibble(
    site = site,
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
    n_duplicate_times = nrow(x) - length(unique(era5_pick_time(x))),
    median_epoch_s = median(diffs),
    n_nonhourly_gaps = sum(diffs != 3600),
    n_missing_required_fields = length(miss),
    grid_latitude = era5_pick_numeric(x, c("latitude"))[which(is.finite(era5_pick_numeric(x, c("latitude"))))[1]],
    grid_longitude = era5_pick_numeric(x, c("longitude"))[which(is.finite(era5_pick_numeric(x, c("longitude"))))[1]]
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

message("ERA5 input preflight passed for all ", nrow(audit), " sites")
