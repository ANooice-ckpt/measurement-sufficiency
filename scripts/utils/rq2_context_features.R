# Layered environmental, micro-environmental, and behavioural context for RQ2.
# Reuse the harmonised MeLiDos diary fields and the existing rq_context rules;
# this file does not alter the core measurement artifacts.

if (!exists("raw_data_path", mode = "function")) source("scripts/utils/melidos_io.R")
if (!exists("rq_context_prepare_diary", mode = "function")) source("scripts/utils/rq_context.R")

rq2_context_external_predictors <- function() {
  c(
    "external_radiation", "external_direct_fraction", "external_cloud",
    "external_cloud_variability", "solar_noon_elevation_deg",
    "external_photoperiod_h", "external_temperature_c", "external_wet_hours"
  )
}

rq2_context_micro_predictors <- function() {
  c(
    "micro_outdoor_fraction", "micro_daylight_indoor_fraction",
    "micro_daylight_outdoor_fraction", "micro_display_fraction"
  )
}

rq2_context_behaviour_predictors <- function() {
  c(
    "behaviour_home_fraction", "behaviour_work_fraction", "behaviour_vehicle_fraction",
    "behaviour_workday", "behaviour_exercise_level", "behaviour_prior_sleep_h"
  )
}

rq2_context_weighted_fraction <- function(flag, weight, eligible) {
  ok <- is.finite(weight) & weight > 0 & !is.na(eligible) & eligible
  if (!any(ok)) return(NA_real_)
  sum(weight[ok] * as.numeric(tidyr::replace_na(flag[ok], FALSE))) / sum(weight[ok])
}

rq2_context_daily_diary <- function(site) {
  diary <- load_raw_file(raw_data_path(site, "lightexposurediary"), "lightexposurediary")
  act_cols <- rq_context_activity_columns()
  required <- c("Id", "Date", "start", "end", "lightsource_primary", act_cols)
  missing <- setdiff(required, names(diary))
  if (length(missing)) stop(site, " lightexposurediary missing columns: ", paste(missing, collapse = ", "))

  classified <- rq_context_prepare_diary(diary)
  raw <- diary |>
    dplyr::select(dplyr::all_of(required)) |>
    dplyr::mutate(
      Id = as.character(Id), Date = as.Date(Date),
      interval_h = as.numeric(difftime(end, start, units = "hours")),
      end = end - lubridate::seconds(1),
      lightsource_primary = as.character(lightsource_primary),
      dplyr::across(dplyr::all_of(act_cols), ~tidyr::replace_na(as.logical(.x), FALSE))
    )

  x <- classified |>
    dplyr::left_join(raw, by = c("Id", "start", "end")) |>
    dplyr::mutate(
      site = site,
      source_reported = !is.na(lightsource_primary) & nzchar(lightsource_primary),
      environment_reported = !is.na(environment),
      activity_reported = !is.na(activity),
      behaviour_work = act_working_indoor | act_working_outdoor
    )
  if (nrow(x) != nrow(classified)) stop(site, " light-exposure diary join changed interval count")

  x |>
    dplyr::group_by(site, Id, Date) |>
    dplyr::summarise(
      micro_outdoor_fraction = rq2_context_weighted_fraction(
        environment == "outdoor", interval_h, environment_reported
      ),
      micro_daylight_indoor_fraction = rq2_context_weighted_fraction(
        lightsource_primary == "Daylight indoors", interval_h, source_reported
      ),
      micro_daylight_outdoor_fraction = rq2_context_weighted_fraction(
        lightsource_primary == "Daylight outdoors (including shade)", interval_h, source_reported
      ),
      micro_display_fraction = rq2_context_weighted_fraction(
        lightsource_primary == "Emissive display light", interval_h, source_reported
      ),
      behaviour_home_fraction = rq2_context_weighted_fraction(
        act_home, interval_h, activity_reported
      ),
      behaviour_work_fraction = rq2_context_weighted_fraction(
        behaviour_work, interval_h, activity_reported
      ),
      behaviour_vehicle_fraction = rq2_context_weighted_fraction(
        act_road_vehicle, interval_h, activity_reported
      ),
      .groups = "drop"
    )
}

rq2_context_daily_sleep <- function(site) {
  sleep <- load_raw_file(raw_data_path(site, "sleepdiaries"), "sleepdiaries")
  required <- c("Id", "wake", "sleep_duration", "daytype2")
  missing <- setdiff(required, names(sleep))
  if (length(missing)) stop(site, " sleepdiary missing columns: ", paste(missing, collapse = ", "))

  duration_h <- if (inherits(sleep$sleep_duration, "difftime")) {
    as.numeric(sleep$sleep_duration, units = "hours")
  } else {
    as.numeric(sleep$sleep_duration)
  }
  day_type <- trimws(tolower(as.character(sleep$daytype2)))
  day_type_code <- suppressWarnings(as.integer(day_type))
  wake_tz <- lubridate::tz(sleep$wake)

  tibble::tibble(
    site = site,
    Id = as.character(sleep$Id),
    Date = as.Date(sleep$wake, tz = wake_tz),
    behaviour_workday = dplyr::case_when(
      day_type_code == 2L | stringr::str_detect(day_type, "work") ~ 1,
      day_type_code == 1L | stringr::str_detect(day_type, "free") ~ 0,
      TRUE ~ NA_real_
    ),
    behaviour_prior_sleep_h = duration_h
  ) |>
    dplyr::group_by(site, Id, Date) |>
    dplyr::summarise(
      behaviour_workday = mean(behaviour_workday, na.rm = TRUE),
      behaviour_prior_sleep_h = mean(behaviour_prior_sleep_h, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      behaviour_workday = dplyr::if_else(is.nan(behaviour_workday), NA_real_, behaviour_workday),
      behaviour_prior_sleep_h = dplyr::if_else(is.nan(behaviour_prior_sleep_h), NA_real_, behaviour_prior_sleep_h)
    )
}

rq2_context_daily_exercise <- function(site) {
  exercise <- load_raw_file(raw_data_path(site, "exercisediary"), "exercisediary")
  required <- c("Id", "Date", "intensity")
  missing <- setdiff(required, names(exercise))
  if (length(missing)) stop(site, " exercisediary missing columns: ", paste(missing, collapse = ", "))

  # MeLiDos imports the REDCap radio item as either its labelled factor or its
  # original 1:4 code, depending on the site/export. Accept exactly those two
  # official representations; do not infer additional exercise categories.
  intensity_text <- trimws(tolower(as.character(exercise$intensity)))
  intensity_code <- suppressWarnings(as.integer(intensity_text))
  exercise_level <- dplyr::case_when(
    intensity_code == 1L | stringr::str_detect(intensity_text, "^vigorous") ~ 3,
    intensity_code == 2L | stringr::str_detect(intensity_text, "^moderate") ~ 2,
    intensity_code == 3L | stringr::str_detect(intensity_text, "^light") ~ 1,
    intensity_code == 4L | stringr::str_detect(intensity_text, "^none") ~ 0,
    TRUE ~ NA_real_
  )

  tibble::tibble(
    site = site,
    Id = as.character(exercise$Id),
    Date = as.Date(exercise$Date),
    behaviour_exercise_level = exercise_level
  ) |>
    dplyr::group_by(site, Id, Date) |>
    dplyr::summarise(
      behaviour_exercise_level = mean(behaviour_exercise_level, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      behaviour_exercise_level = dplyr::if_else(is.nan(behaviour_exercise_level), NA_real_, behaviour_exercise_level)
    )
}

rq2_context_person_day <- function(sites) {
  diary <- purrr::map_dfr(sites, rq2_context_daily_diary)
  sleep <- purrr::map_dfr(sites, rq2_context_daily_sleep)
  exercise <- purrr::map_dfr(sites, rq2_context_daily_exercise)
  dplyr::full_join(diary, sleep, by = c("site", "Id", "Date")) |>
    dplyr::full_join(exercise, by = c("site", "Id", "Date"))
}

# Civil photoperiod follows the same LightLogR add_photoperiod() rule already
# used by the MeLiDos contextual analysis and by this repository's rq_context.R.
rq2_context_daily_photoperiod <- function(site_calendar) {
  required <- c("site", "Date", "timezone", "latitude", "longitude")
  missing <- setdiff(required, names(site_calendar))
  if (length(missing)) stop("site calendar missing columns: ", paste(missing, collapse = ", "))

  site_calendar |>
    dplyr::distinct(dplyr::across(dplyr::all_of(required))) |>
    dplyr::group_split(site) |>
    purrr::map_dfr(function(d) {
      site <- as.character(d$site[[1]])
      tz <- as.character(d$timezone[[1]])
      coords <- c(as.numeric(d$latitude[[1]]), as.numeric(d$longitude[[1]]))
      noon <- as.POSIXct(paste(as.character(d$Date), "12:00:00"), tz = tz)
      pp <- tibble::tibble(Id = "__site__", Datetime = noon) |>
        dplyr::group_by(Id) |>
        LightLogR::add_photoperiod(coords) |>
        dplyr::ungroup()
      tibble::tibble(
        site = site,
        Date = as.Date(d$Date),
        external_photoperiod_h = as.numeric(difftime(pp$dusk, pp$dawn, units = "hours"))
      )
    })
}
