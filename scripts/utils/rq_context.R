# Real-world context helpers for post-core RQ1/RQ2 analyses.
# These functions never modify core artifacts. They annotate cached cleaned
# support series with civil photoperiod and the MeLiDos light-exposure diary,
# then calculate only target representations whose operators remain meaningful
# after restriction to the corresponding context.

rq_context_activity_columns <- function() {
  c(
    "act_sleep", "act_home", "act_road_vehicle", "act_road_open",
    "act_working_indoor", "act_working_outdoor", "act_free_outdoor", "act_other"
  )
}

rq_context_prepare_diary <- function(diary) {
  required <- c("Id", "start", "end", rq_context_activity_columns())
  missing <- setdiff(required, names(diary))
  if (length(missing)) {
    stop("lightexposurediary missing columns: ", paste(missing, collapse = ", "))
  }

  x <- diary |>
    dplyr::select(dplyr::all_of(required)) |>
    dplyr::mutate(
      Id = as.character(Id),
      end = end - lubridate::seconds(1),
      dplyr::across(
        dplyr::all_of(rq_context_activity_columns()),
        ~tidyr::replace_na(as.logical(.x), FALSE)
      )
    )

  x |>
    dplyr::mutate(
      environment = dplyr::case_when(
        !act_sleep & !act_other &
          (act_home | act_working_indoor) &
          !(act_road_vehicle | act_road_open | act_working_outdoor | act_free_outdoor) ~ "indoor",
        !act_sleep & !act_other &
          !(act_home | act_working_indoor) &
          (act_road_vehicle | act_road_open | act_working_outdoor | act_free_outdoor) ~ "outdoor",
        TRUE ~ NA_character_
      ),
      activity = dplyr::case_when(
        !act_sleep & !act_other & act_home &
          !(act_working_indoor | act_road_vehicle | act_road_open | act_working_outdoor | act_free_outdoor) ~ "home",
        !act_sleep & !act_other & act_working_indoor &
          !(act_home | act_road_vehicle | act_road_open | act_working_outdoor | act_free_outdoor) ~ "working",
        !act_sleep & !act_other & act_road_vehicle &
          !(act_home | act_working_indoor | act_road_open | act_working_outdoor | act_free_outdoor) ~ "vehicle",
        !act_sleep & !act_other &
          !(act_home | act_working_indoor | act_road_vehicle) &
          (act_road_open | act_working_outdoor | act_free_outdoor) ~ "outdoors",
        TRUE ~ NA_character_
      )
    ) |>
    dplyr::select(Id, start, end, environment, activity)
}

rq_context_annotate_support <- function(support, diary, coords) {
  if (!all(c("site", "Id", "Date", "Datetime") %in% names(support))) {
    stop("Cached support lacks site/Id/Date/Datetime")
  }
  if (length(coords) != 2L || any(!is.finite(coords))) stop("Invalid site coordinates")

  base_n <- nrow(support)
  out <- support |>
    dplyr::ungroup() |>
    LightLogR::add_photoperiod(coords) |>
    dplyr::mutate(
      photoperiod = tolower(as.character(.data$photoperiod.state)),
      photoperiod_anchor_date = dplyr::case_when(
        photoperiod == "day" ~ as.Date(Datetime),
        photoperiod == "night" & !is.na(dawn) & Datetime < dawn ~ as.Date(Datetime) - 1L,
        photoperiod == "night" ~ as.Date(Datetime),
        TRUE ~ as.Date(NA)
      )
    ) |>
    dplyr::select(-dplyr::any_of(c("dawn", "dusk", "photoperiod.state")))

  diary2 <- rq_context_prepare_diary(diary)
  out <- out |>
    dplyr::left_join(
      diary2,
      by = dplyr::join_by(Id, Datetime >= start, Datetime <= end)
    ) |>
    dplyr::select(-dplyr::any_of(c("start", "end")))

  if (nrow(out) != base_n) stop("Context diary interval join duplicated support timestamps")
  out
}

rq_context_make_series <- function(support_annotated, placement, optical, resolution_s) {
  series <- core_make_series(support_annotated, placement, optical, resolution_s)
  labels <- support_annotated |>
    dplyr::select(
      site, Id, Date, Datetime,
      photoperiod, photoperiod_anchor_date, environment, activity
    ) |>
    dplyr::distinct()
  out <- series |>
    dplyr::left_join(labels, by = c("site", "Id", "Date", "Datetime"))
  if (nrow(out) != nrow(series)) stop("Context label join changed sparse-series row count")
  out
}

# Operator-validity manifest. Full-day analysis remains all 54 target
# representations. Day/night keeps operators that remain interpretable on a
# continuous civil photoperiod interval. Indoor/outdoor and activity are often
# disjoint episodes, so only additive or distributional operators that can be
# reconstructed without concatenating episodes are admitted.
rq_context_metric_manifest <- function(metric_meta) {
  required <- c("metric", "metric_class", "metric_scope", "metric_geometry")
  missing <- setdiff(required, names(metric_meta))
  if (length(missing)) stop("metric metadata missing columns: ", paste(missing, collapse = ", "))

  metric_meta |>
    dplyr::distinct(dplyr::across(dplyr::all_of(required))) |>
    dplyr::mutate(
      metric = as.character(metric),
      is_daily_linear = metric_scope == "daily" & metric_geometry == "linear",
      photoperiod_family_valid = stringr::str_detect(
        metric,
        "(^duration_above_|^period_above_|pulses_above_|^frequency_crossing_|^disparity_index$|^mean_MEDI$|^dose$|^MDER$)"
      ),
      fragmented_family_valid = stringr::str_detect(
        metric,
        "(^duration_above_|^mean_MEDI$|^dose$|^MDER$)"
      ),
      photoperiod_valid = is_daily_linear & photoperiod_family_valid,
      fragmented_context_valid = is_daily_linear & fragmented_family_valid,
      validity_reason = dplyr::case_when(
        photoperiod_valid & fragmented_context_valid ~
          "operator preserved on both continuous photoperiod intervals and disjoint context episodes",
        photoperiod_valid ~
          "operator preserved on a continuous photoperiod interval but not after concatenating disjoint context episodes",
        metric == "nvRD" ~
          "response-dynamics operator depends on preceding regular time-series history and is not reset at context boundaries",
        metric_scope == "multiday" ~
          "requires a multiday/full-day temporal structure",
        metric_geometry == "circular_time" | metric_class == "timing" ~
          "timing construct changes meaning after context restriction",
        TRUE ~
          "operator depends on whole-day or continuous structure not preserved by the prespecified context analysis"
      )
    ) |>
    dplyr::select(
      metric, metric_class, metric_scope, metric_geometry,
      photoperiod_valid, fragmented_context_valid, validity_reason
    )
}

rq_context_safe_numeric <- function(expr) {
  tryCatch({
    value <- force(expr)
    if (is.data.frame(value)) {
      nums <- value[vapply(value, is.numeric, logical(1))]
      if (!length(nums)) return(NA_real_)
      value <- nums[[1]]
    }
    value <- suppressWarnings(as.numeric(value))
    value <- value[is.finite(value)]
    if (length(value)) value[[1]] else NA_real_
  }, error = function(e) NA_real_)
}

rq_context_datetime_2_numeric <- function(x) {
  x |>
    dplyr::mutate(
      dplyr::across(where(is.POSIXct), function(y) as.numeric(hms::as_hms(y)))
    )
}

rq_context_photoperiod_series <- function(series) {
  series |>
    dplyr::filter(
      photoperiod %in% c("day", "night"),
      !is.na(photoperiod_anchor_date)
    ) |>
    dplyr::mutate(
      context_date = as.Date(photoperiod_anchor_date),
      Date = context_date,
      configuration = paste0("ctx__", photoperiod)
    )
}

rq_context_photoperiod_metrics <- function(series, include_spectral = FALSE, include_pulses = TRUE) {
  x <- rq_context_photoperiod_series(series)
  if (!nrow(x)) return(tibble::tibble())

  nobs <- x |>
    dplyr::group_by(site, Id, Date, configuration) |>
    dplyr::summarise(n_observations = sum(is.finite(MEDI)), .groups = "drop")

  base <- x |>
    dplyr::group_by(site, Id, Date, configuration) |>
    dplyr::summarise(
      LightLogR::duration_above_threshold(MEDI, Datetime, "above", 10, na.rm = TRUE, as.df = TRUE),
      LightLogR::duration_above_threshold(MEDI, Datetime, "above", 250, na.rm = TRUE, as.df = TRUE),
      LightLogR::duration_above_threshold(MEDI, Datetime, "above", 1000, na.rm = TRUE, as.df = TRUE),
      LightLogR::period_above_threshold(MEDI, Datetime, "above", 10, na.rm = TRUE, as.df = TRUE),
      LightLogR::period_above_threshold(MEDI, Datetime, "above", 250, na.rm = TRUE, as.df = TRUE),
      LightLogR::period_above_threshold(MEDI, Datetime, "above", 1000, na.rm = TRUE, as.df = TRUE),
      LightLogR::frequency_crossing_threshold(MEDI, 250, na.rm = TRUE, as.df = TRUE),
      LightLogR::disparity_index(MEDI, TRUE, TRUE),
      mean_MEDI = mean(LightLogR::log_zero_inflated(MEDI), na.rm = TRUE),
      LightLogR::dose(MEDI, Datetime, na.rm = TRUE, as.df = TRUE),
      .groups = "drop"
    ) |>
    dplyr::mutate(dplyr::across(where(lubridate::is.duration), as.numeric)) |>
    tidyr::pivot_longer(-c(site, Id, Date, configuration), names_to = "metric", values_to = "value")

  blocks <- list(base)
  if (include_pulses) {
    pulses <- x |>
      dplyr::group_by(site, Id, Date, configuration) |>
      dplyr::summarise(
        LightLogR::pulses_above_threshold(MEDI, Datetime, threshold = 250, na.rm = TRUE, as.df = TRUE) |>
          rq_context_datetime_2_numeric(),
        LightLogR::pulses_above_threshold(MEDI, Datetime, threshold = 1000, na.rm = TRUE, as.df = TRUE) |>
          rq_context_datetime_2_numeric(),
        .groups = "drop"
      ) |>
      dplyr::mutate(dplyr::across(where(lubridate::is.duration), as.numeric)) |>
      tidyr::pivot_longer(-c(site, Id, Date, configuration), names_to = "metric", values_to = "value")
    blocks[[length(blocks) + 1L]] <- pulses
  }

  if (include_spectral) {
    spectral <- x |>
      dplyr::group_by(site, Id, Date, configuration) |>
      dplyr::summarise(MDER = median(MEDI / LIGHT, na.rm = TRUE), .groups = "drop") |>
      tidyr::pivot_longer(-c(site, Id, Date, configuration), names_to = "metric", values_to = "value")
    blocks[[length(blocks) + 1L]] <- spectral
  }

  dplyr::bind_rows(blocks) |>
    dplyr::left_join(nobs, by = c("site", "Id", "Date", "configuration")) |>
    dplyr::transmute(
      site, Id, Date,
      context_family = "photoperiod",
      context_state = sub("^ctx__", "", configuration),
      metric, value, n_observations
    )
}

rq_context_fragmented_metrics <- function(series, context_family, context_col,
                                          include_spectral = FALSE, resolution_s = 10L) {
  allowed_states <- if (identical(context_family, "environment")) {
    c("indoor", "outdoor")
  } else if (identical(context_family, "activity")) {
    c("home", "working", "vehicle", "outdoors")
  } else stop("Unsupported fragmented context family: ", context_family)

  state_sym <- rlang::sym(context_col)
  z <- series |>
    dplyr::mutate(context_state = tolower(as.character(!!state_sym))) |>
    dplyr::arrange(site, Id, Date, Datetime) |>
    dplyr::group_by(site, Id, Date) |>
    dplyr::mutate(
      dt_s = as.numeric(difftime(Datetime, dplyr::lag(Datetime), units = "secs")),
      state_changed = dplyr::row_number() == 1L |
        tidyr::replace_na(context_state != dplyr::lag(context_state), TRUE) |
        tidyr::replace_na(dt_s > 1.5 * resolution_s, FALSE),
      context_episode = cumsum(state_changed)
    ) |>
    dplyr::ungroup() |>
    dplyr::filter(context_state %in% allowed_states)
  if (!nrow(z)) return(tibble::tibble())

  nobs <- z |>
    dplyr::group_by(site, Id, Date, context_state) |>
    dplyr::summarise(n_observations = sum(is.finite(MEDI)), .groups = "drop")

  pooled <- z |>
    dplyr::group_by(site, Id, Date, context_state) |>
    dplyr::summarise(
      mean_MEDI = mean(LightLogR::log_zero_inflated(MEDI), na.rm = TRUE),
      .groups = "drop"
    ) |>
    tidyr::pivot_longer(mean_MEDI, names_to = "metric", values_to = "value")

  if (include_spectral) {
    mder <- z |>
      dplyr::group_by(site, Id, Date, context_state) |>
      dplyr::summarise(MDER = median(MEDI / LIGHT, na.rm = TRUE), .groups = "drop") |>
      tidyr::pivot_longer(MDER, names_to = "metric", values_to = "value")
    pooled <- dplyr::bind_rows(pooled, mder)
  }

  # Additive operators are calculated within each continuous episode and only
  # then summed, so separated bouts are never stitched into a false trajectory.
  epoch_txt <- paste0(as.integer(resolution_s), " sec")
  additive <- z |>
    dplyr::group_by(site, Id, Date, context_state, context_episode) |>
    dplyr::group_modify(~tibble::tibble(
      duration_above_10 = rq_context_safe_numeric(
        LightLogR::duration_above_threshold(.x$MEDI, .x$Datetime, "above", 10,
                                            epoch = epoch_txt, na.rm = TRUE)
      ),
      duration_above_250 = rq_context_safe_numeric(
        LightLogR::duration_above_threshold(.x$MEDI, .x$Datetime, "above", 250,
                                            epoch = epoch_txt, na.rm = TRUE)
      ),
      duration_above_1000 = rq_context_safe_numeric(
        LightLogR::duration_above_threshold(.x$MEDI, .x$Datetime, "above", 1000,
                                            epoch = epoch_txt, na.rm = TRUE)
      ),
      dose = rq_context_safe_numeric(LightLogR::dose(.x$MEDI, .x$Datetime, na.rm = TRUE))
    )) |>
    dplyr::ungroup() |>
    tidyr::pivot_longer(
      c(duration_above_10, duration_above_250, duration_above_1000, dose),
      names_to = "metric", values_to = "episode_value"
    ) |>
    dplyr::group_by(site, Id, Date, context_state, metric) |>
    dplyr::summarise(
      value = if (all(!is.finite(episode_value))) NA_real_ else sum(episode_value[is.finite(episode_value)]),
      .groups = "drop"
    )

  dplyr::bind_rows(pooled, additive) |>
    dplyr::left_join(nobs, by = c("site", "Id", "Date", "context_state")) |>
    dplyr::mutate(context_family = context_family, .before = context_state)
}

rq_context_compute_values <- function(series, metric_manifest,
                                      include_spectral = FALSE,
                                      include_pulses = TRUE,
                                      resolution_s = 10L) {
  photo <- rq_context_photoperiod_metrics(
    series, include_spectral = include_spectral, include_pulses = include_pulses
  ) |>
    dplyr::left_join(metric_manifest, by = "metric") |>
    dplyr::filter(tidyr::replace_na(photoperiod_valid, FALSE))

  env <- rq_context_fragmented_metrics(
    series, "environment", "environment",
    include_spectral = include_spectral, resolution_s = resolution_s
  ) |>
    dplyr::left_join(metric_manifest, by = "metric") |>
    dplyr::filter(tidyr::replace_na(fragmented_context_valid, FALSE))

  act <- rq_context_fragmented_metrics(
    series, "activity", "activity",
    include_spectral = include_spectral, resolution_s = resolution_s
  ) |>
    dplyr::left_join(metric_manifest, by = "metric") |>
    dplyr::filter(tidyr::replace_na(fragmented_context_valid, FALSE))

  dplyr::bind_rows(photo, env, act) |>
    dplyr::select(
      site, Id, Date, context_family, context_state,
      metric, metric_class, metric_scope, metric_geometry,
      value, n_observations
    )
}

rq_context_pair_values <- function(reference_values, candidate_values) {
  keys <- c("site", "Id", "Date", "context_family", "context_state", "metric")
  ref <- reference_values |>
    dplyr::select(
      dplyr::all_of(keys), metric_class, metric_scope, metric_geometry,
      reference_value = value, reference_n_observations = n_observations
    )
  can <- candidate_values |>
    dplyr::select(
      dplyr::all_of(keys),
      candidate_value = value, candidate_n_observations = n_observations
    )
  dplyr::inner_join(can, ref, by = keys) |>
    dplyr::mutate(delta_native = candidate_value - reference_value)
}

rq_context_aggregate_window <- function(daily_values, dates, prefix) {
  z <- daily_values |>
    dplyr::filter(Date %in% dates, is.finite(value)) |>
    dplyr::group_by(
      site, Id, context_family, context_state,
      metric, metric_class, metric_scope, metric_geometry
    ) |>
    dplyr::summarise(
      value = mean(value),
      n_context_days = dplyr::n_distinct(Date),
      n_observations = sum(n_observations, na.rm = TRUE),
      .groups = "drop"
    )
  names(z)[names(z) == "value"] <- paste0(prefix, "_value")
  names(z)[names(z) == "n_context_days"] <- paste0(prefix, "_n_context_days")
  names(z)[names(z) == "n_observations"] <- paste0(prefix, "_n_observations")
  z
}

rq_context_safe_quantile <- function(x, p) {
  x <- x[is.finite(x)]
  if (!length(x)) return(NA_real_)
  unname(stats::quantile(x, p, names = FALSE, type = 7))
}
