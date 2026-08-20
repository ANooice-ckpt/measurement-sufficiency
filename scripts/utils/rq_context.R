# Real-world context helpers for post-core RQ1/RQ2 analyses.
# These functions never modify core artifacts. They annotate the cached cleaned
# support series with photoperiod and the MeLiDos light-exposure diary, then
# construct one canonical context representation: mean log light.

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
      dplyr::across(dplyr::all_of(rq_context_activity_columns()), ~tidyr::replace_na(as.logical(.x), FALSE))
    )

  # Match the public MeLiDos placement-analysis grammar, but retain only the
  # four prespecified primary activity states and the binary indoor/outdoor axis.
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
    dplyr::mutate(photoperiod = as.character(.data$photoperiod.state)) |>
    dplyr::select(-dplyr::any_of(c("dawn", "dusk", "photoperiod.state")))

  diary2 <- rq_context_prepare_diary(diary)
  out <- out |>
    dplyr::left_join(
      diary2,
      by = dplyr::join_by(Id, Datetime >= start, Datetime <= end)
    ) |>
    dplyr::select(-dplyr::any_of(c("start", "end")))

  if (nrow(out) != base_n) {
    stop("Context diary interval join duplicated support timestamps")
  }
  out
}

rq_context_make_series <- function(support_annotated, placement, optical, resolution_s) {
  series <- core_make_series(support_annotated, placement, optical, resolution_s)
  labels <- support_annotated |>
    dplyr::select(site, Id, Date, Datetime, photoperiod, environment, activity) |>
    dplyr::distinct()

  out <- series |>
    dplyr::left_join(labels, by = c("site", "Id", "Date", "Datetime"))
  if (nrow(out) != nrow(series)) stop("Context label join changed sparse-series row count")
  out
}

rq_context_daily_stats <- function(series) {
  series |>
    dplyr::mutate(log_light = LightLogR::log_zero_inflated(MEDI)) |>
    dplyr::select(site, Id, Date, Datetime, log_light, photoperiod, environment, activity) |>
    tidyr::pivot_longer(
      cols = c(photoperiod, environment, activity),
      names_to = "context_family", values_to = "context_state"
    ) |>
    dplyr::filter(!is.na(context_state), is.finite(log_light)) |>
    dplyr::group_by(site, Id, Date, context_family, context_state) |>
    dplyr::summarise(
      sum_log_light = sum(log_light),
      n_observations = dplyr::n(),
      value = sum_log_light / n_observations,
      .groups = "drop"
    )
}

rq_context_pair_daily <- function(reference_series, candidate_series) {
  ref <- rq_context_daily_stats(reference_series) |>
    dplyr::rename(
      reference_value = value,
      reference_n_observations = n_observations,
      reference_sum_log_light = sum_log_light
    )
  can <- rq_context_daily_stats(candidate_series) |>
    dplyr::rename(
      candidate_value = value,
      candidate_n_observations = n_observations,
      candidate_sum_log_light = sum_log_light
    )

  dplyr::inner_join(
    can, ref,
    by = c("site", "Id", "Date", "context_family", "context_state")
  ) |>
    dplyr::mutate(delta_native = candidate_value - reference_value)
}

rq_context_aggregate_dates <- function(daily_stats, dates, value_prefix) {
  z <- daily_stats |>
    dplyr::filter(Date %in% dates) |>
    dplyr::group_by(site, Id, context_family, context_state) |>
    dplyr::summarise(
      sum_log_light = sum(sum_log_light),
      n_observations = sum(n_observations),
      value = sum_log_light / n_observations,
      .groups = "drop"
    )

  names(z)[names(z) == "value"] <- paste0(value_prefix, "_value")
  names(z)[names(z) == "n_observations"] <- paste0(value_prefix, "_n_observations")
  names(z)[names(z) == "sum_log_light"] <- paste0(value_prefix, "_sum_log_light")
  z
}

rq_context_safe_quantile <- function(x, p) {
  x <- x[is.finite(x)]
  if (!length(x)) return(NA_real_)
  unname(stats::quantile(x, p, names = FALSE, type = 7))
}
