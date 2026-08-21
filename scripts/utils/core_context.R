# Configuration-level participant-day context and final metric-cube metadata.

core_config_daily_context <- function(support_path) {
  support <- readRDS(support_path)
  support_id <- unique(support$support_id)
  if (length(support_id) != 1L) stop("Expected one support_id in ", support_path)
  cfgs <- core_config_grid(support_id)

  participant_meta <- support |>
    dplyr::group_by(site, Id) |>
    dplyr::summarise(
      support_recording_start = min(Datetime, na.rm = TRUE),
      support_recording_end = max(Datetime, na.rm = TRUE),
      support_valid_day_count = dplyr::n_distinct(Date),
      support_span_hours = as.numeric(
        difftime(max(Datetime, na.rm = TRUE), min(Datetime, na.rm = TRUE), units = "hours")
      ),
      .groups = "drop"
    )

  protocol_path <- file.path("results", "core", "cache", "protocol_participant_metadata.rds")
  if (file.exists(protocol_path)) {
    protocol_meta <- readRDS(protocol_path) |>
      dplyr::filter(site %in% unique(support$site))
    participant_meta <- participant_meta |>
      dplyr::left_join(protocol_meta, by = c("site", "Id"))
  } else {
    participant_meta <- participant_meta |>
      dplyr::mutate(
        raw_eye_recording_start = as.POSIXct(NA), raw_eye_recording_end = as.POSIXct(NA),
        raw_eye_span_hours = NA_real_, raw_eye_calendar_day_count = NA_integer_,
        protocol_start = as.POSIXct(NA), protocol_end = as.POSIXct(NA),
        protocol_start_date = as.Date(NA), protocol_end_date = as.Date(NA),
        protocol_span_hours = NA_real_, protocol_calendar_date_count = NA_integer_,
        protocol_nominal_7d = NA, protocol_metadata_available = FALSE
      )
  }

  day_index <- support |>
    dplyr::distinct(site, Id, Date) |>
    dplyr::arrange(site, Id, Date) |>
    dplyr::group_by(site, Id) |>
    dplyr::mutate(valid_day_index = dplyr::row_number()) |>
    dplyr::ungroup()

  blocks <- vector("list", nrow(cfgs))
  for (i in seq_len(nrow(cfgs))) {
    cfg <- cfgs[i, ]
    series <- core_make_series(support, cfg$placement, cfg$optical, cfg$resolution_s)

    hourly <- series |>
      dplyr::transmute(site, Id, Date, Datetime, log_light = LightLogR::log_zero_inflated(MEDI)) |>
      dplyr::filter(is.finite(log_light)) |>
      dplyr::mutate(hour = lubridate::hour(Datetime)) |>
      dplyr::group_by(site, Id, Date, hour) |>
      dplyr::summarise(hourly_log_light = mean(log_light), .groups = "drop") |>
      dplyr::group_by(site, Id, Date) |>
      tidyr::complete(hour = 0:23) |>
      dplyr::ungroup() |>
      dplyr::mutate(hour_label = sprintf("%02d", hour)) |>
      dplyr::select(-hour) |>
      tidyr::pivot_wider(names_from = hour_label, values_from = hourly_log_light, names_prefix = "isiv_h")

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
          first_valid_time = if (all(missing)) as.POSIXct(NA, tz = lubridate::tz(.x$Datetime)) else min(.x$Datetime[!missing]),
          last_valid_time = if (all(missing)) as.POSIXct(NA, tz = lubridate::tz(.x$Datetime)) else max(.x$Datetime[!missing])
        )
      }) |>
      dplyr::ungroup() |>
      dplyr::left_join(hourly, by = c("site", "Id", "Date")) |>
      dplyr::left_join(participant_meta, by = c("site", "Id")) |>
      dplyr::left_join(day_index, by = c("site", "Id", "Date")) |>
      dplyr::mutate(
        protocol_day_index = dplyr::if_else(
          protocol_metadata_available & !is.na(protocol_start_date),
          as.integer(Date - protocol_start_date) + 1L, NA_integer_
        ),
        is_within_protocol_interval =
          protocol_metadata_available & !is.na(protocol_start_date) & !is.na(protocol_end_date) &
          Date >= protocol_start_date & Date <= protocol_end_date,
        is_protocol_day1_7 = is_within_protocol_interval & protocol_day_index %in% 1:7,
        is_protocol_return_date = is_within_protocol_interval & protocol_day_index == 8L,
        support_id = support_id,
        placement = cfg$placement,
        optical = cfg$optical,
        resolution_s = cfg$resolution_s,
        is_primary_resolution = cfg$is_primary_resolution,
        config_id = cfg$config_id,
        analysis_unit_type = "participant_day",
        analysis_unit_id = paste(support_id, site, Id, as.character(Date), sep = "|")
      ) |>
      dplyr::select(
        support_id, site, Id, analysis_unit_type, analysis_unit_id, Date,
        valid_day_index, support_valid_day_count,
        raw_eye_recording_start, raw_eye_recording_end,
        raw_eye_span_hours, raw_eye_calendar_day_count,
        protocol_start, protocol_end, protocol_start_date, protocol_end_date,
        protocol_span_hours, protocol_calendar_date_count, protocol_nominal_7d,
        protocol_metadata_available, protocol_day_index, is_within_protocol_interval,
        is_protocol_day1_7, is_protocol_return_date,
        support_recording_start, support_recording_end, support_span_hours,
        placement, optical, resolution_s, is_primary_resolution, config_id,
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
  needs_full_support <- grepl("_medi$", out$support_id) & out$metric %in% c("MDER", "nvRD")
  out$available[needs_full_support] <- FALSE
  out$unavailable_reason[needs_full_support] <- "dual-channel metric requires corresponding *_full support"

  support_roles <- core_support_grid() |> dplyr::distinct(support_id, support_role)
  out |>
    dplyr::left_join(support_roles, by = "support_id") |>
    dplyr::select(
      support_id, support_role, site, Id, analysis_unit_type, analysis_unit_id,
      Date, n_days, placement, optical, resolution_s, is_primary_resolution,
      config_id, metric, metric_class, metric_scope, metric_geometry,
      value, available, unavailable_reason, is_reference_config
    )
}
