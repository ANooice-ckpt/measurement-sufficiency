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
      tidyr::complete(site, Id, Date, hour = 0:23) |>
      tidyr::pivot_wider(
        names_from = hour, values_from = hourly_log_light,
        names_prefix = "isiv_h", names_glue = "isiv_h{sprintf('%02d', hour)}"
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
