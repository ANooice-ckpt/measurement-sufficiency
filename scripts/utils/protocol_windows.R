# Shared protocol-anchored monitoring-duration helpers for RQ1 and RQ3.
# MeLiDos trial_times define the participant-specific study interval. The primary
# seven-day reference is protocol calendar Day 1 through Day 7, anchored at the
# local calendar date of datetime_trial_start. A later Day-8/return-date record is
# audited but can never replace a missing protocol reference day.

protocol_reference_cohort <- function(context_daily) {
  required <- c(
    "support_id", "site", "Id", "Date",
    "protocol_start", "protocol_end",
    "protocol_start_date", "protocol_end_date",
    "protocol_metadata_available"
  )
  missing <- setdiff(required, names(context_daily))
  if (length(missing)) stop("Protocol duration helper missing context columns: ", paste(missing, collapse = ", "))

  context_daily |>
    dplyr::filter(!is.na(Date)) |>
    dplyr::distinct(
      support_id, site, Id, Date,
      protocol_start, protocol_end, protocol_start_date, protocol_end_date,
      protocol_metadata_available
    ) |>
    dplyr::group_by(support_id, site, Id) |>
    dplyr::summarise(
      protocol_metadata_available = dplyr::first(protocol_metadata_available),
      protocol_start = dplyr::first(protocol_start),
      protocol_end = dplyr::first(protocol_end),
      protocol_start_date = dplyr::first(protocol_start_date),
      protocol_end_date = dplyr::first(protocol_end_date),
      all_valid_dates = list(sort(unique(Date))),
      .groups = "drop"
    ) |>
    dplyr::rowwise() |>
    dplyr::mutate(
      valid_dates_in_protocol = list({
        z <- all_valid_dates
        if (!isTRUE(protocol_metadata_available) || is.na(protocol_start_date) || is.na(protocol_end_date)) {
          as.Date(character())
        } else z[z >= protocol_start_date & z <= protocol_end_date]
      }),
      n_valid_dates_all = length(all_valid_dates),
      n_valid_dates_in_protocol = length(valid_dates_in_protocol),
      reference_dates = list({
        if (!isTRUE(protocol_metadata_available) || is.na(protocol_start_date)) {
          as.Date(character())
        } else protocol_start_date + 0:6
      }),
      reference_days_all_valid = {
        z <- reference_dates
        length(z) == 7L && all(z %in% all_valid_dates)
      },
      reference_dates_consecutive = {
        z <- reference_dates
        length(z) == 7L && all(as.integer(diff(z)) == 1L)
      },
      protocol_covers_reference = {
        z <- reference_dates
        length(z) == 7L && !is.na(protocol_end_date) && max(z) <= protocol_end_date
      },
      reference_start = if (length(reference_dates) == 7L) min(reference_dates) else as.Date(NA),
      reference_end = if (length(reference_dates) == 7L) max(reference_dates) else as.Date(NA),
      extra_dates = list({
        z <- valid_dates_in_protocol
        if (length(reference_dates) == 7L) z[z > max(reference_dates)] else as.Date(character())
      }),
      n_extra_valid_dates = length(extra_dates),
      eligible_protocol_7 =
        isTRUE(protocol_metadata_available) &&
        reference_dates_consecutive && protocol_covers_reference && reference_days_all_valid,
      exclusion_reason = dplyr::case_when(
        !isTRUE(protocol_metadata_available) ~ "trial_times metadata unavailable",
        !protocol_covers_reference ~ "protocol interval does not cover calendar Days 1-7",
        !reference_days_all_valid ~ "one or more protocol calendar Days 1-7 are invalid on this support",
        !reference_dates_consecutive ~ "protocol calendar Days 1-7 are not consecutive",
        TRUE ~ NA_character_
      ),
      reference_id = if (eligible_protocol_7) {
        paste(support_id, site, Id, reference_start, reference_end, sep = "|")
      } else NA_character_
    ) |>
    dplyr::ungroup()
}

protocol_reference_audit_table <- function(cohort) {
  cohort |>
    dplyr::mutate(
      all_valid_dates = purrr::map_chr(all_valid_dates, ~paste(as.character(.x), collapse = ";")),
      valid_dates_in_protocol = purrr::map_chr(valid_dates_in_protocol, ~paste(as.character(.x), collapse = ";")),
      reference_dates = purrr::map_chr(reference_dates, ~paste(as.character(.x), collapse = ";")),
      extra_dates = purrr::map_chr(extra_dates, ~paste(as.character(.x), collapse = ";"))
    )
}

make_protocol_duration_windows <- function(eligible_cohort, include_reference = FALSE) {
  eligible <- eligible_cohort |> dplyr::filter(eligible_protocol_7)
  if (!nrow(eligible)) return(tibble::tibble())
  day_lengths <- if (include_reference) 1:7 else 1:6
  out <- vector("list", 0L); k <- 0L
  for (i in seq_len(nrow(eligible))) {
    dates_i <- eligible$reference_dates[[i]]
    for (d in day_lengths) {
      for (j in seq_len(8L - d)) {
        selected <- dates_i[j:(j + d - 1L)]
        k <- k + 1L
        out[[k]] <- tibble::tibble(
          support_id = eligible$support_id[i], site = eligible$site[i], Id = eligible$Id[i],
          reference_id = eligible$reference_id[i], reference_start = eligible$reference_start[i],
          reference_end = eligible$reference_end[i], n_extra_valid_dates = eligible$n_extra_valid_dates[i],
          n_days = d, window_index = j,
          window_id = paste(eligible$reference_id[i], paste0(d, "d"), sprintf("w%02d", j), sep = "|"),
          window_start = min(selected), window_end = max(selected),
          selected_dates = list(selected), reference_dates = list(dates_i)
        )
      }
    }
  }
  dplyr::bind_rows(out)
}
