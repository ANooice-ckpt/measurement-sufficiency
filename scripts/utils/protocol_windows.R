# Complete-analysis-day duration helpers.
#
# Duration is an accumulation dimension. trial_times remains available in
# unit_context for audit/descriptive sensitivity, but it does not define the
# primary cohort or a seven-day reference.

complete_analysis_day_runs <- function(context_daily) {
  required <- c("support_id", "site", "Id", "Date")
  missing <- setdiff(required, names(context_daily))
  if (length(missing)) stop("Duration helper missing columns: ", paste(missing, collapse = ", "))

  days <- context_daily |>
    dplyr::filter(!is.na(Date)) |>
    dplyr::distinct(support_id, site, Id, Date) |>
    dplyr::arrange(support_id, site, Id, Date) |>
    dplyr::group_by(support_id, site, Id) |>
    dplyr::mutate(
      new_run = dplyr::if_else(dplyr::row_number() == 1L, TRUE, as.integer(Date - dplyr::lag(Date)) != 1L),
      run_id = cumsum(new_run)
    ) |>
    dplyr::ungroup()

  if (!nrow(days)) return(tibble::tibble())
  days |>
    dplyr::group_by(support_id, site, Id, run_id) |>
    dplyr::summarise(
      run_start = min(Date), run_end = max(Date), n_complete_days = dplyr::n_distinct(Date),
      member_dates = list(sort(unique(Date))),
      .groups = "drop"
    ) |>
    dplyr::mutate(run_id = as.integer(run_id), has_complete_6d_window = n_complete_days >= 6L)
}

duration_window_manifest <- function(context_daily, max_days = 6L) {
  runs <- complete_analysis_day_runs(context_daily)
  if (!nrow(runs)) return(tibble::tibble())
  max_days <- as.integer(max_days)
  if (!is.finite(max_days) || max_days < 1L) stop("max_days must be positive")

  out <- vector("list", 0L)
  k <- 0L
  for (i in seq_len(nrow(runs))) {
    dates <- runs$member_dates[[i]]
    for (d in seq_len(min(max_days, length(dates)))) {
      for (j in seq_len(length(dates) - d + 1L)) {
        selected <- dates[j:(j + d - 1L)]
        k <- k + 1L
        out[[k]] <- tibble::tibble(
          support_id = runs$support_id[i], site = runs$site[i], Id = runs$Id[i],
          run_id = runs$run_id[i], run_start = runs$run_start[i], run_end = runs$run_end[i],
          n_complete_days_in_run = runs$n_complete_days[i], window_index = j, n_days = d,
          window_start = min(selected), window_end = max(selected), member_dates = list(selected),
          window_id = paste(runs$support_id[i], runs$site[i], runs$Id[i],
                            paste0("run", runs$run_id[i]), paste0(d, "d"),
                            sprintf("w%03d", j), sep = "|"),
          nesting_key = paste(runs$support_id[i], runs$site[i], runs$Id[i], runs$run_id[i], sep = "|")
        )
      }
    }
  }
  windows <- dplyr::bind_rows(out)
  if (!nrow(windows)) return(windows)
  windows |>
    dplyr::group_by(support_id, site, Id, run_id) |>
    dplyr::mutate(
      run_window_count = dplyr::n(), has_complete_6d_window = any(n_days == 6L),
      adjacent_lower_window_id = dplyr::if_else(
        n_days > 1L,
        paste(support_id, site, Id, paste0("run", run_id), paste0(n_days - 1L, "d"), sprintf("w%03d", window_index), sep = "|"),
        NA_character_
      ),
      adjacent_higher_window_id = dplyr::if_else(
        n_days < 6L,
        paste(support_id, site, Id, paste0("run", run_id), paste0(n_days + 1L, "d"), sprintf("w%03d", window_index), sep = "|"),
        NA_character_
      )
    ) |>
    dplyr::ungroup()
}

duration_cohort_audit <- function(windows, runs = NULL) {
  if (is.null(runs)) {
    if (!nrow(windows)) return(tibble::tibble())
    runs <- windows |>
      dplyr::distinct(support_id, site, Id, run_id, run_start, run_end,
                      n_complete_days_in_run, has_complete_6d_window) |>
      dplyr::rename(n_complete_days = n_complete_days_in_run)
  }
  if (!nrow(runs)) return(tibble::tibble())
  window_counts <- windows |>
    dplyr::count(support_id, site, Id, run_id, n_days, name = "n_windows")
  runs |>
    dplyr::group_by(support_id) |>
    dplyr::summarise(
      participants = dplyr::n_distinct(paste(site, Id, sep = "|")), sites = dplyr::n_distinct(site),
      runs = dplyr::n_distinct(paste(site, Id, run_id, sep = "|")), complete_days = sum(n_complete_days),
      runs_with_6d_window = sum(has_complete_6d_window), .groups = "drop"
    ) |>
    dplyr::left_join(
      window_counts |>
        dplyr::group_by(support_id, n_days) |>
        dplyr::summarise(windows = sum(n_windows), .groups = "drop") |>
        tidyr::pivot_wider(names_from = n_days, values_from = windows, names_prefix = "windows_", values_fill = 0L),
      by = "support_id"
    ) |>
    dplyr::arrange(support_id)
}

# Compatibility name for old exploratory callers. It now implements the new
# complete-day semantics and never creates a protocol reference.
make_complete_duration_windows <- duration_window_manifest
