# Helpers for building the configuration-level metric cube.

if (!exists("ms_primary_temporal_s", mode = "function")) {
  source("scripts/utils/analysis_design.R")
}

core_artifact_version <- function() {
  paste0("v4_sparse_sampling_complete_days__", ms_core_design_id())
}

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

# Every configured cadence must be exactly representable as a systematic subset
# of the harmonized 10-s source grid. Primary states are restricted to the
# practical 10-s-to-2-min logging domain; 5 min is retained only as a coarse
# sensitivity state. Cadences coarser than 5 min are not materialised.
core_primary_resolutions <- function() ms_primary_temporal_s()
core_reserve_resolutions <- function() ms_reserve_temporal_s()
core_all_resolutions <- function() ms_all_temporal_s()

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

core_align_all_positions <- function(eye0, chest0, wrist0) {
  eye0 |>
    dplyr::select(Id, Datetime, MEDI, LIGHT) |>
    LightLogR::data2reference(chest0, Reference.column = MEDI_chest) |>
    LightLogR::data2reference(chest0, Data.column = LIGHT, Reference.column = LIGHT_chest) |>
    LightLogR::data2reference(wrist0, Reference.column = MEDI_wrist) |>
    LightLogR::data2reference(wrist0, Data.column = LIGHT, Reference.column = LIGHT_wrist) |>
    dplyr::rename(MEDI_eye = MEDI, LIGHT_eye = LIGHT)
}

core_prepare_support <- function(site, support_id) {
  is_full <- grepl("_full$", support_id)
  if (support_id %in% c("eye_medi", "eye_full")) {
    eye0 <- load_raw_file(raw_data_path(site, "light_glasses"), "light_glasses")
    sleep <- load_raw_file(raw_data_path(site, "sleepdiaries"), "sleepdiaries")
    wear <- load_raw_file(raw_data_path(site, "wearlog"), "wearlog")
    states <- core_build_state_intervals(sleep, wear)
    x <- eye0 |>
      dplyr::transmute(site = site, Id, Datetime, MEDI_eye = MEDI, LIGHT_eye = LIGHT) |>
      core_annotate_filter(states, c("MEDI_eye", "LIGHT_eye"))
    if (is_full) {
      x <- core_apply_common_mask(x, c("MEDI_eye", "LIGHT_eye"), c("MEDI_eye", "LIGHT_eye"))
    }
    return(core_complete_days(x) |>
      dplyr::distinct(site, Id, Datetime, .keep_all = TRUE) |>
      dplyr::mutate(support_id = support_id, .before = 1))
  }

  if (identical(site, "MPI")) return(NULL)
  eye0 <- load_raw_file(raw_data_path(site, "light_glasses"), "light_glasses")
  sleep <- load_raw_file(raw_data_path(site, "sleepdiaries"), "sleepdiaries")
  wear <- load_raw_file(raw_data_path(site, "wearlog"), "wearlog")
  states <- core_build_state_intervals(sleep, wear)

  if (grepl("chest_wrist", support_id, fixed = TRUE)) {
    chest0 <- load_raw_file(raw_data_path(site, "light_chest"), "light_chest")
    wrist0 <- load_raw_file(raw_data_path(site, "light_wrist"), "light_wrist")
    x <- core_align_all_positions(eye0, chest0, wrist0) |>
      dplyr::mutate(site = site, .before = 1)
    measure_cols <- c(
      "MEDI_eye", "LIGHT_eye",
      "MEDI_chest", "LIGHT_chest",
      "MEDI_wrist", "LIGHT_wrist"
    )
    x <- core_annotate_filter(x, states, measure_cols)
    required <- if (is_full) measure_cols else c("MEDI_eye", "MEDI_chest", "MEDI_wrist")
  } else {
    position <- if (grepl("chest", support_id, fixed = TRUE)) "chest" else "wrist"
    candidate0 <- load_raw_file(raw_data_path(site, paste0("light_", position)), paste0("light_", position))
    x <- core_align_pair(eye0, candidate0, position) |>
      dplyr::mutate(site = site, .before = 1)
    med_nm <- paste0("MEDI_", position)
    light_nm <- paste0("LIGHT_", position)
    measure_cols <- c("MEDI_eye", "LIGHT_eye", med_nm, light_nm)
    x <- core_annotate_filter(x, states, measure_cols)
    required <- if (is_full) measure_cols else c("MEDI_eye", med_nm)
  }

  x <- core_apply_common_mask(x, required, measure_cols)
  core_complete_days(x) |>
    dplyr::distinct(site, Id, Datetime, .keep_all = TRUE) |>
    dplyr::mutate(support_id = support_id, .before = 1)
}

core_support_grid <- function() {
  supports <- c(
    "eye_medi", "eye_full",
    "eye_chest_medi", "eye_chest_full",
    "eye_wrist_medi", "eye_wrist_full",
    "eye_chest_wrist_medi", "eye_chest_wrist_full"
  )
  tidyr::crossing(site = melidos_sites(), support_id = supports) |>
    dplyr::filter(!(site == "MPI" & support_id != "eye_medi" & support_id != "eye_full")) |>
    dplyr::mutate(
      support_role = dplyr::if_else(
        support_id %in% c("eye_chest_wrist_medi", "eye_chest_wrist_full"),
        "reserve_joint_all_positions", "primary_or_pairwise"
      )
    )
}

core_config_grid <- function(support_id) {
  placements <- if (grepl("chest_wrist", support_id, fixed = TRUE)) {
    c("eye", "chest", "wrist")
  } else if (grepl("eye_chest", support_id, fixed = TRUE)) {
    c("eye", "chest")
  } else if (grepl("eye_wrist", support_id, fixed = TRUE)) {
    c("eye", "wrist")
  } else {
    "eye"
  }
  opticals <- if (grepl("_full$", support_id)) c("MEDI", "LIGHT") else "MEDI"
  tidyr::crossing(
    placement = placements,
    optical = opticals,
    resolution_s = core_all_resolutions()
  ) |>
    dplyr::mutate(
      config_id = paste(placement, optical, paste0(resolution_s, "s"), sep = "__"),
      is_primary_resolution = resolution_s %in% core_primary_resolutions()
    )
}

# Sparse temporal sampling is anchored to each participant's native phase on
# the harmonized 10-s grid. Coarser states drop scheduled observations only;
# retained timestamps and values remain exact source rows.
core_source_grid_phase <- function(site, Id, sec_round) {
  group_key <- paste(site, Id, sep = "|")
  first <- !duplicated(group_key)
  phase_by_group <- sec_round[first] %% 10L
  names(phase_by_group) <- group_key[first]
  phase <- unname(phase_by_group[group_key])
  if (anyNA(phase) || any(((sec_round - phase) %% 10L) != 0L)) {
    stop("Core source timestamps do not share a stable 10-s grid phase within participant")
  }
  phase
}

core_make_series <- function(support, placement, optical, resolution_s) {
  resolution_s <- as.integer(resolution_s)
  if (!resolution_s %in% core_all_resolutions()) stop("Unsupported resolution: ", resolution_s)
  if (resolution_s %% 10L != 0L) stop("Resolution must be an integer multiple of the 10-s source grid")

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

  sec <- as.numeric(x$Datetime)
  if (any(!is.finite(sec))) stop("Non-finite timestamp in core support")
  sec_round <- round(sec)
  if (any(abs(sec - sec_round) > 1e-6)) stop("Core source timestamps are not on integer seconds")
  phase10 <- core_source_grid_phase(x$site, x$Id, sec_round)
  if (anyDuplicated(paste(x$site, x$Id, sec_round, sep = "|"))) {
    stop("Duplicate source timestamps in core_make_series")
  }
  if (resolution_s == 10L) return(x)

  keep <- ((sec_round - phase10) %% resolution_s) == 0L
  out <- x[keep, , drop = FALSE]
  if (!nrow(out)) stop("Sparse sampling produced no rows at ", resolution_s, " s")

  src_idx <- match(
    paste(out$site, out$Id, as.numeric(out$Datetime), sep = "|"),
    paste(x$site, x$Id, as.numeric(x$Datetime), sep = "|")
  )
  if (anyNA(src_idx)) stop("Sparse-sampled timestamp is not a source timestamp")
  same_num <- function(a, b) all((is.na(a) & is.na(b)) | (!is.na(a) & !is.na(b) & a == b))
  if (!same_num(out$MEDI, x$MEDI[src_idx]) || !same_num(out$LIGHT, x$LIGHT[src_idx])) {
    stop("Sparse sampling altered retained measurement values")
  }
  out
}

core_compute_support_metrics <- function(support_path) {
  support <- readRDS(support_path)
  support_id <- unique(support$support_id)
  if (length(support_id) != 1L) stop("Expected one support_id in ", support_path)
  cfgs <- core_config_grid(support_id)
  n_days <- support |>
    dplyr::distinct(site, Id, Date) |>
    dplyr::count(site, Id, name = "n_days_observed")

  blocks <- vector("list", nrow(cfgs))
  for (i in seq_len(nrow(cfgs))) {
    cfg <- cfgs[i, ]
    series <- core_make_series(support, cfg$placement, cfg$optical, cfg$resolution_s) |>
      dplyr::mutate(configuration = cfg$config_id)
    m <- rq1_all_metrics(
      series,
      include_spectral = identical(cfg$optical, "MEDI") && grepl("_full$", support_id),
      include_pulses = cfg$resolution_s < 300L
    ) |>
      dplyr::left_join(n_days, by = c("site", "Id")) |>
      dplyr::mutate(
        support_id = support_id,
        placement = cfg$placement,
        optical = cfg$optical,
        resolution_s = cfg$resolution_s,
        config_id = cfg$config_id,
        is_primary_resolution = cfg$is_primary_resolution,
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
        placement, optical, resolution_s, is_primary_resolution, config_id,
        metric, value
      )
    blocks[[i]] <- m
  }
  dplyr::bind_rows(blocks)
}

core_expand_metric_availability <- function(emitted, metric_types) {
  daily_types <- metric_types |>
    dplyr::filter(!metric %in% c("interdaily_stability", "intradaily_variability"))
  multi_types <- metric_types |>
    dplyr::filter(metric %in% c("interdaily_stability", "intradaily_variability"))

  units <- emitted |>
    dplyr::distinct(
      support_id, site, Id, analysis_unit_type, analysis_unit_id, Date, n_days,
      placement, optical, resolution_s, is_primary_resolution, config_id
    )
  daily_units <- units |> dplyr::filter(analysis_unit_type == "participant_day")
  multi_units <- units |> dplyr::filter(analysis_unit_type == "participant_multiday")

  dplyr::bind_rows(
    dplyr::cross_join(daily_units, daily_types),
    dplyr::cross_join(multi_units, multi_types)
  ) |>
    dplyr::left_join(
      emitted,
      by = c(
        "support_id", "site", "Id", "analysis_unit_type", "analysis_unit_id",
        "Date", "n_days", "placement", "optical", "resolution_s",
        "is_primary_resolution", "config_id", "metric"
      )
    ) |>
    dplyr::mutate(
      representation_available =
        !(optical == "LIGHT" & metric %in% c("MDER", "nvRD")) &
        !(resolution_s >= 300L & stringr::str_detect(metric, "pulses_above")),
      available = representation_available & is.finite(value),
      unavailable_reason = dplyr::case_when(
        optical == "LIGHT" & metric %in% c("MDER", "nvRD") ~ "requires MEDI and LIGHT simultaneously",
        resolution_s >= 300L & stringr::str_detect(metric, "pulses_above") ~ "pulse operator unavailable at this sampling interval",
        !is.finite(value) ~ "metric undefined or missing on this analysis unit",
        TRUE ~ NA_character_
      ),
      is_reference_config = placement == "eye" & optical == "MEDI" & resolution_s == 10L,
      metric_scope = dplyr::if_else(
        metric %in% c("interdaily_stability", "intradaily_variability"), "multiday", "daily"
      ),
      metric_geometry = dplyr::if_else(metric_class == "timing", "circular_time", "linear")
    ) |>
    dplyr::select(
      support_id, site, Id, analysis_unit_type, analysis_unit_id, Date, n_days,
      placement, optical, resolution_s, is_primary_resolution, config_id,
      metric, metric_class, metric_scope, metric_geometry,
      value, available, unavailable_reason, is_reference_config
    )
}
