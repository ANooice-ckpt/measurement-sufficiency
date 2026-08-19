suppressPackageStartupMessages({
  library(tidyverse)
  library(LightLogR)
})

# RQ1 downstream analysis.
# Scientific source of truth: docs/STUDY_SPEC.md
# Inputs are the completed core artifacts only. This script never returns to the
# harmonized 10-s source and never rebuilds the expensive metric cube.

CORE_METRICS <- "data/derived/core/metric_cube.csv.gz"
CORE_CONTEXT <- "data/derived/core/unit_context.csv.gz"
OUT_DATA <- "data/derived/rq1"
OUT_RESULTS <- "results/rq1"
OUT_DIAG <- "results/diagnostics"

B_BOOT <- suppressWarnings(as.integer(Sys.getenv("RQ1_BOOT", unset = "1000")))
if (!is.finite(B_BOOT) || B_BOOT < 0L) B_BOOT <- 1000L
BOOT_SEED <- 20260820L
PRIMARY_TEMPORAL_S <- c(15L, 20L, 30L, 60L, 300L, 900L, 1800L)
DUAL_CHANNEL_METRICS <- c("MDER", "nvRD")
ISIV_METRICS <- c("interdaily_stability", "intradaily_variability")
NUMERIC_TOL <- 1e-12

for (p in c(CORE_METRICS, CORE_CONTEXT)) {
  if (!file.exists(p)) stop("Missing required core artifact: ", p)
}
dir.create(OUT_DATA, recursive = TRUE, showWarnings = FALSE)
dir.create(OUT_RESULTS, recursive = TRUE, showWarnings = FALSE)
dir.create(OUT_DIAG, recursive = TRUE, showWarnings = FALSE)

message("RQ1: read core artifacts")
cube <- readr::read_csv(CORE_METRICS, show_col_types = FALSE, progress = FALSE) |>
  mutate(Date = as.Date(Date))
context <- readr::read_csv(CORE_CONTEXT, show_col_types = FALSE, progress = FALSE) |>
  mutate(Date = as.Date(Date))

required_cube <- c(
  "support_id", "site", "Id", "analysis_unit_type", "analysis_unit_id", "Date",
  "placement", "optical", "resolution_s", "is_primary_resolution", "config_id",
  "metric", "metric_class", "metric_scope", "metric_geometry",
  "value", "available", "unavailable_reason"
)
required_context <- c(
  "support_id", "site", "Id", "Date", "placement", "optical",
  "resolution_s", "config_id", "support_valid_day_count"
)
missing_cube <- setdiff(required_cube, names(cube))
missing_context <- setdiff(required_context, names(context))
if (length(missing_cube)) stop("metric_cube missing columns: ", paste(missing_cube, collapse = ", "))
if (length(missing_context)) stop("unit_context missing columns: ", paste(missing_context, collapse = ", "))

metric_meta <- cube |>
  distinct(metric, metric_class, metric_scope, metric_geometry)
if (n_distinct(metric_meta$metric) != 54L) {
  stop("Expected 54 target metrics in metric_cube; found ", n_distinct(metric_meta$metric))
}

temporal_label <- function(x) {
  dplyr::case_when(
    x < 60L ~ paste0(x, " s"),
    x %% 60L == 0L ~ paste0(x %/% 60L, " min"),
    TRUE ~ paste0(x, " s")
  )
}
short_temporal_code <- function(x) {
  dplyr::case_when(
    x < 60L ~ paste0(x, "s"),
    x %% 60L == 0L ~ paste0(x %/% 60L, "min"),
    TRUE ~ paste0(x, "s")
  )
}
circular_delta <- function(a, b, period = 86400) {
  ((a - b + period / 2) %% period) - period / 2
}
circular_mean <- function(x, period = 86400) {
  if (!length(x) || any(!is.finite(x))) return(NA_real_)
  theta <- 2 * pi * x / period
  ang <- atan2(mean(sin(theta)), mean(cos(theta))) %% (2 * pi)
  ang * period / (2 * pi)
}
aggregate_daily_representation <- function(x, geometry) {
  if (!length(x) || any(!is.finite(x))) return(NA_real_)
  if (identical(geometry, "circular_time")) circular_mean(x) else mean(x)
}
reference_scale <- function(x, geometry) {
  x <- x[is.finite(x)]
  if (length(x) < 2L) return(NA_real_)
  if (identical(geometry, "circular_time")) {
    center <- circular_mean(x)
    return(stats::sd(circular_delta(x, center)))
  }
  stats::sd(x)
}
safe_quantile <- function(x, p) {
  unname(stats::quantile(x, p, na.rm = TRUE, names = FALSE, type = 7))
}

choose_metric_support <- function(df, medi_support, full_support) {
  df |>
    filter(
      (metric %in% DUAL_CHANNEL_METRICS & support_id == full_support) |
        (!metric %in% DUAL_CHANNEL_METRICS & support_id == medi_support)
    )
}

pair_core_values <- function(candidate, reference, dimension, configuration,
                             configuration_label, configuration_order,
                             comparison_lattice, reference_configuration,
                             ordered_dimension = FALSE) {
  keys <- c(
    "support_id", "site", "Id", "analysis_unit_type",
    "analysis_unit_id", "Date", "metric"
  )
  cnd <- candidate |>
    select(
      all_of(keys), metric_class, metric_scope, metric_geometry,
      candidate_config_id = config_id,
      candidate_value = value,
      candidate_available = available,
      candidate_unavailable_reason = unavailable_reason
    )
  ref <- reference |>
    select(
      all_of(keys),
      reference_config_id = config_id,
      reference_value = value,
      reference_available = available,
      reference_unavailable_reason = unavailable_reason
    )

  inner_join(cnd, ref, by = keys) |>
    mutate(
      dimension = dimension,
      configuration = configuration,
      configuration_label = configuration_label,
      configuration_order = configuration_order,
      ordered_dimension = ordered_dimension,
      comparison_lattice = comparison_lattice,
      reference_configuration = reference_configuration,
      reference_unit_id = analysis_unit_id,
      window_id = NA_character_,
      window_index = NA_integer_,
      window_start = as.Date(NA),
      window_end = as.Date(NA),
      n_days = NA_integer_,
      pair_available =
        candidate_available & reference_available &
        is.finite(candidate_value) & is.finite(reference_value),
      pair_unavailable_reason = case_when(
        !reference_available | !is.finite(reference_value) ~
          coalesce(reference_unavailable_reason, "reference representation unavailable"),
        !candidate_available | !is.finite(candidate_value) ~
          coalesce(candidate_unavailable_reason, "candidate representation unavailable"),
        TRUE ~ NA_character_
      )
    ) |>
    select(
      dimension, configuration, configuration_label, configuration_order,
      ordered_dimension, comparison_lattice, reference_configuration,
      support_id, site, Id, analysis_unit_type, analysis_unit_id,
      reference_unit_id, Date, window_id, window_index, window_start, window_end,
      n_days, metric, metric_class, metric_scope, metric_geometry,
      candidate_config_id, reference_config_id,
      reference_value, candidate_value,
      pair_available, pair_unavailable_reason
    )
}

message("RQ1: construct placement, optical, and temporal comparison lattices")

placement_pairs <- map_dfr(c("chest", "wrist"), function(pos) {
  medi_support <- paste0("eye_", pos, "_medi")
  full_support <- paste0("eye_", pos, "_full")
  z <- cube |>
    filter(
      support_id %in% c(medi_support, full_support),
      placement %in% c("eye", pos),
      optical == "MEDI",
      resolution_s == 10L
    ) |>
    choose_metric_support(medi_support, full_support)

  pair_core_values(
    candidate = z |> filter(placement == pos),
    reference = z |> filter(placement == "eye"),
    dimension = "placement",
    configuration = pos,
    configuration_label = stringr::str_to_title(pos),
    configuration_order = match(pos, c("chest", "wrist")),
    comparison_lattice = paste0("placement_", pos),
    reference_configuration = "Eye MEDI, 10 s",
    ordered_dimension = FALSE
  )
})

optical_base <- cube |>
  filter(
    support_id == "eye_full",
    placement == "eye",
    optical %in% c("MEDI", "LIGHT"),
    resolution_s == 10L
  )
optical_pairs <- pair_core_values(
  candidate = optical_base |> filter(optical == "LIGHT"),
  reference = optical_base |> filter(optical == "MEDI"),
  dimension = "optical",
  configuration = "LIGHT",
  configuration_label = "Photopic illuminance",
  configuration_order = 1L,
  comparison_lattice = "optical",
  reference_configuration = "Eye MEDI, 10 s",
  ordered_dimension = FALSE
)

temporal_pairs <- map_dfr(seq_along(PRIMARY_TEMPORAL_S), function(j) {
  r <- PRIMARY_TEMPORAL_S[j]
  z <- cube |>
    filter(
      support_id %in% c("eye_medi", "eye_full"),
      placement == "eye",
      optical == "MEDI",
      resolution_s %in% c(10L, r)
    ) |>
    choose_metric_support("eye_medi", "eye_full")

  pair_core_values(
    candidate = z |> filter(resolution_s == r),
    reference = z |> filter(resolution_s == 10L),
    dimension = "temporal",
    configuration = short_temporal_code(r),
    configuration_label = temporal_label(r),
    configuration_order = j,
    comparison_lattice = "temporal",
    reference_configuration = "Eye MEDI, 10 s",
    ordered_dimension = TRUE
  )
})

message("RQ1: audit and construct monitoring-duration representations")

duration_context <- context |>
  filter(
    support_id %in% c("eye_medi", "eye_full"),
    placement == "eye",
    optical == "MEDI",
    resolution_s == 10L
  ) |>
  distinct(
    support_id, site, Id, Date, support_valid_day_count,
    raw_eye_recording_start, raw_eye_recording_end,
    raw_eye_span_hours, raw_eye_calendar_day_count,
    .keep_all = TRUE
  )

duration_cohort <- duration_context |>
  group_by(support_id, site, Id) |>
  summarise(
    n_valid_days = n_distinct(Date),
    valid_dates = list(sort(unique(Date))),
    dates_consecutive = {
      z <- sort(unique(Date))
      length(z) == 7L && all(as.integer(diff(z)) == 1L)
    },
    raw_eye_span_hours = first(raw_eye_span_hours),
    raw_eye_calendar_day_count = first(raw_eye_calendar_day_count),
    .groups = "drop"
  ) |>
  mutate(
    eligible_exact_7_consecutive = n_valid_days == 7L & dates_consecutive,
    exclusion_reason = case_when(
      eligible_exact_7_consecutive ~ NA_character_,
      n_valid_days > 7L ~ "more than seven valid days; no arbitrary seven-day truncation",
      n_valid_days < 7L ~ "fewer than seven valid days",
      !dates_consecutive ~ "seven valid days are not consecutive",
      TRUE ~ "not eligible"
    )
  )

duration_audit_csv <- duration_cohort |>
  mutate(valid_dates = map_chr(valid_dates, ~paste(as.character(.x), collapse = ";")))
readr::write_csv(
  duration_audit_csv,
  file.path(OUT_DIAG, "rq1_duration_cohort_audit.csv"),
  na = ""
)

eligible_duration <- duration_cohort |>
  filter(eligible_exact_7_consecutive)

duration_windows_list <- vector("list", 0L)
w_counter <- 0L
for (i in seq_len(nrow(eligible_duration))) {
  support_i <- eligible_duration$support_id[i]
  site_i <- eligible_duration$site[i]
  id_i <- eligible_duration$Id[i]
  dates_i <- eligible_duration$valid_dates[[i]]

  for (d in 1:6) {
    for (j in seq_len(8L - d)) {
      selected <- dates_i[j:(j + d - 1L)]
      w_counter <- w_counter + 1L
      duration_windows_list[[w_counter]] <- tibble(
        support_id = support_i,
        site = site_i,
        Id = id_i,
        n_days = d,
        window_index = j,
        window_id = paste(
          support_i, site_i, id_i, paste0(d, "d"), sprintf("w%02d", j),
          sep = "|"
        ),
        window_start = min(selected),
        window_end = max(selected),
        selected_dates = list(selected)
      )
    }
  }
}
duration_windows <- bind_rows(duration_windows_list)
readr::write_csv(
  duration_windows |>
    mutate(selected_dates = map_chr(selected_dates, ~paste(as.character(.x), collapse = ";"))),
  file.path(OUT_DIAG, "rq1_duration_windows.csv"),
  na = ""
)

duration_daily <- cube |>
  filter(
    support_id %in% c("eye_medi", "eye_full"),
    placement == "eye",
    optical == "MEDI",
    resolution_s == 10L,
    analysis_unit_type == "participant_day",
    !metric %in% ISIV_METRICS
  ) |>
  choose_metric_support("eye_medi", "eye_full") |>
  semi_join(
    eligible_duration |> select(support_id, site, Id),
    by = c("support_id", "site", "Id")
  )

duration_daily_ref <- duration_daily |>
  group_by(
    support_id, site, Id, metric, metric_class, metric_scope, metric_geometry
  ) |>
  summarise(
    n_days_present = n_distinct(Date),
    reference_available = n_days_present == 7L && all(available & is.finite(value)),
    reference_value = if (
      n_days_present == 7L && all(available & is.finite(value))
    ) {
      aggregate_daily_representation(value, first(metric_geometry))
    } else NA_real_,
    reference_unavailable_reason = if (
      n_days_present == 7L && all(available & is.finite(value))
    ) NA_character_ else "one or more daily reference representations unavailable",
    .groups = "drop"
  )

duration_daily_pairs_list <- vector("list", nrow(duration_windows))
for (i in seq_len(nrow(duration_windows))) {
  w <- duration_windows[i, ]
  selected <- w$selected_dates[[1]]
  z <- duration_daily |>
    filter(
      support_id == w$support_id,
      site == w$site,
      Id == w$Id,
      Date %in% selected
    ) |>
    group_by(
      support_id, site, Id, metric, metric_class, metric_scope, metric_geometry
    ) |>
    summarise(
      n_days_present = n_distinct(Date),
      candidate_available =
        n_days_present == w$n_days && all(available & is.finite(value)),
      candidate_value = if (
        n_days_present == w$n_days && all(available & is.finite(value))
      ) {
        aggregate_daily_representation(value, first(metric_geometry))
      } else NA_real_,
      candidate_unavailable_reason = if (
        n_days_present == w$n_days && all(available & is.finite(value))
      ) NA_character_ else "one or more daily candidate representations unavailable",
      .groups = "drop"
    ) |>
    left_join(
      duration_daily_ref |>
        filter(
          support_id == w$support_id,
          site == w$site,
          Id == w$Id
        ),
      by = c(
        "support_id", "site", "Id", "metric", "metric_class",
        "metric_scope", "metric_geometry"
      ),
      suffix = c("_candidate", "_reference")
    ) |>
    transmute(
      dimension = "duration",
      configuration = paste0(w$n_days, "d"),
      configuration_label = paste0(w$n_days, " d"),
      configuration_order = 7L - w$n_days,
      ordered_dimension = TRUE,
      comparison_lattice = "duration",
      reference_configuration = "7 d",
      support_id, site, Id,
      analysis_unit_type = "participant_window",
      analysis_unit_id = w$window_id,
      reference_unit_id = paste(support_id, site, Id, "7d", sep = "|"),
      Date = as.Date(NA),
      window_id = w$window_id,
      window_index = w$window_index,
      window_start = w$window_start,
      window_end = w$window_end,
      n_days = w$n_days,
      metric, metric_class, metric_scope, metric_geometry,
      candidate_config_id = paste0("duration_", w$n_days, "d"),
      reference_config_id = "duration_7d",
      reference_value,
      candidate_value,
      pair_available =
        candidate_available & reference_available &
        is.finite(candidate_value) & is.finite(reference_value),
      pair_unavailable_reason = case_when(
        !reference_available | !is.finite(reference_value) ~
          reference_unavailable_reason,
        !candidate_available | !is.finite(candidate_value) ~
          candidate_unavailable_reason,
        TRUE ~ NA_character_
      )
    )
  duration_daily_pairs_list[[i]] <- z
}
duration_daily_pairs <- bind_rows(duration_daily_pairs_list)

hour_cols <- grep("^isiv_h\\d\\d$", names(context), value = TRUE)
if (length(hour_cols) != 24L) {
  stop("Expected isiv_h00-isiv_h23 in unit_context; found ", length(hour_cols))
}

isiv_from_basis <- function(x) {
  long <- x |>
    select(Date, all_of(hour_cols)) |>
    pivot_longer(
      cols = all_of(hour_cols),
      names_to = "hour_name",
      values_to = "hourly_log_light"
    ) |>
    mutate(
      hour = as.integer(sub("^isiv_h", "", hour_name)),
      Datetime = as.POSIXct(
        as.numeric(Date) * 86400 + hour * 3600,
        origin = "1970-01-01", tz = "UTC"
      )
    ) |>
    arrange(Datetime)

  is <- tryCatch(
    suppressWarnings(
      LightLogR::interdaily_stability(
        long$hourly_log_light, long$Datetime, na.rm = TRUE, as.df = FALSE
      )
    ),
    error = function(e) NA_real_
  )
  iv <- tryCatch(
    suppressWarnings(
      LightLogR::intradaily_variability(
        long$hourly_log_light, long$Datetime, na.rm = TRUE, as.df = FALSE
      )
    ),
    error = function(e) NA_real_
  )
  tibble(
    metric = ISIV_METRICS,
    value = c(as.numeric(is), as.numeric(iv))
  )
}

duration_isiv_context <- duration_context |>
  filter(support_id == "eye_medi") |>
  semi_join(
    eligible_duration |>
      filter(support_id == "eye_medi") |>
      select(support_id, site, Id),
    by = c("support_id", "site", "Id")
  )

isiv_ref_list <- vector("list", 0L)
i_counter <- 0L
eligible_isiv <- eligible_duration |> filter(support_id == "eye_medi")
for (i in seq_len(nrow(eligible_isiv))) {
  support_i <- eligible_isiv$support_id[i]
  site_i <- eligible_isiv$site[i]
  id_i <- eligible_isiv$Id[i]
  x <- duration_isiv_context |>
    filter(support_id == support_i, site == site_i, Id == id_i)
  vals <- isiv_from_basis(x) |>
    left_join(metric_meta, by = "metric") |>
    transmute(
      support_id = support_i, site = site_i, Id = id_i,
      metric, metric_class, metric_scope, metric_geometry,
      reference_value = value,
      reference_available = is.finite(value),
      reference_unavailable_reason = if_else(
        is.finite(value), NA_character_,
        "IS/IV undefined on seven-day hourly basis"
      )
    )
  i_counter <- i_counter + 1L
  isiv_ref_list[[i_counter]] <- vals
}
duration_isiv_ref <- bind_rows(isiv_ref_list)

duration_isiv_pairs_list <- vector("list", 0L)
i_counter <- 0L
for (i in seq_len(nrow(duration_windows))) {
  w <- duration_windows[i, ]
  if (w$support_id != "eye_medi") next
  selected <- w$selected_dates[[1]]
  x <- duration_isiv_context |>
    filter(
      support_id == w$support_id,
      site == w$site,
      Id == w$Id,
      Date %in% selected
    )
  vals <- isiv_from_basis(x) |>
    left_join(metric_meta, by = "metric") |>
    transmute(
      support_id = w$support_id, site = w$site, Id = w$Id,
      metric, metric_class, metric_scope, metric_geometry,
      candidate_value = value,
      candidate_available = is.finite(value),
      candidate_unavailable_reason = if_else(
        is.finite(value), NA_character_,
        "IS/IV undefined on selected hourly basis"
      )
    ) |>
    left_join(
      duration_isiv_ref |>
        filter(
          support_id == w$support_id,
          site == w$site,
          Id == w$Id
        ),
      by = c(
        "support_id", "site", "Id", "metric", "metric_class",
        "metric_scope", "metric_geometry"
      )
    ) |>
    transmute(
      dimension = "duration",
      configuration = paste0(w$n_days, "d"),
      configuration_label = paste0(w$n_days, " d"),
      configuration_order = 7L - w$n_days,
      ordered_dimension = TRUE,
      comparison_lattice = "duration",
      reference_configuration = "7 d",
      support_id, site, Id,
      analysis_unit_type = "participant_window",
      analysis_unit_id = w$window_id,
      reference_unit_id = paste(support_id, site, Id, "7d", sep = "|"),
      Date = as.Date(NA),
      window_id = w$window_id,
      window_index = w$window_index,
      window_start = w$window_start,
      window_end = w$window_end,
      n_days = w$n_days,
      metric, metric_class, metric_scope, metric_geometry,
      candidate_config_id = paste0("duration_", w$n_days, "d"),
      reference_config_id = "duration_7d",
      reference_value,
      candidate_value,
      pair_available =
        candidate_available & reference_available &
        is.finite(candidate_value) & is.finite(reference_value),
      pair_unavailable_reason = case_when(
        !reference_available | !is.finite(reference_value) ~
          reference_unavailable_reason,
        !candidate_available | !is.finite(candidate_value) ~
          candidate_unavailable_reason,
        TRUE ~ NA_character_
      )
    )
  i_counter <- i_counter + 1L
  duration_isiv_pairs_list[[i_counter]] <- vals
}
duration_isiv_pairs <- bind_rows(duration_isiv_pairs_list)

# Concrete correctness check: the stored hourly basis must reproduce the existing
# 7-day core IS/IV values for the same eligible eye_medi participant.
core_isiv_reference <- cube |>
  filter(
    support_id == "eye_medi",
    placement == "eye",
    optical == "MEDI",
    resolution_s == 10L,
    analysis_unit_type == "participant_multiday",
    metric %in% ISIV_METRICS
  ) |>
  semi_join(
    eligible_isiv |> select(support_id, site, Id),
    by = c("support_id", "site", "Id")
  ) |>
  select(support_id, site, Id, metric, core_reference_value = value)

isiv_reconstruction_audit <- duration_isiv_ref |>
  select(support_id, site, Id, metric, reconstructed_reference_value = reference_value) |>
  left_join(
    core_isiv_reference,
    by = c("support_id", "site", "Id", "metric")
  ) |>
  mutate(
    abs_difference = abs(reconstructed_reference_value - core_reference_value),
    pass = is.finite(abs_difference) & abs_difference <= 1e-8
  )
readr::write_csv(
  isiv_reconstruction_audit,
  file.path(OUT_DIAG, "rq1_duration_isiv_reconstruction.csv"),
  na = ""
)
if (nrow(isiv_reconstruction_audit) && any(!isiv_reconstruction_audit$pass)) {
  stop("Duration IS/IV hourly-basis reconstruction does not reproduce core reference values")
}

duration_pairs <- bind_rows(duration_daily_pairs, duration_isiv_pairs)

message("RQ1: derive standardized signed distortion")
pairs <- bind_rows(
  placement_pairs,
  optical_pairs,
  temporal_pairs,
  duration_pairs
)
if (!nrow(pairs)) stop("No RQ1 comparison rows were constructed")

reference_basis <- pairs |>
  filter(pair_available, is.finite(reference_value)) |>
  distinct(
    comparison_lattice, metric, metric_geometry,
    site, Id, reference_unit_id, reference_value
  )

standardizers <- reference_basis |>
  group_by(comparison_lattice, metric, metric_geometry) |>
  summarise(
    n_reference_units = n(),
    standardizer = reference_scale(reference_value, first(metric_geometry)),
    .groups = "drop"
  ) |>
  mutate(
    zero_or_near_zero =
      !is.finite(standardizer) |
      standardizer <= sqrt(.Machine$double.eps)
  )
readr::write_csv(
  standardizers,
  file.path(OUT_DIAG, "rq1_standardizer_audit.csv"),
  na = ""
)

canonical <- pairs |>
  left_join(
    standardizers,
    by = c("comparison_lattice", "metric", "metric_geometry")
  ) |>
  mutate(
    delta = if_else(
      metric_geometry == "circular_time",
      circular_delta(candidate_value, reference_value),
      candidate_value - reference_value
    ),
    available =
      pair_available & !zero_or_near_zero &
      is.finite(delta) & is.finite(standardizer),
    unavailable_reason = case_when(
      !pair_available ~ pair_unavailable_reason,
      zero_or_near_zero ~ "reference dispersion zero or undefined",
      !is.finite(delta) ~ "representation difference undefined",
      TRUE ~ NA_character_
    ),
    e = if_else(available, delta / standardizer, NA_real_)
  ) |>
  select(
    dimension, configuration, configuration_label, configuration_order,
    ordered_dimension, comparison_lattice, reference_configuration,
    support_id, site, Id, analysis_unit_type, analysis_unit_id,
    reference_unit_id, Date, window_id, window_index, window_start, window_end,
    n_days, metric, metric_class, metric_scope, metric_geometry,
    candidate_config_id, reference_config_id,
    reference_value, candidate_value, delta, standardizer, e,
    available, unavailable_reason
  )

saveRDS(
  canonical,
  file.path(OUT_DATA, "rq1_distortion_long.rds"),
  compress = "xz"
)

configuration_manifest <- canonical |>
  distinct(
    dimension, configuration, configuration_label, configuration_order,
    ordered_dimension, reference_configuration
  ) |>
  arrange(
    factor(dimension, levels = c("placement", "optical", "temporal", "duration")),
    configuration_order
  )
readr::write_csv(
  configuration_manifest,
  file.path(OUT_RESULTS, "rq1_configuration_manifest.csv"),
  na = ""
)

bootstrap_ci <- function(g, B = B_BOOT) {
  site_counts <- g |>
    distinct(site, Id) |>
    count(site, name = "n_participants")
  supported <-
    B > 0L &&
    n_distinct(paste(g$site, g$Id, sep = "|")) >= 2L &&
    any(site_counts$n_participants > 1L)

  if (!supported) {
    return(tibble(
      bootstrap_supported = FALSE,
      B_ci_low = NA_real_, B_ci_high = NA_real_,
      A_ci_low = NA_real_, A_ci_high = NA_real_
    ))
  }

  clusters <- g |>
    group_by(site, Id) |>
    summarise(
      sum_e = sum(e),
      sum_abs_e = sum(abs(e)),
      n = n(),
      .groups = "drop"
    )
  by_site <- split(clusters, clusters$site)
  vals <- replicate(B, {
    sampled <- map_dfr(by_site, function(z) {
      z[sample.int(nrow(z), nrow(z), replace = TRUE), , drop = FALSE]
    })
    c(
      B = sum(sampled$sum_e) / sum(sampled$n),
      A = sum(sampled$sum_abs_e) / sum(sampled$n)
    )
  })

  tibble(
    bootstrap_supported = TRUE,
    B_ci_low = safe_quantile(vals["B", ], .025),
    B_ci_high = safe_quantile(vals["B", ], .975),
    A_ci_low = safe_quantile(vals["A", ], .025),
    A_ci_high = safe_quantile(vals["A", ], .975)
  )
}

message("RQ1: summarize empirical distributions and A/B geometry")
x <- canonical |> filter(available, is.finite(e))

summary_base <- x |>
  group_by(
    dimension, configuration, configuration_label, configuration_order,
    comparison_lattice, support_id, metric, metric_class, metric_geometry
  ) |>
  summarise(
    n_participants = n_distinct(paste(site, Id, sep = "|")),
    n_units = n(),
    median_e = median(e),
    q25_e = safe_quantile(e, .25),
    q75_e = safe_quantile(e, .75),
    p025_e = safe_quantile(e, .025),
    p975_e = safe_quantile(e, .975),
    B_mean_signed = mean(e),
    A_mean_absolute = mean(abs(e)),
    .groups = "drop"
  )

set.seed(BOOT_SEED)
cis <- x |>
  group_by(
    dimension, configuration, configuration_label, configuration_order,
    comparison_lattice, support_id, metric, metric_class, metric_geometry
  ) |>
  group_modify(~bootstrap_ci(.x, B = B_BOOT)) |>
  ungroup()

summary <- summary_base |>
  left_join(
    cis,
    by = c(
      "dimension", "configuration", "configuration_label",
      "configuration_order", "comparison_lattice", "support_id",
      "metric", "metric_class", "metric_geometry"
    )
  ) |>
  mutate(
    uncertainty_method = if_else(
      bootstrap_supported,
      paste0(B_BOOT, " participant-cluster bootstrap replicates, stratified by site"),
      "point estimate and empirical unit distribution only"
    )
  )
readr::write_csv(summary, file.path(OUT_RESULTS, "rq1_summary.csv"), na = "")

availability <- canonical |>
  group_by(
    dimension, configuration, configuration_label,
    metric, metric_class, comparison_lattice
  ) |>
  summarise(
    n_total_units = n(),
    n_available_units = sum(available),
    n_participants_total = n_distinct(paste(site, Id, sep = "|")),
    n_participants_available = n_distinct(
      paste(site[available], Id[available], sep = "|")
    ),
    representation_available = any(available),
    unavailable_reason = {
      z <- sort(unique(na.omit(unavailable_reason)))
      if (length(z)) paste(z, collapse = "; ") else NA_character_
    },
    .groups = "drop"
  )
readr::write_csv(
  availability,
  file.path(OUT_RESULTS, "rq1_metric_availability.csv"),
  na = ""
)

sample_flow <- canonical |>
  group_by(dimension, configuration, configuration_label, site) |>
  summarise(
    n_participants_paired = n_distinct(Id),
    n_participants_available = n_distinct(Id[available]),
    n_units_paired = n(),
    n_units_available = sum(available),
    .groups = "drop"
  )
readr::write_csv(sample_flow, file.path(OUT_RESULTS, "rq1_sample_flow.csv"), na = "")

ccc <- function(a, b) {
  ok <- is.finite(a) & is.finite(b)
  a <- a[ok]
  b <- b[ok]
  if (length(a) < 2L) return(NA_real_)
  va <- stats::var(a)
  vb <- stats::var(b)
  cv <- stats::cov(a, b)
  denom <- va + vb + (mean(a) - mean(b))^2
  if (!is.finite(denom) || denom <= 0) return(NA_real_)
  2 * cv / denom
}

retention <- canonical |>
  filter(available) |>
  group_by(
    dimension, configuration, configuration_label,
    metric, metric_class, metric_geometry
  ) |>
  summarise(
    n_units = n(),
    lin_ccc = if (first(metric_geometry) == "linear") {
      ccc(reference_value, candidate_value)
    } else NA_real_,
    spearman_rho = if (
      first(metric_geometry) == "linear" &&
      sum(is.finite(reference_value) & is.finite(candidate_value)) >= 2L
    ) {
      suppressWarnings(
        stats::cor(
          reference_value, candidate_value,
          method = "spearman", use = "complete.obs"
        )
      )
    } else NA_real_,
    diagnostic_note = if_else(
      first(metric_geometry) == "linear",
      "descriptive representation-retention diagnostic",
      "CCC/Spearman not reported for circular-time representation"
    ),
    .groups = "drop"
  )
readr::write_csv(
  retention,
  file.path(OUT_RESULTS, "rq1_retention_diagnostics.csv"),
  na = ""
)

geometry_audit <- summary |>
  transmute(
    dimension, configuration, metric,
    A_mean_absolute, B_mean_signed,
    gap = A_mean_absolute - abs(B_mean_signed),
    pass = A_mean_absolute + NUMERIC_TOL >= abs(B_mean_signed)
  )
readr::write_csv(
  geometry_audit,
  file.path(OUT_DIAG, "rq1_geometry_invariant.csv"),
  na = ""
)
if (any(!geometry_audit$pass)) stop("A >= |B| geometry invariant failed")

support_audit <- canonical |>
  distinct(
    dimension, configuration, metric, comparison_lattice,
    support_id, reference_config_id, candidate_config_id
  ) |>
  arrange(dimension, configuration, metric)
readr::write_csv(
  support_audit,
  file.path(OUT_DIAG, "rq1_support_audit.csv"),
  na = ""
)

writeLines(
  c(
    "# RQ1 run report",
    "",
    sprintf("Generated: %s", Sys.time()),
    "",
    "Inputs:",
    paste0("- ", CORE_METRICS),
    paste0("- ", CORE_CONTEXT),
    "",
    sprintf("Canonical distortion rows: %s", nrow(canonical)),
    sprintf("Finite/available distortion rows: %s", nrow(x)),
    sprintf("Bootstrap replicates where supported: %s", B_BOOT),
    sprintf("Bootstrap seed: %s", BOOT_SEED),
    "",
    "Primary temporal candidates: 15 s, 20 s, 30 s, 1 min, 5 min, 15 min, 30 min.",
    "Duration: exact seven-consecutive-day reference; all contiguous 1-6 d windows.",
    "Duration population bootstrap is omitted automatically when site-stratified participant resampling is degenerate.",
    "",
    "Canonical output:",
    "- data/derived/rq1/rq1_distortion_long.rds",
    "",
    "Main/supporting tables:",
    "- results/rq1/rq1_summary.csv",
    "- results/rq1/rq1_configuration_manifest.csv",
    "- results/rq1/rq1_metric_availability.csv",
    "- results/rq1/rq1_sample_flow.csv",
    "- results/rq1/rq1_retention_diagnostics.csv",
    "",
    "Diagnostics:",
    "- results/diagnostics/rq1_standardizer_audit.csv",
    "- results/diagnostics/rq1_geometry_invariant.csv",
    "- results/diagnostics/rq1_support_audit.csv",
    "- results/diagnostics/rq1_duration_cohort_audit.csv",
    "- results/diagnostics/rq1_duration_windows.csv",
    "- results/diagnostics/rq1_duration_isiv_reconstruction.csv"
  ),
  file.path(OUT_RESULTS, "RQ1_RUN_REPORT.md")
)

message("RQ1 complete.")
message("  ", file.path(OUT_DATA, "rq1_distortion_long.rds"))
message("  ", file.path(OUT_RESULTS, "rq1_summary.csv"))
