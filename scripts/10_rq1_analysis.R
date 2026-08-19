suppressPackageStartupMessages({
  library(tidyverse)
  library(LightLogR)
})
source("scripts/utils/protocol_windows.R")

# RQ1 downstream analysis. Inputs are durable core artifacts only.
CORE_METRICS <- "data/derived/core/metric_cube.csv.gz"
CORE_CONTEXT <- "data/derived/core/unit_context.csv.gz"
OUT_DATA <- "data/derived/rq1"
OUT_RESULTS <- "results/rq1"
OUT_DIAG <- "results/diagnostics"

B_BOOT <- suppressWarnings(as.integer(Sys.getenv("RQ1_BOOT", unset = "1000")))
if (!is.finite(B_BOOT) || B_BOOT < 0L) B_BOOT <- 1000L
BOOT_SEED <- 20260820L
PRIMARY_TEMPORAL_S <- c(20L, 30L, 60L, 300L, 900L, 1800L)
DUAL_CHANNEL_METRICS <- c("MDER", "nvRD")
ISIV_METRICS <- c("interdaily_stability", "intradaily_variability")
NUMERIC_TOL <- 1e-12

for (p in c(CORE_METRICS, CORE_CONTEXT)) if (!file.exists(p)) stop("Missing required core artifact: ", p)
for (d in c(OUT_DATA, OUT_RESULTS, OUT_DIAG)) dir.create(d, recursive = TRUE, showWarnings = FALSE)

message("RQ1: read versioned core artifacts")
cube <- readr::read_csv(CORE_METRICS, show_col_types = FALSE, progress = FALSE) |> mutate(Date = as.Date(Date))
context <- readr::read_csv(CORE_CONTEXT, show_col_types = FALSE, progress = FALSE) |>
  mutate(
    Date = as.Date(Date), protocol_start_date = as.Date(protocol_start_date),
    protocol_end_date = as.Date(protocol_end_date)
  )

required_cube <- c(
  "core_artifact_version", "support_id", "site", "Id", "analysis_unit_type", "analysis_unit_id", "Date",
  "placement", "optical", "resolution_s", "is_primary_resolution", "config_id",
  "metric", "metric_class", "metric_scope", "metric_geometry", "value", "available", "unavailable_reason"
)
required_context <- c(
  "core_artifact_version", "support_id", "site", "Id", "Date", "placement", "optical", "resolution_s",
  "config_id", "support_valid_day_count", "protocol_start", "protocol_end",
  "protocol_start_date", "protocol_end_date", "protocol_metadata_available"
)
miss_cube <- setdiff(required_cube, names(cube)); miss_context <- setdiff(required_context, names(context))
if (length(miss_cube)) stop("metric_cube missing columns: ", paste(miss_cube, collapse = ", "))
if (length(miss_context)) stop("unit_context missing columns: ", paste(miss_context, collapse = ", "))
core_versions <- union(unique(cube$core_artifact_version), unique(context$core_artifact_version))
core_versions <- core_versions[!is.na(core_versions)]
if (length(core_versions) != 1L) stop("Core artifact version mismatch: ", paste(core_versions, collapse = ", "))
CORE_VERSION <- core_versions[[1]]
RQ1_ANALYSIS_VERSION <- paste0("rq1_v2__", CORE_VERSION)
if (any(cube$resolution_s == 15L) || any(context$resolution_s == 15L)) stop("Stale 15-s core artifact detected; rebuild v2 core")
if (!all(c(10L, PRIMARY_TEMPORAL_S) %in% unique(cube$resolution_s))) stop("Core artifact lacks one or more primary temporal resolutions")

metric_meta <- cube |> distinct(metric, metric_class, metric_scope, metric_geometry)
if (n_distinct(metric_meta$metric) != 54L) stop("Expected 54 target metrics")

temporal_label <- function(x) case_when(x < 60L ~ paste0(x, " s"), x %% 60L == 0L ~ paste0(x %/% 60L, " min"), TRUE ~ paste0(x, " s"))
short_temporal_code <- function(x) case_when(x < 60L ~ paste0(x, "s"), x %% 60L == 0L ~ paste0(x %/% 60L, "min"), TRUE ~ paste0(x, "s"))
circular_delta <- function(a, b, period = 86400) ((a - b + period / 2) %% period) - period / 2
circular_mean <- function(x, period = 86400) {
  x <- x[is.finite(x)]; if (!length(x)) return(NA_real_)
  th <- 2*pi*x/period; (atan2(mean(sin(th)), mean(cos(th))) %% (2*pi))*period/(2*pi)
}
aggregate_daily_representation <- function(x, geometry) {
  if (!length(x) || any(!is.finite(x))) return(NA_real_)
  if (identical(geometry, "circular_time")) circular_mean(x) else mean(x)
}
reference_scale <- function(x, geometry) {
  x <- x[is.finite(x)]; if (length(x) < 2L) return(NA_real_)
  if (identical(geometry, "circular_time")) {
    center <- circular_mean(x); return(stats::sd(circular_delta(x, center)))
  }
  stats::sd(x)
}
robust_reference_scale <- function(x, geometry) {
  x <- x[is.finite(x)]; if (length(x) < 2L) return(NA_real_)
  z <- if (identical(geometry, "circular_time")) circular_delta(x, circular_mean(x)) else x
  s <- stats::IQR(z, na.rm = TRUE, type = 7) / 1.349
  if (is.finite(s) && s > sqrt(.Machine$double.eps)) s else NA_real_
}
safe_quantile <- function(x, p) {
  x <- x[is.finite(x)]; if (!length(x)) return(NA_real_)
  unname(stats::quantile(x, p, names = FALSE, type = 7))
}
choose_metric_support <- function(df, medi_support, full_support) {
  df |> filter((metric %in% DUAL_CHANNEL_METRICS & support_id == full_support) |
                (!metric %in% DUAL_CHANNEL_METRICS & support_id == medi_support))
}

pair_core_values <- function(candidate, reference, dimension, configuration, configuration_label,
                             configuration_order, comparison_lattice, reference_configuration,
                             ordered_dimension = FALSE) {
  keys <- c("support_id", "site", "Id", "analysis_unit_type", "analysis_unit_id", "Date", "metric")
  cnd <- candidate |> select(all_of(keys), metric_class, metric_scope, metric_geometry,
                             candidate_config_id = config_id, candidate_value = value,
                             candidate_available = available, candidate_unavailable_reason = unavailable_reason)
  ref <- reference |> select(all_of(keys), reference_config_id = config_id, reference_value = value,
                             reference_available = available, reference_unavailable_reason = unavailable_reason)
  inner_join(cnd, ref, by = keys) |>
    mutate(
      dimension = dimension, configuration = configuration, configuration_label = configuration_label,
      configuration_order = configuration_order, ordered_dimension = ordered_dimension,
      comparison_lattice = comparison_lattice, reference_configuration = reference_configuration,
      reference_unit_id = analysis_unit_id, reference_id = NA_character_,
      reference_window_start = as.Date(NA), reference_window_end = as.Date(NA),
      window_id = NA_character_, window_index = NA_integer_, window_start = as.Date(NA), window_end = as.Date(NA),
      n_days = NA_integer_, pair_available = candidate_available & reference_available & is.finite(candidate_value) & is.finite(reference_value),
      pair_unavailable_reason = case_when(
        !reference_available | !is.finite(reference_value) ~ coalesce(reference_unavailable_reason, "reference representation unavailable"),
        !candidate_available | !is.finite(candidate_value) ~ coalesce(candidate_unavailable_reason, "candidate representation unavailable"),
        TRUE ~ NA_character_
      )
    ) |>
    select(dimension, configuration, configuration_label, configuration_order, ordered_dimension,
           comparison_lattice, reference_configuration, support_id, site, Id, analysis_unit_type,
           analysis_unit_id, reference_unit_id, reference_id, Date, reference_window_start, reference_window_end,
           window_id, window_index, window_start, window_end, n_days, metric, metric_class, metric_scope,
           metric_geometry, candidate_config_id, reference_config_id, reference_value, candidate_value,
           pair_available, pair_unavailable_reason)
}

message("RQ1: placement, optical, and temporal comparison lattices")
placement_pairs <- map_dfr(c("chest", "wrist"), function(pos) {
  medi_support <- paste0("eye_", pos, "_medi"); full_support <- paste0("eye_", pos, "_full")
  z <- cube |>
    filter(support_id %in% c(medi_support, full_support), placement %in% c("eye", pos), optical == "MEDI", resolution_s == 10L) |>
    choose_metric_support(medi_support, full_support)
  pair_core_values(z |> filter(placement == pos), z |> filter(placement == "eye"),
                   "placement", pos, stringr::str_to_title(pos), match(pos, c("chest", "wrist")),
                   paste0("placement_", pos), "Eye MEDI, 10 s", FALSE)
})

optical_base <- cube |> filter(support_id == "eye_full", placement == "eye", optical %in% c("MEDI", "LIGHT"), resolution_s == 10L)
optical_pairs <- pair_core_values(
  optical_base |> filter(optical == "LIGHT"), optical_base |> filter(optical == "MEDI"),
  "optical", "LIGHT", "Photopic illuminance", 1L, "optical", "Eye MEDI, 10 s", FALSE
)

temporal_pairs <- map_dfr(seq_along(PRIMARY_TEMPORAL_S), function(j) {
  r <- PRIMARY_TEMPORAL_S[j]
  z <- cube |>
    filter(support_id %in% c("eye_medi", "eye_full"), placement == "eye", optical == "MEDI", resolution_s %in% c(10L, r)) |>
    choose_metric_support("eye_medi", "eye_full")
  pair_core_values(z |> filter(resolution_s == r), z |> filter(resolution_s == 10L),
                   "temporal", short_temporal_code(r), temporal_label(r), j, "temporal", "Eye MEDI, 10 s", TRUE)
})

message("RQ1: protocol-anchored monitoring-duration representations")
duration_context <- context |>
  filter(support_id %in% c("eye_medi", "eye_full"), placement == "eye", optical == "MEDI", resolution_s == 10L) |>
  distinct(support_id, site, Id, Date, .keep_all = TRUE)
duration_cohort <- protocol_reference_cohort(duration_context)
readr::write_csv(protocol_reference_audit_table(duration_cohort), file.path(OUT_DIAG, "rq1_duration_cohort_audit.csv"), na = "")
eligible_duration <- duration_cohort |> filter(eligible_protocol_7)
duration_windows <- make_protocol_duration_windows(eligible_duration, include_reference = FALSE)
if (nrow(duration_windows)) {
  readr::write_csv(duration_windows |>
    mutate(selected_dates = map_chr(selected_dates, ~paste(as.character(.x), collapse = ";")),
           reference_dates = map_chr(reference_dates, ~paste(as.character(.x), collapse = ";"))),
    file.path(OUT_DIAG, "rq1_duration_windows.csv"), na = "")
} else {
  readr::write_csv(tibble(), file.path(OUT_DIAG, "rq1_duration_windows.csv"))
}

# 52 participant-day target representations.
duration_daily <- cube |>
  filter(support_id %in% c("eye_medi", "eye_full"), placement == "eye", optical == "MEDI", resolution_s == 10L,
         analysis_unit_type == "participant_day", !metric %in% ISIV_METRICS) |>
  choose_metric_support("eye_medi", "eye_full") |>
  semi_join(eligible_duration |> select(support_id, site, Id), by = c("support_id", "site", "Id"))

daily_ref_list <- vector("list", nrow(eligible_duration))
for (i in seq_len(nrow(eligible_duration))) {
  u <- eligible_duration[i, ]; dates <- u$reference_dates[[1]]
  daily_ref_list[[i]] <- duration_daily |>
    filter(support_id == u$support_id, site == u$site, Id == u$Id, Date %in% dates) |>
    group_by(support_id, site, Id, metric, metric_class, metric_scope, metric_geometry) |>
    summarise(
      n_days_present = n_distinct(Date),
      reference_available = n_days_present == 7L && all(replace_na(available, FALSE) & is.finite(value)),
      reference_value = if (n_days_present == 7L && all(replace_na(available, FALSE) & is.finite(value)))
        aggregate_daily_representation(value, first(metric_geometry)) else NA_real_,
      .groups = "drop"
    ) |>
    mutate(reference_id = u$reference_id, reference_window_start = u$reference_start,
           reference_window_end = u$reference_end,
           reference_unavailable_reason = if_else(reference_available, NA_character_, "one or more protocol reference-day representations unavailable"))
}
duration_daily_ref <- bind_rows(daily_ref_list)

duration_daily_pairs_list <- vector("list", nrow(duration_windows))
for (i in seq_len(nrow(duration_windows))) {
  w <- duration_windows[i, ]; selected <- w$selected_dates[[1]]
  duration_daily_pairs_list[[i]] <- duration_daily |>
    filter(support_id == w$support_id, site == w$site, Id == w$Id, Date %in% selected) |>
    group_by(support_id, site, Id, metric, metric_class, metric_scope, metric_geometry) |>
    summarise(
      n_days_present = n_distinct(Date),
      candidate_available = n_days_present == w$n_days && all(replace_na(available, FALSE) & is.finite(value)),
      candidate_value = if (n_days_present == w$n_days && all(replace_na(available, FALSE) & is.finite(value)))
        aggregate_daily_representation(value, first(metric_geometry)) else NA_real_,
      .groups = "drop"
    ) |>
    left_join(duration_daily_ref |> filter(reference_id == w$reference_id),
              by = c("support_id", "site", "Id", "metric", "metric_class", "metric_scope", "metric_geometry")) |>
    transmute(
      dimension = "duration", configuration = paste0(w$n_days, "d"), configuration_label = paste0(w$n_days, " d"),
      configuration_order = 7L - w$n_days, ordered_dimension = TRUE, comparison_lattice = "duration",
      reference_configuration = "7 protocol-anchored days", support_id, site, Id,
      analysis_unit_type = "participant_window", analysis_unit_id = w$window_id,
      reference_unit_id = w$reference_id, reference_id = w$reference_id, Date = as.Date(NA),
      reference_window_start = w$reference_start, reference_window_end = w$reference_end,
      window_id = w$window_id, window_index = w$window_index, window_start = w$window_start,
      window_end = w$window_end, n_days = w$n_days, metric, metric_class, metric_scope, metric_geometry,
      candidate_config_id = paste0("duration_", w$n_days, "d"), reference_config_id = "duration_protocol_7d",
      reference_value, candidate_value,
      pair_available = candidate_available & reference_available & is.finite(candidate_value) & is.finite(reference_value),
      pair_unavailable_reason = case_when(
        !reference_available | !is.finite(reference_value) ~ reference_unavailable_reason,
        !candidate_available | !is.finite(candidate_value) ~ "one or more candidate-day representations unavailable",
        TRUE ~ NA_character_
      )
    )
}
duration_daily_pairs <- bind_rows(duration_daily_pairs_list)

# IS/IV are rebuilt from the stored hourly basis over exactly the selected dates.
hour_cols <- grep("^isiv_h\\d\\d$", names(context), value = TRUE)
if (length(hour_cols) != 24L) stop("Expected isiv_h00-isiv_h23 in unit_context")
isiv_from_basis <- function(x) {
  long <- x |> select(Date, all_of(hour_cols)) |>
    pivot_longer(all_of(hour_cols), names_to = "hour_name", values_to = "hourly_log_light") |>
    mutate(hour = as.integer(sub("^isiv_h", "", hour_name)),
           Datetime = as.POSIXct(as.numeric(Date)*86400 + hour*3600, origin = "1970-01-01", tz = "UTC")) |>
    arrange(Datetime)
  is <- tryCatch(suppressWarnings(LightLogR::interdaily_stability(long$hourly_log_light, long$Datetime, na.rm = TRUE, as.df = FALSE)), error = function(e) NA_real_)
  iv <- tryCatch(suppressWarnings(LightLogR::intradaily_variability(long$hourly_log_light, long$Datetime, na.rm = TRUE, as.df = FALSE)), error = function(e) NA_real_)
  tibble(metric = ISIV_METRICS, value = c(as.numeric(is), as.numeric(iv)))
}
duration_isiv_context <- duration_context |> filter(support_id == "eye_medi") |>
  semi_join(eligible_duration |> filter(support_id == "eye_medi") |> select(support_id, site, Id), by = c("support_id", "site", "Id"))
eligible_isiv <- eligible_duration |> filter(support_id == "eye_medi")
isiv_ref_list <- vector("list", nrow(eligible_isiv))
for (i in seq_len(nrow(eligible_isiv))) {
  u <- eligible_isiv[i, ]; dates <- u$reference_dates[[1]]
  vals <- isiv_from_basis(duration_isiv_context |> filter(site == u$site, Id == u$Id, Date %in% dates)) |>
    left_join(metric_meta, by = "metric")
  isiv_ref_list[[i]] <- vals |> transmute(
    support_id = u$support_id, site = u$site, Id = u$Id, reference_id = u$reference_id,
    reference_window_start = u$reference_start, reference_window_end = u$reference_end,
    metric, metric_class, metric_scope, metric_geometry, reference_value = value,
    reference_available = is.finite(value),
    reference_unavailable_reason = if_else(is.finite(value), NA_character_, "IS/IV undefined on protocol seven-day hourly basis")
  )
}
duration_isiv_ref <- bind_rows(isiv_ref_list)

isiv_pairs <- vector("list", 0L); k <- 0L
for (i in seq_len(nrow(duration_windows))) {
  w <- duration_windows[i, ]; if (w$support_id != "eye_medi") next
  vals <- isiv_from_basis(duration_isiv_context |>
    filter(site == w$site, Id == w$Id, Date %in% w$selected_dates[[1]])) |>
    left_join(metric_meta, by = "metric")
  k <- k + 1L
  isiv_pairs[[k]] <- vals |>
    transmute(support_id = w$support_id, site = w$site, Id = w$Id, metric, metric_class, metric_scope, metric_geometry,
              candidate_value = value, candidate_available = is.finite(value)) |>
    left_join(duration_isiv_ref |> filter(reference_id == w$reference_id),
              by = c("support_id", "site", "Id", "metric", "metric_class", "metric_scope", "metric_geometry")) |>
    transmute(
      dimension = "duration", configuration = paste0(w$n_days, "d"), configuration_label = paste0(w$n_days, " d"),
      configuration_order = 7L - w$n_days, ordered_dimension = TRUE, comparison_lattice = "duration",
      reference_configuration = "7 protocol-anchored days", support_id, site, Id,
      analysis_unit_type = "participant_window", analysis_unit_id = w$window_id,
      reference_unit_id = w$reference_id, reference_id = w$reference_id, Date = as.Date(NA),
      reference_window_start = w$reference_start, reference_window_end = w$reference_end,
      window_id = w$window_id, window_index = w$window_index, window_start = w$window_start, window_end = w$window_end,
      n_days = w$n_days, metric, metric_class, metric_scope, metric_geometry,
      candidate_config_id = paste0("duration_", w$n_days, "d"), reference_config_id = "duration_protocol_7d",
      reference_value, candidate_value,
      pair_available = candidate_available & reference_available & is.finite(candidate_value) & is.finite(reference_value),
      pair_unavailable_reason = case_when(
        !reference_available | !is.finite(reference_value) ~ reference_unavailable_reason,
        !candidate_available | !is.finite(candidate_value) ~ "IS/IV undefined on selected hourly basis",
        TRUE ~ NA_character_
      )
    )
}
duration_isiv_pairs <- bind_rows(isiv_pairs)
duration_pairs <- bind_rows(duration_daily_pairs, duration_isiv_pairs)

# Diagnostic: the selected reference is always exactly seven consecutive dates and
# never includes a later eighth return-day date.
duration_ref_audit <- eligible_duration |>
  transmute(support_id, site, Id, reference_id, reference_start, reference_end,
            n_reference_days = map_int(reference_dates, length), reference_dates_consecutive,
            n_extra_valid_dates, pass = n_reference_days == 7L & reference_dates_consecutive)
readr::write_csv(duration_ref_audit, file.path(OUT_DIAG, "rq1_duration_reference_invariant.csv"), na = "")
if (nrow(duration_ref_audit) && any(!duration_ref_audit$pass)) stop("Protocol duration reference invariant failed")

message("RQ1: standardized signed distortion")
pairs <- bind_rows(placement_pairs, optical_pairs, temporal_pairs, duration_pairs)
if (!nrow(pairs)) stop("No RQ1 comparison rows were constructed")
reference_basis <- pairs |> filter(is.finite(reference_value)) |>
  distinct(comparison_lattice, metric, metric_geometry, site, Id, reference_unit_id, reference_value)
standardizers <- reference_basis |> group_by(comparison_lattice, metric, metric_geometry) |>
  summarise(n_reference_units = n(), standardizer = reference_scale(reference_value, first(metric_geometry)),
            robust_standardizer = robust_reference_scale(reference_value, first(metric_geometry)), .groups = "drop") |>
  mutate(zero_or_near_zero = !is.finite(standardizer) | standardizer <= sqrt(.Machine$double.eps))
readr::write_csv(standardizers, file.path(OUT_DIAG, "rq1_standardizer_audit.csv"), na = "")

canonical <- pairs |> left_join(standardizers, by = c("comparison_lattice", "metric", "metric_geometry")) |>
  mutate(
    zero_or_near_zero = replace_na(zero_or_near_zero, TRUE),
    delta = if_else(metric_geometry == "circular_time", circular_delta(candidate_value, reference_value), candidate_value - reference_value),
    available = pair_available & !zero_or_near_zero & is.finite(delta) & is.finite(standardizer),
    unavailable_reason = case_when(
      !pair_available ~ pair_unavailable_reason, zero_or_near_zero ~ "reference dispersion zero or undefined",
      !is.finite(delta) ~ "representation difference undefined", TRUE ~ NA_character_),
    e = if_else(available, delta / standardizer, NA_real_),
    core_artifact_version = CORE_VERSION, rq1_analysis_version = RQ1_ANALYSIS_VERSION
  ) |>
  select(core_artifact_version, rq1_analysis_version, dimension, configuration, configuration_label, configuration_order,
         ordered_dimension, comparison_lattice, reference_configuration, support_id, site, Id, analysis_unit_type,
         analysis_unit_id, reference_unit_id, reference_id, Date, reference_window_start, reference_window_end,
         window_id, window_index, window_start, window_end, n_days, metric, metric_class, metric_scope, metric_geometry,
         candidate_config_id, reference_config_id, reference_value, candidate_value, pair_available, delta, standardizer, robust_standardizer,
         e, available, unavailable_reason)
saveRDS(canonical, file.path(OUT_DATA, "rq1_distortion_long.rds"), compress = "xz")

configuration_manifest <- canonical |> distinct(dimension, configuration, configuration_label, configuration_order,
                                                 ordered_dimension, reference_configuration) |>
  arrange(factor(dimension, levels = c("placement", "optical", "temporal", "duration")), configuration_order)
readr::write_csv(configuration_manifest, file.path(OUT_RESULTS, "rq1_configuration_manifest.csv"), na = "")

bootstrap_ci <- function(g, B = B_BOOT) {
  site_counts <- g |> distinct(site, Id) |> count(site, name = "n_participants")
  supported <- B > 0L && n_distinct(paste(g$site, g$Id, sep = "|")) >= 2L && any(site_counts$n_participants > 1L)
  if (!supported) return(tibble(bootstrap_supported = FALSE, B_ci_low = NA_real_, B_ci_high = NA_real_, A_ci_low = NA_real_, A_ci_high = NA_real_))
  clusters <- g |> group_by(site, Id) |> summarise(sum_e = sum(e), sum_abs_e = sum(abs(e)), n = n(), .groups = "drop")
  by_site <- split(clusters, clusters$site)
  vals <- replicate(B, {
    sampled <- map_dfr(by_site, ~.x[sample.int(nrow(.x), nrow(.x), replace = TRUE), , drop = FALSE])
    c(B = sum(sampled$sum_e)/sum(sampled$n), A = sum(sampled$sum_abs_e)/sum(sampled$n))
  })
  tibble(bootstrap_supported = TRUE, B_ci_low = safe_quantile(vals["B",], .025), B_ci_high = safe_quantile(vals["B",], .975),
         A_ci_low = safe_quantile(vals["A",], .025), A_ci_high = safe_quantile(vals["A",], .975))
}

message("RQ1: summaries, uncertainty, and diagnostics")
x <- canonical |> filter(available, is.finite(e))
group_vars <- c("dimension", "configuration", "configuration_label", "configuration_order", "comparison_lattice", "support_id",
                "metric", "metric_class", "metric_geometry")
summary_base <- x |> group_by(across(all_of(group_vars))) |>
  summarise(n_participants = n_distinct(paste(site, Id, sep = "|")), n_units = n(), median_e = median(e),
            q25_e = safe_quantile(e,.25), q75_e = safe_quantile(e,.75), p025_e = safe_quantile(e,.025), p975_e = safe_quantile(e,.975),
            B_mean_signed = mean(e), A_mean_absolute = mean(abs(e)), .groups = "drop")
set.seed(BOOT_SEED)
cis <- x |> group_by(across(all_of(group_vars))) |> group_modify(~bootstrap_ci(.x, B_BOOT)) |> ungroup()
summary <- summary_base |> left_join(cis, by = group_vars) |>
  mutate(core_artifact_version = CORE_VERSION, rq1_analysis_version = RQ1_ANALYSIS_VERSION,
         uncertainty_method = if_else(bootstrap_supported,
          paste0(B_BOOT, " participant-cluster bootstrap replicates, stratified by site"),
          "point estimate and empirical unit distribution only"))
readr::write_csv(summary, file.path(OUT_RESULTS, "rq1_summary.csv"), na = "")

# Robust-scale sensitivity: identical raw delta, IQR/1.349 denominator.
robust_sens <- canonical |> filter(pair_available, is.finite(delta), is.finite(robust_standardizer), robust_standardizer > 0) |>
  mutate(e_robust = delta / robust_standardizer) |>
  group_by(across(all_of(group_vars))) |>
  summarise(B_robust = mean(e_robust), A_robust = mean(abs(e_robust)), n_units = n(), .groups = "drop")
readr::write_csv(robust_sens, file.path(OUT_RESULTS, "rq1_robust_scale_sensitivity.csv"), na = "")

# Participant-balanced sensitivity gives each participant total weight one within
# each metric x configuration, complementary to the primary smallest-unit estimand.
participant_balanced <- x |> group_by(across(all_of(group_vars)), site, Id) |>
  summarise(B_participant = mean(e), A_participant = mean(abs(e)), .groups = "drop") |>
  group_by(across(all_of(group_vars))) |>
  summarise(B_participant_balanced = mean(B_participant), A_participant_balanced = mean(A_participant), n_participants = n(), .groups = "drop")
readr::write_csv(participant_balanced, file.path(OUT_RESULTS, "rq1_participant_balanced_sensitivity.csv"), na = "")

availability <- canonical |> group_by(dimension, configuration, configuration_label, metric, metric_class, comparison_lattice) |>
  summarise(n_total_units = n(), n_available_units = sum(available), n_participants_total = n_distinct(paste(site,Id,sep="|")),
            n_participants_available = n_distinct(paste(site[available],Id[available],sep="|")), representation_available = any(available),
            unavailable_reason = {z <- sort(unique(na.omit(unavailable_reason))); if(length(z)) paste(z, collapse="; ") else NA_character_}, .groups="drop")
readr::write_csv(availability, file.path(OUT_RESULTS, "rq1_metric_availability.csv"), na = "")
sample_flow <- canonical |> group_by(dimension, configuration, configuration_label, site) |>
  summarise(n_participants_paired = n_distinct(Id), n_participants_available = n_distinct(Id[available]), n_units_paired=n(), n_units_available=sum(available), .groups="drop")
readr::write_csv(sample_flow, file.path(OUT_RESULTS, "rq1_sample_flow.csv"), na = "")

ccc <- function(a,b) {ok<-is.finite(a)&is.finite(b); a<-a[ok];b<-b[ok];if(length(a)<2)return(NA_real_);va<-var(a);vb<-var(b);cv<-cov(a,b);den<-va+vb+(mean(a)-mean(b))^2;if(!is.finite(den)||den<=0) NA_real_ else 2*cv/den}
retention <- canonical |> filter(available) |> group_by(dimension, configuration, configuration_label, metric, metric_class, metric_geometry) |>
  summarise(n_units=n(), lin_ccc=if(first(metric_geometry)=="linear") ccc(reference_value,candidate_value) else NA_real_,
            spearman_rho=if(first(metric_geometry)=="linear" && sum(is.finite(reference_value)&is.finite(candidate_value))>=2)
              suppressWarnings(cor(reference_value,candidate_value,method="spearman",use="complete.obs")) else NA_real_,
            diagnostic_note=if_else(first(metric_geometry)=="linear","descriptive representation-retention diagnostic","CCC/Spearman not reported for circular-time representation"), .groups="drop")
readr::write_csv(retention, file.path(OUT_RESULTS, "rq1_retention_diagnostics.csv"), na = "")

geometry_audit <- summary |> transmute(dimension,configuration,metric,A_mean_absolute,B_mean_signed,gap=A_mean_absolute-abs(B_mean_signed),pass=A_mean_absolute+NUMERIC_TOL>=abs(B_mean_signed))
readr::write_csv(geometry_audit, file.path(OUT_DIAG,"rq1_geometry_invariant.csv"), na="")
if(any(!geometry_audit$pass)) stop("A >= |B| geometry invariant failed")

support_audit <- canonical |> mutate(expected_support_id = case_when(
  dimension=="placement" & configuration=="chest" & metric %in% DUAL_CHANNEL_METRICS ~ "eye_chest_full",
  dimension=="placement" & configuration=="chest" ~ "eye_chest_medi",
  dimension=="placement" & configuration=="wrist" & metric %in% DUAL_CHANNEL_METRICS ~ "eye_wrist_full",
  dimension=="placement" & configuration=="wrist" ~ "eye_wrist_medi",
  dimension=="optical" ~ "eye_full",
  dimension %in% c("temporal","duration") & metric %in% DUAL_CHANNEL_METRICS ~ "eye_full",
  dimension %in% c("temporal","duration") ~ "eye_medi", TRUE ~ NA_character_),
  support_pass=support_id==expected_support_id) |>
  distinct(dimension,configuration,metric,comparison_lattice,support_id,expected_support_id,support_pass,reference_config_id,candidate_config_id)
readr::write_csv(support_audit,file.path(OUT_DIAG,"rq1_support_audit.csv"),na="")
if(any(!support_audit$support_pass)) stop("RQ1 support-lattice audit failed")

writeLines(c(
  "# RQ1 run report", "", sprintf("Generated: %s", Sys.time()),
  paste0("Core artifact version: ", CORE_VERSION), paste0("RQ1 analysis version: ", RQ1_ANALYSIS_VERSION),
  paste0("Canonical distortion rows: ", nrow(canonical)), paste0("Finite/available rows: ", nrow(x)),
  paste0("Bootstrap replicates where supported: ", B_BOOT),
  "Primary temporal candidates: 20 s, 30 s, 1 min, 5 min, 15 min, 30 min; generated by sparse systematic subsampling of the 10-s grid.",
  paste0("Duration: protocol-anchored seven-day references; eligible participants across supports: ", nrow(eligible_duration), "; all contiguous 1-6 d windows."),
  "Duration bootstrap is participant-cluster/site-stratified whenever the resulting protocol cohort supports it.",
  "Sensitivity outputs: robust reference scale (IQR/1.349) and participant-balanced A/B."
), file.path(OUT_RESULTS,"RQ1_RUN_REPORT.md"))

message("RQ1 complete: ", RQ1_ANALYSIS_VERSION)
message("  ", file.path(OUT_DATA,"rq1_distortion_long.rds"))
message("  ", file.path(OUT_RESULTS,"rq1_summary.csv"))
