suppressPackageStartupMessages({
  library(tidyverse)
  library(LightLogR)
})

# RQ3 downstream analysis.
# Scientific source of truth: docs/STUDY_SPEC.md.
# Single-dimension sufficiency is derived from frozen RQ1 A values.
# Multidimensional sufficiency is rebuilt from actual joint configuration values
# in the core artifacts. No additive synthesis of single-dimension effects is used.

RQ1_SUMMARY <- "results/rq1/rq1_summary.csv"
CORE_METRICS <- "data/derived/core/metric_cube.csv.gz"
CORE_CONTEXT <- "data/derived/core/unit_context.csv.gz"
OUT_DATA <- "data/derived/rq3"
OUT_RESULTS <- "results/rq3"
OUT_DIAG <- "results/diagnostics"

PRIMARY_TEMPORAL_S <- c(10L, 15L, 20L, 30L, 60L, 300L, 900L, 1800L)
ISIV_METRICS <- c("interdaily_stability", "intradaily_variability")
JOINT_SUPPORT <- "eye_chest_wrist_full"
NUMERIC_TOL <- 1e-12

for (p in c(RQ1_SUMMARY, CORE_METRICS, CORE_CONTEXT)) {
  if (!file.exists(p)) stop("Missing required input: ", p)
}
for (d in c(OUT_DATA, OUT_RESULTS, OUT_DIAG)) {
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
}

message("RQ3: read RQ1 summary and core artifacts")
rq1_summary <- readr::read_csv(RQ1_SUMMARY, show_col_types = FALSE, progress = FALSE)
cube <- readr::read_csv(CORE_METRICS, show_col_types = FALSE, progress = FALSE) |>
  mutate(Date = as.Date(Date))
context <- readr::read_csv(CORE_CONTEXT, show_col_types = FALSE, progress = FALSE) |>
  mutate(Date = as.Date(Date))

required_rq1 <- c(
  "dimension", "configuration", "configuration_label", "configuration_order",
  "metric", "metric_class", "metric_geometry", "A_mean_absolute", "B_mean_signed"
)
missing_rq1 <- setdiff(required_rq1, names(rq1_summary))
if (length(missing_rq1)) {
  stop("RQ1 summary missing columns: ", paste(missing_rq1, collapse = ", "))
}

metric_meta <- cube |>
  distinct(metric, metric_class, metric_scope, metric_geometry)
if (n_distinct(metric_meta$metric) != 54L) {
  stop("Expected 54 target metrics in metric_cube; found ", n_distinct(metric_meta$metric))
}

safe_quantile <- function(x, p) {
  x <- x[is.finite(x)]
  if (!length(x)) return(NA_real_)
  unname(stats::quantile(x, p, names = FALSE, type = 7))
}
circular_delta <- function(a, b, period = 86400) {
  ((a - b + period / 2) %% period) - period / 2
}
circular_mean <- function(x, period = 86400) {
  x <- x[is.finite(x)]
  if (!length(x)) return(NA_real_)
  theta <- 2 * pi * x / period
  (atan2(mean(sin(theta)), mean(cos(theta))) %% (2 * pi)) * period / (2 * pi)
}
aggregate_representation <- function(x, geometry) {
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
temporal_label <- function(x) {
  case_when(
    x < 60L ~ paste0(x, " s"),
    x %% 60L == 0L ~ paste0(x %/% 60L, " min"),
    TRUE ~ paste0(x, " s")
  )
}

# -----------------------------------------------------------------------------
# Part 1. Single-dimension sufficiency: exact inverse decision mapping of RQ1 A.
# -----------------------------------------------------------------------------
message("RQ3: derive single-dimension sufficiency functions")

single_base <- rq1_summary |>
  filter(is.finite(A_mean_absolute)) |>
  transmute(
    dimension, configuration, configuration_label, configuration_order,
    metric, metric_class, metric_geometry,
    A = A_mean_absolute,
    B = B_mean_signed,
    is_reference = FALSE,
    requirement_rank = case_when(
      dimension == "temporal" ~ configuration_order + 1L,
      dimension == "duration" ~ configuration_order + 1L,
      TRUE ~ NA_integer_
    )
  )

ordered_metric_meta <- single_base |>
  filter(dimension %in% c("temporal", "duration")) |>
  distinct(dimension, metric, metric_class, metric_geometry)

single_reference <- bind_rows(
  ordered_metric_meta |>
    filter(dimension == "temporal") |>
    transmute(
      dimension, configuration = "10s", configuration_label = "10 s",
      configuration_order = 0L, metric, metric_class, metric_geometry,
      A = 0, B = 0, is_reference = TRUE, requirement_rank = 1L
    ),
  ordered_metric_meta |>
    filter(dimension == "duration") |>
    transmute(
      dimension, configuration = "7d", configuration_label = "7 d",
      configuration_order = 0L, metric, metric_class, metric_geometry,
      A = 0, B = 0, is_reference = TRUE, requirement_rank = 1L
    )
)

single_levels <- bind_rows(single_base, single_reference) |>
  distinct(
    dimension, configuration, configuration_label, metric,
    .keep_all = TRUE
  ) |>
  arrange(dimension, metric, requirement_rank, configuration_order)

readr::write_csv(
  single_levels,
  file.path(OUT_RESULTS, "rq3_single_dimension_levels.csv"),
  na = ""
)

monotonicity <- single_levels |>
  filter(dimension %in% c("temporal", "duration")) |>
  arrange(dimension, metric, requirement_rank) |>
  group_by(dimension, metric, metric_class) |>
  summarise(
    n_levels = n(),
    response_monotone = all(diff(A) >= -NUMERIC_TOL),
    max_reverse_step = if (n() > 1L) min(diff(A), na.rm = TRUE) else 0,
    .groups = "drop"
  )
readr::write_csv(
  monotonicity,
  file.path(OUT_DIAG, "rq3_single_dimension_monotonicity.csv"),
  na = ""
)

single_state_one <- function(g) {
  eps <- sort(unique(c(0, g$A[is.finite(g$A)])))
  if (!length(eps)) return(tibble())

  states <- tidyr::crossing(
    epsilon = eps,
    row_id = seq_len(nrow(g))
  ) |>
    left_join(
      g |> mutate(row_id = row_number()),
      by = "row_id"
    ) |>
    mutate(sufficient = is.finite(A) & A <= epsilon + NUMERIC_TOL)

  if (!first(g$dimension) %in% c("temporal", "duration")) {
    return(states |>
      mutate(
        sufficient_set_threshold_like = NA,
        least_demanding_rank = NA_integer_,
        least_demanding_configuration = NA_character_,
        least_demanding_label = NA_character_,
        minimum_requirement_interpretable = NA
      ))
  }

  req <- states |>
    arrange(epsilon, requirement_rank) |>
    group_by(epsilon) |>
    group_modify(function(z, key) {
      flags <- z$sufficient
      threshold_like <- all(diff(as.integer(flags)) <= 0L)
      ok <- z |> filter(sufficient)
      if (!nrow(ok)) {
        return(tibble(
          sufficient_set_threshold_like = threshold_like,
          least_demanding_rank = NA_integer_,
          least_demanding_configuration = NA_character_,
          least_demanding_label = NA_character_
        ))
      }
      best <- ok |> slice_max(requirement_rank, n = 1, with_ties = FALSE)
      tibble(
        sufficient_set_threshold_like = threshold_like,
        least_demanding_rank = best$requirement_rank,
        least_demanding_configuration = best$configuration,
        least_demanding_label = best$configuration_label
      )
    }) |>
    ungroup() |>
    mutate(minimum_requirement_interpretable = sufficient_set_threshold_like)

  states |>
    left_join(req, by = "epsilon")
}

single_state <- single_levels |>
  group_by(dimension, metric) |>
  group_split(.keep = TRUE) |>
  map_dfr(single_state_one)

saveRDS(
  single_state,
  file.path(OUT_DATA, "rq3_single_dimension_sufficiency.rds"),
  compress = "xz"
)

single_requirement <- single_state |>
  filter(dimension %in% c("temporal", "duration")) |>
  distinct(
    dimension, metric, metric_class, epsilon,
    sufficient_set_threshold_like,
    least_demanding_rank, least_demanding_configuration,
    least_demanding_label, minimum_requirement_interpretable
  ) |>
  arrange(dimension, metric, epsilon)
readr::write_csv(
  single_requirement,
  file.path(OUT_RESULTS, "rq3_single_dimension_requirement.csv"),
  na = ""
)

unordered_thresholds <- single_levels |>
  filter(dimension %in% c("placement", "optical"), !is_reference) |>
  transmute(
    dimension, configuration, configuration_label,
    metric, metric_class, metric_geometry,
    epsilon_entry = A,
    B_at_entry = B
  )
readr::write_csv(
  unordered_thresholds,
  file.path(OUT_RESULTS, "rq3_unordered_sufficiency_thresholds.csv"),
  na = ""
)

# Descriptive coverage curves used by Fig. 4. Epsilon values are exact observed
# entry thresholds, not externally imposed universal recommendations.
coverage_curves <- single_state |>
  filter(dimension %in% c("placement", "optical")) |>
  group_by(dimension, configuration, configuration_label, epsilon) |>
  summarise(
    n_metrics = n_distinct(metric),
    fraction_metrics_sufficient = mean(sufficient),
    .groups = "drop"
  )
readr::write_csv(
  coverage_curves,
  file.path(OUT_RESULTS, "rq3_unordered_coverage_curves.csv"),
  na = ""
)

# -----------------------------------------------------------------------------
# Part 2. Multidimensional sufficiency from actual joint configuration values.
# -----------------------------------------------------------------------------
message("RQ3: construct actual multidimensional joint configuration values")

joint_ref_context <- context |>
  filter(
    support_id == JOINT_SUPPORT,
    placement == "eye",
    optical == "MEDI",
    resolution_s == 10L
  ) |>
  distinct(site, Id, Date, .keep_all = TRUE)

joint_cohort <- joint_ref_context |>
  group_by(site, Id) |>
  summarise(
    n_valid_days = n_distinct(Date),
    valid_dates = list(sort(unique(Date))),
    dates_consecutive = {
      z <- sort(unique(Date))
      length(z) == 7L && all(as.integer(diff(z)) == 1L)
    },
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

readr::write_csv(
  joint_cohort |>
    mutate(valid_dates = map_chr(valid_dates, ~paste(as.character(.x), collapse = ";"))),
  file.path(OUT_DIAG, "rq3_joint_duration_cohort_audit.csv"),
  na = ""
)

eligible_joint <- joint_cohort |> filter(eligible_exact_7_consecutive)

empty_joint <- function() {
  tibble(
    placement = character(), optical = character(), resolution_s = integer(),
    n_days = integer(), metric = character(), metric_class = character(),
    metric_geometry = character(), A_mean_absolute = double(),
    B_mean_signed = double(), n_participants = integer(), n_units = integer()
  )
}

if (!nrow(eligible_joint)) {
  warning(
    "No exact seven-consecutive-day participants on ", JOINT_SUPPORT,
    "; multidimensional RQ3 is not estimable under the frozen joint-support rule."
  )
  joint_canonical <- tibble()
  joint_summary <- empty_joint()
  joint_sufficiency <- tibble()
  pareto_ever <- tibble()
  pareto_frequency <- tibble()
  representative_metrics <- tibble()
} else {
  window_list <- vector("list", 0L)
  wi <- 0L
  for (i in seq_len(nrow(eligible_joint))) {
    dates_i <- eligible_joint$valid_dates[[i]]
    for (d in 1:7) {
      for (j in seq_len(8L - d)) {
        selected <- dates_i[j:(j + d - 1L)]
        wi <- wi + 1L
        window_list[[wi]] <- tibble(
          site = eligible_joint$site[i],
          Id = eligible_joint$Id[i],
          n_days = d,
          window_index = j,
          window_id = paste(
            eligible_joint$site[i], eligible_joint$Id[i],
            paste0(d, "d"), sprintf("w%02d", j), sep = "|"
          ),
          window_start = min(selected),
          window_end = max(selected),
          selected_dates = list(selected)
        )
      }
    }
  }
  joint_windows <- bind_rows(window_list)
  readr::write_csv(
    joint_windows |>
      mutate(selected_dates = map_chr(selected_dates, ~paste(as.character(.x), collapse = ";"))),
    file.path(OUT_DIAG, "rq3_joint_duration_windows.csv"),
    na = ""
  )

  joint_daily <- cube |>
    filter(
      support_id == JOINT_SUPPORT,
      is_primary_resolution,
      resolution_s %in% PRIMARY_TEMPORAL_S,
      analysis_unit_type == "participant_day",
      !metric %in% ISIV_METRICS
    ) |>
    semi_join(
      eligible_joint |> select(site, Id),
      by = c("site", "Id")
    )

  joint_reference_daily <- joint_daily |>
    filter(placement == "eye", optical == "MEDI", resolution_s == 10L) |>
    group_by(site, Id, metric, metric_class, metric_scope, metric_geometry) |>
    summarise(
      n_days_present = n_distinct(Date),
      reference_available =
        n_days_present == 7L && all(replace_na(available, FALSE) & is.finite(value)),
      reference_value = if (
        n_days_present == 7L && all(replace_na(available, FALSE) & is.finite(value))
      ) aggregate_representation(value, first(metric_geometry)) else NA_real_,
      .groups = "drop"
    )

  daily_pairs <- vector("list", nrow(joint_windows))
  for (i in seq_len(nrow(joint_windows))) {
    w <- joint_windows[i, ]
    selected <- w$selected_dates[[1]]
    z <- joint_daily |>
      filter(site == w$site, Id == w$Id, Date %in% selected) |>
      group_by(
        site, Id, placement, optical, resolution_s, config_id,
        metric, metric_class, metric_scope, metric_geometry
      ) |>
      summarise(
        n_days_present = n_distinct(Date),
        candidate_available =
          n_days_present == w$n_days && all(replace_na(available, FALSE) & is.finite(value)),
        candidate_value = if (
          n_days_present == w$n_days && all(replace_na(available, FALSE) & is.finite(value))
        ) aggregate_representation(value, first(metric_geometry)) else NA_real_,
        .groups = "drop"
      ) |>
      left_join(
        joint_reference_daily |>
          filter(site == w$site, Id == w$Id),
        by = c(
          "site", "Id", "metric", "metric_class", "metric_scope", "metric_geometry"
        )
      ) |>
      transmute(
        support_id = JOINT_SUPPORT,
        site, Id, window_id = w$window_id, window_index = w$window_index,
        window_start = w$window_start, window_end = w$window_end,
        n_days = w$n_days,
        placement, optical, resolution_s,
        candidate_config_id = paste0(config_id, "__", w$n_days, "d"),
        reference_config_id = "eye__MEDI__10s__7d",
        metric, metric_class, metric_scope, metric_geometry,
        reference_value, candidate_value,
        pair_available =
          reference_available & candidate_available &
          is.finite(reference_value) & is.finite(candidate_value)
      )
    daily_pairs[[i]] <- z
  }
  joint_daily_pairs <- bind_rows(daily_pairs)

  hour_cols <- grep("^isiv_h\\d\\d$", names(context), value = TRUE)
  if (length(hour_cols) != 24L) {
    stop("Expected isiv_h00-isiv_h23 in unit_context; found ", length(hour_cols))
  }

  isiv_from_basis <- function(x) {
    long <- x |>
      select(Date, all_of(hour_cols)) |>
      pivot_longer(
        cols = all_of(hour_cols),
        names_to = "hour_name", values_to = "hourly_log_light"
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
      suppressWarnings(LightLogR::interdaily_stability(
        long$hourly_log_light, long$Datetime, na.rm = TRUE, as.df = FALSE
      )),
      error = function(e) NA_real_
    )
    iv <- tryCatch(
      suppressWarnings(LightLogR::intradaily_variability(
        long$hourly_log_light, long$Datetime, na.rm = TRUE, as.df = FALSE
      )),
      error = function(e) NA_real_
    )
    tibble(metric = ISIV_METRICS, value = c(as.numeric(is), as.numeric(iv)))
  }

  joint_context_all <- context |>
    filter(
      support_id == JOINT_SUPPORT,
      is_primary_resolution,
      resolution_s %in% PRIMARY_TEMPORAL_S
    ) |>
    semi_join(
      eligible_joint |> select(site, Id),
      by = c("site", "Id")
    )

  ref_isiv <- vector("list", nrow(eligible_joint))
  for (i in seq_len(nrow(eligible_joint))) {
    z <- joint_context_all |>
      filter(
        site == eligible_joint$site[i], Id == eligible_joint$Id[i],
        placement == "eye", optical == "MEDI", resolution_s == 10L
      )
    vals <- isiv_from_basis(z) |>
      left_join(metric_meta, by = "metric") |>
      transmute(
        site = eligible_joint$site[i], Id = eligible_joint$Id[i],
        metric, metric_class, metric_scope, metric_geometry,
        reference_value = value,
        reference_available = is.finite(value)
      )
    ref_isiv[[i]] <- vals
  }
  joint_reference_isiv <- bind_rows(ref_isiv)

  isiv_pairs <- vector("list", 0L)
  ii <- 0L
  for (i in seq_len(nrow(joint_windows))) {
    w <- joint_windows[i, ]
    selected <- w$selected_dates[[1]]
    cfgs <- joint_context_all |>
      filter(site == w$site, Id == w$Id, Date %in% selected) |>
      distinct(placement, optical, resolution_s, config_id)

    for (j in seq_len(nrow(cfgs))) {
      cfg <- cfgs[j, ]
      z <- joint_context_all |>
        filter(
          site == w$site, Id == w$Id, Date %in% selected,
          config_id == cfg$config_id
        )
      vals <- isiv_from_basis(z) |>
        left_join(metric_meta, by = "metric") |>
        transmute(
          support_id = JOINT_SUPPORT,
          site = w$site, Id = w$Id,
          window_id = w$window_id, window_index = w$window_index,
          window_start = w$window_start, window_end = w$window_end,
          n_days = w$n_days,
          placement = cfg$placement,
          optical = cfg$optical,
          resolution_s = cfg$resolution_s,
          candidate_config_id = paste0(cfg$config_id, "__", w$n_days, "d"),
          reference_config_id = "eye__MEDI__10s__7d",
          metric, metric_class, metric_scope, metric_geometry,
          candidate_value = value,
          candidate_available = is.finite(value)
        ) |>
        left_join(
          joint_reference_isiv |>
            filter(site == w$site, Id == w$Id),
          by = c(
            "site", "Id", "metric", "metric_class", "metric_scope", "metric_geometry"
          )
        ) |>
        mutate(
          pair_available =
            reference_available & candidate_available &
            is.finite(reference_value) & is.finite(candidate_value)
        ) |>
        select(-reference_available, -candidate_available)
      ii <- ii + 1L
      isiv_pairs[[ii]] <- vals
    }
  }
  joint_isiv_pairs <- bind_rows(isiv_pairs)

  joint_pairs <- bind_rows(joint_daily_pairs, joint_isiv_pairs)

  joint_standardizers <- joint_pairs |>
    filter(is.finite(reference_value)) |>
    distinct(site, Id, metric, metric_geometry, reference_value) |>
    group_by(metric, metric_geometry) |>
    summarise(
      n_reference_participants = n(),
      standardizer = reference_scale(reference_value, first(metric_geometry)),
      .groups = "drop"
    ) |>
    mutate(
      standardizer_valid =
        is.finite(standardizer) & standardizer > sqrt(.Machine$double.eps)
    )
  readr::write_csv(
    joint_standardizers,
    file.path(OUT_DIAG, "rq3_joint_standardizer_audit.csv"),
    na = ""
  )

  joint_canonical <- joint_pairs |>
    left_join(joint_standardizers, by = c("metric", "metric_geometry")) |>
    mutate(
      delta = if_else(
        metric_geometry == "circular_time",
        circular_delta(candidate_value, reference_value),
        candidate_value - reference_value
      ),
      available =
        pair_available & replace_na(standardizer_valid, FALSE) &
        is.finite(delta) & is.finite(standardizer),
      e = if_else(available, delta / standardizer, NA_real_)
    ) |>
    select(
      support_id, site, Id, window_id, window_index, window_start, window_end,
      n_days, placement, optical, resolution_s,
      candidate_config_id, reference_config_id,
      metric, metric_class, metric_scope, metric_geometry,
      reference_value, candidate_value, standardizer, delta, e, available
    )

  xj <- joint_canonical |> filter(available, is.finite(e))
  joint_summary <- xj |>
    group_by(
      placement, optical, resolution_s, n_days,
      metric, metric_class, metric_geometry
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
    ) |>
    mutate(
      temporal_label = temporal_label(resolution_s),
      joint_configuration = paste(
        placement, optical, temporal_label, paste0(n_days, " d"), sep = " | "
      ),
      epsilon_entry = A_mean_absolute
    )

  geometry_audit <- joint_summary |>
    transmute(
      placement, optical, resolution_s, n_days, metric,
      A_mean_absolute, B_mean_signed,
      gap = A_mean_absolute - abs(B_mean_signed),
      pass = A_mean_absolute + NUMERIC_TOL >= abs(B_mean_signed)
    )
  readr::write_csv(
    geometry_audit,
    file.path(OUT_DIAG, "rq3_joint_geometry_invariant.csv"),
    na = ""
  )
  if (nrow(geometry_audit) && any(!geometry_audit$pass)) {
    stop("RQ3 joint A >= |B| geometry invariant failed")
  }

  pareto_state_one <- function(g) {
    eps <- sort(unique(c(0, g$A_mean_absolute[is.finite(g$A_mean_absolute)])))
    if (!length(eps)) return(tibble())
    out <- vector("list", length(eps))

    for (k in seq_along(eps)) {
      e0 <- eps[k]
      z <- g |>
        mutate(sufficient = A_mean_absolute <= e0 + NUMERIC_TOL)
      z$pareto <- FALSE
      idx <- which(z$sufficient)
      if (length(idx)) {
        for (ii in idx) {
          dominated <- any(vapply(idx, function(jj) {
            if (jj == ii) return(FALSE)
            no_more_demanding <-
              z$resolution_s[jj] >= z$resolution_s[ii] &&
              z$n_days[jj] <= z$n_days[ii]
            strictly_less_demanding <-
              z$resolution_s[jj] > z$resolution_s[ii] ||
              z$n_days[jj] < z$n_days[ii]
            no_more_demanding && strictly_less_demanding
          }, logical(1)))
          z$pareto[ii] <- !dominated
        }
      }
      out[[k]] <- z |>
        mutate(epsilon = e0)
    }
    bind_rows(out)
  }

  joint_sufficiency <- joint_summary |>
    group_by(metric, placement, optical) |>
    group_split(.keep = TRUE) |>
    map_dfr(pareto_state_one)

  pareto_ever <- joint_sufficiency |>
    group_by(
      metric, metric_class, placement, optical,
      resolution_s, temporal_label, n_days,
      joint_configuration, epsilon_entry, A_mean_absolute
    ) |>
    summarise(
      ever_pareto = any(pareto),
      first_pareto_epsilon = if (any(pareto)) min(epsilon[pareto]) else NA_real_,
      last_pareto_epsilon = if (any(pareto)) max(epsilon[pareto]) else NA_real_,
      .groups = "drop"
    )

  pareto_frequency <- pareto_ever |>
    group_by(placement, optical, resolution_s, temporal_label, n_days) |>
    summarise(
      n_metrics_available = n_distinct(metric),
      n_metrics_ever_pareto = n_distinct(metric[ever_pareto]),
      fraction_metrics_ever_pareto =
        n_metrics_ever_pareto / n_metrics_available,
      .groups = "drop"
    )

  metric_scores <- joint_summary |>
    group_by(metric, metric_class) |>
    summarise(
      n_joint_configs = n(),
      median_entry = median(epsilon_entry, na.rm = TRUE),
      entry_range = max(epsilon_entry, na.rm = TRUE) - min(epsilon_entry, na.rm = TRUE),
      .groups = "drop"
    ) |>
    left_join(
      pareto_ever |>
        group_by(metric) |>
        summarise(n_ever_pareto = sum(ever_pareto), .groups = "drop"),
      by = "metric"
    ) |>
    mutate(selection_score = n_ever_pareto + log1p(entry_range))

  representative_metrics <- metric_scores |>
    group_by(metric_class) |>
    slice_max(selection_score, n = 1, with_ties = FALSE) |>
    ungroup() |>
    slice_max(selection_score, n = min(4L, n()), with_ties = FALSE) |>
    arrange(desc(selection_score))
}

saveRDS(
  joint_canonical,
  file.path(OUT_DATA, "rq3_joint_distortion_long.rds"),
  compress = "xz"
)
saveRDS(
  joint_sufficiency,
  file.path(OUT_DATA, "rq3_joint_sufficiency_long.rds"),
  compress = "xz"
)

readr::write_csv(
  joint_summary,
  file.path(OUT_RESULTS, "rq3_joint_summary.csv"),
  na = ""
)
readr::write_csv(
  pareto_ever,
  file.path(OUT_RESULTS, "rq3_pareto_ever.csv"),
  na = ""
)
readr::write_csv(
  pareto_frequency,
  file.path(OUT_RESULTS, "rq3_pareto_frequency.csv"),
  na = ""
)
readr::write_csv(
  representative_metrics,
  file.path(OUT_RESULTS, "rq3_fig5_representative_metrics.csv"),
  na = ""
)

scope <- tibble(
  object = c("single_dimension", "multidimensional_joint"),
  estimable = c(TRUE, nrow(eligible_joint) > 0L),
  support = c("RQ1 comparison-specific maximal supports", JOINT_SUPPORT),
  n_joint_eligible_participants = c(NA_integer_, nrow(eligible_joint)),
  note = c(
    "Single-dimension sufficiency is the exact tolerance projection of RQ1 A values.",
    if (nrow(eligible_joint)) {
      "Joint sufficiency uses actual placement x optical x temporal x duration configuration values; placement/optical facets are incomparable for Pareto dominance."
    } else {
      "No exact seven-day cohort on the strict all-position full-information joint support; no multidimensional frontier is reported."
    }
  )
)
readr::write_csv(scope, file.path(OUT_RESULTS, "rq3_scope.csv"), na = "")

writeLines(
  c(
    "# RQ3 run report",
    "",
    sprintf("Generated: %s", Sys.time()),
    "",
    "Single-dimension sufficiency:",
    "- Uses RQ1 A point estimates exactly; epsilon is a tolerance on expected absolute standardized distortion.",
    "- Temporal resolution and monitoring duration are ordered dimensions.",
    "- Placement and optical states are not assigned an artificial total burden order.",
    "- Nonmonotone ordered responses are retained; minimum-requirement interpretation is flagged only when the sufficient set is threshold-like.",
    "",
    "Multidimensional sufficiency:",
    paste0("- Joint support: ", JOINT_SUPPORT),
    paste0("- Exact seven-day eligible participants: ", nrow(eligible_joint)),
    "- Actual joint configuration values are reconstructed from the core cube/context; no additive synthesis from RQ1 main effects is used.",
    "- Pareto dominance is applied only to temporal resolution and monitoring duration within fixed placement x optical facets.",
    "",
    "Primary outputs:",
    "- data/derived/rq3/rq3_single_dimension_sufficiency.rds",
    "- results/rq3/rq3_single_dimension_requirement.csv",
    "- results/rq3/rq3_unordered_sufficiency_thresholds.csv",
    "- data/derived/rq3/rq3_joint_distortion_long.rds",
    "- data/derived/rq3/rq3_joint_sufficiency_long.rds",
    "- results/rq3/rq3_joint_summary.csv",
    "- results/rq3/rq3_pareto_ever.csv",
    "- results/rq3/rq3_pareto_frequency.csv"
  ),
  file.path(OUT_RESULTS, "RQ3_RUN_REPORT.md")
)

message("RQ3 complete:")
message("  ", file.path(OUT_DATA, "rq3_single_dimension_sufficiency.rds"))
message("  ", file.path(OUT_RESULTS, "rq3_single_dimension_requirement.csv"))
message("  ", file.path(OUT_DATA, "rq3_joint_sufficiency_long.rds"))
message("  ", file.path(OUT_RESULTS, "rq3_pareto_ever.csv"))
