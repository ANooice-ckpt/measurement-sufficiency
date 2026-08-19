suppressPackageStartupMessages({
  library(tidyverse)
  library(nlme)
})

# RQ2 downstream analysis.
# Scientific source of truth: docs/STUDY_SPEC.md and the manuscript RQ2 methods.
# Inputs are RQ1 smallest-unit distortion plus reusable core artifacts only.
# This script never returns to the harmonized 10-s source.

RQ1_DISTORTION <- "data/derived/rq1/rq1_distortion_long.rds"
CORE_METRICS <- "data/derived/core/metric_cube.csv.gz"
CORE_CONTEXT <- "data/derived/core/unit_context.csv.gz"
OUT_DATA <- "data/derived/rq2"
OUT_RESULTS <- "results/rq2"
OUT_DIAG <- "results/diagnostics"

RQ2_BOOT <- suppressWarnings(as.integer(Sys.getenv("RQ2_BOOT", unset = "1000")))
if (!is.finite(RQ2_BOOT) || RQ2_BOOT < 0L) RQ2_BOOT <- 1000L
RQ2_CV_FOLDS <- suppressWarnings(as.integer(Sys.getenv("RQ2_CV_FOLDS", unset = "5")))
if (!is.finite(RQ2_CV_FOLDS) || RQ2_CV_FOLDS < 2L) RQ2_CV_FOLDS <- 5L
RQ2_RUN_MODELS <- !identical(Sys.getenv("RQ2_RUN_MODELS", unset = "1"), "0")
BOOT_SEED <- 20260820L
MODEL_SEED <- 20260821L
PRIMARY_TEMPORAL_S <- c(15L, 20L, 30L, 60L, 300L, 900L, 1800L)
DUAL_CHANNEL_METRICS <- c("MDER", "nvRD")
STATE_METRICS <- c("mean_MEDI", "MDER", "frequency_crossing_250")
NUMERIC_TOL <- 1e-12

for (p in c(RQ1_DISTORTION, CORE_METRICS, CORE_CONTEXT)) {
  if (!file.exists(p)) stop("Missing required input: ", p)
}
if (!requireNamespace("nlme", quietly = TRUE)) {
  stop("RQ2 requires the recommended R package 'nlme'.")
}
for (d in c(OUT_DATA, OUT_RESULTS, OUT_DIAG)) {
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
}

message("RQ2: read RQ1 and core artifacts")
rq1 <- readRDS(RQ1_DISTORTION) |>
  mutate(Date = as.Date(Date), window_start = as.Date(window_start), window_end = as.Date(window_end))
cube <- readr::read_csv(CORE_METRICS, show_col_types = FALSE, progress = FALSE) |>
  mutate(Date = as.Date(Date))
context <- readr::read_csv(CORE_CONTEXT, show_col_types = FALSE, progress = FALSE) |>
  mutate(Date = as.Date(Date))

required_rq1 <- c(
  "dimension", "configuration", "configuration_label", "configuration_order",
  "comparison_lattice", "support_id", "site", "Id", "analysis_unit_type",
  "analysis_unit_id", "Date", "window_id", "window_start", "window_end", "n_days",
  "metric", "metric_class", "metric_geometry", "reference_config_id",
  "reference_value", "candidate_value", "standardizer", "e", "available"
)
missing_rq1 <- setdiff(required_rq1, names(rq1))
if (length(missing_rq1)) stop("RQ1 distortion object missing columns: ", paste(missing_rq1, collapse = ", "))

safe_mean <- function(x) {
  x <- x[is.finite(x)]
  if (length(x)) mean(x) else NA_real_
}
safe_sd <- function(x) {
  x <- x[is.finite(x)]
  if (length(x) >= 2L) stats::sd(x) else NA_real_
}
safe_quantile <- function(x, p) {
  x <- x[is.finite(x)]
  if (!length(x)) return(NA_real_)
  unname(stats::quantile(x, p, names = FALSE, type = 7))
}
circular_mean <- function(x, period = 86400) {
  x <- x[is.finite(x)]
  if (!length(x)) return(NA_real_)
  theta <- 2 * pi * x / period
  (atan2(mean(sin(theta)), mean(cos(theta))) %% (2 * pi)) * period / (2 * pi)
}
circular_delta <- function(a, b, period = 86400) {
  ((a - b + period / 2) %% period) - period / 2
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

# Approximate local solar-noon elevation from latitude and day of year. This is a
# deterministic geometric descriptor available before personal measurement; it is
# not treated as measured personal light exposure.
solar_noon_elevation <- function(latitude, day_of_year) {
  decl <- 23.44 * sin(2 * pi * (284 + day_of_year) / 365.25)
  pmax(0, 90 - abs(latitude - decl))
}

# -----------------------------------------------------------------------------
# 1. Reference-exposure state and external context for each RQ1 distortion unit
# -----------------------------------------------------------------------------
message("RQ2: construct reference-state and external-context features")

state_daily <- cube |>
  filter(
    analysis_unit_type == "participant_day",
    metric %in% STATE_METRICS,
    available,
    is.finite(value)
  ) |>
  select(support_id, site, Id, Date, config_id, metric, value) |>
  distinct() |>
  pivot_wider(names_from = metric, values_from = value)

external_cols <- intersect(
  c(
    "era5_ssrd_daily_mean_w_m2", "era5_direct_fraction",
    "era5_total_cloud_cover_mean", "latitude", "day_of_year",
    "era5_context_available"
  ),
  names(context)
)
needed_external <- c(
  "era5_ssrd_daily_mean_w_m2", "era5_direct_fraction",
  "era5_total_cloud_cover_mean", "latitude", "day_of_year"
)
missing_external <- setdiff(needed_external, external_cols)
if (length(missing_external)) {
  stop("unit_context missing RQ2 external variables: ", paste(missing_external, collapse = ", "))
}

context_daily <- context |>
  select(
    support_id, site, Id, Date, config_id,
    all_of(external_cols)
  ) |>
  distinct()

feature_daily <- full_join(
  state_daily,
  context_daily,
  by = c("support_id", "site", "Id", "Date", "config_id")
) |>
  mutate(
    state_level = mean_MEDI,
    state_spectral = if_else(is.finite(MDER) & MDER > 0, log(MDER), NA_real_),
    state_dynamic = if_else(
      is.finite(frequency_crossing_250) & frequency_crossing_250 >= 0,
      log1p(frequency_crossing_250), NA_real_
    ),
    external_radiation = if_else(
      is.finite(era5_ssrd_daily_mean_w_m2) & era5_ssrd_daily_mean_w_m2 >= 0,
      log1p(era5_ssrd_daily_mean_w_m2), NA_real_
    ),
    external_direct_fraction = era5_direct_fraction,
    external_cloud = era5_total_cloud_cover_mean,
    solar_noon_elevation_deg = solar_noon_elevation(latitude, day_of_year)
  ) |>
  select(
    support_id, site, Id, Date, config_id,
    state_level, state_spectral, state_dynamic,
    external_radiation, external_direct_fraction, external_cloud,
    solar_noon_elevation_deg
  )

# Daily RQ1 units: context is tied to the exact comparison support and reference
# configuration used to create e.
daily_condition <- rq1 |>
  filter(analysis_unit_type == "participant_day") |>
  left_join(
    feature_daily,
    by = c(
      "support_id", "site", "Id", "Date",
      "reference_config_id" = "config_id"
    )
  )

# Participant-level multiday RQ1 units: use the mean daily state/context on the same
# support/reference configuration. This keeps IS/IV in RQ2 without pretending they
# have a participant-day exposure state.
feature_multiday <- feature_daily |>
  group_by(support_id, site, Id, config_id) |>
  summarise(
    across(
      c(
        state_level, state_spectral, state_dynamic,
        external_radiation, external_direct_fraction, external_cloud,
        solar_noon_elevation_deg
      ),
      safe_mean
    ),
    .groups = "drop"
  )

multiday_condition <- rq1 |>
  filter(analysis_unit_type == "participant_multiday") |>
  left_join(
    feature_multiday,
    by = c(
      "support_id", "site", "Id",
      "reference_config_id" = "config_id"
    )
  )

# Duration rows use synthetic config IDs, so reconstruct window context from the
# actual eye-MEDI 10-s core reference on the same support. The primary duration
# state is how atypical the selected window's mean light level is relative to the
# participant's seven-day reference, standardized by that participant's day-to-day
# variation. This varies across contiguous windows and directly represents
# cross-day exposure-state heterogeneity.
core_duration_ref_map <- cube |>
  filter(
    analysis_unit_type == "participant_day",
    placement == "eye", optical == "MEDI", resolution_s == 10L
  ) |>
  distinct(support_id, site, Id, config_id)

duration_units <- rq1 |>
  filter(analysis_unit_type == "participant_window") |>
  distinct(
    support_id, site, Id, analysis_unit_id, window_id,
    window_start, window_end, n_days
  ) |>
  left_join(core_duration_ref_map, by = c("support_id", "site", "Id"))

if (anyDuplicated(duration_units[c("support_id", "site", "Id", "analysis_unit_id")])) {
  stop("Duration RQ2 reference-config mapping is not unique")
}

duration_feature_list <- vector("list", nrow(duration_units))
for (i in seq_len(nrow(duration_units))) {
  u <- duration_units[i, ]
  all_days <- feature_daily |>
    filter(
      support_id == u$support_id,
      site == u$site,
      Id == u$Id,
      config_id == u$config_id
    ) |>
    arrange(Date)
  win <- all_days |>
    filter(Date >= u$window_start, Date <= u$window_end)

  ref_level_mean <- safe_mean(all_days$state_level)
  ref_level_sd <- safe_sd(all_days$state_level)
  win_level_mean <- safe_mean(win$state_level)
  departure <- if (
    is.finite(ref_level_sd) && ref_level_sd > sqrt(.Machine$double.eps) &&
      is.finite(win_level_mean) && is.finite(ref_level_mean)
  ) {
    abs(win_level_mean - ref_level_mean) / ref_level_sd
  } else NA_real_

  duration_feature_list[[i]] <- tibble(
    support_id = u$support_id,
    site = u$site,
    Id = u$Id,
    analysis_unit_id = u$analysis_unit_id,
    state_level = win_level_mean,
    state_spectral = safe_mean(win$state_spectral),
    state_dynamic = safe_mean(win$state_dynamic),
    state_window_departure = departure,
    reference_crossday_sd = ref_level_sd,
    external_radiation = safe_mean(win$external_radiation),
    external_direct_fraction = safe_mean(win$external_direct_fraction),
    external_cloud = safe_mean(win$external_cloud),
    solar_noon_elevation_deg = safe_mean(win$solar_noon_elevation_deg)
  )
}
duration_features <- bind_rows(duration_feature_list)

duration_condition <- rq1 |>
  filter(analysis_unit_type == "participant_window") |>
  left_join(
    duration_features,
    by = c("support_id", "site", "Id", "analysis_unit_id")
  )

condition <- bind_rows(daily_condition, multiday_condition, duration_condition) |>
  mutate(
    state_window_departure = if_else(
      dimension == "duration", state_window_departure, NA_real_
    ),
    primary_state_name = case_when(
      dimension == "placement" ~ "reference eye light level",
      dimension == "optical" ~ "melanopic-photopic ratio",
      dimension == "temporal" ~ "short-term crossing dynamics",
      dimension == "duration" ~ "window departure from seven-day mean",
      TRUE ~ NA_character_
    ),
    primary_state_raw = case_when(
      dimension == "placement" ~ state_level,
      dimension == "optical" ~ state_spectral,
      dimension == "temporal" ~ state_dynamic,
      dimension == "duration" ~ state_window_departure,
      TRUE ~ NA_real_
    ),
    abs_e = abs(e),
    participant_key = paste(site, Id, sep = "|")
  )

# Condition bins are metric-specific empirical tertiles of the relevant state.
# This avoids imposing universal physical cut-points across heterogeneous metrics
# and supports the manuscript's low/mid/high conditional A-B trajectories.
condition <- condition |>
  group_by(dimension, configuration, metric) |>
  mutate(
    state_bin = if (
      sum(is.finite(primary_state_raw)) >= 6L &&
        n_distinct(primary_state_raw[is.finite(primary_state_raw)]) >= 3L
    ) {
      ntile(primary_state_raw, 3L)
    } else NA_integer_,
    state_bin_label = factor(
      case_when(
        state_bin == 1L ~ "Low",
        state_bin == 2L ~ "Middle",
        state_bin == 3L ~ "High",
        TRUE ~ NA_character_
      ),
      levels = c("Low", "Middle", "High")
    )
  ) |>
  ungroup()

saveRDS(condition, file.path(OUT_DATA, "rq2_condition_long.rds"), compress = "xz")

conditional_geometry <- condition |>
  filter(available, is.finite(e), !is.na(state_bin_label)) |>
  group_by(
    dimension, configuration, configuration_label, configuration_order,
    metric, metric_class, metric_geometry, primary_state_name,
    state_bin, state_bin_label
  ) |>
  summarise(
    n_participants = n_distinct(participant_key),
    n_units = n(),
    state_median = median(primary_state_raw, na.rm = TRUE),
    state_q25 = safe_quantile(primary_state_raw, .25),
    state_q75 = safe_quantile(primary_state_raw, .75),
    B_conditional = mean(e),
    A_conditional = mean(abs(e)),
    median_e = median(e),
    p025_e = safe_quantile(e, .025),
    p975_e = safe_quantile(e, .975),
    .groups = "drop"
  )
readr::write_csv(
  conditional_geometry,
  file.path(OUT_RESULTS, "rq2_conditional_geometry.csv"),
  na = ""
)

# Anchor configurations keep Fig. 2 interpretable and the prediction workload
# finite: both placement alternatives, the optical proxy, and the most demanding
# primary alternative for each ordered dimension.
anchor_manifest <- rq1 |>
  filter(available) |>
  distinct(dimension, configuration, configuration_label, configuration_order) |>
  group_by(dimension) |>
  filter(
    dimension == "placement" |
      dimension == "optical" |
      configuration_order == max(configuration_order, na.rm = TRUE)
  ) |>
  ungroup() |>
  mutate(
    prediction_eligible = dimension != "duration",
    anchor_reason = case_when(
      dimension == "placement" ~ "both primary placement alternatives",
      dimension == "optical" ~ "single primary optical alternative",
      dimension == "temporal" ~ "coarsest primary temporal alternative",
      dimension == "duration" ~ "shortest primary monitoring duration",
      TRUE ~ "anchor"
    )
  )
readr::write_csv(anchor_manifest, file.path(OUT_RESULTS, "rq2_anchor_configurations.csv"), na = "")

# Algorithmically select one strongly state-dependent example per dimension for
# Fig. 2a. Score combines magnitude change and signed-location shift.
example_scores <- conditional_geometry |>
  semi_join(anchor_manifest, by = c("dimension", "configuration")) |>
  filter(state_bin %in% c(1L, 3L)) |>
  select(
    dimension, configuration, configuration_label,
    metric, metric_class, state_bin, A_conditional, B_conditional
  ) |>
  pivot_wider(
    names_from = state_bin,
    values_from = c(A_conditional, B_conditional),
    names_prefix = "q"
  ) |>
  filter(
    is.finite(A_conditional_q1), is.finite(A_conditional_q3),
    is.finite(B_conditional_q1), is.finite(B_conditional_q3)
  ) |>
  mutate(
    state_shift_score =
      abs(A_conditional_q3 - A_conditional_q1) +
      abs(B_conditional_q3 - B_conditional_q1)
  ) |>
  group_by(dimension) |>
  slice_max(state_shift_score, n = 1L, with_ties = FALSE) |>
  ungroup()
readr::write_csv(
  example_scores,
  file.path(OUT_RESULTS, "rq2_conditional_examples.csv"),
  na = ""
)

# -----------------------------------------------------------------------------
# 2. Mixed models and external predictability
# -----------------------------------------------------------------------------
message("RQ2: mixed models and out-of-sample predictability")

EXTERNAL_PREDICTORS <- c(
  "external_radiation", "external_direct_fraction",
  "external_cloud", "solar_noon_elevation_deg"
)
MODEL_FAMILIES <- list(
  external_context = EXTERNAL_PREDICTORS,
  exposure_state = "primary_state_raw",
  joint = c("primary_state_raw", EXTERNAL_PREDICTORS)
)
MODEL_OUTCOMES <- c(signed = "e", magnitude = "abs_e")

z_train_test <- function(train, test, predictors) {
  keep <- character()
  for (p in predictors) {
    mu <- mean(train[[p]], na.rm = TRUE)
    sig <- stats::sd(train[[p]], na.rm = TRUE)
    if (!is.finite(mu) || !is.finite(sig) || sig <= sqrt(.Machine$double.eps)) next
    train[[p]] <- (train[[p]] - mu) / sig
    test[[p]] <- (test[[p]] - mu) / sig
    keep <- c(keep, p)
  }
  list(train = train, test = test, predictors = keep)
}

fit_mixed <- function(dat, outcome, predictors) {
  if (!length(predictors)) return(list(fit = NULL, random_structure = NA_character_))
  f <- stats::reformulate(predictors, response = outcome)
  dat$site <- factor(dat$site)
  dat$participant_key <- factor(dat$participant_key)
  ctrl <- nlme::lmeControl(
    opt = "optim", maxIter = 100L, msMaxIter = 100L,
    returnObject = TRUE
  )
  fit <- tryCatch(
    suppressWarnings(nlme::lme(
      fixed = f,
      random = ~1 | site/participant_key,
      data = dat,
      method = "ML",
      na.action = na.omit,
      control = ctrl
    )),
    error = function(e) NULL
  )
  if (!is.null(fit)) return(list(fit = fit, random_structure = "site/participant"))

  fit <- tryCatch(
    suppressWarnings(nlme::lme(
      fixed = f,
      random = ~1 | participant_key,
      data = dat,
      method = "ML",
      na.action = na.omit,
      control = ctrl
    )),
    error = function(e) NULL
  )
  list(
    fit = fit,
    random_structure = if (is.null(fit)) NA_character_ else "participant"
  )
}

predict_fixed <- function(fit, newdata) {
  if (is.null(fit)) return(rep(NA_real_, nrow(newdata)))
  tryCatch(
    as.numeric(stats::predict(fit, newdata = newdata, level = 0)),
    error = function(e) rep(NA_real_, nrow(newdata))
  )
}

performance_metrics <- function(obs, pred) {
  ok <- is.finite(obs) & is.finite(pred)
  obs <- obs[ok]
  pred <- pred[ok]
  if (length(obs) < 2L) {
    return(tibble(n_test = length(obs), rmse = NA_real_, mae = NA_real_, r2 = NA_real_))
  }
  sst <- sum((obs - mean(obs))^2)
  tibble(
    n_test = length(obs),
    rmse = sqrt(mean((obs - pred)^2)),
    mae = mean(abs(obs - pred)),
    r2 = if (is.finite(sst) && sst > 0) 1 - sum((obs - pred)^2) / sst else NA_real_
  )
}

make_participant_folds <- function(dat, k) {
  p <- dat |>
    distinct(site, participant_key) |>
    group_by(site) |>
    mutate(
      random_order = sample.int(n()),
      fold = ((rank(random_order, ties.method = "first") - 1L) %% k) + 1L
    ) |>
    ungroup() |>
    select(site, participant_key, fold)
  left_join(dat, p, by = c("site", "participant_key"))
}

cv_predictions <- function(dat, outcome, predictors, scheme, k = RQ2_CV_FOLDS) {
  out <- vector("list", 0L)
  idx <- 0L

  if (scheme == "participant_grouped") {
    d <- make_participant_folds(dat, k)
    split_values <- sort(unique(d$fold))
    split_col <- "fold"
  } else if (scheme == "leave_site_out") {
    d <- dat |> mutate(.site_split = as.character(site))
    split_values <- sort(unique(d$.site_split))
    split_col <- ".site_split"
  } else stop("Unknown CV scheme: ", scheme)

  for (s in split_values) {
    test_flag <- d[[split_col]] == s
    train <- d[!test_flag, , drop = FALSE]
    test <- d[test_flag, , drop = FALSE]
    if (
      nrow(train) < 20L || nrow(test) < 2L ||
      n_distinct(train$participant_key) < 5L ||
      n_distinct(train$site) < 2L
    ) next

    scaled <- z_train_test(train, test, predictors)
    if (!length(scaled$predictors)) next
    fitted <- fit_mixed(scaled$train, outcome, scaled$predictors)
    pred <- predict_fixed(fitted$fit, scaled$test)

    idx <- idx + 1L
    out[[idx]] <- tibble(
      scheme = scheme,
      split = as.character(s),
      observed = scaled$test[[outcome]],
      predicted = pred,
      random_structure = fitted$random_structure
    )
  }
  bind_rows(out)
}

model_anchor <- condition |>
  filter(available, is.finite(e)) |>
  semi_join(
    anchor_manifest |> filter(prediction_eligible),
    by = c("dimension", "configuration")
  )

model_groups <- model_anchor |>
  distinct(
    dimension, configuration, configuration_label,
    metric, metric_class
  ) |>
  arrange(dimension, configuration, metric)

model_coef_list <- vector("list", 0L)
model_perf_list <- vector("list", 0L)
coef_i <- 0L
perf_i <- 0L

if (RQ2_RUN_MODELS) {
  set.seed(MODEL_SEED)
  for (gi in seq_len(nrow(model_groups))) {
    g <- model_groups[gi, ]
    dat0 <- model_anchor |>
      filter(
        dimension == g$dimension,
        configuration == g$configuration,
        metric == g$metric
      ) |>
      select(
        site, Id, participant_key, e, abs_e, primary_state_raw,
        all_of(EXTERNAL_PREDICTORS)
      )

    # Fair model-family comparison uses the same complete-case rows for all three
    # predictor sets within a metric x anchor-configuration group.
    dat0 <- dat0 |>
      filter(
        if_all(
          all_of(c("e", "abs_e", "primary_state_raw", EXTERNAL_PREDICTORS)),
          is.finite
        )
      )

    if (
      nrow(dat0) < 40L ||
      n_distinct(dat0$participant_key) < 10L ||
      n_distinct(dat0$site) < 3L
    ) next

    for (outcome_name in names(MODEL_OUTCOMES)) {
      outcome <- MODEL_OUTCOMES[[outcome_name]]
      for (family in names(MODEL_FAMILIES)) {
        predictors <- MODEL_FAMILIES[[family]]

        # Full-data model for coefficient table.
        scaled_full <- z_train_test(dat0, dat0, predictors)
        fit_full <- fit_mixed(scaled_full$train, outcome, scaled_full$predictors)
        if (!is.null(fit_full$fit)) {
          tt <- summary(fit_full$fit)$tTable
          coef_i <- coef_i + 1L
          model_coef_list[[coef_i]] <- tibble(
            dimension = g$dimension,
            configuration = g$configuration,
            configuration_label = g$configuration_label,
            metric = g$metric,
            metric_class = g$metric_class,
            outcome = outcome_name,
            model_family = family,
            random_structure = fit_full$random_structure,
            term = rownames(tt),
            estimate = tt[, "Value"],
            std_error = tt[, "Std.Error"],
            df = tt[, "DF"],
            t_value = tt[, "t-value"],
            p_value = tt[, "p-value"]
          )
        }

        for (scheme in c("participant_grouped", "leave_site_out")) {
          preds <- cv_predictions(dat0, outcome, predictors, scheme)
          if (!nrow(preds)) next
          perf <- performance_metrics(preds$observed, preds$predicted)
          perf_i <- perf_i + 1L
          model_perf_list[[perf_i]] <- bind_cols(
            tibble(
              dimension = g$dimension,
              configuration = g$configuration,
              configuration_label = g$configuration_label,
              metric = g$metric,
              metric_class = g$metric_class,
              outcome = outcome_name,
              model_family = family,
              validation_scheme = scheme,
              n_participants = n_distinct(dat0$participant_key),
              n_sites = n_distinct(dat0$site)
            ),
            perf
          )
        }
      }
    }
  }
}

model_coefficients <- bind_rows(model_coef_list)
model_performance <- bind_rows(model_perf_list)
if (!nrow(model_coefficients)) {
  model_coefficients <- tibble(
    dimension = character(), configuration = character(), configuration_label = character(),
    metric = character(), metric_class = character(), outcome = character(),
    model_family = character(), random_structure = character(), term = character(),
    estimate = numeric(), std_error = numeric(), df = numeric(), t_value = numeric(),
    p_value = numeric()
  )
}
if (!nrow(model_performance)) {
  model_performance <- tibble(
    dimension = character(), configuration = character(), configuration_label = character(),
    metric = character(), metric_class = character(), outcome = character(),
    model_family = character(), validation_scheme = character(),
    n_participants = integer(), n_sites = integer(), n_test = integer(),
    rmse = numeric(), mae = numeric(), r2 = numeric()
  )
}
readr::write_csv(model_coefficients, file.path(OUT_RESULTS, "rq2_model_coefficients.csv"), na = "")
readr::write_csv(model_performance, file.path(OUT_RESULTS, "rq2_model_performance.csv"), na = "")

# -----------------------------------------------------------------------------
# 3. Cross-dimensional separability: empirical second-order distortion gamma
# -----------------------------------------------------------------------------
message("RQ2: construct cross-dimensional second-order distortion")

cell_keys <- c(
  "support_id", "site", "Id", "analysis_unit_type", "analysis_unit_id", "metric"
)

cell <- function(z, prefix) {
  z |>
    select(
      all_of(cell_keys), Date, metric_class, metric_geometry,
      value, available, unavailable_reason
    ) |>
    rename_with(
      ~paste0(prefix, .x),
      c("value", "available", "unavailable_reason")
    )
}

make_gamma_cells <- function(z00, za0, z0b, zab, dimension_pair,
                             a_dimension, a_configuration, a_label,
                             b_dimension, b_configuration, b_label,
                             interaction_lattice) {
  c00 <- cell(z00, "m00_")
  ca0 <- cell(za0, "ma0_") |>
    select(all_of(cell_keys), ma0_value, ma0_available, ma0_unavailable_reason)
  c0b <- cell(z0b, "m0b_") |>
    select(all_of(cell_keys), m0b_value, m0b_available, m0b_unavailable_reason)
  cab <- cell(zab, "mab_") |>
    select(all_of(cell_keys), mab_value, mab_available, mab_unavailable_reason)

  c00 |>
    inner_join(ca0, by = cell_keys) |>
    inner_join(c0b, by = cell_keys) |>
    inner_join(cab, by = cell_keys) |>
    transmute(
      dimension_pair = dimension_pair,
      a_dimension = a_dimension,
      a_configuration = a_configuration,
      a_configuration_label = a_label,
      b_dimension = b_dimension,
      b_configuration = b_configuration,
      b_configuration_label = b_label,
      interaction_lattice = interaction_lattice,
      support_id, site, Id, analysis_unit_type, analysis_unit_id, Date,
      metric, metric_class, metric_geometry,
      m00 = m00_value, ma0 = ma0_value, m0b = m0b_value, mab = mab_value,
      cells_available =
        m00_available & ma0_available & m0b_available & mab_available &
        is.finite(m00_value) & is.finite(ma0_value) &
        is.finite(m0b_value) & is.finite(mab_value),
      cell_unavailable_reason = case_when(
        !m00_available | !is.finite(m00_value) ~ coalesce(m00_unavailable_reason, "reference cell unavailable"),
        !ma0_available | !is.finite(ma0_value) ~ coalesce(ma0_unavailable_reason, "a-only cell unavailable"),
        !m0b_available | !is.finite(m0b_value) ~ coalesce(m0b_unavailable_reason, "b-only cell unavailable"),
        !mab_available | !is.finite(mab_value) ~ coalesce(mab_unavailable_reason, "joint cell unavailable"),
        TRUE ~ NA_character_
      )
    )
}

gamma_blocks <- vector("list", 0L)
gb <- 0L

# Placement x optical: requires the corresponding *_full support.
for (pos in c("chest", "wrist")) {
  support <- paste0("eye_", pos, "_full")
  z <- cube |>
    filter(
      support_id == support,
      placement %in% c("eye", pos),
      optical %in% c("MEDI", "LIGHT"),
      resolution_s == 10L
    )
  gb <- gb + 1L
  gamma_blocks[[gb]] <- make_gamma_cells(
    z |> filter(placement == "eye", optical == "MEDI"),
    z |> filter(placement == pos, optical == "MEDI"),
    z |> filter(placement == "eye", optical == "LIGHT"),
    z |> filter(placement == pos, optical == "LIGHT"),
    dimension_pair = "placement × optical",
    a_dimension = "placement", a_configuration = pos,
    a_label = stringr::str_to_title(pos),
    b_dimension = "optical", b_configuration = "LIGHT",
    b_label = "Photopic illuminance",
    interaction_lattice = paste0("placement_optical_", pos)
  )
}

# Placement x temporal: use maximum pairwise support, with *_full only for the two
# intrinsically dual-channel target representations.
for (pos in c("chest", "wrist")) {
  for (r in PRIMARY_TEMPORAL_S) {
    for (support_type in c("medi", "full")) {
      support <- paste0("eye_", pos, "_", support_type)
      metric_filter <- if (support_type == "full") DUAL_CHANNEL_METRICS else setdiff(unique(cube$metric), DUAL_CHANNEL_METRICS)
      z <- cube |>
        filter(
          support_id == support,
          placement %in% c("eye", pos),
          optical == "MEDI",
          resolution_s %in% c(10L, r),
          metric %in% metric_filter
        )
      if (!nrow(z)) next
      gb <- gb + 1L
      gamma_blocks[[gb]] <- make_gamma_cells(
        z |> filter(placement == "eye", resolution_s == 10L),
        z |> filter(placement == pos, resolution_s == 10L),
        z |> filter(placement == "eye", resolution_s == r),
        z |> filter(placement == pos, resolution_s == r),
        dimension_pair = "placement × temporal",
        a_dimension = "placement", a_configuration = pos,
        a_label = stringr::str_to_title(pos),
        b_dimension = "temporal", b_configuration = paste0(r, "s"),
        b_label = if_else(r < 60L, paste0(r, " s"), paste0(r %/% 60L, " min")),
        interaction_lattice = paste0("placement_temporal_", pos, "_", support_type)
      )
    }
  }
}

# Optical x temporal: one common full-information support. LIGHT-only unavailable
# representations remain unavailable rather than being assigned extreme gamma.
for (r in PRIMARY_TEMPORAL_S) {
  z <- cube |>
    filter(
      support_id == "eye_full",
      placement == "eye",
      optical %in% c("MEDI", "LIGHT"),
      resolution_s %in% c(10L, r)
    )
  gb <- gb + 1L
  gamma_blocks[[gb]] <- make_gamma_cells(
    z |> filter(optical == "MEDI", resolution_s == 10L),
    z |> filter(optical == "LIGHT", resolution_s == 10L),
    z |> filter(optical == "MEDI", resolution_s == r),
    z |> filter(optical == "LIGHT", resolution_s == r),
    dimension_pair = "optical × temporal",
    a_dimension = "optical", a_configuration = "LIGHT",
    a_label = "Photopic illuminance",
    b_dimension = "temporal", b_configuration = paste0(r, "s"),
    b_label = if_else(r < 60L, paste0(r, " s"), paste0(r %/% 60L, " min")),
    interaction_lattice = "optical_temporal"
  )
}

gamma_cells <- bind_rows(gamma_blocks)
if (!nrow(gamma_cells)) stop("No observable RQ2 joint configuration cells were constructed")

# Standardizers are fixed within an interaction lattice and target representation.
gamma_standardizers <- gamma_cells |>
  filter(is.finite(m00)) |>
  distinct(
    interaction_lattice, metric, metric_geometry,
    site, Id, analysis_unit_id, m00
  ) |>
  group_by(interaction_lattice, metric, metric_geometry) |>
  summarise(
    n_reference_units = n(),
    standardizer = reference_scale(m00, first(metric_geometry)),
    .groups = "drop"
  ) |>
  mutate(
    zero_or_near_zero =
      !is.finite(standardizer) |
      standardizer <= sqrt(.Machine$double.eps)
  )
readr::write_csv(
  gamma_standardizers,
  file.path(OUT_DIAG, "rq2_gamma_standardizer_audit.csv"),
  na = ""
)

gamma_long <- gamma_cells |>
  left_join(
    gamma_standardizers,
    by = c("interaction_lattice", "metric", "metric_geometry")
  ) |>
  mutate(
    marginal_a_ref_delta = if_else(
      metric_geometry == "circular_time",
      circular_delta(ma0, m00),
      ma0 - m00
    ),
    marginal_a_at_b_delta = if_else(
      metric_geometry == "circular_time",
      circular_delta(mab, m0b),
      mab - m0b
    ),
    gamma_delta = marginal_a_at_b_delta - marginal_a_ref_delta,
    available =
      cells_available & !replace_na(zero_or_near_zero, TRUE) &
      is.finite(gamma_delta) & is.finite(standardizer),
    unavailable_reason = case_when(
      !cells_available ~ cell_unavailable_reason,
      replace_na(zero_or_near_zero, TRUE) ~ "joint-reference dispersion zero or undefined",
      !is.finite(gamma_delta) ~ "second-order representation contrast undefined",
      TRUE ~ NA_character_
    ),
    gamma = if_else(available, gamma_delta / standardizer, NA_real_),
    marginal_a_ref = if_else(available, marginal_a_ref_delta / standardizer, NA_real_),
    marginal_a_at_b = if_else(available, marginal_a_at_b_delta / standardizer, NA_real_),
    participant_key = paste(site, Id, sep = "|")
  )

saveRDS(gamma_long, file.path(OUT_DATA, "rq2_gamma_long.rds"), compress = "xz")

bootstrap_gamma <- function(g, B = RQ2_BOOT) {
  site_counts <- g |>
    distinct(site, Id) |>
    count(site, name = "n_participants")
  supported <-
    B > 0L &&
    n_distinct(g$participant_key) >= 2L &&
    any(site_counts$n_participants > 1L)
  if (!supported) {
    return(tibble(
      bootstrap_supported = FALSE,
      R_ci_low = NA_real_, R_ci_high = NA_real_,
      Q_ci_low = NA_real_, Q_ci_high = NA_real_
    ))
  }

  clusters <- g |>
    group_by(site, Id) |>
    summarise(sum_g = sum(gamma), sum_abs_g = sum(abs(gamma)), n = n(), .groups = "drop")
  by_site <- split(clusters, clusters$site)
  vals <- replicate(B, {
    sampled <- map_dfr(by_site, function(z) {
      z[sample.int(nrow(z), nrow(z), replace = TRUE), , drop = FALSE]
    })
    c(
      R = sum(sampled$sum_g) / sum(sampled$n),
      Q = sum(sampled$sum_abs_g) / sum(sampled$n)
    )
  })
  tibble(
    bootstrap_supported = TRUE,
    R_ci_low = safe_quantile(vals["R", ], .025),
    R_ci_high = safe_quantile(vals["R", ], .975),
    Q_ci_low = safe_quantile(vals["Q", ], .025),
    Q_ci_high = safe_quantile(vals["Q", ], .975)
  )
}

gamma_available <- gamma_long |> filter(available, is.finite(gamma))

gamma_summary_base <- gamma_available |>
  group_by(
    dimension_pair, a_dimension, a_configuration, a_configuration_label,
    b_dimension, b_configuration, b_configuration_label,
    interaction_lattice, metric, metric_class, metric_geometry
  ) |>
  summarise(
    n_participants = n_distinct(participant_key),
    n_units = n(),
    median_gamma = median(gamma),
    q25_gamma = safe_quantile(gamma, .25),
    q75_gamma = safe_quantile(gamma, .75),
    p025_gamma = safe_quantile(gamma, .025),
    p975_gamma = safe_quantile(gamma, .975),
    R_mean_signed = mean(gamma),
    Q_mean_absolute = mean(abs(gamma)),
    marginal_a_ref_mean = mean(marginal_a_ref),
    marginal_a_at_b_mean = mean(marginal_a_at_b),
    .groups = "drop"
  )

set.seed(BOOT_SEED)
gamma_cis <- gamma_available |>
  group_by(
    dimension_pair, a_dimension, a_configuration, a_configuration_label,
    b_dimension, b_configuration, b_configuration_label,
    interaction_lattice, metric, metric_class, metric_geometry
  ) |>
  group_modify(~bootstrap_gamma(.x, B = RQ2_BOOT)) |>
  ungroup()

gamma_summary <- gamma_summary_base |>
  left_join(
    gamma_cis,
    by = c(
      "dimension_pair", "a_dimension", "a_configuration", "a_configuration_label",
      "b_dimension", "b_configuration", "b_configuration_label",
      "interaction_lattice", "metric", "metric_class", "metric_geometry"
    )
  ) |>
  mutate(
    geometry_gap = Q_mean_absolute - abs(R_mean_signed),
    geometry_pass = Q_mean_absolute + NUMERIC_TOL >= abs(R_mean_signed)
  )
if (any(!gamma_summary$geometry_pass)) stop("RQ2 gamma invariant Q >= |R| failed")
readr::write_csv(gamma_summary, file.path(OUT_RESULTS, "rq2_gamma_summary.csv"), na = "")

pair_summary <- gamma_summary |>
  group_by(dimension_pair) |>
  summarise(
    n_metric_configuration_pairs = n(),
    median_Q = median(Q_mean_absolute, na.rm = TRUE),
    q25_Q = safe_quantile(Q_mean_absolute, .25),
    q75_Q = safe_quantile(Q_mean_absolute, .75),
    median_abs_R = median(abs(R_mean_signed), na.rm = TRUE),
    .groups = "drop"
  ) |>
  arrange(desc(median_Q))
readr::write_csv(pair_summary, file.path(OUT_RESULTS, "rq2_gamma_pair_summary.csv"), na = "")

# Fig. 3b archetypes selected algorithmically from Q and |R|/Q geometry.
elig <- gamma_summary |>
  filter(n_participants >= 3L, is.finite(Q_mean_absolute), Q_mean_absolute > 0) |>
  mutate(
    direction_ratio = abs(R_mean_signed) / Q_mean_absolute,
    id = paste(dimension_pair, a_configuration, b_configuration, metric, sep = " | ")
  )
low_ex <- elig |> arrange(Q_mean_absolute, abs(R_mean_signed)) |> slice(1) |> mutate(example_type = "high separability")
pos_ex <- elig |>
  filter(R_mean_signed > 0) |>
  mutate(score = percent_rank(Q_mean_absolute) + percent_rank(direction_ratio)) |>
  arrange(desc(score)) |> slice(1) |> mutate(example_type = "positive dependence")
neg_ex <- elig |>
  filter(R_mean_signed < 0) |>
  mutate(score = percent_rank(Q_mean_absolute) + percent_rank(direction_ratio)) |>
  arrange(desc(score)) |> slice(1) |> mutate(example_type = "negative dependence")
bi_ex <- elig |>
  mutate(score = percent_rank(Q_mean_absolute) + percent_rank(1 - direction_ratio)) |>
  arrange(desc(score)) |> slice(1) |> mutate(example_type = "bidirectional dependence")
gamma_examples <- bind_rows(low_ex, pos_ex, neg_ex, bi_ex) |>
  distinct(id, .keep_all = TRUE)
readr::write_csv(gamma_examples, file.path(OUT_RESULTS, "rq2_gamma_examples.csv"), na = "")

strong_coupling <- gamma_summary |>
  arrange(desc(Q_mean_absolute)) |>
  slice(1)
readr::write_csv(strong_coupling, file.path(OUT_RESULTS, "rq2_strong_coupling_example.csv"), na = "")

# Cross-dimensional analysis is intentionally limited to joint configurations
# directly materialized with adequate support in the core cube. Duration joint
# contrasts are not given population-level inference under the current exact-7-day
# cohort of three participants.
interaction_scope <- tibble(
  dimension_pair = c(
    "placement × optical", "placement × temporal", "optical × temporal",
    "placement × duration", "optical × duration", "temporal × duration"
  ),
  primary_status = c(
    "estimated", "estimated", "estimated",
    "not population-estimated", "not population-estimated", "not population-estimated"
  ),
  reason = c(
    rep("observable joint core configurations", 3),
    rep("strict duration reference cohort has only three eligible participants", 3)
  )
)
readr::write_csv(interaction_scope, file.path(OUT_RESULTS, "rq2_interaction_scope.csv"), na = "")

# Diagnostics and completion report.
condition_audit <- condition |>
  group_by(dimension, configuration) |>
  summarise(
    n_rows = n(),
    n_available_e = sum(available & is.finite(e)),
    n_primary_state = sum(is.finite(primary_state_raw)),
    n_external_complete = sum(
      is.finite(external_radiation) &
      is.finite(external_direct_fraction) &
      is.finite(external_cloud) &
      is.finite(solar_noon_elevation_deg)
    ),
    .groups = "drop"
  )
readr::write_csv(condition_audit, file.path(OUT_DIAG, "rq2_condition_feature_audit.csv"), na = "")

writeLines(
  c(
    "# RQ2 run report",
    "",
    sprintf("Generated: %s", Sys.time()),
    "",
    "Inputs:",
    paste0("- ", RQ1_DISTORTION),
    paste0("- ", CORE_METRICS),
    paste0("- ", CORE_CONTEXT),
    "",
    "Conditionality:",
    "- placement state: reference eye light level (mean_MEDI)",
    "- optical state: log melanopic-photopic ratio (MDER)",
    "- temporal state: log1p frequency crossings at 250 lux-equivalent threshold",
    "- duration state: selected-window departure from participant seven-day mean",
    "- external predictors: daily solar radiation, direct fraction, cloud cover, approximate solar-noon elevation",
    sprintf("- grouped participant CV folds: %d", RQ2_CV_FOLDS),
    sprintf("- prediction models executed: %s", RQ2_RUN_MODELS),
    "",
    "Cross-dimensional separability:",
    "- primary joint pairs: placement x optical, placement x temporal, optical x temporal",
    "- duration-containing pairs are not population-estimated because the strict seven-day cohort has n=3",
    sprintf("- gamma bootstrap replicates where supported: %d", RQ2_BOOT),
    "",
    "Artifacts:",
    "- data/derived/rq2/rq2_condition_long.rds",
    "- data/derived/rq2/rq2_gamma_long.rds",
    "- results/rq2/rq2_conditional_geometry.csv",
    "- results/rq2/rq2_model_coefficients.csv",
    "- results/rq2/rq2_model_performance.csv",
    "- results/rq2/rq2_gamma_summary.csv",
    "- results/rq2/rq2_gamma_pair_summary.csv"
  ),
  file.path(OUT_RESULTS, "RQ2_RUN_REPORT.md")
)

message("RQ2 complete.")
message("  ", file.path(OUT_DATA, "rq2_condition_long.rds"))
message("  ", file.path(OUT_DATA, "rq2_gamma_long.rds"))
message("  ", file.path(OUT_RESULTS, "rq2_conditional_geometry.csv"))
message("  ", file.path(OUT_RESULTS, "rq2_gamma_summary.csv"))
