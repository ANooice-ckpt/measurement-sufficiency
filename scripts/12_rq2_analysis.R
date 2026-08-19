suppressPackageStartupMessages({
  library(tidyverse)
  library(nlme)
})

# RQ2 downstream analysis.
# Scientific source of truth: docs/STUDY_SPEC.md and the manuscript RQ2 methods.
# Inputs are RQ1 smallest-unit distortion plus reusable core artifacts only.
# This script never returns to the harmonized 10-s source.
#
# Runtime design:
# - mixed-model work is parallelized across independent metric x configuration groups;
# - each group is checkpointed independently and can be resumed after interruption;
# - grouped-CV folds are fixed once per group and shared by every model family/outcome;
# - gamma bootstrap is vectorized within group and parallelized across groups;
# - BLAS/OpenMP threads are forced to one per worker to avoid oversubscription.

RQ1_DISTORTION <- "data/derived/rq1/rq1_distortion_long.rds"
CORE_METRICS <- "data/derived/core/metric_cube.csv.gz"
CORE_CONTEXT <- "data/derived/core/unit_context.csv.gz"
OUT_DATA <- "data/derived/rq2"
OUT_RESULTS <- "results/rq2"
OUT_DIAG <- "results/diagnostics"
INTERIM <- "data/interim/rq2"

RQ2_BOOT <- suppressWarnings(as.integer(Sys.getenv("RQ2_BOOT", unset = "1000")))
if (!is.finite(RQ2_BOOT) || RQ2_BOOT < 0L) RQ2_BOOT <- 1000L
RQ2_CV_FOLDS <- suppressWarnings(as.integer(Sys.getenv("RQ2_CV_FOLDS", unset = "5")))
if (!is.finite(RQ2_CV_FOLDS) || RQ2_CV_FOLDS < 2L) RQ2_CV_FOLDS <- 5L
RQ2_RUN_MODELS <- !identical(Sys.getenv("RQ2_RUN_MODELS", unset = "1"), "0")
RQ2_FORCE <- identical(Sys.getenv("RQ2_FORCE", unset = "0"), "1")

physical_cores <- suppressWarnings(parallel::detectCores(logical = FALSE))
logical_cores <- suppressWarnings(parallel::detectCores(logical = TRUE))
if (!is.finite(physical_cores) || physical_cores < 1L) physical_cores <- logical_cores
if (!is.finite(physical_cores) || physical_cores < 1L) physical_cores <- 2L
auto_workers <- max(1L, min(12L, as.integer(physical_cores) - 2L))
requested_workers <- suppressWarnings(as.integer(Sys.getenv("RQ2_WORKERS", unset = as.character(auto_workers))))
if (!is.finite(requested_workers) || requested_workers < 1L) requested_workers <- auto_workers
RQ2_WORKERS <- max(1L, min(requested_workers, logical_cores %||% requested_workers))
RQ2_BATCH_MULT <- suppressWarnings(as.integer(Sys.getenv("RQ2_BATCH_MULT", unset = "2")))
if (!is.finite(RQ2_BATCH_MULT) || RQ2_BATCH_MULT < 1L) RQ2_BATCH_MULT <- 2L

BOOT_SEED <- 20260820L
MODEL_SEED <- 20260821L
PRIMARY_TEMPORAL_S <- c(15L, 20L, 30L, 60L, 300L, 900L, 1800L)
DUAL_CHANNEL_METRICS <- c("MDER", "nvRD")
STATE_METRICS <- c("mean_MEDI", "MDER", "frequency_crossing_250")
NUMERIC_TOL <- 1e-12
MODEL_CHECKPOINT_VERSION <- paste0("v3_psock_fixedfold_f", RQ2_CV_FOLDS)
GAMMA_CHECKPOINT_VERSION <- paste0("v2_vectorized_b", RQ2_BOOT)
MODEL_CKPT_DIR <- file.path(INTERIM, "models", MODEL_CHECKPOINT_VERSION)
GAMMA_CKPT_DIR <- file.path(INTERIM, "gamma_bootstrap", GAMMA_CHECKPOINT_VERSION)

Sys.setenv(
  OMP_NUM_THREADS = "1",
  OPENBLAS_NUM_THREADS = "1",
  MKL_NUM_THREADS = "1",
  VECLIB_MAXIMUM_THREADS = "1",
  NUMEXPR_NUM_THREADS = "1"
)

for (p in c(RQ1_DISTORTION, CORE_METRICS, CORE_CONTEXT)) {
  if (!file.exists(p)) stop("Missing required input: ", p)
}
if (!requireNamespace("nlme", quietly = TRUE)) stop("RQ2 requires the recommended R package 'nlme'.")
for (d in c(OUT_DATA, OUT_RESULTS, OUT_DIAG, MODEL_CKPT_DIR, GAMMA_CKPT_DIR)) {
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
}

message(
  "RQ2 runtime: workers=", RQ2_WORKERS,
  ", CV folds=", RQ2_CV_FOLDS,
  ", gamma bootstrap=", RQ2_BOOT,
  ", force=", RQ2_FORCE
)

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
solar_noon_elevation <- function(latitude, day_of_year) {
  decl <- 23.44 * sin(2 * pi * (284 + day_of_year) / 365.25)
  pmax(0, 90 - abs(latitude - decl))
}
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0L || all(is.na(x))) y else x

checkpoint_valid <- function(path, version) {
  if (!file.exists(path)) return(FALSE)
  x <- tryCatch(readRDS(path), error = function(e) NULL)
  !is.null(x) && identical(x$checkpoint_version, version) && isTRUE(x$complete)
}

atomic_save_rds <- function(object, path) {
  tmp <- paste0(path, ".tmp_", Sys.getpid(), "_", sprintf("%08x", sample.int(.Machine$integer.max, 1L)))
  saveRDS(object, tmp, compress = FALSE)
  if (file.exists(path)) unlink(path)
  ok <- file.rename(tmp, path)
  if (!ok) {
    ok <- file.copy(tmp, path, overwrite = TRUE)
    unlink(tmp)
  }
  if (!ok) stop("Could not finalize checkpoint: ", path)
  invisible(path)
}

run_in_batches <- function(tasks, runner, workers, label) {
  if (!length(tasks)) return(list())
  workers <- max(1L, min(workers, length(tasks)))
  batch_size <- max(1L, workers * RQ2_BATCH_MULT)
  chunks <- split(seq_along(tasks), ceiling(seq_along(tasks) / batch_size))
  out <- vector("list", length(tasks))
  completed <- 0L

  cl <- NULL
  if (workers > 1L) {
    cl <- parallel::makePSOCKcluster(workers)
    parallel::clusterCall(cl, function() {
      Sys.setenv(
        OMP_NUM_THREADS = "1", OPENBLAS_NUM_THREADS = "1",
        MKL_NUM_THREADS = "1", VECLIB_MAXIMUM_THREADS = "1",
        NUMEXPR_NUM_THREADS = "1"
      )
      NULL
    })
  }
  on_exit <- function() {
    if (!is.null(cl)) try(parallel::stopCluster(cl), silent = TRUE)
  }

  tryCatch({
    for (chunk in chunks) {
      batch <- tasks[chunk]
      ans <- if (is.null(cl)) {
        lapply(batch, runner)
      } else {
        parallel::parLapplyLB(cl, batch, runner)
      }
      out[chunk] <- ans
      completed <- completed + length(chunk)
      message(sprintf("[%s] %d/%d pending tasks completed", label, completed, length(tasks)))
    }
  }, finally = on_exit())
  out
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
  select(support_id, site, Id, Date, config_id, all_of(external_cols)) |>
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

daily_condition <- rq1 |>
  filter(analysis_unit_type == "participant_day") |>
  left_join(
    feature_daily,
    by = c(
      "support_id", "site", "Id", "Date",
      "reference_config_id" = "config_id"
    )
  )

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
  win <- all_days |> filter(Date >= u$window_start, Date <= u$window_end)

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
  ) |>
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
readr::write_csv(conditional_geometry, file.path(OUT_RESULTS, "rq2_conditional_geometry.csv"), na = "")

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
readr::write_csv(example_scores, file.path(OUT_RESULTS, "rq2_conditional_examples.csv"), na = "")

# -----------------------------------------------------------------------------
# 2. Mixed models and external predictability: parallel + resumable
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

empty_model_coefficients <- function() {
  tibble(
    dimension = character(), configuration = character(), configuration_label = character(),
    metric = character(), metric_class = character(), outcome = character(),
    model_family = character(), random_structure = character(), term = character(),
    estimate = numeric(), std_error = numeric(), df = numeric(), t_value = numeric(),
    p_value = numeric()
  )
}
empty_model_performance <- function() {
  tibble(
    dimension = character(), configuration = character(), configuration_label = character(),
    metric = character(), metric_class = character(), outcome = character(),
    model_family = character(), validation_scheme = character(),
    n_participants = integer(), n_sites = integer(), n_test = integer(),
    rmse = numeric(), mae = numeric(), r2 = numeric()
  )
}

# The worker is deliberately self-contained so PSOCK workers do not need the
# parent global environment. This works on Windows as well as Unix-like systems.
run_model_task <- function(task) {
  t0 <- proc.time()[[3]]
  tryCatch({
    dat0 <- task$data
    meta <- task$meta

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
      list(fit = fit, random_structure = if (is.null(fit)) NA_character_ else "participant")
    }

    predict_fixed <- function(fit, newdata) {
      if (is.null(fit)) return(rep(NA_real_, nrow(newdata)))
      tryCatch(
        as.numeric(stats::predict(fit, newdata = newdata, level = 0)),
        error = function(e) rep(NA_real_, nrow(newdata))
      )
    }

    perf <- function(obs, pred) {
      ok <- is.finite(obs) & is.finite(pred)
      obs <- obs[ok]
      pred <- pred[ok]
      if (length(obs) < 2L) {
        return(data.frame(n_test = length(obs), rmse = NA_real_, mae = NA_real_, r2 = NA_real_))
      }
      sst <- sum((obs - mean(obs))^2)
      data.frame(
        n_test = length(obs),
        rmse = sqrt(mean((obs - pred)^2)),
        mae = mean(abs(obs - pred)),
        r2 = if (is.finite(sst) && sst > 0) 1 - sum((obs - pred)^2) / sst else NA_real_
      )
    }

    # One deterministic participant fold map per metric x configuration group.
    # All model families and both outcomes use this same map.
    make_fold_map <- function(dat, k, seed) {
      p <- unique(dat[c("site", "participant_key")])
      p$fold <- NA_integer_
      set.seed(seed)
      for (s in sort(unique(p$site))) {
        ii <- which(p$site == s)
        ord <- sample(ii, length(ii), replace = FALSE)
        p$fold[ord] <- ((seq_along(ord) - 1L) %% k) + 1L
      }
      p
    }

    cv_performance <- function(dat, outcome, predictors, scheme, fold_map) {
      if (scheme == "participant_grouped") {
        d <- merge(dat, fold_map, by = c("site", "participant_key"), all.x = TRUE, sort = FALSE)
        split_values <- sort(unique(d$fold[is.finite(d$fold)]))
        split_vector <- d$fold
      } else if (scheme == "leave_site_out") {
        d <- dat
        split_values <- sort(unique(as.character(d$site)))
        split_vector <- as.character(d$site)
      } else stop("Unknown CV scheme")

      obs_all <- numeric()
      pred_all <- numeric()
      structures <- character()
      for (s in split_values) {
        test_flag <- split_vector == s
        train <- d[!test_flag, , drop = FALSE]
        test <- d[test_flag, , drop = FALSE]
        if (
          nrow(train) < 20L || nrow(test) < 2L ||
          length(unique(train$participant_key)) < 5L ||
          length(unique(train$site)) < 2L
        ) next
        scaled <- z_train_test(train, test, predictors)
        if (!length(scaled$predictors)) next
        fitted <- fit_mixed(scaled$train, outcome, scaled$predictors)
        pred <- predict_fixed(fitted$fit, scaled$test)
        obs_all <- c(obs_all, scaled$test[[outcome]])
        pred_all <- c(pred_all, pred)
        structures <- c(structures, fitted$random_structure)
      }
      list(
        performance = perf(obs_all, pred_all),
        random_structure = paste(sort(unique(na.omit(structures))), collapse = "+")
      )
    }

    fold_map <- make_fold_map(dat0, task$cv_folds, task$seed)
    coef_rows <- list()
    perf_rows <- list()
    ci <- 0L
    pi <- 0L

    for (outcome_name in names(task$model_outcomes)) {
      outcome <- task$model_outcomes[[outcome_name]]
      for (family in names(task$model_families)) {
        predictors <- task$model_families[[family]]

        scaled_full <- z_train_test(dat0, dat0, predictors)
        fit_full <- fit_mixed(scaled_full$train, outcome, scaled_full$predictors)
        if (!is.null(fit_full$fit)) {
          tt <- summary(fit_full$fit)$tTable
          ci <- ci + 1L
          coef_rows[[ci]] <- data.frame(
            dimension = meta$dimension,
            configuration = meta$configuration,
            configuration_label = meta$configuration_label,
            metric = meta$metric,
            metric_class = meta$metric_class,
            outcome = outcome_name,
            model_family = family,
            random_structure = fit_full$random_structure,
            term = rownames(tt),
            estimate = tt[, "Value"],
            std_error = tt[, "Std.Error"],
            df = tt[, "DF"],
            t_value = tt[, "t-value"],
            p_value = tt[, "p-value"],
            row.names = NULL,
            check.names = FALSE
          )
        }

        for (scheme in c("participant_grouped", "leave_site_out")) {
          cv <- cv_performance(dat0, outcome, predictors, scheme, fold_map)
          pp <- cv$performance
          pi <- pi + 1L
          perf_rows[[pi]] <- data.frame(
            dimension = meta$dimension,
            configuration = meta$configuration,
            configuration_label = meta$configuration_label,
            metric = meta$metric,
            metric_class = meta$metric_class,
            outcome = outcome_name,
            model_family = family,
            validation_scheme = scheme,
            n_participants = length(unique(dat0$participant_key)),
            n_sites = length(unique(dat0$site)),
            n_test = pp$n_test,
            rmse = pp$rmse,
            mae = pp$mae,
            r2 = pp$r2,
            row.names = NULL
          )
        }
      }
    }

    coefficient_df <- if (length(coef_rows)) do.call(rbind, coef_rows) else data.frame()
    performance_df <- if (length(perf_rows)) do.call(rbind, perf_rows) else data.frame()
    elapsed <- proc.time()[[3]] - t0
    audit <- data.frame(
      group_index = task$group_index,
      dimension = meta$dimension,
      configuration = meta$configuration,
      metric = meta$metric,
      n_rows = nrow(dat0),
      n_participants = length(unique(dat0$participant_key)),
      n_sites = length(unique(dat0$site)),
      n_coefficient_rows = nrow(coefficient_df),
      n_performance_rows = nrow(performance_df),
      elapsed_seconds = elapsed,
      status = "completed",
      error = NA_character_,
      stringsAsFactors = FALSE
    )
    object <- list(
      checkpoint_version = task$checkpoint_version,
      complete = TRUE,
      coefficients = coefficient_df,
      performance = performance_df,
      audit = audit
    )
    tmp <- paste0(task$checkpoint_path, ".tmp_", Sys.getpid())
    saveRDS(object, tmp, compress = FALSE)
    if (file.exists(task$checkpoint_path)) unlink(task$checkpoint_path)
    ok <- file.rename(tmp, task$checkpoint_path)
    if (!ok) {
      ok <- file.copy(tmp, task$checkpoint_path, overwrite = TRUE)
      unlink(tmp)
    }
    if (!ok) stop("Could not write model checkpoint")
    list(ok = TRUE, checkpoint_path = task$checkpoint_path, error = NA_character_)
  }, error = function(e) {
    list(ok = FALSE, checkpoint_path = task$checkpoint_path, error = conditionMessage(e))
  })
}

model_anchor <- condition |>
  filter(available, is.finite(e)) |>
  semi_join(
    anchor_manifest |> filter(prediction_eligible),
    by = c("dimension", "configuration")
  )

model_groups <- model_anchor |>
  distinct(dimension, configuration, configuration_label, metric, metric_class) |>
  arrange(dimension, configuration, metric) |>
  mutate(group_index = row_number())

model_tasks <- list()
model_task_manifest <- list()
model_ineligible <- list()
mt <- 0L
mi <- 0L

if (RQ2_RUN_MODELS && nrow(model_groups)) {
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
      ) |>
      filter(
        if_all(
          all_of(c("e", "abs_e", "primary_state_raw", EXTERNAL_PREDICTORS)),
          is.finite
        )
      )

    eligible <-
      nrow(dat0) >= 40L &&
      n_distinct(dat0$participant_key) >= 10L &&
      n_distinct(dat0$site) >= 3L

    ckpt <- file.path(MODEL_CKPT_DIR, sprintf("group_%04d.rds", g$group_index))
    if (!eligible) {
      mi <- mi + 1L
      model_ineligible[[mi]] <- tibble(
        group_index = g$group_index,
        dimension = g$dimension,
        configuration = g$configuration,
        metric = g$metric,
        n_rows = nrow(dat0),
        n_participants = n_distinct(dat0$participant_key),
        n_sites = n_distinct(dat0$site),
        n_coefficient_rows = 0L,
        n_performance_rows = 0L,
        elapsed_seconds = 0,
        status = "ineligible",
        error = "requires >=40 complete rows, >=10 participants, and >=3 sites",
        checkpoint_reused = FALSE
      )
      next
    }

    reused <- !RQ2_FORCE && checkpoint_valid(ckpt, MODEL_CHECKPOINT_VERSION)
    mt <- mt + 1L
    model_task_manifest[[mt]] <- tibble(
      group_index = g$group_index,
      checkpoint_path = ckpt,
      checkpoint_reused = reused
    )
    if (!reused) {
      model_tasks[[length(model_tasks) + 1L]] <- list(
        group_index = g$group_index,
        meta = list(
          dimension = g$dimension,
          configuration = g$configuration,
          configuration_label = g$configuration_label,
          metric = g$metric,
          metric_class = g$metric_class
        ),
        data = as.data.frame(dat0),
        cv_folds = RQ2_CV_FOLDS,
        seed = MODEL_SEED + g$group_index * 1009L,
        model_families = MODEL_FAMILIES,
        model_outcomes = MODEL_OUTCOMES,
        checkpoint_version = MODEL_CHECKPOINT_VERSION,
        checkpoint_path = ckpt
      )
    }
  }
}

model_task_manifest <- bind_rows(model_task_manifest)
model_ineligible <- bind_rows(model_ineligible)
reused_n <- if (nrow(model_task_manifest)) sum(model_task_manifest$checkpoint_reused) else 0L
message(
  "RQ2 models: groups=", nrow(model_groups),
  ", eligible=", nrow(model_task_manifest),
  ", reused=", reused_n,
  ", pending=", length(model_tasks)
)

model_run_results <- if (RQ2_RUN_MODELS) {
  run_in_batches(model_tasks, run_model_task, RQ2_WORKERS, "models")
} else list()
model_errors <- keep(model_run_results, ~!isTRUE(.x$ok))
if (length(model_errors)) {
  err_tbl <- map_dfr(model_errors, ~tibble(checkpoint_path = .x$checkpoint_path, error = .x$error))
  readr::write_csv(err_tbl, file.path(OUT_DIAG, "rq2_model_worker_errors.csv"), na = "")
  stop("One or more RQ2 model workers failed; inspect results/diagnostics/rq2_model_worker_errors.csv")
}

model_objects <- list()
if (nrow(model_task_manifest)) {
  for (i in seq_len(nrow(model_task_manifest))) {
    p <- model_task_manifest$checkpoint_path[i]
    if (!checkpoint_valid(p, MODEL_CHECKPOINT_VERSION)) {
      stop("Expected model checkpoint is missing or invalid: ", p)
    }
    model_objects[[i]] <- readRDS(p)
  }
}

coef_parts <- map(model_objects, "coefficients")
perf_parts <- map(model_objects, "performance")
model_coefficients <- bind_rows(coef_parts)
model_performance <- bind_rows(perf_parts)
if (!nrow(model_coefficients)) model_coefficients <- empty_model_coefficients()
if (!nrow(model_performance)) model_performance <- empty_model_performance()

model_audit_completed <- map_dfr(seq_along(model_objects), function(i) {
  z <- as_tibble(model_objects[[i]]$audit)
  z$checkpoint_reused <- model_task_manifest$checkpoint_reused[i]
  z
})
model_audit <- bind_rows(model_audit_completed, model_ineligible) |>
  arrange(group_index)
readr::write_csv(model_audit, file.path(OUT_DIAG, "rq2_model_task_audit.csv"), na = "")
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
    rename_with(~paste0(prefix, .x), c("value", "available", "unavailable_reason"))
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

all_metrics <- unique(cube$metric)
for (pos in c("chest", "wrist")) {
  for (r in PRIMARY_TEMPORAL_S) {
    for (support_type in c("medi", "full")) {
      support <- paste0("eye_", pos, "_", support_type)
      metric_filter <- if (support_type == "full") DUAL_CHANNEL_METRICS else setdiff(all_metrics, DUAL_CHANNEL_METRICS)
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
readr::write_csv(gamma_standardizers, file.path(OUT_DIAG, "rq2_gamma_standardizer_audit.csv"), na = "")

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

gamma_available <- gamma_long |> filter(available, is.finite(gamma))

gamma_group_keys <- c(
  "dimension_pair", "a_dimension", "a_configuration", "a_configuration_label",
  "b_dimension", "b_configuration", "b_configuration_label",
  "interaction_lattice", "metric", "metric_class", "metric_geometry"
)

gamma_summary_base <- gamma_available |>
  group_by(across(all_of(gamma_group_keys))) |>
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

# Pre-aggregate participant clusters before sending bootstrap work to workers.
gamma_clusters <- gamma_available |>
  group_by(across(all_of(gamma_group_keys)), site, Id) |>
  summarise(
    sum_g = sum(gamma),
    sum_abs_g = sum(abs(gamma)),
    n = n(),
    .groups = "drop"
  )

gamma_nested <- gamma_clusters |>
  group_by(across(all_of(gamma_group_keys))) |>
  nest() |>
  ungroup() |>
  mutate(group_index = row_number())

run_gamma_task <- function(task) {
  tryCatch({
    z <- task$data
    B <- task$B
    site_counts <- table(z$site)
    supported <-
      B > 0L &&
      nrow(z) >= 2L &&
      any(site_counts > 1L)

    if (!supported) {
      ci <- data.frame(
        bootstrap_supported = FALSE,
        R_ci_low = NA_real_, R_ci_high = NA_real_,
        Q_ci_low = NA_real_, Q_ci_high = NA_real_
      )
    } else {
      set.seed(task$seed)
      total_g <- numeric(B)
      total_abs <- numeric(B)
      total_n <- numeric(B)
      for (s in sort(unique(z$site))) {
        zs <- z[z$site == s, , drop = FALSE]
        ns <- nrow(zs)
        idx <- matrix(sample.int(ns, ns * B, replace = TRUE), nrow = ns, ncol = B)
        total_g <- total_g + colSums(matrix(zs$sum_g[idx], nrow = ns, ncol = B))
        total_abs <- total_abs + colSums(matrix(zs$sum_abs_g[idx], nrow = ns, ncol = B))
        total_n <- total_n + colSums(matrix(zs$n[idx], nrow = ns, ncol = B))
      }
      r <- total_g / total_n
      q <- total_abs / total_n
      ci <- data.frame(
        bootstrap_supported = TRUE,
        R_ci_low = unname(stats::quantile(r, .025, names = FALSE, type = 7)),
        R_ci_high = unname(stats::quantile(r, .975, names = FALSE, type = 7)),
        Q_ci_low = unname(stats::quantile(q, .025, names = FALSE, type = 7)),
        Q_ci_high = unname(stats::quantile(q, .975, names = FALSE, type = 7))
      )
    }
    object <- list(
      checkpoint_version = task$checkpoint_version,
      complete = TRUE,
      ci = ci
    )
    tmp <- paste0(task$checkpoint_path, ".tmp_", Sys.getpid())
    saveRDS(object, tmp, compress = FALSE)
    if (file.exists(task$checkpoint_path)) unlink(task$checkpoint_path)
    ok <- file.rename(tmp, task$checkpoint_path)
    if (!ok) {
      ok <- file.copy(tmp, task$checkpoint_path, overwrite = TRUE)
      unlink(tmp)
    }
    if (!ok) stop("Could not write gamma bootstrap checkpoint")
    list(ok = TRUE, checkpoint_path = task$checkpoint_path, error = NA_character_)
  }, error = function(e) {
    list(ok = FALSE, checkpoint_path = task$checkpoint_path, error = conditionMessage(e))
  })
}

gamma_tasks <- list()
gamma_manifest <- vector("list", nrow(gamma_nested))
for (i in seq_len(nrow(gamma_nested))) {
  ckpt <- file.path(GAMMA_CKPT_DIR, sprintf("group_%04d.rds", i))
  reused <- !RQ2_FORCE && checkpoint_valid(ckpt, GAMMA_CHECKPOINT_VERSION)
  gamma_manifest[[i]] <- tibble(group_index = i, checkpoint_path = ckpt, checkpoint_reused = reused)
  if (!reused) {
    gamma_tasks[[length(gamma_tasks) + 1L]] <- list(
      data = as.data.frame(gamma_nested$data[[i]]),
      B = RQ2_BOOT,
      seed = BOOT_SEED + i * 1013L,
      checkpoint_version = GAMMA_CHECKPOINT_VERSION,
      checkpoint_path = ckpt
    )
  }
}
gamma_manifest <- bind_rows(gamma_manifest)
message(
  "RQ2 gamma bootstrap: groups=", nrow(gamma_nested),
  ", reused=", sum(gamma_manifest$checkpoint_reused),
  ", pending=", length(gamma_tasks)
)

gamma_run_results <- run_in_batches(gamma_tasks, run_gamma_task, RQ2_WORKERS, "gamma bootstrap")
gamma_errors <- keep(gamma_run_results, ~!isTRUE(.x$ok))
if (length(gamma_errors)) {
  err_tbl <- map_dfr(gamma_errors, ~tibble(checkpoint_path = .x$checkpoint_path, error = .x$error))
  readr::write_csv(err_tbl, file.path(OUT_DIAG, "rq2_gamma_worker_errors.csv"), na = "")
  stop("One or more RQ2 gamma workers failed; inspect results/diagnostics/rq2_gamma_worker_errors.csv")
}

gamma_ci_rows <- vector("list", nrow(gamma_nested))
for (i in seq_len(nrow(gamma_nested))) {
  p <- gamma_manifest$checkpoint_path[i]
  if (!checkpoint_valid(p, GAMMA_CHECKPOINT_VERSION)) stop("Missing gamma checkpoint: ", p)
  gamma_ci_rows[[i]] <- bind_cols(
    gamma_nested[i, gamma_group_keys, drop = FALSE],
    as_tibble(readRDS(p)$ci)
  )
}
gamma_cis <- bind_rows(gamma_ci_rows)

readr::write_csv(
  gamma_manifest,
  file.path(OUT_DIAG, "rq2_gamma_bootstrap_task_audit.csv"),
  na = ""
)

gamma_summary <- gamma_summary_base |>
  left_join(gamma_cis, by = gamma_group_keys) |>
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
    "Runtime:",
    sprintf("- workers: %d", RQ2_WORKERS),
    sprintf("- model checkpoint version: %s", MODEL_CHECKPOINT_VERSION),
    sprintf("- gamma checkpoint version: %s", GAMMA_CHECKPOINT_VERSION),
    sprintf("- force recomputation: %s", RQ2_FORCE),
    sprintf("- model checkpoints reused: %d", reused_n),
    sprintf("- gamma checkpoints reused: %d", sum(gamma_manifest$checkpoint_reused)),
    "- BLAS/OpenMP threads are limited to one per worker.",
    "- grouped participant folds are fixed once per metric x configuration and shared across all model families/outcomes.",
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
    "- results/rq2/rq2_gamma_pair_summary.csv",
    "",
    "Execution diagnostics:",
    "- results/diagnostics/rq2_model_task_audit.csv",
    "- results/diagnostics/rq2_gamma_bootstrap_task_audit.csv"
  ),
  file.path(OUT_RESULTS, "RQ2_RUN_REPORT.md")
)

message("RQ2 complete.")
message("  ", file.path(OUT_DATA, "rq2_condition_long.rds"))
message("  ", file.path(OUT_DATA, "rq2_gamma_long.rds"))
message("  ", file.path(OUT_RESULTS, "rq2_conditional_geometry.csv"))
message("  ", file.path(OUT_RESULTS, "rq2_gamma_summary.csv"))
