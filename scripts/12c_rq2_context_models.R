# Layered RQ2 context models.
# This runs after the frozen v5 RQ2 runtime has built the canonical transition
# objects. It reuses those objects and adds only contextual annotations/models.

if (!exists("normalize_primary", mode = "function") || !exists("unit_features") || !exists("task_catalog")) {
  stop("12c_rq2_context_models.R must run after the canonical RQ2 v5 runtime")
}
source("scripts/utils/rq2_context_features.R")

if (!isTRUE(RUN_MODELS)) {
  message("RQ2 layered context models disabled by RQ2_RUN_MODELS=0")
} else {
  CONTEXT_EXTERNAL <- rq2_context_external_predictors()
  CONTEXT_MICRO <- rq2_context_micro_predictors()
  CONTEXT_BEHAVIOUR <- rq2_context_behaviour_predictors()
  CONTEXT_ALL <- c(CONTEXT_EXTERNAL, CONTEXT_MICRO, CONTEXT_BEHAVIOUR)
  CONTEXT_VERSION <- paste0("rq2_layered_context_v1__", RQ2_VERSION)
  CONTEXT_SHARD_ROOT <- file.path(OUT, "context_model_input_shards", CONTEXT_VERSION)
  CONTEXT_CHECKPOINTS <- file.path(OUT, "context_checkpoints", CONTEXT_VERSION)
  dir.create(CONTEXT_SHARD_ROOT, recursive = TRUE, showWarnings = FALSE)
  dir.create(CONTEXT_CHECKPOINTS, recursive = TRUE, showWarnings = FALSE)

  # One row per participant-day on the same eye/MEDI/10-s calendar already used
  # by the RQ2 exposure-state construction. ERA5 columns come directly from the
  # validated unit_context artifact; no new weather ingestion rule is introduced.
  context_calendar <- context |>
    filter(
      support_id == "eye_medi", placement == "eye", optical == "MEDI", resolution_s == 10L
    ) |>
    select(
      site, Id, Date, timezone, latitude, longitude, day_of_year,
      era5_ssrd_daily_mean_w_m2, era5_direct_fraction,
      era5_total_cloud_cover_mean, era5_total_cloud_cover_sd,
      era5_t2m_mean_c, `era5_wet_hours_0.1mm`
    ) |>
    distinct() |>
    mutate(Date = as.Date(Date))

  photoperiod_daily <- rq2_context_daily_photoperiod(
    context_calendar |> select(site, Date, timezone, latitude, longitude) |> distinct()
  )
  person_daily <- rq2_context_person_day(sort(unique(context_calendar$site)))

  layered_daily <- context_calendar |>
    left_join(photoperiod_daily, by = c("site", "Date")) |>
    left_join(person_daily, by = c("site", "Id", "Date")) |>
    mutate(
      external_radiation = if_else(
        is.finite(era5_ssrd_daily_mean_w_m2) & era5_ssrd_daily_mean_w_m2 >= 0,
        log1p(era5_ssrd_daily_mean_w_m2), NA_real_
      ),
      external_direct_fraction = era5_direct_fraction,
      external_cloud = era5_total_cloud_cover_mean,
      external_cloud_variability = era5_total_cloud_cover_sd,
      solar_noon_elevation_deg = pmax(
        0, 90 - abs(latitude - 23.44 * sin(2 * pi * (284 + day_of_year) / 365.25))
      ),
      external_temperature_c = era5_t2m_mean_c,
      external_wet_hours = .data[["era5_wet_hours_0.1mm"]]
    ) |>
    select(site, Id, Date, all_of(CONTEXT_ALL))

  readr::write_csv(layered_daily, file.path(DIAG, "rq2_layered_context_day_features.csv"), na = "")
  readr::write_csv(
    tibble(
      predictor = CONTEXT_ALL,
      predictor_family = c(
        rep("External opportunity", length(CONTEXT_EXTERNAL)),
        rep("Micro-environment", length(CONTEXT_MICRO)),
        rep("Behaviour", length(CONTEXT_BEHAVIOUR))
      )
    ),
    file.path(DIAG, "rq2_layered_context_predictor_catalog.csv"), na = ""
  )

  # Duration transitions receive the mean context of the exact longer-window
  # member dates, matching the existing RQ2 rule for exposure level and ERA5.
  layered_window <- duration_manifest |>
    select(support_id, site, Id, window_id, n_days, member_dates) |>
    tidyr::unnest_longer(member_dates, values_to = "Date") |>
    mutate(Date = as.Date(Date)) |>
    left_join(layered_daily, by = c("site", "Id", "Date")) |>
    group_by(support_id, site, Id, window_id, n_days) |>
    summarise(across(all_of(CONTEXT_ALL), safe_mean), .groups = "drop")

  unit_base <- unit_features |>
    select(
      transition_unit_key, dimension, comparison_pair_id, support_id, site, Id, Date,
      window_id_a, window_id_b, n_days_a, n_days_b, participant_key,
      primary_state_raw, duration_day_variability
    )
  unit_features_layered <- bind_rows(
    unit_base |>
      filter(dimension != "duration") |>
      left_join(layered_daily, by = c("site", "Id", "Date")),
    unit_base |>
      filter(dimension == "duration") |>
      left_join(
        layered_window |>
          select(support_id, site, Id, window_id, all_of(CONTEXT_ALL)) |>
          rename(window_id_b = window_id),
        by = c("support_id", "site", "Id", "window_id_b")
      )
  )
  if (anyDuplicated(unit_features_layered$transition_unit_key)) {
    stop("RQ2 layered context transition-unit key is not unique")
  }

  context_part_token <- function(i) sprintf("part_%03d", as.integer(i) - 1L)
  build_context_shards <- function(path, i) {
    pdir <- file.path(CONTEXT_SHARD_ROOT, context_part_token(i))
    marker <- file.path(pdir, ".complete")
    manifest_path <- file.path(pdir, "manifest.csv")
    if (file.exists(marker) && file.exists(manifest_path)) {
      marker_text <- readLines(marker, warn = FALSE)
      if (length(marker_text) && identical(marker_text[[1]], CONTEXT_VERSION)) {
        return(readr::read_csv(manifest_path, show_col_types = FALSE, progress = FALSE))
      }
    }
    dir.create(pdir, recursive = TRUE, showWarnings = FALSE)

    z <- normalize_primary(readRDS(path)) |>
      filter(available, is.finite(z)) |>
      select(
        dimension, comparison_pair_id, metric, metric_class, support_id, site, Id,
        transition_unit_key, z
      ) |>
      left_join(
        unit_features_layered |>
          select(
            transition_unit_key, participant_key, primary_state_raw,
            duration_day_variability, all_of(CONTEXT_ALL)
          ),
        by = "transition_unit_key"
      ) |>
      mutate(abs_z = abs(z)) |>
      left_join(task_catalog, by = c("dimension", "comparison_pair_id", "metric", "metric_class"))
    if (any(is.na(z$task_index))) stop("RQ2 layered context task join failed in ", basename(path))

    model_z <- z |>
      select(
        task_index, site, participant_key, z, abs_z, primary_state_raw,
        duration_day_variability, all_of(CONTEXT_ALL)
      )
    groups <- split(model_z, model_z$task_index)
    rows <- vector("list", length(groups)); k <- 0L
    for (nm in names(groups)) {
      k <- k + 1L
      g <- groups[[nm]] |> select(-task_index)
      spath <- file.path(pdir, paste0(sanitize_task(as.integer(nm)), ".rds"))
      saveRDS(g, spath, compress = "gzip")
      rows[[k]] <- tibble(
        part_index = i, task_index = as.integer(nm),
        shard_path = normalizePath(spath, winslash = "/", mustWork = TRUE),
        rows = nrow(g), bytes = as.numeric(file.info(spath)$size)
      )
    }
    manifest <- bind_rows(rows)
    readr::write_csv(manifest, manifest_path, na = "")
    writeLines(CONTEXT_VERSION, marker)
    manifest
  }

  CONTEXT_STREAM_WORKERS <- ms_resolve_workers("RQ2_STREAM_WORKERS", default = 12L, cap = 24L)
  context_shard_task <- function(task) build_context_shards(task$path, task$i)
  context_shard_tasks <- lapply(seq_along(part_paths), function(i) {
    list(path = part_paths[[i]], i = i)
  })
  message(
    "RQ2 layered context: build model-input shards from canonical RQ1 parts across ",
    CONTEXT_STREAM_WORKERS, " PSOCK workers"
  )
  context_shard_manifest <- bind_rows(ms_parallel_map(
    context_shard_tasks, context_shard_task,
    workers = CONTEXT_STREAM_WORKERS,
    packages = c("tidyverse"),
    exports = c(
      "context_shard_task", "build_context_shards", "CONTEXT_SHARD_ROOT",
      "context_part_token", "CONTEXT_VERSION", "normalize_primary",
      "unit_features_layered", "CONTEXT_ALL", "task_catalog", "sanitize_task"
    )
  ))
  readr::write_csv(
    context_shard_manifest,
    file.path(OUT, "rq2_layered_context_model_input_shard_manifest.csv"), na = ""
  )

  context_family_predictors <- function(dimension, family) {
    state <- if (identical(dimension, "duration")) {
      c("primary_state_raw", "duration_day_variability")
    } else {
      "primary_state_raw"
    }
    if (identical(family, "external_context")) return(CONTEXT_EXTERNAL)
    if (identical(family, "microenvironment")) return(CONTEXT_MICRO)
    if (identical(family, "behaviour")) return(CONTEXT_BEHAVIOUR)
    if (identical(family, "exposure_state")) return(state)
    if (identical(family, "joint")) return(c(state, CONTEXT_EXTERNAL, CONTEXT_MICRO, CONTEXT_BEHAVIOUR))
    character()
  }

  fit_context_task <- function(task) {
    dat <- bind_rows(lapply(task$shard_paths, readRDS))
    meta <- task$meta

    scale_train_test <- function(tr, te, predictors) {
      keep <- character()
      for (p in predictors) {
        finite <- is.finite(tr[[p]])
        if (sum(finite) < 3L) next
        mu <- mean(tr[[p]][finite]); s <- sd(tr[[p]][finite])
        if (!is.finite(mu) || !is.finite(s) || s <= sqrt(.Machine$double.eps)) next
        tr[[p]] <- (tr[[p]] - mu) / s
        te[[p]] <- (te[[p]] - mu) / s
        keep <- c(keep, p)
      }
      list(tr = tr, te = te, keep = keep)
    }

    fit_one <- function(d, outcome, predictors) {
      if (!length(predictors) || nrow(d) < 20L || n_distinct(d$participant_key) < 3L) {
        return(list(fit = NULL, random_structure = NA_character_))
      }
      d$site <- factor(d$site); d$participant_key <- factor(d$participant_key)
      f <- reformulate(predictors, response = outcome)
      ctrl <- nlme::lmeControl(opt = "optim", maxIter = 100L, msMaxIter = 100L, returnObject = TRUE)
      fit <- tryCatch(
        suppressWarnings(nlme::lme(
          fixed = f, random = ~1 | site/participant_key, data = d,
          method = "ML", na.action = na.omit, control = ctrl
        )),
        error = function(e) NULL
      )
      if (!is.null(fit)) return(list(fit = fit, random_structure = "site/participant"))
      fit <- tryCatch(
        suppressWarnings(nlme::lme(
          fixed = f, random = ~1 | participant_key, data = d,
          method = "ML", na.action = na.omit, control = ctrl
        )),
        error = function(e) NULL
      )
      list(fit = fit, random_structure = if (is.null(fit)) NA_character_ else "participant")
    }

    performance <- function(obs, pred) {
      ok <- is.finite(obs) & is.finite(pred); obs <- obs[ok]; pred <- pred[ok]
      if (length(obs) < 2L) {
        return(tibble(n_test = length(obs), rmse = NA_real_, mae = NA_real_, r2 = NA_real_))
      }
      sst <- sum((obs - mean(obs))^2)
      tibble(
        n_test = length(obs), rmse = sqrt(mean((obs - pred)^2)),
        mae = mean(abs(obs - pred)),
        r2 = if (sst > 0) 1 - sum((obs - pred)^2) / sst else NA_real_
      )
    }

    set.seed(task$seed)
    pm_base <- dat |>
      distinct(site, participant_key) |>
      group_by(site) |>
      mutate(fold = sample(rep(seq_len(task$folds), length.out = n()))) |>
      ungroup()
    coefs <- list(); perfs <- list(); ci <- 0L; pi <- 0L
    families <- c("external_context", "microenvironment", "behaviour", "exposure_state", "joint")

    for (oname in names(OUTCOMES)) for (fname in families) {
      outcome <- OUTCOMES[[oname]]
      candidates <- context_family_predictors(meta$dimension[[1]], fname)
      usable <- candidates[vapply(candidates, function(p) {
        x <- dat[[p]]
        sum(is.finite(x)) >= 3L && is.finite(sd(x[is.finite(x)])) &&
          sd(x[is.finite(x)]) > sqrt(.Machine$double.eps)
      }, logical(1))]
      if (!length(usable)) next
      d <- dat |> filter(is.finite(.data[[outcome]]), if_all(all_of(usable), is.finite))
      if (nrow(d) < 20L || n_distinct(d$participant_key) < 3L) next

      sc <- scale_train_test(d, d, usable)
      full_fit <- fit_one(sc$tr, outcome, sc$keep)
      if (!is.null(full_fit$fit)) {
        tt <- summary(full_fit$fit)$tTable
        ci <- ci + 1L
        coefs[[ci]] <- tibble(
          dimension = meta$dimension, comparison_pair_id = meta$comparison_pair_id,
          metric = meta$metric, outcome = oname, model_family = fname,
          random_structure = full_fit$random_structure, term = rownames(tt),
          estimate = tt[, "Value"], std_error = tt[, "Std.Error"], df = tt[, "DF"],
          t_value = tt[, "t-value"], p_value = tt[, "p-value"]
        )
      }

      d_cv <- d |> left_join(pm_base, by = c("site", "participant_key"))
      obs <- pred <- numeric()
      for (sp in sort(unique(d_cv$fold))) {
        tr <- d_cv |> filter(fold != sp)
        te <- d_cv |> filter(fold == sp)
        if (nrow(tr) < 20L || nrow(te) < 2L || n_distinct(tr$participant_key) < 3L) next
        ss <- scale_train_test(tr, te, usable)
        ff <- fit_one(ss$tr, outcome, ss$keep)
        if (is.null(ff$fit)) next
        pr <- tryCatch(
          as.numeric(predict(ff$fit, newdata = ss$te, level = 0)),
          error = function(e) rep(NA_real_, nrow(ss$te))
        )
        obs <- c(obs, ss$te[[outcome]]); pred <- c(pred, pr)
      }
      perf <- performance(obs, pred)
      pi <- pi + 1L
      perfs[[pi]] <- tibble(
        dimension = meta$dimension, comparison_pair_id = meta$comparison_pair_id,
        metric = meta$metric, outcome = oname, model_family = fname,
        validation_scheme = "participant_grouped",
        n_participants = n_distinct(d$participant_key), n_sites = n_distinct(d$site),
        n_test = perf$n_test, rmse = perf$rmse, mae = perf$mae, r2 = perf$r2
      )
    }
    list(
      checkpoint_version = task$checkpoint_version, complete = TRUE,
      coefficients = bind_rows(coefs), performance = bind_rows(perfs)
    )
  }

  CONTEXT_PROGRESS_LOG <- file.path("results", "logs", "rq2_layered_context_model_progress.tsv")
  writeLines(
    "timestamp\ttask_index\tstatus\tdimension\tcomparison_pair_id\tmetric",
    CONTEXT_PROGRESS_LOG
  )
  fit_context_task_checkpoint <- function(task) {
    log_progress <- function(status) {
      cat(
        paste(
          format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"), task$index, status,
          task$meta$dimension[[1]], task$meta$comparison_pair_id[[1]], task$meta$metric[[1]],
          sep = "\t"
        ),
        "\n", file = task$progress_path, append = TRUE, sep = ""
      )
    }
    cached <- if (file.exists(task$checkpoint_path)) {
      tryCatch(readRDS(task$checkpoint_path), error = function(e) NULL)
    } else NULL
    if (!is.null(cached) && identical(cached$checkpoint_version, task$checkpoint_version) && isTRUE(cached$complete)) {
      log_progress("reused")
      return(list(index = task$index, path = task$checkpoint_path, reused = TRUE))
    }
    obj <- fit_context_task(task)
    tmp <- paste0(task$checkpoint_path, ".tmp.", Sys.getpid())
    saveRDS(obj, tmp, compress = FALSE)
    if (file.exists(task$checkpoint_path)) unlink(task$checkpoint_path)
    if (!file.rename(tmp, task$checkpoint_path)) stop("Could not install RQ2 layered-context checkpoint")
    log_progress("completed")
    list(index = task$index, path = task$checkpoint_path, reused = FALSE)
  }

  context_tasks <- lapply(seq_len(nrow(task_catalog)), function(i) {
    meta <- task_catalog[i, ]
    paths <- context_shard_manifest |>
      filter(task_index == meta$task_index) |>
      pull(shard_path)
    checkpoint_path <- file.path(CONTEXT_CHECKPOINTS, paste0(sanitize_task(meta$task_index), ".rds"))
    list(
      index = meta$task_index[[1]], meta = meta, shard_paths = paths,
      folds = RQ2_CV_FOLDS, seed = MODEL_SEED + meta$task_index[[1]],
      checkpoint_version = paste0(
        CONTEXT_VERSION, "__core__", CORE_VERSION, "__participant_cv__", RQ2_CV_FOLDS
      ),
      checkpoint_path = checkpoint_path,
      progress_path = normalizePath(CONTEXT_PROGRESS_LOG, winslash = "/", mustWork = FALSE),
      cost = sum(context_shard_manifest$bytes[context_shard_manifest$task_index == meta$task_index], na.rm = TRUE)
    )
  })
  context_tasks <- keep(context_tasks, ~length(.x$shard_paths) > 0L)
  order_idx <- order(vapply(context_tasks, `[[`, numeric(1), "cost"), decreasing = TRUE)
  scheduled <- context_tasks[order_idx]
  message(
    "RQ2 layered context models: ", length(scheduled), " tasks across ", RQ2_WORKERS,
    " PSOCK workers; ", RQ2_CV_FOLDS, "-fold participant-grouped CV"
  )
  refs <- ms_parallel_map(
    scheduled, fit_context_task_checkpoint, workers = RQ2_WORKERS, seed = MODEL_SEED,
    packages = c("tidyverse", "nlme"),
    exports = c(
      "fit_context_task_checkpoint", "fit_context_task", "context_family_predictors",
      "CONTEXT_EXTERNAL", "CONTEXT_MICRO", "CONTEXT_BEHAVIOUR", "OUTCOMES"
    )
  )

  context_results <- vector("list", nrow(task_catalog))
  for (r in refs) context_results[[r$index]] <- readRDS(r$path)
  context_coefficients <- bind_rows(map(context_results, ~if (is.null(.x)) NULL else .x$coefficients))
  context_performance <- bind_rows(map(context_results, ~if (is.null(.x)) NULL else .x$performance))

  readr::write_csv(context_coefficients, file.path(OUT, "rq2_model_coefficients.csv"), na = "")
  readr::write_csv(context_performance, file.path(OUT, "rq2_model_performance.csv"), na = "")
  readr::write_csv(context_coefficients, file.path(OUT, "rq2_layered_context_model_coefficients.csv"), na = "")
  readr::write_csv(context_performance, file.path(OUT, "rq2_layered_context_model_performance.csv"), na = "")

  context_manifest <- bind_rows(lapply(seq_len(nrow(task_catalog)), function(i) {
    meta <- task_catalog[i, ]
    obj <- context_results[[meta$task_index]]
    checkpoint_path <- file.path(CONTEXT_CHECKPOINTS, paste0(sanitize_task(meta$task_index), ".rds"))
    tibble(
      artifact_type = "rq2_layered_context_model_checkpoint_v1",
      rq1_analysis_version = RQ1_VERSION, rq2_analysis_version = RQ2_VERSION,
      core_artifact_version = CORE_VERSION, context_model_version = CONTEXT_VERSION,
      task_index = meta$task_index, dimension = meta$dimension,
      comparison_pair_id = meta$comparison_pair_id, metric = meta$metric,
      metric_class = meta$metric_class,
      checkpoint_path = normalizePath(checkpoint_path, winslash = "/", mustWork = FALSE),
      checkpoint_present = file.exists(checkpoint_path),
      checkpoint_complete = !is.null(obj) && isTRUE(obj$complete),
      coefficient_rows = if (!is.null(obj)) nrow(obj$coefficients) else 0L,
      performance_rows = if (!is.null(obj)) nrow(obj$performance) else 0L,
      run_models = TRUE
    )
  }))
  readr::write_csv(context_manifest, file.path(OUT, "rq2_model_artifact_manifest.csv"), na = "")
  readr::write_csv(context_manifest, file.path(OUT, "rq2_layered_context_model_manifest.csv"), na = "")

  writeLines(
    c(
      paste0("RQ2 layered context version: ", CONTEXT_VERSION),
      paste0("External opportunity predictors: ", paste(CONTEXT_EXTERNAL, collapse = ", ")),
      paste0("Micro-environment predictors: ", paste(CONTEXT_MICRO, collapse = ", ")),
      paste0("Behaviour predictors: ", paste(CONTEXT_BEHAVIOUR, collapse = ", ")),
      "Exposure-state predictors retain the frozen RQ2 definition.",
      "All continuous predictors are z-standardized inside each training split; CV is participant-grouped."
    ),
    file.path(OUT, "rq2_layered_context_run_report.txt")
  )
}
