# Independent, frozen-input RQ2 recovery prototype. No upstream entrypoint is sourced.
# Rscript scripts/12d_rq2_recovery.R --check-inputs  # read-only, no pair decompression
# RQ2_RECOVERY_WORKERS=36 Rscript scripts/12d_rq2_recovery.R --run
# See docs/RQ2_RECOVERY.md for the estimand, deployment boundary and outputs.
suppressPackageStartupMessages({ library(dplyr); library(tibble) })
source("scripts/utils/rq1_inference.R")
source("scripts/utils/rq2_context_features.R")
source("scripts/utils/rq2_model_helpers.R")
source("scripts/utils/parallel_runtime.R")

recovery_predictors <- function() c(rq2_context_external_predictors(),
  rq2_context_micro_predictors(), rq2_context_behaviour_predictors())
recovery_delta <- function(a, b, circular) {
  if (circular) ((a - b + 43200) %% 86400) - 43200 else a - b
}
recovery_require <- function(x, fields, label) {
  missing <- setdiff(fields, names(x))
  if (length(missing)) stop(label, " missing fields: ", paste(missing, collapse = ", "))
}
recovery_hash <- function(x) {
  p <- tempfile(); on.exit(unlink(p)); saveRDS(x, p, version = 3)
  unname(tools::md5sum(p))
}
recovery_atomic <- function(x, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  tmp <- paste0(path, ".tmp.", Sys.getpid())
  on.exit(unlink(tmp), add = TRUE)
  saveRDS(x, tmp, compress = "gzip")
  if (file.exists(path)) unlink(path)
  if (!file.rename(tmp, path)) stop("Cannot install recovery checkpoint: ", path)
}
recovery_read_csv <- function(path) {
  header <- names(readr::read_csv(path, n_max = 0L, show_col_types = FALSE, progress = FALSE))
  types <- setNames(lapply(intersect(c("site", "Id"), header), function(p) readr::col_character()),
    intersect(c("site", "Id"), header))
  readr::read_csv(path, col_types = do.call(readr::cols, c(types, list(.default = readr::col_guess()))),
    show_col_types = FALSE, progress = FALSE)
}

# Validate small contracts even when local pair parts are absent. No reconstruction.
recovery_inputs <- function() {
  paths <- c(pairwise = "results/rq1/rq1_pairwise_change_long.rds",
    summary = "results/rq1/rq1_pairwise_summary.csv",
    scales = "results/diagnostics/rq1_standardizer_audit.csv",
    context = "results/diagnostics/rq2_layered_context_day_features.csv")
  problems <- paste0("Missing frozen input: ", paths[!file.exists(paths)])
  problems <- problems[nzchar(problems) & problems != "Missing frozen input: "]
  if (any(!file.exists(paths))) return(list(problems = problems, paths = paths))
  upstream <- readRDS(paths[["pairwise"]])
  version <- rq1_pairwise_version(upstream)
  core <- paste0("v4_sparse_sampling_complete_days__", ms_core_design_id())
  ms_assert_version(upstream, "analysis_design_id", ms_analysis_design_id())
  ms_assert_version(upstream, "core_artifact_version", core)
  if (!rq1_pairwise_is_partitioned(upstream)) stop("Expected frozen partitioned RQ1 manifest")
  recovery_require(upstream$part_manifest, c("part", "dimension"), "RQ1 part manifest")
  records <- upstream$part_manifest |> filter(dimension == "placement_optical_temporal")
  if (!nrow(records)) stop("No declared non-duration RQ1 parts; refusing to scan duration")
  upstream$parts <- as.character(records$part)
  parts <- rq1_pairwise_part_paths(upstream)
  for (p in parts) {
    if (!file.exists(p)) problems <- c(problems, paste0("Missing frozen anchor part: ", p))
    if (!file.exists(paste0(p, ".ok"))) problems <- c(problems, paste0("Missing completion marker: ", p, ".ok"))
  }
  summary <- recovery_read_csv(paths[["summary"]])
  tryCatch(rq1_assert_summary_version(upstream, summary), error = function(e)
    problems <<- c(problems, conditionMessage(e)))
  tryCatch(ms_assert_version(summary, "core_artifact_version", core), error = function(e)
    problems <<- c(problems, conditionMessage(e)))
  recovery_require(summary, c("dimension", "comparison_pair_id", "metric", "metric_geometry",
    "metric_class", "A_mean_absolute", "comparison_lattice"), "RQ1 summary")
  summary <- semi_join(summary, rq1_inference_anchor_map(), by = c("dimension", "comparison_pair_id")) |>
    filter(!metric %in% c("interdaily_stability", "intradaily_variability"))
  ms_assert_unique(summary, c("dimension", "comparison_pair_id", "metric"), "Anchor summary")
  scales <- recovery_read_csv(paths[["scales"]])
  recovery_require(scales, c("comparison_lattice", "metric", "metric_geometry",
    "standardizer", "scale_anchor_config"), "Frozen scales")
  ms_assert_unique(scales, c("comparison_lattice", "metric", "metric_geometry"), "Frozen scales")
  context <- recovery_read_csv(paths[["context"]])
  recovery_require(context, c("site", "Id", "Date", recovery_predictors()), "Frozen context")
  context <- context |> mutate(Date = as.Date(Date)) |>
    select(site, Id, Date, all_of(recovery_predictors()))
  ms_assert_unique(context, c("site", "Id", "Date"), "Frozen context")
  # All-NA columns can be parsed as logical; permit those, never parse text silently.
  for (p in recovery_predictors()) {
    if (!is.numeric(context[[p]]) && !all(is.na(context[[p]]))) stop("Non-numeric context: ", p)
    context[[p]] <- as.numeric(context[[p]])
  }
  list(problems = problems, paths = c(paths, parts), upstream = upstream,
    version = version, core = core, summary = summary, scales = scales, context = context)
}

# Select through the same Fig.2 anchor contract, while preserving z/scale audit fields.
recovery_pairs <- function(inputs) {
  wanted <- rq1_inference_anchor_map()
  filter_anchor <- function(z) z |> filter(analysis_unit_type == "participant_day") |>
    semi_join(wanted, by = c("dimension", "comparison_pair_id"))
  pieces <- lapply(rq1_pairwise_part_paths(inputs$upstream), function(p) {
    z <- filter_anchor(readRDS(p))
    recovery_require(z, c("config_a_id", "config_b_id", "comparison_lattice", "z", "delta",
      "available", "scale_anchor_config", "rq1_analysis_version", "core_artifact_version"), p)
    ms_assert_version(z, "rq1_analysis_version", inputs$version, p)
    ms_assert_version(z, "core_artifact_version", inputs$core, p)
    a <- rq1_inference_pairs(as.data.frame(z))
    keys <- c("dimension", "comparison_pair_id", "support_id", "site", "Id", "Date", "metric")
    z <- z |> mutate(Id = as.character(Id), Date = as.Date(Date))
    a <- left_join(a, select(z, all_of(keys), config_a_id, config_b_id,
      comparison_lattice, z, delta, available, scale_anchor_config), by = keys, relationship = "one-to-one")
    if (any(a$config_a_id != a$candidate_config | a$config_b_id != a$reference_config))
      stop("Anchor configuration orientation mismatch")
    a
  })
  x <- bind_rows(pieces)
  ms_assert_unique(x, c("candidate_config", "site", "Id", "Date", "metric"), "Recovery pairs")
  if (n_distinct(x$metric) != 52L || n_distinct(x$candidate_config) != 8L)
    stop("Expected 52 daily metrics and eight anchors, including unavailable rows")
  dual <- x$metric %in% rq1_inference_contract()$dual_channel_metrics
  expected <- ifelse(x$dimension == "placement", paste0("eye_", x$placement,
    ifelse(dual, "_full", "_medi")), ifelse(x$dimension == "optical", "eye_full",
      ifelse(dual, "eye_full", "eye_medi")))
  if (any(x$support_id != expected)) stop("Non-maximal or unexpected anchor support")
  x <- left_join(x, inputs$scales, by = c("comparison_lattice", "metric", "metric_geometry", "scale_anchor_config"),
    relationship = "many-to-one")
  if (any(is.na(x$standardizer) & x$available, na.rm = TRUE)) stop("Missing frozen scale for available pair")
  x$eligible <- is.na(x$pair_reason) & coalesce(x$available, FALSE) &
    is.finite(x$standardizer) & x$standardizer > sqrt(.Machine$double.eps)
  if (any(x$eligible & x$optical == "LIGHT" & dual)) stop("LIGHT-only dual-channel metric marked available")
  err <- recovery_delta(x$reference_value, x$candidate_value, FALSE)
  circ <- x$metric_geometry == "circular_time"
  if (any(!x$metric_geometry %in% c("linear", "circular_time"))) stop("Unknown metric geometry")
  err[circ] <- recovery_delta(x$reference_value[circ], x$candidate_value[circ], TRUE)
  ok <- x$eligible
  if (any(!is.finite(x$delta[ok])) || any(abs(err[ok] - x$delta[ok]) >
      1e-8 * (1 + abs(x$delta[ok])))) stop("Frozen delta disagrees with paired-value geometry")
  if (any(!is.finite(x$z[ok])) || any(abs(err[ok] / x$standardizer[ok] - x$z[ok]) >
      1e-8 * (1 + abs(x$z[ok])))) stop("Frozen scale/orientation does not reproduce RQ1 z")
  x$participant_key <- paste(x$site, x$Id, sep = "::")
  x <- left_join(x, mutate(inputs$context, context_row_present = TRUE),
    by = c("site", "Id", "Date"), relationship = "many-to-one")
  x$context_row_present <- coalesce(x$context_row_present, FALSE)
  x
}

# Imputation, missingness indicators, variance screening and scaling use training only.
# The only measurement predictors are this task's Y_L (or its sin/cos).
recovery_design <- function(tr, te, context, circular) {
  basis <- function(d) {
    if (circular) data.frame(low_sin = sin(d$candidate_value * 2*pi/86400),
      low_cos = cos(d$candidate_value * 2*pi/86400)) else data.frame(low = d$candidate_value)
  }
  a <- basis(tr); b <- basis(te); audit <- list()
  for (p in context) {
    good <- is.finite(tr[[p]])
    med <- if (any(good)) median(tr[[p]][good]) else 0
    a[[p]] <- ifelse(good, tr[[p]], med)
    b[[p]] <- ifelse(is.finite(te[[p]]), te[[p]], med)
    a[[paste0(p, "_missing")]] <- as.numeric(!good)
    b[[paste0(p, "_missing")]] <- as.numeric(!is.finite(te[[p]]))
    audit[[p]] <- tibble(predictor = p, train_observed = sum(good),
      test_observed = sum(is.finite(te[[p]])), train_median = med)
  }
  scaled <- rq2_model_helpers()$scale_train_test(a, b, names(a))
  aa <- cbind(intercept = 1, as.matrix(scaled$tr[, scaled$keep, drop = FALSE]))
  bb <- cbind(intercept = 1, as.matrix(scaled$te[, scaled$keep, drop = FALSE]))
  list(tr = aa, te = bb, kept = scaled$keep, audit = bind_rows(audit),
    # Store all fitted preprocessing parameters for reproducible deployment audit.
    centers = vapply(a[scaled$keep], mean, numeric(1)),
    scales = vapply(a[scaled$keep], sd, numeric(1)))
}
recovery_predict <- function(tr, te, context, circular, lambda) {
  d <- recovery_design(tr, te, context, circular)
  y <- if (circular) cbind(sin(tr$reference_value * 2*pi/86400),
    cos(tr$reference_value * 2*pi/86400)) else matrix(tr$reference_value, ncol = 1)
  penalty <- diag(ncol(d$tr)); penalty[1, 1] <- 0
  beta <- solve(crossprod(d$tr) / nrow(tr) + lambda * penalty, crossprod(d$tr, y) / nrow(tr))
  pr <- d$te %*% beta
  fallback <- rep(FALSE, nrow(te))
  if (circular) {
    fallback <- sqrt(rowSums(pr^2)) < 1e-10
    pr <- (atan2(pr[, 1], pr[, 2]) %% (2*pi)) * 86400/(2*pi)
    # Undefined predicted direction: explicitly audited raw fallback, never drop rows.
    pr[fallback] <- te$candidate_value[fallback] %% 86400
  } else pr <- as.numeric(pr)
  if (any(!is.finite(pr))) stop("Non-finite held-out predictions")
  list(prediction = pr, fallback = fallback,
    model = list(beta = beta, columns = colnames(d$tr), centers = d$centers,
      scales = d$scales, context_imputation = d$audit))
}
recovery_fit <- function(task) {
  x <- readRDS(task$input)
  x <- x[x$eligible, , drop = FALSE]
  if (nrow(x) < 20L || n_distinct(x$participant_key) < 4L)
    return(list(status = "unavailable_support", complete = TRUE, predictions = tibble(), models = list()))
  circular <- identical(unique(x$metric_geometry), "circular_time")
  x$calibration <- x$context <- NA_real_
  x$calibration_fallback <- x$context_fallback <- FALSE
  models <- list()
  for (f in sort(unique(x$fold))) {
    ti <- which(x$fold != f); vi <- which(x$fold == f)
    tr <- x[ti, , drop = FALSE]; te <- x[vi, , drop = FALSE]
    if (nrow(tr) < 15L || n_distinct(tr$participant_key) < 3L)
      return(list(status = "unavailable_training_support", complete = TRUE, predictions = tibble(), models = list()))
    stopifnot(!any(tr$participant_key %in% te$participant_key))
    for (state in c("calibration", "context")) {
      fit <- recovery_predict(tr, te, if (state == "context") recovery_predictors() else character(),
        circular, task$lambda)
      x[[state]][vi] <- fit$prediction
      x[[paste0(state, "_fallback")]][vi] <- fit$fallback
      models[[paste(f, state, sep = "_")]] <- c(list(fold = f, state = state,
        n_train = nrow(tr), n_test = nrow(te), n_train_participants = n_distinct(tr$participant_key)), fit$model)
    }
  }
  x$raw <- x$candidate_value
  pred <- bind_rows(lapply(c("raw", "calibration", "context"), function(state) {
    fallback <- if (state == "raw") rep(FALSE, nrow(x)) else x[[paste0(state, "_fallback")]]
    x |> transmute(site, Id, Date, support_id, participant_key, fold, context_row_present,
      state = state, Y_L = candidate_value, Y_H = reference_value,
      prediction = .data[[state]], error = recovery_delta(.data[[state]], reference_value, circular),
      standardized_error = error / standardizer,
      circular_fallback = .env$fallback)
  }))
  list(status = "complete", complete = TRUE, predictions = pred, models = models)
}
recovery_task <- function(task) {
  old <- if (file.exists(task$output)) tryCatch(readRDS(task$output), error = function(e) NULL) else NULL
  if (!is.null(old) && identical(old$run_id, task$run_id) && isTRUE(old$complete))
    return(list(index = task$index, path = task$output, status = old$status, reused = TRUE))
  started <- Sys.time()
  result <- tryCatch(recovery_fit(task), error = function(e)
    list(status = "failed", complete = FALSE, error = conditionMessage(e), predictions = tibble(), models = list()))
  result$run_id <- task$run_id; result$meta <- task$meta
  result$elapsed_seconds <- as.numeric(difftime(Sys.time(), started, units = "secs"))
  recovery_atomic(result, task$output)
  list(index = task$index, path = task$output, status = result$status, reused = FALSE)
}

recovery_run <- function(inputs) {
  integer_env <- function(name, default, minimum) {
    v <- suppressWarnings(as.integer(Sys.getenv(name, as.character(default))))
    if (length(v) != 1L || !is.finite(v) || v < minimum) stop("Invalid ", name)
    v
  }
  seed <- integer_env("RQ2_RECOVERY_SEED", 20260912L, 1L)
  folds <- integer_env("RQ2_RECOVERY_FOLDS", 5L, 2L)
  lambda <- as.numeric(Sys.getenv("RQ2_RECOVERY_LAMBDA", "0.01"))
  floor <- as.numeric(Sys.getenv("RQ2_RECOVERY_G_FLOOR", "0.000001"))
  if (!is.finite(lambda) || lambda <= 0 || !is.finite(floor) || floor <= 0) stop("Invalid lambda/G floor")
  workers <- ms_resolve_workers("RQ2_RECOVERY_WORKERS", default = 36L, cap = 48L)
  ms_worker_init()
  code <- c("scripts/12d_rq2_recovery.R", "scripts/utils/rq1_inference.R",
    "scripts/utils/rq1_inference_contract.R", "scripts/utils/rq1_pairwise_artifacts.R",
    "scripts/utils/rq2_context_features.R", "scripts/utils/rq2_model_helpers.R",
    "scripts/utils/parallel_runtime.R", "scripts/utils/analysis_design.R", "scripts/utils/artifact_validation.R",
    "scripts/12c_rq2_context_models.R", "scripts/utils/rq_context.R", "scripts/utils/melidos_io.R")
  provenance <- list(recovery_version = "rq2_recovery_v1", rq1_analysis_version = inputs$version,
    core_artifact_version = inputs$core, analysis_design_id = ms_analysis_design_id(),
    input_md5 = tools::md5sum(inputs$paths), code_md5 = tools::md5sum(code),
    seed = seed, folds = folds, lambda = lambda, G_floor = floor, predictors = recovery_predictors(),
    R = R.version.string, packages = sapply(c("dplyr", "tibble", "readr", "data.table"),
      function(p) as.character(utils::packageVersion(p))),
    context_provenance_limit = "Legacy frozen context CSV has no upstream version; content fingerprint and producer code audited; no regeneration",
    scale_role = "Frozen RQ1 SD used ONLY for scoring; never for fitting/tuning",
    model = "Fixed ridge penalty; train-fold imputation/missing indicators/scaling; no target-state predictors")
  run_id <- recovery_hash(provenance)
  out <- file.path("results/rq2/recovery", inputs$version, run_id)
  dir.create(out, recursive = TRUE, showWarnings = FALSE)
  recovery_atomic(c(provenance, list(run_id = run_id, workers = workers,
    started = Sys.time(), session = capture.output(sessionInfo()))), file.path(out, "provenance.rds"))
  message("Recovery: validate and extract frozen non-duration anchors")
  x <- recovery_pairs(inputs)
  pm <- x |> distinct(site, participant_key) |> arrange(site, participant_key)
  if (folds > nrow(pm)) stop("More folds than participants")
  set.seed(seed, kind = "Mersenne-Twister", normal.kind = "Inversion", sample.kind = "Rejection")
  # One global site-stratified participant map shared by all metrics/contrasts/states.
  pm <- pm |> group_by(site) |> mutate(fold = sample(rep(seq_len(folds), length.out = n()))) |> ungroup()
  x <- left_join(x, pm, by = c("site", "participant_key"), relationship = "many-to-one")
  readr::write_csv(pm, file.path(out, "participant_folds.csv"))
  keys <- c("dimension", "comparison_pair_id", "candidate_config", "support_id",
    "metric", "metric_class", "metric_geometry")
  groups <- x |> group_by(across(all_of(keys))) |> group_split(.keep = TRUE)
  catalog <- bind_rows(lapply(groups, function(g) {
    meta <- distinct(select(g, all_of(keys)))
    bind_cols(meta, tibble(n_rows = nrow(g), n_eligible = sum(g$eligible),
      n_context_rows = sum(g$context_row_present & g$eligible),
      n_eligible_participants = n_distinct(g$participant_key[g$eligible]),
      A_raw_all_eligible = if (any(g$eligible)) mean(abs(g$z[g$eligible])) else NA_real_))
  })) |> mutate(task_index = row_number()) |>
    left_join(select(inputs$summary, dimension, comparison_pair_id, metric,
      A_frozen_RQ1 = A_mean_absolute), by = c("dimension", "comparison_pair_id", "metric"), relationship = "many-to-one")
  mismatch <- with(catalog, is.finite(A_raw_all_eligible) &
    (!is.finite(A_frozen_RQ1) | abs(A_raw_all_eligible - A_frozen_RQ1) > 1e-7 * (1 + abs(A_frozen_RQ1))))
  if (any(mismatch)) stop("Anchor raw A does not reproduce frozen RQ1 summary; inspect support/version")
  readr::write_csv(catalog, file.path(out, "task_catalog.csv"))
  unavailable <- x |> filter(!eligible) |> count(dimension, comparison_pair_id, metric, support_id, pair_reason, available,
    name = "n_rows")
  readr::write_csv(unavailable, file.path(out, "unavailable_audit.csv"))
  readr::write_csv(tibble(predictor = recovery_predictors(), family = c(rep("external", 8),
    rep("microenvironment", 4), rep("behaviour", 6))), file.path(out, "predictor_allowlist.csv"))
  tasks <- lapply(seq_along(groups), function(i) {
    ip <- file.path(out, "inputs", sprintf("task_%04d.rds", i))
    recovery_atomic(groups[[i]], ip)
    list(index = i, input = ip, output = file.path(out, "checkpoints", sprintf("task_%04d.rds", i)),
      run_id = run_id, lambda = lambda, meta = catalog[i, ])
  })
  rm(x, groups); invisible(gc(FALSE))
  message("Recovery: ", length(tasks), " tasks; ", workers, " workers; ", folds, " grouped folds; ", out)
  refs <- ms_parallel_map(tasks, recovery_task, workers = workers, seed = seed,
    packages = c("dplyr", "tibble"), exports = c("recovery_task", "recovery_fit", "recovery_predict",
      "recovery_design", "recovery_delta", "recovery_predictors", "recovery_atomic", "rq2_model_helpers",
      "rq2_context_external_predictors", "rq2_context_micro_predictors", "rq2_context_behaviour_predictors"))
  state_rows <- list(); statuses <- list(); fold_rows <- list()
  for (r in refs) {
    obj <- readRDS(r$path)
    statuses[[r$index]] <- bind_cols(catalog[r$index, ], tibble(status = obj$status,
      error = if (is.null(obj$error)) NA_character_ else obj$error,
      reused = r$reused, checkpoint = r$path, elapsed_seconds = obj$elapsed_seconds))
    if (!nrow(obj$predictions)) {
      state_rows[[r$index]] <- bind_cols(catalog[rep(r$index, 3L), ], tibble(
        state = c("raw", "calibration", "context"), n_test = 0L, n_test_participants = 0L,
        MAE_native = NA_real_, RMSE_native = NA_real_, A = NA_real_, B = NA_real_,
        standardized_RMSE = NA_real_, n_circular_fallback = 0L, status = obj$status))
      next
    }
    s <- obj$predictions |> group_by(state) |> summarise(n_test = n(),
      n_test_participants = n_distinct(participant_key), MAE_native = mean(abs(error)),
      RMSE_native = sqrt(mean(error^2)), A = mean(abs(standardized_error)),
      B = mean(standardized_error), standardized_RMSE = sqrt(mean(standardized_error^2)),
      n_circular_fallback = sum(circular_fallback), status = obj$status, .groups = "drop")
    stopifnot(n_distinct(s$n_test) == 1L)
    state_rows[[r$index]] <- bind_cols(catalog[rep(r$index, nrow(s)), ], s)
    f <- obj$predictions |> group_by(fold, state) |> summarise(n_test = n(),
      A = mean(abs(standardized_error)), .groups = "drop")
    fold_rows[[r$index]] <- bind_cols(catalog[rep(r$index, nrow(f)), ], f)
  }
  status <- bind_rows(statuses); states <- bind_rows(state_rows)
  readr::write_csv(status, file.path(out, "task_status.csv"))
  readr::write_csv(states, file.path(out, "heldout_errors.csv"))
  readr::write_csv(bind_rows(fold_rows), file.path(out, "fold_errors.csv"))
  if (any(is.finite(states$A))) recovery_summaries(filter(states, is.finite(A)), out, floor)
  recovery_atomic(list(run_id = run_id, complete = !any(status$status == "failed"),
    statuses = status, heldout_errors = states, provenance = provenance), file.path(out, "recovery_manifest.rds"))
  message("Recovery outputs: ", out)
  if (any(status$status == "failed")) stop("Some recovery tasks failed; successful checkpoints retained. Restart same command.")
  invisible(out)
}

recovery_summaries <- function(states, out, floor) {
  raw <- states |> filter(state == "raw") |> select(task_index, A_raw = A)
  cal <- states |> filter(state == "calibration") |> select(task_index, A_calibration = A)
  comparison <- states |> filter(state != "raw") |>
    left_join(raw, by = "task_index") |> left_join(cal, by = "task_index") |>
    mutate(delta_A = A_raw - A, G = if_else(A_raw > floor, 1 - A / A_raw, NA_real_),
      G_denominator_small = A_raw <= floor,
      context_increment = if_else(state == "context", A_calibration - A, NA_real_))
  # Descriptive raw-magnitude adjustment, not evidence of statistical independence.
  comparison <- comparison |> group_by(comparison_pair_id, state) |> group_modify(function(d, key) {
    d$delta_A_raw_adjusted <- NA_real_
    if (nrow(d) >= 8L && n_distinct(d$A_raw) >= 4L) {
      fit <- lm(delta_A ~ log1p(A_raw) + I(log1p(A_raw)^2), data = d)
      d$delta_A_raw_adjusted <- residuals(fit)
    }
    d$raw_magnitude_bin <- dplyr::ntile(d$A_raw, min(4L, nrow(d)))
    d
  }) |> ungroup()
  readr::write_csv(comparison, file.path(out, "recovery_comparison.csv"))
  overview <- comparison |> group_by(comparison_pair_id, state) |> summarise(n_metrics = n(),
    fraction_improved = mean(delta_A > 0), median_delta_A = median(delta_A),
    median_G = if (any(is.finite(G))) median(G[is.finite(G)]) else NA_real_,
    fraction_context_better_than_calibration = if (first(state) == "context") mean(context_increment > 0) else NA_real_,
    median_context_increment = if (first(state) == "context") median(context_increment) else NA_real_,
    raw_recovered_spearman = if (sd(A_raw) > 0 && sd(A) > 0) cor(A_raw, A, method = "spearman") else NA_real_,
    adjusted_delta_IQR = if (any(is.finite(delta_A_raw_adjusted))) IQR(delta_A_raw_adjusted, na.rm = TRUE) else NA_real_,
    .groups = "drop")
  readr::write_csv(overview, file.path(out, "recovery_overview.csv"))
  bins <- comparison |> group_by(comparison_pair_id, state, raw_magnitude_bin) |>
    summarise(n_metrics = n(), raw_min = min(A_raw), raw_max = max(A_raw),
      delta_median = median(delta_A), delta_q25 = quantile(delta_A, .25), delta_q75 = quantile(delta_A, .75),
      .groups = "drop")
  readr::write_csv(bins, file.path(out, "raw_magnitude_structure.csv"))
}

if (sys.nframe() == 0L) {
  args <- commandArgs(trailingOnly = TRUE)
  if (length(args) != 1L || !args %in% c("--check-inputs", "--run"))
    stop("Use --check-inputs (read-only) or --run (formal recovery analysis)")
  inputs <- recovery_inputs()
  if (length(inputs$problems)) stop(paste(c(inputs$problems,
    "No upstream process was invoked. Supply the existing frozen artifacts."), collapse = "\n"))
  if (args == "--check-inputs") message("Frozen paths and small schemas validated; pair contents are checked only by --run.")
  else recovery_run(inputs)
}
