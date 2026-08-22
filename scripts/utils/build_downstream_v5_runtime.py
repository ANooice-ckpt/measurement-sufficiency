#!/usr/bin/env python3
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
OUT = ROOT / "results" / "runtime"
OUT.mkdir(parents=True, exist_ok=True)


def replace_once(text: str, old: str, new: str, label: str) -> str:
    n = text.count(old)
    if n != 1:
        raise RuntimeError(f"{label}: expected exactly one anchor, found {n}")
    return text.replace(old, new, 1)


def copy_runtime(src_name: str, out_name: str, transform=None):
    src = ROOT / "scripts" / src_name
    text = src.read_text(encoding="utf-8")
    if transform is not None:
        text = transform(text)
    dest = OUT / out_name
    dest.write_text(text, encoding="utf-8")
    print(dest.relative_to(ROOT))


def patch_rq2(text: str) -> str:
    # ends_with("_b") would also remove scientific keys such as window_id_b,
    # n_days_b and analysis_unit_id_b. Drop only temporary predictor aliases.
    old = 'select(-longer_window_level, -longer_window_variability, -ends_with("_b"), -shorter_window_variability)'
    new = '''select(
      -longer_window_level, -longer_window_variability,
      -external_radiation_b, -external_direct_fraction_b, -external_cloud_b,
      -solar_noon_elevation_deg_b, -shorter_window_variability
    )'''
    text = replace_once(text, old, new, "RQ2 temporary duration-column patch")

    # Participant-grouped CV must use one frozen fold assignment per scientific
    # task. Re-drawing folds inside each model-family/outcome loop makes model
    # performance comparisons depend on different test participants.
    old_seed = '''  set.seed(task$seed)
  coefs <- list(); perfs <- list(); ci <- 0L; pi <- 0L'''
    new_seed = '''  set.seed(task$seed)
  pm_base <- dat |>
    distinct(site, participant_key) |>
    group_by(site) |>
    mutate(fold = sample(rep(seq_len(task$folds), length.out = n()))) |>
    ungroup()
  coefs <- list(); perfs <- list(); ci <- 0L; pi <- 0L'''
    text = replace_once(text, old_seed, new_seed, "RQ2 frozen CV-fold map patch")

    old_pm = '''    pm <- d |> distinct(site, participant_key) |>
      group_by(site) |>
      mutate(fold = sample(rep(seq_len(task$folds), length.out = n()))) |>
      ungroup()
    d_cv <- d |> left_join(pm, by = c("site", "participant_key"))'''
    new_pm = '''    d_cv <- d |> left_join(pm_base, by = c("site", "participant_key"))'''
    text = replace_once(text, old_pm, new_pm, "RQ2 reuse frozen CV folds patch")

    # Four study sites are too few for LOSO to serve as the primary CV estimate.
    # Keep the participant-grouped five-fold CV, which matches the independent
    # sampling unit, and remove LOSO without changing any RQ2 estimand/full fit.
    old_cv = '''    for (scheme in c("participant_grouped", "leave_site_out")) {
      obs <- pred <- numeric()
      splits <- if (scheme == "participant_grouped") sort(unique(d_cv$fold)) else sort(unique(d_cv$site))
      for (sp in splits) {
        if (scheme == "participant_grouped") {
          tr <- d_cv |> filter(fold != sp)
          te <- d_cv |> filter(fold == sp)
        } else {
          tr <- d_cv |> filter(site != sp)
          te <- d_cv |> filter(site == sp)
        }
        if (nrow(tr) < 20L || nrow(te) < 2L || n_distinct(tr$participant_key) < 3L) next
        ss <- scale_train_test(tr, te, usable)
        ff <- fit_one(ss$tr, outcome, ss$keep)
        if (is.null(ff$fit)) next
        pr <- tryCatch(as.numeric(predict(ff$fit, newdata = ss$te, level = 0)),
                       error = function(e) rep(NA_real_, nrow(ss$te)))
        obs <- c(obs, ss$te[[outcome]]); pred <- c(pred, pr)
      }
      perf <- performance(obs, pred)
      pi <- pi + 1L
      perfs[[pi]] <- tibble(
        dimension = meta$dimension, comparison_pair_id = meta$comparison_pair_id, metric = meta$metric,
        outcome = oname, model_family = fname, validation_scheme = scheme,
        n_participants = n_distinct(d$participant_key), n_sites = n_distinct(d$site),
        n_test = perf$n_test, rmse = perf$rmse, mae = perf$mae, r2 = perf$r2
      )
    }'''
    new_cv = '''    obs <- pred <- numeric()
    for (sp in sort(unique(d_cv$fold))) {
      tr <- d_cv |> filter(fold != sp)
      te <- d_cv |> filter(fold == sp)
      if (nrow(tr) < 20L || nrow(te) < 2L || n_distinct(tr$participant_key) < 3L) next
      ss <- scale_train_test(tr, te, usable)
      ff <- fit_one(ss$tr, outcome, ss$keep)
      if (is.null(ff$fit)) next
      pr <- tryCatch(as.numeric(predict(ff$fit, newdata = ss$te, level = 0)),
                     error = function(e) rep(NA_real_, nrow(ss$te)))
      obs <- c(obs, ss$te[[outcome]]); pred <- c(pred, pr)
    }
    perf <- performance(obs, pred)
    pi <- pi + 1L
    perfs[[pi]] <- tibble(
      dimension = meta$dimension, comparison_pair_id = meta$comparison_pair_id, metric = meta$metric,
      outcome = oname, model_family = fname, validation_scheme = "participant_grouped",
      n_participants = n_distinct(d$participant_key), n_sites = n_distinct(d$site),
      n_test = perf$n_test, rmse = perf$rmse, mae = perf$mae, r2 = perf$r2
    )'''
    text = replace_once(text, old_cv, new_cv, "RQ2 participant-only CV patch")

    # Model checkpoints from the former full+participant-CV+LOSO pipeline are
    # incompatible with participant-CV-only validation. Bump checkpoint identity
    # without changing RQ2_VERSION, so existing expensive model-input shards are
    # still reused.
    old_checkpoint = 'paste0(RQ2_VERSION, "__core__", CORE_VERSION, "__folds__", RQ2_CV_FOLDS)'
    new_checkpoint = 'paste0(RQ2_VERSION, "__core__", CORE_VERSION, "__participant_cv__", RQ2_CV_FOLDS)'
    n_checkpoint = text.count(old_checkpoint)
    if n_checkpoint != 2:
        raise RuntimeError(f"RQ2 checkpoint-version patch: expected exactly two anchors, found {n_checkpoint}")
    text = text.replace(old_checkpoint, new_checkpoint)

    # Lightweight progress: every task appends one line after either reuse or a
    # newly completed checkpoint. This avoids touching the parallel scheduler and
    # therefore adds effectively no model-compute overhead.
    old_fit_checkpoint = '''fit_task_checkpoint <- function(task) {
  cached <- if (file.exists(task$checkpoint_path)) tryCatch(readRDS(task$checkpoint_path), error = function(e) NULL) else NULL
  if (!is.null(cached) && identical(cached$checkpoint_version, task$checkpoint_version) && isTRUE(cached$complete)) {
    return(list(index = task$index, path = task$checkpoint_path, reused = TRUE))
  }
  obj <- fit_task(task)
  tmp <- paste0(task$checkpoint_path, ".tmp.", Sys.getpid())
  saveRDS(obj, tmp, compress = FALSE)
  if (file.exists(task$checkpoint_path)) unlink(task$checkpoint_path)
  if (!file.rename(tmp, task$checkpoint_path)) stop("Could not install RQ2 model checkpoint")
  list(index = task$index, path = task$checkpoint_path, reused = FALSE)
}'''
    new_fit_checkpoint = '''fit_task_checkpoint <- function(task) {
  log_progress <- function(status) {
    line <- paste(
      format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"), task$index, status,
      task$meta$dimension[[1]], task$meta$comparison_pair_id[[1]], task$meta$metric[[1]],
      sep = "\\t"
    )
    cat(line, "\\n", file = task$progress_path, append = TRUE, sep = "")
  }
  cached <- if (file.exists(task$checkpoint_path)) tryCatch(readRDS(task$checkpoint_path), error = function(e) NULL) else NULL
  if (!is.null(cached) && identical(cached$checkpoint_version, task$checkpoint_version) && isTRUE(cached$complete)) {
    log_progress("reused")
    return(list(index = task$index, path = task$checkpoint_path, reused = TRUE))
  }
  obj <- fit_task(task)
  tmp <- paste0(task$checkpoint_path, ".tmp.", Sys.getpid())
  saveRDS(obj, tmp, compress = FALSE)
  if (file.exists(task$checkpoint_path)) unlink(task$checkpoint_path)
  if (!file.rename(tmp, task$checkpoint_path)) stop("Could not install RQ2 model checkpoint")
  log_progress("completed")
  list(index = task$index, path = task$checkpoint_path, reused = FALSE)
}'''
    text = replace_once(text, old_fit_checkpoint, new_fit_checkpoint, "RQ2 task-progress writer patch")

    old_model_tasks = '''model_tasks <- lapply(seq_len(nrow(task_catalog)), function(i) {'''
    new_model_tasks = '''PROGRESS_LOG <- file.path("results", "logs", "rq2_model_progress.tsv")
dir.create(dirname(PROGRESS_LOG), recursive = TRUE, showWarnings = FALSE)
if (RUN_MODELS) {
  writeLines("timestamp\\ttask_index\\tstatus\\tdimension\\tcomparison_pair_id\\tmetric", PROGRESS_LOG)
}

model_tasks <- lapply(seq_len(nrow(task_catalog)), function(i) {'''
    text = replace_once(text, old_model_tasks, new_model_tasks, "RQ2 progress-log initialization patch")

    old_task_fields = '''    checkpoint_path = checkpoint_path,
    cost = sum(shard_manifest$bytes[shard_manifest$task_index == meta$task_index], na.rm = TRUE)'''
    new_task_fields = '''    checkpoint_path = checkpoint_path,
    progress_path = normalizePath(PROGRESS_LOG, winslash = "/", mustWork = FALSE),
    cost = sum(shard_manifest$bytes[shard_manifest$task_index == meta$task_index], na.rm = TRUE)'''
    text = replace_once(text, old_task_fields, new_task_fields, "RQ2 progress-path task patch")

    old_model_message = '''  message("RQ2 v5 models: ", length(scheduled), " tasks across ", RQ2_WORKERS, " PSOCK workers")'''
    new_model_message = '''  message("RQ2 v5 models: ", length(scheduled), " tasks across ", RQ2_WORKERS, " PSOCK workers; participant-grouped ", RQ2_CV_FOLDS, "-fold CV only")
  message("RQ2 model progress: ", PROGRESS_LOG, " (one line per processed task; total = ", length(scheduled), ")")'''
    text = replace_once(text, old_model_message, new_model_message, "RQ2 progress-message patch")

    old_report = '  "Cross-validation holds out all rows belonging to the selected participant/site; no match()-based first-row reduction is used.",'
    new_report = '  "Cross-validation is participant-grouped only and holds out all rows belonging to each selected participant; LOSO is not run.",'
    text = replace_once(text, old_report, new_report, "RQ2 run-report CV patch")

    old_invariant = '# - CV splits retain every row belonging to a held-out participant/site.'
    new_invariant = '# - participant-grouped CV retains every row belonging to each held-out participant.'
    text = replace_once(text, old_invariant, new_invariant, "RQ2 invariant-comment patch")
    return text


def patch_rq3(text: str) -> str:
    # Base merge() on tibbles does not accept data.table's allow.cartesian.
    # dplyr's explicit many-to-many join is the intended Cartesian product
    # inside each already-fixed participant/support/metric group.
    old = 'merge(a, b, by = c("support_id", "site", "Id"), allow.cartesian = TRUE) |>'
    new = 'inner_join(a, b, by = c("support_id", "site", "Id"), relationship = "many-to-many") |>'
    text = replace_once(text, old, new, "RQ3 Cartesian join patch")

    # The joint scale anchor must remain the frozen high-information state,
    # eye / MEDI / 10 s / 6 d. Pooling chest/wrist or LIGHT into the denominator
    # would make the standardized joint distortion depend on the alternatives.
    old_anchor = 'filter(resolution_s == 10L, n_days == 6L, available, is.finite(value)) |>'
    new_anchor = 'filter(placement == "eye", optical == "MEDI", resolution_s == 10L, n_days == 6L, available, is.finite(value)) |>'
    text = replace_once(text, old_anchor, new_anchor, "RQ3 joint anchor patch")

    # Unavailable state representations are not observed joint states.
    old_states = '''state_parts[[i]] <- z |>
    distinct(support_id, placement, optical, resolution_s, n_days, metric, metric_class, metric_geometry)'''
    new_states = '''state_parts[[i]] <- z |>
    filter(available, is.finite(value)) |>
    distinct(support_id, placement, optical, resolution_s, n_days, metric, metric_class, metric_geometry)'''
    text = replace_once(text, old_states, new_states, "RQ3 available joint-state catalogue patch")

    # A state is analysable only when its support/metric anchor dispersion is
    # defined. Otherwise it is unavailable, not a false upper boundary.
    old_catalog = '''joint_state_catalog <- bind_rows(state_parts) |>
  distinct() |>
  mutate(config_id = paste0("r", resolution_s, "__d", n_days))'''
    new_catalog = '''joint_state_catalog <- bind_rows(state_parts) |>
  distinct() |>
  left_join(joint_anchor, by = c("support_id", "metric", "metric_geometry")) |>
  filter(is.finite(standardizer), standardizer > 0) |>
  select(-standardizer) |>
  mutate(config_id = paste0("r", resolution_s, "__d", n_days))'''
    text = replace_once(text, old_catalog, new_catalog, "RQ3 analysable joint-state catalogue patch")

    # joint_state_catalog names the generic state config_id, while outgoing
    # comparisons necessarily carry the same state as config_a_id.
    old_joint_join = '"n_days" = "n_days_a", "config_id", "metric", "metric_class", "metric_geometry")'
    new_joint_join = '"n_days" = "n_days_a", "config_id" = "config_a_id", "metric", "metric_class", "metric_geometry")'
    text = replace_once(text, old_joint_join, new_joint_join, "RQ3 joint config-id join patch")

    # Fig. 4d is a pair-level fraction across metrics. Grouping by metric_class
    # creates duplicate epsilon rows that the plot then incorrectly connects as
    # one line, so metric class must not define the coverage estimand.
    old_cov = 'group_by(dimension, comparison_pair_id, config_a_id, config_b_id, metric_class) |>'
    new_cov = 'group_by(dimension, comparison_pair_id, config_a_id, config_b_id) |>'
    text = replace_once(text, old_cov, new_cov, "RQ3 unordered coverage grouping patch")
    return text


copy_runtime("12_rq2_analysis_v5.R", "12_rq2_analysis_v5.runtime.R", patch_rq2)
copy_runtime("13_plot_rq2_v5.R", "13_plot_rq2_v5.runtime.R")
copy_runtime("14_rq3_analysis_v5.R", "14_rq3_analysis_v5.runtime.R", patch_rq3)
copy_runtime("15_plot_rq3_v5.R", "15_plot_rq3_v5.runtime.R")
print("Downstream v5 runtime generated")
