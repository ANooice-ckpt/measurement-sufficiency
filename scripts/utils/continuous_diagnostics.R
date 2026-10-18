# Frozen-result transformations only: no exposure construction, model fit or bootstrap.
continuous_require <- function(x, columns, label) {
  if (!all(columns %in% names(x))) stop(label, " lacks: ", paste(setdiff(columns, names(x)), collapse = ", "))
}

continuous_unique <- function(x, keys, label) {
  continuous_require(x, keys, label)
  if (anyNA(x[, ..keys]) || anyDuplicated(x[, ..keys])) stop(label, " has missing/duplicate keys")
}

continuous_ratio <- function(numerator, denominator) {
  z <- rep(NA_real_, length(numerator))
  good <- is.finite(numerator) & is.finite(denominator) & denominator > 0
  z[good] <- numerator[good] / denominator[good]
  z
}

continuous_sqrt_ms <- function(ss, n, label) {
  if (!is.finite(ss) || !is.finite(n) || n <= 0) stop(label, ": invalid squared norm/support")
  if (ss < -1e-9) stop(label, ": negative squared norm")
  sqrt(max(0, ss) / n)
}

# All cross-products use the same participant-demeaned rows and day weights as
# the paired FE models. Keeping beta_r fixed removes re-estimation, not demeaning.
continuous_fixed_beta <- function(rr, cc, rc, beta_r, beta_c, outcome_ss, n_days) {
  p <- length(beta_r)
  if (!p || length(beta_c) != p || any(vapply(list(rr, cc, rc),
      function(m) !is.matrix(m) || !identical(dim(m), c(p, p)) || any(!is.finite(m)), logical(1))) ||
      any(!is.finite(c(beta_r, beta_c)))) stop("Invalid fixed-beta design statistics")
  q <- function(b, m, c = b) as.numeric(crossprod(b, m %*% c))
  r_ss <- q(beta_r, rr)
  c_ss <- q(beta_c, cc)
  fitted_diff_ss <- r_ss + c_ss - 2 * q(beta_r, rc, beta_c)
  fixed_diff_ss <- q(beta_r, rr + cc - rc - t(rc))
  list(
    outcome_within_rms = continuous_sqrt_ms(outcome_ss, n_days, "Outcome"),
    reference_reconstructed_rms = continuous_sqrt_ms(r_ss, n_days, "Reference signal"),
    candidate_reconstructed_rms = continuous_sqrt_ms(c_ss, n_days, "Candidate signal"),
    refitted_reconstructed_rms = continuous_sqrt_ms(fitted_diff_ss, n_days, "Refitted difference"),
    fixed_beta_within_candidate_rms = continuous_sqrt_ms(q(beta_r, cc), n_days, "Fixed-beta signal"),
    fixed_beta_within_difference_rms = continuous_sqrt_ms(fixed_diff_ss, n_days, "Fixed-beta difference")
  )
}

continuous_signal_export <- function(summary, terms, blocks, weak_r2 = .001) {
  s <- data.table::as.data.table(data.table::copy(summary))
  tt <- data.table::as.data.table(data.table::copy(terms))
  b <- data.table::as.data.table(data.table::copy(blocks))
  keys <- c("candidate_config", "support_id", "metric", "outcome")
  continuous_unique(s, keys, "Inference summary")
  continuous_unique(tt, c(keys, "term"), "Inference terms")
  continuous_unique(b, c(keys, "participant"), "Inference blocks")
  continuous_require(s, c("status", "metric_geometry", "signal_reference_rms", "signal_candidate_rms",
    "signal_difference_rms", "signal_difference_outcome_sd", "signal_reference_r2", "n_matched_days",
    "n_participants"), "Inference summary")
  continuous_require(b, c("n_days", "reference_xx", "candidate_xx", "reference_candidate_xx", "outcome_within_ss"), "Inference blocks")
  task_key <- function(d) do.call(paste, c(as.list(d[, ..keys]), sep = "\r"))
  task_ids <- task_key(s)
  term_rows <- split(seq_len(nrow(tt)), task_key(tt))
  block_rows <- split(seq_len(nrow(b)), task_key(b))
  extras <- vector("list", nrow(s))
  for (i in seq_len(nrow(s))) {
    if (!identical(s$status[i], "estimated")) next
    bi <- block_rows[[task_ids[i]]]; ti <- term_rows[[task_ids[i]]]
    if (!length(bi) || !length(ti)) stop("Missing sufficient statistics for estimated task: ", task_ids[i])
    terms_expected <- if (s$metric_geometry[i] == "linear") "reference_SD" else c("sin_time", "cos_time")
    tr <- tt[ti][match(terms_expected, term)]
    if (anyNA(tr$term) || nrow(tr) != length(ti)) stop("Wrong exposure coordinates for task: ", task_ids[i])
    bb <- b[bi]
    n <- sum(bb$n_days)
    if (n != s$n_matched_days[i] || nrow(bb) != s$n_participants[i]) stop("Fixed-beta matched support mismatch")
    v <- continuous_fixed_beta(Reduce(`+`, bb$reference_xx), Reduce(`+`, bb$candidate_xx),
      Reduce(`+`, bb$reference_candidate_xx), tr$reference_beta, tr$candidate_beta,
      sum(bb$outcome_within_ss), n)
    original <- c(s$signal_reference_rms[i], s$signal_candidate_rms[i], s$signal_difference_rms[i],
      s$signal_difference_outcome_sd[i])
    reproduced <- c(v$reference_reconstructed_rms, v$candidate_reconstructed_rms,
      v$refitted_reconstructed_rms, continuous_ratio(v$refitted_reconstructed_rms, v$outcome_within_rms))
    difference <- abs(original - reproduced)
    if (any(!is.finite(difference)) || any(difference > 1e-8 * pmax(1, abs(original)))) {
      stop("Sufficient statistics fail to reproduce frozen paired signals: ", task_ids[i])
    }
    extras[[i]] <- c(list(row_id = i, reconstruction_max_absolute_error = max(difference)), v)
  }
  e <- data.table::rbindlist(extras, fill = TRUE)
  if (!nrow(e)) stop("No estimated frozen signal tasks")
  s[, row_id := seq_len(.N)]
  s <- merge(s, e, by = "row_id", all.x = TRUE, sort = FALSE)
  data.table::setorder(s, row_id)
  s[, `:=`(
    refitted_difference_over_outcome_rms = continuous_ratio(signal_difference_rms, outcome_within_rms),
    refitted_difference_over_reference_rms = continuous_ratio(signal_difference_rms, signal_reference_rms),
    fixed_beta_within_difference_over_outcome_rms = continuous_ratio(fixed_beta_within_difference_rms, outcome_within_rms),
    fixed_beta_within_difference_over_reference_rms = continuous_ratio(fixed_beta_within_difference_rms, signal_reference_rms),
    reference_weak_r2_threshold = weak_r2,
    reference_denominator_status = data.table::fcase(
      !is.finite(signal_reference_rms), "unavailable",
      signal_reference_rms == 0, "zero_reference_rms",
      is.finite(signal_reference_r2) & signal_reference_r2 < weak_r2, "nonzero_reference_r2_below_0.001",
      default = "nonzero_reference"),
    propagation_status = ifelse(status == "estimated", "derived_from_frozen_matched_within_design", "unavailable"),
    outcome_unit = unname(c(sleep_quality = "sleep_quality_points_1_to_5", awakenings = "awakening_count",
      awake_duration = "minutes", kss = "KSS_points", positive_affect = "affect_points_0_to_6",
      negative_affect = "affect_points_0_to_6")[outcome]),
    signal_estimand = "same_matched_participant_demeaned_rows_day_weighted",
    ratio_interpretation = "displacement_ratio_not_fraction_of_signal_lost")]
  if (anyNA(s$outcome_unit)) stop("Unknown outcome unit")
  s[, row_id := NULL]
  s[]
}

continuous_signal_summary <- function(s, groups, measures) {
  d <- data.table::melt(s, id.vars = c(groups, "status", "reference_denominator_status"),
    measure.vars = measures, variable.name = "quantity", value.name = "value")
  d[, {
    v <- value[is.finite(value)]
    qs <- if (length(v)) as.numeric(stats::quantile(v, c(.25, .5, .75, .9, 1))) else rep(NA_real_, 5)
    list(n_tasks = .N, n_estimated = sum(status == "estimated"), n_finite = length(v),
      n_reference_zero = sum(reference_denominator_status == "zero_reference_rms"),
      n_reference_weak_nonzero = sum(reference_denominator_status == "nonzero_reference_r2_below_0.001"),
      q25 = qs[1], median = qs[2], q75 = qs[3], p90 = qs[4], maximum = qs[5])
  }, by = c(groups, "quantity")]
}

continuous_risk_export <- function(scores) {
  d <- data.table::as.data.table(data.table::copy(scores))[target == "mean_risk"]
  keys <- c("task_index", "repeat_id", "comparison_pair_id", "metric", "support_id")
  continuous_unique(d, c(keys, "state"), "Continuous prediction scores")
  if (!nrow(d) || anyNA(d$state) || !"null" %in% d$state || any(!is.finite(d$mae)) ||
      any(!is.finite(d$mse)) || any(d$mae < 0 | d$mse < 0)) stop("Invalid continuous prediction losses/baseline")
  expected <- sort(unique(d$state))
  state_sets <- d[, .(complete_states = identical(sort(state), expected)), by = keys]
  if (any(!state_sets$complete_states)) stop("Unpaired information states in continuous prediction scores")
  baseline <- d[state == "null", c(keys, "mae", "mse"), with = FALSE]
  data.table::setnames(baseline, c("mae", "mse"), c("null_mae", "null_mse"))
  d <- merge(d, baseline, by = keys, all.x = TRUE, sort = FALSE)
  d[, `:=`(mae_skill_pct = 100 * (1 - continuous_ratio(mae, null_mae)),
    mse_skill_pct = 100 * (1 - continuous_ratio(mse, null_mse)),
    mae_baseline_status = ifelse(null_mae > 0, "positive", "zero_skill_undefined"),
    mse_baseline_status = ifelse(null_mse > 0, "positive", "zero_skill_undefined"))]
  # Frozen scores already average errors over held-out rows within a target.
  # Each target then gets equal weight; only afterwards form the loss ratio.
  p <- d[, .(n_metrics = data.table::uniqueN(metric), n_tasks = .N,
    n_zero_null_mae = sum(null_mae == 0), n_zero_null_mse = sum(null_mse == 0),
    mae = mean(mae), mse = mean(mse), null_mae = mean(null_mae), null_mse = mean(null_mse)),
    by = .(comparison_pair_id, repeat_id, state)]
  if (any(p$n_metrics != p$n_tasks)) stop("Multiple tasks per metric would change equal-target weighting")
  p[, `:=`(mae_skill_pct = 100 * (1 - continuous_ratio(mae, null_mae)),
    mse_skill_pct = 100 * (1 - continuous_ratio(mse, null_mse)),
    validation_partition = ifelse(repeat_id == 1L, "primary", "repeat"),
    response = "D_abs_z_frozen_RQ1_scale",
    aggregation = "mean_target_losses_then_ratio_to_outer_training_mean",
    mse_skill_definition = "training_mean_relative_skill_not_conventional_R_squared")]
  list(task = d, contrast = p)
}
