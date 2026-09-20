# Downstream association preservation; no light-series or metric operators.
# Outcomes are linear within-person projections in their native diary/EMA units.
source("scripts/utils/analysis_design.R")
source("scripts/utils/artifact_validation.R")
source("scripts/utils/rq1_inference_contract.R")
if (!exists("rq1_pairwise_load", mode = "function")) {
  source("scripts/utils/rq1_pairwise_artifacts.R")
}

rq1_inference_version <- function(rq1_version) {
  paste0("rq1_inference_v5_composition_anchor8__", rq1_version)
}

rq1_inference_anchor_map <- function() {
  tibble::tribble(
    ~candidate_config, ~placement, ~optical, ~resolution_s, ~dimension, ~comparison_pair_id, ~contrast_label, ~contrast_order,
    "chest__MEDI__10s", "chest", "MEDI", 10L, "placement", "chest_vs_eye", "Chest vs eye", 1L,
    "wrist__MEDI__10s", "wrist", "MEDI", 10L, "placement", "wrist_vs_eye", "Wrist vs eye", 2L,
    "eye__LIGHT__10s", "eye", "LIGHT", 10L, "optical", "LIGHT_vs_MEDI", "LIGHT vs MEDI", 3L,
    "eye__MEDI__20s", "eye", "MEDI", 20L, "temporal", "20s_vs_10s", "20 s vs 10 s", 4L,
    "eye__MEDI__30s", "eye", "MEDI", 30L, "temporal", "30s_vs_10s", "30 s vs 10 s", 5L,
    "eye__MEDI__40s", "eye", "MEDI", 40L, "temporal", "40s_vs_10s", "40 s vs 10 s", 6L,
    "eye__MEDI__60s", "eye", "MEDI", 60L, "temporal", "60s_vs_10s", "60 s vs 10 s", 7L,
    "eye__MEDI__120s", "eye", "MEDI", 120L, "temporal", "120s_vs_10s", "120 s vs 10 s", 8L
  )
}

rq1_outcome_metadata <- function(outcomes = rq1_inference_contract()$outcomes) {
  contract <- rq1_inference_contract()
  tibble::tibble(
    outcome = outcomes,
    outcome_domain = unname(contract$outcome_domain[outcomes]),
    outcome_label = unname(contract$outcome_label[outcomes])
  )
}

rq1_sleep_outcomes <- function(x, site) {
  contract <- rq1_inference_contract()
  sleep_outcomes <- names(contract$outcome_domain)[contract$outcome_domain == "Sleep"]
  required <- c("Id", "wake", "sleepprep", "sleepquality", "awakenings", "awake_duration")
  if (!all(required %in% names(x))) stop(site, " sleepdiary lacks required outcome fields")
  tz <- attr(x$wake, "tzone")
  if (length(tz) != 1L || !nzchar(tz)) stop(site, " diary has no explicit wake timezone")
  quality_levels <- c("Very poor", "Poor", "Fair", "Good", "Very good")
  quality <- as.character(x$sleepquality)
  if (any(!is.na(quality) & !quality %in% quality_levels)) stop(site, " unknown sleep quality category")
  if (!is.numeric(x$awakenings) || !is.numeric(x$awake_duration)) {
    stop(site, " expected harmonized numeric awakening count and awake minutes")
  }
  awake <- if (inherits(x$awake_duration, "difftime")) {
    as.numeric(x$awake_duration, units = "mins")
  } else as.numeric(x$awake_duration)
  out <- tibble::tibble(
    site = site, Id = as.character(x$Id),
    wake_date = as.Date(x$wake, tz = tz),
    # D's complete calendar-day exposure is paired with the next morning D+1.
    Date = as.Date(x$wake, tz = tz) - 1L,
    valid_interval = !is.na(x$sleepprep) & !is.na(x$wake) & x$wake > x$sleepprep,
    sleep_quality = as.numeric(match(quality, quality_levels)),
    awakenings = as.numeric(x$awakenings), awake_duration = awake
  )

  # A diary without participant id or wake date cannot be paired to exposure day D.
  # Drop only those structurally unalignable rows and report the count explicitly.
  missing_key <- is.na(out$Id) | !nzchar(out$Id) | is.na(out$Date)
  if (any(missing_key)) {
    message("RQ1 inference: ", site, " sleepdiary drops ", sum(missing_key),
            " row(s) without participant/wake-date alignment")
    out <- out[!missing_key, , drop = FALSE]
  }
  if (!nrow(out)) return(tibble::tibble())

  duplicate_days <- out |>
    dplyr::count(site, Id, Date, name = "n_records") |>
    dplyr::filter(n_records > 1L)
  if (nrow(duplicate_days)) {
    message("RQ1 inference: ", site, " sleepdiary resolves ", nrow(duplicate_days),
            " duplicate participant-day(s) conservatively")
  }

  long <- out |>
    tidyr::pivot_longer(
      dplyr::all_of(sleep_outcomes), names_to = "outcome", values_to = "outcome_value"
    ) |>
    dplyr::mutate(
      row_reason = dplyr::case_when(
        !valid_interval ~ "invalid_sleep_interval",
        !is.finite(outcome_value) ~ "missing_outcome",
        outcome != "sleep_quality" & outcome_value < 0 ~ "negative_outcome",
        outcome == "awakenings" & outcome_value != floor(outcome_value) ~ "noninteger_count",
        TRUE ~ NA_character_
      )
    )

  collapse_one <- function(g, key) {
    valid <- is.na(g$row_reason) & is.finite(g$outcome_value)
    values <- unique(as.numeric(g$outcome_value[valid]))
    n_records <- nrow(g)
    if (length(values) == 1L) {
      value <- values[[1]]
      reason <- NA_character_
      duplicate_status <- if (n_records > 1L) "consistent_duplicate_collapsed" else "single"
    } else if (length(values) > 1L) {
      value <- NA_real_
      reason <- "conflicting_duplicate_sleepdiary"
      duplicate_status <- "conflicting_duplicate_excluded"
    } else {
      value <- NA_real_
      reasons <- unique(g$row_reason[!is.na(g$row_reason)])
      priority <- c("noninteger_count", "negative_outcome", "missing_outcome", "invalid_sleep_interval")
      hit <- priority[priority %in% reasons]
      reason <- if (length(hit)) hit[[1]] else "missing_outcome"
      duplicate_status <- if (n_records > 1L) "duplicate_no_valid_value" else "single_invalid"
    }
    tibble::tibble(
      outcome_value = value,
      outcome_reason = reason,
      outcome_n_observations = if (is.na(reason)) 1L else 0L,
      outcome_n_records = n_records,
      outcome_duplicate_status = duplicate_status,
      outcome_source = "sleepdiary"
    )
  }

  collapsed <- long |>
    dplyr::group_by(site, Id, Date, outcome) |>
    dplyr::group_modify(collapse_one) |>
    dplyr::ungroup() |>
    dplyr::left_join(rq1_outcome_metadata(sleep_outcomes), by = "outcome", relationship = "many-to-one")
  ms_assert_unique(collapsed, c("site", "Id", "Date", "outcome"), paste(site, "sleep outcomes"))
  collapsed
}

rq1_ema_scale_numeric <- function(x, labels, first_code) {
  if (is.factor(x)) {
    observed_levels <- levels(x)
    if (!identical(observed_levels, labels)) {
      stop("EMA factor labels differ from the frozen MeLiDos questionnaire scale", call. = FALSE)
    }
    out <- as.integer(x) + first_code - 1L
  } else if (is.numeric(x)) {
    out <- as.numeric(x)
  } else {
    ch <- as.character(x)
    out <- suppressWarnings(as.numeric(ch))
    unresolved <- !is.na(ch) & !nzchar(trimws(ch)) == FALSE & !is.finite(out)
    if (any(unresolved)) {
      idx <- match(ch[unresolved], labels)
      if (anyNA(idx)) stop("EMA values do not match the frozen MeLiDos questionnaire scale", call. = FALSE)
      out[unresolved] <- idx + first_code - 1L
    }
  }
  allowed <- seq.int(first_code, length(labels) + first_code - 1L)
  out[!is.na(out) & !out %in% allowed] <- NA_real_
  as.numeric(out)
}

rq1_ema_daily_outcomes <- function(x, site) {
  contract <- rq1_inference_contract()
  required <- c("Id", "Datetime", "anxious", "elated", "sad", "angry", "irritable", "energetic", "kss")
  if (!all(required %in% names(x))) stop(site, " currentconditions lacks required EMA fields")
  if (!inherits(x$Datetime, "POSIXct")) stop(site, " currentconditions Datetime must be POSIXct")
  tz <- attr(x$Datetime, "tzone")
  if (length(tz) != 1L || !nzchar(tz)) stop(site, " currentconditions has no explicit local timezone")

  mood_labels <- c("Not at all", "Slightly", "Somewhat", "Moderately", "Quite a bit", "Very much so", "Extremely")
  kss_labels <- c(
    "Extremely alert", "Very alert", "Alert", "Rather alert", "Neither alert nor sleepy",
    "Some signs of sleepiness", "Sleepy, but no effort to keep awake",
    "Sleepy, but some effort to keep awake",
    "Very sleepy, great effort to keep awake, fighting sleep",
    "Extremely sleepy, can't keep awake"
  )
  mood_vars <- c("anxious", "elated", "sad", "angry", "irritable", "energetic")
  z <- tibble::as_tibble(x) |>
    dplyr::transmute(
      site = site, Id = as.character(Id), Datetime,
      anxious = rq1_ema_scale_numeric(anxious, mood_labels, 0L),
      elated = rq1_ema_scale_numeric(elated, mood_labels, 0L),
      sad = rq1_ema_scale_numeric(sad, mood_labels, 0L),
      angry = rq1_ema_scale_numeric(angry, mood_labels, 0L),
      irritable = rq1_ema_scale_numeric(irritable, mood_labels, 0L),
      energetic = rq1_ema_scale_numeric(energetic, mood_labels, 0L),
      kss = rq1_ema_scale_numeric(kss, kss_labels, 1L)
    ) |>
    dplyr::filter(!is.na(Datetime)) |>
    dplyr::mutate(
      Date = as.Date(Datetime, tz = tz),
      local_minute = as.numeric(format(Datetime, "%H", tz = tz)) * 60 +
        as.numeric(format(Datetime, "%M", tz = tz)) +
        as.numeric(format(Datetime, "%S", tz = tz)) / 60
    )
  if (!nrow(z)) return(tibble::tibble())

  slot_minutes <- contract$ema_slots_h * 60
  distance_matrix <- abs(outer(z$local_minute, slot_minutes, "-"))
  nearest <- max.col(-distance_matrix, ties.method = "first")
  z$ema_slot_h <- contract$ema_slots_h[nearest]
  z$slot_distance_min <- distance_matrix[cbind(seq_len(nrow(z)), nearest)]
  z <- z |>
    dplyr::filter(slot_distance_min <= contract$ema_slot_tolerance_min) |>
    dplyr::arrange(site, Id, Date, ema_slot_h, slot_distance_min, Datetime) |>
    dplyr::group_by(site, Id, Date, ema_slot_h) |>
    dplyr::slice_head(n = 1L) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      positive_affect = dplyr::if_else(
        is.finite(elated) & is.finite(energetic), (elated + energetic) / 2, NA_real_
      ),
      negative_affect = dplyr::if_else(
        is.finite(anxious) & is.finite(sad) & is.finite(angry) & is.finite(irritable),
        (anxious + sad + angry + irritable) / 4, NA_real_
      )
    )

  ema_outcomes <- c("kss", "positive_affect", "negative_affect")
  out <- z |>
    dplyr::select(site, Id, Date, ema_slot_h, dplyr::all_of(ema_outcomes)) |>
    tidyr::pivot_longer(dplyr::all_of(ema_outcomes), names_to = "outcome", values_to = "response_value") |>
    dplyr::group_by(site, Id, Date, outcome) |>
    dplyr::summarise(
      outcome_n_observations = sum(is.finite(response_value)),
      outcome_value = if (outcome_n_observations >= contract$ema_min_slots) {
        mean(response_value[is.finite(response_value)])
      } else NA_real_,
      .groups = "drop"
    ) |>
    dplyr::mutate(
      outcome_reason = dplyr::if_else(
        outcome_n_observations >= contract$ema_min_slots,
        NA_character_, "insufficient_ema_slots"
      ),
      outcome_source = "currentconditions"
    ) |>
    dplyr::left_join(rq1_outcome_metadata(ema_outcomes), by = "outcome", relationship = "many-to-one")
  ms_assert_unique(out, c("site", "Id", "Date", "outcome"), paste(site, "daily EMA outcomes"))
  out
}

# RQ1 already freezes the exact participant-day support and both state values for
# every scientific pair. Reuse those values directly rather than depending on the
# larger Core metric cube again. For all eight anchor contrasts state_a is the
# lower-information candidate and state_b is the eye/MEDI/10-s reference.
rq1_inference_pairs <- function(pairwise) {
  contract <- rq1_inference_contract()
  anchors <- rq1_inference_anchor_map()
  wanted <- paste(anchors$dimension, anchors$comparison_pair_id, sep = "|")
  columns <- c(
    "dimension", "comparison_pair_id", "support_id", "site", "Id",
    "analysis_unit_type", "Date", "metric", "metric_class", "metric_scope",
    "metric_geometry", "value_a", "value_b", "available_a", "available_b",
    "pair_available", "pair_unavailable_reason"
  )
  x <- rq1_pairwise_load(
    pairwise,
    columns = columns,
    filter_fn = function(z) {
      z |>
        dplyr::filter(
          analysis_unit_type == "participant_day",
          paste(dimension, comparison_pair_id, sep = "|") %in% wanted
        )
    }
  ) |>
    dplyr::mutate(Id = as.character(Id), Date = as.Date(Date)) |>
    dplyr::inner_join(
      anchors,
      by = c("dimension", "comparison_pair_id"),
      relationship = "many-to-one"
    ) |>
    dplyr::transmute(
      support_id, site, Id, Date, metric, metric_class, metric_scope, metric_geometry,
      reference_value = value_b, candidate_value = value_a,
      reference_available = available_b, candidate_available = available_a,
      candidate_config, placement, optical, resolution_s, dimension, comparison_pair_id,
      contrast_label, contrast_order, reference_config = contract$reference_config,
      pair_reason = dplyr::case_when(
        !dplyr::coalesce(reference_available, FALSE) | !is.finite(reference_value) ~ "reference_unavailable",
        !dplyr::coalesce(candidate_available, FALSE) | !is.finite(candidate_value) ~
          dplyr::coalesce(pair_unavailable_reason, "candidate_unavailable"),
        !dplyr::coalesce(pair_available, FALSE) ~ dplyr::coalesce(pair_unavailable_reason, "pair_unavailable"),
        TRUE ~ NA_character_
      )
    )

  ms_assert_unique(
    x,
    c("candidate_config", "support_id", "site", "Id", "Date", "metric"),
    "RQ1 frozen inferential anchor pairs"
  )
  x
}

# The 20-s-vs-10-s anchor uses the canonical eye-only maximal support already
# defined by RQ1: eye_medi for ordinary metrics and eye_full for MDER/nvRD. Its
# state_b therefore provides one canonical eye/MEDI/10-s reference landscape
# without reopening the Core metric cube.
rq1_inference_reference_pairs <- function(anchor_pairs) {
  contract <- rq1_inference_contract()
  x <- anchor_pairs |>
    dplyr::filter(candidate_config == "eye__MEDI__20s") |>
    dplyr::transmute(
      support_id, site, Id, Date, metric, metric_class, metric_scope, metric_geometry,
      reference_value, candidate_value = reference_value,
      reference_available, candidate_available = reference_available,
      candidate_config = contract$reference_config, reference_config = contract$reference_config,
      placement = "eye", optical = "MEDI", resolution_s = 10L,
      dimension = "reference", comparison_pair_id = "reference", contrast_label = "Reference", contrast_order = 0L,
      pair_reason = dplyr::if_else(
        dplyr::coalesce(reference_available, FALSE) & is.finite(reference_value),
        NA_character_, "reference_unavailable"
      )
    )
  ms_assert_unique(
    x,
    c("support_id", "site", "Id", "Date", "metric"),
    "canonical RQ1 reference association pairs"
  )
  x
}

# Sufficient statistics after participant demeaning. A bootstrap draw weights
# entire participant blocks identically in reference and candidate fits.
rq1_inference_stats <- function(X, y, participant) {
  groups <- split(seq_along(y), participant)
  lapply(groups, function(idx) {
    xx <- X[idx, , drop = FALSE]
    xx <- sweep(xx, 2L, colMeans(xx))
    yy <- y[idx] - mean(y[idx])
    list(xx = crossprod(xx), xy = crossprod(xx, yy))
  })
}

rq1_inference_solve <- function(blocks, weights) {
  xx <- Reduce(`+`, Map(function(b, w) b$xx * w, blocks, weights))
  xy <- Reduce(`+`, Map(function(b, w) b$xy * w, blocks, weights))
  if (!all(is.finite(xx)) || rcond(xx) < 1e-10 || max(abs(xx)) < 1e-12) {
    return(rep(NA_real_, nrow(xx)))
  }
  as.numeric(solve(xx, xy))
}

rq1_inference_solve_draws <- function(blocks, weights) {
  p <- nrow(blocks[[1]]$xx)
  xx <- vapply(blocks, function(b) as.vector(b$xx), numeric(p * p)) %*% weights
  xy <- vapply(blocks, function(b) as.vector(b$xy), numeric(p)) %*% weights
  matrix(vapply(seq_len(ncol(weights)), function(i) {
    a <- matrix(xx[, i], nrow = p)
    if (!all(is.finite(a)) || rcond(a) < 1e-10 || max(abs(a)) < 1e-12) return(rep(NA_real_, p))
    as.numeric(solve(a, xy[, i]))
  }, numeric(p)), nrow = p)
}

rq1_inference_quadnorm <- function(v, covariance) {
  v <- as.numeric(v)
  covariance <- as.matrix(covariance)
  if (!length(v) || any(!is.finite(v)) || any(!is.finite(covariance)) ||
      nrow(covariance) != length(v) || ncol(covariance) != length(v) ||
      rcond(covariance) < 1e-10) return(NA_real_)
  q <- as.numeric(crossprod(v, solve(covariance, v)))
  if (!is.finite(q) || q < -1e-8) return(NA_real_)
  sqrt(max(0, q))
}

# Additive squared-error decomposition in the actual exposure design space.
# The stable term includes a common offset; it is not a between-person variance.
rq1_distortion_components <- function(xr, xc, participant, site) {
  blocks <- lapply(split(seq_len(nrow(xr)), participant), function(idx) {
    e <- xc[idx,,drop=FALSE] - xr[idx,,drop=FALSE]
    stable <- colMeans(e)
    within <- sweep(e, 2L, stable)
    tibble::tibble(participant = participant[idx[1]], site = site[idx[1]],
      n_days = length(idx), between_ss = length(idx) * sum(stable^2),
      within_ss = sum(within^2), total_ss = sum(e^2))
  })
  blocks <- dplyr::bind_rows(blocks)
  if (any(abs(blocks$total_ss - blocks$between_ss - blocks$within_ss) >
          1e-10 * pmax(1, blocks$total_ss))) stop("Distortion decomposition identity failed")
  n <- sum(blocks$n_days)
  total <- sum(blocks$total_ss) / n
  between <- sum(blocks$between_ss) / n
  within <- sum(blocks$within_ss) / n
  list(summary = tibble::tibble(distortion_total_ms = total,
    distortion_between_ms = between, distortion_within_ms = within,
    D_T = sqrt(total), f_W = if (total > 1e-15) within / total else NA_real_,
    distortion_within_rms = sqrt(within),
    distortion_within_fraction = if (total > 1e-15) within / total else NA_real_),
    blocks = blocks)
}

# Descriptive nested model, separately for every domain and exposure geometry.
# The added predictor is within share, per primary-task SD, conditional on
# log1p(matched-support total RMS). Frozen A is a
# covariate, never replaced by outcome-matched distortion. No model selection.
rq1_component_regression <- function(g, within = g$f_W,
                                     deviation = g$inference_deviation, x_scale = NULL) {
  x <- within; y <- log1p(deviation)
  if (is.null(x_scale)) x_scale <- stats::sd(x)
  empty <- list(beta = NA_real_, delta_r2 = NA_real_, x_scale = x_scale)
  if (nrow(g) < 10L || !is.finite(x_scale) || x_scale < 1e-12) return(empty)
  if (any(!is.finite(c(x,y,g$D_T,g$rq1_distortion_A)))) return(empty)
  dat <- data.frame(A = log1p(g$rq1_distortion_A), total = log1p(g$D_T),
    contrast = factor(g$candidate_config), outcome = factor(g$outcome))
  terms <- c("A", "total", if (nlevels(dat$contrast) > 1L) "contrast",
             if (nlevels(dat$outcome) > 1L) "outcome")
  Z <- stats::model.matrix(stats::reformulate(terms), dat)
  if (nrow(g) <= ncol(Z) + 2L || any(!is.finite(c(x, y, Z)))) return(empty)
  rx <- stats::lm.fit(Z, x / x_scale)$residuals
  ry <- stats::lm.fit(Z, y)$residuals
  den <- sum(rx^2); tss <- sum((y - mean(y))^2)
  if (den < 1e-12 || tss < 1e-12) return(empty)
  beta <- sum(rx * ry) / den
  list(beta = beta, delta_r2 = (sum(ry^2) - sum((ry - beta * rx)^2)) / tss,
       x_scale = x_scale)
}

rq1_component_link <- function(summary, blocks, B, seed = 20260919L) {
  keys <- c("candidate_config", "metric", "outcome")
  g <- summary |>
    dplyr::filter(status == "estimated", is.finite(inference_deviation),
      is.finite(rq1_distortion_A), is.finite(D_T), is.finite(f_W)) |>
    dplyr::mutate(component_task = dplyr::row_number())
  if (!nrow(g)) return(list(
    summary=summary |> dplyr::distinct(outcome_domain,metric_geometry) |>
      dplyr::mutate(n_tasks=0L,n_metrics=0L,within_share_sd=NA_real_,beta_within=NA_real_,
        beta_lower=NA_real_,beta_upper=NA_real_,incremental_r2=NA_real_,
        incremental_r2_lower=NA_real_,incremental_r2_upper=NA_real_,bootstrap_requested=B,
        bootstrap_valid=0L,status="not_estimable"),
    bootstrap=tibble::tibble(outcome_domain=character(),metric_geometry=character(),
      replicate=integer(),beta_within=double(),incremental_r2=double()),
    correlations=tibble::tibble(outcome_domain=character(),metric_geometry=character(),
      comparison_pair_id=character(),n_tasks=integer(),rho_A=double(),rho_within=double())))
  blocks <- blocks |> dplyr::inner_join(g |> dplyr::select(dplyr::all_of(keys), component_task),
                                       by = keys, relationship = "many-to-one")
  people <- blocks |> dplyr::distinct(participant, site) |> dplyr::arrange(site, participant)
  # One shared participant draw across ALL metrics, contrasts and outcomes.
  # Task-specific supports are retained by assigning zero rows outside that task.
  set.seed(seed)
  W <- matrix(0L, nrow(people), B)
  for (idx in split(seq_len(nrow(people)), people$site)) {
    for (b in seq_len(B)) W[idx,b] <- tabulate(idx[sample.int(length(idx), length(idx), TRUE)],
                                             nbins = nrow(people))[idx]
  }
  within_draw <- total_draw <- deviation_draw <- matrix(NA_real_, nrow(g), B)
  for (z in split(blocks, blocks$component_task)) {
    k <- z$component_task[1]
    w <- W[match(z$participant, people$participant),,drop=FALSE]
    if (!B) next
    denom <- as.vector(z$n_days %*% w)
    total_ss <- as.vector(z$total_ss %*% w)
    total_draw[k,] <- sqrt(total_ss / denom)
    within_draw[k,] <- ifelse(total_ss / denom > 1e-15,
                              as.vector(z$within_ss %*% w) / total_ss, NA_real_)
    sr <- Map(function(xx,xy) list(xx=xx,xy=xy), z$reference_xx, z$reference_xy)
    sc <- Map(function(xx,xy) list(xx=xx,xy=xy), z$candidate_xx, z$candidate_xy)
    rb <- rq1_inference_solve_draws(sr,w); cb <- rq1_inference_solve_draws(sc,w)
    # Reference uncertainty and exposure scaling are frozen at the primary fit.
    deviation_draw[k,] <- vapply(seq_len(B), function(b)
      rq1_inference_quadnorm(cb[,b]-rb[,b], z$reference_covariance[[1]]), numeric(1))
  }
  models <- list(); draws <- list(); i <- 0L
  for (ix in split(seq_len(nrow(g)), interaction(g$outcome_domain, g$metric_geometry, drop=TRUE))) {
    i <- i+1L; z <- g[ix,]
    point <- rq1_component_regression(z)
    boot <- lapply(seq_len(B), function(b) {
      # Do not change the target task set when a sparse resample is singular.
      zb <- z; zb$D_T <- total_draw[ix,b]
      rq1_component_regression(zb, within_draw[ix,b], deviation_draw[ix,b], point$x_scale)
    })
    beta <- vapply(boot, `[[`, numeric(1), "beta")
    delta <- vapply(boot, `[[`, numeric(1), "delta_r2")
    ok <- is.finite(beta) & is.finite(delta)
    reliable <- sum(ok) >= max(20L, ceiling(.8*B))
    ci <- function(x) if (reliable) as.numeric(stats::quantile(x[ok],c(.025,.975))) else c(NA_real_,NA_real_)
    bc <- ci(beta); dc <- ci(delta)
    models[[i]] <- tibble::tibble(outcome_domain=z$outcome_domain[1], metric_geometry=z$metric_geometry[1],
      n_tasks=nrow(z), n_metrics=dplyr::n_distinct(z$metric),
      within_share_sd=point$x_scale, beta_within=point$beta, beta_lower=bc[1], beta_upper=bc[2],
      incremental_r2=point$delta_r2, incremental_r2_lower=dc[1], incremental_r2_upper=dc[2],
      bootstrap_requested=B, bootstrap_valid=sum(ok),
      status=if (!is.finite(point$beta)) "not_estimable" else if (reliable) "estimated" else "bootstrap_unreliable")
    draws[[i]] <- tibble::tibble(outcome_domain=z$outcome_domain[1], metric_geometry=z$metric_geometry[1],
      replicate=seq_len(B), beta_within=beta, incremental_r2=delta)
  }
  correlations <- g |> dplyr::group_by(outcome_domain, metric_geometry, comparison_pair_id) |>
    dplyr::summarise(n_tasks=dplyr::n(),
      rho_A=suppressWarnings(stats::cor(rq1_distortion_A,inference_deviation,method="spearman")),
      rho_within=suppressWarnings(stats::cor(distortion_within_rms,inference_deviation,method="spearman")),
      .groups="drop")
  list(summary=dplyr::bind_rows(models), bootstrap=dplyr::bind_rows(draws), correlations=correlations)
}

rq1_inference_fit <- function(g, B = 1000L, seed = 20260911L) {
  circular <- identical(g$metric_geometry[[1]], "circular_time")
  terms <- if (circular) c("sin_time", "cos_time") else "reference_SD"
  good <- g |>
    dplyr::filter(is.na(pair_reason), is.na(outcome_reason), is.finite(outcome_value)) |>
    dplyr::mutate(participant = paste(site, Id, sep = "|")) |>
    dplyr::group_by(participant) |> dplyr::filter(dplyr::n() >= 2L) |> dplyr::ungroup() |>
    dplyr::arrange(participant, Date)
  base <- tibble::tibble(
    term = terms, n_reference_days = nrow(g), n_matched_days = nrow(good),
    n_unavailable_pairs = sum(!is.na(g$pair_reason)),
    n_missing_outcomes = sum(is.na(g$pair_reason) & !is.na(g$outcome_reason)),
    n_participants = dplyr::n_distinct(good$participant),
    reference_beta = NA_real_, candidate_beta = NA_real_, beta_difference = NA_real_,
    reference_lower = NA_real_, reference_upper = NA_real_,
    candidate_lower = NA_real_, candidate_upper = NA_real_,
    difference_lower = NA_real_, difference_upper = NA_real_,
    reference_scale = NA_real_, distortion_A = NA_real_, distortion_B = NA_real_,
    bootstrap_requested = B, bootstrap_valid = 0L, status = "insufficient_repeated_support"
  )
  support <- good |> dplyr::select(site, Id, Date)
  components <- tibble::tibble(distortion_total_ms=NA_real_, distortion_between_ms=NA_real_,
    distortion_within_ms=NA_real_, D_T=NA_real_, f_W=NA_real_,
    distortion_within_rms=NA_real_, distortion_within_fraction=NA_real_)
  component_blocks <- tibble::tibble()
  finish <- function(summary, draws = tibble::tibble(),
                     reference_strength = NA_real_, candidate_strength = NA_real_,
                     inference_deviation = NA_real_, covariance_rcond = NA_real_,
                     bootstrap_valid_joint = 0L) {
    task <- tibble::tibble(
      basis = if (circular) "circular_sin_cos" else "linear_reference_SD",
      n_reference_days = nrow(g), n_matched_days = nrow(good),
      n_unavailable_pairs = sum(!is.na(g$pair_reason)),
      n_missing_outcomes = sum(is.na(g$pair_reason) & !is.na(g$outcome_reason)),
      n_participants = dplyr::n_distinct(good$participant),
      matched_support_distortion_A = summary$distortion_A[[1]],
      matched_support_distortion_B = summary$distortion_B[[1]],
      reference_association_strength = reference_strength,
      candidate_association_strength = candidate_strength,
      inference_deviation = inference_deviation,
      reference_covariance_rcond = covariance_rcond,
      bootstrap_requested = B, bootstrap_valid_joint = bootstrap_valid_joint,
      status = paste(sort(unique(summary$status)), collapse = ";")
    )
    list(summary = summary, task_summary = dplyr::bind_cols(task, components),
         support = support, bootstrap = draws, component_blocks = component_blocks)
  }
  if (all(!is.na(g$pair_reason))) {
    base$status <- "measurement_unavailable"; return(finish(base))
  }
  if (!any(is.na(g$pair_reason) & is.na(g$outcome_reason) & is.finite(g$outcome_value))) {
    base$status <- "outcome_support_unavailable"; return(finish(base))
  }
  if (base$n_participants[[1]] < 4L) return(finish(base))

  r <- good$reference_value
  candidate <- good$candidate_value
  if (circular) {
    theta <- 2 * pi * r / 86400
    origin <- atan2(mean(sin(theta)), mean(cos(theta))) * 86400 / (2 * pi)
    scale <- stats::sd(((r - origin + 43200) %% 86400) - 43200)
    delta <- ((candidate - r + 43200) %% 86400) - 43200
  } else {
    scale <- stats::sd(r)
    delta <- candidate - r
  }
  if (!is.finite(scale) || scale <= sqrt(.Machine$double.eps)) {
    base$status <- "reference_scale_unavailable"; return(finish(base))
  }
  if (circular) {
    xr <- cbind(sin(theta), cos(theta))
    xc <- cbind(sin(2 * pi * candidate / 86400), cos(2 * pi * candidate / 86400))
  } else {
    xr <- matrix((r - mean(r)) / scale, ncol = 1L)
    xc <- matrix((candidate - mean(r)) / scale, ncol = 1L)
  }
  base$reference_scale <- scale
  base$distortion_A <- mean(abs(delta / scale))
  base$distortion_B <- mean(delta / scale)
  decomposition <- rq1_distortion_components(xr,xc,good$participant,good$site)
  components <- decomposition$summary
  y <- good$outcome_value
  if (sum((y - ave(y, good$participant))^2) < 1e-12) {
    base$status <- "no_within_participant_outcome_variation"; return(finish(base))
  }
  sr <- rq1_inference_stats(xr, y, good$participant)
  sc <- rq1_inference_stats(xc, y, good$participant)
  component_blocks <- decomposition$blocks
  block_order <- match(component_blocks$participant, names(sr))
  component_blocks$reference_xx <- lapply(sr[block_order], `[[`, "xx")
  component_blocks$reference_xy <- lapply(sr[block_order], `[[`, "xy")
  component_blocks$candidate_xx <- lapply(sc[block_order], `[[`, "xx")
  component_blocks$candidate_xy <- lapply(sc[block_order], `[[`, "xy")
  component_blocks$reference_covariance <- rep(list(matrix(NA_real_,length(terms),length(terms))),nrow(component_blocks))
  weights <- rep(1L, length(sr))
  br <- rq1_inference_solve(sr, weights)
  bc <- rq1_inference_solve(sc, weights)
  if (any(!is.finite(c(br, bc)))) {
    base$status <- "singular_within_participant_exposure"; return(finish(base))
  }
  base$reference_beta <- br
  base$candidate_beta <- bc
  base$beta_difference <- bc - br
  base$status <- if (B > 0L) "estimated" else "estimated_without_bootstrap"

  cluster_site <- good$site[match(names(sr), good$participant)]
  strata <- split(seq_along(sr), cluster_site)
  set.seed(seed)
  draws <- tibble::tibble()
  reference_strength <- candidate_strength <- inference_deviation <- covariance_rcond <- NA_real_
  bootstrap_valid_joint <- 0L
  if (B > 0L) {
    weights <- matrix(vapply(seq_len(B), function(b) {
      sampled <- unlist(lapply(strata, function(idx) idx[sample.int(length(idx), length(idx), replace = TRUE)]))
      tabulate(sampled, nbins = length(sr))
    }, integer(length(sr))), nrow = length(sr))
    rb_matrix <- rq1_inference_solve_draws(sr, weights)
    cb_matrix <- rq1_inference_solve_draws(sc, weights)
    rb <- as.vector(rb_matrix)
    cb <- as.vector(cb_matrix)
    draws <- tibble::tibble(
      replicate = rep(seq_len(B), each = length(terms)), term = rep(terms, B),
      reference_beta = rb, candidate_beta = cb, beta_difference = cb - rb
    )
    for (j in seq_along(terms)) {
      d <- draws |> dplyr::filter(term == terms[[j]], is.finite(reference_beta), is.finite(candidate_beta))
      base$bootstrap_valid[[j]] <- nrow(d)
      if (nrow(d) < max(20L, ceiling(.8 * B))) {
        base$status[[j]] <- "bootstrap_unreliable"; next
      }
      for (field in c("reference", "candidate", "difference")) {
        col <- switch(field, reference = "reference_beta", candidate = "candidate_beta", difference = "beta_difference")
        ci <- stats::quantile(d[[col]], c(.025, .975), names = FALSE)
        base[[paste0(field, "_lower")]][[j]] <- ci[[1]]
        base[[paste0(field, "_upper")]][[j]] <- ci[[2]]
      }
    }
    valid_joint <- apply(is.finite(rb_matrix), 2L, all) & apply(is.finite(cb_matrix), 2L, all)
    bootstrap_valid_joint <- sum(valid_joint)
    if (bootstrap_valid_joint >= max(20L, ceiling(.8 * B))) {
      ref_draws <- t(rb_matrix[, valid_joint, drop = FALSE])
      covariance <- if (length(terms) == 1L) {
        matrix(stats::var(ref_draws[, 1]), nrow = 1L, ncol = 1L)
      } else {
        stats::cov(ref_draws)
      }
      covariance_rcond <- if (all(is.finite(covariance))) rcond(covariance) else NA_real_
      reference_strength <- rq1_inference_quadnorm(br, covariance)
      candidate_strength <- rq1_inference_quadnorm(bc, covariance)
      inference_deviation <- rq1_inference_quadnorm(bc - br, covariance)
      component_blocks$reference_covariance <- rep(list(covariance),nrow(component_blocks))
    }
  }
  finish(base, draws, reference_strength, candidate_strength, inference_deviation,
         covariance_rcond, bootstrap_valid_joint)
}
