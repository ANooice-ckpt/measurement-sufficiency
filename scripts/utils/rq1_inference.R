# Downstream association preservation; no light-series or metric operators.
# Outcomes are linear within-person projections in their native diary units.
source("scripts/utils/analysis_design.R")
source("scripts/utils/artifact_validation.R")

rq1_inference_version <- function(rq1_version) {
  paste0("rq1_inference_v1__", rq1_version)
}

rq1_sleep_outcomes <- function(x, site) {
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
    diary_timezone = tz,
    valid_interval = !is.na(x$sleepprep) & !is.na(x$wake) & x$wake > x$sleepprep,
    sleep_quality = as.numeric(match(quality, quality_levels)),
    awakenings = as.numeric(x$awakenings), awake_duration = awake
  )
  ms_assert_unique(out, c("site", "Id", "Date"), paste(site, "sleepdiary"))
  out |>
    tidyr::pivot_longer(c(sleep_quality, awakenings, awake_duration),
                        names_to = "outcome", values_to = "outcome_value") |>
    dplyr::mutate(
      outcome_reason = dplyr::case_when(
        !valid_interval ~ "invalid_sleep_interval",
        !is.finite(outcome_value) ~ "missing_outcome",
        outcome != "sleep_quality" & outcome_value < 0 ~ "negative_outcome",
        outcome == "awakenings" & outcome_value != floor(outcome_value) ~ "noninteger_count",
        TRUE ~ NA_character_
      ),
      outcome_value = dplyr::if_else(is.na(outcome_reason), outcome_value, NA_real_)
    )
}

rq1_inference_pairs <- function(cube) {
  x <- cube |>
    dplyr::filter(analysis_unit_type == "participant_day", resolution_s %in% ms_primary_temporal_s()) |>
    dplyr::mutate(Id = as.character(Id), Date = as.Date(Date))
  keys <- c("support_id", "site", "Id", "Date", "metric")
  ms_assert_unique(x, c(keys, "config_id"), "daily metric cube")
  configs <- tidyr::crossing(placement = c("eye", "chest", "wrist"),
                            optical = c("MEDI", "LIGHT"), resolution_s = ms_primary_temporal_s()) |>
    dplyr::filter(!(placement == "eye" & optical == "MEDI" & resolution_s == 10L))
  dplyr::bind_rows(lapply(seq_len(nrow(configs)), function(i) {
    cfg <- configs[i, ]
    prefix <- if (cfg$placement == "eye") "eye" else paste0("eye_", cfg$placement)
    z <- x |> dplyr::filter(
      support_id == paste0(prefix, dplyr::if_else(optical == "LIGHT" |
        metric %in% c("MDER", "nvRD") | cfg$optical == "LIGHT", "_full", "_medi"))
    )
    ref <- z |> dplyr::filter(placement == "eye", optical == "MEDI", resolution_s == 10L) |>
      dplyr::select(dplyr::all_of(keys), metric_class, metric_geometry,
                    reference_value = value, reference_available = available)
    cand <- z |> dplyr::filter(placement == cfg$placement, optical == cfg$optical,
                               resolution_s == cfg$resolution_s) |>
      dplyr::select(dplyr::all_of(keys), candidate_value = value, candidate_available = available)
    # Reference-day left join retains missing candidates explicitly for audit.
    dplyr::left_join(ref, cand, by = keys, relationship = "one-to-one") |>
      dplyr::mutate(
        candidate_config = paste(cfg$placement, cfg$optical, paste0(cfg$resolution_s, "s"), sep = "__"),
        placement = cfg$placement, optical = cfg$optical, resolution_s = cfg$resolution_s,
        reference_config = "eye__MEDI__10s",
        pair_reason = dplyr::case_when(
          cfg$optical == "LIGHT" & metric %in% c("MDER", "nvRD") ~ "LIGHT_only_metric_unavailable",
          !dplyr::coalesce(reference_available, FALSE) | !is.finite(reference_value) ~ "reference_unavailable",
          !dplyr::coalesce(candidate_available, FALSE) | !is.finite(candidate_value) ~ "candidate_unavailable",
          TRUE ~ NA_character_
        )
      )
  }))
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
  # Aggregate all cluster sufficient statistics once, avoiding a row-level fit
  # and an R list reduction for each of the 1,000 bootstrap replicates.
  xx <- vapply(blocks, function(b) as.vector(b$xx), numeric(p * p)) %*% weights
  xy <- vapply(blocks, function(b) as.vector(b$xy), numeric(p)) %*% weights
  matrix(vapply(seq_len(ncol(weights)), function(i) {
    a <- matrix(xx[, i], nrow = p)
    if (!all(is.finite(a)) || rcond(a) < 1e-10 || max(abs(a)) < 1e-12) return(rep(NA_real_, p))
    as.numeric(solve(a, xy[, i]))
  }, numeric(p)), nrow = p)
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
  finish <- function(summary, draws = tibble::tibble()) list(summary = summary, support = support, bootstrap = draws)
  if (all(!is.na(g$pair_reason))) {
    base$status <- "measurement_unavailable"; return(finish(base))
  }
  if (!any(is.na(g$pair_reason) & is.na(g$outcome_reason) & is.finite(g$outcome_value))) {
    base$status <- "outcome_support_unavailable"; return(finish(base))
  }
  # Four clusters is a numerical minimum, not a universal adequacy threshold.
  if (base$n_participants[[1]] < 4L) return(finish(base))
  r <- good$reference_value; c <- good$candidate_value
  if (circular) {
    theta <- 2 * pi * r / 86400
    origin <- atan2(mean(sin(theta)), mean(cos(theta))) * 86400 / (2 * pi)
    scale <- stats::sd(((r - origin + 43200) %% 86400) - 43200)
    delta <- ((c - r + 43200) %% 86400) - 43200
    xr <- cbind(sin(theta), cos(theta))
    xc <- cbind(sin(2 * pi * c / 86400), cos(2 * pi * c / 86400))
  } else {
    scale <- stats::sd(r); delta <- c - r
    xr <- matrix((r - mean(r)) / scale, ncol = 1L)
    xc <- matrix((c - mean(r)) / scale, ncol = 1L)
  }
  if (!is.finite(scale) || scale <= sqrt(.Machine$double.eps)) {
    base$status <- "reference_scale_unavailable"; return(finish(base))
  }
  base$reference_scale <- scale
  base$distortion_A <- mean(abs(delta / scale)); base$distortion_B <- mean(delta / scale)
  y <- good$outcome_value
  if (sum((y - ave(y, good$participant))^2) < 1e-12) {
    base$status <- "no_within_participant_outcome_variation"; return(finish(base))
  }
  sr <- rq1_inference_stats(xr, y, good$participant)
  sc <- rq1_inference_stats(xc, y, good$participant)
  weights <- rep(1L, length(sr))
  br <- rq1_inference_solve(sr, weights); bc <- rq1_inference_solve(sc, weights)
  if (any(!is.finite(c(br, bc)))) {
    base$status <- "singular_within_participant_exposure"; return(finish(base))
  }
  base$reference_beta <- br; base$candidate_beta <- bc; base$beta_difference <- bc - br
  base$status <- if (B > 0L) "estimated" else "estimated_without_bootstrap"
  cluster_site <- good$site[match(names(sr), good$participant)]
  strata <- split(seq_along(sr), cluster_site)
  set.seed(seed)
  draws <- tibble::tibble()
  if (B > 0L) {
    weights <- matrix(vapply(seq_len(B), function(b) {
      sampled <- unlist(lapply(strata, function(idx) idx[sample.int(length(idx), length(idx), replace = TRUE)]))
      tabulate(sampled, nbins = length(sr))
    }, integer(length(sr))), nrow = length(sr))
    rb <- as.vector(rq1_inference_solve_draws(sr, weights))
    cb <- as.vector(rq1_inference_solve_draws(sc, weights))
    draws <- tibble::tibble(replicate = rep(seq_len(B), each = length(terms)), term = rep(terms, B),
                            reference_beta = rb, candidate_beta = cb, beta_difference = cb - rb)
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
  }
  finish(base, draws)
}
