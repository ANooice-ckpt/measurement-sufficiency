# Lightweight descriptions of frozen, target-specific joint-pair summaries.
# A common reference CONFIGURATION is not proof of common parent-window rows.
# These loss-record envelopes are separate from the epsilon-conditioned RQ3
# sufficient-set Pareto analysis. No value, window, model or R_obs is rebuilt.

rq3_cr_version <- function() "rq3_common_reference_v1_summary_loss_records_is2d"

rq3_cr_key <- function(x, fields) {
  if (anyNA(x[fields])) stop("Missing scientific key in common-reference input")
  do.call(paste, c(x[fields], sep = "\r"))
}

rq3_cr_require <- function(x, fields, name) {
  absent <- setdiff(fields, names(x))
  if (length(absent)) stop(name, " missing: ", paste(absent, collapse = ", "))
}

rq3_cr_versions <- function(pairs, states) {
  fields <- c("core_artifact_version", "rq1_analysis_version", "rq3_analysis_version")
  rq3_cr_require(pairs, fields, "pair summary")
  rq3_cr_require(states, fields, "state summary")
  out <- lapply(fields, function(f) {
    p <- unique(pairs[[f]]); s <- unique(states[[f]])
    if (length(p) != 1L || length(s) != 1L || anyNA(c(p, s)) ||
        !nzchar(p) || !identical(p, s)) stop("Inconsistent upstream version: ", f)
    p
  })
  names(out) <- fields
  out
}

rq3_cr_groups <- function(x) {
  split(seq_len(nrow(x)), rq3_cr_key(x, c("support_id", "placement", "optical", "metric")))
}

rq3_cr_projection <- function(pairs, states, windows = NULL,
                              resolutions = ms_primary_temporal_s(),
                              durations = ms_primary_duration_days(),
                              expected_metrics = 54L) {
  pf <- c("support_id", "placement", "optical", "resolution_a", "n_days_a",
          "config_a_id", "resolution_b", "n_days_b", "config_b_id", "metric",
          "metric_class", "metric_geometry", "A", "B", "n_units", "n_participants")
  sf <- c("support_id", "placement", "optical", "resolution_s", "n_days", "metric",
          "metric_class", "metric_geometry", "config_id", "R_obs", "status")
  rq3_cr_require(pairs, pf, "pair summary")
  rq3_cr_require(states, sf, "state summary")
  versions <- rq3_cr_versions(pairs, states)
  pair_key <- c("support_id", "placement", "optical", "metric", "config_a_id", "config_b_id")
  state_key <- c("support_id", "placement", "optical", "metric", "config_id")
  if (anyDuplicated(rq3_cr_key(pairs, pair_key))) stop("Duplicate frozen pair key")
  if (anyDuplicated(rq3_cr_key(states, state_key))) stop("Duplicate frozen state key")
  if (any(!is.finite(pairs$A) | !is.finite(pairs$B) | pairs$A < 0 |
          pairs$A + 1e-10 < abs(pairs$B))) stop("Invalid A/B in frozen pair summary")
  if (any(!is.finite(pairs$n_units) | pairs$n_units < 1 |
          !is.finite(pairs$n_participants) | pairs$n_participants < 1 |
          pairs$n_participants > pairs$n_units)) stop("Invalid frozen support counts")
  if (any(!pairs$resolution_a %in% resolutions | !pairs$resolution_b %in% resolutions |
          !pairs$n_days_a %in% durations | !pairs$n_days_b %in% durations) ||
      any(!states$resolution_s %in% resolutions | !states$n_days %in% durations)) {
    stop("Input contains states outside the primary design")
  }
  if (any(pairs$resolution_b > pairs$resolution_a | pairs$n_days_b < pairs$n_days_a |
          (pairs$resolution_b == pairs$resolution_a & pairs$n_days_b == pairs$n_days_a))) {
    stop("Frozen pair is not a strict higher-state comparison")
  }
  if (any(pairs$config_a_id != paste0("r", pairs$resolution_a, "__d", pairs$n_days_a)) ||
      any(pairs$config_b_id != paste0("r", pairs$resolution_b, "__d", pairs$n_days_b)) ||
      any(states$config_id != paste0("r", states$resolution_s, "__d", states$n_days))) {
    stop("Configuration identity does not match coordinates")
  }
  # Reject reserve / duplicate narrower supports instead of silently pooling them.
  for (x in list(pairs, states)) {
    base <- unname(c(eye = "eye", chest = "eye_chest", wrist = "eye_wrist")[x$placement])
    expected <- paste0(base, ifelse(x$optical == "LIGHT" | x$metric %in% c("MDER", "nvRD"), "_full", "_medi"))
    if (anyNA(base) || any(!x$optical %in% c("MEDI", "LIGHT")) ||
        any(x$support_id != expected)) stop("Input contains non-maximal configuration support")
  }
  catalog <- unique(rbind(pairs[c("metric", "metric_class", "metric_geometry")],
                          states[c("metric", "metric_class", "metric_geometry")]))
  if (anyDuplicated(catalog$metric)) stop("Conflicting metric metadata")
  if (nrow(catalog) != expected_metrics) stop("Incomplete published target inventory")
  facets <- unique(rbind(pairs[c("placement", "optical")], states[c("placement", "optical")]))
  lattice <- expand.grid(resolution_s = resolutions, n_days = durations,
                         KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
  out <- merge(merge(facets, catalog, by = NULL), lattice, by = NULL)
  base <- unname(c(eye = "eye", chest = "eye_chest", wrist = "eye_wrist")[out$placement])
  out$support_id <- paste0(base, ifelse(out$optical == "LIGHT" | out$metric %in% c("MDER", "nvRD"), "_full", "_medi"))
  out$config_id <- paste0("r", out$resolution_s, "__d", out$n_days)
  ref_resolution <- min(resolutions); ref_days <- max(durations)
  ref_id <- paste0("r", ref_resolution, "__d", ref_days)
  out$reference_config_id <- ref_id
  out$reference_resolution_s <- ref_resolution
  out$reference_n_days <- ref_days
  out$nominal_records <- 86400 * out$n_days / out$resolution_s

  ref_pairs <- pairs[pairs$resolution_b == ref_resolution & pairs$n_days_b == ref_days, ]
  names(ref_pairs)[names(ref_pairs) == "config_a_id"] <- "config_id"
  idx <- match(rq3_cr_key(out, state_key), rq3_cr_key(ref_pairs, state_key))
  state_idx <- match(rq3_cr_key(out, state_key), rq3_cr_key(states, state_key))
  reference_keys <- out[state_key]; reference_keys$config_id <- ref_id
  reference_idx <- match(rq3_cr_key(reference_keys, state_key), rq3_cr_key(states, state_key))
  reference_available <- !is.na(reference_idx) &
    states$status[reference_idx] %in% c("resolved", "boundary_unresolved")
  candidate_available <- !is.na(state_idx) &
    states$status[state_idx] %in% c("resolved", "boundary_unresolved")
  is_self <- out$config_id == ref_id
  out$D_reference <- ref_pairs$A[idx]
  out$B_reference_minus_candidate <- ref_pairs$B[idx]
  out$n_pair_units <- ref_pairs$n_units[idx]
  out$n_pair_participants <- ref_pairs$n_participants[idx]
  out$status <- ifelse(!is.na(idx), "observed_reference_pair", "unavailable_reference_pair")
  out$status[!candidate_available] <- "unavailable_candidate_state"
  out$status[!reference_available] <- "unavailable_reference_state"
  self_ok <- is_self & reference_available
  out$D_reference[self_ok] <- 0
  out$B_reference_minus_candidate[self_ok] <- 0
  out$status[self_ok] <- "reference_self_identity"
  out$status[out$optical == "LIGHT" & out$metric %in% c("MDER", "nvRD")] <- "unavailable_light_only_metric"
  out$status[out$metric == "interdaily_stability" & out$n_days < 2L] <- "unavailable_is_requires_two_days"
  out$available <- out$status %in% c("observed_reference_pair", "reference_self_identity")
  if (any(out$available & !is.finite(out$D_reference))) stop("Available reference comparison has nonfinite D")
  out$D_reference[!out$available] <- NA_real_
  out$B_reference_minus_candidate[!out$available] <- NA_real_
  # Counts are retained only for accepted comparisons, not for invalid single-day IS.
  out$n_pair_units[!out$available] <- NA_real_
  out$n_pair_participants[!out$available] <- NA_real_
  out$source_state_status <- states$status[state_idx]
  out$source_R_obs <- states$R_obs[state_idx]
  out$source_R_obs_usable <- !is.na(state_idx) & out$available
  out$source_R_obs_usable[is_self] <- FALSE
  # Never expose an invalid single-day IS as an active remaining-response value.
  out$source_R_obs[!out$source_R_obs_usable] <- NA_real_
  if (any(out$available & is.finite(out$source_R_obs) &
          out$D_reference > out$source_R_obs + 1e-10)) {
    stop("Reference-pair D exceeds its frozen all-higher R_obs; inputs disagree")
  }
  out$comparison_basis <- "frozen_pair_specific_nested_windows"
  out$reference_identity_verified_across_candidates <- FALSE
  out$reference_weights_verified_across_candidates <- FALSE
  out$standardizer_basis <- "support_specific_eye_MEDI_10s_6d_SD"
  out$reference_scope <- "same_placement_optical_facet"
  out <- out[do.call(order, out[c("placement", "optical", "metric", "nominal_records", "n_days", "resolution_s")]), ]
  rownames(out) <- NULL

  # Same-budget minima and loss-record non-dominance describe the exported
  # summaries only. Cross-candidate sample / reference-weight control is unknown.
  out$same_budget_n_candidates <- out$same_budget_n_available <- NA_integer_
  out$same_budget_complete <- NA
  out$same_budget_min_D <- NA_real_
  out$is_summary_same_budget_min <- NA
  out$is_summary_loss_record_nondominated <- NA
  envelopes <- list(); marginal <- list(); e <- m <- 0L
  id_fields <- c("support_id", "placement", "optical", "metric", "metric_class", "metric_geometry")
  for (ids in rq3_cr_groups(out)) {
    x <- out[ids, ]; usable <- which(x$available & is.finite(x$D_reference))
    budgets <- sort(unique(x$nominal_records))
    for (j in seq_along(budgets)) {
      b <- budgets[j]; same <- which(x$nominal_records == b)
      same_good <- intersect(same, usable)
      minimum <- if (length(same_good)) min(x$D_reference[same_good]) else NA_real_
      oi <- ids[same]
      out$same_budget_n_candidates[oi] <- length(same)
      out$same_budget_n_available[oi] <- length(same_good)
      out$same_budget_complete[oi] <- length(same_good) == length(same)
      out$same_budget_min_D[oi] <- minimum
      if (length(same_good)) out$is_summary_same_budget_min[ids[same_good]] <- x$D_reference[same_good] == minimum
      under <- which(x$nominal_records <= b); good <- intersect(under, usable)
      best <- if (length(good)) min(x$D_reference[good]) else NA_real_
      winners <- good[x$D_reference[good] == best]
      # Keep all ties at the cheapest attaining cost; expensive equal-loss rows
      # are dominated in the explicitly stated (nominal records, summary D) plane.
      if (length(winners)) winners <- winners[x$nominal_records[winners] == min(x$nominal_records[winners])]
      if (!length(winners)) winners <- NA_integer_
      for (w in winners) {
        e <- e + 1L
        z <- x[1L, id_fields, drop = FALSE]
        z$budget_from_records <- b
        z$budget_until_records_exclusive <- if (j < length(budgets)) budgets[j + 1L] else NA_real_
        z$maximum_evaluated_records <- max(budgets)
        z$n_candidates_within_budget <- length(under)
        z$n_available_within_budget <- length(good)
        z$candidate_set_complete <- length(under) == length(good)
        z$minimum_summary_D <- best
        z$attaining_config_id <- x$config_id[w]
        z$attaining_records <- x$nominal_records[w]
        z$attaining_n_pair_units <- x$n_pair_units[w]
        z$attaining_n_pair_participants <- x$n_pair_participants[w]
        z$status <- if (length(good)) "descriptive_available_candidate_envelope" else "unavailable_no_candidate_within_budget"
        z$controlled_frontier_established <- FALSE
        envelopes[[e]] <- z
      }
    }
    for (i in usable) {
      dominates <- x$nominal_records[usable] <= x$nominal_records[i] &
        x$D_reference[usable] <= x$D_reference[i] &
        (x$nominal_records[usable] < x$nominal_records[i] | x$D_reference[usable] < x$D_reference[i])
      out$is_summary_loss_record_nondominated[ids[i]] <- !any(dominates)
    }
    for (i in seq_len(nrow(x))) {
      higher <- which((x$n_days == x$n_days[i] &
                         x$resolution_s == if (x$resolution_s[i] > min(resolutions)) max(resolutions[resolutions < x$resolution_s[i]]) else -1) |
                        (x$resolution_s == x$resolution_s[i] &
                           x$n_days == if (x$n_days[i] < max(durations)) min(durations[durations > x$n_days[i]]) else -1))
      for (h in higher) {
        m <- m + 1L
        z <- x[i, id_fields, drop = FALSE]
        z$config_from <- x$config_id[i]; z$config_to <- x$config_id[h]
        z$axis <- if (x$n_days[i] == x$n_days[h]) "temporal" else "duration"
        z$records_from <- x$nominal_records[i]; z$records_to <- x$nominal_records[h]
        z$additional_nominal_records <- z$records_to - z$records_from
        z$nominal_record_ratio <- z$records_to / z$records_from
        z$D_from <- x$D_reference[i]; z$D_to <- x$D_reference[h]
        z$summary_D_reduction <- z$D_from - z$D_to
        z$status_from <- x$status[i]; z$status_to <- x$status[h]
        z$n_pair_units_from <- x$n_pair_units[i]; z$n_pair_units_to <- x$n_pair_units[h]
        z$n_pair_participants_from <- x$n_pair_participants[i]
        z$n_pair_participants_to <- x$n_pair_participants[h]
        z$matched_reference_window_gain_established <- FALSE
        marginal[[m]] <- z
      }
    }
  }

  support_audit <- out[c(id_fields, "config_id", "n_days", "resolution_s", "status", "n_pair_units", "n_pair_participants")]
  support_audit$potential_reference_windows <- support_audit$potential_reference_participants <- NA_integer_
  if (!is.null(windows)) {
    rq3_cr_require(windows, c("support_id", "site", "Id", "n_days", "window_id"), "window manifest")
    ref_windows <- windows[windows$n_days == ref_days, ]
    if (anyDuplicated(ref_windows$window_id)) stop("Duplicate reference window in manifest")
    for (support in unique(support_audit$support_id)) {
      wi <- ref_windows[ref_windows$support_id == support, ]
      si <- support_audit$support_id == support
      support_audit$potential_reference_windows[si] <- nrow(wi)
      support_audit$potential_reference_participants[si] <- length(unique(paste(wi$site, wi$Id, sep = "|")))
    }
  }
  support_audit$potential_subwindows_per_reference <- ref_days - support_audit$n_days + 1L
  support_audit$potential_pair_units <- support_audit$potential_reference_windows * support_audit$potential_subwindows_per_reference
  support_audit$pair_count_equals_potential <- with(support_audit, n_pair_units == potential_pair_units)
  support_audit$complete_parent_count_if_uniform <- with(support_audit, n_pair_units / potential_subwindows_per_reference)
  support_audit$count_audit_status <- ifelse(is.na(support_audit$n_pair_units), "no_accepted_pair_count",
    ifelse(is.na(support_audit$potential_pair_units), "manifest_not_supplied",
      ifelse(support_audit$pair_count_equals_potential, "counts_match_potential_not_identity_proof",
        ifelse(support_audit$n_pair_units < support_audit$potential_pair_units, "counts_below_potential_metric_or_source_availability", "counts_exceed_manifest_potential_check_provenance"))))
  support_audit$reference_identity_verified <- FALSE
  support_audit$reference_weight_equality_verified <- FALSE

  same_budget <- out[out$same_budget_n_candidates > 1L, ]
  nondominated <- out[!is.na(out$is_summary_loss_record_nondominated) & out$is_summary_loss_record_nondominated, ]
  tables <- list(target_differences = out, same_budget_choices = same_budget,
                 summary_loss_record_nondominated = nondominated,
                 summary_budget_envelope = do.call(rbind, envelopes),
                 adjacent_summary_changes = do.call(rbind, marginal),
                 reference_support_count_audit = support_audit)
  for (name in names(tables)) {
    tables[[name]]$common_reference_version <- rq3_cr_version()
    for (field in names(versions)) tables[[name]][[field]] <- versions[[field]]
    rownames(tables[[name]]) <- NULL
  }
  list(tables = tables, upstream_versions = versions,
       common_reference_version = rq3_cr_version(),
       reference = list(resolution_s = ref_resolution, n_days = ref_days,
                        scope = "within_each_support_placement_optical_metric"))
}
