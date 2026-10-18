# Synthetic estimand/availability checks; no raw or frozen data required.
source("scripts/utils/analysis_design.R")
source("scripts/utils/rq3_common_reference.R")

states <- expand.grid(placement = "eye", optical = c("MEDI", "LIGHT"),
                     resolution_s = c(10L, 20L, 40L), n_days = c(1L, 2L, 6L),
                     metric = c("mean_MEDI", "interdaily_stability", "MDER"),
                     KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
states$support_id <- ifelse(states$optical == "LIGHT" | states$metric == "MDER", "eye_full", "eye_medi")
states$metric_class <- ifelse(states$metric == "interdaily_stability", "temporal dynamics", "test")
states$metric_geometry <- "linear"
states$config_id <- paste0("r", states$resolution_s, "__d", states$n_days)
states$R_obs <- .9
states$status <- "resolved"
at_reference <- states$config_id == "r10__d6"
states$R_obs[at_reference] <- NA_real_
states$status[at_reference] <- "boundary_unresolved"
states <- states[!(states$optical == "LIGHT" & states$metric == "MDER"), ]
states$core_artifact_version <- "synthetic_core"
states$rq1_analysis_version <- "synthetic_rq1"
states$rq3_analysis_version <- "synthetic_rq3"
pairs <- states[!is.na(states$R_obs), ]
names(pairs)[names(pairs) == "resolution_s"] <- "resolution_a"
names(pairs)[names(pairs) == "n_days"] <- "n_days_a"
names(pairs)[names(pairs) == "config_id"] <- "config_a_id"
pairs$resolution_b <- 10L; pairs$n_days_b <- 6L; pairs$config_b_id <- "r10__d6"
pairs$A <- .5; pairs$B <- 0; pairs$n_units <- 60L; pairs$n_participants <- 10L
# A larger recording count may have worse loss. The budget envelope must not
# turn that increase into a negative-cost benefit, and marginal reduction may be negative.
pairs$A[pairs$config_a_id == "r40__d2"] <- .2
pairs$A[pairs$config_a_id == "r20__d2"] <- .7
# Same nominal count: 40s x 2d and 20s x 1d both have 4320 records.
pairs$A[pairs$config_a_id == "r20__d1"] <- .4

project <- function(p = pairs, s = states) {
  rq3_cr_projection(p, s, resolutions = c(10L, 20L, 40L), durations = c(1L, 2L, 6L), expected_metrics = 3L)
}
result <- project(); x <- result$tables$target_differences
stopifnot(nrow(x) == 54L, !anyDuplicated(rq3_cr_key(x, c("support_id", "placement", "optical", "metric", "config_id"))))
is1 <- x$metric == "interdaily_stability" & x$n_days == 1L
optical_missing <- x$metric == "MDER" & x$optical == "LIGHT"
stopifnot(sum(is1) == 6L, all(x$status[is1] == "unavailable_is_requires_two_days"),
          all(is.na(x$D_reference[is1])), all(is.na(x$is_summary_loss_record_nondominated[is1])),
          all(is.na(x$D_reference[optical_missing])),
          all(is.na(x$is_summary_same_budget_min[optical_missing])))
self <- x$status == "reference_self_identity"
stopifnot(all(x$D_reference[self] == 0), all(is.na(x$source_R_obs[self])),
          all(is.na(x$n_pair_units[self])),
          all(x$source_R_obs[x$available & !self] == .9))
same <- x[x$optical == "MEDI" & x$metric == "mean_MEDI" & x$nominal_records == 4320, ]
stopifnot(nrow(same) == 2L, same$is_summary_same_budget_min[same$config_id == "r40__d2"],
          !same$is_summary_same_budget_min[same$config_id == "r20__d1"])
env <- result$tables$summary_budget_envelope
env <- env[env$optical == "MEDI" & env$metric == "mean_MEDI", ]
stopifnot(all(diff(env$minimum_summary_D) <= 0),
          all(!env$controlled_frontier_established))
m <- result$tables$adjacent_summary_changes
row <- m[m$optical == "MEDI" & m$metric == "mean_MEDI" & m$config_from == "r40__d2" & m$config_to == "r20__d2", ]
stopifnot(nrow(row) == 1L, abs(row$summary_D_reduction + .5) < 1e-12,
          row$nominal_record_ratio == 2, !row$matched_reference_window_gain_established)
# An absent reference must never be manufactured as a zero-loss state.
# Use coordinates explicitly because row names are not scientific identities.
missing_ref <- states[!(states$metric == "mean_MEDI" & states$config_id == "r10__d6"), ]
z <- project(s = missing_ref)$tables$target_differences
stopifnot(all(is.na(z$D_reference[z$metric == "mean_MEDI"])))
missing_candidate <- states[!(states$metric == "mean_MEDI" & states$config_id == "r40__d2"), ]
z <- project(s = missing_candidate)$tables$target_differences
absent <- z$metric == "mean_MEDI" & z$config_id == "r40__d2"
stopifnot(all(z$status[absent] == "unavailable_candidate_state"), all(is.na(z$D_reference[absent])))
# Losing one pair preserves an unavailable row and exposes incomplete choice sets.
p <- pairs[!(pairs$metric == "mean_MEDI" & pairs$optical == "MEDI" & pairs$config_a_id == "r40__d2"), ]
z <- project(p = p)$tables$target_differences
lost <- z$metric == "mean_MEDI" & z$optical == "MEDI" & z$config_id == "r40__d2"
stopifnot(sum(lost) == 1L, !z$available[lost], is.na(z$D_reference[lost]), !z$same_budget_complete[lost])
expect_error <- function(expr) stopifnot(inherits(try(force(expr), silent = TRUE), "try-error"))
expect_error(project(p = rbind(pairs, pairs[1L, ])))
bad <- states; bad$rq3_analysis_version[1L] <- "other_version"
expect_error(project(s = bad))
bad <- pairs; bad$A[1L] <- -.1
expect_error(project(p = bad))
bad <- pairs; bad$support_id[1L] <- "eye_chest_wrist_full"
expect_error(project(p = bad))
cat("Common-reference tests passed: availability, missing references, budget ties, non-monotonic losses, version and key guards.\n")
