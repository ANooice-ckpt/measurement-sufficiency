# Bounded static/synthetic verification. Never sources a Core/RQ main analysis.
# Run from repository root: Rscript scripts/tests/validate_engineering.R
suppressPackageStartupMessages(library(tidyverse))
source("scripts/utils/rq1_inference.R")
source("scripts/utils/rq1_pairwise_artifacts.R")
source("scripts/utils/melidos_io.R")
source("scripts/10b_rq1_inferential_preservation.R")

files <- list.files("scripts", pattern = "\\.R$", recursive = TRUE, full.names = TRUE)
invisible(lapply(files, parse))
check_sources <- function(expr) {
  if (is.call(expr) && identical(expr[[1]], as.name("source")) &&
      length(expr) >= 2L && is.character(expr[[2]]) && startsWith(expr[[2]], "scripts/")) {
    stopifnot(file.exists(expr[[2]]))
  }
  if (is.call(expr) || is.expression(expr)) invisible(lapply(as.list(expr), check_sources))
}
invisible(lapply(files, function(path) check_sources(parse(path))))
expect_error <- function(expr) stopifnot(inherits(tryCatch(force(expr), error = identity), "error"))

contract <- rq1_inference_contract()
stopifnot(
  length(contract$outcomes) == 6L,
  identical(unname(unique(contract$outcome_domain)), c("Sleep", "Alertness", "Affect")),
  contract$ema_min_slots == 2L,
  identical(contract$ema_slots_h, c(11, 14, 17, 20))
)

# Sleep: calendar alignment uses the diary's local zone, not host zone/UTC.
diary <- tibble(
  Id = "001",
  wake = as.POSIXct("2025-04-02 01:00:00", tz = "Europe/Berlin"),
  sleepprep = as.POSIXct("2025-04-01 22:00:00", tz = "Europe/Berlin"),
  sleepquality = factor("Good", levels = c("Good", "Poor")),
  awakenings = 2, awake_duration = 12
)
d <- rq1_sleep_outcomes(diary, "test")
stopifnot(
  nrow(d) == 3L,
  all(d$Date == as.Date("2025-04-01")),
  all(d$outcome_domain == "Sleep"),
  d$outcome_value[d$outcome == "sleep_quality"] == 4,
  d$outcome_value[d$outcome == "awake_duration"] == 12
)
expect_error(rq1_sleep_outcomes(bind_rows(diary, diary), "test"))
bad <- diary; bad$sleepquality <- "Unexpected"
expect_error(rq1_sleep_outcomes(bad, "test"))

# EMA: retain the nearest response to each nominal 11/14/17/20 h slot, reject
# out-of-window extras, and aggregate KSS / positive affect / negative affect.
mood_levels <- c("Not at all", "Slightly", "Somewhat", "Moderately", "Quite a bit", "Very much so", "Extremely")
kss_levels <- c(
  "Extremely alert", "Very alert", "Alert", "Rather alert", "Neither alert nor sleepy",
  "Some signs of sleepiness", "Sleepy, but no effort to keep awake",
  "Sleepy, but some effort to keep awake",
  "Very sleepy, great effort to keep awake, fighting sleep",
  "Extremely sleepy, can't keep awake"
)
ema_times <- as.POSIXct(
  c("2025-04-01 10:55:00", "2025-04-01 14:05:00", "2025-04-01 17:10:00", "2025-04-01 20:05:00", "2025-04-01 23:30:00"),
  tz = "Europe/Berlin"
)
ema <- tibble(
  Id = "001", Datetime = ema_times,
  anxious = factor(rep("Slightly", 5), levels = mood_levels),
  elated = factor(rep("Somewhat", 5), levels = mood_levels),
  sad = factor(rep("Not at all", 5), levels = mood_levels),
  angry = factor(rep("Not at all", 5), levels = mood_levels),
  irritable = factor(rep("Slightly", 5), levels = mood_levels),
  energetic = factor(rep("Moderately", 5), levels = mood_levels),
  kss = factor(rep("Rather alert", 5), levels = kss_levels)
)
e <- rq1_ema_daily_outcomes(ema, "test")
stopifnot(
  nrow(e) == 3L,
  all(e$Date == as.Date("2025-04-01")),
  all(e$outcome_n_observations == 4L),
  e$outcome_value[e$outcome == "kss"] == 4,
  e$outcome_value[e$outcome == "positive_affect"] == 2.5,
  e$outcome_value[e$outcome == "negative_affect"] == 0.5,
  e$outcome_domain[e$outcome == "kss"] == "Alertness",
  all(e$outcome_domain[e$outcome != "kss"] == "Affect")
)

# Outcome expansion must keep observed valid rows valid; only absent outcomes
# receive the generic outcome_not_observed reason.
outcome_rows <- bind_rows(d, e)
mini_pair <- tibble(site = "test", Id = "001", Date = as.Date("2025-04-01"), marker = 1L)
expanded <- rq1_expand_outcome_support(mini_pair, outcome_rows)
stopifnot(
  nrow(expanded) == 6L,
  setequal(expanded$outcome, contract$outcomes),
  all(is.na(expanded$outcome_reason)),
  all(expanded$outcome_n_observations > 0L)
)
expanded_missing <- rq1_expand_outcome_support(
  mini_pair,
  outcome_rows |> filter(outcome != "kss")
)
stopifnot(
  expanded_missing$outcome_reason[expanded_missing$outcome == "kss"] == "outcome_not_observed",
  all(is.na(expanded_missing$outcome_reason[expanded_missing$outcome != "kss"]))
)

# Downstream inference is deliberately restricted to eight single-axis
# contrasts against eye/MEDI/10 s; no Cartesian multi-axis grid or reserve state.
anchors <- rq1_inference_anchor_map()
stopifnot(
  nrow(anchors) == 8L,
  n_distinct(anchors$candidate_config) == 8L,
  identical(anchors$contrast_order, 1:8),
  setequal(
    anchors$candidate_config,
    c("chest__MEDI__10s", "wrist__MEDI__10s", "eye__LIGHT__10s",
      "eye__MEDI__20s", "eye__MEDI__30s", "eye__MEDI__40s",
      "eye__MEDI__60s", "eye__MEDI__120s")
  )
)

# Frozen RQ1 pair values, not the Core metric cube, are the inferential input.
# state_a is the lower-information candidate and state_b is the reference.
pairwise <- anchors |>
  tidyr::crossing(metric = c("mean_MEDI", "MDER"), day = 1:3) |>
  mutate(
    support_id = case_when(
      dimension == "placement" & placement == "chest" & metric == "MDER" ~ "eye_chest_full",
      dimension == "placement" & placement == "chest" ~ "eye_chest_medi",
      dimension == "placement" & placement == "wrist" & metric == "MDER" ~ "eye_wrist_full",
      dimension == "placement" & placement == "wrist" ~ "eye_wrist_medi",
      dimension == "optical" ~ "eye_full",
      metric == "MDER" ~ "eye_full",
      TRUE ~ "eye_medi"
    ),
    site = "A", Id = "001", Date = as.Date("2025-01-01") + day,
    analysis_unit_type = "participant_day",
    metric_class = "level", metric_scope = "daily", metric_geometry = "linear",
    value_a = as.numeric(day), value_b = as.numeric(day) + 0.25,
    available_a = !(candidate_config == "eye__LIGHT__10s" & metric == "MDER"),
    available_b = TRUE,
    pair_available = available_a & available_b,
    pair_unavailable_reason = if_else(pair_available, NA_character_, "candidate unavailable")
  ) |>
  select(
    dimension, comparison_pair_id, support_id, site, Id, analysis_unit_type, Date,
    metric, metric_class, metric_scope, metric_geometry, value_a, value_b,
    available_a, available_b, pair_available, pair_unavailable_reason
  )
pairs <- rq1_inference_pairs(pairwise)
stopifnot(
  n_distinct(pairs$candidate_config) == 8L,
  !any(pairs$resolution_s == 300L),
  all(pairs$support_id[pairs$candidate_config == "chest__MEDI__10s" & pairs$metric == "mean_MEDI"] == "eye_chest_medi"),
  all(pairs$support_id[pairs$candidate_config == "wrist__MEDI__10s" & pairs$metric == "MDER"] == "eye_wrist_full"),
  all(pairs$support_id[pairs$candidate_config == "eye__LIGHT__10s"] == "eye_full"),
  all(!is.na(pairs$pair_reason[pairs$candidate_config == "eye__LIGHT__10s" & pairs$metric == "MDER"])),
  all(pairs$reference_value == pairwise$value_b),
  all(pairs$candidate_value == pairwise$value_a)
)
reference_pairs <- rq1_inference_reference_pairs(pairs)
stopifnot(
  all(reference_pairs$candidate_config == "eye__MEDI__10s"),
  all(reference_pairs$support_id[reference_pairs$metric == "mean_MEDI"] == "eye_medi"),
  all(reference_pairs$support_id[reference_pairs$metric == "MDER"] == "eye_full")
)
expect_error(rq1_inference_pairs(bind_rows(pairwise, pairwise[1, ])))

# FE slopes agree with an independently fitted participant-dummy linear model.
set.seed(42)
g <- crossing(Id = sprintf("P%02d", 1:12), day = 1:5) |>
  mutate(
    site = if_else(Id <= "P06", "A", "B"), Date = as.Date("2025-01-01") + day,
    reference_value = rnorm(n()) + as.integer(factor(Id)),
    candidate_value = reference_value,
    outcome_value = 2 * reference_value + as.integer(factor(Id)) + rnorm(n()),
    metric_geometry = "linear", pair_reason = NA_character_, outcome_reason = NA_character_
  )
fit <- rq1_inference_fit(g, B = 40L)
expected <- unname(coef(lm(outcome_value ~ I(reference_value / sd(reference_value)) + factor(Id), data = g))[[2]])
stopifnot(
  abs(fit$summary$reference_beta - expected) < 1e-10,
  fit$summary$beta_difference == 0,
  fit$summary$difference_lower == 0,
  all(fit$bootstrap$beta_difference == 0),
  nrow(fit$support) == nrow(g),
  is.finite(fit$task_summary$reference_association_strength),
  abs(fit$task_summary$inference_deviation) < 1e-12
)

scaled <- g; scaled$candidate_value <- 2 * scaled$reference_value
ff <- rq1_inference_fit(scaled, B = 40L)
stopifnot(
  abs(ff$summary$candidate_beta * 2 - ff$summary$reference_beta) < 1e-10,
  is.finite(ff$task_summary$inference_deviation),
  ff$task_summary$inference_deviation > 0
)

# Row order and constant participant offsets cannot change paired FE estimates.
shifted <- g
shifted$candidate_value <- shifted$candidate_value + as.integer(factor(g$Id)) * 100
stopifnot(
  abs(rq1_inference_fit(shifted, B = 0L)$summary$beta_difference) < 1e-10,
  identical(fit$summary, rq1_inference_fit(g[nrow(g):1, ], B = 40L)$summary)
)
missing <- g; missing$pair_reason[1] <- "candidate_unavailable"
stopifnot(nrow(rq1_inference_fit(missing, B = 0L)$support) == nrow(g) - 1L)
singular <- g; singular$candidate_value <- 1
stopifnot(rq1_inference_fit(singular, B = 0L)$summary$status == "singular_within_participant_exposure")
flat <- g; flat$outcome_value <- as.numeric(factor(flat$Id))
stopifnot(rq1_inference_fit(flat, B = 0L)$summary$status == "no_within_participant_outcome_variation")
unavailable <- g; unavailable$pair_reason <- "candidate_unavailable"
stopifnot(rq1_inference_fit(unavailable, B = 0L)$summary$status == "measurement_unavailable")

# Circular predictors are treated as one two-parameter association geometry.
circ <- g
circ$metric_geometry <- "circular_time"
circ$reference_value <- (g$reference_value * 6000) %% 86400
circ$candidate_value <- circ$reference_value + 86400
cf <- rq1_inference_fit(circ, B = 40L)
stopifnot(
  nrow(cf$summary) == 2L,
  max(abs(cf$summary$beta_difference)) < 1e-10,
  all(cf$summary$distortion_A < 1e-12),
  is.finite(cf$task_summary$inference_deviation),
  cf$task_summary$inference_deviation < 1e-10,
  abs(rq1_inference_quadnorm(c(1, 2), diag(2)) - sqrt(5)) < 1e-12
)

# Validate version guards and all advertised RDS compression modes.
ms_assert_version(list(v = "current"), "v", "current")
expect_error(ms_assert_version(list(v = c("current", NA)), "v", "current"))
expect_error(ms_assert_version(list(), "v", "current"))

tmp <- tempfile("engineering_validation_"); dir.create(tmp)
previous <- Sys.getenv("RQ1_PART_COMPRESSION", unset = NA_character_)
for (compression in c("gzip", "bzip2", "xz", "none")) {
  Sys.setenv(RQ1_PART_COMPRESSION = compression)
  path <- file.path(tmp, paste0(compression, ".rds"))
  rq1_write_part_atomic(g, path)
  stopifnot(identical(readRDS(path), g), file.exists(paste0(path, ".ok")))
}
if (is.na(previous)) Sys.unsetenv("RQ1_PART_COMPRESSION") else Sys.setenv(RQ1_PART_COMPRESSION = previous)

# RQ2 extracted helpers retain direct functional tests.
source("scripts/utils/rq2_model_helpers.R")
helpers <- rq2_model_helpers()
scaled_helper <- helpers$scale_train_test(tibble(x = c(1, 2, 3)), tibble(x = c(10, 20)), "x")
stopifnot(identical(scaled_helper$te$x, c(8, 18)), helpers$performance(c(1, 2), c(1, 2))$r2 == 1)

# Figure identity/routing is tested once in its package-free contract test.
source("scripts/tests/validate_figure_registry.R")

cat("PASS: all R sources parse; three-domain frozen-pairwise outcome/support/FE/bootstrap/circular/version/compression checks\n")
