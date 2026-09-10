# Bounded static/synthetic verification. Never sources a Core/RQ main analysis.
# Run from repository root: Rscript scripts/tests/validate_engineering.R
suppressPackageStartupMessages(library(tidyverse))
source("scripts/utils/rq1_inference.R")
source("scripts/utils/rq1_pairwise_artifacts.R")
source("scripts/utils/melidos_io.R")
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

# Calendar alignment uses the diary's local zone, not the host zone/UTC.
diary <- tibble(Id = "001", wake = as.POSIXct("2025-04-02 01:00:00", tz = "Europe/Berlin"),
                sleepprep = as.POSIXct("2025-04-01 22:00:00", tz = "Europe/Berlin"),
                sleepquality = factor("Good", levels = c("Good", "Poor")),
                awakenings = 2, awake_duration = 12)
d <- rq1_sleep_outcomes(diary, "test")
stopifnot(all(d$Date == as.Date("2025-04-01")), d$outcome_value[d$outcome == "sleep_quality"] == 4,
          d$outcome_value[d$outcome == "awake_duration"] == 12)
expect_error(rq1_sleep_outcomes(bind_rows(diary, diary), "test"))
bad <- diary; bad$sleepquality <- "Unexpected"
expect_error(rq1_sleep_outcomes(bad, "test"))

# Matched supports retain separate pairwise cohorts and exclude reserve states.
cube <- crossing(support_id = c("eye_medi", "eye_full", "eye_chest_medi", "eye_chest_full", "eye_wrist_medi", "eye_wrist_full"),
                 placement = c("eye", "chest", "wrist"), optical = c("MEDI", "LIGHT"),
                 resolution_s = c(ms_primary_temporal_s(), 300L), metric = c("mean_MEDI", "MDER"), day = 1:3) |>
  mutate(site = "A", Id = "001", Date = as.Date("2025-01-01") + day,
         analysis_unit_type = "participant_day", config_id = paste(placement, optical, paste0(resolution_s, "s"), sep = "__"),
         metric_class = "level", metric_geometry = "linear", available = TRUE, value = as.numeric(day))
pairs <- rq1_inference_pairs(cube)
stopifnot(n_distinct(pairs$candidate_config) == 35L, !any(pairs$resolution_s == 300L),
          all(pairs$support_id[pairs$placement == "chest" & pairs$optical == "MEDI" & pairs$metric == "mean_MEDI"] == "eye_chest_medi"),
          all(pairs$support_id[pairs$placement == "wrist" & pairs$optical == "LIGHT"] == "eye_wrist_full"),
          all(!is.na(pairs$pair_reason[pairs$optical == "LIGHT" & pairs$metric == "MDER"])))
expect_error(rq1_inference_pairs(bind_rows(cube, cube[1, ])))

# FE slopes agree with an independently fitted participant-dummy linear model.
set.seed(42)
g <- crossing(Id = sprintf("P%02d", 1:12), day = 1:5) |>
  mutate(site = if_else(Id <= "P06", "A", "B"), Date = as.Date("2025-01-01") + day,
         reference_value = rnorm(n()) + as.integer(factor(Id)),
         candidate_value = reference_value,
         outcome_value = 2 * reference_value + as.integer(factor(Id)) + rnorm(n()),
         metric_geometry = "linear", pair_reason = NA_character_, outcome_reason = NA_character_)
fit <- rq1_inference_fit(g, B = 40L)
expected <- unname(coef(lm(outcome_value ~ I(reference_value / sd(reference_value)) + factor(Id), data = g))[[2]])
stopifnot(abs(fit$summary$reference_beta - expected) < 1e-10,
          fit$summary$beta_difference == 0, fit$summary$difference_lower == 0,
          all(fit$bootstrap$beta_difference == 0), nrow(fit$support) == nrow(g))
scaled <- g; scaled$candidate_value <- 2 * scaled$reference_value
ff <- rq1_inference_fit(scaled, B = 40L)
stopifnot(abs(ff$summary$candidate_beta * 2 - ff$summary$reference_beta) < 1e-10)
# Row order and constant participant offsets cannot change the paired FE result.
shifted <- g; shifted$candidate_value <- shifted$candidate_value + as.integer(factor(g$Id)) * 100
stopifnot(abs(rq1_inference_fit(shifted, B = 0L)$summary$beta_difference) < 1e-10,
          identical(fit$summary, rq1_inference_fit(g[nrow(g):1, ], B = 40L)$summary))
missing <- g; missing$pair_reason[1] <- "candidate_unavailable"
stopifnot(nrow(rq1_inference_fit(missing, B = 0L)$support) == nrow(g) - 1L)
singular <- g; singular$candidate_value <- 1
stopifnot(rq1_inference_fit(singular, B = 0L)$summary$status == "singular_within_participant_exposure")
flat <- g; flat$outcome_value <- as.numeric(factor(flat$Id))
stopifnot(rq1_inference_fit(flat, B = 0L)$summary$status == "no_within_participant_outcome_variation")
unavailable <- g; unavailable$pair_reason <- "candidate_unavailable"
stopifnot(rq1_inference_fit(unavailable, B = 0L)$summary$status == "measurement_unavailable")

# Circular predictors are invariant to full-period shifts, including midnight.
circ <- g; circ$metric_geometry <- "circular_time"
circ$reference_value <- (g$reference_value * 6000) %% 86400
circ$candidate_value <- circ$reference_value + 86400
cf <- rq1_inference_fit(circ, B = 40L)
stopifnot(nrow(cf$summary) == 2L, max(abs(cf$summary$beta_difference)) < 1e-10,
          all(cf$summary$distortion_A < 1e-12))

# Validate version guards and all advertised RDS compression modes.
ms_assert_version(list(v = "current"), "v", "current")
expect_error(ms_assert_version(list(v = c("current", NA)), "v", "current"))
expect_error(ms_assert_version(list(), "v", "current"))

# RQ2 helpers were moved without changing expressions or numerical behavior.
source("scripts/utils/rq2_model_helpers.R")
helpers <- rq2_model_helpers()
old_rq2 <- system2("git", c("show", "HEAD:scripts/12_rq2_analysis.R"), stdout = TRUE)
a <- which(startsWith(old_rq2, "  scale_train_test <- function"))
b <- which(startsWith(old_rq2, "  set.seed(task$seed)"))
if (length(a) == 1L && length(b) == 1L) {
  original <- new.env(parent = globalenv())
  eval(parse(text = old_rq2[a:(b - 1L)]), original)
  for (name in names(helpers)) stopifnot(identical(body(helpers[[name]]), body(original[[name]])))
}
scaled <- helpers$scale_train_test(tibble(x = c(1, 2, 3)), tibble(x = c(10, 20)), "x")
stopifnot(identical(scaled$te$x, c(8, 18)), helpers$performance(c(1, 2), c(1, 2))$r2 == 1)

# RQ1's extracted stage is token-for-token equivalent to its former block.
old_rq1 <- system2("git", c("show", "HEAD:scripts/10_rq1_analysis.R"), stdout = TRUE)
a <- which(startsWith(old_rq1, "rank_group_vars <-"))
b <- which(startsWith(old_rq1, "participant_balanced <-"))
if (length(a) == 1L && length(b) == 1L) {
  stopifnot(identical(parse(text = old_rq1[a:(b - 1L)], keep.source = FALSE),
                      parse("scripts/utils/rq1_relational_preservation.R", keep.source = FALSE)))
}
tmp <- tempfile("engineering_validation_"); dir.create(tmp)
previous <- Sys.getenv("RQ1_PART_COMPRESSION", unset = NA_character_)
for (compression in c("gzip", "bzip2", "xz", "none")) {
  Sys.setenv(RQ1_PART_COMPRESSION = compression)
  path <- file.path(tmp, paste0(compression, ".rds"))
  rq1_write_part_atomic(g, path)
  stopifnot(identical(readRDS(path), g), file.exists(paste0(path, ".ok")))
}
if (is.na(previous)) Sys.unsetenv("RQ1_PART_COMPRESSION") else Sys.setenv(RQ1_PART_COMPRESSION = previous)

# Shared RQ3 projection regression: compare with the pre-refactor block only,
# on a tiny joint-state fixture (no duration/core analysis is executed).
old <- system2("git", c("show", "HEAD:scripts/14_rq3_analysis.R"), stdout = TRUE)
start <- which(startsWith(old, "joint_outgoing <-"))
end <- which(startsWith(old, "boundary_audit <-"))
if (length(start) == 1L && length(end) == 1L) {
  setup <- function() {
    e <- new.env(parent = globalenv())
    e$joint_state_catalog <- tibble(support_id = "eye_medi", placement = "eye", optical = "MEDI",
      resolution_s = c(120L, 60L, 10L), n_days = c(1L, 2L, 6L), metric = "mean_MEDI",
      metric_class = "level", metric_geometry = "linear", config_id = c("r120__d1", "r60__d2", "r10__d6"))
    e$joint_pair_summary <- e$joint_state_catalog[1:2, ] |>
      rename(resolution_a = resolution_s, n_days_a = n_days, config_a_id = config_id) |>
      mutate(config_b_id = "r10__d6", A = c(.5, .1))
    e$NUMERIC_TOL <- 1e-12; e$CORE_VERSION <- "core"; e$RQ1_VERSION <- "rq1"; e$RQ3_VERSION <- "rq3"
    e$temporal_label <- ms_temporal_label; e$OUT <- tmp
    e
  }
  before <- setup(); after <- setup()
  eval(parse(text = old[start:(end - 1L)]), envir = before)
  sys.source("scripts/utils/rq3_joint_projection.R", envir = after)
  for (obj in c("joint", "pareto_occupancy", "pareto_summary", "pareto_frequency")) {
    stopifnot(identical(before[[obj]], after[[obj]]))
  }
}
cat("PASS: all R sources parse; diary/support/FE/paired bootstrap/circular/version/compression/RQ3 regression checks\n")
