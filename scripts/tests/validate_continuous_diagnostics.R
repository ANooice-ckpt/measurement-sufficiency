# Small independent row-level fixtures; no production fitting or artifact reads.
suppressPackageStartupMessages(library(data.table))
source("scripts/utils/continuous_diagnostics.R")

# A stable offset and invertible rescaling are absorbed by a refit, but holding
# beta fixed retains the within rescaling. Compare dense row-level signals to
# sufficient-statistic propagation for both one and two exposure coordinates.
for (p in c(1L, 2L)) {
  set.seed(910 + p)
  participant <- rep(1:5, each = 4)
  xr <- matrix(rnorm(20 * p), ncol = p)
  transform <- if (p == 1L) matrix(-2, 1, 1) else matrix(c(0, -1, 1, 0), 2, 2)
  xc <- xr %*% transform + participant
  demean <- function(x) apply(x, 2, function(v) v - ave(v, participant))
  wr <- demean(xr); wc <- demean(xc)
  beta_r <- seq_len(p) / 3
  beta_c <- as.numeric(solve(transform, beta_r))
  y <- as.numeric(wr %*% beta_r) + rnorm(20)
  y <- y - ave(y, participant)
  a <- continuous_fixed_beta(crossprod(wr), crossprod(wc), crossprod(wr, wc),
    beta_r, beta_c, sum(y^2), nrow(wr))
  direct <- sqrt(mean((wc %*% beta_r - wr %*% beta_r)^2))
  stopifnot(abs(a$fixed_beta_within_difference_rms - direct) < 1e-12,
    a$refitted_reconstructed_rms < 1e-7, a$fixed_beta_within_difference_rms > 0,
    abs(a$outcome_within_rms - sqrt(mean(y^2))) < 1e-12)
}
stopifnot(is.na(continuous_ratio(1, 0)), continuous_ratio(0, 2) == 0,
  is.na(continuous_ratio(NA_real_, 1)))

# Unequal target losses expose the scientifically important distinction between
# pooled-loss skill and the mean of target skills; zero baselines stay explicit.
d <- CJ(task_index = 1:2, state = c("null", "context"))
d[, `:=`(repeat_id = 1L, comparison_pair_id = "synthetic", metric = paste0("m", task_index),
  support_id = "pair", target = "mean_risk", metric_class = "level", dimension = "placement")]
d[, mae := ifelse(state == "null", c(1, 1, 9, 9), c(.5, .5, 9, 9))]
d[, mse := mae^2]
o <- continuous_risk_export(d)
stopifnot(abs(o$contrast[state == "context", mae_skill_pct] - 5) < 1e-12,
  o$contrast[state == "null", mae_skill_pct] == 0,
  all(o$task$state %in% c("null", "context")))
z <- copy(d); z[task_index == 1, `:=`(mae = 0, mse = 0)]
oz <- continuous_risk_export(z)
stopifnot(all(is.na(oz$task[task_index == 1, mae_skill_pct])),
  all(is.finite(oz$contrast$mae_skill_pct)))
bad <- try(continuous_risk_export(d[-1]), silent = TRUE)
stopifnot(inherits(bad, "try-error"))
cat("PASS: fixed-beta row-level agreement, coordinate transforms, zero denominators, paired-state equal-target skill\n")
