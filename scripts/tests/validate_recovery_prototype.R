# Lightweight synthetic interfaces only: never reads formal recovery pairs or fits real data.
source("scripts/12d_rq2_recovery.R")
stopifnot(length(recovery_predictors()) == 18L, nrow(rq1_inference_anchor_map()) == 8L)
stopifnot(recovery_delta(60, 86340, TRUE) == 120)
stopifnot(recovery_delta(86340, 60, TRUE) == -120)
stopifnot(!any(grepl("state|MEDI|target|reference", recovery_predictors())))
set.seed(82)
n <- 120L
d <- tibble(site = "synthetic", Id = rep(sprintf("p%02d", 1:40), each = 3),
  Date = rep(as.Date("2020-01-01") + 0:2, 40), participant_key = Id,
  support_id = "eye_chest_medi", fold = rep(rep(1:5, 8), each = 3),
  candidate_value = rnorm(n), standardizer = 2, metric_geometry = "linear",
  eligible = TRUE, context_row_present = TRUE)
for (p in recovery_predictors()) d[[p]] <- NA_real_
d$external_temperature_c <- rnorm(n)
d$reference_value <- d$candidate_value + 3 * d$external_temperature_c
tr <- d[d$fold != 1, ]; te <- d[d$fold == 1, ]
fit <- recovery_predict(tr, te, recovery_predictors(), FALSE, .01)
te_bad <- te; te_bad$reference_value <- 1e12; te_bad$target_state <- 1e20
fit_bad <- recovery_predict(tr, te_bad, recovery_predictors(), FALSE, .01)
stopifnot(identical(fit$prediction, fit_bad$prediction))
te_missing <- te; te_missing$external_temperature_c[1] <- NA
stopifnot(all(is.finite(recovery_predict(tr, te_missing, recovery_predictors(), FALSE, .01)$prediction)))
stopifnot(!"external_cloud" %in% fit$model$columns)
stopifnot(!"external_cloud_missing" %in% fit$model$columns)

tmp <- tempfile("recovery_synthetic_"); dir.create(tmp)
input <- file.path(tmp, "input.rds"); saveRDS(d, input)
task <- list(input = input, output = file.path(tmp, "checkpoint.rds"), index = 1L,
  run_id = "synthetic", lambda = .01, meta = tibble(metric = "synthetic"))
r <- recovery_task(task); obj <- readRDS(r$path)
stopifnot(obj$complete, nrow(obj$predictions) == 3L*n)
a <- obj$predictions |> group_by(state) |> summarise(A = mean(abs(standardized_error)))
stopifnot(a$A[a$state == "context"] < a$A[a$state == "raw"] / 10)
stopifnot(a$A[a$state == "context"] < a$A[a$state == "calibration"] / 10)
stopifnot(isTRUE(recovery_task(task)$reused))
# Missing task input fails locally; another completed task is preserved and reusable.
bad <- task; bad$input <- file.path(tmp, "absent.rds"); bad$output <- file.path(tmp, "bad.rds")
suppressWarnings(b <- recovery_task(bad))
stopifnot(b$status == "failed", !readRDS(b$path)$complete)
bad$input <- input
stopifnot(recovery_task(bad)$status == "complete")

# Clock representation wraps through midnight without discontinuous target slopes.
dc <- d; dc$metric_geometry <- "circular_time"
dc$candidate_value <- (seq(82000, 90500, length.out = n)) %% 86400
dc$reference_value <- (dc$candidate_value + 500) %% 86400
saveRDS(dc, input)
ct <- task; ct$run_id <- "circular"; ct$output <- file.path(tmp, "circular.rds")
co <- readRDS(recovery_task(ct)$path)
stopifnot(co$complete, all(is.finite(co$predictions$standardized_error)))
stopifnot(all(co$predictions$prediction[co$predictions$state != "raw"] >= 0),
  all(co$predictions$prediction[co$predictions$state != "raw"] < 86400))

# Small PSOCK interface check: independent synthetic tasks, no production artifacts.
exports <- c("recovery_task", "recovery_fit", "recovery_predict", "recovery_design",
  "recovery_delta", "recovery_predictors", "recovery_atomic", "rq2_model_helpers",
  "rq2_context_external_predictors", "rq2_context_micro_predictors", "rq2_context_behaviour_predictors")
t1 <- ct; t1$output <- file.path(tmp, "psock1.rds")
t2 <- ct; t2$output <- file.path(tmp, "psock2.rds"); t2$index <- 2L
rr <- ms_parallel_map(list(t1, t2), recovery_task, workers = 2L,
  packages = c("dplyr", "tibble"), exports = exports)
stopifnot(all(vapply(rr, function(r) r$status == "complete", logical(1))))
stopifnot(identical(readRDS(rr[[1]]$path)$predictions, co$predictions))

# Summary interfaces, including a genuinely zero raw denominator.
s <- expand.grid(task_index = 1:12, state = c("raw", "calibration", "context"), stringsAsFactors = FALSE) |>
  as_tibble() |> mutate(comparison_pair_id = "synthetic", metric = paste0("m", task_index),
    A = (task_index - 1) * ifelse(state == "raw", .1, ifelse(state == "context", .02, .08)))
recovery_summaries(s, tmp, 1e-6)
sm <- readr::read_csv(file.path(tmp, "recovery_comparison.csv"), show_col_types = FALSE)
stopifnot(all(is.na(sm$G[sm$A_raw == 0])), all(sm$G_denominator_small[sm$A_raw == 0]))
stopifnot(all(sm$context_increment[sm$state == "context"] >= 0))

# Tiny synthetic frozen-pair contract: all eight anchors/52 metrics, no formal fit.
anchors <- rq1_inference_anchor_map()
metrics <- c("MDER", "nvRD", paste0("synthetic_metric_", 1:50))
fixture <- anchors[rep(seq_len(nrow(anchors)), each = 52L), ] |>
  mutate(metric = rep(metrics, 8), metric_geometry = "linear", metric_class = "synthetic",
    metric_scope = "participant_day", analysis_unit_type = "participant_day",
    site = "synthetic", Id = "person", Date = as.Date("2020-01-01"),
    dual = metric %in% c("MDER", "nvRD"),
    support_id = ifelse(dimension == "placement", paste0("eye_", placement, ifelse(dual, "_full", "_medi")),
      ifelse(dimension == "optical", "eye_full", ifelse(dual, "eye_full", "eye_medi"))),
    comparison_lattice = ifelse(dimension == "placement", paste0("placement_", placement), dimension),
    scale_anchor_config = ifelse(dimension == "placement", "eye_state",
      ifelse(dimension == "optical", "MEDI_state", "eye__MEDI__10s")),
    config_a_id = candidate_config, config_b_id = "eye__MEDI__10s",
    value_a = 1, value_b = 3, delta = 2, z = 1,
    available_a = !(optical == "LIGHT" & dual), available_b = TRUE,
    pair_available = available_a, available = pair_available,
    rq1_analysis_version = "synthetic", core_artifact_version = "synthetic")
fixture$value_a[!fixture$available] <- NA_real_
fixture$z[!fixture$available] <- NA_real_
fixture_path <- file.path(tmp, "frozen_synthetic.rds"); saveRDS(fixture, fixture_path)
ctx <- d[1, c("site", "Id", "Date", recovery_predictors())]; ctx$Id <- "person"
fi <- list(upstream = list(artifact_type = "partitioned_rq1_pairwise_change", part_dir = tmp,
  parts = basename(fixture_path)), version = "synthetic", core = "synthetic", context = ctx,
  scales = fixture |> distinct(comparison_lattice, metric, metric_geometry, scale_anchor_config) |>
    mutate(standardizer = 2))
fp <- recovery_pairs(fi)
stopifnot(nrow(fp) == 416L, sum(fp$eligible) == 414L, all(fp$context_row_present))
fails <- function(expr) inherits(tryCatch(force(expr), error = identity), "error")
wrong <- fixture; wrong$config_b_id[1] <- "wrist__MEDI__10s"; saveRDS(wrong, fixture_path)
stopifnot(fails(recovery_pairs(fi)))
wrong <- fixture; wrong$support_id[1] <- "eye_chest_wrist_full"; saveRDS(wrong, fixture_path)
stopifnot(fails(recovery_pairs(fi)))
saveRDS(fixture, fixture_path)
fi$scales$standardizer <- 3
stopifnot(fails(recovery_pairs(fi)))
unlink(tmp, recursive = TRUE)
cat("PASS: synthetic linear/circular, leakage boundary, imputation, shared CV support, checkpoint retry, PSOCK, summaries and frozen orientation/support/scale contracts.\n")
