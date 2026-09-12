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
for (p in recovery_layer_predictors("context")) d[[p]] <- NA_real_
d$external_temperature_c <- rnorm(n)
d$reference_value <- d$candidate_value + 3 * d$external_temperature_c
tr <- d[d$fold != 1, ]; te <- d[d$fold == 1, ]
fit <- recovery_predict(tr, te, recovery_layer_predictors("context"), FALSE, .01)
te_bad <- te; te_bad$reference_value <- 1e12; te_bad$target_state <- 1e20
fit_bad <- recovery_predict(tr, te_bad, recovery_layer_predictors("context"), FALSE, .01)
stopifnot(identical(fit$prediction, fit_bad$prediction))
te_missing <- te; te_missing$external_temperature_c[1] <- NA
stopifnot(all(is.finite(recovery_predict(tr, te_missing, recovery_layer_predictors("context"), FALSE, .01)$prediction)))
stopifnot(!"external_cloud" %in% fit$model$columns)
stopifnot(!"external_cloud_missing" %in% fit$model$columns)

tmp <- tempfile("recovery_synthetic_"); dir.create(tmp)
input <- file.path(tmp, "input.rds"); saveRDS(d, input)
task <- list(input = input, output = file.path(tmp, "checkpoint.rds"), index = 1L,
  run_id = "synthetic", lambda = .01, learner = "ridge", seed = 82L,
  xgb_config = recovery_xgb_config(), meta = tibble(metric = "synthetic"))
r <- recovery_task(task); obj <- readRDS(r$path)
stopifnot(obj$complete, nrow(obj$predictions) == 4L*n)
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
  "recovery_target", "recovery_inner", "recovery_boost", "recovery_xgb_config",
  "recovery_layer_predictors", "recovery_signature_predictors", "recovery_temporal_predictors",
  "recovery_states", "recovery_dayparts",
  "rq2_context_external_predictors", "rq2_context_micro_predictors", "rq2_context_behaviour_predictors")
t1 <- ct; t1$output <- file.path(tmp, "psock1.rds")
t2 <- ct; t2$output <- file.path(tmp, "psock2.rds"); t2$index <- 2L
rr <- ms_parallel_map(list(t1, t2), recovery_task, workers = 2L,
  packages = c("dplyr", "tibble"), exports = exports)
stopifnot(all(vapply(rr, function(r) r$status == "complete", logical(1))))
stopifnot(identical(readRDS(rr[[1]]$path)$predictions, co$predictions))

# Summary interfaces, including a genuinely zero raw denominator.
s <- expand.grid(task_index = 1:12, state = recovery_states(), stringsAsFactors = FALSE) |>
  as_tibble() |> mutate(comparison_pair_id = "synthetic", learner = "ridge", metric = paste0("m", task_index),
    A = (task_index - 1) * ifelse(state == "raw", .1,
      ifelse(state == "context", .02, ifelse(state == "signature", .05, .08))))
recovery_summaries(s, tmp, 1e-6)
sm <- readr::read_csv(file.path(tmp, "recovery_comparison.csv"), show_col_types = FALSE)
stopifnot(all(is.na(sm$G[sm$A_raw == 0])), all(sm$G_denominator_small[sm$A_raw == 0]))
stopifnot(all(sm$context_increment[sm$state == "context"] >= 0))
stopifnot(all(abs(sm$context_increment[sm$state == "context"] -
  (sm$A_signature[sm$state == "context"] - sm$A[sm$state == "context"])) < 1e-12))
both <- bind_rows(s, mutate(s, task_index = task_index + 12L, learner = "xgboost", A = A * 2))
recovery_summaries(both, tmp, 1e-6)
overview <- readr::read_csv(file.path(tmp, "recovery_overview.csv"), show_col_types = FALSE)
stopifnot(nrow(overview) == 6L, all(overview$n_metrics == 12L))

# Separate recoverable signals ensure signature and context are actually consumed.
set.seed(318)
layer_d <- d; layer_d$sl_mean <- rnorm(n)
layer_d$reference_value <- layer_d$candidate_value + 3*layer_d$sl_mean + 2*layer_d$external_temperature_c
layer_errors <- vapply(c("calibration", "signature", "context"), function(state) {
  pr <- recovery_predict(layer_d[layer_d$fold != 1, ], layer_d[layer_d$fold == 1, ],
    recovery_layer_predictors(state), FALSE, .01)
  mean(abs(pr$prediction - layer_d$reference_value[layer_d$fold == 1]))
}, numeric(1))
stopifnot(layer_errors[["signature"]] < layer_errors[["calibration"]],
  layer_errors[["context"]] < layer_errors[["signature"]])

# Identity anchoring must survive regularization, including a zero circular correction.
identity_tr <- tr; identity_tr$reference_value <- identity_tr$candidate_value
stopifnot(max(abs(recovery_predict(identity_tr, te, recovery_layer_predictors("context"), FALSE, 1e5)$prediction -
  te$candidate_value)) < 1e-12)
identity_c <- dc; identity_c$reference_value <- identity_c$candidate_value
stopifnot(max(abs(recovery_delta(recovery_predict(identity_c, dc, character(), TRUE, 1e5)$prediction,
  dc$candidate_value, TRUE))) < 1e-8)
stopifnot(max(abs(co$predictions$correction[co$predictions$state != "raw"] - 500)) < 1e-7)
for (field in c("reference_value", "delta", "z", "A", "outcome", "participant_key", "site")) {
  stopifnot(inherits(tryCatch(recovery_design(tr, te, field, FALSE), error = identity), "error"))
}

# Instrument the backend boundary without needing/installing XGBoost locally.
# This tests routing/isolation, not the numerical XGBoost engine.
local({
  real_boost <- recovery_boost
  on.exit(assign("recovery_boost", real_boost, envir = .GlobalEnv))
  calls <- list()
  assign("recovery_boost", function(inner, inner_y, full, full_y, config, seed) {
    calls[[length(calls) + 1L]] <<- list(inner = inner, inner_y = inner_y,
      full = full, full_y = full_y, config = config, seed = seed)
    list(prediction = rep(0, nrow(full$te)), model = list(selected_rounds = 1L))
  }, envir = .GlobalEnv)
  p <- recovery_predict(tr, te, recovery_layer_predictors("context"), FALSE, .01, "xgboost", 82L)
  p_bad <- recovery_predict(tr, te_bad, recovery_layer_predictors("context"), FALSE, .01, "xgboost", 82L)
  cal <- recovery_predict(tr, te, character(), FALSE, .01, "xgboost", 82L)
  stopifnot(identical(calls[[1]], calls[[2]]), identical(p$prediction, te$candidate_value))
  stopifnot(identical(p$model$inner_participants, cal$model$inner_participants),
    identical(calls[[1]]$config, calls[[3]]$config), identical(calls[[1]]$seed, calls[[3]]$seed))
  stopifnot(all(recovery_predictors() %in% colnames(calls[[1]]$full$tr)),
    identical(colnames(calls[[3]]$full$tr), "low"))
  stopifnot(!any(p$model$inner_participants$participant_key %in% te$participant_key))
  inner_val <- p$model$inner_participants$participant_key[p$model$inner_participants$inner_validation]
  changed <- tr; changed$external_temperature_c[changed$participant_key %in% inner_val] <- 1e9
  p_changed <- recovery_predict(changed, te, recovery_layer_predictors("context"), FALSE, .01, "xgboost", 82L)
  stopifnot(identical(calls[[1]]$inner$tr, calls[[4]]$inner$tr),
    identical(p$model$inner_centers, p_changed$model$inner_centers))
  pc <- recovery_predict(identity_c, dc, recovery_layer_predictors("context"), TRUE, .01, "xgboost", 82L)
  stopifnot(max(abs(recovery_delta(pc$prediction, dc$candidate_value, TRUE))) < 1e-8)
  mock_task <- ct; mock_task$learner <- "xgboost"; mock_task$output <- file.path(tmp, "mock_xgb.rds")
  mock_task$run_id <- "mock_xgb"
  mock_result <- readRDS(recovery_task(mock_task)$path)
  stopifnot(mock_result$complete, nrow(mock_result$predictions) == nrow(co$predictions),
    all(mock_result$predictions$learner == "xgboost"),
    identical(filter(mock_result$predictions, state == "raw")$standardized_error,
      filter(co$predictions, state == "raw")$standardized_error))
})
if (requireNamespace("xgboost", quietly = TRUE)) {
  cfg <- recovery_xgb_config(); cfg$nrounds <- 10L; cfg$early_stopping_rounds <- 3L
  xb <- recovery_predict(tr, te, recovery_layer_predictors("context"), FALSE, .01, "xgboost", 82L, cfg)
  xb_bad <- recovery_predict(tr, te_bad, recovery_layer_predictors("context"), FALSE, .01, "xgboost", 82L, cfg)
  stopifnot(identical(xb$prediction, xb_bad$prediction),
    xb$model$boosters[[1]]$selected_rounds >= 1L, xb$model$boosters[[1]]$selected_rounds <= 10L,
    is.raw(xb$model$boosters[[1]]$booster_raw))
  cat("PASS: real XGBoost synthetic backend smoke check.\n")
} else cat("SKIP: real XGBoost numerical smoke check (package absent); nested interface tested with instrumented backend.\n")

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

# Compact signature uses only the matching low configuration's frozen hourly basis.
u <- fixture |> distinct(support_id, site, Id, Date, config_a_id, placement, optical, resolution_s) |>
  rename(config_id = config_a_id) |> mutate(core_artifact_version = "synthetic", analysis_unit_type = "participant_day")
for (j in 0:23) u[[sprintf("isiv_h%02d", j)]] <- j / 10
sl <- recovery_signature(u, "synthetic")
stopifnot(length(recovery_signature_predictors()) == 16L, all(abs(sl$sl_morning - .8) < 1e-12),
  all(abs(sl$sl_adjacent_abs_change - .1) < 1e-12))
hi <- u[1, ]; hi$config_id <- "eye__MEDI__10s"; hi$placement <- "eye"; hi$resolution_s <- 10L
for (j in 0:23) hi[[sprintf("isiv_h%02d", j)]] <- 1e9
stopifnot(identical(sl, recovery_signature(bind_rows(u, hi), "synthetic")))
bad_u <- u; bad_u$resolution_s[1] <- 120L
stopifnot(fails(recovery_signature(bad_u, "synthetic")))

# Frozen temporal context supplies four real dayparts, including explicit missingness.
temporal_obj <- list(artifact_type = "recovery_deployable_dayparts_v1",
  daypart_contract = "local_06_11_14_18_24_v1", core_artifact_version = "synthetic", rq1_analysis_version = "synthetic",
  sources = data.frame(role = c("core_weather", "harmonized_diary"), path = c("weather", "diary"),
    md5 = rep(paste(rep("0", 32), collapse = ""), 2)),
  data = tibble(site = "synthetic", Id = "person", Date = as.Date("2020-01-01"), timezone = "UTC",
    daypart = names(recovery_dayparts()), radiation_mean_w_m2 = c(0, 100, 200, 0),
    outdoor_fraction = .2, work_fraction = .4, weather_hours = c(5, 3, 4, 6),
    environment_hours = c(5, 3, 4, 0), activity_hours = c(5, 3, 4, 6)))
ct <- recovery_temporal_context(temporal_obj, "synthetic", "synthetic")
stopifnot(all(recovery_temporal_predictors() %in% names(ct)), is.na(ct$ct_micro_evening),
  identical(ct$ct_external_midday, log1p(100)))
joined <- recovery_join_information(fp, sl, ct)
stopifnot(nrow(joined) == nrow(fp), identical(joined$eligible, fp$eligible))
stopifnot(fails(recovery_join_information(fp, sl[-1, ], ct)))
bad_ct <- temporal_obj; bad_ct$data <- bad_ct$data[-1, ]
stopifnot(fails(recovery_temporal_context(bad_ct, "synthetic", "synthetic")))
decomp <- readr::read_csv(file.path(tmp, "loss_decomposition.csv"), show_col_types = FALSE)
stopifnot(all(abs(decomp$reconstruction_error) < 1e-12))

# Daypart producer/cache: real overlap, duplicate union, conflicts, midnight,
# partial weather minutes, first creation, exact reuse and hash invalidation.
local({
  root <- file.path(tmp, "daypart_cache_test"); dir.create(root)
  unit_path <- file.path(root, "unit.csv"); weather_path <- file.path(root, "weather.csv")
  diary_path <- file.path(root, "diary.RData"); cache_path <- file.path(root, "context_dayparts.rds")
  cal <- tibble(core_artifact_version = "synthetic", analysis_unit_type = "participant_day",
    site = "synthetic", Id = "person", Date = as.Date("2020-01-01") + 0:1, timezone = "UTC")
  readr::write_csv(cal, unit_path)
  times <- as.POSIXct("2020-01-01", tz = "UTC") + seq(0, 2*86400-60, by = 60)
  w <- tibble(core_artifact_version = "synthetic", site = "synthetic", timezone = "UTC",
    time_utc = times, ssrd_w_m2 = 100)
  readr::write_csv(w, weather_path)
  lightexposurediary <- tibble(Id = "person",
    start = as.POSIXct(c("2020-01-01 06:00:00", "2020-01-01 10:30:00", "2020-01-01 10:30:00",
      "2020-01-01 11:30:00", "2020-01-01 23:30:00"), tz = "UTC"),
    end = as.POSIXct(c("2020-01-01 07:00:00", "2020-01-01 11:30:00", "2020-01-01 11:30:00",
      "2020-01-01 12:00:00", "2020-01-02 06:30:00"), tz = "UTC"))
  for (p in rq_context_activity_columns()) lightexposurediary[[p]] <- FALSE
  lightexposurediary$act_home[c(1, 4)] <- TRUE
  lightexposurediary$act_working_outdoor[c(2, 3, 5)] <- TRUE
  save(lightexposurediary, file = diary_path)
  args <- list(path = cache_path, weather_path = weather_path, unit_path = unit_path,
    diary_paths = c(synthetic = diary_path), core_version = "synthetic", rq1_version = "synthetic")
  first <- do.call(recovery_ensure_dayparts, args)
  hash <- tools::md5sum(cache_path); modified <- file.info(cache_path)$mtime
  second <- do.call(recovery_ensure_dayparts, args)
  stopifnot(!first$reused, second$reused, identical(hash, tools::md5sum(cache_path)),
    identical(modified, file.info(cache_path)$mtime))
  dd <- first$object$data
  day1 <- dd[dd$Date == as.Date("2020-01-01"), ]
  stopifnot(abs(day1$outdoor_fraction[day1$daypart == "morning"] - 1/3) < 1e-12,
    abs(day1$outdoor_fraction[day1$daypart == "midday"] - .5) < 1e-12,
    day1$environment_hours[day1$daypart == "midday"] == 1,
    dd$environment_hours[dd$Date == as.Date("2020-01-02") & dd$daypart == "morning"] == .5)
  intervals <- data.frame(start = c(0, 1800), end = c(3600, 3600), outdoor = c(1, 0), work = c(1, 0))
  conflict <- recovery_diary_part(intervals, 0, 3600)
  stopifnot(conflict[["environment_hours"]] == .5, conflict[["environment_conflict_hours"]] == .5)
  partial <- recovery_weather_part(data.frame(seconds = c(0, 60), ssrd_w_m2 = c(100, 200)), 30, 90)
  stopifnot(partial[["radiation_mean_w_m2"]] == 150)
  # Local civil boundaries remain local across DST; 06..24 is not UTC 06..24.
  dst <- recovery_daypart_bounds(as.Date("2025-03-30"), "Europe/Berlin", "morning")
  stopifnot(format(as.POSIXct(dst[1], origin = "1970-01-01", tz = "UTC"), "%H:%M") == "04:00")
  w$ssrd_w_m2 <- 200; readr::write_csv(w, weather_path)
  third <- do.call(recovery_ensure_dayparts, args)
  stopifnot(!third$reused, all(third$object$data$radiation_mean_w_m2 == 200),
    !identical(hash, tools::md5sum(cache_path)))
  stale <- third$object; stale$provenance$builder_hash <- "old_contract"; saveRDS(stale, cache_path)
  fourth <- do.call(recovery_ensure_dayparts, args)
  stopifnot(!fourth$reused, identical(fourth$object$data, third$object$data))
  corrupt <- fourth$object; corrupt$data$work_fraction[1] <- .9; saveRDS(corrupt, cache_path)
  stopifnot(fails(do.call(recovery_ensure_dayparts, args)))
})
unlink(tmp, recursive = TRUE)
cat("PASS: synthetic linear/circular, leakage boundary, imputation, shared CV support, checkpoint retry, PSOCK, summaries and frozen orientation/support/scale contracts.\n")
