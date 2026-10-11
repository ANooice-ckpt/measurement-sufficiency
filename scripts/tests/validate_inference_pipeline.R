# Lightweight synthetic worker/serialization contract; no analysis or plot entrypoint.
# Complements validate_engineering.R's outcome, geometry and matched-support tests.
suppressPackageStartupMessages(library(dplyr))
source("scripts/utils/rq1_inference.R")
source("scripts/utils/parallel_runtime.R")

# Load just the two worker functions, without sourcing the production runner.
worker_names <- c("rq1_fit_inference_group_task", "rq1_fit_inference_groups")
worker_definitions <- Filter(function(x) is.call(x) && identical(x[[1]], as.name("<-")) &&
  is.symbol(x[[2]]) && as.character(x[[2]]) %in% worker_names,
  parse("scripts/10b_rq1_inferential_preservation.R"))
stopifnot(length(worker_definitions) == length(worker_names))
invisible(lapply(worker_definitions, eval, envir = globalenv()))

contract <- rq1_inference_contract()
anchors <- rq1_inference_anchor_map()
stopifnot(nrow(anchors) == 8L, !anyDuplicated(anchors$candidate_config),
          identical(anchors$resolution_s[anchors$dimension == "temporal"],
                    c(20L, 30L, 40L, 60L, 120L)),
          !contract$reference_config %in% anchors$candidate_config)
people <- expand.grid(person = 1:6, day = 1:4)
pairs <- bind_rows(lapply(anchors$candidate_config, function(config) {
  people |>
    transmute(site = if_else(person <= 3L, "A", "B"), Id = sprintf("P%02d", person),
              Date = as.Date("2025-01-01") + day,
              candidate_config = config, metric = "mean_MEDI", outcome = "sleep_quality",
              reference_value = sin(day + person) + day * .2,
              candidate_value = reference_value,
              outcome_value = 2 * reference_value + person + cos(day * person) / 10,
              metric_geometry = "linear", pair_reason = NA_character_,
              outcome_reason = NA_character_)
}))
keys <- c("candidate_config", "metric", "outcome")
serial <- rq1_fit_inference_groups(pairs, keys, B = 25L, seed_base = 817L, workers = 1L)
parallel <- rq1_fit_inference_groups(pairs, keys, B = 25L, seed_base = 817L, workers = 2L)
stopifnot(identical(serial, parallel), length(serial) == contract$anchor_count)
summary <- bind_rows(lapply(serial, `[[`, "task_summary"))
stopifnot(n_distinct(summary$candidate_config) == 8L,
          all(summary$status == "estimated"),
          all(abs(summary$signal_difference_outcome_sd) < 1e-12),
          all(abs(summary$inference_deviation) < 1e-12))

# Frozen data survive serialization with their types, row order and attributes.
local({
  path <- tempfile(fileext = ".rds")
  on.exit(unlink(path))
  artifact <- list(rq1_inference_version = rq1_inference_version("synthetic_rq1"),
                   contrast_summary = summary, paired_fits = serial)
  saveRDS(artifact, path)
  stopifnot(identical(readRDS(path), artifact))
})
cat("PASS: eight-anchor serial/PSOCK parity and frozen RDS round trip (synthetic only)\n")

# Independent small-matrix projector check, including the worst outcome direction.
# Production uses only thin QR/SVD; dense projectors are confined to this fixture.
g <- filter(pairs, candidate_config == anchors$candidate_config[1])
g$candidate_value <- -2 * g$reference_value + as.numeric(factor(g$Id))
gain_fit <- rq1_inference_fit(g, B=40L)
stopifnot(abs(gain_fit$task_summary$within_linear_gain + 2) < 1e-12,
          gain_fit$task_summary$signal_geometry_bound < 1e-7)
circ <- g
circ$metric_geometry <- "circular_time"
circ$reference_value <- (g$reference_value * 15000) %% 86400
circ$candidate_value <- (circ$reference_value + 7200) %% 86400
rotation_fit <- rq1_inference_fit(circ, B=40L)
stopifnot(is.na(rotation_fit$task_summary$within_linear_gain),
          rotation_fit$task_summary$signal_geometry_bound < 1e-7)
circ$candidate_value <- (circ$candidate_value + seq_len(nrow(circ))^2 * 100) %% 86400
circular_fit <- rq1_inference_fit(circ, B=40L)
for (fixture in list(mutate(g, candidate_value=reference_value+cos(seq_len(n()))), circ)) {
  fit <- rq1_inference_fit(fixture, B=0L)
  design <- function(x) {
    X <- if (fixture$metric_geometry[1] == "linear") matrix(x, ncol=1L)
      else cbind(sin(2*pi*x/86400), cos(2*pi*x/86400))
    apply(X, 2L, function(z) z-ave(z, fixture$Id))
  }
  wr <- design(fixture$reference_value); wc <- design(fixture$candidate_value)
  projector <- function(X) X %*% solve(crossprod(X), t(X))
  eig <- eigen(projector(wc)-projector(wr), symmetric=TRUE)
  worst <- which.max(abs(eig$values))
  bound <- fit$task_summary$signal_geometry_bound
  stopifnot(abs(bound-abs(eig$values[worst])) < 1e-10,
            fit$task_summary$signal_difference_outcome_sd <= bound+1e-10)
  fixture$outcome_value <- eig$vectors[, worst]
  stopifnot(abs(rq1_inference_fit(fixture, B=0L)$task_summary$signal_difference_outcome_sd-bound) < 1e-10)
  if (fixture$metric_geometry[1] == "linear") {
    stopifnot(abs(bound^2-fit$task_summary$within_shape_loss) < 1e-10)
  }
}

source("scripts/14b_rq3_resource_allocation.R")
for (fit in list(serial[[1]], gain_fit, rotation_fit, circular_fit)) {
  curve <- resource_compensation_curve(fit$component_blocks, replicates=30L, seed=912L, sizes=c(4L, 6L))
  stopifnot(all(curve$mc_valid == 30L), all(curve$matched_days_mean == 4 * curve$N),
            all(curve$site_allocation == ifelse(curve$N == 4L, "A:2;B:2", "A:3;B:3")),
            max(abs(curve$total_ms-curve$configuration_ms-curve$sampling_ms-curve$cross_ms)) < 1e-9)
  full <- filter(curve, N == 6L)
  stopifnot(max(abs(full$sampling_mean)) < 1e-10,
            abs(full$configuration_mean[full$space == "coefficient_reference_uncertainty"]-
                  fit$task_summary$inference_deviation) < 1e-8,
            abs(full$configuration_mean[full$space == "signal_full_support_outcome_sd"]-
                  fit$task_summary$signal_difference_outcome_sd) < 1e-7)
  if (fit$task_summary$signal_geometry_bound < 1e-7) {
    stopifnot(max(curve$configuration_mean[curve$space == "signal_full_support_outcome_sd"]) < 1e-7)
  }
}

# Exercise the frozen-input entrypoint in temp files, retaining unavailable rows.
local({
  input <- tempfile(fileext=".rds"); output <- tempfile(fileext=".csv")
  on.exit(unlink(c(input, output)))
  current_rq1 <- paste0("rq1_v6_maximal_duration_support__", core_artifact_version(), "__", ms_analysis_design_id())
  tasks <- summary |> mutate(support_id="synthetic", metric_geometry="linear", outcome_domain="Sleep", rq1_distortion_A=0)
  missing <- tasks[1, ]; missing$metric <- "unavailable_fixture"; missing$status <- "measurement_unavailable"
  a <- list(artifact_type="rq1_inferential_preservation", core_artifact_version=core_artifact_version(),
    analysis_design_id=ms_analysis_design_id(), rq1_analysis_version=current_rq1,
    rq1_inference_version=rq1_inference_version(current_rq1),
    contrast_summary=bind_rows(tasks, missing), component_blocks=bind_rows(lapply(serial, `[[`, "component_blocks")))
  saveRDS(a, input)
  result <- resource_compensation_main(input, output, replicates=10L, seed=912L)
  stopifnot(file.exists(output), nrow(result) == nrow(readr::read_csv(output, show_col_types=FALSE)),
            result$curve_status[result$metric == "unavailable_fixture"] == "measurement_unavailable",
            all(is.na(result$N[result$metric == "unavailable_fixture"])),
            all(result$configuration_mean[result$metric != "unavailable_fixture"] < 1e-7))
})
cat("PASS: projection bound/gain, fixed-support participant-count decomposition and frozen-input CSV (synthetic only)\n")
