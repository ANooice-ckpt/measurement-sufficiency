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
