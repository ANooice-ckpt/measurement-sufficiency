# Canonical RQ2 entrypoint. The corrected implementation is generated from the
# versioned v5 source plus exact runtime safety patches so direct/manual runs
# cannot fall back to the retired v4 duration grouping or CV implementation.
py <- Sys.which(c("python3", "python"))
py <- unname(py[nzchar(py)][1])
if (!length(py) || is.na(py) || !nzchar(py)) stop("Python 3 is required to build the corrected RQ2 runtime")
status <- system2(py, file.path("scripts", "utils", "build_downstream_v5_runtime.py"))
if (!identical(status, 0L)) stop("Failed to build corrected downstream v5 runtime")
status <- system2(py, file.path("scripts", "utils", "patch_rq2_stream_runtime.py"))
if (!identical(status, 0L)) stop("Failed to patch parallel/checkpointed RQ2 streaming runtime")
status <- system2(py, file.path("scripts", "utils", "patch_rq2_context_stream_runtime.py"))
if (!identical(status, 0L)) stop("Failed to patch parallel RQ2 layered-context streaming runtime")

# The layered model stage supersedes the former external/state/joint fits but
# still needs the canonical runtime's transition features and gamma products.
# Preserve the user's model switch, suppress only those redundant base fits, and
# then restore the requested setting for the layered stage.
.requested_rq2_models <- tolower(Sys.getenv("RQ2_RUN_MODELS", unset = "1")) %in% c("1", "true", "yes")
.rq2_models_env_original <- Sys.getenv("RQ2_RUN_MODELS", unset = NA_character_)
Sys.setenv(RQ2_RUN_MODELS = "0")
source(file.path("results", "runtime", "12_rq2_analysis_v5.runtime.R"), local = .GlobalEnv)
RUN_MODELS <- .requested_rq2_models
Sys.setenv(RQ2_RUN_MODELS = if (.requested_rq2_models) "1" else "0")

# The layered contextual extension deliberately runs in the same R process: it
# reuses the validated canonical transition objects created above and never
# reconstructs RQ1/core objects under a second set of rules.
source(file.path("results", "runtime", "12c_rq2_context_models.runtime.R"), local = .GlobalEnv)

if (isTRUE(RUN_MODELS)) {
  expected_context_files <- file.path(OUT, c(
    "rq2_layered_context_model_coefficients.csv",
    "rq2_layered_context_model_performance.csv",
    "rq2_layered_context_model_manifest.csv",
    "rq2_layered_context_run_report.txt"
  ))
  missing_context_files <- expected_context_files[!file.exists(expected_context_files)]
  if (length(missing_context_files)) {
    stop("Layered RQ2 context stage did not produce: ", paste(missing_context_files, collapse = ", "))
  }

  expected_families <- c(
    "external_context", "microenvironment", "behaviour", "exposure_state", "joint"
  )
  observed_families <- unique(as.character(context_coefficients$model_family))
  missing_families <- setdiff(expected_families, observed_families)
  if (length(missing_families)) {
    stop("Layered RQ2 model families missing from coefficients: ", paste(missing_families, collapse = ", "))
  }

  # Fig. 2b is allowed to show only a fixed representative subset, but the
  # corresponding joint-model layers must actually have been estimated.
  required_joint_terms <- c(
    "micro_outdoor_fraction", "micro_daylight_indoor_fraction",
    "behaviour_work_fraction", "behaviour_exercise_level"
  )
  observed_joint_terms <- unique(as.character(
    context_coefficients$term[context_coefficients$model_family == "joint"]
  ))
  missing_joint_terms <- setdiff(required_joint_terms, observed_joint_terms)
  if (length(missing_joint_terms)) {
    stop("Layered RQ2 joint-model terms required for Fig. 2b are missing: ",
         paste(missing_joint_terms, collapse = ", "))
  }

  cat(
    "\nLayered context extension: ", CONTEXT_VERSION,
    "\nBase legacy coefficient fits: intentionally skipped; canonical conditional/gamma products retained.",
    "\nModel families: external opportunity, micro-environment, behaviour, exposure state, joint.",
    "\nContext inputs reuse unit_context ERA5 fields plus harmonised MeLiDos diaries; no core or ERA5 ingestion rule is redefined.\n",
    file = file.path(OUT, "RQ2_RUN_REPORT.md"), append = TRUE, sep = ""
  )

  # Delete large row-level model inputs only after the entire layered RQ2 stage
  # has succeeded. Durable pass-1 unit-feature checkpoints are intentionally kept.
  if (!KEEP_MODEL_INPUTS) {
    if (exists("SHARD_DIR") && dir.exists(SHARD_DIR)) {
      unlink(SHARD_DIR, recursive = TRUE, force = TRUE)
    }
    if (exists("CONTEXT_SHARD_ROOT") && dir.exists(CONTEXT_SHARD_ROOT)) {
      unlink(CONTEXT_SHARD_ROOT, recursive = TRUE, force = TRUE)
    }
  }
}

if (is.na(.rq2_models_env_original)) {
  Sys.unsetenv("RQ2_RUN_MODELS")
} else {
  Sys.setenv(RQ2_RUN_MODELS = .rq2_models_env_original)
}
rm(.requested_rq2_models, .rq2_models_env_original)
