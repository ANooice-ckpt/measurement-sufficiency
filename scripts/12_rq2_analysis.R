# Canonical RQ2 entrypoint. The corrected implementation is generated from the
# versioned v5 source plus exact runtime safety patches so direct/manual runs
# cannot fall back to the retired v4 duration grouping or CV implementation.
py <- Sys.which(c("python3", "python"))
py <- unname(py[nzchar(py)][1])
if (!length(py) || is.na(py) || !nzchar(py)) stop("Python 3 is required to build the corrected RQ2 runtime")
status <- system2(py, file.path("scripts", "utils", "build_downstream_v5_runtime.py"))
if (!identical(status, 0L)) stop("Failed to build corrected downstream v5 runtime")
source(file.path("results", "runtime", "12_rq2_analysis_v5.runtime.R"), local = .GlobalEnv)

# The layered contextual extension deliberately runs in the same R process: it
# reuses the validated canonical transition objects created above and never
# reconstructs RQ1/core objects under a second set of rules.
source(file.path("scripts", "12c_rq2_context_models.R"), local = .GlobalEnv)
