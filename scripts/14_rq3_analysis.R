# Canonical RQ3 entrypoint. The corrected v5 runtime implements type-level
# residual stability, nested joint duration comparisons and the proper burden
# direction for Pareto dominance.
py <- Sys.which(c("python3", "python"))
py <- unname(py[nzchar(py)][1])
if (!length(py) || is.na(py) || !nzchar(py)) stop("Python 3 is required to build the corrected RQ3 runtime")
status <- system2(py, file.path("scripts", "utils", "build_downstream_v5_runtime.py"))
if (!identical(status, 0L)) stop("Failed to build corrected downstream v5 runtime")
source(file.path("results", "runtime", "14_rq3_analysis_v5.runtime.R"), local = .GlobalEnv)
