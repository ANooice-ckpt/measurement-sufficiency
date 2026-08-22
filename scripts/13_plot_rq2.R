# Canonical RQ2 plotting entrypoint. Plotting is generated from frozen v5 RQ2
# outputs only; it does not refit models or reconstruct analysis artifacts.
py <- Sys.which(c("python3", "python"))
py <- unname(py[nzchar(py)][1])
if (!length(py) || is.na(py) || !nzchar(py)) stop("Python 3 is required to build the corrected RQ2 plotting runtime")
status <- system2(py, file.path("scripts", "utils", "build_downstream_v5_runtime.py"))
if (!identical(status, 0L)) stop("Failed to build corrected downstream v5 runtime")
source(file.path("results", "runtime", "13_plot_rq2_v5.runtime.R"), local = .GlobalEnv)
