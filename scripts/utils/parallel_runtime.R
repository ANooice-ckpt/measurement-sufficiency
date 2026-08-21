# Cross-platform base-R parallel runtime for RQ stages.
# One PSOCK design is used on Windows and Linux so task granularity and RNG
# behavior are comparable locally and on the server. BLAS/OpenMP stays at one
# thread per worker to prevent nested oversubscription.

ms_resolve_workers <- function(env_name, default = 1L, cap = NULL) {
  physical <- suppressWarnings(parallel::detectCores(logical = FALSE))
  logical <- suppressWarnings(parallel::detectCores(logical = TRUE))
  fallback <- if (is.finite(physical) && physical > 0) physical else if (is.finite(logical) && logical > 0) logical else 1L
  value <- suppressWarnings(as.integer(Sys.getenv(env_name, unset = as.character(default))))
  if (!is.finite(value) || value < 1L) value <- default
  value <- min(value, fallback)
  if (!is.null(cap)) value <- min(value, as.integer(cap))
  max(1L, value)
}

ms_worker_init <- function(root = normalizePath(".", winslash = "/", mustWork = TRUE), packages = character()) {
  setwd(root)
  Sys.setenv(
    OMP_NUM_THREADS = "1", OPENBLAS_NUM_THREADS = "1", MKL_NUM_THREADS = "1",
    VECLIB_MAXIMUM_THREADS = "1", NUMEXPR_NUM_THREADS = "1"
  )
  if (length(packages)) suppressPackageStartupMessages(lapply(packages, require, character.only = TRUE))
  NULL
}

ms_parallel_map <- function(X, FUN, workers = 1L, seed = NULL, packages = character(), exports = character()) {
  if (!length(X)) return(list())
  workers <- min(max(1L, as.integer(workers)), length(X))
  if (workers == 1L) {
    if (!is.null(seed)) set.seed(seed)
    return(lapply(X, FUN))
  }
  cl <- parallel::makePSOCKcluster(workers)
  on.exit(parallel::stopCluster(cl), add = TRUE)
  root <- normalizePath(".", winslash = "/", mustWork = TRUE)
  parallel::clusterCall(cl, ms_worker_init, root = root, packages = packages)
  exports <- intersect(unique(exports), ls(envir = .GlobalEnv))
  if (length(exports)) parallel::clusterExport(cl, exports, envir = .GlobalEnv)
  if (!is.null(seed)) parallel::clusterSetRNGStream(cl, iseed = as.integer(seed))
  parallel::parLapplyLB(cl, X, FUN, chunk.size = 1L)
}

ms_parallel_map_dfr <- function(X, FUN, workers = 1L, seed = NULL, packages = character(), exports = character()) {
  dplyr::bind_rows(ms_parallel_map(X, FUN, workers, seed, packages, exports))
}
