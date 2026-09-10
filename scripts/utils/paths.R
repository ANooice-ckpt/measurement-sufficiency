# Stable repository output locations. `data/` is reserved for raw inputs;
# every generated artifact is durable under results/.

# vroom may materialise compressed CSV input while parsing.  Point it at the
# parent temporary volume rather than a per-session child directory, which can
# become quota-limited on local Windows installations.  Users/servers may
# override VROOM_TEMP_PATH explicitly.
if (!nzchar(Sys.getenv("VROOM_TEMP_PATH"))) {
  vroom_temp_root <- dirname(tempdir())
  dir.create(vroom_temp_root, recursive = TRUE, showWarnings = FALSE)
  Sys.setenv(VROOM_TEMP_PATH = normalizePath(vroom_temp_root, winslash = "/", mustWork = FALSE))
}
# Honor an explicit override too: vroom cannot create a missing parent folder
# and otherwise reports a misleading disk-space error even on a free volume.
vroom_temp_path <- Sys.getenv("VROOM_TEMP_PATH")
if (!dir.exists(vroom_temp_path)) dir.create(vroom_temp_path, recursive = TRUE, showWarnings = FALSE)
if (!dir.exists(vroom_temp_path)) stop("Cannot create VROOM_TEMP_PATH: ", vroom_temp_path)

results_root <- function() "results"
core_root <- function() file.path(results_root(), "core")
core_cache_root <- function(version = NULL) {
  p <- file.path(core_root(), "cache")
  if (!is.null(version)) p <- file.path(p, version)
  p
}
rq_root <- function(rq) file.path(results_root(), rq)
diagnostics_root <- function() file.path(results_root(), "diagnostics")
logs_root <- function() file.path(results_root(), "logs")

ensure_result_dirs <- function(...) {
  dirs <- c(...)
  invisible(lapply(dirs, dir.create, recursive = TRUE, showWarnings = FALSE))
}
