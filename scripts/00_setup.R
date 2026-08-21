options(repos = c(CRAN = "https://cloud.r-project.org"))

required_r <- package_version("4.5.0")
if (getRversion() != required_r) {
  stop(sprintf("This project run is pinned to R 4.5.0; current runtime is %s", getRversion()))
}

if (!requireNamespace("renv", quietly = TRUE)) {
  install.packages("renv")
}

# The repository was initially snapshotted under R 4.4.2. Restoring that lockfile
# verbatim under R 4.5.0 can force obsolete R-recommended/package builds and is not
# the intended migration path. Once a genuine R-4.5.0 lockfile exists, ordinary
# setup returns to a normal lockfile restore.
lock_r_version <- NA_character_
if (file.exists("renv.lock")) {
  lock_head <- readLines("renv.lock", n = 20L, warn = FALSE)
  version_line <- grep('"Version"[[:space:]]*:', lock_head, value = TRUE)[1]
  if (length(version_line) && !is.na(version_line)) {
    lock_r_version <- sub(
      '.*"Version"[[:space:]]*:[[:space:]]*"([^"]+)".*',
      '\\1',
      version_line
    )
  }
}

renv::settings$r.version("4.5.0")

scientific_pins <- c(
  "LightLogR@0.10.3",
  "melidosData@1.0.6"
)
project_runtime <- c(
  "tidyverse",
  "readxl",
  "gt",
  "cowplot",
  "nlme",
  "lattice"
)

if (identical(lock_r_version, "4.5.0")) {
  message("Restoring the R 4.5.0 project lockfile")
  # retry=TRUE is useful after a platform/toolchain change because it allows renv
  # to retry a failed stale binary/source record with the currently available build.
  renv::restore(prompt = FALSE, retry = TRUE)
} else {
  message(sprintf(
    "Migrating project environment to R 4.5.0 (existing lockfile R version: %s)",
    ifelse(is.na(lock_r_version), "unknown/none", lock_r_version)
  ))

  # Do not restore the stale R-4.4.2 lockfile. Install current R-4.5-compatible
  # runtime dependencies first, then re-assert the two scientific package pins.
  renv::install(project_runtime, prompt = FALSE)
  renv::install(scientific_pins, prompt = FALSE)
}

# RQ2 uses nlme directly. Keep it and its recommended-package dependency
# available even when the system R installation does not expose them outside
# the project library, without updating them on every already-complete run.
direct_runtime <- c("nlme", "lattice")
missing_direct_runtime <- direct_runtime[!vapply(
  direct_runtime,
  requireNamespace,
  logical(1),
  quietly = TRUE
)]
if (length(missing_direct_runtime)) {
  renv::install(missing_direct_runtime, prompt = FALSE)
}

# Scientific package versions are part of the reproduction boundary.
required_versions <- c(LightLogR = "0.10.3", melidosData = "1.0.6")
for (pkg in names(required_versions)) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop("Required package is not installed after setup: ", pkg)
  }
  observed <- as.character(packageVersion(pkg))
  if (!identical(observed, required_versions[[pkg]])) {
    stop(sprintf(
      "Package %s must be %s; observed %s",
      pkg, required_versions[[pkg]], observed
    ))
  }
}

# Snapshot the actual R-4.5.0 project state. With implicit snapshots, unrelated
# packages left behind by an interrupted older restore are not pulled into the lockfile.
renv::snapshot(prompt = FALSE)

dir.create("logs", showWarnings = FALSE, recursive = TRUE)
writeLines(capture.output(sessionInfo()), "logs/sessionInfo_setup.txt")

message("Environment ready under R 4.5.0")
message("LightLogR: ", packageVersion("LightLogR"))
message("melidosData: ", packageVersion("melidosData"))
message("Updated lockfile: renv.lock")
