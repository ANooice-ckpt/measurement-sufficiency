options(repos = c(CRAN = "https://cloud.r-project.org"))

required_r <- package_version("4.5.0")
if (getRversion() != required_r) {
  stop(sprintf("This project run is pinned to R 4.5.0; current runtime is %s", getRversion()))
}

if (!requireNamespace("renv", quietly = TRUE)) install.packages("renv")
if (!file.exists("renv.lock")) renv::init(bare = TRUE, restart = FALSE)

# Restore the recorded package environment first. The snapshot at the end records
# the active R 4.5.0 runtime in renv.lock without changing scientific package pins.
renv::restore(prompt = FALSE)
core <- c("LightLogR@0.10.3", "melidosData@1.0.6", "tidyverse", "readxl", "gt", "cowplot")
renv::install(core, prompt = FALSE)
renv::snapshot(prompt = FALSE)

dir.create("logs", showWarnings = FALSE, recursive = TRUE)
writeLines(capture.output(sessionInfo()), "logs/sessionInfo_setup.txt")
