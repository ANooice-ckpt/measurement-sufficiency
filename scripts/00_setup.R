options(repos = c(CRAN = "https://cloud.r-project.org"))

if (!requireNamespace("renv", quietly = TRUE)) install.packages("renv")
if (!file.exists("renv.lock")) renv::init(bare = TRUE, restart = FALSE)

core <- c("LightLogR@0.10.3", "melidosData@1.0.6", "tidyverse", "gt", "cowplot")
renv::install(core, prompt = FALSE)
renv::snapshot(prompt = FALSE)

dir.create("logs", showWarnings = FALSE, recursive = TRUE)
writeLines(capture.output(sessionInfo()), "logs/sessionInfo_setup.txt")
