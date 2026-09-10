# Synthetic end-to-end fixture in a temporary project. No real core/RQ analysis.
suppressPackageStartupMessages(library(tidyverse))
source("scripts/utils/core_artifacts.R")
source("scripts/utils/melidos_io.R")
root <- normalizePath(".", winslash = "/")
fixture <- tempfile("inference_fixture_", tmpdir = dirname(tempdir())); dir.create(fixture)
dir.create(file.path(fixture, "scripts"))
file.copy(file.path(root, "scripts", "utils"), file.path(fixture, "scripts"), recursive = TRUE)
file.copy(file.path(root, "scripts", c("10b_rq1_inferential_preservation.R", "16_plot_supplementary.R")), file.path(fixture, "scripts"))
for (dir in c("results/core", "results/rq1", "data/raw/melidos")) dir.create(file.path(fixture, dir), recursive = TRUE)
set.seed(8)
people <- crossing(Id = sprintf("P%02d", 1:6), day = 1:4) |>
  mutate(Date = as.Date("2025-01-01") + day, v = rnorm(n()),
         theta = runif(n(), 0, 86400))
cube <- crossing(support_id = c("eye_medi", "eye_full", "eye_chest_medi", "eye_chest_full", "eye_wrist_medi", "eye_wrist_full"),
                 placement = c("eye", "chest", "wrist"), optical = c("MEDI", "LIGHT"),
                 resolution_s = ms_primary_temporal_s(), metric = c("mean_MEDI", "timing_test"), people) |>
  mutate(site = "TEST", analysis_unit_type = "participant_day", available = TRUE,
         metric_class = if_else(metric == "mean_MEDI", "level", "timing"),
         metric_geometry = if_else(metric == "mean_MEDI", "linear", "circular_time"),
         value = if_else(metric == "mean_MEDI", v, theta),
         config_id = paste(placement, optical, paste0(resolution_s, "s"), sep = "__"),
         core_artifact_version = core_artifact_version())
readr::write_csv(cube, file.path(fixture, "results/core/metric_cube.csv.gz"))
saveRDS(list(artifact_type = "partitioned_rq1_pairwise_change", parts = "unused", part_dir = "unused",
             core_artifact_version = core_artifact_version(), analysis_design_id = ms_analysis_design_id(),
             rq1_analysis_version = "synthetic_rq1"), file.path(fixture, "results/rq1/rq1_pairwise_change_long.rds"))
sleepdiary <- people |> transmute(Id,
  wake = as.POSIXct(paste(Date + 1L, "07:00:00"), tz = "Europe/Berlin"),
  sleepprep = as.POSIXct(paste(Date, "23:00:00"), tz = "Europe/Berlin"),
  sleepquality = factor(c("Very poor", "Poor", "Fair", "Good", "Very good")[(day %% 5) + 1L]),
  awakenings = as.numeric(day %% 3), awake_duration = as.numeric(day * 4))
sleepdiary$sleepquality[1] <- NA
save(sleepdiary, file = file.path(fixture, "data/raw/melidos/TEST__sleepdiaries.RData"))
Sys.setenv(RQ1_INFERENCE_BOOT = "25", MS_SUPPLEMENTARY_ONLY = "inference", MS_FIG_DPI = "300",
           VROOM_TEMP_PATH = fixture)
setwd(fixture)
source("scripts/10b_rq1_inferential_preservation.R")
a <- rq1_run_inference()
stopifnot(n_distinct(a$summary$candidate_config) == 35L,
          all(a$summary$status == "estimated"),
          max(abs(a$summary$beta_difference)) < 1e-12,
          all(a$summary$n_matched_days[a$summary$outcome == "sleep_quality"] == 23L),
          all(a$summary$n_matched_days[a$summary$outcome != "sleep_quality"] == 24L),
          nrow(a$input_provenance) == 3L)
# Separate plotting process reads only the frozen result in the isolated project.
script <- file.path(fixture, "scripts/16_plot_supplementary.R")
status <- system2(file.path(R.home("bin"), "Rscript"), c("--vanilla", shQuote(script)))
stopifnot(status == 0L, file.exists("results/figures/FigS_RQ1_inferential_preservation.png"),
          !any(file.exists(file.path("results/figures", paste0("Fig", 1:5, ".png")))))
setwd(root)
cat("PASS: synthetic analysis -> frozen artifact -> supplementary figure\n")
cat("Fixture:", normalizePath(fixture, winslash = "/"), "\n")
