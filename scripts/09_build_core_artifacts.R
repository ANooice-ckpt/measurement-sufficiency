source("scripts/utils/melidos_io.R")
source("scripts/utils/rq1_metrics.R")
source("scripts/utils/core_artifacts.R")
suppressPackageStartupMessages({
  library(tidyverse)
  library(melidosData)
  library(readxl)
})

required_r <- package_version("4.5.0")
if (getRversion() != required_r) {
  stop(sprintf("Core artifact build is pinned to R 4.5.0; current runtime is %s", getRversion()))
}

Sys.setenv(OMP_NUM_THREADS = "1", OPENBLAS_NUM_THREADS = "1", MKL_NUM_THREADS = "1")
workers <- as.integer(Sys.getenv("CORE_WORKERS", unset = "1"))
if (!is.finite(workers) || workers < 1L) workers <- 1L
if (.Platform$OS.type == "windows" && workers > 1L) {
  message("CORE_WORKERS>1 uses forked workers only on Unix-like systems; falling back to 1 on Windows.")
  workers <- 1L
}
force_rebuild <- identical(Sys.getenv("CORE_FORCE", unset = "0"), "1")

dir.create("data/interim/core/supports", recursive = TRUE, showWarnings = FALSE)
dir.create("data/interim/core/metrics", recursive = TRUE, showWarnings = FALSE)
dir.create("data/derived/core", recursive = TRUE, showWarnings = FALSE)
dir.create("logs", recursive = TRUE, showWarnings = FALSE)

message("Core artifact build: R ", getRversion(), ", workers=", workers, ", force=", force_rebuild)

metric_types <- read_excel("external/zauner_position/data/metric_types.xlsx") |>
  transmute(metric = name, metric_class = metric_type)
if (n_distinct(metric_types$metric) != 54L) stop("Expected 54 metric definitions")

# Stage 1: prepare six explicit comparison/support lattices. RQ1 main effects keep
# their maximal support; stricter *_full supports are used only when joint optical
# configurations require all channels on the same underlying time support.
support_grid <- core_support_grid()
prepare_one <- function(i) {
  row <- support_grid[i, ]
  path <- file.path("data/interim/core/supports", paste0(row$site, "__", row$support_id, ".rds"))
  if (!force_rebuild && file.exists(path)) return(path)
  message("prepare support: ", row$site, " / ", row$support_id)
  x <- core_prepare_support(row$site, row$support_id)
  if (is.null(x)) return(NA_character_)
  saveRDS(x, path, compress = FALSE)
  path
}
idx <- seq_len(nrow(support_grid))
support_paths <- if (workers > 1L) {
  parallel::mclapply(idx, prepare_one, mc.cores = min(workers, length(idx)))
} else lapply(idx, prepare_one)
support_paths <- unlist(support_paths, use.names = FALSE)
support_paths <- support_paths[!is.na(support_paths) & file.exists(support_paths)]

# Stage 2: compute every observable placement x optical x temporal-resolution
# configuration within each support lattice. Each support block is persistent so
# an interrupted cloud run can resume without recomputing completed blocks.
compute_one <- function(path) {
  key <- tools::file_path_sans_ext(basename(path))
  out <- file.path("data/interim/core/metrics", paste0(key, "__metrics.rds"))
  if (!force_rebuild && file.exists(out)) return(out)
  message("compute metric block: ", key)
  m <- core_compute_support_metrics(path)
  saveRDS(m, out, compress = FALSE)
  out
}
metric_paths <- if (workers > 1L) {
  parallel::mclapply(support_paths, compute_one, mc.cores = min(workers, length(support_paths)))
} else lapply(support_paths, compute_one)
metric_paths <- unlist(metric_paths, use.names = FALSE)

# Stage 3: merge the expensive configuration-level values into one durable cube.
emitted <- map_dfr(metric_paths, readRDS)
metric_cube <- core_expand_metric_availability(emitted, metric_types)
readr::write_csv(metric_cube, "data/derived/core/metric_cube.csv.gz", na = "")

# Stage 4: one daily context row per support-specific participant-day.
quality <- map_dfr(support_paths, ~core_support_quality(readRDS(.x)))
site_meta <- core_site_metadata()
era5_daily <- map_dfr(seq_len(nrow(site_meta)), function(i) {
  out <- core_read_era5_daily(site_meta$site[i], site_meta$timezone[i])
  if (is.null(out)) tibble(site = character(), Date = as.Date(character())) else out
})
unit_context <- quality |>
  left_join(site_meta, by = "site") |>
  left_join(era5_daily, by = c("site", "Date")) |>
  mutate(
    year = lubridate::year(Date),
    month = lubridate::month(Date),
    day_of_year = lubridate::yday(Date),
    weekday = lubridate::wday(Date, label = TRUE, abbr = TRUE)
  ) |>
  select(
    support_id, site, Id, analysis_unit_type, analysis_unit_id, Date,
    city, country, timezone, latitude, longitude,
    expected_epochs, valid_epochs, valid_fraction, n_missing_blocks, largest_missing_gap_s,
    first_valid_time, last_valid_time, year, month, day_of_year, weekday,
    everything()
  )
readr::write_csv(unit_context, "data/derived/core/unit_context.csv.gz", na = "")

# Small factual diagnostics only; the two CSV.GZ files above are the core artifacts.
diag <- tibble(
  artifact = c("metric_cube", "unit_context"),
  rows = c(nrow(metric_cube), nrow(unit_context)),
  participants = c(n_distinct(metric_cube$Id), n_distinct(unit_context$Id)),
  sites = c(n_distinct(metric_cube$site), n_distinct(unit_context$site))
)
write.csv(diag, "logs/core_artifact_summary.csv", row.names = FALSE)
writeLines(capture.output(sessionInfo()), "logs/sessionInfo_core_artifacts.txt")

message("Done:")
message("  data/derived/core/metric_cube.csv.gz")
message("  data/derived/core/unit_context.csv.gz")
