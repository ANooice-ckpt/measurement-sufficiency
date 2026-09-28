# Run the production support-cache adapter and core validator against temporary
# files. No raw source inputs or production output directories are opened.
suppressPackageStartupMessages(library(tidyverse))
source("scripts/utils/core_artifacts.R")
source("scripts/utils/protocol_windows.R")

local({
  repo <- normalizePath(".", winslash = "/")
  root <- tempfile("core_pipeline_contract_")
  dir.create(root)
  on.exit({setwd(repo); unlink(root, recursive = TRUE)}, add = TRUE)
  adapter <- Filter(function(x) is.call(x) && identical(x[[1]], as.name("<-")) &&
    identical(x[[2]], as.name("prepare_one")), parse("scripts/09_build_core_artifacts.R"))
  stopifnot(length(adapter) == 1L)
  e <- new.env(parent = globalenv())
  e$SUPPORT_DIR <- file.path(root, "supports"); dir.create(e$SUPPORT_DIR)
  e$support_grid <- tibble(site = c("valid", "excluded"), support_id = "eye_medi")
  e$force_rebuild <- FALSE
  e$core_prepare_support <- function(site, support_id) {
    x <- tibble(Id = if (site == "valid") "A" else character())
    attr(x, "source_sampling_audit") <- tibble(site = site, support_id = support_id,
      Id = "A", placement = "eye", source_available = site == "valid",
      support_available = site == "valid")
    x
  }
  eval(adapter[[1]], e)
  fresh <- lapply(1:2, e$prepare_one)
  e$core_prepare_support <- function(...) stop("Resume must reuse the audited support")
  resumed <- lapply(1:2, e$prepare_one)
  stopifnot(identical(fresh, resumed), file.exists(fresh[[1]]$path),
    is.na(fresh[[2]]$path), nrow(fresh[[2]]$sampling) == 1L,
    !fresh[[2]]$sampling$support_available)

  dir.create(file.path(root, "scripts", "utils"), recursive = TRUE)
  for (name in c("analysis_design.R", "core_artifacts.R", "artifact_validation.R")) {
    stopifnot(file.copy(file.path(repo, "scripts", "utils", name),
                        file.path(root, "scripts", "utils", name)))
  }
  setwd(root)
  dir.create("results/core", recursive = TRUE)
  dir.create("results/diagnostics", recursive = TRUE)
  version <- core_artifact_version()
  cube <- crossing(Id = c("A", "C"), resolution_s = ms_all_temporal_s()) |>
    mutate(site = "test", support_id = "eye_medi", placement = "eye", optical = "MEDI",
      is_primary_resolution = resolution_s %in% ms_primary_temporal_s(), core_artifact_version = version)
  audit <- tibble(Id = c("A", "B", "C"), site = "test", support_id = "eye_medi", placement = "eye",
    source_available = c(TRUE, FALSE, TRUE), support_available = c(TRUE, FALSE, TRUE), core_artifact_version = version)
  write_csv(audit, "results/diagnostics/core_source_sampling_audit.csv")
  write_csv(tibble(key = c("core_artifact_version", "primary_resolutions_s", "reserve_resolutions_s"),
    value = c(version, paste(ms_primary_temporal_s(), collapse = ","), paste(ms_reserve_temporal_s(), collapse = ","))),
    "results/core/core_manifest.csv")
  windows <- duration_window_manifest(tibble(Id = "A", site = "test", support_id = "eye_medi",
    Date = as.Date("2025-01-01") + 0:5))
  run <- function(values = cube, manifest = windows) {
    write_csv(values, "results/core/metric_cube.csv.gz")
    saveRDS(manifest, "results/core/duration_window_manifest.rds")
    tryCatch({source(file.path(repo, "scripts/09b_validate_core_design.R"), local = new.env()); NULL},
      error = conditionMessage)
  }
  stopifnot(is.null(run()))
  stopifnot(grepl("ineligible native source", run(bind_rows(cube, mutate(cube, Id = "B"))), fixed = TRUE))
  # The site still has every cadence through C; A's missing state must fail.
  stopifnot(grepl("participant/configuration", run(filter(cube, !(Id == "A" & resolution_s == 120L))), fixed = TRUE))
  dangling <- windows; dangling$adjacent_higher_window_id[[1]] <- "absent"
  stopifnot(grepl("adjacent ID", run(manifest = dangling), fixed = TRUE))
  stopifnot(grepl("core_artifact_version", run(mutate(cube, core_artifact_version = "retired_core")), fixed = TRUE))
})
cat("PASS: support-audit resume and production core version/source/state/window guards\n")
