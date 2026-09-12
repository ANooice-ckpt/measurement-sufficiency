# Tiny path-only fixture; no frozen pair contents or models are loaded.
source("scripts/12d_rq2_recovery.R")
local({
  root <- tempfile("recovery_paths_")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE))
  version <- "synthetic_version"
  fallback <- file.path(root, "results", "rq1", "pairwise_parts", version)
  dir.create(fallback, recursive = TRUE)
  manifest <- list(artifact_type = "partitioned_rq1_pairwise_change",
    rq1_analysis_version = version, part_dir = file.path(root, "absent_ecs_directory"),
    parts = c("anchor_a.rds", "anchor_b.rds"))
  path <- file.path(root, "frozen_manifest.rds")
  saveRDS(manifest, path)
  before <- tools::md5sum(path)
  expected <- file.path(fallback, manifest$parts)
  stopifnot(all(file.create(c(expected, paste0(expected, ".ok")))))

  resolved <- recovery_resolve_parts(readRDS(path), root)
  stopifnot(identical(resolved$upstream$part_dir, fallback),
    identical(resolved$paths, expected), length(resolved$problems) == 0L,
    identical(readRDS(path), manifest), identical(tools::md5sum(path), before))

  # Every declared part AND marker is required, not merely the first part.
  unlink(paste0(expected[2], ".ok"))
  stopifnot(length(recovery_resolve_parts(manifest, root)$problems) == 1L)
  unlink(expected[2])
  stopifnot(length(recovery_resolve_parts(manifest, root)$problems) == 2L)
  stopifnot(all(file.create(c(expected[2], paste0(expected[2], ".ok")))))

  # Existing original directory wins even when incomplete and fallback is complete.
  dir.create(manifest$part_dir)
  original <- recovery_resolve_parts(manifest, root)
  stopifnot(identical(original$upstream, manifest), length(original$problems) == 4L)
  original_paths <- file.path(manifest$part_dir, manifest$parts)
  stopifnot(all(file.create(c(original_paths, paste0(original_paths, ".ok")))))
  original <- recovery_resolve_parts(manifest, root)
  stopifnot(identical(original$upstream, manifest), length(original$problems) == 0L,
    identical(tools::md5sum(path), before))
})
cat("PASS: original-path precedence, same-version fallback, all parts/markers, immutable manifest.\n")
