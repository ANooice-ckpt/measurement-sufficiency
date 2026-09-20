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

  # A wholly absent copy must report the requested repository root, never fall
  # back a second time to the working repository (which can contain old parts).
  absent_root <- file.path(root, "not_copied")
  absent <- manifest; absent$part_dir <- file.path(root, "missing_origin")
  resolved <- recovery_resolve_parts(absent, absent_root)
  stopifnot(identical(resolved$paths,
    file.path(absent_root,"results","rq1","pairwise_parts",version,manifest$parts)),
    length(resolved$problems)==4L)
})
cat("PASS: original-path precedence, same-version fallback, all parts/markers, immutable manifest.\n")

# Read-only validation of an explicitly selected export must not refresh stale
# dayparts. Inject every upstream dependency; no weather/diary data are accessed.
local({
  root<-tempfile("daypart_validation_");dir.create(root)
  on.exit(unlink(root,recursive=TRUE))
  path<-file.path(root,"dayparts.rds")
  e<-new.env(parent=globalenv())
  e$check<-recovery_ensure_dayparts;environment(e$check)<-e
  e$recovery_daypart_provenance<-function(...)list(version="current")
  e$recovery_hash<-function(...)"fixture_hash"
  e$recovery_temporal_context<-function(...)invisible(TRUE)
  e$recovery_read_csv<-function(...)stop("BUILD_FORBIDDEN_IN_UNIT_TEST")
  check<-function()e$check(path,"unused","unused",character(),"core","rq1",allow_build=FALSE)
  err<-tryCatch(check(),error=identity)
  stopifnot(inherits(err,"error"),grepl("Missing or stale",conditionMessage(err)),!file.exists(path))
  obj<-list(provenance=list(version="current"),data_md5="fixture_hash",data="fixture")
  saveRDS(obj,path);before<-tools::md5sum(path)
  stopifnot(isTRUE(check()$reused),identical(tools::md5sum(path),before))
  obj$provenance$version<-"old";saveRDS(obj,path);before<-tools::md5sum(path)
  err<-tryCatch(check(),error=identity)
  stopifnot(inherits(err,"error"),grepl("Missing or stale",conditionMessage(err)),
    identical(tools::md5sum(path),before))
})
cat("PASS: explicit-export daypart validation cannot build or overwrite inputs.\n")
