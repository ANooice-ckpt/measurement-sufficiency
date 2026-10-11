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

# Shared daily-input/signature/daypart checks retained from the retired recovery
# estimator test. No historical learner or formal data is used.
local({
  tmp <- tempfile("reliability_inputs_"); dir.create(tmp)
  on.exit(unlink(tmp, recursive=TRUE), add=TRUE)
# Tiny synthetic frozen-pair contract: all eight anchors/52 metrics, no formal fit.
anchors <- rq1_inference_anchor_map()
metrics <- c("MDER", "nvRD", paste0("synthetic_metric_", 1:50))
fixture <- anchors[rep(seq_len(nrow(anchors)), each = 52L), ] |>
  mutate(metric = rep(metrics, 8), metric_geometry = "linear", metric_class = "synthetic",
    metric_scope = "participant_day", analysis_unit_type = "participant_day",
    site = "synthetic", Id = "person", Date = as.Date("2020-01-01"),
    dual = metric %in% c("MDER", "nvRD"),
    support_id = ifelse(dimension == "placement", paste0("eye_", placement, ifelse(dual, "_full", "_medi")),
      ifelse(dimension == "optical", "eye_full", ifelse(dual, "eye_full", "eye_medi"))),
    comparison_lattice = ifelse(dimension == "placement", paste0("placement_", placement), dimension),
    scale_anchor_config = ifelse(dimension == "placement", "eye_state",
      ifelse(dimension == "optical", "MEDI_state", "eye__MEDI__10s")),
    config_a_id = candidate_config, config_b_id = "eye__MEDI__10s",
    value_a = 1, value_b = 3, delta = 2, z = 1,
    available_a = !(optical == "LIGHT" & dual), available_b = TRUE,
    pair_available = available_a, available = pair_available,
    rq1_analysis_version = "synthetic", core_artifact_version = "synthetic")
fixture$value_a[!fixture$available] <- NA_real_
fixture$z[!fixture$available] <- NA_real_
fixture_path <- file.path(tmp, "frozen_synthetic.rds"); saveRDS(fixture, fixture_path)
ctx <- tibble(site="synthetic", Id="person", Date=as.Date("2020-01-01"))
for (field in recovery_predictors()) ctx[[field]] <- 0
fi <- list(upstream = list(artifact_type = "partitioned_rq1_pairwise_change", part_dir = tmp,
  parts = basename(fixture_path)), version = "synthetic", core = "synthetic", context = ctx,
  scales = fixture |> distinct(comparison_lattice, metric, metric_geometry, scale_anchor_config) |>
    mutate(standardizer = 2))
fp <- recovery_pairs(fi)
stopifnot(nrow(fp) == 416L, sum(fp$eligible) == 414L, all(fp$context_row_present))
fails <- function(expr) inherits(tryCatch(force(expr), error = identity), "error")
wrong <- fixture; wrong$config_b_id[1] <- "wrist__MEDI__10s"; saveRDS(wrong, fixture_path)
stopifnot(fails(recovery_pairs(fi)))
wrong <- fixture; wrong$support_id[1] <- "eye_chest_wrist_full"; saveRDS(wrong, fixture_path)
stopifnot(fails(recovery_pairs(fi)))
saveRDS(fixture, fixture_path)
fi$scales$standardizer <- 3
stopifnot(fails(recovery_pairs(fi)))

# Compact signature uses only the matching low configuration's frozen hourly basis.
u <- fixture |> distinct(support_id, site, Id, Date, config_a_id, placement, optical, resolution_s) |>
  rename(config_id = config_a_id) |> mutate(core_artifact_version = "synthetic", analysis_unit_type = "participant_day")
for (j in 0:23) u[[sprintf("isiv_h%02d", j)]] <- j / 10
sl <- recovery_signature(u, "synthetic")
stopifnot(length(recovery_signature_predictors()) == 16L, all(abs(sl$sl_morning - .8) < 1e-12),
  all(abs(sl$sl_adjacent_abs_change - .1) < 1e-12))
hi <- u[1, ]; hi$config_id <- "eye__MEDI__10s"; hi$placement <- "eye"; hi$resolution_s <- 10L
for (j in 0:23) hi[[sprintf("isiv_h%02d", j)]] <- 1e9
stopifnot(identical(sl, recovery_signature(bind_rows(u, hi), "synthetic")))
bad_u <- u; bad_u$resolution_s[1] <- 120L
stopifnot(fails(recovery_signature(bad_u, "synthetic")))

# Frozen temporal context supplies four real dayparts, including explicit missingness.
temporal_obj <- list(artifact_type = "recovery_deployable_dayparts",
  daypart_contract = "local_06_11_14_18_24", core_artifact_version = "synthetic", rq1_analysis_version = "synthetic",
  sources = data.frame(role = c("core_weather", "harmonized_diary"), path = c("weather", "diary"),
    md5 = rep(paste(rep("0", 32), collapse = ""), 2)),
  data = tibble(site = "synthetic", Id = "person", Date = as.Date("2020-01-01"), timezone = "UTC",
    daypart = names(recovery_dayparts()), radiation_mean_w_m2 = c(0, 100, 200, 0),
    outdoor_fraction = .2, work_fraction = .4, daylight_indoor_fraction=.1,
    daylight_outdoor_fraction=.2, display_fraction=.1, home_fraction=.4, vehicle_fraction=0,
    source_hours=c(5,3,4,6), weather_hours = c(5, 3, 4, 6),
    environment_hours = c(5, 3, 4, 0), activity_hours = c(5, 3, 4, 6)))
ct <- recovery_temporal_context(temporal_obj, "synthetic", "synthetic")
stopifnot(all(recovery_temporal_predictors() %in% names(ct)), is.na(ct$ct_outdoor_evening),
  identical(ct$ct_radiation_midday, log1p(100)))
joined <- recovery_join_information(fp, sl, ct)
stopifnot(nrow(joined) == nrow(fp), identical(joined$eligible, fp$eligible))
stopifnot(fails(recovery_join_information(fp, sl[-1, ], ct)))
bad_ct <- temporal_obj; bad_ct$data <- bad_ct$data[-1, ]
stopifnot(fails(recovery_temporal_context(bad_ct, "synthetic", "synthetic")))
# Daypart producer/cache: real overlap, duplicate union, conflicts, midnight,
# partial weather minutes, first creation, exact reuse and hash invalidation.
local({
  root <- file.path(tmp, "daypart_cache_test"); dir.create(root)
  unit_path <- file.path(root, "unit.csv"); weather_path <- file.path(root, "weather.csv")
  diary_path <- file.path(root, "diary.RData"); cache_path <- file.path(root, "context_dayparts.rds")
  cal <- tibble(core_artifact_version = "synthetic", analysis_unit_type = "participant_day",
    site = "synthetic", Id = "person", Date = as.Date("2020-01-01") + 0:1, timezone = "UTC")
  readr::write_csv(cal, unit_path)
  times <- as.POSIXct("2020-01-01", tz = "UTC") + seq(0, 2*86400-60, by = 60)
  w <- tibble(core_artifact_version = "synthetic", site = "synthetic", timezone = "UTC",
    time_utc = times, ssrd_w_m2 = 100)
  readr::write_csv(w, weather_path)
  lightexposurediary <- tibble(Id = "person",
    start = as.POSIXct(c("2020-01-01 06:00:00", "2020-01-01 10:30:00", "2020-01-01 10:30:00",
      "2020-01-01 11:30:00", "2020-01-01 23:30:00"), tz = "UTC"),
    end = as.POSIXct(c("2020-01-01 07:00:00", "2020-01-01 11:30:00", "2020-01-01 11:30:00",
      "2020-01-01 12:00:00", "2020-01-02 06:30:00"), tz = "UTC"))
  lightexposurediary$lightsource_primary <- "Daylight outdoors (including shade)"
  for (p in rq_context_activity_columns()) lightexposurediary[[p]] <- FALSE
  lightexposurediary$act_home[c(1, 4)] <- TRUE
  lightexposurediary$act_working_outdoor[c(2, 3, 5)] <- TRUE
  save(lightexposurediary, file = diary_path)
  args <- list(path = cache_path, weather_path = weather_path, unit_path = unit_path,
    diary_paths = c(synthetic = diary_path), core_version = "synthetic", rq1_version = "synthetic")
  first <- do.call(recovery_ensure_dayparts, args)
  hash <- tools::md5sum(cache_path); modified <- file.info(cache_path)$mtime
  second <- do.call(recovery_ensure_dayparts, args)
  stopifnot(!first$reused, second$reused, identical(hash, tools::md5sum(cache_path)),
    identical(modified, file.info(cache_path)$mtime))
  dd <- first$object$data
  day1 <- dd[dd$Date == as.Date("2020-01-01"), ]
  stopifnot(abs(day1$outdoor_fraction[day1$daypart == "morning"] - 1/3) < 1e-12,
    abs(day1$outdoor_fraction[day1$daypart == "midday"] - .5) < 1e-12,
    day1$environment_hours[day1$daypart == "midday"] == 1,
    dd$environment_hours[dd$Date == as.Date("2020-01-02") & dd$daypart == "morning"] == .5)
  intervals <- data.frame(start = c(0, 1800), end = c(3600, 3600), outdoor = c(1, 0), work = c(1, 0),
    daylight_indoor=0, daylight_outdoor=1, display=0, home=0, vehicle=0)
  conflict <- recovery_diary_part(intervals, 0, 3600)
  stopifnot(conflict[["environment_hours"]] == .5, conflict[["environment_conflict_hours"]] == .5)
  partial <- recovery_weather_part(data.frame(seconds = c(0, 60), ssrd_w_m2 = c(100, 200)), 30, 90)
  stopifnot(partial[["radiation_mean_w_m2"]] == 150)
  # Local civil boundaries remain local across DST; 06..24 is not UTC 06..24.
  dst <- recovery_daypart_bounds(as.Date("2025-03-30"), "Europe/Berlin", "morning")
  stopifnot(format(as.POSIXct(dst[1], origin = "1970-01-01", tz = "UTC"), "%H:%M") == "04:00")
  w$ssrd_w_m2 <- 200; readr::write_csv(w, weather_path)
  third <- do.call(recovery_ensure_dayparts, args)
  stopifnot(!third$reused, all(third$object$data$radiation_mean_w_m2 == 200),
    !identical(hash, tools::md5sum(cache_path)))
  stale <- third$object; stale$provenance$builder_hash <- "old_contract"; saveRDS(stale, cache_path)
  fourth <- do.call(recovery_ensure_dayparts, args)
  stopifnot(!fourth$reused, identical(fourth$object$data, third$object$data))
  corrupt <- fourth$object; corrupt$data$work_fraction[1] <- .9; saveRDS(corrupt, cache_path)
  stopifnot(fails(do.call(recovery_ensure_dayparts, c(args, list(allow_build=FALSE)))))
  repaired <- do.call(recovery_ensure_dayparts, args)
  stopifnot(!repaired$reused, identical(repaired$object$data, fourth$object$data))
})
})
cat("PASS: frozen daily anchors, candidate-only signatures and diary/weather dayparts.\n")
