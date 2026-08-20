suppressPackageStartupMessages({
  library(tidyverse)
  library(LightLogR)
})
source("scripts/utils/melidos_io.R")
source("scripts/utils/core_artifacts.R")
source("scripts/utils/protocol_windows.R")
source("scripts/utils/rq_context.R")

# Compact post-core RQ1 extension.
# Whole-day 54-metric distortion remains the primary RQ1 object. This script
# adds one canonical primitive representation (mean log light) conditioned on
# three real-world contexts: photoperiod, indoor/outdoor, and activity.
# It reads cached cleaned support series and never rebuilds or changes core.

RQ1_DISTORTION <- "data/derived/rq1/rq1_distortion_long.rds"
CORE_CONTEXT <- "data/derived/core/unit_context.csv.gz"
OUT_DATA <- "data/derived/rq1"
OUT_RESULTS <- "results/rq1"
OUT_DIAG <- "results/diagnostics"
PRIMARY_TEMPORAL_S <- c(20L, 30L, 60L, 300L, 900L, 1800L)
B_BOOT <- suppressWarnings(as.integer(Sys.getenv("RQ1_BOOT", unset = "1000")))
if (!is.finite(B_BOOT) || B_BOOT < 0L) B_BOOT <- 1000L
BOOT_SEED <- 20260822L

for (p in c(RQ1_DISTORTION, CORE_CONTEXT)) if (!file.exists(p)) stop("Missing required upstream artifact: ", p)
for (d in c(OUT_DATA, OUT_RESULTS, OUT_DIAG)) dir.create(d, recursive = TRUE, showWarnings = FALSE)

rq1 <- readRDS(RQ1_DISTORTION)
rq1_versions <- unique(rq1$rq1_analysis_version); rq1_versions <- rq1_versions[!is.na(rq1_versions)]
core_versions <- unique(rq1$core_artifact_version); core_versions <- core_versions[!is.na(core_versions)]
if (length(rq1_versions) != 1L || length(core_versions) != 1L) stop("RQ1/core version mismatch")
RQ1_VERSION <- rq1_versions[[1]]
CORE_VERSION <- core_versions[[1]]
RQ1_CONTEXT_VERSION <- paste0("rq1_context_v1__", RQ1_VERSION)
SUPPORT_DIR <- file.path("data", "interim", "core", CORE_VERSION, "supports")
if (!dir.exists(SUPPORT_DIR)) stop("Missing cached core support directory: ", SUPPORT_DIR)

context_core <- readr::read_csv(CORE_CONTEXT, show_col_types = FALSE, progress = FALSE) |>
  mutate(Date = as.Date(Date), protocol_start_date = as.Date(protocol_start_date), protocol_end_date = as.Date(protocol_end_date))
site_meta <- core_site_metadata()

# The public MeLiDos light-exposure diary is deliberately a post-core input.
# Rerun scripts/01_download_melidos.R after pulling this update if it is absent.
diary_paths <- setNames(vapply(melidos_sites(), function(s) raw_data_path(s, "lightexposurediary"), character(1)), melidos_sites())
missing_diaries <- diary_paths[!file.exists(diary_paths)]
if (length(missing_diaries)) {
  stop(
    "Missing lightexposurediary raw files for: ", paste(names(missing_diaries), collapse = ", "),
    ". Run Rscript scripts/01_download_melidos.R; no core rebuild is required."
  )
}

annotated_cache <- new.env(parent = emptyenv())
get_annotated_support <- function(site, support_id) {
  key <- paste(site, support_id, sep = "__")
  if (exists(key, envir = annotated_cache, inherits = FALSE)) return(get(key, envir = annotated_cache, inherits = FALSE))
  path <- file.path(SUPPORT_DIR, paste0(key, ".rds"))
  if (!file.exists(path)) return(NULL)
  meta <- site_meta |> filter(.data$site == site)
  if (nrow(meta) != 1L) stop("Missing site metadata for ", site)
  support <- readRDS(path)
  diary <- load_raw_file(diary_paths[[site]], "lightexposurediary")
  out <- rq_context_annotate_support(support, diary, c(meta$latitude, meta$longitude))
  assign(key, out, envir = annotated_cache)
  out
}

pair_daily_context <- function(ann, ref_placement, ref_optical, ref_resolution,
                               can_placement, can_optical, can_resolution,
                               dimension, configuration, configuration_label,
                               configuration_order, comparison_lattice,
                               reference_configuration, support_id) {
  ref <- rq_context_make_series(ann, ref_placement, ref_optical, ref_resolution)
  can <- rq_context_make_series(ann, can_placement, can_optical, can_resolution)
  rq_context_pair_daily(ref, can) |>
    mutate(
      dimension = dimension,
      configuration = configuration,
      configuration_label = configuration_label,
      configuration_order = configuration_order,
      comparison_lattice = comparison_lattice,
      reference_configuration = reference_configuration,
      support_id = support_id,
      analysis_unit_type = "participant_day_context",
      analysis_unit_id = paste(site, Id, Date, context_family, context_state, sep = "|"),
      reference_unit_id = analysis_unit_id,
      reference_id = NA_character_,
      window_id = NA_character_, window_index = NA_integer_, n_days = NA_integer_
    ) |>
    select(
      dimension, configuration, configuration_label, configuration_order,
      comparison_lattice, reference_configuration, support_id,
      site, Id, Date, analysis_unit_type, analysis_unit_id, reference_unit_id,
      reference_id, window_id, window_index, n_days,
      context_family, context_state,
      reference_n_observations, candidate_n_observations,
      reference_value, candidate_value, delta_native
    )
}

message("RQ1 context: placement, optical, temporal primitive representations")
blocks <- list(); b <- 0L
for (site_i in melidos_sites()) {
  if (site_i != "MPI") {
    for (pos in c("chest", "wrist")) {
      sid <- paste0("eye_", pos, "_medi")
      ann <- get_annotated_support(site_i, sid)
      if (!is.null(ann)) {
        b <- b + 1L
        blocks[[b]] <- pair_daily_context(
          ann, "eye", "MEDI", 10L, pos, "MEDI", 10L,
          "placement", pos, stringr::str_to_title(pos), match(pos, c("chest", "wrist")),
          paste0("placement_", pos), "Eye MEDI, 10 s", sid
        )
      }
    }
  }

  ann_full <- get_annotated_support(site_i, "eye_full")
  if (!is.null(ann_full)) {
    b <- b + 1L
    blocks[[b]] <- pair_daily_context(
      ann_full, "eye", "MEDI", 10L, "eye", "LIGHT", 10L,
      "optical", "LIGHT", "Photopic illuminance", 1L,
      "optical", "Eye MEDI, 10 s", "eye_full"
    )
  }

  ann_eye <- get_annotated_support(site_i, "eye_medi")
  if (!is.null(ann_eye)) {
    for (j in seq_along(PRIMARY_TEMPORAL_S)) {
      r <- PRIMARY_TEMPORAL_S[[j]]
      label <- if (r < 60L) paste0(r, " s") else paste0(r %/% 60L, " min")
      code <- if (r < 60L) paste0(r, "s") else paste0(r %/% 60L, "min")
      b <- b + 1L
      blocks[[b]] <- pair_daily_context(
        ann_eye, "eye", "MEDI", 10L, "eye", "MEDI", r,
        "temporal", code, label, j,
        "temporal", "Eye MEDI, 10 s", "eye_medi"
      )
    }
  }
}

raw_pairs <- bind_rows(blocks)
if (!nrow(raw_pairs)) stop("No context-conditioned placement/optical/temporal rows were produced")

message("RQ1 context: protocol-anchored duration primitive representations")
duration_context <- context_core |>
  filter(support_id == "eye_medi", placement == "eye", optical == "MEDI", resolution_s == 10L) |>
  distinct(support_id, site, Id, Date, .keep_all = TRUE)
duration_cohort <- protocol_reference_cohort(duration_context)
eligible_duration <- duration_cohort |> filter(eligible_protocol_7)
duration_windows <- make_protocol_duration_windows(eligible_duration, include_reference = FALSE)

# Build one compact per-day sufficient statistic for the canonical context
# representation, then aggregate those sums/counts over each protocol window.
duration_daily_context <- map_dfr(melidos_sites(), function(site_i) {
  ann <- get_annotated_support(site_i, "eye_medi")
  if (is.null(ann)) return(tibble())
  rq_context_daily_stats(rq_context_make_series(ann, "eye", "MEDI", 10L))
})

duration_blocks <- vector("list", nrow(duration_windows))
for (i in seq_len(nrow(duration_windows))) {
  w <- duration_windows[i, ]
  pdat <- duration_daily_context |> filter(site == w$site, Id == w$Id)
  if (!nrow(pdat)) next
  ref <- rq_context_aggregate_dates(pdat, w$reference_dates[[1]], "reference")
  can <- rq_context_aggregate_dates(pdat, w$selected_dates[[1]], "candidate")
  duration_blocks[[i]] <- inner_join(can, ref, by = c("site", "Id", "context_family", "context_state")) |>
    mutate(
      delta_native = candidate_value - reference_value,
      dimension = "duration",
      configuration = paste0(w$n_days, "d"),
      configuration_label = paste0(w$n_days, " d"),
      configuration_order = 7L - w$n_days,
      comparison_lattice = "duration",
      reference_configuration = "7 protocol-anchored days",
      support_id = "eye_medi",
      Date = as.Date(NA),
      analysis_unit_type = "participant_window_context",
      analysis_unit_id = paste(w$window_id, context_family, context_state, sep = "|"),
      reference_unit_id = paste(w$reference_id, context_family, context_state, sep = "|"),
      reference_id = w$reference_id,
      window_id = w$window_id,
      window_index = w$window_index,
      n_days = w$n_days
    ) |>
    rename(
      reference_n_observations = reference_n_observations,
      candidate_n_observations = candidate_n_observations
    ) |>
    select(
      dimension, configuration, configuration_label, configuration_order,
      comparison_lattice, reference_configuration, support_id,
      site, Id, Date, analysis_unit_type, analysis_unit_id, reference_unit_id,
      reference_id, window_id, window_index, n_days,
      context_family, context_state,
      reference_n_observations, candidate_n_observations,
      reference_value, candidate_value, delta_native
    )
}
raw_pairs <- bind_rows(raw_pairs, bind_rows(duration_blocks)) |>
  mutate(
    context_family = as.character(context_family),
    context_state = tolower(as.character(context_state))
  ) |>
  filter(
    (context_family == "photoperiod" & context_state %in% c("day", "night")) |
      (context_family == "environment" & context_state %in% c("indoor", "outdoor")) |
      (context_family == "activity" & context_state %in% c("home", "working", "vehicle", "outdoors"))
  )

# One fixed reference scale per comparison lattice and context family. Context
# states share the denominator so day/night and indoor/outdoor are directly
# comparable; no state-specific renormalisation is allowed.
reference_basis <- raw_pairs |>
  filter(is.finite(reference_value)) |>
  distinct(comparison_lattice, context_family, site, Id, reference_unit_id, context_state, reference_value)
standardizers <- reference_basis |>
  group_by(comparison_lattice, context_family) |>
  summarise(
    n_reference_units = n(),
    standardizer = sd(reference_value),
    .groups = "drop"
  ) |>
  mutate(zero_or_undefined = !is.finite(standardizer) | standardizer <= sqrt(.Machine$double.eps))
readr::write_csv(standardizers, file.path(OUT_DIAG, "rq1_context_standardizer_audit.csv"), na = "")

canonical <- raw_pairs |>
  left_join(standardizers, by = c("comparison_lattice", "context_family")) |>
  mutate(
    available = is.finite(reference_value) & is.finite(candidate_value) & is.finite(delta_native) &
      !replace_na(zero_or_undefined, TRUE),
    e = if_else(available, delta_native / standardizer, NA_real_),
    abs_e = abs(e),
    metric = "mean_log_light",
    metric_class = "level",
    core_artifact_version = CORE_VERSION,
    rq1_analysis_version = RQ1_VERSION,
    rq1_context_analysis_version = RQ1_CONTEXT_VERSION
  ) |>
  select(
    core_artifact_version, rq1_analysis_version, rq1_context_analysis_version,
    dimension, configuration, configuration_label, configuration_order,
    comparison_lattice, reference_configuration, support_id,
    site, Id, Date, analysis_unit_type, analysis_unit_id, reference_unit_id,
    reference_id, window_id, window_index, n_days,
    context_family, context_state, metric, metric_class,
    reference_n_observations, candidate_n_observations,
    reference_value, candidate_value, delta_native, standardizer,
    e, abs_e, available
  )
saveRDS(canonical, file.path(OUT_DATA, "rq1_context_distortion_long.rds"), compress = "xz")

safe_q <- rq_context_safe_quantile
bootstrap_ci <- function(g, B = B_BOOT) {
  clusters <- g |> group_by(site, Id) |> summarise(sum_e = sum(e), sum_abs_e = sum(abs_e), n = n(), .groups = "drop")
  site_counts <- clusters |> count(site, name = "n_participants")
  supported <- B > 0L && nrow(clusters) >= 2L && any(site_counts$n_participants > 1L)
  if (!supported) return(tibble(bootstrap_supported = FALSE, B_ci_low = NA_real_, B_ci_high = NA_real_, A_ci_low = NA_real_, A_ci_high = NA_real_))
  by_site <- split(clusters, clusters$site)
  vals <- replicate(B, {
    sampled <- map_dfr(by_site, ~.x[sample.int(nrow(.x), nrow(.x), replace = TRUE), , drop = FALSE])
    c(B = sum(sampled$sum_e) / sum(sampled$n), A = sum(sampled$sum_abs_e) / sum(sampled$n))
  })
  tibble(
    bootstrap_supported = TRUE,
    B_ci_low = safe_q(vals["B", ], .025), B_ci_high = safe_q(vals["B", ], .975),
    A_ci_low = safe_q(vals["A", ], .025), A_ci_high = safe_q(vals["A", ], .975)
  )
}

group_vars <- c(
  "dimension", "configuration", "configuration_label", "configuration_order",
  "comparison_lattice", "context_family", "context_state", "metric", "metric_class"
)
x <- canonical |> filter(available, is.finite(e))
summary_base <- x |>
  group_by(across(all_of(group_vars))) |>
  summarise(
    n_participants = n_distinct(paste(site, Id, sep = "|")),
    n_units = n(),
    median_e = median(e), q25_e = safe_q(e, .25), q75_e = safe_q(e, .75),
    p025_e = safe_q(e, .025), p975_e = safe_q(e, .975),
    B_mean_signed = mean(e), A_mean_absolute = mean(abs_e),
    .groups = "drop"
  )
set.seed(BOOT_SEED)
cis <- x |> group_by(across(all_of(group_vars))) |> group_modify(~bootstrap_ci(.x, B_BOOT)) |> ungroup()
context_summary <- summary_base |>
  left_join(cis, by = group_vars) |>
  mutate(
    core_artifact_version = CORE_VERSION,
    rq1_analysis_version = RQ1_VERSION,
    rq1_context_analysis_version = RQ1_CONTEXT_VERSION
  )
readr::write_csv(context_summary, file.path(OUT_RESULTS, "rq1_context_summary.csv"), na = "")

coverage <- canonical |>
  group_by(dimension, configuration, context_family, context_state, site) |>
  summarise(
    n_participants = n_distinct(Id), n_units = n(), n_available = sum(available),
    reference_observations = sum(reference_n_observations, na.rm = TRUE),
    candidate_observations = sum(candidate_n_observations, na.rm = TRUE),
    .groups = "drop"
  )
readr::write_csv(coverage, file.path(OUT_DIAG, "rq1_context_coverage.csv"), na = "")

geometry_audit <- context_summary |>
  transmute(
    dimension, configuration, context_family, context_state,
    A_mean_absolute, B_mean_signed,
    pass = A_mean_absolute + 1e-12 >= abs(B_mean_signed)
  )
if (nrow(geometry_audit) && any(!geometry_audit$pass)) stop("Context A >= |B| invariant failed")
readr::write_csv(geometry_audit, file.path(OUT_DIAG, "rq1_context_geometry_invariant.csv"), na = "")

writeLines(c(
  "# RQ1 context run report", "",
  paste0("Core artifact version: ", CORE_VERSION),
  paste0("RQ1 upstream version: ", RQ1_VERSION),
  paste0("RQ1 context version: ", RQ1_CONTEXT_VERSION),
  paste0("Canonical context rows: ", nrow(canonical)),
  paste0("Available standardized rows: ", nrow(x)),
  "Context representation: mean LightLogR zero-inflated log light on the smallest valid context unit.",
  "Contexts: civil photoperiod (day/night), diary-derived environment (indoor/outdoor), diary-derived activity (home/working/vehicle/outdoors).",
  "The reference scale is fixed within comparison lattice x context family and shared across context states.",
  "This script reads cached support artifacts only; it does not rebuild core."
), file.path(OUT_RESULTS, "RQ1_CONTEXT_RUN_REPORT.md"))

message("RQ1 context complete: ", RQ1_CONTEXT_VERSION)
message("  ", file.path(OUT_DATA, "rq1_context_distortion_long.rds"))
message("  ", file.path(OUT_RESULTS, "rq1_context_summary.csv"))
