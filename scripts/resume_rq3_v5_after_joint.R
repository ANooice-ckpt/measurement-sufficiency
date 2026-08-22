suppressPackageStartupMessages(library(tidyverse))
source("scripts/utils/paths.R")
source("scripts/utils/duration_artifacts.R")
source("scripts/utils/rq1_pairwise_artifacts.R")

RQ1_LONG <- file.path("results", "rq1", "rq1_pairwise_change_long.rds")
RQ1_SUMMARY <- file.path("results", "rq1", "rq1_pairwise_summary.csv")
DURATION_CUBE <- file.path("results", "core", "duration_metric_cube.rds")
JOINT_CACHE <- file.path("results", "rq3", "rq3_joint_stability.rds")
SINGLE_CSV <- file.path("results", "rq3", "rq3_observed_stability.csv")
OUT <- file.path("results", "rq3")
DIAG <- file.path("results", "diagnostics")
ensure_result_dirs(OUT, DIAG)
for (p in c(RQ1_LONG, RQ1_SUMMARY, DURATION_CUBE, JOINT_CACHE, SINGLE_CSV)) {
  if (!file.exists(p)) stop("Missing RQ3 recovery input: ", p)
}

pairwise_artifact <- readRDS(RQ1_LONG)
pair_summary <- readr::read_csv(RQ1_SUMMARY, show_col_types = FALSE, progress = FALSE)
duration_artifact <- readRDS(DURATION_CUBE)
if (!duration_cube_is_partitioned(duration_artifact)) stop("RQ3 recovery requires partitioned duration metric cube")
duration_part_paths <- file.path(duration_artifact$part_dir, duration_artifact$parts)
if (any(!file.exists(duration_part_paths))) stop("Missing duration metric cube part")

RQ1_VERSION <- rq1_pairwise_version(pairwise_artifact)
CORE_VERSION <- unique(na.omit(c(pairwise_artifact$core_artifact_version, pair_summary$core_artifact_version)))
if (length(CORE_VERSION) != 1L) stop("Core version mismatch")
CORE_VERSION <- CORE_VERSION[[1]]
RQ3_VERSION <- paste0("rq3_v5_type_level_nested_pareto_fixed__", RQ1_VERSION)
NUMERIC_TOL <- 1e-12
DUAL <- c("MDER", "nvRD")
PRIMARY_TEMPORAL_S <- c(10L, 20L, 30L, 60L, 300L, 900L, 1800L)

circular_delta <- function(a, b, period = 86400) ((a - b + period / 2) %% period) - period / 2
circular_mean <- function(x, period = 86400) {
  x <- x[is.finite(x)]
  if (!length(x)) return(NA_real_)
  th <- 2 * pi * x / period
  (atan2(mean(sin(th)), mean(cos(th))) %% (2 * pi)) * period / (2 * pi)
}
aggregate_scale <- function(x, geometry) {
  x <- x[is.finite(x)]
  if (length(x) < 2L) return(NA_real_)
  if (identical(geometry, "circular_time")) sd(circular_delta(x, circular_mean(x))) else sd(x)
}
temporal_label <- function(x) case_when(
  x < 60L ~ paste0(x, " s"),
  x %% 60L == 0L ~ paste0(x %/% 60L, " min"),
  TRUE ~ paste0(x, " s")
)
metric_support_filter <- function(df) {
  df |> filter((metric %in% DUAL & str_detect(support_id, "_full$")) |
                (!metric %in% DUAL & !str_detect(support_id, "_full$")))
}
read_duration_primary <- function(path) {
  readRDS(path) |>
    filter(resolution_s %in% PRIMARY_TEMPORAL_S) |>
    metric_support_filter() |>
    select(support_id, site, Id, placement, optical, resolution_s, window_id, n_days,
           window_start, window_end, metric, metric_class, metric_geometry, value, available)
}

message("RQ3 recovery: reuse cached nested joint-pair summary")
joint_pair_summary <- readRDS(JOINT_CACHE)
required_joint <- c(
  "support_id", "placement", "optical", "resolution_a", "n_days_a", "config_a_id",
  "resolution_b", "n_days_b", "config_b_id", "metric", "metric_class", "metric_geometry",
  "n_units", "A", "B", "n_participants"
)
missing_joint <- setdiff(required_joint, names(joint_pair_summary))
if (length(missing_joint)) stop("Cached joint summary has incompatible schema: ", paste(missing_joint, collapse = ", "))
if (nrow(joint_pair_summary) && any(joint_pair_summary$A + NUMERIC_TOL < abs(joint_pair_summary$B))) {
  stop("Cached RQ3 joint A >= |B| invariant failed")
}

message("RQ3 recovery: rebuild only anchor/state catalogue over ", length(duration_part_paths), " parts")
anchor_parts <- vector("list", length(duration_part_paths))
state_parts <- vector("list", length(duration_part_paths))
for (i in seq_along(duration_part_paths)) {
  z <- read_duration_primary(duration_part_paths[[i]])
  anchor_parts[[i]] <- z |>
    filter(placement == "eye", optical == "MEDI", resolution_s == 10L, n_days == 6L,
           available, is.finite(value)) |>
    select(support_id, metric, metric_geometry, value)
  state_parts[[i]] <- z |>
    filter(available, is.finite(value)) |>
    distinct(support_id, placement, optical, resolution_s, n_days, metric, metric_class, metric_geometry)
  if (i %% 8L == 0L || i == length(duration_part_paths)) message("  state catalogue ", i, "/", length(duration_part_paths))
  rm(z); invisible(gc(FALSE))
}
joint_anchor <- bind_rows(anchor_parts) |>
  group_by(support_id, metric, metric_geometry) |>
  summarise(standardizer = aggregate_scale(value, first(metric_geometry)), .groups = "drop")
joint_state_catalog <- bind_rows(state_parts) |>
  distinct() |>
  left_join(joint_anchor, by = c("support_id", "metric", "metric_geometry")) |>
  filter(is.finite(standardizer), standardizer > 0) |>
  select(-standardizer) |>
  mutate(config_id = paste0("r", resolution_s, "__d", n_days))
rm(anchor_parts, state_parts)
invisible(gc())

joint_outgoing <- joint_pair_summary |>
  group_by(support_id, placement, optical, resolution_a, n_days_a, config_a_id,
           metric, metric_class, metric_geometry) |>
  summarise(
    n_higher_observed = n_distinct(config_b_id), R_obs = max(A),
    worst_higher_config = config_b_id[which.max(A)], .groups = "drop"
  )

joint <- joint_state_catalog |>
  left_join(
    joint_outgoing,
    by = c("support_id", "placement", "optical", "resolution_s" = "resolution_a",
           "n_days" = "n_days_a", "config_id" = "config_a_id",
           "metric", "metric_class", "metric_geometry")
  ) |>
  mutate(
    status = if_else(coalesce(n_higher_observed, 0L) == 0L, "boundary_unresolved", "resolved"),
    n_higher_observed = coalesce(n_higher_observed, 0L),
    R_obs = if_else(status == "resolved", R_obs, NA_real_),
    joint_configuration = paste(placement, optical, temporal_label(resolution_s), paste0(n_days, " d"), sep = " | "),
    epsilon_entry = R_obs,
    core_artifact_version = CORE_VERSION,
    rq1_analysis_version = RQ1_VERSION,
    rq3_analysis_version = RQ3_VERSION
  )
readr::write_csv(joint, file.path(OUT, "rq3_joint_summary.csv"), na = "")

pareto_at <- function(g, epsilon) {
  z <- g |> filter(status == "resolved", is.finite(R_obs), R_obs <= epsilon + NUMERIC_TOL)
  if (!nrow(z)) return(character())
  keep <- vapply(seq_len(nrow(z)), function(i) {
    !any(
      z$resolution_s >= z$resolution_s[[i]] & z$n_days <= z$n_days[[i]] &
        (z$resolution_s > z$resolution_s[[i]] | z$n_days < z$n_days[[i]])
    )
  }, logical(1))
  z$config_id[keep]
}

message("RQ3 recovery: compute Pareto summaries")
pareto_rows <- list(); pr <- 0L
for (g in joint |> group_by(support_id, placement, optical, metric) |> group_split(.keep = TRUE)) {
  breaks <- sort(unique(c(0, g$R_obs[is.finite(g$R_obs)])))
  if (length(breaks) < 2L) next
  for (i in seq_len(length(breaks) - 1L)) {
    lo <- breaks[[i]]; hi <- breaks[[i + 1L]]; ep <- (lo + hi) / 2
    front <- pareto_at(g, ep)
    pr <- pr + 1L
    pareto_rows[[pr]] <- g |>
      transmute(
        support_id, placement, optical, metric, metric_class, resolution_s, n_days, config_id,
        epsilon_entry = R_obs, epsilon_interval_start = lo, epsilon_interval_end = hi,
        epsilon_interval_width = hi - lo, epsilon_midpoint = ep,
        sufficient = status == "resolved" & is.finite(R_obs) & R_obs <= ep + NUMERIC_TOL,
        pareto = config_id %in% front
      )
  }
}
pareto_occupancy <- bind_rows(pareto_rows)
pareto_summary <- pareto_occupancy |>
  group_by(support_id, placement, optical, metric, metric_class, resolution_s, n_days, config_id, epsilon_entry) |>
  summarise(
    ever_pareto = any(pareto),
    pareto_tolerance_width = sum(epsilon_interval_width[pareto]),
    tolerance_domain_width = sum(epsilon_interval_width),
    pareto_persistence = if (tolerance_domain_width > 0) pareto_tolerance_width / tolerance_domain_width else NA_real_,
    .groups = "drop"
  )
pareto_frequency <- pareto_summary |>
  group_by(support_id, placement, optical, resolution_s, n_days) |>
  summarise(
    n_metrics_available = n_distinct(metric),
    n_metrics_ever_pareto = n_distinct(metric[ever_pareto]),
    fraction_metrics_ever_pareto = n_metrics_ever_pareto / n_metrics_available,
    mean_pareto_persistence = mean(pareto_persistence, na.rm = TRUE),
    .groups = "drop"
  )
readr::write_csv(pareto_occupancy, file.path(OUT, "rq3_pareto_occupancy.csv"), na = "")
readr::write_csv(pareto_summary, file.path(OUT, "rq3_pareto_frontiers.csv"), na = "")
readr::write_csv(pareto_summary, file.path(OUT, "rq3_pareto_ever.csv"), na = "")
readr::write_csv(pareto_frequency, file.path(OUT, "rq3_pareto_frequency.csv"), na = "")

single <- readr::read_csv(SINGLE_CSV, show_col_types = FALSE, progress = FALSE)
boundary_audit <- single |>
  transmute(
    dimension, metric, state_id, n_higher_observed, R_obs, status,
    pass = if_else(n_higher_observed == 0L,
                   is.na(R_obs) & status == "boundary_unresolved",
                   is.finite(R_obs) & status == "resolved")
  )
readr::write_csv(boundary_audit, file.path(DIAG, "rq3_boundary_unresolved_audit.csv"), na = "")
if (any(!boundary_audit$pass)) stop("RQ3 boundary/status invariant failed")

joint_boundary_audit <- joint |>
  transmute(
    support_id, placement, optical, resolution_s, n_days, metric,
    n_higher_observed, R_obs, status,
    pass = if_else(n_higher_observed == 0L,
                   is.na(R_obs) & status == "boundary_unresolved",
                   is.finite(R_obs) & status == "resolved")
  )
readr::write_csv(joint_boundary_audit, file.path(DIAG, "rq3_joint_boundary_unresolved_audit.csv"), na = "")
if (any(!joint_boundary_audit$pass)) stop("RQ3 joint boundary/status invariant failed")

writeLines(c(
  "# RQ3 run report", "",
  paste0("RQ1 upstream: ", RQ1_VERSION),
  paste0("RQ3 analysis version: ", RQ3_VERSION),
  "Recovered after the cached nested joint-pair summary; the expensive Cartesian nested-pair stage was not recomputed.",
  "Joint temporal-duration pairs are actual nested-window comparisons.",
  "Joint A/B and R_obs are aggregated by generic (resolution, duration) configuration type within fixed support x placement x optical facets.",
  "Pareto dominance treats coarser temporal resolution and shorter monitoring duration as lower burden."
), file.path(OUT, "RQ3_RUN_REPORT.md"))

message("RQ3 recovery: build figures")
source("scripts/15_plot_rq3_v5.R", local = .GlobalEnv)
message("RQ3 recovery complete: ", RQ3_VERSION)
