suppressPackageStartupMessages(library(tidyverse))
source("scripts/utils/analysis_design.R")
source("scripts/utils/paths.R")
source("scripts/utils/parallel_runtime.R")
source("scripts/utils/duration_artifacts.R")
source("scripts/utils/rq1_pairwise_artifacts.R")

# Corrected RQ3 implementation.
# - single-dimension R_obs is defined on configuration TYPES from the frozen
#   RQ1 pairwise summary, not repeated participant/window identities;
# - unresolved upper boundaries are not treated as insufficient when checking
#   threshold-like sufficient sets;
# - joint temporal x duration comparisons require actual nested windows;
# - joint A/B and R_obs are aggregated by joint configuration TYPE;
# - Pareto dominance uses the correct burden direction: coarser temporal
#   resolution and shorter duration are less demanding.

RQ1_LONG <- file.path("results", "rq1", "rq1_pairwise_change_long.rds")
RQ1_SUMMARY <- file.path("results", "rq1", "rq1_pairwise_summary.csv")
LOCAL <- file.path("results", "rq1", "rq1_local_transition_summary.csv")
DURATION_CUBE <- file.path("results", "core", "duration_metric_cube.rds")
OUT <- file.path("results", "rq3")
DIAG <- file.path("results", "diagnostics")
RQ3_WORKERS <- ms_resolve_workers("RQ3_WORKERS", default = 1L, cap = 48L)
RQ3_PART_WORKERS <- ms_resolve_workers("RQ3_PART_WORKERS", default = min(8L, RQ3_WORKERS), cap = 12L)
ensure_result_dirs(OUT, DIAG)
for (p in c(RQ1_LONG, RQ1_SUMMARY, LOCAL, DURATION_CUBE)) if (!file.exists(p)) stop("Missing RQ3 input: ", p)

PRIMARY_TEMPORAL_S <- ms_primary_temporal_s()
PRIMARY_DURATION_DAYS <- ms_primary_duration_days()
ANALYSIS_DESIGN_ID <- ms_analysis_design_id()

pairwise_artifact <- readRDS(RQ1_LONG)
pair_summary <- readr::read_csv(RQ1_SUMMARY, show_col_types = FALSE, progress = FALSE)
local <- readr::read_csv(LOCAL, show_col_types = FALSE, progress = FALSE)
duration_artifact <- readRDS(DURATION_CUBE)
if (!duration_cube_is_partitioned(duration_artifact)) stop("RQ3 v5 requires partitioned duration metric cube")
duration_part_paths <- file.path(duration_artifact$part_dir, duration_artifact$parts)
if (any(!file.exists(duration_part_paths))) stop("Missing duration metric cube part")

RQ1_VERSION <- rq1_pairwise_version(pairwise_artifact)
if (!is.null(pairwise_artifact$analysis_design_id) && !identical(as.character(pairwise_artifact$analysis_design_id[[1]]), ANALYSIS_DESIGN_ID)) {
  stop("RQ1 artifact analysis design does not match current frozen design")
}
CORE_VERSION <- unique(na.omit(c(pairwise_artifact$core_artifact_version, pair_summary$core_artifact_version)))
if (length(CORE_VERSION) != 1L) stop("Core version mismatch")
CORE_VERSION <- CORE_VERSION[[1]]
RQ3_VERSION <- paste0("rq3_v5_type_level_nested_pareto_fixed__", RQ1_VERSION, "__", ANALYSIS_DESIGN_ID)
NUMERIC_TOL <- 1e-12
DUAL <- c("MDER", "nvRD")

safe_q <- function(x, p) { x <- x[is.finite(x)]; if (length(x)) unname(quantile(x, p, names = FALSE)) else NA_real_ }
circular_delta <- function(a, b, period = 86400) ((a - b + period / 2) %% period) - period / 2
circular_mean <- function(x, period = 86400) {
  x <- x[is.finite(x)]; if (!length(x)) return(NA_real_)
  th <- 2 * pi * x / period
  (atan2(mean(sin(th)), mean(cos(th))) %% (2 * pi)) * period / (2 * pi)
}
aggregate_scale <- function(x, geometry) {
  x <- x[is.finite(x)]; if (length(x) < 2L) return(NA_real_)
  if (identical(geometry, "circular_time")) sd(circular_delta(x, circular_mean(x))) else sd(x)
}
temporal_label <- ms_temporal_label
metric_support_filter <- function(df) {
  df |> filter((metric %in% DUAL & str_detect(support_id, "_full$")) |
                (!metric %in% DUAL & !str_detect(support_id, "_full$")))
}

# -----------------------------------------------------------------------------
# Single-dimension observed residual instability on configuration TYPES.
# -----------------------------------------------------------------------------
ordered_pairs <- pair_summary |>
  filter(dimension %in% c("temporal", "duration"), is.finite(A_mean_absolute))
if (any(ordered_pairs$dimension == "duration" & grepl("__to__", ordered_pairs$comparison_pair_id, fixed = TRUE))) {
  stop("Concrete duration window ids leaked into RQ3 single-dimension summary")
}

state_catalog <- bind_rows(
  ordered_pairs |>
    transmute(dimension, comparison_lattice, metric, metric_class, metric_geometry,
              state_id = config_a_id, state_label = config_a_label),
  ordered_pairs |>
    transmute(dimension, comparison_lattice, metric, metric_class, metric_geometry,
              state_id = config_b_id, state_label = config_b_label)
) |>
  distinct() |>
  mutate(
    requirement_rank = case_when(
      dimension == "temporal" ~ match(state_label, temporal_label(rev(PRIMARY_TEMPORAL_S))),
      dimension == "duration" ~ suppressWarnings(as.integer(str_extract(state_label, "^\\d+"))),
      TRUE ~ NA_integer_
    )
  )
if (any(!is.finite(state_catalog$requirement_rank))) stop("RQ3 could not rank an ordered configuration state")

single <- state_catalog |>
  group_by(dimension, comparison_lattice, metric, metric_class, metric_geometry) |>
  group_modify(function(states, key) {
    states <- states |> arrange(requirement_rank)
    p <- ordered_pairs |>
      filter(dimension == key$dimension[[1]], comparison_lattice == key$comparison_lattice[[1]],
             metric == key$metric[[1]])
    map_dfr(seq_len(nrow(states)), function(i) {
      c0 <- states$state_id[[i]]
      higher <- p |> filter(config_a_id == c0, is.finite(A_mean_absolute))
      n_higher <- n_distinct(higher$config_b_id)
      if (!n_higher) {
        tibble(
          state_id = c0, state_label = states$state_label[[i]], requirement_rank = states$requirement_rank[[i]],
          n_higher_observed = 0L, worst_refinement_config = NA_character_, R_obs = NA_real_,
          status = "boundary_unresolved", boundary = TRUE
        )
      } else {
        w <- higher |> slice_max(A_mean_absolute, n = 1L, with_ties = FALSE)
        tibble(
          state_id = c0, state_label = states$state_label[[i]], requirement_rank = states$requirement_rank[[i]],
          n_higher_observed = n_higher, worst_refinement_config = w$config_b_id[[1]],
          R_obs = w$A_mean_absolute[[1]], status = "resolved", boundary = FALSE
        )
      }
    })
  }) |>
  ungroup() |>
  mutate(core_artifact_version = CORE_VERSION, rq1_analysis_version = RQ1_VERSION,
         rq3_analysis_version = RQ3_VERSION)
readr::write_csv(single, file.path(OUT, "rq3_observed_stability.csv"), na = "")
saveRDS(single, file.path(OUT, "rq3_sufficiency_long.rds"), compress = "xz")

# Epsilon breakpoints are metric/dimension specific. This avoids a global
# Cartesian product with irrelevant tolerances and keeps the result compact.
sufficiency_long <- single |>
  group_by(dimension, comparison_lattice, metric, metric_class, metric_geometry) |>
  group_modify(function(g, key) {
    eps <- sort(unique(c(0, g$R_obs[is.finite(g$R_obs)])))
    tidyr::crossing(g, epsilon = eps) |>
      mutate(sufficient = status == "resolved" & is.finite(R_obs) & R_obs <= epsilon + NUMERIC_TOL)
  }) |>
  ungroup()
readr::write_csv(sufficiency_long, file.path(OUT, "rq3_sufficiency_long.csv"), na = "")

single_requirement <- sufficiency_long |>
  group_by(dimension, metric, epsilon) |>
  group_modify(function(z, key) {
    resolved <- z |> filter(status == "resolved") |> arrange(requirement_rank)
    ok <- resolved$sufficient
    threshold_like <- length(ok) < 2L || all(diff(as.integer(ok)) >= 0L)
    least <- if (threshold_like && any(ok)) {
      resolved$state_id[which.min(ifelse(ok, resolved$requirement_rank, Inf))]
    } else NA_character_
    tibble(
      n_resolved_states = nrow(resolved), sufficient_states = sum(ok),
      sufficient_set_threshold_like = threshold_like,
      least_demanding_sufficient_state = least
    )
  }) |>
  ungroup()
readr::write_csv(single_requirement, file.path(OUT, "rq3_single_dimension_requirement.csv"), na = "")

# Placement/optical remain target-aligned substitutability facets, not burden axes.
unordered <- pair_summary |>
  filter(dimension %in% c("placement", "optical")) |>
  transmute(
    dimension, comparison_pair_id, config_a_id, config_b_id, config_a_label, config_b_label,
    orientation_type, orientation_basis, metric, metric_class, metric_geometry,
    epsilon_entry = A_mean_absolute, A = A_mean_absolute, B = B_mean_signed,
    substitutable_at_epsilon_zero = is.finite(epsilon_entry) & epsilon_entry <= 0 + NUMERIC_TOL,
    core_artifact_version, rq1_analysis_version, rq3_analysis_version = RQ3_VERSION
  )
readr::write_csv(unordered, file.path(OUT, "rq3_unordered_substitutability.csv"), na = "")
coverage <- unordered |>
  group_by(dimension, comparison_pair_id, config_a_id, config_b_id, metric_class) |>
  group_modify(function(g, key) {
    eps <- sort(unique(c(0, g$epsilon_entry[is.finite(g$epsilon_entry)])))
    tibble(
      epsilon = eps, n_metrics = n_distinct(g$metric),
      fraction_metrics_substitutable = map_dbl(eps, function(e) mean(g$epsilon_entry <= e + NUMERIC_TOL, na.rm = TRUE))
    )
  }) |>
  ungroup()
readr::write_csv(coverage, file.path(OUT, "rq3_unordered_coverage_curves.csv"), na = "")

# Adjacent G sequence. Requirement rank increases with burden in BOTH ordered axes.
convergence <- local |>
  filter(dimension %in% c("temporal", "duration")) |>
  mutate(
    requirement_position = case_when(
      dimension == "duration" ~ suppressWarnings(as.integer(str_extract(lower_level, "^\\d+"))),
      dimension == "temporal" ~ match(as.character(lower_level), temporal_label(rev(PRIMARY_TEMPORAL_S))),
      TRUE ~ NA_integer_
    ),
    boundary_proximity = case_when(
      dimension == "duration" ~ requirement_position / max(PRIMARY_DURATION_DAYS),
      dimension == "temporal" ~ requirement_position / length(PRIMARY_TEMPORAL_S),
      TRUE ~ NA_real_
    ),
    G = as.numeric(G), rq3_analysis_version = RQ3_VERSION
  )
readr::write_csv(convergence, file.path(OUT, "rq3_convergence_profile.csv"), na = "")

# -----------------------------------------------------------------------------
# Joint temporal x duration stability, streamed over duration-cube parts.
# -----------------------------------------------------------------------------
read_duration_primary <- function(path) {
  readRDS(path) |>
    filter(resolution_s %in% PRIMARY_TEMPORAL_S) |>
    metric_support_filter() |>
    select(support_id, site, Id, placement, optical, resolution_s, window_id, n_days,
           window_start, window_end, metric, metric_class, metric_geometry, value, available)
}

message("RQ3 v5: collect joint anchor values and observed joint-state catalogue")
anchor_parts <- vector("list", length(duration_part_paths))
state_parts <- vector("list", length(duration_part_paths))
for (i in seq_along(duration_part_paths)) {
  z <- read_duration_primary(duration_part_paths[[i]])
  anchor_parts[[i]] <- z |>
    filter(resolution_s == 10L, n_days == 6L, available, is.finite(value)) |>
    select(support_id, metric, metric_geometry, value)
  state_parts[[i]] <- z |>
    distinct(support_id, placement, optical, resolution_s, n_days, metric, metric_class, metric_geometry)
  rm(z); invisible(gc(FALSE))
}
joint_anchor <- bind_rows(anchor_parts) |>
  group_by(support_id, metric, metric_geometry) |>
  summarise(standardizer = aggregate_scale(value, first(metric_geometry)), .groups = "drop")
joint_state_catalog <- bind_rows(state_parts) |>
  distinct() |>
  mutate(config_id = paste0("r", resolution_s, "__d", n_days))
rm(anchor_parts, state_parts)
invisible(gc())

build_joint_part <- function(path) {
  z <- read_duration_primary(path)
  groups <- z |> group_by(support_id, site, Id, placement, optical, metric) |> group_split(.keep = TRUE)
  partial <- bind_rows(lapply(groups, function(g) {
    if (nrow(g) < 2L) return(tibble())
    a <- g |>
      select(support_id, site, Id, placement, optical, metric, metric_class, metric_geometry,
             resolution_a = resolution_s, n_days_a = n_days, window_a = window_id,
             start_a = window_start, end_a = window_end, value_a = value, available_a = available)
    b <- g |>
      select(support_id, site, Id, resolution_b = resolution_s, n_days_b = n_days, window_b = window_id,
             start_b = window_start, end_b = window_end, value_b = value, available_b = available)
    merge(a, b, by = c("support_id", "site", "Id"), allow.cartesian = TRUE) |>
      filter(
        resolution_b <= resolution_a, n_days_b >= n_days_a,
        resolution_b < resolution_a | n_days_b > n_days_a,
        start_b <= start_a, end_a <= end_b
      ) |>
      left_join(joint_anchor, by = c("support_id", "metric", "metric_geometry")) |>
      mutate(
        delta = if_else(metric_geometry == "circular_time", circular_delta(value_b, value_a), value_b - value_a),
        available = coalesce(available_a, FALSE) & coalesce(available_b, FALSE) &
          is.finite(delta) & is.finite(standardizer) & standardizer > 0,
        z = if_else(available, delta / standardizer, NA_real_),
        config_a_id = paste0("r", resolution_a, "__d", n_days_a),
        config_b_id = paste0("r", resolution_b, "__d", n_days_b),
        participant_key = paste(site, Id, sep = "|")
      ) |>
      filter(available, is.finite(z)) |>
      group_by(support_id, placement, optical, resolution_a, n_days_a, config_a_id,
               resolution_b, n_days_b, config_b_id, metric, metric_class, metric_geometry) |>
      summarise(
        n_units = n(), sum_abs = sum(abs(z)), sum_signed = sum(z),
        participant_keys = list(unique(participant_key)), .groups = "drop"
      )
  }))
  rm(z, groups); invisible(gc(FALSE))
  partial
}

message("RQ3 v5: build nested joint-pair partial summaries over ", length(duration_part_paths),
        " parts; workers=", RQ3_PART_WORKERS)
cost <- as.numeric(file.info(duration_part_paths)$size)
ord <- order(cost, decreasing = TRUE, na.last = TRUE)
partials_scheduled <- ms_parallel_map(
  duration_part_paths[ord], build_joint_part, workers = RQ3_PART_WORKERS,
  packages = "tidyverse",
  exports = c("build_joint_part", "read_duration_primary", "metric_support_filter", "PRIMARY_TEMPORAL_S",
              "DUAL", "joint_anchor", "circular_delta")
)
partials <- partials_scheduled[order(ord)]
joint_pair_summary <- bind_rows(partials) |>
  group_by(support_id, placement, optical, resolution_a, n_days_a, config_a_id,
           resolution_b, n_days_b, config_b_id, metric, metric_class, metric_geometry) |>
  summarise(
    n_units = sum(n_units), A = sum(sum_abs) / sum(n_units), B = sum(sum_signed) / sum(n_units),
    n_participants = n_distinct(unlist(participant_keys, use.names = FALSE)), .groups = "drop"
  )
rm(partials, partials_scheduled)
invisible(gc())
if (nrow(joint_pair_summary) && any(joint_pair_summary$A + NUMERIC_TOL < abs(joint_pair_summary$B))) {
  stop("RQ3 joint A >= |B| invariant failed")
}
readr::write_csv(joint_pair_summary, file.path(OUT, "rq3_joint_pair_summary.csv"), na = "")
saveRDS(joint_pair_summary, file.path(OUT, "rq3_joint_stability.rds"), compress = "xz")

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
           "n_days" = "n_days_a", "config_id", "metric", "metric_class", "metric_geometry")
  ) |>
  mutate(
    status = if_else(coalesce(n_higher_observed, 0L) == 0L, "boundary_unresolved", "resolved"),
    n_higher_observed = coalesce(n_higher_observed, 0L),
    R_obs = if_else(status == "resolved", R_obs, NA_real_),
    joint_configuration = paste(placement, optical, temporal_label(resolution_s), paste0(n_days, " d"), sep = " | "),
    epsilon_entry = R_obs, core_artifact_version = CORE_VERSION,
    rq1_analysis_version = RQ1_VERSION, rq3_analysis_version = RQ3_VERSION
  )
readr::write_csv(joint, file.path(OUT, "rq3_joint_summary.csv"), na = "")

pareto_at <- function(g, epsilon) {
  z <- g |> filter(status == "resolved", is.finite(R_obs), R_obs <= epsilon + NUMERIC_TOL)
  if (!nrow(z)) return(character())
  keep <- vapply(seq_len(nrow(z)), function(i) {
    # A lower-burden sufficient competitor is coarser/equal in time
    # (numerically larger seconds) and shorter/equal in duration.
    !any(
      z$resolution_s >= z$resolution_s[[i]] & z$n_days <= z$n_days[[i]] &
        (z$resolution_s > z$resolution_s[[i]] | z$n_days < z$n_days[[i]])
    )
  }, logical(1))
  z$config_id[keep]
}

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
    mean_pareto_persistence = mean(pareto_persistence, na.rm = TRUE), .groups = "drop"
  )
readr::write_csv(pareto_occupancy, file.path(OUT, "rq3_pareto_occupancy.csv"), na = "")
readr::write_csv(pareto_summary, file.path(OUT, "rq3_pareto_frontiers.csv"), na = "")
readr::write_csv(pareto_summary, file.path(OUT, "rq3_pareto_ever.csv"), na = "")
readr::write_csv(pareto_frequency, file.path(OUT, "rq3_pareto_frequency.csv"), na = "")

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
  transmute(support_id, placement, optical, resolution_s, n_days, metric,
            n_higher_observed, R_obs, status,
            pass = if_else(n_higher_observed == 0L,
                           is.na(R_obs) & status == "boundary_unresolved",
                           is.finite(R_obs) & status == "resolved"))
readr::write_csv(joint_boundary_audit, file.path(DIAG, "rq3_joint_boundary_unresolved_audit.csv"), na = "")
if (any(!joint_boundary_audit$pass)) stop("RQ3 joint boundary/status invariant failed")

writeLines(c(
  "# RQ3 run report", "",
  paste0("RQ1 upstream: ", RQ1_VERSION), paste0("RQ3 analysis version: ", RQ3_VERSION),
  paste0("Analysis design: ", ANALYSIS_DESIGN_ID),
  paste0("Primary temporal states: ", paste(PRIMARY_TEMPORAL_S, collapse = ", "), " s."),
  "Single-dimension R_obs is computed on temporal/duration configuration TYPES from RQ1 pair summaries.",
  "Threshold-like sufficient-set checks exclude unresolved boundaries and require false->true monotone ordering with increasing burden.",
  "Least-demanding sufficient state is the minimum-burden sufficient state when and only when the resolved sufficient set is threshold-like.",
  "Joint temporal-duration pairs are actual nested-window comparisons; equal duration implies the same observed dates.",
  "Joint A/B and R_obs are aggregated by generic (resolution, duration) configuration type within fixed support x placement x optical facets.",
  "Pareto dominance treats coarser temporal resolution and shorter monitoring duration as lower burden inside the sufficient region.",
  paste0("Joint duration-part workers: ", RQ3_PART_WORKERS)
), file.path(OUT, "RQ3_RUN_REPORT.md"))
message("RQ3 complete: ", RQ3_VERSION)
