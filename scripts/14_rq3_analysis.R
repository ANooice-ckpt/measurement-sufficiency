suppressPackageStartupMessages(library(tidyverse))
source("scripts/utils/paths.R")
source("scripts/utils/parallel_runtime.R")
source("scripts/utils/duration_artifacts.R")
source("scripts/utils/rq1_pairwise_artifacts.R")

# RQ3 is a projection of observed pairwise change. Temporal and duration use
# observed residual instability along measurement-requirement directions;
# placement/optical use target-aligned substitutability without a burden order.
RQ1_LONG <- file.path("results", "rq1", "rq1_pairwise_change_long.rds")
RQ1_SUMMARY <- file.path("results", "rq1", "rq1_pairwise_summary.csv")
LOCAL <- file.path("results", "rq1", "rq1_local_transition_summary.csv")
CORE_METRICS <- file.path("results", "core", "metric_cube.csv.gz")
DURATION_CUBE <- file.path("results", "core", "duration_metric_cube.rds")
OUT <- file.path("results", "rq3")
DIAG <- file.path("results", "diagnostics")
RQ3_WORKERS <- ms_resolve_workers("RQ3_WORKERS", default = 1L, cap = 48L)
ensure_result_dirs(OUT, DIAG)
for (p in c(RQ1_LONG, RQ1_SUMMARY, LOCAL, CORE_METRICS, DURATION_CUBE)) {
  if (!file.exists(p)) stop("Missing RQ3 input: ", p)
}

pairwise_artifact <- readRDS(RQ1_LONG)
pair_summary <- readr::read_csv(RQ1_SUMMARY, show_col_types = FALSE, progress = FALSE)
local <- readr::read_csv(LOCAL, show_col_types = FALSE, progress = FALSE)
cube <- readr::read_csv(CORE_METRICS, show_col_types = FALSE, progress = FALSE) |>
  mutate(Date = as.Date(Date))
duration_cube <- load_duration_metric_cube(readRDS(DURATION_CUBE), columns = c(
  "support_id", "site", "Id", "window_id", "n_days", "window_start", "window_end", "placement",
  "optical", "resolution_s", "config_id", "metric", "metric_class", "metric_geometry", "value", "available"
))
RQ1_VERSION <- rq1_pairwise_version(pairwise_artifact)
CORE_VERSION <- unique(na.omit(c(pairwise_artifact$core_artifact_version, pair_summary$core_artifact_version)))
if (length(CORE_VERSION) != 1L) stop("Core version mismatch")
CORE_VERSION <- CORE_VERSION[[1]]
RQ3_VERSION <- paste0("rq3_v4_observed_stability__", RQ1_VERSION)
NUMERIC_TOL <- 1e-12
DUAL <- c("MDER", "nvRD")
PRIMARY_TEMPORAL_S <- c(10L, 20L, 30L, 60L, 300L, 900L, 1800L)

temporal_label <- function(x) case_when(
  x < 60L ~ paste0(x, " s"),
  x %% 60L == 0L ~ paste0(x %/% 60L, " min"),
  TRUE ~ paste0(x, " s")
)
safe_q <- function(x, p) {
  x <- x[is.finite(x)]
  if (length(x)) unname(quantile(x, p, names = FALSE)) else NA_real_
}
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
  if (geometry == "circular_time") sd(circular_delta(x, circular_mean(x))) else sd(x)
}

metric_support_filter <- function(df) {
  df |> filter((metric %in% DUAL & str_detect(support_id, "_full$")) |
                 (!metric %in% DUAL & !str_detect(support_id, "_full$")))
}

# -------------------------------------------------------------------------
# Single-dimension observed residual instability
# -------------------------------------------------------------------------
temporal_states <- cube |>
  filter(analysis_unit_type == "participant_day", placement == "eye", optical == "MEDI",
         resolution_s %in% PRIMARY_TEMPORAL_S) |>
  metric_support_filter() |>
  distinct(support_id, site, Id, metric, metric_class, metric_geometry, config_id, resolution_s) |>
  mutate(
    dimension = "temporal", state_id = config_id, state_label = temporal_label(resolution_s),
    requirement_rank = match(resolution_s, rev(PRIMARY_TEMPORAL_S))
  )
duration_states <- duration_cube |>
  filter((metric %in% DUAL & str_detect(support_id, "_full$")) |
           (!metric %in% DUAL & !str_detect(support_id, "_full$"))) |>
  distinct(support_id, site, Id, metric, metric_class, metric_geometry, config_id,
           window_id, n_days, window_start, window_end) |>
  mutate(
    dimension = "duration", state_id = paste0(config_id, "__", window_id),
    state_label = paste0(n_days, " d (", window_start, "–", window_end, ")"),
    requirement_rank = n_days
  )
state_catalog <- bind_rows(
  temporal_states |> select(-resolution_s),
  duration_states
) |>
  distinct(dimension, support_id, site, Id, metric, metric_class, metric_geometry,
           state_id, state_label, requirement_rank)

single_stability <- function(g) {
  dim <- first(g$dimension)
  metric_name <- first(g$metric)
  support <- first(g$support_id)
  site <- first(g$site)
  id <- first(g$Id)
  states <- g |> distinct(state_id, state_label, requirement_rank) |> arrange(requirement_rank)
  lattice <- if (dim == "temporal") "temporal" else "duration"
  s <- pair_summary |>
    filter(
      dimension == dim, metric == metric_name, comparison_lattice == lattice,
      config_a_id %in% states$state_id, config_b_id %in% states$state_id
    )
  map_dfr(seq_len(nrow(states)), function(i) {
    c0 <- states$state_id[i]
    higher <- s |> filter(config_a_id == c0, config_b_id != c0, is.finite(A_mean_absolute))
    n_higher <- n_distinct(higher$config_b_id)
    if (!n_higher) {
      tibble(
        dimension = dim, support_id = support, site = site, Id = id,
        metric = metric_name, metric_class = first(g$metric_class),
        metric_geometry = first(g$metric_geometry), state_id = c0,
        state_label = states$state_label[i], requirement_rank = states$requirement_rank[i],
        n_higher_observed = 0L, worst_refinement_config = NA_character_,
        R_obs = NA_real_, status = "boundary_unresolved", boundary = TRUE
      )
    } else {
      w <- higher |> slice_max(A_mean_absolute, n = 1L, with_ties = FALSE)
      tibble(
        dimension = dim, support_id = support, site = site, Id = id,
        metric = metric_name, metric_class = first(g$metric_class),
        metric_geometry = first(g$metric_geometry), state_id = c0,
        state_label = states$state_label[i], requirement_rank = states$requirement_rank[i],
        n_higher_observed = n_higher, worst_refinement_config = w$config_b_id,
        R_obs = w$A_mean_absolute, status = "resolved", boundary = FALSE
      )
    }
  })
}
single_groups <- state_catalog |>
  group_by(dimension, support_id, site, Id, metric) |>
  group_split(.keep = TRUE)
single <- bind_rows(map(single_groups, single_stability)) |>
  mutate(
    core_artifact_version = CORE_VERSION, rq1_analysis_version = RQ1_VERSION,
    rq3_analysis_version = RQ3_VERSION
  )
readr::write_csv(single, file.path(OUT, "rq3_observed_stability.csv"), na = "")
saveRDS(single, file.path(OUT, "rq3_sufficiency_long.rds"), compress = "xz")

eps_domain <- sort(unique(c(0, single$R_obs[is.finite(single$R_obs)])))
sufficiency_long <- if (length(eps_domain)) {
  tidyr::crossing(
    single |> select(dimension, support_id, site, Id, metric, metric_class,
                     metric_geometry, state_id, state_label, requirement_rank, R_obs,
                     n_higher_observed, status),
    epsilon = eps_domain
  ) |>
    mutate(sufficient = status == "resolved" & is.finite(R_obs) &
             R_obs <= epsilon + NUMERIC_TOL)
} else tibble()
readr::write_csv(sufficiency_long, file.path(OUT, "rq3_sufficiency_long.csv"), na = "")

single_requirement <- sufficiency_long |>
  filter(dimension %in% c("temporal", "duration")) |>
  group_by(dimension, metric, epsilon) |>
  group_modify(function(.x, .y) {
    z <- .x |> arrange(requirement_rank)
    ok <- z$sufficient
    tibble(
      sufficient_states = sum(ok),
      sufficient_set_threshold_like = length(ok) < 2L || all(diff(as.integer(ok)) <= 0L),
      least_demanding_sufficient_state = if (any(ok)) z$state_id[which.max(ifelse(ok, z$requirement_rank, -Inf))] else NA_character_
    )
  }) |>
  ungroup()
readr::write_csv(single_requirement, file.path(OUT, "rq3_single_dimension_requirement.csv"), na = "")

# Placement/optical pairs are oriented alternative -> target-aligned state, but
# remain outside the temporal/duration measurement-burden order.
unordered <- pair_summary |>
  filter(dimension %in% c("placement", "optical")) |>
  transmute(
    dimension, comparison_pair_id, config_a_id, config_b_id, config_a_label, config_b_label,
    orientation_type, orientation_basis,
    metric, metric_class, metric_geometry, epsilon_entry = A_mean_absolute,
    A = A_mean_absolute, B = B_mean_signed,
    substitutable_at_epsilon = epsilon_entry <= 0,
    core_artifact_version, rq1_analysis_version
  )
readr::write_csv(unordered, file.path(OUT, "rq3_unordered_substitutability.csv"), na = "")
coverage <- unordered |>
  group_by(dimension, comparison_pair_id, config_a_id, config_b_id, metric_class) |>
  group_modify(function(.x, .y) {
    eps <- sort(unique(c(0, .x$epsilon_entry[is.finite(.x$epsilon_entry)])))
    tibble(
      epsilon = eps, n_metrics = n_distinct(.x$metric),
      fraction_metrics_substitutable = map_dbl(eps, function(e) mean(.x$epsilon_entry <= e + NUMERIC_TOL))
    )
  }) |>
  ungroup()
readr::write_csv(coverage, file.path(OUT, "rq3_unordered_coverage_curves.csv"), na = "")

# Boundary convergence is the local G sequence; no asymptotic model is added.
convergence <- local |>
  mutate(
    requirement_position = if_else(
      dimension == "duration",
      as.integer(str_extract(lower_level, "^\\d+")),
      match(as.character(lower_level), temporal_label(PRIMARY_TEMPORAL_S))
    ),
    boundary_proximity = if_else(
      dimension == "duration", requirement_position / 6, requirement_position / length(PRIMARY_TEMPORAL_S)
    ),
    G = as.numeric(G), rq3_analysis_version = RQ3_VERSION
  )
readr::write_csv(convergence, file.path(OUT, "rq3_convergence_profile.csv"), na = "")

# -------------------------------------------------------------------------
# Multidimensional actual joint states within fixed placement x optical facets.
# -------------------------------------------------------------------------
joint_states <- duration_cube |>
  filter(
    ((metric %in% DUAL) & str_detect(support_id, "_full$")) |
      ((!metric %in% DUAL) & !str_detect(support_id, "_full$")),
    resolution_s %in% PRIMARY_TEMPORAL_S
  ) |>
  select(
    support_id, site, Id, placement, optical, resolution_s, config_id, window_id,
    n_days, window_start, window_end, metric, metric_class, metric_geometry, value, available
  ) |>
  mutate(joint_id = paste(config_id, window_id, sep = "__"))
joint_anchor <- joint_states |>
  filter(resolution_s == 10L, n_days == 6L, available, is.finite(value)) |>
  group_by(support_id, metric, metric_geometry) |>
  summarise(standardizer = aggregate_scale(value, first(metric_geometry)), .groups = "drop")

build_joint_pairs <- function(g) {
  if (nrow(g) < 2L) return(tibble())
  a <- g |>
    select(support_id, site, Id, placement, optical, metric, metric_class, metric_geometry,
           resolution_a = resolution_s, n_days_a = n_days, joint_a = joint_id,
           value_a = value, available_a = available)
  b <- g |>
    select(support_id, site, Id, resolution_b = resolution_s, n_days_b = n_days,
           joint_b = joint_id, value_b = value, available_b = available)
  merge(a, b, by = c("support_id", "site", "Id"), allow.cartesian = TRUE) |>
    filter(
      resolution_b <= resolution_a, n_days_b >= n_days_a,
      resolution_b < resolution_a | n_days_b > n_days_a
    ) |>
    mutate(
      config_a_id = joint_a, config_b_id = joint_b,
      delta = if_else(metric_geometry == "circular_time", circular_delta(value_b, value_a), value_b - value_a),
      requirement_relation = "a_no_more_demanding_than_b"
    )
}
joint_groups <- joint_states |>
  group_by(support_id, site, Id, placement, optical, metric) |>
  group_split(.keep = TRUE)
joint_pairs <- ms_parallel_map_dfr(
  joint_groups, build_joint_pairs, workers = RQ3_WORKERS,
  packages = "tidyverse", exports = "build_joint_pairs"
) |>
  left_join(joint_anchor, by = c("support_id", "metric", "metric_geometry")) |>
  mutate(
    available = coalesce(available_a, FALSE) & coalesce(available_b, FALSE) &
      is.finite(delta) & is.finite(standardizer) & standardizer > 0,
    z = if_else(available, delta / standardizer, NA_real_)
  )
joint_pair_summary <- joint_pairs |>
  filter(available, is.finite(z)) |>
  group_by(
    support_id, placement, optical, resolution_a, n_days_a, config_a_id,
    config_b_id, metric, metric_class, metric_geometry
  ) |>
  summarise(n_participants = n_distinct(paste(site, Id, sep = "|")), n_units = n(),
            A = mean(abs(z)), B = mean(z), .groups = "drop")
joint <- joint_states |>
  distinct(
    support_id, placement, optical, resolution_s, n_days, joint_id, metric,
    metric_class, metric_geometry
  ) |>
  rename(config_id = joint_id) |>
  left_join(
    joint_pair_summary |>
      group_by(
        support_id, placement, optical, resolution_a, n_days_a, metric,
        metric_class, metric_geometry, config_a_id
      ) |>
      summarise(
        n_higher_observed = n_distinct(config_b_id), R_obs = max(A),
        worst_higher_config = config_b_id[which.max(A)], .groups = "drop"
      ),
    by = c(
      "support_id", "placement", "optical", "resolution_s" = "resolution_a",
      "n_days" = "n_days_a", "metric", "metric_class", "metric_geometry",
      "config_id" = "config_a_id"
    )
  ) |>
  mutate(
    status = if_else(coalesce(n_higher_observed, 0L) == 0L, "boundary_unresolved", "resolved"),
    n_higher_observed = coalesce(n_higher_observed, 0L),
    R_obs = if_else(status == "resolved", R_obs, NA_real_),
    joint_configuration = paste(
      placement, optical, temporal_label(resolution_s), paste0(n_days, " d"), sep = " | "
    ),
    epsilon_entry = R_obs, core_artifact_version = CORE_VERSION,
    rq1_analysis_version = RQ1_VERSION, rq3_analysis_version = RQ3_VERSION
  )
readr::write_csv(joint_pair_summary, file.path(OUT, "rq3_joint_pair_summary.csv"), na = "")
saveRDS(joint_pairs, file.path(OUT, "rq3_joint_stability.rds"), compress = "xz")
readr::write_csv(joint, file.path(OUT, "rq3_joint_summary.csv"), na = "")

pareto_at <- function(g, epsilon) {
  z <- g |> filter(status == "resolved", is.finite(R_obs), R_obs <= epsilon + NUMERIC_TOL)
  if (!nrow(z)) return(character())
  keep <- vapply(seq_len(nrow(z)), function(i) {
    !any(
      z$resolution_s <= z$resolution_s[i] & z$n_days <= z$n_days[i] &
        (z$resolution_s < z$resolution_s[i] | z$n_days < z$n_days[i])
    )
  }, logical(1))
  z$config_id[keep]
}
pareto_rows <- list()
pr <- 0L
for (g in joint |> group_by(support_id, placement, optical, metric) |> group_split(.keep = TRUE)) {
  breaks <- sort(unique(c(0, g$R_obs[is.finite(g$R_obs)])))
  if (length(breaks) < 2L) next
  for (i in seq_len(length(breaks) - 1L)) {
    lo <- breaks[i]; hi <- breaks[i + 1L]; ep <- (lo + hi) / 2
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
  group_by(
    support_id, placement, optical, metric, metric_class, resolution_s, n_days,
    config_id, epsilon_entry
  ) |>
  summarise(
    ever_pareto = any(pareto),
    pareto_tolerance_width = sum(epsilon_interval_width[pareto]),
    tolerance_domain_width = sum(epsilon_interval_width),
    pareto_persistence = if (tolerance_domain_width > 0) pareto_tolerance_width / tolerance_domain_width else NA_real_,
    .groups = "drop"
  )
pareto_frequency <- pareto_summary |>
  group_by(placement, optical, resolution_s, n_days) |>
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

boundary_audit <- single |>
  transmute(
    dimension, metric, state_id, n_higher_observed, R_obs, status,
    pass = if_else(
      n_higher_observed == 0L,
      is.na(R_obs) & status == "boundary_unresolved",
      is.finite(R_obs) & status == "resolved"
    )
  )
readr::write_csv(boundary_audit, file.path(DIAG, "rq3_boundary_unresolved_audit.csv"), na = "")
if (any(!boundary_audit$pass)) stop("RQ3 boundary/status invariant failed")

writeLines(c(
  "# RQ3 run report", "",
  paste0("RQ1 upstream: ", RQ1_VERSION),
  paste0("RQ3 analysis version: ", RQ3_VERSION),
  "Single-dimension estimand: R_obs(c)=max A(c,c') over all higher observed states.",
  "Observed upper boundaries have n_higher_observed=0, R_obs=NA, status=boundary_unresolved.",
  "Placement/optical substitutability is evaluated along target-aligned comparisons; these dimensions have no measurement-burden order.",
  "Joint stability uses actual duration_metric_cube values within placement x optical facets.",
  "Joint signed changes use the same lower-requirement -> higher-requirement orientation as RQ1.",
  "Pareto persistence is deterministic tolerance-domain occupancy over intervals bounded by observed R_obs."
), file.path(OUT, "RQ3_RUN_REPORT.md"))
message("RQ3 complete: ", RQ3_VERSION)