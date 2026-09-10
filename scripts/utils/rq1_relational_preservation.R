# RQ1 stage module; sourced in the canonical analysis environment after parts and summaries.
# Required: part_paths, RQ1_DIMENSIONS, RELATIONAL_WORKERS, CORE_VERSION, RQ1_ANALYSIS_VERSION, OUT.
# -----------------------------------------------------------------------------
# Relational preservation used by Fig. 1a
# -----------------------------------------------------------------------------
# This is an RQ1-derived estimand, not a plotting calculation. It consumes the
# canonical paired RQ1 parts and quantifies preservation of unit ordering with
# Spearman correlation. Circular-time representations are excluded because
# ordinary ranks do not respect circular geometry. Temporal and duration use
# only their anchor projections, matching the Fig. 1 comparison grammar.
rank_group_vars <- c(
  "dimension", "comparison_lattice", "comparison_pair_id", "config_a_id", "config_b_id",
  "config_a_label", "config_b_label", "orientation_type", "orientation_basis", "base_config_id",
  "metric", "metric_class", "metric_geometry"
)

rq1_rank_fragment <- function(part_path) {
  x <- readRDS(part_path) |>
    filter(
      available, is.finite(z), is.finite(value_a), is.finite(value_b),
      dimension %in% RQ1_DIMENSIONS,
      dimension %in% c("placement", "optical") | coalesce(anchor_projection, FALSE)
    ) |>
    mutate(
      dimension = as.character(dimension),
      base_config_id = if_else(
        dimension == "duration",
        sub("^(.*)__([^|]+\\|.*)$", "\\1", as.character(config_a_id)),
        NA_character_
      ),
      comparison_pair_id = if_else(
        dimension == "duration", paste0(n_days_a, "d_vs_", n_days_b, "d"),
        as.character(comparison_pair_id)
      ),
      config_a_id = if_else(
        dimension == "duration", paste0("duration_", n_days_a, "d"), as.character(config_a_id)
      ),
      config_b_id = if_else(
        dimension == "duration", paste0("duration_", n_days_b, "d"), as.character(config_b_id)
      ),
      config_a_label = if_else(
        dimension == "duration", paste0(n_days_a, " d"), as.character(config_a_label)
      ),
      config_b_label = if_else(
        dimension == "duration", paste0(n_days_b, " d"), as.character(config_b_label)
      )
    )
  if (!nrow(x)) return(tibble())
  x |>
    group_by(across(all_of(rank_group_vars))) |>
    summarise(
      n_units = n(),
      A_sum = sum(abs(z)),
      participant_keys = list(unique(paste(site, Id, sep = "|"))),
      value_a_values = list(as.numeric(value_a)),
      value_b_values = list(as.numeric(value_b)),
      .groups = "drop"
    )
}

message("RQ1 relational preservation over ", length(part_paths), " canonical parts; workers=", RELATIONAL_WORKERS)
rank_schedule_idx <- order(as.numeric(file.info(part_paths)$size), decreasing = TRUE, na.last = TRUE)
rank_fragments_scheduled <- ms_parallel_map(
  part_paths[rank_schedule_idx], rq1_rank_fragment,
  workers = RELATIONAL_WORKERS,
  packages = c("tidyverse"),
  exports = c("rank_group_vars", "RQ1_DIMENSIONS", "rq1_rank_fragment")
)
rank_fragments <- rank_fragments_scheduled[order(rank_schedule_idx)]
rank_rows <- bind_rows(rank_fragments)
if (!nrow(rank_rows)) stop("No rows available for RQ1 relational-preservation analysis")

rank_base <- rank_rows |>
  group_by(across(all_of(rank_group_vars))) |>
  summarise(
    n_units = sum(n_units),
    A_sum = sum(A_sum),
    participant_keys = list(unique(unlist(participant_keys, use.names = FALSE))),
    value_a_values = list(unlist(value_a_values, use.names = FALSE)),
    value_b_values = list(unlist(value_b_values, use.names = FALSE)),
    .groups = "drop"
  ) |>
  mutate(
    n_participants = map_int(participant_keys, length),
    n_unique_a = map_int(value_a_values, ~n_distinct(.x[is.finite(.x)])),
    n_unique_b = map_int(value_b_values, ~n_distinct(.x[is.finite(.x)])),
    rho_spearman = pmap_dbl(
      list(value_a_values, value_b_values, metric_geometry, n_units, n_unique_a, n_unique_b),
      function(a, b, geometry, n, ua, ub) {
        if (identical(geometry, "circular_time") || n < 3L || ua < 2L || ub < 2L) return(NA_real_)
        r <- suppressWarnings(stats::cor(a, b, method = "spearman", use = "complete.obs"))
        if (is.finite(r)) max(-1, min(1, r)) else NA_real_
      }
    ),
    A_mean_absolute = if_else(n_units > 0, A_sum / n_units, NA_real_),
    rank_loss = if_else(is.finite(rho_spearman), 1 - rho_spearman, NA_real_),
    rank_preservation_available = is.finite(rho_spearman),
    rank_unavailable_reason = case_when(
      metric_geometry == "circular_time" ~ "not applicable to circular-time geometry",
      n_units < 3L ~ "fewer than 3 paired analysis units",
      n_unique_a < 2L | n_unique_b < 2L ~ "constant or degenerate values",
      !is.finite(rho_spearman) ~ "Spearman correlation undefined",
      TRUE ~ NA_character_
    ),
    pair_label = paste(config_a_label, "to", config_b_label),
    core_artifact_version = CORE_VERSION,
    rq1_analysis_version = RQ1_ANALYSIS_VERSION,
    rank_estimand = "Spearman correlation across paired RQ1 analysis units within comparison/base configuration"
  ) |>
  select(
    all_of(rank_group_vars), pair_label, n_participants, n_units, n_unique_a, n_unique_b,
    A_mean_absolute, rho_spearman, rank_loss, rank_preservation_available,
    rank_unavailable_reason, rank_estimand, core_artifact_version, rq1_analysis_version
  )
readr::write_csv(rank_base, file.path(OUT, "rq1_relational_preservation.csv"), na = "")
# Compatibility audit name retained, but it is now written by RQ1 rather than the plotter.
readr::write_csv(rank_base, file.path(OUT, "fig1_rank_preservation.csv"), na = "")

rank_dimension_metric <- rank_base |>
  filter(rank_preservation_available, is.finite(A_mean_absolute), is.finite(rank_loss)) |>
  group_by(dimension, metric, metric_class) |>
  summarise(
    n_oriented_pairs = n(),
    A_typical = median(A_mean_absolute, na.rm = TRUE),
    rank_loss_typical = median(rank_loss, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(core_artifact_version = CORE_VERSION, rq1_analysis_version = RQ1_ANALYSIS_VERSION)

rank_dimension_summary <- rank_dimension_metric |>
  group_by(dimension, metric_class) |>
  summarise(
    n_metrics = n_distinct(metric),
    A_median = median(A_typical, na.rm = TRUE),
    A_q25 = quantile(A_typical, .25, na.rm = TRUE, names = FALSE),
    A_q75 = quantile(A_typical, .75, na.rm = TRUE, names = FALSE),
    rank_loss_median = median(rank_loss_typical, na.rm = TRUE),
    rank_loss_q25 = quantile(rank_loss_typical, .25, na.rm = TRUE, names = FALSE),
    rank_loss_q75 = quantile(rank_loss_typical, .75, na.rm = TRUE, names = FALSE),
    .groups = "drop"
  ) |>
  mutate(core_artifact_version = CORE_VERSION, rq1_analysis_version = RQ1_ANALYSIS_VERSION)

rank_dimension_assoc <- rank_dimension_metric |>
  group_by(dimension) |>
  summarise(
    n_metrics = n_distinct(metric),
    rho_A_rank = if (n_metrics >= 3L && n_distinct(A_typical) >= 2L &&
                     n_distinct(rank_loss_typical) >= 2L) {
      suppressWarnings(stats::cor(A_typical, rank_loss_typical, method = "spearman", use = "complete.obs"))
    } else NA_real_,
    .groups = "drop"
  ) |>
  mutate(core_artifact_version = CORE_VERSION, rq1_analysis_version = RQ1_ANALYSIS_VERSION)

if (!nrow(rank_dimension_summary)) stop("No non-circular rows available for RQ1 relational-preservation summary")
readr::write_csv(rank_dimension_metric, file.path(OUT, "rq1_relational_preservation_dimension_metric.csv"), na = "")
readr::write_csv(rank_dimension_summary, file.path(OUT, "rq1_relational_preservation_dimension_summary.csv"), na = "")
readr::write_csv(rank_dimension_assoc, file.path(OUT, "rq1_distortion_rank_association.csv"), na = "")
# Compatibility audit name retained; no figure code computes this table anymore.
readr::write_csv(rank_dimension_summary, file.path(OUT, "fig1_panel_a_aggregated.csv"), na = "")
rm(rank_fragments, rank_rows)
invisible(gc(FALSE))

