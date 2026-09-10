# Shared frozen joint-summary projection used by RQ3 and its recovery entrypoint.
# Caller supplies joint_pair_summary, joint_state_catalog, versions, OUT and NUMERIC_TOL.
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
           "n_days" = "n_days_a", "config_id" = "config_a_id", "metric", "metric_class", "metric_geometry")
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

