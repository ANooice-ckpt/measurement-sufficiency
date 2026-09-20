# Shared frozen joint-summary projection used by RQ3 and its recovery entrypoint.
# Caller supplies joint_pair_summary, joint_state_catalog, versions, OUT and NUMERIC_TOL.
rq3_composition_failure <- function(pairs, states, tol=1e-12) {
  keys <- c("support_id","placement","optical","metric","metric_class","metric_geometry",
            "resolution_a","n_days_a","config_a_id")
  finite <- pairs |> dplyr::filter(is.finite(A))
  one_axis <- function(x, prefix) {
    x <- x |> dplyr::arrange(config_b_id) |>
      dplyr::group_by(dplyr::across(dplyr::all_of(keys))) |>
      dplyr::summarise(R=max(A),n_higher=dplyr::n_distinct(config_b_id),
        worst_config=config_b_id[which.max(A)],
        worst_units=n_units[which.max(A)],worst_participants=n_participants[which.max(A)],
        min_units=min(n_units),max_units=max(n_units),
        min_participants=min(n_participants),max_participants=max(n_participants),.groups="drop")
    names(x)[!names(x) %in% keys] <- paste0(prefix,"_",names(x)[!names(x) %in% keys])
    x
  }
  temporal <- one_axis(finite |> dplyr::filter(resolution_b < resolution_a,n_days_b == n_days_a),"temporal")
  duration <- one_axis(finite |> dplyr::filter(resolution_b == resolution_a,n_days_b > n_days_a),"duration")
  joint_support <- one_axis(finite,"joint")
  worst <- finite |> dplyr::group_by(dplyr::across(dplyr::all_of(keys))) |>
    dplyr::arrange(dplyr::desc(A),config_b_id,.by_group=TRUE) |> dplyr::slice_head(n=1) |>
    dplyr::ungroup() |> dplyr::select(dplyr::all_of(keys),worst_joint_refinement=config_b_id,worst_resolution_s=resolution_b,
      worst_n_days=n_days_b,worst_n_units=n_units,worst_n_participants=n_participants)
  z <- states |> dplyr::rename(resolution_a=resolution_s,n_days_a=n_days,config_a_id=config_id) |>
    dplyr::left_join(temporal,by=keys,relationship="one-to-one") |>
    dplyr::left_join(duration,by=keys,relationship="one-to-one") |>
    dplyr::left_join(joint_support,by=keys,relationship="one-to-one") |>
    dplyr::left_join(worst,by=keys,relationship="one-to-one") |>
    dplyr::mutate(
      temporal_n_higher=dplyr::coalesce(temporal_n_higher,0L),
      duration_n_higher=dplyr::coalesce(duration_n_higher,0L),
      composition_resolved=temporal_n_higher>0L & duration_n_higher>0L & is.finite(R_obs),
      composition_status=dplyr::if_else(composition_resolved,"resolved","axis_unresolved"),
      R_T=temporal_R, R_D=duration_R, R_J=R_obs,
      failure_interval_bounds="[start,end)",
      R_axis=dplyr::if_else(composition_resolved,pmax(temporal_R,duration_R),NA_real_),
      failure_interval_start=R_axis,
      failure_interval_end=dplyr::if_else(composition_resolved,R_obs,NA_real_),
      failure_interval_width=dplyr::if_else(composition_resolved,pmax(0,R_obs-R_axis),NA_real_),
      ever_composition_failure=composition_resolved & is.finite(failure_interval_width) & failure_interval_width>tol
    ) |>
    dplyr::rename(resolution_s=resolution_a,n_days=n_days_a,config_id=config_a_id)
  if (any(z$composition_resolved & z$R_obs + tol < z$R_axis,na.rm=TRUE) ||
      any(abs(z$R_obs-z$joint_R)>tol,na.rm=TRUE)) stop("Joint/axis residual-instability inconsistency")
  # The finite diagnostic slices include the existing Fig.6 .10/.25/.50 slices.
  # Full tolerance dependence is retained exactly in the state-level intervals.
  slices <- c(.05,.10,.20,.25,.30,.50,1)
  long <- tidyr::crossing(z,epsilon=slices) |>
    dplyr::mutate(axis_pass=composition_resolved & is.finite(R_axis) & R_axis<=epsilon+tol,
                  composition_failure=axis_pass & R_obs>epsilon+tol)
  counts <- function(x, groups, scope) {
    x |> dplyr::group_by(dplyr::across(dplyr::all_of(c("epsilon",groups)))) |>
      dplyr::summarise(n_states=dplyr::n(),n_resolved=sum(composition_resolved),
        n_axis_pass=sum(axis_pass),n_failure=sum(composition_failure),
        failure_rate=if(sum(axis_pass)>0)sum(composition_failure)/sum(axis_pass) else NA_real_,
        .groups="drop") |> dplyr::mutate(summary_scope=scope)
  }
  summary <- dplyr::bind_rows(
    counts(long,c("support_id","placement","optical"),"facet"),
    counts(long,c("support_id","placement","optical","metric_class"),"class_facet"),
    counts(long,c("support_id","placement","optical","resolution_s","n_days"),"cell_facet"),
    counts(long,c("placement","optical"),"placement_optical"),
    counts(long,c("placement","optical","resolution_s","n_days"),"cell_placement_optical"),
    counts(long,c("resolution_s","n_days"),"cell_all_facets")
  )
  list(states=z,summary=summary)
}

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
composition <- rq3_composition_failure(joint_pair_summary,joint,NUMERIC_TOL)
composition$summary <- composition$summary |>
  mutate(core_artifact_version=CORE_VERSION,rq1_analysis_version=RQ1_VERSION,rq3_analysis_version=RQ3_VERSION)
readr::write_csv(composition$states,file.path(OUT,"rq3_composition_failure_states.csv"),na="")
readr::write_csv(composition$summary,file.path(OUT,"rq3_composition_failure_summary.csv"),na="")

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
  # Final row freezes the frontier at the inclusive upper endpoint. Above it
  # the frontier cannot change; its width is zero in the finite occupancy domain.
  for (i in seq_along(breaks)) {
    terminal <- i == length(breaks)
    lo <- breaks[[i]]; hi <- if (terminal) lo else breaks[[i + 1L]]
    ep <- lo
    front <- pareto_at(g, ep)
    pr <- pr + 1L
    pareto_rows[[pr]] <- g |>
      transmute(
        support_id, placement, optical, metric, metric_class, resolution_s, n_days, config_id,
        epsilon_entry = R_obs, epsilon_interval_start = lo, epsilon_interval_end = hi,
        epsilon_interval_width = hi - lo, epsilon_midpoint = (lo + hi) / 2,
        terminal_endpoint = terminal,
        core_artifact_version = CORE_VERSION, rq1_analysis_version = RQ1_VERSION,
        rq3_analysis_version = RQ3_VERSION,
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
# Optional legacy alias of the unchanged canonical frontier table.
if (identical(Sys.getenv("MS_EXPORT_COMPAT_CSV", unset = "0"), "1")) {
  readr::write_csv(pareto_summary, file.path(OUT, "rq3_pareto_ever.csv"), na = "")
}
readr::write_csv(pareto_frequency, file.path(OUT, "rq3_pareto_frequency.csv"), na = "")

# Keep the existing pair-summary data-frame contract and filenames. The additive
# attribute stores task decisions beside their frozen joint source; old readers
# still see the same pair rows and columns. No missing target is silently dropped.
task_inventory <- readxl::read_excel("external/zauner_position/data/metric_types.xlsx") |>
  dplyr::transmute(metric = name, metric_class = metric_type)
if (dplyr::n_distinct(task_inventory$metric) != 54L) stop("Expected 54 task target definitions")
task_projection <- rq3_task_projection(joint, task_inventory, tol = NUMERIC_TOL)
task_projection$core_artifact_version <- CORE_VERSION
task_projection$rq1_analysis_version <- RQ1_VERSION
task_projection$rq3_analysis_version <- RQ3_VERSION
attr(joint_pair_summary, "task_projection") <- task_projection
saveRDS(joint_pair_summary, file.path(OUT, "rq3_joint_stability.rds"), compress = "xz")

