# One maximal stored support per placement/optical/metric facet. Reference eye
# rows on these supports remain available separately for scale construction.
rq3_support_filter <- function(df) {
  base <- unname(c(eye = "eye", chest = "eye_chest", wrist = "eye_wrist")[df$placement])
  full <- df$optical == "LIGHT" | df$metric %in% c("MDER", "nvRD")
  expected <- paste0(base, ifelse(full, "_full", "_medi"))
  keep <- !is.na(base) & df$support_id == expected &
    !(df$optical == "LIGHT" & df$metric %in% c("MDER", "nvRD"))
  df[which(keep), , drop = FALSE]
}

# A task is an explicit set of required published targets, all at the same
# tolerance. Default bundles use the descriptive classes plus the full inventory;
# classes are sets here, never inferential replicates. Callers may supply a named
# list of metric vectors instead. Each target retains its own maximal support.
rq3_task_projection <- function(joint, metric_inventory, tasks = NULL, tol = 1e-12) {
  inventory <- dplyr::distinct(metric_inventory, metric, metric_class)
  if (anyDuplicated(inventory$metric)) stop("Task inventory has conflicting metric classes")
  if (is.null(tasks)) {
    tasks <- c(list(all_targets = inventory$metric),
               split(inventory$metric, inventory$metric_class))
  }
  if (!length(tasks) || is.null(names(tasks)) || any(!nzchar(names(tasks))) ||
      anyDuplicated(names(tasks)) || any(lengths(tasks) == 0L) ||
      any(!unlist(tasks, use.names = FALSE) %in% inventory$metric)) {
    stop("Tasks require unique names and nonempty sets of published targets")
  }
  members <- dplyr::bind_rows(lapply(names(tasks), function(task) {
    tibble::tibble(task_id = task, metric = unique(tasks[[task]]))
  })) |> dplyr::left_join(inventory, by = "metric")
  configurations <- dplyr::distinct(joint, placement, optical, resolution_s, n_days, config_id)
  required <- tidyr::crossing(configurations, members) |>
    dplyr::mutate(
      support_id = paste0(unname(c(eye = "eye", chest = "eye_chest", wrist = "eye_wrist")[placement]),
        ifelse(optical == "LIGHT" | metric %in% c("MDER", "nvRD"), "_full", "_medi")),
      optically_available = !(optical == "LIGHT" & metric %in% c("MDER", "nvRD"))
    ) |>
    dplyr::left_join(
      joint |> dplyr::select(support_id, placement, optical, resolution_s, n_days, config_id,
                             metric, status, R_obs),
      by = c("support_id", "placement", "optical", "resolution_s", "n_days", "config_id", "metric"),
      relationship = "many-to-one"
    ) |>
    dplyr::mutate(available = optically_available & !is.na(status),
                  resolved = available & !is.na(status) & status == "resolved" & is.finite(R_obs))
  states <- required |>
    dplyr::group_by(task_id, placement, optical, resolution_s, n_days, config_id) |>
    dplyr::summarise(
      n_required = dplyr::n(), n_available = sum(available), n_resolved = sum(resolved),
      missing_targets = paste(sort(metric[!available]), collapse = ";"),
      unresolved_targets = paste(sort(metric[available & !resolved]), collapse = ";"),
      support_ids = paste(sort(unique(support_id)), collapse = ";"),
      status = dplyr::case_when(
        n_available < n_required ~ "unavailable",
        n_resolved < n_required ~ "boundary_unresolved",
        TRUE ~ "resolved"
      ),
      R_task = if (all(resolved)) max(R_obs) else NA_real_,
      limiting_targets = if (all(resolved)) paste(sort(metric[abs(R_obs - max(R_obs)) <= tol]), collapse = ";") else "",
      .groups = "drop"
    )
  # Max-over-targets is exactly the intersection of their sufficient sets.
  # Freeze the whole breakpoint domain; plots only select intervals and facets.
  frontiers <- states |>
    dplyr::group_by(task_id, placement, optical) |>
    dplyr::group_modify(function(g, key) {
      breaks <- sort(unique(c(0, g$R_task[is.finite(g$R_task)])))
      dplyr::bind_rows(lapply(seq_along(breaks), function(i) {
        epsilon <- breaks[[i]]
        sufficient <- ifelse(g$status == "resolved", g$R_task <= epsilon + tol, NA)
        z <- g[which(sufficient), , drop = FALSE]
        frontier <- vapply(seq_len(nrow(z)), function(j) {
          !any(z$resolution_s >= z$resolution_s[[j]] & z$n_days <= z$n_days[[j]] &
            (z$resolution_s > z$resolution_s[[j]] | z$n_days < z$n_days[[j]]))
        }, logical(1))
        g |>
          dplyr::mutate(epsilon_interval_start = epsilon,
            epsilon_interval_end = breaks[[min(i + 1L, length(breaks))]],
            terminal_endpoint = i == length(breaks), sufficient = sufficient,
            pareto = ifelse(status == "resolved", config_id %in% z$config_id[frontier], NA))
      }))
    }) |> dplyr::ungroup()
  list(task_projection_version = "rq3_task_projection_v1", inventory = members,
       states = states, frontiers = frontiers)
}
