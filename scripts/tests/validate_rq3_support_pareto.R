# Synthetic regression coverage; no core inputs or raw series are needed.
suppressPackageStartupMessages(library(tidyverse))
source("scripts/utils/rq3_support.R")
facets <- tidyr::crossing(
  support_id = c("eye_medi", "eye_full", "eye_chest_medi", "eye_chest_full",
                 "eye_wrist_medi", "eye_wrist_full", "eye_chest_wrist_medi", "eye_chest_wrist_full"),
  placement = c("eye", "chest", "wrist"), optical = c("MEDI", "LIGHT"),
  metric = c("mean_MEDI", "MDER")
)
kept <- rq3_support_filter(facets)
stopifnot(nrow(kept) == 9L,
          !anyDuplicated(kept[c("placement", "optical", "metric")]),
          !any(grepl("chest_wrist", kept$support_id)),
          all(endsWith(kept$support_id[kept$optical == "LIGHT"], "_full")),
          !any(kept$optical == "LIGHT" & kept$metric == "MDER"))

check_projection <- function(A) {
  OUT <- tempfile("rq3_projection_")
  dir.create(OUT)
  on.exit(unlink(OUT, recursive = TRUE), add = TRUE)
  CORE_VERSION <- RQ1_VERSION <- RQ3_VERSION <- "synthetic"
  NUMERIC_TOL <- 1e-12
  temporal_label <- function(x) paste(x, "s")
  joint_state_catalog <- tibble(
    support_id = "eye_medi", placement = "eye", optical = "MEDI",
    resolution_s = c(120L, 10L), n_days = c(1L, 6L),
    config_id = c("r120__d1", "r10__d6"), metric = "mean_MEDI",
    metric_class = "test", metric_geometry = "linear"
  )
  joint_pair_summary <- joint_state_catalog[1, ] |>
    rename(resolution_a = resolution_s, n_days_a = n_days, config_a_id = config_id) |>
    mutate(config_b_id = "r10__d6", resolution_b=10L,n_days_b=6L,
           n_units=20L,n_participants=4L,A = A)
  source("scripts/utils/rq3_joint_projection.R", local = TRUE)
  saved <- readRDS(file.path(OUT, "rq3_joint_stability.rds"))
  stopifnot(is.data.frame(saved), nrow(saved) == nrow(joint_pair_summary),
    identical(attr(saved, "task_projection")$task_projection_version, "rq3_task_projection_v1"))
  stopifnot(all(!composition$states$composition_resolved),
            all(is.na(composition$summary$failure_rate)))
  endpoint <- pareto_occupancy |> filter(terminal_endpoint, config_id == "r120__d1")
  stopifnot(nrow(endpoint) == 1L, endpoint$pareto, endpoint$sufficient,
            endpoint$epsilon_interval_width == 0,
            endpoint$epsilon_interval_start == A,
            pareto_summary$ever_pareto[pareto_summary$config_id == "r120__d1"],
            all(pareto_summary$tolerance_domain_width == A),
            all(is.na(joint$R_obs[joint$status == "boundary_unresolved"])))
  # Endpoint must be selectable at and above the largest frozen breakpoint.
  for (eps in c(A, A + 1)) {
    selected <- pareto_occupancy |>
      filter(config_id == "r120__d1",
             epsilon_interval_start <= eps + NUMERIC_TOL,
             epsilon_interval_end > eps + NUMERIC_TOL | terminal_endpoint)
    stopifnot(nrow(selected) == 1L, selected$pareto)
  }
  # Additive .2 + .2 changes already produce a failure at epsilon=.25;
  # composition failure therefore does not, by itself, prove non-additivity.
  example <- bind_rows(joint_pair_summary,joint_pair_summary,joint_pair_summary)
  example$config_b_id <- c("r10__d1","r120__d6","r10__d6")
  example$resolution_b <- c(10L,120L,10L)
  example$n_days_b <- c(1L,6L,6L)
  example$A <- c(.2,.2,.4)
  state <- joint[1,]; state$R_obs <- .4
  cf <- rq3_composition_failure(example,state)
  stopifnot(cf$states$composition_resolved,
    cf$states$R_T==.2,cf$states$R_D==.2,cf$states$R_J==.4,
    cf$states$worst_joint_refinement=="r10__d6",
    cf$states$worst_n_units==20L,cf$states$worst_n_participants==4L,
    cf$states$temporal_worst_config=="r10__d1",
    cf$states$duration_worst_config=="r120__d6",
    cf$states$failure_interval_bounds=="[start,end)",
    abs(cf$states$failure_interval_width-.2)<1e-12,
    cf$summary$failure_rate[cf$summary$summary_scope=="facet" & cf$summary$epsilon==.25]==1,
    cf$summary$failure_rate[cf$summary$summary_scope=="facet" & cf$summary$epsilon==.5]==0,
    is.na(cf$summary$failure_rate[cf$summary$summary_scope=="facet" & cf$summary$epsilon==.1]))
  example$A <- c(.2,.2,.2); state$R_obs <- .2
  cf <- rq3_composition_failure(example,state)
  stopifnot(!cf$states$ever_composition_failure,cf$states$failure_interval_width==0)
}
check_projection(.5)
check_projection(0)

# A task frontier is the intersection of its required targets, not the union or
# majority of their individual frontiers. Missing targets cannot silently pass.
inventory <- tibble(metric = c("mean_MEDI", "dose", "MDER", "crossings"),
                    metric_class = c("level", "level", "spectral", "temporal dynamics"))
cells <- tibble(resolution_s = c(120, 60, 120, 60, 10), n_days = c(1, 1, 2, 2, 2),
                R_obs = c(.6, .3, .4, .2, NA_real_)) |>
  mutate(config_id = paste0("r", resolution_s, "__d", n_days),
         status = if_else(is.finite(R_obs), "resolved", "boundary_unresolved"))
task_joint <- tidyr::crossing(cells, inventory, placement = c("eye", "wrist"),
                             optical = c("MEDI", "LIGHT")) |>
  filter(!(optical == "LIGHT" & metric == "MDER")) |>
  mutate(support_id = paste0(if_else(placement == "eye", "eye", "eye_wrist"),
      if_else(optical == "LIGHT" | metric == "MDER", "_full", "_medi")),
    R_obs = if_else(metric == "dose", R_obs / 2, R_obs))
tasks <- rq3_task_projection(task_joint, inventory,
  tasks = list(level = c("mean_MEDI", "dose"), combined = c("mean_MEDI", "MDER")))
stopifnot(all(tasks$states$n_required == 2L),
  all(tasks$states$status[tasks$states$optical == "LIGHT" & tasks$states$task_id == "combined"] == "unavailable"),
  all(is.na(tasks$states$R_task[tasks$states$status != "resolved"])),
  tasks$states$support_ids[tasks$states$task_id == "combined" & tasks$states$placement == "eye" &
    tasks$states$optical == "MEDI" & tasks$states$config_id == "r120__d1"] == "eye_full;eye_medi")
select_task_interval <- function(x, eps) {
  x |> filter(task_id == "level", placement == "eye", optical == "MEDI",
    epsilon_interval_start <= eps + 1e-12,
    epsilon_interval_end > eps + 1e-12 | terminal_endpoint)
}
for (eps in c(.4, .5)) {
  z <- select_task_interval(tasks$frontiers, eps)
  stopifnot(nrow(z) == nrow(cells),
    setequal(z$config_id[which(z$pareto)], c("r60__d1", "r120__d2")),
    z$sufficient[z$config_id == "r60__d2"], !z$pareto[z$config_id == "r60__d2"],
    is.na(z$sufficient[z$config_id == "r10__d2"]))
}
for (eps in c(.6, 2)) {
  z <- select_task_interval(tasks$frontiers, eps)
  stopifnot(setequal(z$config_id[which(z$pareto)], "r120__d1"))
}
missing <- rq3_task_projection(task_joint |> filter(!(metric == "dose" & config_id == "r120__d1")),
  inventory, list(level = c("mean_MEDI", "dose")))
stopifnot(all(missing$states$status[missing$states$config_id == "r120__d1"] == "unavailable"),
  all(missing$states$missing_targets[missing$states$config_id == "r120__d1"] == "dose"))
absent_inventory <- bind_rows(inventory, tibble(metric = "absent", metric_class = "level"))
absent <- rq3_task_projection(task_joint, absent_inventory)
stopifnot(all(absent$states$status[absent$states$task_id %in% c("all_targets", "level")] == "unavailable"))
nonmonotone <- task_joint |>
  mutate(R_obs = if_else(config_id == "r120__d1", .1, R_obs)) |>
  rq3_task_projection(inventory, list(level = c("mean_MEDI", "dose")))
z <- select_task_interval(nonmonotone$frontiers, .1)
stopifnot(z$sufficient[z$config_id == "r120__d1"],
          !z$sufficient[z$config_id == "r60__d1"], sum(z$pareto, na.rm = TRUE) == 1L)

# Compile only the new task-panel grammar with synthetic frozen decisions.
# No production figure entrypoint, device, rendering or file save is invoked.
published_classes <- readxl::read_excel("external/zauner_position/data/metric_types.xlsx")$metric_type
stopifnot(all(c("level", "timing", "temporal dynamics") %in% published_classes))
source("scripts/utils/fig6_redesign.R")
plot_tasks <- rq3_task_projection(task_joint, inventory,
  list(level = c("mean_MEDI", "dose"), timing = "mean_MEDI", `temporal dynamics` = "crossings"))$frontiers |>
  filter(placement == "eye", optical == "MEDI", epsilon_interval_start <= .4,
         epsilon_interval_end > .4 | terminal_endpoint) |>
  mutate(resolution_rank = match(resolution_s, c(120, 60, 10)),
         task_id = factor(task_id, levels = c("level", "timing", "temporal dynamics")))
plot_env <- new.env(parent = environment())
plot_env$tasks <- plot_tasks
plot_env$base <- ggplot2::theme_void()
plot_env$lattice_axes <- list()
plot_env$unresolved <- "#E1E5E7"
plot_env$muted <- "#657078"
for (expr in as.list(body(ms_fig6_redesign))[-1L]) {
  if (is.call(expr) && identical(expr[[1L]], as.name("<-")) &&
      paste(deparse(expr[[2L]]), collapse = "") %in%
        c("tasks$decision", "task_names", "tasks$task_label", "b")) eval(expr, plot_env)
}
panel <- ggplot2::ggplot_build(plot_env$b)
stopifnot(nrow(panel$data[[1L]]) == nrow(plot_tasks),
  length(unique(panel$data[[1L]]$PANEL)) == 3L,
  nrow(panel$data[[2L]]) == sum(plot_tasks$pareto, na.rm = TRUE),
  nrow(panel$data[[3L]]) == sum(plot_tasks$status != "resolved"))
cat("RQ3 maximal-support, task-set completeness and sufficient/Pareto contracts passed\n")
