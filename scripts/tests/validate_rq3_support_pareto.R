# Synthetic regression coverage; no core inputs or raw series are needed.
suppressPackageStartupMessages(library(tidyverse))
source("scripts/utils/rq3_support.R")
facets <- tidyr::crossing(
  support_id = c("eye_medi", "eye_full", "eye_chest_medi", "eye_chest_full",
                 "eye_wrist_medi", "eye_wrist_full", "eye_chest_wrist_medi", "eye_chest_wrist_full"),
  placement = c("eye", "chest", "wrist"), optical = c("MEDI", "LIGHT"),
  metric = c("mean_MEDI", "MDER")
)
kept <- ms_maximal_configuration_support(facets)
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
  boundary <- pareto_occupancy |> filter(config_id == "r10__d6")
  stopifnot(all(boundary$status == "boundary_unresolved"),
    all(is.na(boundary$sufficient)), all(is.na(boundary$pareto)),
    is.na(pareto_summary$ever_pareto[pareto_summary$config_id == "r10__d6"]),
    is.na(pareto_summary$pareto_tolerance_width[pareto_summary$config_id == "r10__d6"]),
    is.na(pareto_frequency$fraction_metrics_ever_pareto[pareto_frequency$resolution_s == 10L]),
    pareto_frequency$n_metrics_resolved[pareto_frequency$resolution_s == 10L] == 0L)
  exported <- readr::read_csv(file.path(OUT, "rq3_pareto_occupancy.csv"), show_col_types = FALSE)
  stopifnot(all(is.na(exported$sufficient[exported$status == "boundary_unresolved"])))
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

# Execute the single-axis producer and query its frozen decisions as Fig. 5
# does. Include a non-threshold-like set, an upper boundary and exact endpoints.
local({
  single <- tibble(dimension="duration", comparison_lattice="duration", metric="synthetic",
    metric_class="level", metric_geometry="linear", state_id=paste0("duration_",1:4,"d"),
    requirement_rank=1:4, R_obs=c(.1,.8,.2,NA_real_),
    status=c(rep("resolved",3),"boundary_unresolved"))
  NUMERIC_TOL <- 1e-12
  code <- parse("scripts/14_rq3_analysis.R")
  for (name in c("sufficiency_long", "single_requirement")) {
    expr <- Filter(function(e) is.call(e) && identical(e[[1]],as.name("<-")) &&
      identical(e[[2]],as.name(name)), as.list(code))
    stopifnot(length(expr)==1L); eval(expr[[1]])
  }
  stopifnot(all(is.na(sufficiency_long$sufficient[sufficiency_long$status=="boundary_unresolved"])))
  plot_code <- parse("scripts/15a_plot_fig5.R")
  helper <- Filter(function(e) is.call(e) && identical(e[[1]],as.name("<-")) &&
    identical(e[[2]],as.name("fig5_requirement_lookup")), as.list(plot_code))
  stopifnot(length(helper)==1L); eval(helper[[1]])
  frozen <- single_requirement |>
    left_join(select(single,state_id,requirement_rank),
      by=c("least_demanding_sufficient_state"="state_id")) |>
    rename(minimum_rank=requirement_rank)
  ranks <- fig5_requirement_lookup(frozen,c(0,.1,.2,.5,.8,1))
  stopifnot(identical(ranks,c(NA_real_,NA_real_,NA_real_,NA_real_,1,1)))
})

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
source("scripts/utils/figure_style.R")
source("scripts/utils/fig6_redesign.R")
source("scripts/utils/analysis_design.R")
# The rejected neighbour is limited by dose (tied with MDER for all targets),
# while dose is NOT limiting at the sufficient frontier cells.
plot_joint <- task_joint |> mutate(R_obs = if_else(config_id == "r120__d1" &
  metric %in% c("dose", "MDER"), .8, R_obs))
plot_tasks <- rq3_task_projection(plot_joint, inventory,
  list(level = c("mean_MEDI", "dose"), `temporal dynamics` = "crossings",
       all_targets = inventory$metric))$frontiers |>
  filter(placement == "eye", optical == "MEDI", epsilon_interval_start <= .4,
         epsilon_interval_end > .4 | terminal_endpoint) |>
  mutate(resolution_rank = match(resolution_s, c(120, 60, 10)),
         task_id = factor(task_id, levels = c("level", "temporal dynamics", "all_targets")),
         nominal_sample_reduction = (6 / 10) / (n_days / resolution_s))
raw_plot_tasks <- plot_tasks
plot_tasks <- ms_fig6_task_explanation(raw_plot_tasks, inventory)
front <- plot_tasks |> filter(task_id == "level", pareto %in% TRUE)
stopifnot(front$shorter_blocking_targets[front$n_days == 2] == "dose",
  front$coarser_blocking_targets[front$resolution_s == 60] == "dose",
  all(front$limiting_targets == "mean_MEDI"),
  all(is.na(front$shorter_blocker_key[front$n_days == 1])),
  all(front$shorter_status[front$n_days == 1] == "outside_domain"),
  all(front$coarser_status[front$resolution_s == 120] == "outside_domain"),
  all(plot_tasks$coarser_status[plot_tasks$resolution_s == 10] == "not_observed"),
  identical(plot_tasks[c("status", "sufficient", "pareto")],
            raw_plot_tasks[c("status", "sufficient", "pareto")]))
for (neighbour_status in c("boundary_unresolved", "unavailable")) {
  unknown <- raw_plot_tasks |> mutate(
    status = if_else(config_id == "r120__d1", neighbour_status, status),
    sufficient = if_else(config_id == "r120__d1", NA, sufficient)) |>
    ms_fig6_task_explanation(inventory)
  stopifnot(all(is.na(unknown$shorter_blocker_key)), all(is.na(unknown$coarser_blocker_key)))
}
# Display selection keeps every frozen state, including unresolved decisions.
selected <- ms_fig6_frontier_display(raw_plot_tasks, epsilon = .4)
stopifnot(nrow(selected) == nrow(raw_plot_tasks),
  identical(selected |> arrange(task_id, config_id) |> select(status, sufficient, pareto),
            raw_plot_tasks |> arrange(task_id, config_id) |> select(status, sufficient, pareto)))
# Equal scheduled counts do not collapse distinct cadence/duration designs.
equal_counts <- tibble(task_id = "level", placement = "eye", optical = "MEDI",
  config_id = c("r30__d3", "r40__d4", "r20__d1"),
  resolution_s = c(30, 40, 20), n_days = c(3, 4, 1),
  epsilon_interval_start = 0, epsilon_interval_end = 1, terminal_endpoint = FALSE,
  status = c("resolved", "resolved", "unavailable"),
  sufficient = c(TRUE, TRUE, NA), pareto = c(TRUE, TRUE, NA))
equal_display <- ms_fig6_frontier_display(equal_counts)
stopifnot(nrow(equal_display) == 3L, sum(equal_display$pareto %in% TRUE) == 2L,
  n_distinct(equal_display$scheduled_records_saved[equal_display$pareto %in% TRUE]) == 1L,
  is.na(equal_display$sufficient[equal_display$config_id == "r20__d1"]),
  is.na(equal_display$pareto[equal_display$config_id == "r20__d1"]))
# A frozen failure interval is half-open. Unresolved states retain NA.
interval_fixture <- tibble(composition_resolved = c(TRUE, TRUE, FALSE, TRUE),
  ever_composition_failure = c(TRUE, TRUE, FALSE, FALSE),
  failure_interval_start = c(.1, .25, NA, NA),
  failure_interval_end = c(.25, .5, NA, NA))
stopifnot(identical(ms_fig6_composition_display(interval_fixture)$failure_at_slice,
                    c(FALSE, TRUE, NA, FALSE)))
# Tolerance displays use only frozen intervals and retain all tradeoff choices.
interval_choices <- equal_counts |> mutate(task_id = "all_targets",
  epsilon_interval_start = .1, epsilon_interval_end = .3)
tolerance_display <- ms_fig6_tolerance_display(interval_choices)
stopifnot(nrow(tolerance_display$allocations) == 2L,
  all(tolerance_display$savings$n_pareto == 2L),
  all(tolerance_display$savings$x_start == .1),
  all(tolerance_display$savings$x_end == .3),
  tolerance_display$savings$savings_low == tolerance_display$savings$savings_high)
no_choice <- ms_fig6_tolerance_display(interval_choices |> mutate(pareto = NA))
stopifnot(nrow(no_choice$allocations) == 0L, no_choice$savings$n_pareto == 0L,
  is.na(no_choice$savings$savings_low), is.na(no_choice$savings$savings_high))
# Matrix cells preserve frozen interval support and keep equal-cost alternatives.
# A terminal endpoint is included only at its endpoint, never carried forward.
matrix_choices <- interval_choices |> mutate(n_required = 2L)
matrix_display <- ms_fig6_matrix_display(matrix_choices, c(.05, .1, .2, .3, .4))
stopifnot(nrow(matrix_display) == 5L,
  all(matrix_display$display_status[matrix_display$epsilon %in% c(.05, .3, .4)] == "outside_frozen_domain"),
  all(matrix_display$n_pareto[matrix_display$epsilon %in% c(.1, .2)] == 2L))
endpoint <- ms_fig6_matrix_display(matrix_choices |> mutate(terminal_endpoint = TRUE), c(.3, .4))
stopifnot(endpoint$n_pareto[endpoint$epsilon == .3] == 2L,
  endpoint$display_status[endpoint$epsilon == .4] == "outside_frozen_domain")
unknown_matrix <- ms_fig6_matrix_display(matrix_choices |>
  mutate(status = "boundary_unresolved", sufficient = NA, pareto = NA), .2)
stopifnot(unknown_matrix$display_status == "unresolved", is.na(unknown_matrix$savings_high))
cat("RQ3 maximal-support, task-set completeness and sufficient/Pareto contracts passed\n")
