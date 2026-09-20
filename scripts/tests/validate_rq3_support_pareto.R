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
cat("RQ3 maximal-support and terminal Pareto contracts passed\n")
