# Standalone design translation; run from the repository root with Rscript.
# Default: reads two frozen outputs and writes ONE allocation CSV.
# --compensation: reads only frozen RQ1 sufficient statistics and writes the
# configuration x participant-count curve. Does not run upstream analyses.
# Optional runtime controls: RQ3_RESOURCE_MC (500), RQ3_RESOURCE_BOOT (1000),
# RQ3_RESOURCE_SEED (20260930). No RQ3 estimand/version is changed.
#
# Common support: all 52 daily metrics AND all three sleep outcomes must be
# matched on a day. Retain runs of >=6 consecutive calendar days; the benchmark
# uses ALL days in these runs, once each (not duplicated overlapping windows).
# This explicit common cohort is only for the resource-allocation comparison.
# Draw a common run per person with probability proportional to its number of
# six-day windows, then use one uniform start quantile for D=6/4/3. This gives
# nested windows and keeps the run mixture fixed across designs. Conditional on
# the chosen run, each legal D-day start is equally likely.
#
# Participant FE, reference-SD / circular sin-cos basis and coefficient distance
# reuse RQ1 inference helpers. Full-cohort bootstrap covariance is fixed across
# allocations; MC participants are sampled WITHOUT replacement within site.
# Positive broad-minus-concentrated deviation means more days performed better.
# Quantiles describe allocation Monte Carlo spread, NOT confidence intervals.
# Each existing draw also records exposure contrast J and its participant
# effective count N_eff. J is reference-SD squared for linear targets and the
# trace of the demeaned sin/cos cross-product for circular targets; compare
# allocations within a target, not J across these geometries. This is exposure
# design information, not outcome-noise-adjusted Fisher information.
# The duration link is a descriptive Spearman correlation across metrics, kept
# separate by geometry; correlated metrics are not independent inferential units.

suppressPackageStartupMessages(library(dplyr))
source("scripts/utils/core_artifacts.R")
source("scripts/utils/rq1_inference.R")

resource_allocation_plan <- function(calendar, designs, replicates, seed) {
  people <- distinct(calendar, participant, site) |> arrange(site, participant)
  strata <- split(seq_len(nrow(people)), people$site)
  # Sequential proportional quotas are monotone: each smaller sample is a
  # subset of the larger sample, including when integer site counts must round.
  capacity <- lengths(strata)
  counts <- integer(length(strata))
  schedule <- integer(max(designs$N))
  for (i in seq_along(schedule)) {
    deficit <- i * capacity / sum(capacity) - counts
    deficit[counts == capacity] <- -Inf
    s <- which.max(deficit)
    schedule[i] <- s
    counts[s] <- counts[s] + 1L
  }
  windows <- list()
  lookup <- vector("list", nrow(people))
  for (p in seq_len(nrow(people))) {
    z <- filter(calendar, .data$participant == .env$people$participant[.env$p])
    runs <- split(z$row, z$run)
    lookup[[p]] <- lapply(runs, function(rows) {
      by_days <- lapply(designs$days, function(D) {
        ids <- integer(length(rows) - D + 1L)
        for (start in seq_along(ids)) {
          ids[start] <- length(windows) + 1L
          windows[[length(windows) + 1L]] <<- rows[seq.int(start, length.out = D)]
        }
        ids
      })
      list(ids = by_days, weight = length(rows) - 5L)
    })
  }
  draws <- lapply(designs$N, function(N) matrix(NA_integer_, N, replicates))
  set.seed(seed)
  for (b in seq_len(replicates)) {
    shuffled <- lapply(strata, function(idx) idx[sample.int(length(idx))])
    used <- integer(length(strata))
    order <- vapply(schedule, function(s) {
      used[s] <<- used[s] + 1L
      shuffled[[s]][used[s]]
    }, integer(1))
    run <- vapply(lookup, function(x) sample.int(length(x), 1L,
                                              prob = vapply(x, `[[`, numeric(1), "weight")), integer(1))
    u <- runif(nrow(people))
    for (j in seq_len(nrow(designs))) {
      draws[[j]][, b] <- vapply(head(order, designs$N[j]), function(p) {
        ids <- lookup[[p]][[run[p]]]$ids[[j]]
        ids[1L + floor(u[p] * length(ids))]
      }, integer(1))
    }
  }
  site_counts <- vapply(designs$N, function(N) {
    paste(names(strata), tabulate(head(schedule, N), nbins = length(strata)),
          sep = ":", collapse = ";")
  }, character(1))
  list(windows = windows, draws = draws, site_counts = site_counts)
}

resource_allocation_summary <- function(x) {
  x <- x[is.finite(x)]
  if (!length(x)) return(c(mean = NA, median = NA, q025 = NA, q975 = NA, valid = 0))
  c(mean = mean(x), median = median(x),
    q025 = unname(quantile(x, .025)), q975 = unname(quantile(x, .975)), valid = length(x))
}

resource_allocation_main <- function(
    inference_path = file.path("results", "rq1", "inference", rq1_inference_contract()$artifact_filename),
    duration_path = "results/rq3/rq3_observed_stability.csv",
    output_path = "results/rq3/rq3_resource_allocation_summary.csv",
    replicates = as.integer(Sys.getenv("RQ3_RESOURCE_MC", "500")),
    bootstrap = as.integer(Sys.getenv("RQ3_RESOURCE_BOOT", "1000")),
    seed = as.integer(Sys.getenv("RQ3_RESOURCE_SEED", "20260930"))) {
  stopifnot(length(replicates) == 1L, !is.na(replicates), replicates >= 2L,
            length(bootstrap) == 1L, !is.na(bootstrap), bootstrap >= 20L,
            length(seed) == 1L, !is.na(seed), seed >= 0L, seed < .Machine$integer.max)
  for (p in c(inference_path, duration_path)) if (!file.exists(p)) stop("Missing frozen input: ", p)
  a <- readRDS(inference_path)
  ms_assert_version(a, "artifact_type", "rq1_inferential_preservation")
  ms_assert_version(a, "core_artifact_version", core_artifact_version())
  ms_assert_version(a, "analysis_design_id", ms_analysis_design_id())
  rq1_version <- rq1_pairwise_version(a)
  ms_assert_version(a, "rq1_analysis_version", paste0("rq1_v6_maximal_duration_support__",
                                                     core_artifact_version(), "__", ms_analysis_design_id()))
  ms_assert_version(a, "rq1_inference_version", rq1_inference_version(rq1_version))
  duration <- readr::read_csv(duration_path, show_col_types = FALSE, progress = FALSE)
  expected_rq3 <- paste0("rq3_v9_unresolved_decisions__", rq1_version, "__", ms_analysis_design_id())
  ms_assert_version(duration, "core_artifact_version", a$core_artifact_version)
  ms_assert_version(duration, "rq1_analysis_version", rq1_version)
  ms_assert_version(duration, "rq3_analysis_version", expected_rq3)
  p <- a$reference_pair_audit
  required <- c("site", "Id", "Date", "metric", "metric_geometry", "metric_scope",
                "outcome", "reference_config", "candidate_config", "reference_value",
                "candidate_value", "reference_available", "pair_reason", "outcome_reason", "outcome_value")
  if (!is.data.frame(p) || !all(required %in% names(p))) stop("Frozen reference_pair_audit is missing required fields")
  for (v in c("core_artifact_version", "rq1_analysis_version", "rq1_inference_version")) {
    ms_assert_version(p, v, a[[v]], "reference_pair_audit")
  }
  contract <- rq1_inference_contract()
  ms_assert_version(p, "reference_config", contract$reference_config)
  ms_assert_version(p, "candidate_config", contract$reference_config)
  sleep <- names(contract$outcome_domain)[contract$outcome_domain == "Sleep"]
  p <- filter(p, outcome %in% sleep, metric_scope == "daily") |>
    mutate(Date = as.Date(Date), participant = paste(site, Id, sep = "|"),
           task = paste(metric, outcome, sep = "__"))
  ms_assert_unique(p, c("site", "Id", "Date", "metric", "outcome"))
  tasks <- distinct(p, task, metric, metric_geometry, outcome) |> arrange(metric, outcome)
  if (n_distinct(tasks$metric) != contract$daily_metric_count ||
      nrow(tasks) != contract$daily_metric_count * length(sleep) ||
      any(!tasks$metric_geometry %in% c("linear", "circular_time"))) {
    stop("Expected all 52 daily metrics x three Sleep outcomes with known geometry")
  }
  ms_assert_unique(tasks, "task")
  good <- filter(p, is.na(pair_reason), is.na(outcome_reason), reference_available,
                 is.finite(reference_value), is.finite(outcome_value))
  if (any(good$candidate_value != good$reference_value | !is.finite(good$candidate_value))) {
    stop("Reference audit contains non-reference candidate values")
  }
  calendar <- good |> count(site, Id, participant, Date) |> filter(n == nrow(tasks)) |>
    arrange(participant, Date) |> group_by(participant) |>
    mutate(run = cumsum(c(TRUE, diff(as.integer(Date)) != 1L))) |>
    group_by(participant, run) |> filter(n() >= 6L) |> ungroup() |>
    mutate(row = row_number())
  pool_n <- n_distinct(calendar$participant)
  k <- min(10L, pool_n %/% 4L)
  message("Common six-day eligible pool: ", pool_n, " participants; ", nrow(calendar), " matched days.")
  if (k < 2L) stop("Need >=8 common six-day eligible participants (smallest FE fit needs >=4); no allocation result written")
  designs <- tibble(design = c("concentrated", "intermediate", "broad"),
                    N = as.integer(c(2, 3, 4) * k), days = c(6L, 4L, 3L)) |>
    mutate(device_days = N * days)
  message("Equal-budget designs: ", paste(designs$N, designs$days, sep = " x ", collapse = " / "))
  plan <- resource_allocation_plan(calendar, designs, replicates, seed)
  d3 <- duration |> filter(dimension == "duration", state_id == "duration_3d") |>
    transmute(metric, metric_geometry, R_obs_3d = R_obs, R_obs_3d_status = status)
  ms_assert_unique(d3, c("metric", "metric_geometry"))
  if (!nrow(d3)) stop("Missing frozen RQ3 duration_3d states")
  deviations <- array(NA_real_, c(nrow(tasks), nrow(designs), replicates))
  rows <- vector("list", nrow(tasks))
  for (t in seq_len(nrow(tasks))) {
    g <- filter(good, task == tasks$task[t]) |>
      inner_join(select(calendar, site, Id, Date, row), by = c("site", "Id", "Date")) |>
      arrange(row)
    stopifnot(identical(g$row, calendar$row))
    # Fit once on the full common eligible data, with reference == candidate.
    # The helper supplies the exact current FE fit and site-cluster bootstrap.
    fit <- rq1_inference_fit(g, B = bootstrap, seed = seed + 1L)
    status <- fit$task_summary$status[[1]]
    reference_estimable <- identical(status, "estimated") &&
      is.finite(fit$task_summary$inference_deviation[[1]])
    information <- effective <- matrix(NA_real_, nrow(designs), replicates)
    if (reference_estimable) {
      beta <- fit$summary$reference_beta
      covariance <- fit$component_blocks$reference_covariance[[1]]
    } else if (identical(status, "estimated")) status <- "reference_covariance_unavailable"
    if (is.finite(fit$summary$reference_scale[[1]])) {
      r <- g$reference_value
      X <- if (tasks$metric_geometry[t] == "circular_time") {
        cbind(sin(2 * pi * r / 86400), cos(2 * pi * r / 86400))
      } else matrix((r - mean(r)) / fit$summary$reference_scale[[1]], ncol = 1L)
      blocks <- lapply(plan$windows, function(idx) {
        rq1_inference_stats(X[idx, , drop = FALSE], g$outcome_value[idx], rep("one", length(idx)))[[1]]
      })
      window_J <- vapply(blocks, function(z) sum(diag(z$xx)), numeric(1))
      within_y <- vapply(plan$windows, function(idx) {
        y <- g$outcome_value[idx]; sum((y - mean(y))^2)
      }, numeric(1))
      for (j in seq_len(nrow(designs))) for (b in seq_len(replicates)) {
        idx <- plan$draws[[j]][, b]
        Ji <- window_J[idx]
        information[j, b] <- sum(Ji)
        effective[j, b] <- if (sum(Ji) > 0) sum(Ji)^2 / sum(Ji^2) else NA_real_
        if (!reference_estimable || sum(within_y[idx]) < 1e-12) next
        candidate <- rq1_inference_solve(blocks[idx], rep(1L, length(idx)))
        deviations[t, j, b] <- rq1_inference_quadnorm(candidate - beta, covariance)
      }
    }
    delta <- deviations[t, 3L, ] - deviations[t, 1L, ]
    comparison <- resource_allocation_summary(delta)
    rows[[t]] <- bind_rows(lapply(seq_len(nrow(designs)), function(j) {
      s <- resource_allocation_summary(deviations[t, j, ])
      js <- resource_allocation_summary(information[j, ])
      ns <- resource_allocation_summary(effective[j, ])
      bind_cols(designs[j, ], tasks[t, ], tibble(
        status = if (status != "estimated") status else if (!s["valid"]) "mc_not_estimable"
          else if (s["valid"] < replicates) "mc_partially_estimable" else "estimated",
        deviation_mean = s["mean"], deviation_median = s["median"],
        deviation_q025 = s["q025"], deviation_q975 = s["q975"], mc_valid = as.integer(s["valid"]),
        J_mean = js["mean"], J_q025 = js["q025"], J_q975 = js["q975"],
        N_eff_mean = ns["mean"], N_eff_q025 = ns["q025"], N_eff_q975 = ns["q975"],
        information_mc_valid = as.integer(js["valid"]), N_eff_mc_valid = as.integer(ns["valid"]),
        information_basis = if (tasks$metric_geometry[t] == "linear") "reference_SD_squared" else "sin_cos_trace",
        task_broad_minus_concentrated = comparison["mean"],
        task_difference_q025 = comparison["q025"], task_difference_q975 = comparison["q975"],
        paired_mc_valid = as.integer(comparison["valid"]),
        reference_bootstrap_valid = fit$task_summary$bootstrap_valid_joint[[1]],
        site_allocation = plan$site_counts[j]))
    }))
    if (t %% 12L == 0L || t == nrow(tasks)) message("Completed ", t, "/", nrow(tasks), " tasks")
  }
  # Average the three outcomes WITHIN each paired replicate before summarizing.
  # All three must be estimable: missing outcomes never change metric weights.
  metric_difference <- bind_rows(lapply(split(seq_len(nrow(tasks)), tasks$metric), function(idx) {
    delta <- matrix(deviations[idx, 3L, ] - deviations[idx, 1L, ], nrow = length(idx))
    s <- resource_allocation_summary(colMeans(delta))
    tibble(metric = tasks$metric[idx[1]], metric_geometry = tasks$metric_geometry[idx[1]],
           metric_allocation_difference = s["mean"], metric_difference_q025 = s["q025"],
           metric_difference_q975 = s["q975"], metric_paired_mc_valid = as.integer(s["valid"]))
  })) |> left_join(d3, by = c("metric", "metric_geometry")) |>
    mutate(R_obs_3d_status = coalesce(R_obs_3d_status, "state_unavailable"))
  links <- metric_difference |> group_by(metric_geometry) |> group_modify(function(z, key) {
    z <- filter(z, is.finite(metric_allocation_difference), is.finite(R_obs_3d), R_obs_3d_status == "resolved")
    rho <- if (nrow(z) >= 3L && sd(z$R_obs_3d) > 0 && sd(z$metric_allocation_difference) > 0)
      cor(z$R_obs_3d, z$metric_allocation_difference, method = "spearman") else NA_real_
    tibble(duration_link_rho = rho, duration_link_n_metrics = nrow(z))
  }) |> ungroup()
  result <- bind_rows(rows) |> left_join(metric_difference, by = c("metric", "metric_geometry")) |>
    left_join(links, by = "metric_geometry")
  # Use the same set of tasks for each design-level average.
  shared <- result |> group_by(task) |> summarise(ok = all(is.finite(deviation_mean)), .groups = "drop")
  overall <- result |> semi_join(filter(shared, ok), by = "task") |> group_by(design) |>
    summarise(design_mean_task_deviation = mean(deviation_mean), design_n_tasks = n(), .groups = "drop")
  hashes <- unname(tools::md5sum(c(inference_path, duration_path)))
  result <- result |> left_join(overall, by = "design") |> mutate(
    reference_config = contract$reference_config, eligible_N = pool_n, eligible_days = nrow(calendar),
    mc_requested = replicates, bootstrap_requested = bootstrap, seed = seed,
    resource_allocation_version = "resource_allocation_v2_within_information",
    core_artifact_version = a$core_artifact_version, rq1_inference_version = a$rq1_inference_version,
    rq3_analysis_version = expected_rq3, inference_md5 = hashes[1], duration_md5 = hashes[2])
  dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
  readr::write_csv(result, output_path, na = "NA")
  message("Wrote ", nrow(result), " task-design rows to ", output_path)
  invisible(result)
}

# Whole participants are sampled without replacement on EACH contrast/task's
# matched support. The full-support scale, reference covariance and signal
# evaluation rows stay fixed as N changes. This is a finite-cohort experiment.
resource_compensation_curve <- function(blocks, replicates, seed,
    sizes = unique(pmax(4L, as.integer(ceiling(nrow(blocks) * c(.25, .5, .75, 1)))))) {
  stopifnot(nrow(blocks) >= 4L, replicates >= 2L, all(sizes >= 4L & sizes <= nrow(blocks)))
  blocks <- arrange(blocks, site, participant)
  sr <- Map(function(xx, xy) list(xx=xx, xy=xy), blocks$reference_xx, blocks$reference_xy)
  sc <- Map(function(xx, xy) list(xx=xx, xy=xy), blocks$candidate_xx, blocks$candidate_xy)
  br_full <- rq1_inference_solve(sr, rep(1L, nrow(blocks)))
  covariance <- blocks$reference_covariance[[1]]
  precision <- if (is.finite(rq1_inference_quadnorm(br_full * 0, covariance))) solve(covariance)
    else matrix(NA_real_, length(br_full), length(br_full))
  rr <- Reduce(`+`, blocks$reference_xx)
  cc <- Reduce(`+`, blocks$candidate_xx)
  rc <- Reduce(`+`, blocks$reference_candidate_xx)
  signal_gram <- rbind(cbind(rr, rc), cbind(t(rc), cc)) / sum(blocks$outcome_within_ss)

  # Nested proportional site quotas, including integer rounding. No duplicated
  # participant and no new day/window selection; paired fits use identical rows.
  strata <- split(seq_len(nrow(blocks)), blocks$site)
  capacity <- lengths(strata)
  used <- integer(length(strata))
  schedule <- integer(nrow(blocks))
  for (i in seq_along(schedule)) {
    deficit <- i * capacity / sum(capacity) - used
    deficit[used == capacity] <- -Inf
    s <- which.max(deficit)
    schedule[i] <- s
    used[s] <- used[s] + 1L
  }
  set.seed(seed)
  orders <- replicate(replicates, {
    shuffled <- lapply(strata, function(idx) idx[sample.int(length(idx))])
    used <- integer(length(strata))
    vapply(schedule, function(s) {
      used[s] <<- used[s] + 1L
      shuffled[[s]][used[s]]
    }, integer(1))
  })
  squared <- function(x, M) pmax(0, colSums(x * (M %*% x)))
  bind_rows(lapply(sizes, function(N) {
    weights <- matrix(0L, nrow(blocks), replicates)
    weights[cbind(as.vector(orders[seq_len(N),,drop=FALSE]), rep(seq_len(replicates), each=N))] <- 1L
    br <- rq1_inference_solve_draws(sr, weights)
    bc <- rq1_inference_solve_draws(sc, weights)
    valid <- colSums(!is.finite(br) | !is.finite(bc)) == 0L &
      as.vector(blocks$outcome_within_ss %*% weights) >= 1e-12
    br[, !valid] <- bc[, !valid] <- NA_real_
    sampling <- br - br_full
    # Coefficient vectors add exactly. In signal space the same identity is
    # evaluated on the full matched support, never on a changing sample of days.
    spaces <- list(
      coefficient_reference_uncertainty = list(a=bc-br, b=sampling, M=precision),
      signal_full_support_outcome_sd = list(a=rbind(-br, bc), b=rbind(sampling, sampling*0), M=signal_gram))
    bind_rows(lapply(names(spaces), function(space) {
      z <- spaces[[space]]
      q <- list(configuration=squared(z$a, z$M), sampling=squared(z$b, z$M),
                total=squared(z$a+z$b, z$M))
      cross <- 2 * colSums(z$a * (z$M %*% z$b))
      ok <- is.finite(q$total) & is.finite(q$configuration) & is.finite(q$sampling)
      stats <- unlist(lapply(names(q), function(name) {
        s <- resource_allocation_summary(sqrt(q[[name]][ok]))
        c(setNames(s[c("mean", "median", "q025", "q975")], paste0(name, "_", c("mean", "median", "q025", "q975"))),
          setNames(if (any(ok)) mean(q[[name]][ok]) else NA_real_, paste0(name, "_ms")))
      }), use.names=TRUE)
      bind_cols(tibble(N=N, participant_fraction=N/nrow(blocks), eligible_N=nrow(blocks),
        eligible_days=sum(blocks$n_days), matched_days_mean=mean(as.vector(blocks$n_days %*% weights)),
        space=space, mc_valid=sum(ok),
        curve_status=if (!any(ok)) "mc_not_estimable" else if (all(ok)) "estimated" else "mc_partially_estimable",
        cross_ms=if (any(ok)) mean(cross[ok]) else NA_real_,
        site_allocation=paste(names(strata), tabulate(head(schedule, N), nbins=length(strata)), sep=":", collapse=";")),
        tibble::as_tibble_row(as.list(stats)))
    }))
  }))
}

resource_compensation_main <- function(
    inference_path = file.path("results", "rq1", "inference", rq1_inference_contract()$artifact_filename),
    output_path = "results/rq3/rq3_configuration_participant_curve.csv",
    replicates = as.integer(Sys.getenv("RQ3_RESOURCE_MC", "500")),
    seed = as.integer(Sys.getenv("RQ3_RESOURCE_SEED", "20260930"))) {
  stopifnot(length(replicates) == 1L, is.finite(replicates), replicates >= 2L,
            length(seed) == 1L, is.finite(seed), seed >= 0L)
  a <- readRDS(inference_path)
  ms_assert_version(a, "artifact_type", "rq1_inferential_preservation")
  ms_assert_version(a, "core_artifact_version", core_artifact_version())
  ms_assert_version(a, "analysis_design_id", ms_analysis_design_id())
  rq1_version <- paste0("rq1_v6_maximal_duration_support__", core_artifact_version(), "__", ms_analysis_design_id())
  ms_assert_version(a, "rq1_analysis_version", rq1_version)
  ms_assert_version(a, "rq1_inference_version", rq1_inference_version(rq1_version))
  keys <- c("candidate_config", "metric", "outcome")
  tasks <- a$contrast_summary |>
    select(all_of(keys), support_id, metric_geometry, outcome_domain, status,
           rq1_distortion_A, within_linear_gain, signal_geometry_bound) |>
    arrange(candidate_config, metric, outcome) |> mutate(task_index=row_number())
  ms_assert_unique(tasks, keys)
  stopifnot(seed + nrow(tasks) < .Machine$integer.max,
            all(tasks$candidate_config %in% rq1_inference_anchor_map()$candidate_config))
  blocks <- left_join(a$component_blocks, select(tasks, all_of(keys), task_index), by=keys)
  required <- c("reference_candidate_xx", "outcome_within_ss", "reference_covariance")
  if (!all(required %in% names(blocks))) stop("Missing frozen compensation statistics; rerun 10b")
  ms_assert_unique(blocks, c(keys, "participant"))
  by_task <- split(blocks, factor(blocks$task_index, levels=seq_len(nrow(tasks))))
  rows <- lapply(seq_len(nrow(tasks)), function(i) {
    b <- by_task[[i]]
    curve <- if (tasks$status[i] == "estimated" && nrow(b) >= 4L) {
      resource_compensation_curve(b, replicates, seed+i)
    } else tibble(N=NA_integer_, curve_status=tasks$status[i])
    if (i %% 100L == 0L) message("Participant-count curves: ", i, "/", nrow(tasks))
    bind_cols(select(tasks[i, ], -task_index), curve)
  })
  result <- bind_rows(rows) |> mutate(reference_config=rq1_inference_contract()$reference_config,
    mc_requested=replicates, seed=seed, compensation_version="configuration_participants_v1",
    core_artifact_version=a$core_artifact_version, rq1_analysis_version=rq1_version,
    rq1_inference_version=a$rq1_inference_version, inference_md5=unname(tools::md5sum(inference_path)))
  dir.create(dirname(output_path), recursive=TRUE, showWarnings=FALSE)
  readr::write_csv(result, output_path, na="NA")
  message("Wrote ", nrow(result), " configuration/participant-count rows to ", output_path)
  invisible(result)
}

if (sys.nframe() == 0L) {
  if ("--compensation" %in% commandArgs(trailingOnly=TRUE)) resource_compensation_main()
  else resource_allocation_main()
}
