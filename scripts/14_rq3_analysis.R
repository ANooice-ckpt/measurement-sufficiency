# Canonical RQ3 analysis; former v5 runtime patches are consolidated here.
suppressPackageStartupMessages(library(tidyverse))
source("scripts/utils/analysis_design.R")
source("scripts/utils/core_artifacts.R")
source("scripts/utils/paths.R")
source("scripts/utils/artifact_validation.R")
source("scripts/utils/parallel_runtime.R")
source("scripts/utils/duration_artifacts.R")
source("scripts/utils/rq1_pairwise_artifacts.R")
source("scripts/utils/rq3_support.R")

# Explicit, inexpensive appendix preparation from frozen target-level R_obs.
# This mode never opens duration blocks or changes the canonical RQ3 outputs.
rq3_prepare_appendix <- function() {
  joint_path <- "results/rq3/rq3_joint_summary.csv"
  upstream_path <- "results/rq1/rq1_pairwise_summary.csv"
  inventory_path <- "external/zauner_position/data/metric_types.xlsx"
  for (path in c(joint_path, upstream_path, inventory_path)) {
    if (!file.exists(path)) stop("Missing appendix input: ", path)
  }
  joint <- readr::read_csv(joint_path, show_col_types = FALSE, progress = FALSE)
  upstream <- readr::read_csv(upstream_path, show_col_types = FALSE, progress = FALSE)
  one <- function(x) {
    x <- unique(x)
    if (length(x) != 1L || is.na(x)) stop("Appendix input has mixed/missing versions")
    x[[1]]
  }
  core_version <- one(joint$core_artifact_version)
  rq1_version <- one(joint$rq1_analysis_version)
  rq3_version <- one(joint$rq3_analysis_version)
  expected_rq3 <- paste0("rq3_v10_min2day_is__", rq1_version, "__", ms_analysis_design_id())
  if (!identical(core_version, core_artifact_version()) ||
      !identical(core_version, one(upstream$core_artifact_version)) ||
      !identical(rq1_version, one(upstream$rq1_analysis_version)) ||
      !any(startsWith(rq1_version, c("rq1_v6_", "rq1_v7_"))) || !identical(rq3_version, expected_rq3)) {
    stop("Appendix requires mutually compatible current Core/RQ1/RQ3 results")
  }
  inventory <- readxl::read_excel(inventory_path) |>
    transmute(metric = name, metric_class = metric_type)
  if (n_distinct(inventory$metric) != 54L) stop("Expected 54 published targets")
  if (any(!joint$resolution_s %in% ms_primary_temporal_s()) ||
      any(!joint$n_days %in% ms_primary_duration_days())) stop("Unexpected joint design states")
  if (any(joint$status != "resolved" & is.finite(joint$R_obs))) {
    stop("Unresolved joint states must retain undefined R_obs")
  }
  # All-54 is optically available only under MEDI. Each target retains its own
  # maximal support; the inventory is never intersected onto all-position data.
  joint <- joint |> filter(optical == "MEDI")
  tasks <- c(list(all_targets = inventory$metric),
    setNames(lapply(inventory$metric, function(metric) setdiff(inventory$metric, metric)),
             paste0("omit__", inventory$metric)))
  projection <- rq3_task_projection(joint, inventory, tasks = tasks)
  epsilon <- .50
  # Query the frozen task intervals at this descriptive tolerance slice.
  selected <- projection$frontiers |>
    filter(epsilon_interval_start <= epsilon + 1e-12) |>
    group_by(task_id, placement, optical, config_id) |>
    slice_max(epsilon_interval_start, n = 1, with_ties = FALSE) |> ungroup()
  keys <- c("placement", "optical", "resolution_s", "n_days", "config_id")
  baseline <- selected |> filter(task_id == "all_targets") |>
    select(all_of(keys), primary_status = status,
           primary_sufficient = sufficient, primary_pareto = pareto)
  detail <- selected |> filter(task_id != "all_targets") |>
    mutate(omitted_target = sub("^omit__", "", task_id), epsilon = epsilon) |>
    left_join(baseline, by = keys, relationship = "many-to-one") |>
    mutate(sufficient_changed = if_else(is.na(sufficient) | is.na(primary_sufficient),
                                       NA, sufficient != primary_sufficient),
           pareto_changed = if_else(is.na(pareto) | is.na(primary_pareto),
                                   NA, pareto != primary_pareto))
  cells <- detail |> group_by(across(all_of(keys))) |>
    summarise(primary_status = first(primary_status),
      primary_sufficient = first(primary_sufficient), primary_pareto = first(primary_pareto),
      n_omissions = n(), n_resolved = sum(status == "resolved"),
      n_sufficient_comparisons = sum(!is.na(sufficient_changed)),
      n_pareto_comparisons = sum(!is.na(pareto_changed)),
      sufficient_change_fraction = if (any(!is.na(sufficient_changed))) mean(sufficient_changed, na.rm = TRUE) else NA_real_,
      pareto_change_fraction = if (any(!is.na(pareto_changed))) mean(pareto_changed, na.rm = TRUE) else NA_real_,
      .groups = "drop")
  targets <- detail |> group_by(placement, optical, omitted_target) |>
    summarise(n_resolved_comparisons = sum(!is.na(pareto_changed)),
      n_sufficient_cells_changed = if (any(!is.na(sufficient_changed))) sum(sufficient_changed, na.rm = TRUE) else NA_integer_,
      n_frontier_cells_changed = if (any(!is.na(pareto_changed))) sum(pareto_changed, na.rm = TRUE) else NA_integer_,
      changed_frontier = if (any(!is.na(pareto_changed))) any(pareto_changed, na.rm = TRUE) else NA,
      .groups = "drop") |>
    left_join(inventory, by = c("omitted_target" = "metric"))
  output <- list(artifact_type = "rq3_appendix_joint_target_sensitivity",
    appendix_analysis_version = "appendix_joint_target_v1",
    core_artifact_version = core_version, rq1_analysis_version = rq1_version,
    rq3_analysis_version = rq3_version, analysis_design_id = ms_analysis_design_id(),
    input_md5 = tools::md5sum(c(joint_path, upstream_path, inventory_path)),
    epsilon = epsilon, optical = "MEDI", n_targets = 54L,
    interpretation = "Deterministic leave-one-target-out sensitivity, not a participant-bootstrap probability",
    unavailable_validation = c("participant-bootstrap selection stability", "alternative sampling phases"),
    baseline = baseline, states = detail, cells = cells, targets = targets)
  saveRDS(output, "results/rq3/rq3_appendix_joint_target_sensitivity.rds", compress = "xz")
  message("Appendix All-54 target sensitivity prepared from frozen RQ3; no Core rebuild")
  invisible(output)
}

# Focused appendix uncertainty/scale analysis. Reconstruct only 10/20-s MEDI
# windows from durable daily values and exact hourly bases; canonical outputs
# remain frozen. Participant draws are shared across targets and pair types.
rq3_appendix_robustness <- function() {
  source("scripts/utils/protocol_windows.R")
  out_dir <- "results/rq3/appendix_robustness"
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  input_files <- c("results/core/metric_cube.csv.gz", "results/core/unit_context.csv.gz",
    "results/core/duration_window_manifest.rds", "results/rq3/rq3_joint_summary.csv",
    "results/rq3/rq3_joint_pair_summary.csv", "results/rq3/fig6_constraint_states_display.csv")
  hashes <- tools::md5sum(input_files)
  inventory <- readxl::read_excel("external/zauner_position/data/metric_types.xlsx") |>
    transmute(metric = name, metric_class = metric_type)
  supports <- c("eye_medi", "eye_full", "eye_chest_medi", "eye_chest_full", "eye_wrist_medi", "eye_wrist_full")
  cache_path <- file.path(out_dir, "focused_window_values.rds")
  if (file.exists(cache_path)) {
    cache <- readRDS(cache_path)
    if (!identical(cache$input_md5, hashes)) stop("Focused-window inputs changed; remove stale appendix cache explicitly")
  } else {
    message("Appendix: read durable daily values and exact hourly bases")
    m <- readr::read_csv(input_files[[1]], show_col_types = FALSE, progress = FALSE,
      col_types = cols(Id = col_character()), num_threads = 4) |>
      filter(support_id %in% supports, optical == "MEDI", resolution_s %in% c(10L,20L))
    u <- readr::read_csv(input_files[[2]], show_col_types = FALSE, progress = FALSE,
      col_select = c(core_artifact_version, support_id, site, Id, Date, analysis_unit_type,
        placement, optical, resolution_s, is_primary_resolution, config_id, isiv_hourly_basis),
      col_types = cols(Id = col_character()), num_threads = 4) |>
      filter(support_id %in% supports, optical == "MEDI", resolution_s %in% c(10L,20L))
    ms_assert_version(m, "core_artifact_version", core_artifact_version())
    ms_assert_version(u, "core_artifact_version", core_artifact_version())
    message("Appendix: reconstruct focused windows without raw-source access")
    built <- build_duration_metric_cube(m, u, inventory)
    expected <- readRDS(input_files[[3]]) |> filter(support_id %in% supports)
    stopifnot(setequal(built$manifest$window_id, expected$window_id))
    cache <- list(input_md5 = hashes, cube = built$cube, windows = built$manifest,
      daily = m, context = u, core_artifact_version = core_artifact_version())
    saveRDS(cache, cache_path, compress = "gzip")
    rm(m,u,built); invisible(gc())
  }
  cdiff <- function(a,b) ((a-b+43200) %% 86400)-43200
  scale_values <- function(x, geometry) {
    if (geometry == "circular_time") x <- cdiff(x, duration_circular_mean(x))
    c(sd = sd(x), robust = IQR(x)/1.349)
  }
  anchors <- cache$cube |> filter(placement == "eye", resolution_s == 10L, n_days == 6L, available, is.finite(value)) |>
    select(support_id, site, Id, metric, metric_geometry, window_id, value) |>
    mutate(participant = paste(site, Id, sep = "|"))
  scales <- anchors |> group_by(support_id, metric, metric_geometry) |>
    summarise(n_anchor_windows = n(), n_anchor_participants = n_distinct(participant),
      standardizer = scale_values(value, first(metric_geometry))[["sd"]],
      robust_standardizer = scale_values(value, first(metric_geometry))[["robust"]], .groups = "drop")
  readr::write_csv(scales, file.path(out_dir,"joint_scale_audit.csv"))
  states <- readr::read_csv(input_files[[6]], show_col_types = FALSE) |>
    select(placement, resolution_s, n_days, role) |> distinct() |>
    mutate(config_id = paste0("r",resolution_s,"__d",n_days))
  stopifnot(nrow(states) == 9L)
  # Participant sufficient statistics retain original smallest-unit weighting.
  make_stats <- function(cube) {
    z <- cube |> ms_maximal_configuration_support() |> filter(available, is.finite(value))
    groups <- z |> group_by(support_id, site, Id, placement, optical, metric, metric_geometry) |> group_split()
    bind_rows(lapply(groups, function(g) {
      a <- g |> inner_join(states |> select(placement,resolution_s,n_days), by=c("placement","resolution_s","n_days")) |>
        transmute(resolution_a=resolution_s,n_days_a=n_days,start_a=window_start,end_a=window_end,value_a=value)
      if (!nrow(a)) return(tibble())
      b <- g |> transmute(resolution_b=resolution_s,n_days_b=n_days,start_b=window_start,end_b=window_end,value_b=value)
      crossing(ai=seq_len(nrow(a)),bi=seq_len(nrow(b))) |>
        transmute(resolution_a=a$resolution_a[ai],n_days_a=a$n_days_a[ai],
          resolution_b=b$resolution_b[bi],n_days_b=b$n_days_b[bi],
          start_a=a$start_a[ai],end_a=a$end_a[ai],start_b=b$start_b[bi],end_b=b$end_b[bi],
          value_a=a$value_a[ai],value_b=b$value_b[bi]) |>
        filter(resolution_b <= resolution_a,n_days_b >= n_days_a,
          resolution_b < resolution_a | n_days_b > n_days_a,start_b <= start_a,end_a <= end_b) |>
        mutate(delta=if (g$metric_geometry[[1]] == "circular_time") cdiff(value_b,value_a) else value_b-value_a) |>
        group_by(resolution_a,n_days_a,resolution_b,n_days_b) |>
        summarise(n_units=n(),sum_abs=sum(abs(delta)),.groups="drop") |>
        mutate(support_id=g$support_id[[1]],site=g$site[[1]],Id=g$Id[[1]],
          participant=paste(site,Id,sep="|"),placement=g$placement[[1]],metric=g$metric[[1]],
          metric_geometry=g$metric_geometry[[1]])
    }))
  }
  stats_path <- file.path(out_dir,"participant_pair_statistics.rds")
  if (file.exists(stats_path)) stats <- readRDS(stats_path) else {
    message("Appendix: collect participant-level paired changes")
    stats <- make_stats(cache$cube); saveRDS(stats,stats_path,compress="gzip")
  }
  pair_keys <- c("support_id","placement","metric","metric_geometry","resolution_a","n_days_a","resolution_b","n_days_b")
  pairs <- stats |> group_by(across(all_of(pair_keys))) |>
    summarise(n_units=sum(n_units),sum_abs=sum(sum_abs),.groups="drop") |>
    left_join(scales,by=c("support_id","metric","metric_geometry")) |>
    mutate(A=sum_abs/n_units/standardizer)
  frozen_pairs <- readr::read_csv(input_files[[5]],show_col_types=FALSE,progress=FALSE) |>
    filter(optical == "MEDI") |> select(all_of(pair_keys),frozen_A=A,frozen_units=n_units)
  check <- pairs |> left_join(frozen_pairs,by=pair_keys,relationship="one-to-one")
  stopifnot(!anyNA(check$frozen_A),all(check$n_units==check$frozen_units),
    max(abs(check$A-check$frozen_A))<1e-9)
  message("Appendix: focused pairs reproduce frozen RQ3; max error ",signif(max(abs(check$A-check$frozen_A)),3))
  # Robust rescaling uses RQ3's support-specific six-day eye reference, not RQ1 scales.
  joint <- readr::read_csv(input_files[[4]],show_col_types=FALSE,progress=FALSE) |> filter(optical=="MEDI")
  robust_joint <- joint |> left_join(scales,by=c("support_id","metric","metric_geometry")) |>
    mutate(primary_R_obs=R_obs, robust_scale_available=is.finite(robust_standardizer)&robust_standardizer>0,
      R_obs=if_else(status=="resolved" & robust_scale_available,R_obs*standardizer/robust_standardizer,NA_real_),
      status=if_else(!robust_scale_available,"unavailable_scale",status))
  # A missing scale makes the required target unavailable; never drop it from All-54.
  robust_for_projection <- robust_joint |> filter(robust_scale_available)
  robust_projection <- rq3_task_projection(robust_for_projection,inventory,tasks=list(all_targets=inventory$metric))
  robust_selected <- robust_projection$frontiers |> filter(epsilon_interval_start <= .5+1e-12) |>
    group_by(task_id,placement,optical,config_id) |> slice_max(epsilon_interval_start,n=1,with_ties=FALSE) |> ungroup()
  readr::write_csv(robust_joint,file.path(out_dir,"joint_robust_scale_targets.csv"))
  readr::write_csv(robust_selected,file.path(out_dir,"joint_robust_scale_decisions.csv"))
  # Full-estimator cluster bootstrap: paired changes AND their SD anchors are re-estimated.
  B <- as.integer(Sys.getenv("RQ3_APPENDIX_BOOTSTRAP", "1000")); stopifnot(B>=100L)
  set.seed(20261014)
  scale_diagnostics <- list()
  scale_lookup <- setNames(scales$standardizer,paste(scales$support_id,scales$metric,sep="|"))
  bootstrap <- bind_rows(lapply(c("eye","chest","wrist"),function(place) {
    st <- stats |> filter(placement==place)
    an <- anchors |> filter(support_id %in% unique(st$support_id))
    roster <- bind_rows(st |> select(site,participant),an |> select(site,participant)) |> distinct() |> arrange(site,participant)
    weights <- matrix(0,nrow=nrow(roster),ncol=B)
    for (site_i in unique(roster$site)) {
      ix <- which(roster$site==site_i)
      weights[ix,] <- rmultinom(B,length(ix),rep(1/length(ix),length(ix)))
      stopifnot(all(colSums(weights[ix,,drop=FALSE])==length(ix)))
    }
    scale_boot <- split(an,paste(an$support_id,an$metric,sep="|"))
    scale_boot <- lapply(scale_boot,function(a) {
      w <- weights[match(a$participant,roster$participant),,drop=FALSE]; n <- colSums(w); x <- a$value
      if (a$metric_geometry[[1]] == "circular_time") {
        mu <- atan2(colSums(w*sin(2*pi*x/86400)),colSums(w*cos(2*pi*x/86400)))*86400/(2*pi)
        dev <- vapply(mu,function(m) cdiff(x,m),numeric(length(x)))
        dev <- sweep(dev,2,colSums(w*dev)/n,"-")
      } else {
        dev <- sweep(matrix(x,nrow=length(x),ncol=B),2,colSums(w*x)/n,"-")
      }
      variance <- colSums(w*dev^2)/(n-1)
      represented_values <- if(a$metric_geometry[[1]]=="circular_time") x%%86400 else x
      distinct_count <- colSums(rowsum(w,match(represented_values,unique(represented_values)),reorder=FALSE)>0)
      estimated <- ifelse(n>1,sqrt(variance),NA_real_)
      estimated[n>1 & distinct_count<2] <- 0
      # Independent scalar evaluation of identical participant draws validates
      # both circular centring and degenerate-reference handling.
      for(j in seq_len(min(20L,B))) {
        expanded <- rep(x,w[,j])
        expected <- if(length(expanded)>1L) scale_values(expanded,a$metric_geometry[[1]])[["sd"]] else NA_real_
        stopifnot(identical(is.finite(expected),is.finite(estimated[[j]])))
        if(is.finite(expected)) stopifnot(abs(expected-estimated[[j]])<1e-7*(1+abs(expected)),
          (expected==0)==(estimated[[j]]==0))
      }
      estimated
    })
    scale_boot <- scale_boot[unique(paste(st$support_id,st$metric,sep="|"))]
    scale_diagnostics[[place]] <<- tibble(placement=place,scale_key=names(scale_boot),
      n_draws=B,n_undefined=vapply(scale_boot,function(s) sum(!is.finite(s)|s<=0),integer(1)))
    message("Appendix: bootstrap ",place," with ",nrow(roster)," participant clusters")
    by_pair <- st |> group_by(across(all_of(pair_keys))) |> group_split()
    paired <- lapply(by_pair,function(g) {
      w <- weights[match(g$participant,roster$participant),,drop=FALSE]
      denom <- colSums(w*g$n_units)
      numer <- colSums(w*g$sum_abs)
      scale_key <- paste(g$support_id[[1]],g$metric[[1]],sep="|")
      sb <- scale_boot[[scale_key]];sf <- scale_lookup[[scale_key]]
      tibble(draw=seq_len(B),placement=place,metric=g$metric[[1]],
        resolution_s=g$resolution_a[[1]],n_days=g$n_days_a[[1]],
        A=ifelse(denom>0 & is.finite(sb)&sb>0,numer/denom/sb,NA_real_),
        A_fixed=ifelse(denom>0,numer/denom/sf,NA_real_))
    }) |> bind_rows()
    paired |> pivot_longer(c(A,A_fixed),names_to="scale_policy",values_to="pair_A") |>
      mutate(scale_policy=if_else(scale_policy=="A","reestimated","fixed")) |>
      group_by(scale_policy,draw,placement,metric,resolution_s,n_days) |>
      summarise(R_obs=if(all(is.finite(pair_A))) max(pair_A) else NA_real_,.groups="drop") |>
      group_by(scale_policy,draw,placement,resolution_s,n_days) |>
      summarise(n_targets=n(),n_resolved=sum(is.finite(R_obs)),
        R_K=if(n()==54L && all(is.finite(R_obs))) max(R_obs) else NA_real_,
        limiting_target=if(n()==54L && all(is.finite(R_obs))) metric[which.max(R_obs)] else NA_character_,.groups="drop")
  }))
  qs <- function(x,p) {x<-x[is.finite(x)];if(length(x)) unname(quantile(x,p)) else NA_real_}
  bootstrap_summary <- bootstrap |> group_by(scale_policy,placement,resolution_s,n_days) |>
    summarise(n_draws=n(),n_resolved=sum(is.finite(R_K)),
      n_sufficient=sum(is.finite(R_K)&R_K<=.5),
      R_K_low=qs(R_K,.025),R_K_median=qs(R_K,.5),R_K_high=qs(R_K,.975),
      sufficient_probability=if(any(is.finite(R_K))) mean(R_K[is.finite(R_K)]<=.5) else NA_real_,.groups="drop") |>
    left_join(states,by=c("placement","resolution_s","n_days"))
  primary <- joint |> inner_join(states,by=c("placement","resolution_s","n_days","config_id")) |>
    group_by(placement,resolution_s,n_days) |> summarise(primary_R_K=max(R_obs),.groups="drop")
  bootstrap_summary <- bootstrap_summary |> left_join(primary,by=c("placement","resolution_s","n_days"))
  readr::write_csv(bootstrap,file.path(out_dir,"participant_bootstrap_draws.csv"))
  readr::write_csv(bootstrap_summary,file.path(out_dir,"participant_bootstrap_summary.csv"))
  scale_diagnostics <- bind_rows(scale_diagnostics)
  readr::write_csv(scale_diagnostics,file.path(out_dir,"bootstrap_scale_diagnostics.csv"))
  output <- list(artifact_version="appendix_robustness_v1",input_md5=hashes,
    core_artifact_version=core_artifact_version(),rq1_analysis_version=unique(joint$rq1_analysis_version),
    rq3_analysis_version=unique(joint$rq3_analysis_version),epsilon=.5,n_bootstrap=B,seed=20261014L,
    estimand="Site-stratified participant bootstrap of All-54 R_K; SD anchor re-estimated; nine prespecified states, not full-frontier selection probabilities",
    scales=scales,robust_joint=robust_joint,robust_decisions=robust_selected,
    bootstrap_summary=bootstrap_summary |> filter(scale_policy=="reestimated"),
    bootstrap_fixed_summary=bootstrap_summary |> filter(scale_policy=="fixed"),
    bootstrap=bootstrap,bootstrap_scale_diagnostics=scale_diagnostics,
    frozen_pair_max_error=max(abs(check$A-check$frozen_A)),phase_status="pending")
  saveRDS(output,file.path(out_dir,"rq3_appendix_robustness.rds"),compress="gzip")
  print(bootstrap_summary,n=18,width=Inf)
  invisible(output)
}

# The 20-s neighbour has exactly two source-grid phases (0 and 10 s).
# This explicitly requested observation-operator sensitivity is the sole raw
# source use here. Eligibility and windows stay fixed; no primary cache changes.
rq3_appendix_phase <- function() {
  source("scripts/utils/protocol_windows.R")
  out_dir <- "results/rq3/appendix_robustness"
  base <- readRDS(file.path(out_dir,"focused_window_values.rds"))
  result_path <- file.path(out_dir,"rq3_appendix_robustness.rds")
  prepare_only <- "--appendix-phase-prepare" %in% commandArgs(trailingOnly=TRUE)
  result <- if(file.exists(result_path)) readRDS(result_path) else NULL
  stopifnot(identical(as.character(tools::md5sum(names(base$input_md5))),as.character(base$input_md5)))
  if(!prepare_only && is.null(result)) stop("Run --appendix-robustness before finalizing phase analysis")
  if(!is.null(result)) stopifnot(identical(base$input_md5,result$input_md5))
  phase_dir <- file.path(out_dir,"phase_parts");dir.create(phase_dir,recursive=TRUE,showWarnings=FALSE)
  jobs <- base$context |> filter(resolution_s==20L) |>
    mutate(target_placement=case_when(grepl("chest",support_id)~"chest",grepl("wrist",support_id)~"wrist",TRUE~"eye")) |>
    filter(placement==target_placement) |> distinct(support_id,site,placement) |> arrange(support_id,site)
  tasks <- lapply(seq_len(nrow(jobs)),function(i) {
    j <- jobs[i,]
    list(support_id=j$support_id[[1]],site=j$site[[1]],placement=j$placement[[1]],
      cache_path=file.path(phase_dir,paste0(j$support_id,"__",j$site,".rds")),
      expected=base$context |> filter(support_id==j$support_id[[1]],site==j$site[[1]],placement==j$placement[[1]],resolution_s==20L) |>
        distinct(site,Id,Date),
      primary_check=if(j$support_id[[1]]=="eye_medi" && j$site[[1]]=="BAUA")
        base$daily |> filter(support_id=="eye_medi",site=="BAUA",placement=="eye",resolution_s==20L,
          analysis_unit_type=="participant_day",!metric %in% c("MDER","nvRD")) else NULL)
  })
  worker <- function(task) {
    source("scripts/utils/melidos_io.R");source("scripts/utils/rq1_metrics.R")
    source("scripts/utils/core_artifacts.R");source("scripts/utils/core_context.R")
    if(getRversion()!=package_version("4.5.0") || packageVersion("LightLogR")!=package_version("0.10.3")) stop("Phase sensitivity requires frozen R/LightLogR runtime")
    raw_paths <- c(raw_data_path(task$site,"light_glasses"),raw_data_path(task$site,"sleepdiaries"),raw_data_path(task$site,"wearlog"))
    if(task$placement!="eye") raw_paths <- c(raw_paths,raw_data_path(task$site,paste0("light_",task$placement)))
    provenance <- tools::md5sum(raw_paths)
    if(file.exists(task$cache_path)) {
      old <- readRDS(task$cache_path)
      if(!identical(old$raw_md5,provenance)) stop("Phase source changed: ",task$site)
      return(old)
    }
    support <- core_prepare_support(task$site,task$support_id) |> ungroup()
    observed <- support |> distinct(site,Id,Date) |> mutate(Id=as.character(Id))
    stopifnot(setequal(paste(observed$site,observed$Id,observed$Date),
      paste(task$expected$site,task$expected$Id,task$expected$Date)))
    ten <- core_make_series(support,task$placement,"MEDI",10L) |> ungroup()
    seconds <- round(as.numeric(ten$Datetime));phase10 <- core_source_grid_phase(ten$site,ten$Id,seconds)
    keep <- ((seconds-phase10) %% 20L)==10L
    alt <- ten[keep,] |> mutate(configuration="alternate_20s_phase10")
    stopifnot(all(((round(as.numeric(alt$Datetime))-phase10[keep]) %% 20L)==10L),
      identical(alt$MEDI,ten$MEDI[keep]),identical(alt$LIGHT,ten$LIGHT[keep]))
    full <- grepl("_full$",task$support_id)
    daily <- if(full) rq1_spectral_daily_metrics(alt) else rq1_daily_metrics(alt)
    hourly <- core_isiv_hourly_basis(alt)
    primary_error <- NA_real_
    if(!is.null(task$primary_check)) {
      original <- core_make_series(support,task$placement,"MEDI",20L) |> mutate(configuration="primary_20s")
      check <- rq1_daily_metrics(original) |> mutate(Id=as.character(Id)) |>
        inner_join(task$primary_check |> select(site,Id,Date,metric,expected_value=value,metric_geometry),
          by=c("site","Id","Date","metric"),relationship="one-to-one")
      delta <- ifelse(check$metric_geometry=="circular_time",((check$value-check$expected_value+43200)%%86400)-43200,check$value-check$expected_value)
      stopifnot(nrow(check)==nrow(task$primary_check),all(is.finite(check$value)==is.finite(check$expected_value)),
        all(abs(delta[is.finite(delta)])<1e-7*(1+abs(check$expected_value[is.finite(delta)]))))
      primary_error <- max(abs(delta),na.rm=TRUE)
    }
    add_keys <- function(d) d |> mutate(support_id=task$support_id,placement=task$placement,Id=as.character(Id),resolution_s=20L)
    out <- list(raw_md5=provenance,daily=add_keys(daily),hourly=add_keys(hourly),
      audit=tibble(support_id=task$support_id,site=task$site,placement=task$placement,
        n_participants=n_distinct(alt$Id),n_days=nrow(observed),n_source_rows=nrow(ten),
        n_retained_rows=nrow(alt),exact_source_subset=TRUE,primary_reproduction_max_error=primary_error))
    saveRDS(out,task$cache_path,compress="gzip");out
  }
  environment(worker) <- .GlobalEnv
  message("Appendix phase: ",length(tasks)," site/support jobs; 20-s alternate phase only")
  parts <- ms_parallel_map(tasks,worker,workers=ms_resolve_workers("RQ3_PHASE_WORKERS",4L,6L),packages=c("tidyverse","LightLogR"))
  if(prepare_only) {
    print(bind_rows(lapply(parts,`[[`,"audit")));return(invisible(parts))
  }
  ad <- bind_rows(lapply(parts,`[[`,"daily")) |> select(support_id,site,Id,Date,placement,resolution_s,metric,alternate_value=value) |> mutate(replaced=TRUE)
  ah <- bind_rows(lapply(parts,`[[`,"hourly")) |> select(support_id,site,Id,Date,placement,resolution_s,alternate_basis=isiv_hourly_basis)
  m <- base$daily |> left_join(ad,by=c("support_id","site","Id","Date","placement","resolution_s","metric"),relationship="one-to-one") |>
    mutate(value=if_else(replaced %in% TRUE,alternate_value,value),available=is.finite(value)) |> select(-alternate_value,-replaced)
  u <- base$context |> left_join(ah,by=c("support_id","site","Id","Date","placement","resolution_s"),relationship="one-to-one") |>
    mutate(isiv_hourly_basis=coalesce(alternate_basis,isiv_hourly_basis)) |> select(-alternate_basis)
  inventory <- readxl::read_excel("external/zauner_position/data/metric_types.xlsx") |> transmute(metric=name,metric_class=metric_type)
  # Only alternate values need window aggregation; all 10-s target values are
  # already durable. Eye/10-s context rows retain original window eligibility.
  m <- m |> filter(resolution_s==20L) |> ms_maximal_configuration_support()
  u <- u |> mutate(target_placement=case_when(grepl("chest",support_id)~"chest",grepl("wrist",support_id)~"wrist",TRUE~"eye")) |>
    filter((placement=="eye" & resolution_s==10L) |
      (resolution_s==20L & placement==target_placement & !grepl("_full$",support_id))) |> select(-target_placement)
  built <- build_duration_metric_cube(m,u,inventory)
  stopifnot(setequal(base$windows$window_id,built$manifest$window_id))
  z <- bind_rows(base$cube |> filter(resolution_s==10L),built$cube |> filter(resolution_s==20L)) |>
    ms_maximal_configuration_support() |> filter(available,is.finite(value))
  focal <- result$bootstrap_summary |> filter(resolution_s==20L) |> select(placement,n_days)
  phase_pairs <- function(values) {
    keys <- c("support_id","site","Id","placement","metric","metric_geometry")
    values <- values |> inner_join(focal |> rename(min_days=n_days),by="placement") |> filter(n_days>=min_days)
    a <- values |> filter(resolution_s==20L,n_days==min_days) |>
      select(all_of(keys),resolution_a=resolution_s,n_days_a=n_days,start_a=window_start,end_a=window_end,value_a=value)
    b <- values |> select(all_of(keys),resolution_b=resolution_s,n_days_b=n_days,start_b=window_start,end_b=window_end,value_b=value)
    inner_join(a,b,by=keys,relationship="many-to-many") |>
      filter(resolution_b<=resolution_a,n_days_b>=n_days_a,
        resolution_b<resolution_a | n_days_b>n_days_a,start_b<=start_a,end_a<=end_b) |>
      mutate(delta=if_else(metric_geometry=="circular_time",((value_b-value_a+43200)%%86400)-43200,value_b-value_a)) |>
      group_by(support_id,placement,metric,metric_geometry,resolution_a,n_days_a,resolution_b,n_days_b) |>
      summarise(n_units=n(),sum_abs=sum(abs(delta)),.groups="drop")
  }
  partial <- phase_pairs(z)
  pk <- c("support_id","placement","metric","metric_geometry","resolution_a","n_days_a","resolution_b","n_days_b")
  alternate <- partial |> group_by(across(all_of(pk))) |>
    summarise(n_units=sum(n_units),sum_abs=sum(sum_abs),.groups="drop") |>
    left_join(result$scales,by=c("support_id","metric","metric_geometry")) |> mutate(A=sum_abs/n_units/standardizer)
  primary_pairs <- readr::read_csv("results/rq3/rq3_joint_pair_summary.csv",show_col_types=FALSE,progress=FALSE) |>
    filter(optical=="MEDI",resolution_a==20L) |>
    inner_join(focal,by=c("placement","n_days_a"="n_days")) |>
    select(all_of(pk),primary_A=A,primary_n_units=n_units)
  primary_check <- phase_pairs(base$cube |> ms_maximal_configuration_support() |> filter(available,is.finite(value))) |>
    left_join(result$scales,by=c("support_id","metric","metric_geometry")) |>
    left_join(primary_pairs,by=pk,relationship="one-to-one")
  stopifnot(nrow(primary_check)==nrow(primary_pairs),!anyNA(primary_check$primary_A),
    all(primary_check$n_units==primary_check$primary_n_units),
    max(abs(primary_check$sum_abs/primary_check$n_units/primary_check$standardizer-primary_check$primary_A))<1e-9)
  paired <- primary_pairs |> left_join(alternate |> select(all_of(pk),alternate_A=A,alternate_n_units=n_units),by=pk,relationship="one-to-one")
  detail <- paired |> group_by(support_id,placement,metric,metric_geometry,resolution_a,n_days_a) |>
    summarise(primary_R_obs=max(primary_A),alternate_R_obs=if(all(is.finite(alternate_A))) max(alternate_A) else NA_real_,
      n_pair_types=n(),n_alternate_pair_types=sum(is.finite(alternate_A)),.groups="drop")
  summary <- detail |> group_by(placement,resolution_a,n_days_a) |>
    summarise(n_targets=n(),n_resolved=sum(is.finite(alternate_R_obs)),primary_R_K=max(primary_R_obs),
      alternate_R_K=if(n()==54L && all(is.finite(alternate_R_obs))) max(alternate_R_obs) else NA_real_,
      primary_limiting_target=metric[which.max(primary_R_obs)],
      alternate_limiting_target=if(all(is.finite(alternate_R_obs))) metric[which.max(alternate_R_obs)] else NA_character_,.groups="drop") |>
    mutate(primary_sufficient=primary_R_K<=.5,alternate_sufficient=alternate_R_K<=.5)
  result$phase_status <- "complete";result$phase_targets <- detail;result$phase_summary <- summary
  result$phase_audit <- bind_rows(lapply(parts,`[[`,"audit"))
  result$phase_raw_md5 <- lapply(parts,`[[`,"raw_md5")
  result$phase_scope <- "Both possible 20-s phases on the 10-s source grid; three prespecified MEDI neighbours; all 54 targets and all higher states; unchanged 10-s states are phase-invariant"
  readr::write_csv(paired,file.path(out_dir,"phase_pair_comparison.csv"))
  readr::write_csv(detail,file.path(out_dir,"phase_target_comparison.csv"))
  readr::write_csv(summary,file.path(out_dir,"phase_state_summary.csv"))
  readr::write_csv(result$phase_audit,file.path(out_dir,"phase_source_audit.csv"))
  saveRDS(result,result_path,compress="gzip")
  print(summary,width=Inf);invisible(result)
}

if (any(c("--appendix-phase","--appendix-phase-prepare") %in% commandArgs(trailingOnly = TRUE))) {
  rq3_appendix_phase()
  quit(save = "no", status = 0L)
}

if ("--appendix-robustness" %in% commandArgs(trailingOnly = TRUE)) {
  rq3_appendix_robustness()
  quit(save = "no", status = 0L)
}

if ("--appendix" %in% commandArgs(trailingOnly = TRUE)) {
  rq3_prepare_appendix()
  quit(save = "no", status = 0L)
}

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
REFRESH_FROZEN <- "--refresh-frozen-availability" %in% commandArgs(trailingOnly = TRUE)
ensure_result_dirs(OUT, DIAG)
required_inputs <- c(RQ1_LONG, RQ1_SUMMARY, LOCAL,
  if (REFRESH_FROZEN) file.path(OUT, c("rq3_joint_summary.csv", "rq3_joint_stability.rds")) else DURATION_CUBE)
for (p in required_inputs) if (!file.exists(p)) stop("Missing RQ3 input: ", p)

PRIMARY_TEMPORAL_S <- ms_primary_temporal_s()
PRIMARY_DURATION_DAYS <- ms_primary_duration_days()
ANALYSIS_DESIGN_ID <- ms_analysis_design_id()

pairwise_artifact <- readRDS(RQ1_LONG)
pair_summary <- readr::read_csv(RQ1_SUMMARY, show_col_types = FALSE, progress = FALSE)
rq1_assert_summary_version(pairwise_artifact, pair_summary)
local <- readr::read_csv(LOCAL, show_col_types = FALSE, progress = FALSE)
rq1_assert_summary_version(pairwise_artifact, local)
if (!REFRESH_FROZEN) {
  duration_artifact <- readRDS(DURATION_CUBE)
  if (!duration_cube_is_partitioned(duration_artifact)) stop("RQ3 requires partitioned duration metric cube")
  duration_part_paths <- file.path(duration_artifact$part_dir, duration_artifact$parts)
  if (any(!file.exists(duration_part_paths))) stop("Missing duration metric cube part")
}

RQ1_VERSION <- rq1_pairwise_version(pairwise_artifact)
if (!is.null(pairwise_artifact$analysis_design_id) && !identical(as.character(pairwise_artifact$analysis_design_id[[1]]), ANALYSIS_DESIGN_ID)) {
  stop("RQ1 artifact analysis design does not match current frozen design")
}
CORE_VERSION <- unique(na.omit(c(pairwise_artifact$core_artifact_version, pair_summary$core_artifact_version)))
if (length(CORE_VERSION) != 1L) stop("Core version mismatch")
CORE_VERSION <- CORE_VERSION[[1]]
ms_assert_version(pairwise_artifact, "core_artifact_version", core_artifact_version())
if (!REFRESH_FROZEN) ms_assert_version(duration_artifact, "core_artifact_version", CORE_VERSION)
RQ3_VERSION <- paste0("rq3_v10_min2day_is__", RQ1_VERSION, "__", ANALYSIS_DESIGN_ID)
NUMERIC_TOL <- 1e-12
DUAL <- c("MDER", "nvRD")
if (REFRESH_FROZEN) {
  # The correction changes only availability at IS/1 d. All other paired
  # values, six-day scales and supports are identical, so their frozen A/B
  # summaries can be reused without enumerating windows or refitting models.
  frozen_paths <- file.path(OUT, c("rq3_joint_summary.csv", "rq3_joint_stability.rds"))
  frozen_joint <- readr::read_csv(frozen_paths[[1]], show_col_types = FALSE, progress = FALSE)
  frozen_pairs <- readRDS(frozen_paths[[2]])
  compatible <- paste0(c("rq3_v9_unresolved_decisions__", "rq3_v10_min2day_is__"),
                       RQ1_VERSION, "__", ANALYSIS_DESIGN_ID)
  for (z in list(frozen_joint, frozen_pairs)) {
    ms_assert_version(z, "core_artifact_version", CORE_VERSION)
    ms_assert_version(z, "rq1_analysis_version", RQ1_VERSION)
    ver <- unique(z$rq3_analysis_version)
    if (length(ver) != 1L || !ver %in% compatible) stop("Incompatible frozen RQ3 availability-refresh input")
  }
  if (!all(frozen_joint$resolution_s %in% PRIMARY_TEMPORAL_S) ||
      !all(frozen_joint$n_days %in% PRIMARY_DURATION_DAYS) ||
      !all(is.finite(frozen_pairs$A)) || any(frozen_pairs$A + NUMERIC_TOL < abs(frozen_pairs$B))) {
    stop("Invalid frozen RQ3 domain or pair summaries")
  }
  refresh_provenance <- tibble(path = frozen_paths, md5 = unname(tools::md5sum(frozen_paths)),
    source_rq3_version = c(unique(frozen_joint$rq3_analysis_version), unique(frozen_pairs$rq3_analysis_version)),
    refreshed_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
    output_rq3_version = RQ3_VERSION, correction = "single-day IS unavailable; all other A/B unchanged")
  audit_path <- file.path(DIAG, "rq3_availability_refresh_inputs.csv")
  if (file.exists(audit_path)) refresh_provenance <- bind_rows(
    readr::read_csv(audit_path, show_col_types = FALSE, col_types = cols(.default = col_character())),
    refresh_provenance)
  readr::write_csv(refresh_provenance, audit_path)
}

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

# -----------------------------------------------------------------------------
# Single-dimension observed residual instability on configuration TYPES.
# -----------------------------------------------------------------------------
ordered_pairs <- pair_summary |>
  filter(dimension %in% c("temporal", "duration"), is.finite(A_mean_absolute) |
    (dimension == "duration" & metric == "interdaily_stability" & config_a_label == "1 d"))
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
  ) |>
  # v7 finite pair summaries omit unavailable IS/1-d pairs. Keep that state
  # explicit even when it no longer appears as either endpoint of a pair.
  bind_rows(ordered_pairs |>
    filter(dimension == "duration", metric == "interdaily_stability") |>
    distinct(dimension, comparison_lattice, metric, metric_class, metric_geometry) |>
    mutate(state_id = "duration_1d", state_label = "1 d", requirement_rank = 1L)) |>
  distinct()
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
      if (key$dimension[[1]] == "duration" && key$metric[[1]] == "interdaily_stability" &&
          states$requirement_rank[[i]] < 2L) {
        return(tibble(state_id = c0, state_label = states$state_label[[i]],
          requirement_rank = states$requirement_rank[[i]], n_higher_observed = 0L,
          worst_refinement_config = NA_character_, R_obs = NA_real_,
          status = "unavailable", boundary = FALSE))
      }
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
      mutate(sufficient = if_else(status == "resolved" & is.finite(R_obs),
                                  R_obs <= epsilon + NUMERIC_TOL, NA))
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
    substitutable_at_epsilon_zero = if_else(is.finite(epsilon_entry), epsilon_entry <= NUMERIC_TOL, NA),
    core_artifact_version, rq1_analysis_version, rq3_analysis_version = RQ3_VERSION
  )
readr::write_csv(unordered, file.path(OUT, "rq3_unordered_substitutability.csv"), na = "")
coverage <- unordered |>
  group_by(dimension, comparison_pair_id, config_a_id, config_b_id) |>
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
    G = if_else(dimension == "duration" & metric == "interdaily_stability" &
      requirement_position < 2L, NA_real_, as.numeric(G)), rq3_analysis_version = RQ3_VERSION
  )
readr::write_csv(convergence, file.path(OUT, "rq3_convergence_profile.csv"), na = "")

# -----------------------------------------------------------------------------
# Joint temporal x duration stability, streamed over duration-cube parts.
# -----------------------------------------------------------------------------
if (!REFRESH_FROZEN) {
read_duration_primary <- function(path) {
  readRDS(path) |>
    duration_apply_availability() |>
    filter(resolution_s %in% PRIMARY_TEMPORAL_S, n_days %in% PRIMARY_DURATION_DAYS) |>
    select(support_id, site, Id, placement, optical, resolution_s, window_id, n_days,
           window_start, window_end, metric, metric_class, metric_geometry, value, available)
}

message("RQ3 v5: collect joint anchor values and observed joint-state catalogue")
anchor_parts <- vector("list", length(duration_part_paths))
state_parts <- vector("list", length(duration_part_paths))
for (i in seq_along(duration_part_paths)) {
  z <- read_duration_primary(duration_part_paths[[i]])
  anchor_parts[[i]] <- z |>
    filter(placement == "eye", optical == "MEDI", resolution_s == 10L, n_days == max(PRIMARY_DURATION_DAYS), available, is.finite(value)) |>
    select(support_id, metric, metric_geometry, value)
  state_parts[[i]] <- z |>
    ms_maximal_configuration_support() |>
    filter((available & is.finite(value)) | (metric == "interdaily_stability" & n_days < 2L)) |>
    distinct(support_id, placement, optical, resolution_s, n_days, metric, metric_class, metric_geometry)
  rm(z); invisible(gc(FALSE))
}
joint_anchor <- bind_rows(anchor_parts) |>
  group_by(support_id, metric, metric_geometry) |>
  summarise(standardizer = aggregate_scale(value, first(metric_geometry)), .groups = "drop")
joint_state_catalog <- bind_rows(state_parts) |>
  distinct() |>
  left_join(joint_anchor, by = c("support_id", "metric", "metric_geometry")) |>
  filter(is.finite(standardizer), standardizer > 0) |>
  select(-standardizer) |>
  mutate(config_id = paste0("r", resolution_s, "__d", n_days))
rm(anchor_parts, state_parts)
invisible(gc())

build_joint_part <- function(path) {
  z <- ms_maximal_configuration_support(read_duration_primary(path))
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
    inner_join(a, b, by = c("support_id", "site", "Id"), relationship = "many-to-many") |>
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
  exports = c("build_joint_part", "read_duration_primary", "duration_apply_availability", "ms_maximal_configuration_support", "PRIMARY_TEMPORAL_S", "PRIMARY_DURATION_DAYS",
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
} else {
  message("RQ3: refresh availability and projections from frozen pairs; no duration blocks or fits")
  joint_state_catalog <- frozen_joint |>
    select(support_id, placement, optical, resolution_s, n_days, config_id,
           metric, metric_class, metric_geometry)
  removed <- frozen_pairs |>
    filter(metric == "interdaily_stability" & (n_days_a < 2L | n_days_b < 2L))
  if (nrow(removed)) readr::write_csv(removed, file.path(DIAG, "rq3_removed_single_day_is_pairs.csv"))
  joint_pair_summary <- frozen_pairs |>
    filter(!(metric == "interdaily_stability" & (n_days_a < 2L | n_days_b < 2L)))
  attr(joint_pair_summary, "task_projection") <- NULL
}
if (nrow(joint_pair_summary) && any(joint_pair_summary$A + NUMERIC_TOL < abs(joint_pair_summary$B))) {
  stop("RQ3 joint A >= |B| invariant failed")
}
joint_pair_summary <- joint_pair_summary |>
  mutate(core_artifact_version = CORE_VERSION, rq1_analysis_version = RQ1_VERSION,
         rq3_analysis_version = RQ3_VERSION)
readr::write_csv(joint_pair_summary, file.path(OUT, "rq3_joint_pair_summary.csv"), na = "")
saveRDS(joint_pair_summary, file.path(OUT, "rq3_joint_stability.rds"), compress = "xz")
source("scripts/utils/rq3_joint_projection.R", local = TRUE)

boundary_audit <- single |>
  transmute(
    dimension, metric, state_id, n_higher_observed, R_obs, status,
    pass = if_else(status == "unavailable",
                   metric == "interdaily_stability" & dimension == "duration" & state_id == "duration_1d" & is.na(R_obs),
                   if_else(n_higher_observed == 0L,
                   is.na(R_obs) & status == "boundary_unresolved",
                   is.finite(R_obs) & status == "resolved"))
  )
readr::write_csv(boundary_audit, file.path(DIAG, "rq3_boundary_unresolved_audit.csv"), na = "")
if (any(!boundary_audit$pass)) stop("RQ3 boundary/status invariant failed")
joint_boundary_audit <- joint |>
  transmute(support_id, placement, optical, resolution_s, n_days, metric,
            n_higher_observed, R_obs, status,
            pass = if_else(status == "unavailable",
                           metric == "interdaily_stability" & n_days < 2L & is.na(R_obs),
                           if_else(n_higher_observed == 0L,
                           is.na(R_obs) & status == "boundary_unresolved",
                           is.finite(R_obs) & status == "resolved")))
readr::write_csv(joint_boundary_audit, file.path(DIAG, "rq3_joint_boundary_unresolved_audit.csv"), na = "")
if (any(!joint_boundary_audit$pass)) stop("RQ3 joint boundary/status invariant failed")
if (REFRESH_FROZEN) {
  unchanged <- !(frozen_joint$metric == "interdaily_stability" & frozen_joint$n_days < 2L)
  key <- c("support_id", "placement", "optical", "resolution_s", "n_days", "metric")
  before <- frozen_joint[unchanged, c(key, "R_obs", "status")] |> arrange(across(all_of(key)))
  after <- joint |> filter(!(metric == "interdaily_stability" & n_days < 2L)) |>
    select(all_of(c(key, "R_obs", "status"))) |> arrange(across(all_of(key)))
  stopifnot(isTRUE(all.equal(before, after, tolerance = 1e-12, check.attributes = FALSE)))
  message("Verified: every non-single-day-IS R_obs/status is unchanged")
}

writeLines(c(
  "# RQ3 run report", "",
  paste0("RQ1 upstream: ", RQ1_VERSION), paste0("RQ3 analysis version: ", RQ3_VERSION),
  paste0("Analysis design: ", ANALYSIS_DESIGN_ID),
  paste0("Frozen availability refresh: ", REFRESH_FROZEN, "; upstream RQ1 version is retained, not relabelled."),
  "Single-day IS is unavailable; IV remains eligible. All-target and Dynamics tasks retain the missing required target and NA decisions.",
  paste0("Primary temporal states: ", paste(PRIMARY_TEMPORAL_S, collapse = ", "), " s."),
  "Single-dimension R_obs is computed on temporal/duration configuration TYPES from RQ1 pair summaries.",
  "Threshold-like sufficient-set checks exclude unresolved boundaries and require false->true monotone ordering with increasing burden.",
  "Least-demanding sufficient state is the minimum-burden sufficient state when and only when the resolved sufficient set is threshold-like.",
  "Joint temporal-duration pairs are actual nested-window comparisons; equal duration implies the same observed dates.",
  "Joint A/B and R_obs are aggregated by generic (resolution, duration) configuration type within fixed support x placement x optical facets.",
  "Pareto dominance treats coarser temporal resolution and shorter monitoring duration as lower burden inside the sufficient region.",
  "Task bundles intersect the sufficient sets of every required target at a common tolerance, retaining target-specific maximal supports.",
  "The joint-stability RDS retains its pair-summary data frame and adds task inventory, availability, entry tolerance and full sufficient/Pareto intervals as task_projection metadata.",
  "Composition failure: max(temporal-only R, duration-only R) <= epsilon < joint R on the same starting state and scale.",
  "Both single-axis refinements must be observed. Counts retain pair-specific maximal supports; failure is not proof of non-additivity.",
  paste0("Joint duration-part workers: ", RQ3_PART_WORKERS)
), file.path(OUT, "RQ3_RUN_REPORT.md"))
message("RQ3 complete: ", RQ3_VERSION)
