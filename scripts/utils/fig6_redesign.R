# Fig. 6 display only. Inputs are frozen joint/task decisions and display grids.
# No new pooling, sufficiency classification, Pareto calculation or fitting.
# Explain a single frozen facet/tolerance slice in its existing display table.
# Axis requirements are conditional slices, not coordinates of one optimum.
ms_fig6_task_explanation <- function(tasks, inventory) {
  classes <- stats::setNames(inventory$metric_class, inventory$metric)
  collect_targets <- function(x) paste(sort(unique(unlist(strsplit(x[nzchar(x)], ";", fixed = TRUE)))), collapse = ";")
  target_classes <- function(x) paste(sort(unique(unname(classes[strsplit(x, ";", fixed = TRUE)[[1L]]]))), collapse = ";")
  by_interval <- tasks |> dplyr::group_by(task_id, resolution_s) |>
    dplyr::summarise(required_days_at_interval = if (any(sufficient %in% TRUE))
      min(n_days[sufficient %in% TRUE]) else NA_real_, .groups = "drop")
  by_days <- tasks |> dplyr::group_by(task_id, n_days) |>
    dplyr::summarise(required_interval_at_days_s = if (any(sufficient %in% TRUE))
      max(resolution_s[sufficient %in% TRUE]) else NA_real_, .groups = "drop")
  limits <- tasks |> dplyr::group_by(task_id) |>
    dplyr::summarise(frontier_limiting_targets = collect_targets(limiting_targets[pareto %in% TRUE]),
                     .groups = "drop")
  tasks <- tasks |>
    dplyr::mutate(
      limiting_target_classes = vapply(limiting_targets, target_classes, character(1)),
      # Level and dynamics are alternative goals, not nested target sets.
      comparison_task = unname(c(`temporal dynamics` = "level", all_targets = "temporal dynamics")[as.character(task_id)]),
      comparison_relation = dplyr::case_when(task_id == "temporal dynamics" ~ "alternative_target_bundle",
        task_id == "all_targets" ~ "expanded_target_bundle", TRUE ~ NA_character_)
    ) |>
    dplyr::left_join(limits, by = "task_id") |>
    dplyr::left_join(by_interval, by = c("task_id", "resolution_s")) |>
    dplyr::left_join(by_days, by = c("task_id", "n_days")) |>
    dplyr::left_join(dplyr::rename(by_interval, comparison_days = required_days_at_interval),
      by = c("comparison_task" = "task_id", "resolution_s")) |>
    dplyr::left_join(dplyr::rename(by_days, comparison_interval_s = required_interval_at_days_s),
      by = c("comparison_task" = "task_id", "n_days")) |>
    dplyr::mutate(
      extra_days_vs_comparison_at_interval = required_days_at_interval - comparison_days,
      sampling_rate_ratio_vs_comparison_at_days = comparison_interval_s / required_interval_at_days_s
    ) |>
    dplyr::select(-comparison_days, -comparison_interval_s)
  # Read the immediate lower-burden neighbours on the frozen lattice. Missing
  # cells are never skipped. Only a frozen FALSE decision identifies a blocker;
  # the current sufficient cell's largest R_task need not identify that metric.
  intervals <- sort(ms_primary_temporal_s())
  tasks <- tasks |>
    dplyr::mutate(shorter_n_days = n_days - 1L,
      coarser_resolution_s = c(intervals[-1L], NA_real_)[match(resolution_s, intervals)]) |>
    dplyr::left_join(tasks |> dplyr::transmute(task_id, resolution_s, shorter_n_days = n_days,
      shorter_status = status, shorter_sufficient = sufficient, shorter_R_task = R_task,
      shorter_limiting_targets = limiting_targets), by = c("task_id", "resolution_s", "shorter_n_days")) |>
    dplyr::left_join(tasks |> dplyr::transmute(task_id, n_days, coarser_resolution_s = resolution_s,
      coarser_status = status, coarser_sufficient = sufficient, coarser_R_task = R_task,
      coarser_limiting_targets = limiting_targets), by = c("task_id", "n_days", "coarser_resolution_s")) |>
    dplyr::mutate(
      shorter_status = dplyr::coalesce(shorter_status, ifelse(shorter_n_days < 1L, "outside_domain", "not_observed")),
      coarser_status = dplyr::coalesce(coarser_status, ifelse(is.na(coarser_resolution_s), "outside_domain", "not_observed")),
      shorter_blocking_targets = ifelse(pareto %in% TRUE & shorter_status == "resolved" &
        shorter_sufficient %in% FALSE, shorter_limiting_targets, NA_character_),
      coarser_blocking_targets = ifelse(pareto %in% TRUE & coarser_status == "resolved" &
        coarser_sufficient %in% FALSE, coarser_limiting_targets, NA_character_)) |>
    dplyr::select(-shorter_limiting_targets, -coarser_limiting_targets)
  # One compact key per exact limiting set retains all ties without long cell
  # labels. The key and full identities travel in the existing display CSV.
  blocker_sets <- sort(unique(stats::na.omit(c(tasks$shorter_blocking_targets, tasks$coarser_blocking_targets))))
  tasks |>
    dplyr::mutate(shorter_blocker_key = match(shorter_blocking_targets, blocker_sets),
                  coarser_blocker_key = match(coarser_blocking_targets, blocker_sets))
}

# Select an existing tolerance interval, preserving every frozen decision.
ms_fig6_frontier_display <- function(frontiers, epsilon = .50) {
  frontiers |>
    dplyr::filter(optical == "MEDI",
      epsilon_interval_start <= epsilon,
      epsilon_interval_end > epsilon | (terminal_endpoint & epsilon_interval_end >= epsilon)) |>
    dplyr::group_by(task_id, placement, optical, config_id) |>
    dplyr::slice_max(epsilon_interval_start, n = 1, with_ties = FALSE) |>
    dplyr::ungroup() |>
    dplyr::mutate(epsilon_display = epsilon,
      scheduled_records_saved = 100 * (1 - (n_days / resolution_s) / (6 / 10)))
}

ms_fig6_composition_display <- function(states, epsilon = .25) {
  states |>
    dplyr::mutate(epsilon_display = epsilon,
      # Membership in the already frozen half-open failure interval.
      failure_at_slice = dplyr::if_else(composition_resolved,
        ever_composition_failure & failure_interval_start <= epsilon &
          failure_interval_end > epsilon, NA))
}

# Project frozen tolerance intervals; never infer a new sufficient or Pareto set.
ms_fig6_tolerance_display <- function(frontiers, domain = c(.05, 1)) {
  z <- frontiers |>
    dplyr::filter(optical == "MEDI", epsilon_interval_end >= domain[1],
      epsilon_interval_start <= domain[2]) |>
    dplyr::mutate(x_start = pmax(epsilon_interval_start, domain[1]),
      x_end = pmin(epsilon_interval_end, domain[2]),
      records_saved = 100 * (1 - (n_days / resolution_s) / (6 / 10)))
  savings <- z |>
    dplyr::group_by(task_id, placement, optical, epsilon_interval_start,
      epsilon_interval_end, x_start, x_end, terminal_endpoint) |>
    dplyr::summarise(n_pareto = sum(pareto %in% TRUE),
      savings_low = if (any(pareto %in% TRUE)) min(records_saved[pareto %in% TRUE]) else NA_real_,
      savings_high = if (any(pareto %in% TRUE)) max(records_saved[pareto %in% TRUE]) else NA_real_,
      .groups = "drop")
  allocations <- z |>
    dplyr::filter(task_id == "all_targets", pareto %in% TRUE)
  list(savings = savings, allocations = allocations)
}

# Tabulate existing decisions at the evaluated tolerance slices. Missing frozen
# intervals remain explicit: a terminal endpoint is not permission to extrapolate.
ms_fig6_matrix_display <- function(frontiers, tolerances = c(.05, .1, .2, .25, .3, .5, 1)) {
  keys <- frontiers |> dplyr::filter(optical == "MEDI") |>
    dplyr::distinct(task_id, placement, optical, n_required)
  cells <- lapply(tolerances, function(eps) {
    selected <- ms_fig6_frontier_display(frontiers, eps)
    summary <- selected |> dplyr::group_by(task_id, placement, optical, n_required) |>
      dplyr::summarise(n_states = dplyr::n(), n_resolved = sum(status == "resolved"),
        all_unavailable = all(status == "unavailable"),
        n_pareto = sum(pareto %in% TRUE),
        savings_low = if (any(pareto %in% TRUE)) min(scheduled_records_saved[pareto %in% TRUE]) else NA_real_,
        savings_high = if (any(pareto %in% TRUE)) max(scheduled_records_saved[pareto %in% TRUE]) else NA_real_,
        .groups = "drop")
    keys |> dplyr::left_join(summary, by = c("task_id", "placement", "optical", "n_required")) |>
      dplyr::mutate(epsilon = eps, display_status = dplyr::case_when(
        is.na(n_states) ~ "outside_frozen_domain",
        all_unavailable ~ "unavailable",
        n_resolved == 0 ~ "unresolved",
        n_pareto == 0 ~ "no_sufficient_choice",
        TRUE ~ "pareto_choices"))
  }) |> dplyr::bind_rows()
  stopifnot(!anyDuplicated(cells[c("task_id", "placement", "optical", "epsilon")]))
  cells
}

ms_fig6_redesign <- function(frontiers, composition, joint) {
  # All panels query frozen analysis outputs; no diagnostic preview or derived
  # display CSV is an input to the canonical figure.
  task_states <- ms_fig6_frontier_display(frontiers, .50)
  portfolio <- task_states |> dplyr::filter(pareto %in% TRUE)
  projected <- ms_fig6_tolerance_display(frontiers)
  savings <- projected$savings
  failure <- composition |> dplyr::filter(summary_scope == "placement_optical")
  stopifnot(!anyDuplicated(failure[c("placement", "optical", "epsilon")]),
    all(is.na(failure$failure_rate[failure$n_axis_pass == 0])),
    all(abs(failure$failure_rate - failure$n_failure / failure$n_axis_pass) < 1e-10, na.rm = TRUE))
  all_ink <- "#263B46"
  palette <- c(MS_METRIC_COLORS, all_targets=all_ink)
  blue <- "#2C607A"; orange <- "#B46E40"; ink <- "#28343B"; muted <- "#8B989F"
  place <- function(x) factor(x, c("eye", "chest", "wrist"), c("Eye", "Chest", "Wrist"))
  cadences <- ms_primary_temporal_s()
  short <- c(all_targets="All 54", duration="Duration", `exposure history`="History",
             level="Level", spectrum="Spectrum", `temporal dynamics`="Dynamics", timing="Timing")
  base_theme <- theme_minimal(base_size=7, base_family=MS_FONT) +
    theme(panel.grid.minor=element_blank(),
          panel.grid.major=element_line(colour="#E6EBEE", linewidth=.25),
          axis.title=element_text(size=7,colour=ink), axis.text=element_text(size=6.4,colour=ink),
          strip.text=element_text(size=7.5,face="bold",colour=ink),
          panel.spacing.x=grid::unit(4,"mm"),
          plot.margin=margin(3,6,3,3), legend.position="none")

  # Exact shared coordinates have one marker and combined direct labels.
  points <- portfolio |> mutate(x=match(resolution_s,cadences)) |>
    group_by(placement,x,n_days) |>
    summarise(label=paste(unname(short[task_id]),collapse="\n"),
              all_targets=any(task_id=="all_targets"), n_choices=n(),.groups="drop")
  stopifnot(sum(points$n_choices)==nrow(portfolio))
  offsets <- read.table(text="placement x n_days tx ty hj
  eye 1 4 1.05 5.35 0
  eye 2 3 1.75 2.50 .5
  eye 2 4 1.85 4.58 .5
  eye 3 4 3.25 3.46 .5
  eye 4 5 4.00 5.55 .5
  eye 5 3 4.83 3.55 .5
  eye 6 2 5.88 1.23 1
  chest 1 4 1.10 3.45 0
  chest 1 5 1.10 5.58 0
  chest 3 3 2.80 2.48 .5
  chest 4 4 4.12 3.43 0
  chest 4 5 3.75 5.48 .5
  chest 5 6 5.08 6.47 .5
  chest 6 2 5.88 1.23 1
  wrist 1 3 1.12 2.53 0
  wrist 1 4 1.12 4.05 0
  wrist 1 5 1.12 5.46 0
  wrist 4 3 3.63 2.45 .5
  wrist 4 5 3.62 5.55 .5
  wrist 5 4 5.20 4.38 .5
  wrist 5 5 5.38 5.58 .5
  wrist 6 2 5.85 1.48 1
  wrist 6 3 5.87 3.18 1", header=TRUE)
  points <- left_join(points, offsets, by=c("placement","x","n_days")) |>
    mutate(placement=place(placement))
  stopifnot(!anyNA(points$tx))
  pa <- ggplot(points, aes(x,n_days)) +
    geom_segment(aes(xend=tx,yend=ty),colour="#C4CDD2",linewidth=.28) +
    geom_point(data=filter(points,!all_targets),size=2.15,shape=21,fill="#98ABB5",colour="white",stroke=.35) +
    geom_point(data=filter(points,all_targets),size=3.4,shape=21,fill=all_ink,colour="white",stroke=.45) +
    geom_text(aes(x=tx,y=ty,label=label,hjust=hj),size=2.05,colour=ink,
              lineheight=.94,fontface=ifelse(points$all_targets,"bold","plain"),family=MS_FONT) +
    facet_grid(~placement) +
    scale_x_continuous(breaks=1:6,labels=cadences,limits=c(.65,6.40),expand=c(0,0)) +
    scale_y_continuous(breaks=1:6,limits=c(.8,6.70),expand=c(0,0)) +
    labs(x="Sampling interval (s; observed levels)",y="Monitoring days") + base_theme

  # Restore every target bundle's tolerance profile. These are projections of
  # frozen intervals, with all Pareto savings endpoints retained.
  bundle_order <- c("duration","exposure history","level","spectrum",
                    "temporal dynamics","timing","all_targets")
  profiles <- savings |> arrange(placement,task_id,x_start) |>
    group_by(placement,task_id) |>
    mutate(previous_low=lag(savings_low),previous_high=lag(savings_high),previous_end=lag(x_end)) |>
    ungroup() |> mutate(placement=place(placement),task_id=factor(task_id,bundle_order)) |>
    filter(n_pareto>0)
  stopifnot(setequal(as.character(unique(profiles$task_id)),bundle_order))
  profile_subset <- filter(profiles,task_id!="all_targets")
  profile_all <- filter(profiles,task_id=="all_targets")
  selected <- profile_all |> filter(x_start<=.5,x_end>.5) |>
    mutate(label=sprintf("%.1f%%",savings_low))
  entries <- profile_all |> group_by(placement) |> summarise(x=min(x_start),.groups="drop")
  pb2 <- ggplot(profiles,aes(colour=task_id)) +
    geom_vline(xintercept=.5,colour="#A5AFB5",linewidth=.28,linetype=3) +
    geom_rect(aes(xmin=x_start,xmax=x_end,ymin=savings_low,ymax=savings_high,fill=task_id),
              colour=NA,alpha=.12,show.legend=FALSE) +
    geom_segment(data=profile_subset,aes(x=x_start,xend=x_end,y=savings_low,yend=savings_low),linewidth=.35,alpha=.78) +
    geom_segment(data=filter(profile_subset,savings_high>savings_low+1e-9),
                 aes(x=x_start,xend=x_end,y=savings_high,yend=savings_high),linewidth=.30,alpha=.7) +
    geom_segment(data=filter(profile_subset,is.finite(previous_low),abs(x_start-previous_end)<1e-9),
                 aes(x=x_start,xend=x_start,y=previous_low,yend=savings_low),linewidth=.3,alpha=.7) +
    geom_segment(data=filter(profile_subset,is.finite(previous_high),abs(x_start-previous_end)<1e-9),
                 aes(x=x_start,xend=x_start,y=previous_high,yend=savings_high),linewidth=.3,alpha=.7) +
    geom_segment(data=profile_all,aes(x=x_start,xend=x_end,y=savings_low,yend=savings_low),linewidth=.85) +
    geom_segment(data=filter(profile_all,savings_high>savings_low+1e-9),
                 aes(x=x_start,xend=x_end,y=savings_high,yend=savings_high),linewidth=.65) +
    geom_segment(data=filter(profile_all,is.finite(previous_low),abs(x_start-previous_end)<1e-9),
                 aes(x=x_start,xend=x_start,y=previous_low,yend=savings_low),linewidth=.65) +
    geom_segment(data=filter(profile_all,is.finite(previous_high),abs(x_start-previous_end)<1e-9),
                 aes(x=x_start,xend=x_start,y=previous_high,yend=savings_high),linewidth=.45) +
    geom_point(data=selected,aes(x=.5,y=savings_low),size=1.6) +
    geom_text(data=selected,aes(x=.51,y=savings_low+11,label=label),colour=ink,hjust=0,size=1.95,family=MS_FONT) +
    geom_point(data=entries,aes(x=x,y=0),inherit.aes=FALSE,shape=25,fill=all_ink,colour=all_ink,size=1.7) +
    scale_colour_manual(values=palette,breaks=bundle_order,labels=unname(short[bundle_order]),name=NULL) +
    scale_fill_manual(values=palette,guide="none") +
    facet_grid(~placement) +
    scale_x_continuous(breaks=c(.05,.25,.5,.75,1),labels=c(".05",".25",".50",".75","1.00"),
                       limits=c(.025,1.03),expand=c(0,0)) +
    scale_y_continuous(breaks=c(0,50,100),limits=c(-5,104),expand=c(0,0)) +
    labs(x="Tolerance ε",y="Scheduled records\nsaved (%)") + base_theme +
    theme(strip.text=element_blank(),panel.grid.major.x=element_blank())
  legend <- get_legend(pb2+theme(legend.position="bottom",legend.text=element_text(size=6),
                                 legend.key.width=grid::unit(3,"mm"),legend.key.height=grid::unit(2,"mm"),
                                 legend.margin=margin(0,0,0,0),legend.box.margin=margin(0,0,0,0))+
                         guides(colour=guide_legend(nrow=1,override.aes=list(linewidth=.8,alpha=1))))

  # Use the existing selected state plus its immediate lower-burden neighbours.
  # R_task and sufficient flags are read, never recalculated for display.
  chosen <- portfolio |> filter(task_id=="all_targets")
  # This accepted layout has one all-target Pareto choice per placement. Fail
  # explicitly if a future analysis changes this rather than choosing a winner.
  stopifnot(nrow(chosen)==3L, !anyDuplicated(chosen$placement))
  query <- bind_rows(
    chosen |> transmute(placement,optical,resolution_s,n_days,role=3L),
    chosen |> transmute(placement,optical,resolution_s,n_days=n_days-1L,role=2L),
    chosen |> transmute(placement,optical,resolution_s=cadences[match(resolution_s,cadences)+1L],n_days,role=1L))
  state_detail <- task_states |> filter(task_id=="all_targets") |>
    inner_join(query,by=c("placement","optical","resolution_s","n_days"))
  metric_detail <- joint |> inner_join(query,by=c("placement","optical","resolution_s","n_days"))
  stopifnot(nrow(state_detail)==3L*nrow(chosen),
            all(metric_detail$status=="resolved"),
            all(state_detail$sufficient[state_detail$role==3] %in% TRUE),
            all(state_detail$sufficient[state_detail$role!=3] %in% FALSE))
  check <- metric_detail |> group_by(placement,role) |>
    summarise(n=n_distinct(metric),rows=n(),maximum=max(R_obs),.groups="drop") |>
    left_join(state_detail |> select(placement,role,R_task,n_required),by=c("placement","role"))
  stopifnot(all(check$n==check$n_required),all(check$rows==check$n),all(abs(check$maximum-check$R_task)<1e-10))
  metric_names <- c(mean_timing_above_1000="Mean timing > 1,000",
                    mean_timing_above_250="Mean timing > 250",
                    mean_timing_above_10="Mean timing > 10",
                    disparity_index="Disparity index")
  state_detail_export <- state_detail
  metric_detail_export <- metric_detail
  stopifnot(all(state_detail$limiting_targets %in% names(metric_names)))
  state_detail <- state_detail |> mutate(placement=place(placement),
    description=paste0(resolution_s," s × ",n_days," d  ·  ",unname(metric_names[limiting_targets])))
  metric_detail <- metric_detail |> mutate(placement=place(placement))
  stopifnot(!anyNA(state_detail$description))
  pc2 <- ggplot(metric_detail,aes(R_obs,role)) +
    annotate("rect",xmin=.5,xmax=1.4,ymin=.5,ymax=3.7,fill="#F6EDE6",alpha=.65) +
    geom_hline(yintercept=1:3,colour="#E6EBEE",linewidth=.25) +
    geom_vline(xintercept=.5,linetype=2,colour="#986B4A",linewidth=.4) +
    geom_point(aes(colour=metric_class),position=position_jitter(width=0,height=.085,seed=246),
               size=.75,alpha=.62) +
    geom_point(data=state_detail,aes(x=R_task),shape=18,size=2.05,colour=all_ink) +
    geom_text(data=state_detail,aes(x=R_task+.035,label=sprintf("%.3f",R_task)),
              hjust=0,size=1.95,colour=ink,family=MS_FONT) +
    geom_text(data=state_detail,aes(x=.02,y=role+.32,label=description),
              hjust=0,size=1.87,colour=ink,family=MS_FONT) +
    scale_colour_manual(values=palette) + facet_grid(~placement) +
    scale_x_continuous(limits=c(0,1.4),breaks=c(0,.5,1),expand=c(0,0)) +
    scale_y_continuous(limits=c(.60,3.64),breaks=1:3,labels=c("Coarser","1 day less","Chosen"),expand=c(0,0)) +
    labs(x="Residual instability, R",y=NULL) + base_theme +
    theme(strip.text=element_blank(),panel.grid=element_blank(),axis.text.y=element_text(size=6.0))

  # Keep all optical facets and tolerances; provide exact counts at the .25 slice.
  ff <- failure |> mutate(placement=place(placement),rate=100*failure_rate,
                           low_n=n_axis_pass>0 & n_axis_pass<10)
  count_key <- ff |> filter(epsilon==.25) |>
    mutate(key_y=ifelse(optical=="MEDI",49,40),key_label=paste0(optical,"  ",n_failure,"/",n_axis_pass))
  pd2 <- ggplot(ff,aes(epsilon,rate,colour=optical,group=optical)) +
    geom_vline(xintercept=.5,colour="#A5AFB5",linewidth=.28,linetype=3) +
    geom_line(aes(linetype=optical),linewidth=.48,na.rm=TRUE) +
    geom_point(data=filter(ff,n_axis_pass>=10),size=1.4) +
    geom_point(data=filter(ff,low_n),shape=21,fill="white",size=2,stroke=.65) +
    geom_point(data=filter(ff,n_axis_pass==0),aes(y=-4),shape=4,size=1.5,stroke=.45) +
    annotate("text",x=.57,y=57,label="Counts at ε = .25",hjust=0,size=1.8,colour=ink,family=MS_FONT) +
    geom_segment(data=count_key,aes(x=.56,xend=.62,y=key_y,yend=key_y,linetype=optical),linewidth=.55) +
    geom_text(data=count_key,aes(x=.65,y=key_y,label=key_label),colour=ink,hjust=0,size=1.8,family=MS_FONT) +
    geom_text(data=filter(ff,placement=="Chest",low_n),aes(x=.095,y=53,label=paste0(n_failure,"/",n_axis_pass)),
              colour=ink,hjust=0,size=1.9,family=MS_FONT) +
    geom_text(data=filter(ff,placement=="Wrist",low_n,optical=="MEDI"),
              aes(x=.08,y=6,label=paste0(n_failure,"/",n_axis_pass," each")),colour=ink,hjust=0,size=1.8,family=MS_FONT) +
    scale_colour_manual(values=c(MEDI=blue,LIGHT=orange)) +
    scale_linetype_manual(values=c(MEDI=1,LIGHT=2)) + facet_grid(~placement) +
    scale_x_continuous(breaks=c(.05,.25,.5,.75,1),labels=c(".05",".25",".50",".75","1.00"),
                       limits=c(.025,1.03),expand=c(0,0)) +
    scale_y_continuous(breaks=c(0,10,25,50),limits=c(-7,62),expand=c(0,0)) +
    labs(x="Tolerance ε",y="Joint failures among\naxis passes (%)") + base_theme +
    theme(strip.text=element_blank(),panel.grid.major.x=element_blank())

  fields <- align_plots(pa,pb2,pc2,pd2,align="v",axis="lr")
  title <- function(label) ggdraw()+draw_label(label,x=.079,hjust=0,y=.5,
    size=8.2,fontface="bold",fontfamily=MS_FONT,colour=ink)
  # The accepted main figure has no footnote block. Explanatory definitions live
  # in FIGURE_ARCHITECTURE and the manuscript caption; axes and keys stay visible.
  final <- plot_grid(
    title("a  Target choice changes the sufficient joint design · ε = .50"),fields[[1]],
    title("b  Tolerance profiles retain differences between target bundles"),fields[[2]],legend,
    title("c  Which targets prevent another reduction? · ε = .50"),fields[[3]],
    title("d  Separate-axis sufficiency does not always compose"),fields[[4]],
    ncol=1,rel_heights=c(.24,2.50,.24,1.40,.23,.24,1.50,.24,1.62))
  list(plot=final,a=pa,b=pb2,c=pc2,d=pd2,
    savings=projected$savings,allocations=projected$allocations,failure=failure,
    design_coordinates=points,state_detail=state_detail_export,metric_detail=metric_detail_export)
}
