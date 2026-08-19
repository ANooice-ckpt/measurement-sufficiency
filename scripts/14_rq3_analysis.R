suppressPackageStartupMessages({library(tidyverse);library(LightLogR)})
source("scripts/utils/protocol_windows.R")
# RQ3: inverse sufficiency + multidimensional facet-specific Pareto frontiers.
RQ1_SUMMARY<-"results/rq1/rq1_summary.csv";CORE_METRICS<-"data/derived/core/metric_cube.csv.gz";CORE_CONTEXT<-"data/derived/core/unit_context.csv.gz";OUT_DATA<-"data/derived/rq3";OUT_RESULTS<-"results/rq3";OUT_DIAG<-"results/diagnostics"
PRIMARY_TEMPORAL_S<-c(10L,20L,30L,60L,300L,900L,1800L);ISIV_METRICS<-c("interdaily_stability","intradaily_variability");DUAL_CHANNEL_METRICS<-c("MDER","nvRD");NUMERIC_TOL<-1e-12
for(p in c(RQ1_SUMMARY,CORE_METRICS,CORE_CONTEXT))if(!file.exists(p))stop("Missing RQ3 input: ",p);for(d in c(OUT_DATA,OUT_RESULTS,OUT_DIAG))dir.create(d,recursive=TRUE,showWarnings=FALSE)
message("RQ3: read RQ1 + core artifacts")
rq1<-readr::read_csv(RQ1_SUMMARY,show_col_types=FALSE,progress=FALSE);cube<-readr::read_csv(CORE_METRICS,show_col_types=FALSE,progress=FALSE)|>mutate(Date=as.Date(Date));context<-readr::read_csv(CORE_CONTEXT,show_col_types=FALSE,progress=FALSE)|>mutate(Date=as.Date(Date),protocol_start_date=as.Date(protocol_start_date),protocol_end_date=as.Date(protocol_end_date))
if(!"rq1_analysis_version"%in%names(rq1))stop("RQ1 summary predates v2");RQ1_VERSION<-unique(na.omit(rq1$rq1_analysis_version));if(length(RQ1_VERSION)!=1)stop("RQ1 version mismatch");RQ1_VERSION<-RQ1_VERSION[[1]];CORE_VERSION<-unique(na.omit(rq1$core_artifact_version));if(length(CORE_VERSION)!=1)stop("core version mismatch");CORE_VERSION<-CORE_VERSION[[1]];if(any(cube$resolution_s==15L))stop("Stale 15-s core")
meta<-cube|>distinct(metric,metric_class,metric_scope,metric_geometry);if(n_distinct(meta$metric)!=54)stop("Expected 54 metrics")
safe_q<-function(x,p){x<-x[is.finite(x)];if(length(x))unname(quantile(x,p,names=FALSE,type=7))else NA_real_};circular_delta<-function(a,b,period=86400)((a-b+period/2)%%period)-period/2;circular_mean<-function(x,period=86400){x<-x[is.finite(x)];if(!length(x))return(NA_real_);th<-2*pi*x/period;(atan2(mean(sin(th)),mean(cos(th)))%%(2*pi))*period/(2*pi)};agg<-function(x,g){if(!length(x)||any(!is.finite(x)))return(NA_real_);if(identical(g,"circular_time"))circular_mean(x)else mean(x)};refscale<-function(x,g){x<-x[is.finite(x)];if(length(x)<2)return(NA_real_);if(identical(g,"circular_time"))sd(circular_delta(x,circular_mean(x)))else sd(x)};tlabel<-function(x)case_when(x<60~paste0(x," s"),x%%60==0~paste0(x%/%60," min"),TRUE~paste0(x," s"))

# -----------------------------------------------------------------------------
# 1. Single-dimension inverse sufficiency from frozen RQ1 A
# -----------------------------------------------------------------------------
message("RQ3: single-dimension inverse sufficiency")
single_base<-rq1|>filter(is.finite(A_mean_absolute))|>transmute(dimension,configuration,configuration_label,configuration_order,metric,metric_class,metric_geometry,A=A_mean_absolute,B=B_mean_signed,is_reference=FALSE,requirement_rank=if_else(dimension%in%c("temporal","duration"),configuration_order+1L,NA_integer_))
om<-single_base|>filter(dimension%in%c("temporal","duration"))|>distinct(dimension,metric,metric_class,metric_geometry)
sref<-bind_rows(om|>filter(dimension=="temporal")|>transmute(dimension,configuration="10s",configuration_label="10 s",configuration_order=0L,metric,metric_class,metric_geometry,A=0,B=0,is_reference=TRUE,requirement_rank=1L),om|>filter(dimension=="duration")|>transmute(dimension,configuration="7d",configuration_label="7 d",configuration_order=0L,metric,metric_class,metric_geometry,A=0,B=0,is_reference=TRUE,requirement_rank=1L))
levels<-bind_rows(single_base,sref)|>distinct(dimension,configuration,configuration_label,metric,.keep_all=TRUE)|>arrange(dimension,metric,requirement_rank,configuration_order)
readr::write_csv(levels,file.path(OUT_RESULTS,"rq3_single_dimension_levels.csv"),na="")
mono<-levels|>filter(dimension%in%c("temporal","duration"))|>arrange(dimension,metric,requirement_rank)|>group_by(dimension,metric,metric_class)|>summarise(n_levels=n(),response_monotone=all(diff(A)>=-NUMERIC_TOL),max_reverse_step=if(n()>1)min(diff(A),na.rm=TRUE)else 0,.groups="drop");readr::write_csv(mono,file.path(OUT_DIAG,"rq3_single_dimension_monotonicity.csv"),na="")
state_one<-function(g){eps<-sort(unique(c(0,g$A[is.finite(g$A)])));st<-tidyr::crossing(epsilon=eps,row_id=seq_len(nrow(g)))|>left_join(g|>mutate(row_id=row_number()),by="row_id")|>mutate(sufficient=is.finite(A)&A<=epsilon+NUMERIC_TOL);if(!first(g$dimension)%in%c("temporal","duration"))return(st|>mutate(sufficient_set_threshold_like=NA,least_demanding_rank=NA_integer_,least_demanding_configuration=NA_character_,least_demanding_label=NA_character_,minimum_requirement_interpretable=NA));req<-st|>arrange(epsilon,requirement_rank)|>group_by(epsilon)|>group_modify(~{flags<-.x$sufficient;tl<-all(diff(as.integer(flags))<=0L);ok<-.x|>filter(sufficient);if(!nrow(ok))return(tibble(sufficient_set_threshold_like=tl,least_demanding_rank=NA_integer_,least_demanding_configuration=NA_character_,least_demanding_label=NA_character_));best<-ok|>slice_max(requirement_rank,n=1,with_ties=FALSE);tibble(sufficient_set_threshold_like=tl,least_demanding_rank=best$requirement_rank,least_demanding_configuration=best$configuration,least_demanding_label=best$configuration_label)})|>ungroup()|>mutate(minimum_requirement_interpretable=sufficient_set_threshold_like);st|>left_join(req,by="epsilon")}
single_state<-levels|>group_by(dimension,metric)|>group_split(.keep=TRUE)|>map_dfr(state_one);saveRDS(single_state,file.path(OUT_DATA,"rq3_single_dimension_sufficiency.rds"),compress="xz")
single_req<-single_state|>filter(dimension%in%c("temporal","duration"))|>distinct(dimension,metric,metric_class,epsilon,sufficient_set_threshold_like,least_demanding_rank,least_demanding_configuration,least_demanding_label,minimum_requirement_interpretable)|>arrange(dimension,metric,epsilon);readr::write_csv(single_req,file.path(OUT_RESULTS,"rq3_single_dimension_requirement.csv"),na="")
unordered<-levels|>filter(dimension%in%c("placement","optical"),!is_reference)|>transmute(dimension,configuration,configuration_label,metric,metric_class,metric_geometry,epsilon_entry=A,B_at_entry=B);readr::write_csv(unordered,file.path(OUT_RESULTS,"rq3_unordered_sufficiency_thresholds.csv"),na="")
coverage_one<-function(g){
  eps<-sort(unique(c(0,g$epsilon_entry[is.finite(g$epsilon_entry)])))
  tibble(
    dimension=first(g$dimension),configuration=first(g$configuration),configuration_label=first(g$configuration_label),
    metric_class=if("metric_class"%in%names(g))as.character(first(g$metric_class))else "All",
    epsilon=eps,n_metrics=n_distinct(g$metric),
    fraction_metrics_sufficient=map_dbl(eps,~mean(g$epsilon_entry<=.x+NUMERIC_TOL,na.rm=TRUE))
  )
}
coverage_all<-unordered|>
  group_by(dimension,configuration,configuration_label)|>
  group_split(.keep=TRUE)|>map_dfr(function(g){z<-coverage_one(g);z$metric_class<-"All";z})
coverage_class<-unordered|>
  group_by(dimension,configuration,configuration_label,metric_class)|>
  group_split(.keep=TRUE)|>map_dfr(coverage_one)
coverage<-bind_rows(coverage_all,coverage_class)
readr::write_csv(coverage,file.path(OUT_RESULTS,"rq3_unordered_coverage_curves.csv"),na="")

# -----------------------------------------------------------------------------
# 2. Actual multidimensional configs on facet-specific maximal supports
# -----------------------------------------------------------------------------
message("RQ3: facet-specific multidimensional configuration values")
# Non-dual targets use maximal MEDI supports; MDER/nvRD use matching *_full
# supports. LIGHT candidate facets necessarily use full support and cannot define
# MDER/nvRD. Unordered placement/optical facets are never forced onto a common
# all-position cohort merely for Pareto comparison.
plan<-tribble(~placement,~optical,~support_id,~metric_group,
 "eye","MEDI","eye_medi","nondual","eye","MEDI","eye_full","dual","eye","LIGHT","eye_full","nondual",
 "chest","MEDI","eye_chest_medi","nondual","chest","MEDI","eye_chest_full","dual","chest","LIGHT","eye_chest_full","nondual",
 "wrist","MEDI","eye_wrist_medi","nondual","wrist","MEDI","eye_wrist_full","dual","wrist","LIGHT","eye_wrist_full","nondual")

hour_cols<-grep("^isiv_h\\d\\d$",names(context),value=TRUE);if(length(hour_cols)!=24)stop("Expected 24 IS/IV hourly basis columns")
isiv_basis<-function(x){long<-x|>select(Date,all_of(hour_cols))|>pivot_longer(all_of(hour_cols),names_to="hn",values_to="v")|>mutate(hour=as.integer(sub("^isiv_h","",hn)),Datetime=as.POSIXct(as.numeric(Date)*86400+hour*3600,origin="1970-01-01",tz="UTC"))|>arrange(Datetime);is<-tryCatch(suppressWarnings(LightLogR::interdaily_stability(long$v,long$Datetime,na.rm=TRUE,as.df=FALSE)),error=function(e)NA_real_);iv<-tryCatch(suppressWarnings(LightLogR::intradaily_variability(long$v,long$Datetime,na.rm=TRUE,as.df=FALSE)),error=function(e)NA_real_);tibble(metric=ISIV_METRICS,value=c(as.numeric(is),as.numeric(iv)))}

# Protocol cohorts are support-specific, based on the eye-MEDI 10-s reference on
# exactly the same support used by the candidate facet.
supports <- unique(plan$support_id)
cohorts <- list(); windows <- list(); window_membership <- list(); reference_membership <- list(); cohort_audit <- list()
for (sup in supports) {
  cx <- context |>
    filter(support_id == sup, placement == "eye", optical == "MEDI", resolution_s == 10L) |>
    distinct(support_id, site, Id, Date, .keep_all = TRUE)
  co <- protocol_reference_cohort(cx)
  cohorts[[sup]] <- co
  cohort_audit[[sup]] <- protocol_reference_audit_table(co) |> mutate(facet_support = sup)
  ww <- make_protocol_duration_windows(co |> filter(eligible_protocol_7), include_reference = TRUE)
  windows[[sup]] <- ww
  if (nrow(ww)) {
    window_membership[[sup]] <- ww |>
      select(support_id, site, Id, reference_id, window_id, n_days, selected_dates) |>
      tidyr::unnest_longer(selected_dates, values_to = "Date") |>
      mutate(Date = as.Date(Date))
  } else window_membership[[sup]] <- tibble()
  eco <- co |> filter(eligible_protocol_7)
  if (nrow(eco)) {
    reference_membership[[sup]] <- eco |>
      select(support_id, site, Id, reference_id, reference_dates) |>
      tidyr::unnest_longer(reference_dates, values_to = "Date") |>
      mutate(Date = as.Date(Date))
  } else reference_membership[[sup]] <- tibble()
}
readr::write_csv(bind_rows(cohort_audit), file.path(OUT_DIAG, "rq3_joint_duration_cohort_audit.csv"), na = "")

pair_blocks <- list(); pb <- 0L
for (pi in seq_len(nrow(plan))) {
  pl <- plan[pi, ]
  sup <- pl$support_id
  metric_set <- if (pl$metric_group == "dual") DUAL_CHANNEL_METRICS else setdiff(meta$metric, DUAL_CHANNEL_METRICS)
  co <- cohorts[[sup]] |> filter(eligible_protocol_7)
  wm <- window_membership[[sup]]
  rm <- reference_membership[[sup]]
  if (!nrow(co) || !nrow(wm) || !nrow(rm)) next

  # Reference daily representations: eye MEDI, 10 s, fixed protocol Days 1-7.
  ref_daily <- cube |>
    filter(
      support_id == sup, analysis_unit_type == "participant_day",
      placement == "eye", optical == "MEDI", resolution_s == 10L,
      metric %in% metric_set, !metric %in% ISIV_METRICS
    ) |>
    inner_join(rm, by = c("support_id", "site", "Id", "Date")) |>
    group_by(support_id, site, Id, reference_id, metric, metric_class, metric_scope, metric_geometry) |>
    summarise(
      n_reference_days = n_distinct(Date),
      reference_available = n_reference_days == 7L && all(replace_na(available, FALSE) & is.finite(value)),
      reference_value = if (n_reference_days == 7L && all(replace_na(available, FALSE) & is.finite(value))) {
        agg(value, first(metric_geometry))
      } else NA_real_,
      .groups = "drop"
    )

  # Candidate daily representations are vectorized over every contiguous window.
  cand_daily <- cube |>
    filter(
      support_id == sup, analysis_unit_type == "participant_day",
      placement == pl$placement, optical == pl$optical,
      resolution_s %in% PRIMARY_TEMPORAL_S,
      metric %in% metric_set, !metric %in% ISIV_METRICS
    ) |>
    inner_join(wm, by = c("support_id", "site", "Id", "Date")) |>
    group_by(
      support_id, site, Id, reference_id, window_id, n_days,
      placement, optical, resolution_s,
      metric, metric_class, metric_scope, metric_geometry
    ) |>
    summarise(
      n_candidate_days = n_distinct(Date),
      candidate_available = n_candidate_days == first(n_days) && all(replace_na(available, FALSE) & is.finite(value)),
      candidate_value = if (n_candidate_days == first(n_days) && all(replace_na(available, FALSE) & is.finite(value))) {
        agg(value, first(metric_geometry))
      } else NA_real_,
      .groups = "drop"
    ) |>
    left_join(
      ref_daily,
      by = c("support_id", "site", "Id", "reference_id", "metric", "metric_class", "metric_scope", "metric_geometry")
    ) |>
    transmute(
      support_id, site, Id, reference_id, window_id, n_days,
      placement, optical, resolution_s, metric, metric_class, metric_geometry,
      reference_value, candidate_value,
      pair_available = reference_available & candidate_available & is.finite(reference_value) & is.finite(candidate_value)
    )
  pb <- pb + 1L; pair_blocks[[pb]] <- cand_daily

  # IS/IV belong to the non-dual metric branch. We still evaluate the LightLogR
  # operators directly, but construct all window/config groups with one join rather
  # than repeatedly filtering the full context table.
  if (pl$metric_group == "nondual") {
    ref_ctx <- context |>
      filter(support_id == sup, placement == "eye", optical == "MEDI", resolution_s == 10L) |>
      inner_join(rm, by = c("support_id", "site", "Id", "Date")) |>
      group_by(support_id, site, Id, reference_id) |>
      group_modify(~isiv_basis(.x)) |>
      ungroup() |>
      left_join(meta, by = "metric") |>
      transmute(support_id, site, Id, reference_id, metric, metric_class, metric_geometry,
                reference_value = value, reference_available = is.finite(value))

    cand_ctx <- context |>
      filter(
        support_id == sup, placement == pl$placement, optical == pl$optical,
        resolution_s %in% PRIMARY_TEMPORAL_S
      ) |>
      inner_join(wm, by = c("support_id", "site", "Id", "Date")) |>
      group_by(support_id, site, Id, reference_id, window_id, n_days, placement, optical, resolution_s) |>
      group_modify(~isiv_basis(.x)) |>
      ungroup() |>
      left_join(meta, by = "metric") |>
      transmute(support_id, site, Id, reference_id, window_id, n_days, placement, optical, resolution_s,
                metric, metric_class, metric_geometry, candidate_value = value, candidate_available = is.finite(value)) |>
      left_join(ref_ctx, by = c("support_id", "site", "Id", "reference_id", "metric", "metric_class", "metric_geometry")) |>
      transmute(
        support_id, site, Id, reference_id, window_id, n_days, placement, optical, resolution_s,
        metric, metric_class, metric_geometry, reference_value, candidate_value,
        pair_available = reference_available & candidate_available & is.finite(reference_value) & is.finite(candidate_value)
      )
    pb <- pb + 1L; pair_blocks[[pb]] <- cand_ctx
  }
}
joint_pairs<-bind_rows(pair_blocks)
if(!nrow(joint_pairs)){
  joint_canonical<-tibble()
  joint_sufficiency<-tibble()
  joint_summary<-tibble(
    support_id=character(),placement=character(),optical=character(),resolution_s=integer(),n_days=integer(),
    metric=character(),metric_class=character(),metric_geometry=character(),n_participants=integer(),n_units=integer(),
    median_e=double(),q25_e=double(),q75_e=double(),p025_e=double(),p975_e=double(),
    B_mean_signed=double(),A_mean_absolute=double(),temporal_label=character(),joint_configuration=character(),epsilon_entry=double()
  )
  pareto_ever<-tibble(
    metric=character(),metric_class=character(),support_id=character(),placement=character(),optical=character(),
    resolution_s=integer(),temporal_label=character(),n_days=integer(),joint_configuration=character(),
    epsilon_entry=double(),A_mean_absolute=double(),ever_pareto=logical(),first_pareto_epsilon=double(),last_pareto_epsilon=double()
  )
  pareto_frequency<-tibble(
    placement=character(),optical=character(),resolution_s=integer(),temporal_label=character(),n_days=integer(),
    n_metrics_available=integer(),n_metrics_ever_pareto=integer(),fraction_metrics_ever_pareto=double()
  )
  representatives<-tibble(metric=character(),metric_class=character(),n_joint_configs=integer(),median_entry=double(),entry_range=double(),n_ever_pareto=integer(),selection_score=double())
}else{
  std<-joint_pairs|>filter(is.finite(reference_value))|>distinct(support_id,site,Id,metric,metric_geometry,reference_id,reference_value)|>group_by(support_id,metric,metric_geometry)|>summarise(n_reference_participants=n_distinct(paste(site,Id,sep="|")),standardizer=refscale(reference_value,first(metric_geometry)),.groups="drop")|>mutate(valid=is.finite(standardizer)&standardizer>sqrt(.Machine$double.eps));readr::write_csv(std,file.path(OUT_DIAG,"rq3_joint_standardizer_audit.csv"),na="")
  joint_canonical<-joint_pairs|>left_join(std,by=c("support_id","metric","metric_geometry"))|>mutate(delta=if_else(metric_geometry=="circular_time",circular_delta(candidate_value,reference_value),candidate_value-reference_value),available=pair_available&replace_na(valid,FALSE)&is.finite(delta)&is.finite(standardizer),e=if_else(available,delta/standardizer,NA_real_),core_artifact_version=CORE_VERSION,rq1_analysis_version=RQ1_VERSION)
  joint_summary<-joint_canonical|>filter(available,is.finite(e))|>group_by(support_id,placement,optical,resolution_s,n_days,metric,metric_class,metric_geometry)|>summarise(n_participants=n_distinct(paste(site,Id,sep="|")),n_units=n(),median_e=median(e),q25_e=safe_q(e,.25),q75_e=safe_q(e,.75),p025_e=safe_q(e,.025),p975_e=safe_q(e,.975),B_mean_signed=mean(e),A_mean_absolute=mean(abs(e)),.groups="drop")|>mutate(temporal_label=tlabel(resolution_s),joint_configuration=paste(placement,optical,temporal_label,paste0(n_days," d"),sep=" | "),epsilon_entry=A_mean_absolute)
  if(any(joint_summary$A_mean_absolute+NUMERIC_TOL<abs(joint_summary$B_mean_signed)))stop("RQ3 joint A >= |B| failed")
  # Full epsilon states and Pareto within fixed unordered facet.
  pareto_one<-function(g){eps<-sort(unique(c(0,g$epsilon_entry)));map_dfr(eps,function(ep){z<-g|>mutate(epsilon=ep,sufficient=epsilon_entry<=ep+NUMERIC_TOL,pareto=FALSE);idx<-which(z$sufficient);if(length(idx))for(ii in idx){dom<-any(vapply(idx,function(jj){if(jj==ii)return(FALSE);no_more<-z$resolution_s[jj]>=z$resolution_s[ii]&&z$n_days[jj]<=z$n_days[ii];strict<-z$resolution_s[jj]>z$resolution_s[ii]||z$n_days[jj]<z$n_days[ii];no_more&&strict},logical(1)));z$pareto[ii]<-!dom};z})}
  joint_sufficiency<-joint_summary|>group_by(metric,placement,optical)|>group_split(.keep=TRUE)|>map_dfr(pareto_one)
  pareto_ever<-joint_sufficiency|>group_by(metric,metric_class,support_id,placement,optical,resolution_s,temporal_label,n_days,joint_configuration,epsilon_entry,A_mean_absolute)|>summarise(ever_pareto=any(pareto),first_pareto_epsilon=if(any(pareto))min(epsilon[pareto])else NA_real_,last_pareto_epsilon=if(any(pareto))max(epsilon[pareto])else NA_real_,.groups="drop")
  pareto_frequency<-pareto_ever|>group_by(placement,optical,resolution_s,temporal_label,n_days)|>summarise(n_metrics_available=n_distinct(metric),n_metrics_ever_pareto=n_distinct(metric[ever_pareto]),fraction_metrics_ever_pareto=n_metrics_ever_pareto/n_metrics_available,.groups="drop")
  scores<-joint_summary|>group_by(metric,metric_class)|>summarise(n_joint_configs=n(),median_entry=median(epsilon_entry),entry_range=max(epsilon_entry)-min(epsilon_entry),.groups="drop")|>left_join(pareto_ever|>group_by(metric)|>summarise(n_ever_pareto=sum(ever_pareto),.groups="drop"),by="metric")|>mutate(selection_score=n_ever_pareto+log1p(entry_range));representatives<-scores|>group_by(metric_class)|>slice_max(selection_score,n=1,with_ties=FALSE)|>ungroup()|>slice_max(selection_score,n=min(4L,n()),with_ties=FALSE)|>arrange(desc(selection_score))
}
saveRDS(joint_canonical,file.path(OUT_DATA,"rq3_joint_distortion_long.rds"),compress="xz");saveRDS(joint_sufficiency,file.path(OUT_DATA,"rq3_joint_sufficiency_long.rds"),compress="xz");readr::write_csv(joint_summary,file.path(OUT_RESULTS,"rq3_joint_summary.csv"),na="");readr::write_csv(pareto_ever,file.path(OUT_RESULTS,"rq3_pareto_ever.csv"),na="");readr::write_csv(pareto_frequency,file.path(OUT_RESULTS,"rq3_pareto_frequency.csv"),na="");readr::write_csv(representatives,file.path(OUT_RESULTS,"rq3_fig5_representative_metrics.csv"),na="")
cohort_summary<-bind_rows(map(cohorts,~.x|>summarise(n_total=n(),n_eligible=sum(eligible_protocol_7))))|>mutate(support_id=names(cohorts),.before=1)
readr::write_csv(cohort_summary,file.path(OUT_RESULTS,"rq3_joint_support_cohorts.csv"),na="")
scope<-tibble(object=c("single_dimension","multidimensional_joint"),estimable=c(TRUE,nrow(joint_summary)>0),support=c("RQ1 comparison-specific maximal supports","facet-specific maximal supports; reference and candidate share the same support"),n_joint_eligible_participants=c(NA_integer_,if(nrow(cohort_summary))max(cohort_summary$n_eligible)else 0L),note=c("Single-dimension sufficiency is the exact tolerance projection of RQ1 A values.","Actual placement x optical x temporal x duration configurations; Pareto dominance only over temporal resolution and monitoring duration within fixed placement x optical facets."));readr::write_csv(scope,file.path(OUT_RESULTS,"rq3_scope.csv"),na="")
writeLines(c("# RQ3 run report","",paste0("Upstream: ",RQ1_VERSION),"Sufficiency: S_k(epsilon) uses expected absolute standardized distortion A.","epsilon_entry(c)=A(c): the minimum tolerance at which an observed configuration becomes sufficient.","Multidimensional results use facet-specific maximal supports; placement and optical states remain incomparable; Pareto dominance applies only to temporal resolution and duration."),file.path(OUT_RESULTS,"RQ3_RUN_REPORT.md"))
message("RQ3 complete: ",RQ1_VERSION)
