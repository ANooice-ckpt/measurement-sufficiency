suppressPackageStartupMessages({
  library(tidyverse)
  library(nlme)
})

# RQ2 downstream analysis: conditionality/predictability + cross-dimensional separability.
RQ1_DISTORTION <- "data/derived/rq1/rq1_distortion_long.rds"
CORE_METRICS <- "data/derived/core/metric_cube.csv.gz"
CORE_CONTEXT <- "data/derived/core/unit_context.csv.gz"
OUT_DATA <- "data/derived/rq2"; OUT_RESULTS <- "results/rq2"; OUT_DIAG <- "results/diagnostics"; INTERIM <- "data/interim/rq2"
RQ2_BOOT <- suppressWarnings(as.integer(Sys.getenv("RQ2_BOOT", unset="1000"))); if(!is.finite(RQ2_BOOT)||RQ2_BOOT<0) RQ2_BOOT<-1000L
RQ2_CV_FOLDS <- suppressWarnings(as.integer(Sys.getenv("RQ2_CV_FOLDS", unset="5"))); if(!is.finite(RQ2_CV_FOLDS)||RQ2_CV_FOLDS<2) RQ2_CV_FOLDS<-5L
RQ2_RUN_MODELS <- !identical(Sys.getenv("RQ2_RUN_MODELS", unset="1"),"0")
RQ2_FORCE <- identical(Sys.getenv("RQ2_FORCE", unset="0"),"1")
physical <- suppressWarnings(parallel::detectCores(logical=FALSE)); logical <- suppressWarnings(parallel::detectCores(logical=TRUE))
if(!is.finite(physical)||physical<1) physical<-ifelse(is.finite(logical),logical,2L)
auto_workers <- max(1L,min(12L,as.integer(physical)-2L))
RQ2_WORKERS <- suppressWarnings(as.integer(Sys.getenv("RQ2_WORKERS",unset=as.character(auto_workers)))); if(!is.finite(RQ2_WORKERS)||RQ2_WORKERS<1)RQ2_WORKERS<-auto_workers
if(is.finite(logical)) RQ2_WORKERS<-min(RQ2_WORKERS,logical)
BOOT_SEED<-20260820L; MODEL_SEED<-20260821L
PRIMARY_TEMPORAL_S<-c(20L,30L,60L,300L,900L,1800L); DUAL_CHANNEL_METRICS<-c("MDER","nvRD"); STATE_METRICS<-c("mean_MEDI","MDER","frequency_crossing_250"); NUMERIC_TOL<-1e-12
Sys.setenv(OMP_NUM_THREADS="1",OPENBLAS_NUM_THREADS="1",MKL_NUM_THREADS="1",VECLIB_MAXIMUM_THREADS="1",NUMEXPR_NUM_THREADS="1")
for(p in c(RQ1_DISTORTION,CORE_METRICS,CORE_CONTEXT)) if(!file.exists(p)) stop("Missing input: ",p)
for(d in c(OUT_DATA,OUT_RESULTS,OUT_DIAG,INTERIM)) dir.create(d,recursive=TRUE,showWarnings=FALSE)

safe_mean<-function(x){x<-x[is.finite(x)];if(length(x))mean(x)else NA_real_}
safe_sd<-function(x){x<-x[is.finite(x)];if(length(x)>=2)sd(x)else NA_real_}
safe_q<-function(x,p){x<-x[is.finite(x)];if(length(x))unname(quantile(x,p,names=FALSE,type=7))else NA_real_}
circular_mean<-function(x,period=86400){x<-x[is.finite(x)];if(!length(x))return(NA_real_);th<-2*pi*x/period;(atan2(mean(sin(th)),mean(cos(th)))%%(2*pi))*period/(2*pi)}
circular_delta<-function(a,b,period=86400)((a-b+period/2)%%period)-period/2
reference_scale<-function(x,geometry){x<-x[is.finite(x)];if(length(x)<2)return(NA_real_);if(identical(geometry,"circular_time")){c<-circular_mean(x);sd(circular_delta(x,c))}else sd(x)}
solar_noon_elevation<-function(latitude,day_of_year){decl<-23.44*sin(2*pi*(284+day_of_year)/365.25);pmax(0,90-abs(latitude-decl))}

message("RQ2: read upstream artifacts")
rq1<-readRDS(RQ1_DISTORTION)|>mutate(Date=as.Date(Date),window_start=as.Date(window_start),window_end=as.Date(window_end),reference_window_start=as.Date(reference_window_start),reference_window_end=as.Date(reference_window_end))
cube<-readr::read_csv(CORE_METRICS,show_col_types=FALSE,progress=FALSE)|>mutate(Date=as.Date(Date))
context<-readr::read_csv(CORE_CONTEXT,show_col_types=FALSE,progress=FALSE)|>mutate(Date=as.Date(Date))
if(!all(c("rq1_analysis_version","core_artifact_version")%in%names(rq1))) stop("RQ1 artifact predates v2; rerun RQ1")
up_versions<-unique(rq1$rq1_analysis_version);up_versions<-up_versions[!is.na(up_versions)];if(length(up_versions)!=1)stop("RQ1 version mismatch")
RQ1_VERSION<-up_versions[[1]]; CORE_VERSION<-unique(rq1$core_artifact_version);CORE_VERSION<-CORE_VERSION[!is.na(CORE_VERSION)];if(length(CORE_VERSION)!=1)stop("core version mismatch")
CORE_VERSION<-CORE_VERSION[[1]]
if(any(cube$resolution_s==15L))stop("Stale 15-s core detected")
version_token<-gsub("[^A-Za-z0-9_.-]","_",RQ1_VERSION)
MODEL_CKPT_DIR<-file.path(INTERIM,"models",paste0("v4_",version_token,"_f",RQ2_CV_FOLDS))
dir.create(MODEL_CKPT_DIR,recursive=TRUE,showWarnings=FALSE)
message("RQ2 runtime: upstream=",RQ1_VERSION,", workers=",RQ2_WORKERS,", folds=",RQ2_CV_FOLDS,", boot=",RQ2_BOOT,", force=",RQ2_FORCE)

# -----------------------------------------------------------------------------
# 1. Reference exposure state and personal-measurement-independent context
# -----------------------------------------------------------------------------
state_daily<-cube|>filter(analysis_unit_type=="participant_day",metric%in%STATE_METRICS,available,is.finite(value))|>
  select(support_id,site,Id,Date,config_id,metric,value)|>distinct()|>pivot_wider(names_from=metric,values_from=value)
external_needed<-c("era5_ssrd_daily_mean_w_m2","era5_direct_fraction","era5_total_cloud_cover_mean","latitude","day_of_year")
if(length(setdiff(external_needed,names(context))))stop("unit_context lacks RQ2 external variables")
context_daily<-context|>select(support_id,site,Id,Date,config_id,all_of(external_needed))|>distinct()
feature_daily<-full_join(state_daily,context_daily,by=c("support_id","site","Id","Date","config_id"))|>
  mutate(state_level=mean_MEDI,state_spectral=if_else(is.finite(MDER)&MDER>0,log(MDER),NA_real_),
         state_dynamic=if_else(is.finite(frequency_crossing_250)&frequency_crossing_250>=0,log1p(frequency_crossing_250),NA_real_),
         external_radiation=if_else(is.finite(era5_ssrd_daily_mean_w_m2)&era5_ssrd_daily_mean_w_m2>=0,log1p(era5_ssrd_daily_mean_w_m2),NA_real_),
         external_direct_fraction=era5_direct_fraction,external_cloud=era5_total_cloud_cover_mean,
         solar_noon_elevation_deg=solar_noon_elevation(latitude,day_of_year))|>
  select(support_id,site,Id,Date,config_id,state_level,state_spectral,state_dynamic,external_radiation,external_direct_fraction,external_cloud,solar_noon_elevation_deg)

daily_condition<-rq1|>filter(analysis_unit_type=="participant_day")|>left_join(feature_daily,by=c("support_id","site","Id","Date","reference_config_id"="config_id"))
feature_multiday<-feature_daily|>group_by(support_id,site,Id,config_id)|>summarise(across(c(state_level,state_spectral,state_dynamic,external_radiation,external_direct_fraction,external_cloud,solar_noon_elevation_deg),safe_mean),.groups="drop")
multiday_condition<-rq1|>filter(analysis_unit_type=="participant_multiday")|>left_join(feature_multiday,by=c("support_id","site","Id","reference_config_id"="config_id"))

# Duration features use the exact protocol-anchored reference/window carried by RQ1.
core_duration_ref_map<-cube|>filter(analysis_unit_type=="participant_day",placement=="eye",optical=="MEDI",resolution_s==10L)|>distinct(support_id,site,Id,config_id)
duration_units<-rq1|>filter(analysis_unit_type=="participant_window")|>distinct(support_id,site,Id,analysis_unit_id,reference_id,reference_window_start,reference_window_end,window_start,window_end,n_days)|>left_join(core_duration_ref_map,by=c("support_id","site","Id"))
if(anyDuplicated(duration_units[c("support_id","site","Id","analysis_unit_id")]))stop("Duration RQ2 mapping not unique")
# Vectorized window feature construction: join each protocol reference once, then
# summarize reference and selected-window states without thousands of repeated filters.
duration_features<-feature_daily|>
  inner_join(duration_units,by=c("support_id","site","Id","config_id"))|>
  filter(Date>=reference_window_start,Date<=reference_window_end)|>
  group_by(support_id,site,Id,analysis_unit_id)|>
  summarise(
    ref_level_mean=safe_mean(state_level),ref_level_sd=safe_sd(state_level),
    state_level=safe_mean(state_level[Date>=window_start & Date<=window_end]),
    state_spectral=safe_mean(state_spectral[Date>=window_start & Date<=window_end]),
    state_dynamic=safe_mean(state_dynamic[Date>=window_start & Date<=window_end]),
    external_radiation=safe_mean(external_radiation[Date>=window_start & Date<=window_end]),
    external_direct_fraction=safe_mean(external_direct_fraction[Date>=window_start & Date<=window_end]),
    external_cloud=safe_mean(external_cloud[Date>=window_start & Date<=window_end]),
    solar_noon_elevation_deg=safe_mean(solar_noon_elevation_deg[Date>=window_start & Date<=window_end]),
    .groups="drop"
  )|>
  mutate(state_window_departure=if_else(
    is.finite(ref_level_sd)&ref_level_sd>sqrt(.Machine$double.eps)&is.finite(state_level)&is.finite(ref_level_mean),
    abs(state_level-ref_level_mean)/ref_level_sd,NA_real_
  ))|>
  select(-ref_level_mean,-ref_level_sd)
duration_condition<-rq1|>filter(analysis_unit_type=="participant_window")|>left_join(duration_features,by=c("support_id","site","Id","analysis_unit_id"))

condition<-bind_rows(daily_condition,multiday_condition,duration_condition)|>
  mutate(state_window_departure=if_else(dimension=="duration",state_window_departure,NA_real_),
         primary_state_name=case_when(dimension=="placement"~"reference eye light level",dimension=="optical"~"melanopic-photopic ratio",dimension=="temporal"~"short-term crossing dynamics",dimension=="duration"~"window departure from seven-day mean",TRUE~NA_character_),
         primary_state_raw=case_when(dimension=="placement"~state_level,dimension=="optical"~state_spectral,dimension=="temporal"~state_dynamic,dimension=="duration"~state_window_departure,TRUE~NA_real_),
         abs_e=abs(e),participant_key=paste(site,Id,sep="|"))

# Freeze one Low/Middle/High state partition on the reference exposure units, then
# apply it to every target metric. State is a property of the exposure process,
# not a metric-specific re-ranking.
state_units<-condition|>filter(is.finite(primary_state_raw))|>distinct(dimension,configuration,support_id,site,Id,analysis_unit_id,primary_state_raw)
state_bins<-state_units|>group_by(dimension,configuration,support_id)|>mutate(state_bin=if(n()>=6&&n_distinct(primary_state_raw)>=3)ntile(primary_state_raw,3L)else NA_integer_)|>ungroup()|>
  mutate(state_bin_label=factor(case_when(state_bin==1L~"Low",state_bin==2L~"Middle",state_bin==3L~"High",TRUE~NA_character_),levels=c("Low","Middle","High")))
condition<-condition|>left_join(state_bins|>select(dimension,configuration,support_id,site,Id,analysis_unit_id,state_bin,state_bin_label),by=c("dimension","configuration","support_id","site","Id","analysis_unit_id"))
saveRDS(condition,file.path(OUT_DATA,"rq2_condition_long.rds"),compress="xz")
readr::write_csv(state_bins,file.path(OUT_DIAG,"rq2_reference_state_bins.csv"),na="")

conditional_geometry<-condition|>filter(available,is.finite(e),!is.na(state_bin_label))|>group_by(dimension,configuration,configuration_label,configuration_order,metric,metric_class,metric_geometry,primary_state_name,state_bin,state_bin_label)|>
  summarise(n_participants=n_distinct(participant_key),n_units=n(),state_median=median(primary_state_raw,na.rm=TRUE),state_q25=safe_q(primary_state_raw,.25),state_q75=safe_q(primary_state_raw,.75),B_conditional=mean(e),A_conditional=mean(abs(e)),median_e=median(e),p025_e=safe_q(e,.025),p975_e=safe_q(e,.975),.groups="drop")
readr::write_csv(conditional_geometry,file.path(OUT_RESULTS,"rq2_conditional_geometry.csv"),na="")

anchor_manifest<-rq1|>filter(available)|>distinct(dimension,configuration,configuration_label,configuration_order)|>group_by(dimension)|>filter(dimension%in%c("placement","optical")|configuration_order==max(configuration_order,na.rm=TRUE))|>ungroup()|>
  mutate(prediction_eligible=TRUE,anchor_reason=case_when(dimension=="placement"~"both primary placement alternatives",dimension=="optical"~"single primary optical alternative",dimension=="temporal"~"coarsest primary temporal alternative",dimension=="duration"~"shortest monitoring duration",TRUE~"anchor"))
readr::write_csv(anchor_manifest,file.path(OUT_RESULTS,"rq2_anchor_configurations.csv"),na="")
example_scores<-conditional_geometry|>semi_join(anchor_manifest,by=c("dimension","configuration"))|>filter(state_bin%in%c(1L,3L))|>select(dimension,configuration,configuration_label,metric,metric_class,state_bin,A_conditional,B_conditional)|>pivot_wider(names_from=state_bin,values_from=c(A_conditional,B_conditional),names_prefix="q")|>filter(is.finite(A_conditional_q1), is.finite(A_conditional_q3), is.finite(B_conditional_q1), is.finite(B_conditional_q3))|>mutate(state_shift_score=abs(A_conditional_q3-A_conditional_q1)+abs(B_conditional_q3-B_conditional_q1))|>group_by(dimension)|>slice_max(state_shift_score,n=1,with_ties=FALSE)|>ungroup()
readr::write_csv(example_scores,file.path(OUT_RESULTS,"rq2_conditional_examples.csv"),na="")

# -----------------------------------------------------------------------------
# 2. Parallel/resumable mixed models
# -----------------------------------------------------------------------------
message("RQ2: mixed models and out-of-sample predictability")
EXTERNAL_PREDICTORS<-c("external_radiation","external_direct_fraction","external_cloud","solar_noon_elevation_deg")
MODEL_FAMILIES<-list(external_context=EXTERNAL_PREDICTORS,exposure_state="primary_state_raw",joint=c("primary_state_raw",EXTERNAL_PREDICTORS))
MODEL_OUTCOMES<-c(signed="e",magnitude="abs_e")
checkpoint_valid<-function(path,version){if(!file.exists(path))return(FALSE);x<-tryCatch(readRDS(path),error=function(e)NULL);!is.null(x)&&identical(x$checkpoint_version,version)&&isTRUE(x$complete)}
run_model_task<-function(task){
  t0<-proc.time()[[3]]
  tryCatch({
    dat0<-task$data;meta<-task$meta
    scale_tt<-function(train,test,preds){keep<-character();for(p in preds){mu<-mean(train[[p]],na.rm=TRUE);s<-sd(train[[p]],na.rm=TRUE);if(!is.finite(mu)||!is.finite(s)||s<=sqrt(.Machine$double.eps))next;train[[p]]<-(train[[p]]-mu)/s;test[[p]]<-(test[[p]]-mu)/s;keep<-c(keep,p)};list(train=train,test=test,predictors=keep)}
    fit_mixed<-function(dat,outcome,preds){if(!length(preds))return(list(fit=NULL,random_structure=NA_character_));f<-reformulate(preds,response=outcome);dat$site<-factor(dat$site);dat$participant_key<-factor(dat$participant_key);ctrl<-nlme::lmeControl(opt="optim",maxIter=100L,msMaxIter=100L,returnObject=TRUE);fit<-tryCatch(suppressWarnings(nlme::lme(fixed=f,random=~1|site/participant_key,data=dat,method="ML",na.action=na.omit,control=ctrl)),error=function(e)NULL);if(!is.null(fit))return(list(fit=fit,random_structure="site/participant"));fit<-tryCatch(suppressWarnings(nlme::lme(fixed=f,random=~1|participant_key,data=dat,method="ML",na.action=na.omit,control=ctrl)),error=function(e)NULL);list(fit=fit,random_structure=if(is.null(fit))NA_character_ else "participant")}
    predict_fixed<-function(fit,newdata)if(is.null(fit))rep(NA_real_,nrow(newdata))else tryCatch(as.numeric(predict(fit,newdata=newdata,level=0)),error=function(e)rep(NA_real_,nrow(newdata)))
    perf<-function(obs,pred){ok<-is.finite(obs)&is.finite(pred);obs<-obs[ok];pred<-pred[ok];if(length(obs)<2)return(data.frame(n_test=length(obs),rmse=NA,mae=NA,r2=NA));sst<-sum((obs-mean(obs))^2);data.frame(n_test=length(obs),rmse=sqrt(mean((obs-pred)^2)),mae=mean(abs(obs-pred)),r2=if(sst>0)1-sum((obs-pred)^2)/sst else NA_real_)}
    set.seed(task$seed);pm<-unique(dat0[c("site","participant_key")]);pm$fold<-NA_integer_;for(s in sort(unique(pm$site))){ii<-which(pm$site==s);ord<-sample(ii);pm$fold[ord]<-((seq_along(ord)-1L)%%task$cv_folds)+1L}
    cv<-function(outcome,preds,scheme){d<-dat0;if(scheme=="participant_grouped"){d<-merge(d,pm,by=c("site","participant_key"),all.x=TRUE,sort=FALSE);splits<-sort(unique(d$fold));sv<-d$fold}else{splits<-sort(unique(as.character(d$site)));sv<-as.character(d$site)};oo<-numeric();pp<-numeric();for(s in splits){testflag<-sv==s;tr<-d[!testflag,,drop=FALSE];te<-d[testflag,,drop=FALSE];if(nrow(tr)<20||nrow(te)<2||length(unique(tr$participant_key))<5||length(unique(tr$site))<2)next;sc<-scale_tt(tr,te,preds);if(!length(sc$predictors))next;fit<-fit_mixed(sc$train,outcome,sc$predictors);pr<-predict_fixed(fit$fit,sc$test);oo<-c(oo,sc$test[[outcome]]);pp<-c(pp,pr)};perf(oo,pp)}
    coefs<-list();perfs<-list();ci<-0L;pi<-0L
    for(oname in names(task$outcomes)){outcome<-task$outcomes[[oname]];for(fam in names(task$families)){preds<-task$families[[fam]];sc<-scale_tt(dat0,dat0,preds);fit<-fit_mixed(sc$train,outcome,sc$predictors);if(!is.null(fit$fit)){tt<-summary(fit$fit)$tTable;ci<-ci+1L;coefs[[ci]]<-data.frame(dimension=meta$dimension,configuration=meta$configuration,configuration_label=meta$configuration_label,metric=meta$metric,metric_class=meta$metric_class,outcome=oname,model_family=fam,random_structure=fit$random_structure,term=rownames(tt),estimate=tt[,"Value"],std_error=tt[,"Std.Error"],df=tt[,"DF"],t_value=tt[,"t-value"],p_value=tt[,"p-value"],row.names=NULL)};for(scheme in c("participant_grouped","leave_site_out")){p<-cv(outcome,preds,scheme);pi<-pi+1L;perfs[[pi]]<-data.frame(dimension=meta$dimension,configuration=meta$configuration,configuration_label=meta$configuration_label,metric=meta$metric,metric_class=meta$metric_class,outcome=oname,model_family=fam,validation_scheme=scheme,n_participants=length(unique(dat0$participant_key)),n_sites=length(unique(dat0$site)),n_test=p$n_test,rmse=p$rmse,mae=p$mae,r2=p$r2)}}}
    obj<-list(checkpoint_version=task$checkpoint_version,complete=TRUE,coefficients=if(length(coefs))do.call(rbind,coefs)else data.frame(),performance=if(length(perfs))do.call(rbind,perfs)else data.frame(),audit=data.frame(group_index=task$group_index,dimension=meta$dimension,configuration=meta$configuration,metric=meta$metric,n_rows=nrow(dat0),n_participants=length(unique(dat0$participant_key)),n_sites=length(unique(dat0$site)),elapsed_seconds=proc.time()[[3]]-t0,status="completed"))
    tmp<-paste0(task$checkpoint_path,".tmp_",Sys.getpid());saveRDS(obj,tmp,compress=FALSE);if(file.exists(task$checkpoint_path))unlink(task$checkpoint_path);if(!file.rename(tmp,task$checkpoint_path)){if(!file.copy(tmp,task$checkpoint_path,overwrite=TRUE))stop("checkpoint write failed");unlink(tmp)};list(ok=TRUE,path=task$checkpoint_path,error=NA_character_)
  },error=function(e)list(ok=FALSE,path=task$checkpoint_path,error=conditionMessage(e)))
}
run_parallel_batches<-function(tasks,runner,workers,label){if(!length(tasks))return(list());workers<-min(max(1L,workers),length(tasks));chunks<-split(seq_along(tasks),ceiling(seq_along(tasks)/(workers*2L)));out<-vector("list",length(tasks));cl<-if(workers>1)parallel::makePSOCKcluster(workers)else NULL;if(!is.null(cl))parallel::clusterCall(cl,function(){Sys.setenv(OMP_NUM_THREADS="1",OPENBLAS_NUM_THREADS="1",MKL_NUM_THREADS="1");NULL});on.exit(if(!is.null(cl))parallel::stopCluster(cl),add=TRUE);done<-0L;for(ch in chunks){ans<-if(is.null(cl))lapply(tasks[ch],runner)else parallel::parLapplyLB(cl,tasks[ch],runner);out[ch]<-ans;done<-done+length(ch);message(sprintf("[%s] %d/%d pending tasks completed",label,done,length(tasks)))};out}

model_anchor<-condition|>filter(available,is.finite(e))|>semi_join(anchor_manifest|>filter(prediction_eligible),by=c("dimension","configuration"))
model_groups<-model_anchor|>distinct(dimension,configuration,configuration_label,metric,metric_class)|>arrange(dimension,configuration,metric)|>mutate(group_index=row_number())
MODEL_CKPT_VERSION<-paste0("rq2_models__",version_token,"__f",RQ2_CV_FOLDS)
tasks<-list();audit_ineligible<-list();manifest_rows<-list()
if(RQ2_RUN_MODELS){for(gi in seq_len(nrow(model_groups))){g<-model_groups[gi,];dat0<-model_anchor|>filter(dimension==g$dimension,configuration==g$configuration,metric==g$metric)|>select(site,Id,participant_key,e,abs_e,primary_state_raw,all_of(EXTERNAL_PREDICTORS))|>filter(if_all(all_of(c("e","abs_e","primary_state_raw",EXTERNAL_PREDICTORS)),is.finite));eligible<-nrow(dat0)>=40&&n_distinct(dat0$participant_key)>=10&&n_distinct(dat0$site)>=3;ckpt<-file.path(MODEL_CKPT_DIR,sprintf("group_%04d.rds",g$group_index));if(!eligible){audit_ineligible[[length(audit_ineligible)+1]]<-tibble(group_index=g$group_index,dimension=g$dimension,configuration=g$configuration,metric=g$metric,n_rows=nrow(dat0),n_participants=n_distinct(dat0$participant_key),n_sites=n_distinct(dat0$site),elapsed_seconds=0,status="ineligible",checkpoint_reused=FALSE);next};reused<-!RQ2_FORCE&&checkpoint_valid(ckpt,MODEL_CKPT_VERSION);manifest_rows[[length(manifest_rows)+1]]<-tibble(group_index=g$group_index,path=ckpt,checkpoint_reused=reused);if(!reused)tasks[[length(tasks)+1]]<-list(group_index=g$group_index,meta=as.list(g[1,c("dimension","configuration","configuration_label","metric","metric_class")]),data=as.data.frame(dat0),cv_folds=RQ2_CV_FOLDS,seed=MODEL_SEED+g$group_index*1009L,families=MODEL_FAMILIES,outcomes=MODEL_OUTCOMES,checkpoint_version=MODEL_CKPT_VERSION,checkpoint_path=ckpt)}}
mm<-bind_rows(manifest_rows);message("RQ2 models: groups=",nrow(model_groups),", eligible=",nrow(mm),", reused=",if(nrow(mm))sum(mm$checkpoint_reused)else 0,", pending=",length(tasks))
res<-if(RQ2_RUN_MODELS)run_parallel_batches(tasks,run_model_task,RQ2_WORKERS,"models")else list();errs<-keep(res,~!isTRUE(.x$ok));if(length(errs)){readr::write_csv(map_dfr(errs,~tibble(path=.x$path,error=.x$error)),file.path(OUT_DIAG,"rq2_model_worker_errors.csv"));stop("RQ2 model worker failure")}
objs<-if(nrow(mm))map(mm$path,~{if(!checkpoint_valid(.x,MODEL_CKPT_VERSION))stop("Invalid checkpoint: ",.x);readRDS(.x)})else list()
model_coefficients<-bind_rows(map(objs,"coefficients"));model_performance<-bind_rows(map(objs,"performance"))
if(!nrow(model_coefficients))model_coefficients<-tibble(dimension=character(),configuration=character(),configuration_label=character(),metric=character(),metric_class=character(),outcome=character(),model_family=character(),random_structure=character(),term=character(),estimate=double(),std_error=double(),df=double(),t_value=double(),p_value=double())
if(!nrow(model_performance))model_performance<-tibble(dimension=character(),configuration=character(),configuration_label=character(),metric=character(),metric_class=character(),outcome=character(),model_family=character(),validation_scheme=character(),n_participants=integer(),n_sites=integer(),n_test=integer(),rmse=double(),mae=double(),r2=double())
completed_audit<-map_dfr(seq_along(objs),~as_tibble(objs[[.x]]$audit)|>mutate(checkpoint_reused=mm$checkpoint_reused[.x]));model_audit<-bind_rows(completed_audit,bind_rows(audit_ineligible))|>arrange(group_index)
readr::write_csv(model_audit,file.path(OUT_DIAG,"rq2_model_task_audit.csv"),na="");readr::write_csv(model_coefficients,file.path(OUT_RESULTS,"rq2_model_coefficients.csv"),na="");readr::write_csv(model_performance,file.path(OUT_RESULTS,"rq2_model_performance.csv"),na="")

# -----------------------------------------------------------------------------
# 3. Cross-dimensional second-order distortion gamma
# -----------------------------------------------------------------------------
message("RQ2: cross-dimensional second-order distortion")
cell_keys<-c("support_id","site","Id","analysis_unit_type","analysis_unit_id","metric")
cell<-function(z,prefix)z|>select(all_of(cell_keys),Date,metric_class,metric_geometry,value,available,unavailable_reason)|>rename_with(~paste0(prefix,.x),c("value","available","unavailable_reason"))
make_gamma_cells<-function(z00,za0,z0b,zab,dimension_pair,a_dimension,a_configuration,a_label,b_dimension,b_configuration,b_label,interaction_lattice){c00<-cell(z00,"m00_");ca0<-cell(za0,"ma0_")|>select(all_of(cell_keys),ma0_value,ma0_available,ma0_unavailable_reason);c0b<-cell(z0b,"m0b_")|>select(all_of(cell_keys),m0b_value,m0b_available,m0b_unavailable_reason);cab<-cell(zab,"mab_")|>select(all_of(cell_keys),mab_value,mab_available,mab_unavailable_reason);c00|>inner_join(ca0,by=cell_keys)|>inner_join(c0b,by=cell_keys)|>inner_join(cab,by=cell_keys)|>transmute(dimension_pair=dimension_pair,a_dimension=a_dimension,a_configuration=a_configuration,a_configuration_label=a_label,b_dimension=b_dimension,b_configuration=b_configuration,b_configuration_label=b_label,interaction_lattice=interaction_lattice,support_id,site,Id,analysis_unit_type,analysis_unit_id,Date,metric,metric_class,metric_geometry,m00=m00_value,ma0=ma0_value,m0b=m0b_value,mab=mab_value,cells_available=m00_available&ma0_available&m0b_available&mab_available&is.finite(m00)&is.finite(ma0)&is.finite(m0b)&is.finite(mab))}
gamma_blocks<-list();gb<-0L
for(pos in c("chest","wrist")){support<-paste0("eye_",pos,"_full");z<-cube|>filter(support_id==support,placement%in%c("eye",pos),optical%in%c("MEDI","LIGHT"),resolution_s==10L);gb<-gb+1L;gamma_blocks[[gb]]<-make_gamma_cells(z|>filter(placement=="eye",optical=="MEDI"),z|>filter(placement==pos,optical=="MEDI"),z|>filter(placement=="eye",optical=="LIGHT"),z|>filter(placement==pos,optical=="LIGHT"),"placement × optical","placement",pos,str_to_title(pos),"optical","LIGHT","Photopic illuminance",paste0("placement_optical_",pos))}
all_metrics<-unique(cube$metric)
for(pos in c("chest","wrist"))for(r in PRIMARY_TEMPORAL_S)for(st in c("medi","full")){support<-paste0("eye_",pos,"_",st);mf<-if(st=="full")DUAL_CHANNEL_METRICS else setdiff(all_metrics,DUAL_CHANNEL_METRICS);z<-cube|>filter(support_id==support,placement%in%c("eye",pos),optical=="MEDI",resolution_s%in%c(10L,r),metric%in%mf);if(!nrow(z))next;gb<-gb+1L;gamma_blocks[[gb]]<-make_gamma_cells(z|>filter(placement=="eye",resolution_s==10L),z|>filter(placement==pos,resolution_s==10L),z|>filter(placement=="eye",resolution_s==r),z|>filter(placement==pos,resolution_s==r),"placement × temporal","placement",pos,str_to_title(pos),"temporal",paste0(r,"s"),if_else(r<60,paste0(r," s"),paste0(r%/%60," min")),paste0("placement_temporal_",pos,"_",st))}
for(r in PRIMARY_TEMPORAL_S){z<-cube|>filter(support_id=="eye_full",placement=="eye",optical%in%c("MEDI","LIGHT"),resolution_s%in%c(10L,r));gb<-gb+1L;gamma_blocks[[gb]]<-make_gamma_cells(z|>filter(optical=="MEDI",resolution_s==10L),z|>filter(optical=="LIGHT",resolution_s==10L),z|>filter(optical=="MEDI",resolution_s==r),z|>filter(optical=="LIGHT",resolution_s==r),"optical × temporal","optical","LIGHT","Photopic illuminance","temporal",paste0(r,"s"),if_else(r<60,paste0(r," s"),paste0(r%/%60," min")),"optical_temporal")}
gamma_cells<-bind_rows(gamma_blocks);if(!nrow(gamma_cells))stop("No observable gamma cells")
gstd<-gamma_cells|>filter(is.finite(m00))|>distinct(interaction_lattice,metric,metric_geometry,site,Id,analysis_unit_id,m00)|>group_by(interaction_lattice,metric,metric_geometry)|>summarise(n_reference_units=n(),standardizer=reference_scale(m00,first(metric_geometry)),.groups="drop")|>mutate(zero=!is.finite(standardizer)|standardizer<=sqrt(.Machine$double.eps))
readr::write_csv(gstd,file.path(OUT_DIAG,"rq2_gamma_standardizer_audit.csv"),na="")
gamma_long<-gamma_cells|>left_join(gstd,by=c("interaction_lattice","metric","metric_geometry"))|>mutate(marginal_a_ref_delta=if_else(metric_geometry=="circular_time",circular_delta(ma0,m00),ma0-m00),marginal_a_at_b_delta=if_else(metric_geometry=="circular_time",circular_delta(mab,m0b),mab-m0b),gamma_delta=marginal_a_at_b_delta-marginal_a_ref_delta,available=cells_available&!replace_na(zero,TRUE)&is.finite(gamma_delta)&is.finite(standardizer),gamma=if_else(available,gamma_delta/standardizer,NA_real_),marginal_a_ref=if_else(available,marginal_a_ref_delta/standardizer,NA_real_),marginal_a_at_b=if_else(available,marginal_a_at_b_delta/standardizer,NA_real_),participant_key=paste(site,Id,sep="|"),rq1_analysis_version=RQ1_VERSION,core_artifact_version=CORE_VERSION)
saveRDS(gamma_long,file.path(OUT_DATA,"rq2_gamma_long.rds"),compress="xz")
ga<-gamma_long|>filter(available,is.finite(gamma))
group_gamma<-c("dimension_pair","a_dimension","a_configuration","a_configuration_label","b_dimension","b_configuration","b_configuration_label","interaction_lattice","metric","metric_class","metric_geometry")
base<-ga|>group_by(across(all_of(group_gamma)))|>summarise(n_participants=n_distinct(participant_key),n_units=n(),median_gamma=median(gamma),q25_gamma=safe_q(gamma,.25),q75_gamma=safe_q(gamma,.75),p025_gamma=safe_q(gamma,.025),p975_gamma=safe_q(gamma,.975),R_mean_signed=mean(gamma),Q_mean_absolute=mean(abs(gamma)),marginal_a_ref_mean=mean(marginal_a_ref),marginal_a_at_b_mean=mean(marginal_a_at_b),.groups="drop")
boot_one<-function(g){
  site_counts<-g|>distinct(site,Id)|>count(site,name="n")
  supported<-RQ2_BOOT>0&&n_distinct(g$participant_key)>=2&&any(site_counts$n>1)
  if(!supported)return(tibble(bootstrap_supported=FALSE,R_ci_low=NA_real_,R_ci_high=NA_real_,Q_ci_low=NA_real_,Q_ci_high=NA_real_))
  cl<-g|>group_by(site,Id)|>summarise(sum_g=sum(gamma),sum_abs=sum(abs(gamma)),n=n(),.groups="drop")
  bys<-split(cl,cl$site);total_g<-numeric(RQ2_BOOT);total_a<-numeric(RQ2_BOOT);total_n<-numeric(RQ2_BOOT)
  for(z in bys){nz<-nrow(z);idx<-matrix(sample.int(nz,RQ2_BOOT*nz,replace=TRUE),nrow=RQ2_BOOT,ncol=nz);total_g<-total_g+rowSums(matrix(z$sum_g[idx],nrow=RQ2_BOOT));total_a<-total_a+rowSums(matrix(z$sum_abs[idx],nrow=RQ2_BOOT));total_n<-total_n+rowSums(matrix(z$n[idx],nrow=RQ2_BOOT))}
  rv<-total_g/total_n;qv<-total_a/total_n
  tibble(bootstrap_supported=TRUE,R_ci_low=safe_q(rv,.025),R_ci_high=safe_q(rv,.975),Q_ci_low=safe_q(qv,.025),Q_ci_high=safe_q(qv,.975))
}
set.seed(BOOT_SEED);cis<-ga|>group_by(across(all_of(group_gamma)))|>group_modify(~boot_one(.x))|>ungroup();gamma_summary<-base|>left_join(cis,by=group_gamma)|>mutate(geometry_gap=Q_mean_absolute-abs(R_mean_signed),geometry_pass=Q_mean_absolute+NUMERIC_TOL>=abs(R_mean_signed));if(any(!gamma_summary$geometry_pass))stop("Q >= |R| failed")
readr::write_csv(gamma_summary,file.path(OUT_RESULTS,"rq2_gamma_summary.csv"),na="")
pair_summary<-gamma_summary|>group_by(dimension_pair)|>summarise(n_metric_configuration_pairs=n(),median_Q=median(Q_mean_absolute,na.rm=TRUE),q25_Q=safe_q(Q_mean_absolute,.25),q75_Q=safe_q(Q_mean_absolute,.75),median_abs_R=median(abs(R_mean_signed),na.rm=TRUE),.groups="drop")|>arrange(desc(median_Q));readr::write_csv(pair_summary,file.path(OUT_RESULTS,"rq2_gamma_pair_summary.csv"),na="")
elig<-gamma_summary|>filter(n_participants>=3,is.finite(Q_mean_absolute),Q_mean_absolute>0)|>mutate(direction_ratio=abs(R_mean_signed)/Q_mean_absolute,id=paste(dimension_pair,a_configuration,b_configuration,metric,sep=" | "))
low<-elig|>arrange(Q_mean_absolute,abs(R_mean_signed))|>slice(1)|>mutate(example_type="high separability");pos<-elig|>filter(R_mean_signed>0)|>mutate(score=percent_rank(Q_mean_absolute)+percent_rank(direction_ratio))|>arrange(desc(score))|>slice(1)|>mutate(example_type="positive dependence");neg<-elig|>filter(R_mean_signed<0)|>mutate(score=percent_rank(Q_mean_absolute)+percent_rank(direction_ratio))|>arrange(desc(score))|>slice(1)|>mutate(example_type="negative dependence");bi<-elig|>mutate(score=percent_rank(Q_mean_absolute)+percent_rank(1-direction_ratio))|>arrange(desc(score))|>slice(1)|>mutate(example_type="bidirectional dependence");gamma_examples<-bind_rows(low,pos,neg,bi)|>distinct(id,.keep_all=TRUE);readr::write_csv(gamma_examples,file.path(OUT_RESULTS,"rq2_gamma_examples.csv"),na="");readr::write_csv(gamma_summary|>arrange(desc(Q_mean_absolute))|>slice(1),file.path(OUT_RESULTS,"rq2_strong_coupling_example.csv"),na="")

interaction_scope<-tibble(dimension_pair=c("placement × optical","placement × temporal","optical × temporal","placement × duration","optical × duration","temporal × duration"),status=c("estimated","estimated","estimated","not population-estimated","not population-estimated","not population-estimated"),scope_reason=c(rep("observable joint core cells on the required common support",3),rep("duration-containing second-order contrasts are outside the primary cross-dimensional estimand; duration is evaluated with protocol-anchored windows and facet-specific sufficiency downstream",3)))
readr::write_csv(interaction_scope,file.path(OUT_RESULTS,"rq2_interaction_scope.csv"),na="")
condition_audit<-condition|>group_by(dimension,configuration)|>summarise(n_rows=n(),n_available_e=sum(available&is.finite(e)),n_primary_state=sum(is.finite(primary_state_raw)),n_external_complete=sum(is.finite(external_radiation) & is.finite(external_direct_fraction) & is.finite(external_cloud) & is.finite(solar_noon_elevation_deg)),.groups="drop")
readr::write_csv(condition_audit,file.path(OUT_DIAG,"rq2_condition_feature_audit.csv"),na="")
writeLines(c("# RQ2 run report","",paste0("Upstream: ",RQ1_VERSION),paste0("Workers: ",RQ2_WORKERS),"Exposure-state tertiles are frozen on unique reference exposure units within dimension x configuration x support and shared by all metrics.","External context is personal-measurement-independent context, not assumed to be known before the study.","Primary gamma pairs: placement x optical, placement x temporal, optical x temporal."),file.path(OUT_RESULTS,"RQ2_RUN_REPORT.md"))
message("RQ2 complete: ",RQ1_VERSION)
