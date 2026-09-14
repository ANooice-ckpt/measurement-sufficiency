# Context-conditioned reliability from immutable daily anchor inputs.
# No raw exposure processing, recovery fitting, or RQ3 recomputation.
source("scripts/utils/rq2_risk_models.R")

# Fresh builds can export the same configuration-local dictionary without ever
# fitting historical recovery models. Existing validated input exports are reused.
reliability_prepare_inputs <- function() {
  inputs<-recovery_inputs();if(length(inputs$problems))stop(paste(inputs$problems,collapse="\n"))
  p<-list(core_artifact_version=inputs$core,rq1_analysis_version=inputs$version,
    predictors=recovery_predictors(),signature_predictors=recovery_signature_predictors(),
    temporal_predictors=recovery_temporal_predictors(),input_md5=tools::md5sum(inputs$paths),
    builder_version="conditional_reliability_daily_inputs_v1",
    builder_code_md5=tools::md5sum(c("scripts/12d_rq2_recovery.R","scripts/utils/rq2_context_features.R",
      "scripts/utils/rq2_conditional_reliability.R")))
  root<-file.path("results/rq2/reliability_inputs",inputs$version,recovery_hash(p))
  dest<-file.path(root,"input_manifest.rds")
  if(file.exists(dest))return(root)
  x<-recovery_pairs(inputs)
  signature<-recovery_signature(recovery_read_csv(inputs$paths[["unit_context"]]),inputs$core,requested=x)
  x<-recovery_join_information(x,signature,inputs$temporal)
  pm<-data.table::as.data.table(unique(x[c("site","participant_key")]))
  data.table::setorder(pm,site,participant_key);set.seed(20260912)
  pm[,fold:=sample(rep(1:5,length.out=.N)),by=site]
  x<-data.table::as.data.table(x);x[,fold:=pm$fold[match(participant_key,pm$participant_key)]]
  keys<-c("dimension","comparison_pair_id","candidate_config","support_id","metric","metric_class","metric_geometry")
  x[,task_index:=.GRP,by=keys]
  catalog<-x[,.(n_rows=.N,n_eligible=sum(eligible),n_eligible_participants=data.table::uniqueN(participant_key[eligible]),
    A_raw_all_eligible=if(any(eligible))mean(abs(z[eligible])) else NA_real_),by=c("task_index",keys)]
  ss<-data.table::as.data.table(inputs$summary)[,.(dimension,comparison_pair_id,metric,A_frozen_RQ1=A_mean_absolute)]
  catalog<-merge(catalog,ss,by=c("dimension","comparison_pair_id","metric"));setorder(catalog,task_index)
  catalog[,status:=ifelse(n_eligible>=20 & n_eligible_participants>=4,"complete","unavailable_support")]
  dir.create(file.path(root,"inputs"),recursive=TRUE,showWarnings=FALSE)
  for(i in catalog$task_index)recovery_atomic(as.data.frame(x[task_index==i][,task_index:=NULL]),file.path(root,"inputs",sprintf("task_%04d.rds",i)))
  data.table::fwrite(pm,file.path(root,"participant_folds.csv"))
  recovery_atomic(list(complete=TRUE,provenance=p,statuses=catalog),dest)
  root
}

reliability_source <- function(path=Sys.getenv("RQ2_RELIABILITY_INPUT_RUN","")) {
  if(!nzchar(path)) {
    current_version<-rq1_pairwise_version(readRDS("results/rq1/rq1_pairwise_change_long.rds"))
    candidates<-c(Sys.glob("results/rq2/reliability_inputs/*/*/input_manifest.rds"),Sys.glob("results/rq2/recovery/*/*/recovery_manifest.rds"))
    candidates<-candidates[vapply(candidates,function(p){
      m<-readRDS(p);identical(m$provenance$rq1_analysis_version,current_version) &&
        (identical(m$provenance$builder_version,"conditional_reliability_daily_inputs_v1") ||
         identical(m$provenance$estimator_version,"anchored_shared_split_candidate_library_v2")) && isTRUE(m$complete)
    },logical(1))]
    if(!length(candidates))path<-reliability_prepare_inputs()
    else if(length(candidates)==1L)path<-dirname(candidates)
    else stop("Set RQ2_RELIABILITY_INPUT_RUN to one immutable daily-input export")
  }
  manifest<-if(file.exists(file.path(path,"input_manifest.rds")))"input_manifest.rds" else "recovery_manifest.rds"
  m<-readRDS(file.path(path,manifest));p<-m$provenance
  current<-readRDS("results/rq1/rq1_pairwise_change_long.rds")
  stopifnot(identical(p$rq1_analysis_version,rq1_pairwise_version(current)),
    identical(p$core_artifact_version,current$core_artifact_version),
    length(p$signature_predictors)==16L,length(c(p$predictors,p$temporal_predictors))==50L)
  catalog<-data.table::as.data.table(m$statuses)
  files<-file.path(path,"inputs",sprintf("task_%04d.rds",catalog$task_index))
  if(any(!file.exists(files)))stop("Missing frozen daily task inputs")
  list(path=path,manifest=m,catalog=catalog,files=files)
}

reliability_validate_input <- function(x,meta) {
  stopifnot(!anyDuplicated(x[,c("site","Id","Date"),with=FALSE]),
    all(x$metric==meta$metric),all(x$comparison_pair_id==meta$comparison_pair_id),
    all(x$participant_key==paste(x$site,x$Id,sep="::")))
  e<-x$eligible;circ<-x$metric_geometry=="circular_time"
  delta<-x$reference_value-x$candidate_value
  delta[circ]<-((delta[circ]+43200)%%86400)-43200
  if(any(!is.finite(x$z[e]))||any(!is.finite(x$standardizer[e]))||any(x$standardizer[e]<=0)||
    any(abs(delta[e]/x$standardizer[e]-x$z[e])>1e-7*(1+abs(x$z[e]))))stop("Frozen geometry/scale mismatch")
  if(any(e) && abs(mean(abs(x$z[e]))-meta$A_frozen_RQ1)>1e-7*(1+meta$A_frozen_RQ1))stop("Frozen A mismatch")
  invisible(TRUE)
}

reliability_task <- function(task) {
  data.table::setDTthreads(1L)
  if(file.exists(task$output)){
    old<-readRDS(task$output)
    if(identical(old$run_id,task$run_id)&&isTRUE(old$complete))return(task$output)
  }
  started<-proc.time()[3];x<-data.table::as.data.table(readRDS(task$input))
  reliability_validate_input(x,task$meta)
  x<-x[eligible==TRUE]
  if(nrow(x)<20L||data.table::uniqueN(x$participant_key)<4L){
    saveRDS(list(complete=TRUE,status="unavailable_support",run_id=task$run_id,meta=task$meta),task$output)
    return(task$output)
  }
  circular<-unique(x$metric_geometry)=="circular_time"
  x[,low_1:=if(circular)sin(candidate_value*2*pi/86400) else candidate_value]
  x[,low_2:=if(circular)cos(candidate_value*2*pi/86400) else 0]
  all<-list();j<-0L;audits<-list()
  for(rep_id in sort(unique(task$folds$repeat_id))){
    map<-task$folds[repeat_id==rep_id]
    x[,fold:=map$fold[match(participant_key,map$participant_key)]]
    stopifnot(!anyNA(x$fold))
    for(f in sort(unique(x$fold))){
      tr<-x[fold!=f];te<-x[fold==f]
      if(nrow(tr)<15||data.table::uniqueN(tr$participant_key)<3)stop("Insufficient training support")
      stopifnot(!any(tr$participant_key%in%te$participant_key))
      y<-cbind(mean_risk=abs(tr$z),sapply(task$epsilon,function(e)as.numeric(abs(tr$z)>e)))
      colnames(y)[-1]<-paste0("exceed_",task$epsilon)
      low<-rq2_risk_design(tr,te,c("low_1","low_2",task$signature))
      ctx<-rq2_risk_design(tr,te,task$context)
      context_fit<-rq2_risk_predict(ctx,y,10,TRUE)
      preds<-list(null=matrix(colMeans(y),nrow(te),ncol(y),byrow=TRUE),context=context_fit$test,
        measurement=rq2_risk_predict(low,y,10),joint=rq2_risk_increment(low,ctx,y,10),
        measurement_capacity=rq2_risk_predict(low,y,20))
      # Site-only baseline is an explicit nuisance-control sensitivity, never a
      # predictor in the context or measurement dictionaries.
      site_prior<-preds$null
      for(s in unique(te$site))if(any(tr$site==s))site_prior[te$site==s,]<-
        matrix(colMeans(y[tr$site==s,,drop=FALSE]),sum(te$site==s),ncol(y),byrow=TRUE)
      preds$site_prior<-site_prior
      cuts<-as.numeric(quantile(pmax(0,context_fit$train[,1]),c(1/3,2/3)))
      group<-ifelse(context_fit$test[,1]<=cuts[1],"Lower",ifelse(context_fit$test[,1]<=cuts[2],"Middle","Higher"))
      audits[[length(audits)+1L]]<-data.table::data.table(repeat_id=rep_id,fold=f,n_train=nrow(tr),n_test=nrow(te),
        n_train_participants=data.table::uniqueN(tr$participant_key),n_test_participants=data.table::uniqueN(te$participant_key),
        context_lower_cut=cuts[1],context_upper_cut=cuts[2],n_low_basis=ncol(low$train),n_context_basis=ncol(ctx$train))
      for(state_name in names(preds)){
        pr<-preds[[state_name]];pr[,1]<-pmax(0,pr[,1]);pr[,-1]<-rq2_risk_monotone(pr[,-1,drop=FALSE])
        for(k in seq_len(ncol(y))){
          j<-j+1L;truth<-if(k==1)abs(te$z) else as.numeric(abs(te$z)>task$epsilon[k-1])
          all[[j]]<-data.table::data.table(task_index=task$index,site=te$site,participant_key=te$participant_key,
            Date=te$Date,repeat_id=rep_id,fold=f,state=state_name,target=colnames(y)[k],
            prediction=pr[,k],truth=truth,D=abs(te$z),context_group=group)
        }
      }
    }
  }
  obj<-list(complete=TRUE,status="complete",run_id=task$run_id,meta=task$meta,
    predictions=data.table::rbindlist(all),fold_audit=data.table::rbindlist(audits),elapsed=proc.time()[3]-started)
  tmp<-paste0(task$output,".tmp");saveRDS(obj,tmp,compress=TRUE)
  if(!file.rename(tmp,task$output))stop("Could not install reliability checkpoint")
  task$output
}

reliability_summarize <- function(paths,prov,out) {
  paths<-as.character(unlist(paths,use.names=FALSE))
  scores<-people<-profiles<-curves<-audit<-metadata<-timing<-list()
  for(i in seq_along(paths)){
    obj<-readRDS(paths[i]);metadata[[i]]<-obj$meta
    timing[[i]]<-data.table::data.table(task_index=obj$meta$task_index,status=obj$status,
      elapsed_seconds=if(is.null(obj$elapsed))NA_real_ else obj$elapsed)
    if(obj$status!="complete")next
    z<-obj$predictions;meta<-obj$meta
    scores[[i]]<-z[,.(mse=mean((prediction-truth)^2),mae=mean(abs(prediction-truth))),by=.(task_index,repeat_id,state,target)]
    # Each metric is an analytical unit; tolerance slices have equal diagnostic
    # weight. Participant clusters, not metrics, form uncertainty replicates.
    d<-z[startsWith(target,"exceed_") & repeat_id==1L]
    people[[i]]<-d[,.(sse=sum((prediction-truth)^2)/length(prov$epsilon),n=.N/length(prov$epsilon)),
      by=.(task_index,state,site,participant_key)]
    # Groups are defined using outer-training predicted context-risk cutpoints.
    # The same groups are used at every tolerance; no test-outcome grouping.
    d<-z[state=="context" & repeat_id==1L]
    profiles[[i]]<-d[,.(n=.N,predicted=mean(prediction),observed=mean(truth),A=mean(D)),by=.(task_index,target,context_group)]
    curves[[i]]<-d[,.(sum_D=sum(D),n=.N,events=sum(truth)),by=.(task_index,target,context_group,site,participant_key)]
    audit[[i]]<-cbind(task_index=meta$task_index,obj$fold_audit)
  }
  meta<-data.table::rbindlist(metadata)
  meta<-unique(meta[,.(task_index,metric,metric_class,dimension,comparison_pair_id,support_id)])
  scores<-merge(data.table::rbindlist(scores),meta,by="task_index")
  people<-merge(data.table::rbindlist(people),meta,by="task_index")
  profiles<-merge(data.table::rbindlist(profiles),meta,by="task_index")
  curves<-merge(data.table::rbindlist(curves),meta,by="task_index")
  data.table::fwrite(scores,file.path(out,"task_scores.csv"));data.table::fwrite(profiles,file.path(out,"context_profiles.csv"))
  data.table::fwrite(people,file.path(out,"participant_brier.csv"));data.table::fwrite(curves,file.path(out,"participant_profiles.csv"))
  data.table::fwrite(data.table::rbindlist(audit),file.path(out,"fold_audit.csv"))
  data.table::fwrite(data.table::rbindlist(timing),file.path(out,"task_status.csv"))
  pair_scores<-scores[startsWith(target,"exceed_"),.(mse=mean(mse)),by=.(comparison_pair_id,repeat_id,state)]
  pair_scores<-data.table::dcast(pair_scores,comparison_pair_id+repeat_id~state,value.var="mse")
  pair_scores[,`:=`(context_skill=100*(1-context/null),context_increment=100*(1-joint/measurement),
    capacity_control=100*(1-joint/measurement_capacity),site_control=100*(1-context/site_prior))]
  data.table::fwrite(pair_scores,file.path(out,"repeat_scores.csv"))
  # Fixed-prediction, site-stratified participant bootstrap. Repeated refits above
  # separately assess fold instability; this is not a refit bootstrap CI.
  pm<-unique(people[,.(site,participant_key)]);B<-prov$bootstrap
  set.seed(20260914);w<-matrix(0,nrow(pm),B)
  for(s in unique(pm$site)){ix<-which(pm$site==s);for(b in seq_len(B))w[ix,b]<-tabulate(sample.int(length(ix),length(ix),TRUE),nbins=length(ix))}
  ci<-list();j<-0L
  comparisons<-list(context_skill=c("context","null"),context_increment=c("joint","measurement"),
    capacity_control=c("joint","measurement_capacity"),site_control=c("context","site_prior"))
  for(pair in unique(people$comparison_pair_id)){
    d<-people[comparison_pair_id==pair];tasks<-sort(unique(d$task_index)); boot<-list();point<-numeric()
    for(st in unique(d$state)){
      zz<-d[state==st];num<-den<-matrix(0,length(tasks),nrow(pm));idx<-cbind(match(zz$task_index,tasks),match(zz$participant_key,pm$participant_key))
      num[idx]<-zz$sse;den[idx]<-zz$n
      boot[[st]]<-colMeans((num%*%w)/(den%*%w),na.rm=TRUE);point[st]<-mean(rowSums(num)/rowSums(den))
    }
    for(nm in names(comparisons)){
      cmp<-comparisons[[nm]];v<-100*(1-boot[[cmp[1]]]/boot[[cmp[2]]]);j<-j+1L
      ci[[j]]<-data.table::data.table(comparison_pair_id=pair,contrast=nm,estimate=100*(1-point[cmp[1]]/point[cmp[2]]),
        lo=quantile(v,.025),hi=quantile(v,.975))
    }
  }
  ci<-data.table::rbindlist(ci);data.table::fwrite(ci,file.path(out,"information_value_ci.csv"))
  # Pooled loss ratios use equal metric weighting. Group curves additionally
  # disclose their actual coverage; no unsupported group is assigned zero loss.
  profile<-profiles[,.(observed=mean(observed),predicted=mean(predicted),n_metrics=.N,n_days=sum(n)),
    by=.(comparison_pair_id,target,context_group)]
  profile[,coverage:=n_days/sum(n_days),by=.(comparison_pair_id,target)]
  data.table::fwrite(profile,file.path(out,"context_profile_summary.csv"))
  list(task_scores=scores,information_value=ci,repeat_scores=pair_scores,context_profiles=profile,metric_profiles=profiles)
}

reliability_finalize <- function(out=Sys.getenv("RQ2_RELIABILITY_RUN_DIR","")) {
  if(!nzchar(out))stop("Set RQ2_RELIABILITY_RUN_DIR for --summarize")
  prov<-readRDS(file.path(out,"provenance.rds"));paths<-Sys.glob(file.path(out,"checkpoints","task_*.rds"))
  if(length(paths)!=length(prov$input_md5))stop("Incomplete checkpoint set")
  for(p in paths){o<-readRDS(p);if(!isTRUE(o$complete)||!identical(o$run_id,basename(out)))stop("Invalid checkpoint ",p)}
  current<-readRDS("results/rq1/rq1_pairwise_change_long.rds")
  stopifnot(identical(prov$rq1_analysis_version,rq1_pairwise_version(current)),
    identical(prov$core_artifact_version,current$core_artifact_version))
  summaries<-reliability_summarize(paths,prov,out)
  artifact<-c(list(complete=TRUE,run_id=basename(out),provenance=prov,checkpoints=paths,run_dir=out,
    summary_code_md5=tools::md5sum("scripts/utils/rq2_conditional_reliability.R")),summaries)
  recovery_atomic(artifact,file.path(out,"reliability_manifest.rds"))
  recovery_atomic(artifact,"results/rq2/rq2_conditional_reliability.rds")
  message("Conditional reliability complete: ",out)
  invisible(out)
}

reliability_run <- function() {
  data.table::setDTthreads(1L);src<-reliability_source()
  pm<-data.table::fread(file.path(src$path,"participant_folds.csv"));pm[,repeat_id:=1L]
  maps<-list(pm)
  for(r in 2:3){
    z<-copy(pm);set.seed(20260913+r);z[,fold:=sample(rep(1:5,length.out=.N)),by=site];z[,repeat_id:=r];maps[[r]]<-z
  }
  folds<-data.table::rbindlist(maps)
  code<-c("scripts/12d_rq2_recovery.R","scripts/utils/rq2_conditional_reliability.R","scripts/utils/rq2_risk_models.R")
  prov<-list(rq2_analysis_version="conditional_reliability_v1",rq1_analysis_version=src$manifest$provenance$rq1_analysis_version,
    core_artifact_version=src$manifest$provenance$core_artifact_version,source_run=normalizePath(src$path,winslash="/"),
    input_md5=tools::md5sum(src$files),code_md5=tools::md5sum(code),folds=folds,epsilon=c(.05,.1,.2,.3,.5,1),
    context=c(src$manifest$provenance$predictors,src$manifest$provenance$temporal_predictors),
    signature=src$manifest$provenance$signature_predictors,edf=10L,bootstrap=1000L,
    validation="three site-stratified participant-grouped five-fold partitions; primary is the frozen source partition",
    estimand="P(abs(z)>epsilon | information), with mean abs(z) context profiles",
    loss="Brier score averaged equally across the six declared tolerance slices and then equally across available metrics",
    model="uniform additive natural-spline ridge; orthogonal context block preserves measurement baseline; monotone probability projection",
    R=R.version.string,packages=sapply(c("data.table","splines"),function(p)as.character(utils::packageVersion(p))))
  id<-recovery_hash(prov);out<-file.path("results/rq2/reliability",prov$rq1_analysis_version,id)
  dir.create(file.path(out,"checkpoints"),recursive=TRUE,showWarnings=FALSE)
  saveRDS(prov,file.path(out,"provenance.rds"));data.table::fwrite(folds,file.path(out,"participant_folds.csv"))
  tasks<-lapply(seq_len(nrow(src$catalog)),function(i)list(index=src$catalog$task_index[i],meta=src$catalog[i],input=src$files[i],
    output=file.path(out,"checkpoints",sprintf("task_%04d.rds",src$catalog$task_index[i])),run_id=id,
    folds=folds,epsilon=prov$epsilon,context=prov$context,signature=prov$signature))
  workers<-ms_resolve_workers("RQ2_RELIABILITY_WORKERS",if(.Platform$OS.type=="windows")12L else 36L,48L)
  message("Conditional reliability: ",length(tasks)," tasks, ",workers," workers; ",out)
  paths<-ms_parallel_map(tasks,reliability_task,workers,packages="data.table",exports=c("reliability_task","reliability_validate_input",
    "rq2_risk_design","rq2_risk_predict","rq2_risk_increment","rq2_risk_monotone"))
  reliability_finalize(out)
}
