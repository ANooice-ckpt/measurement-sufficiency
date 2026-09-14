# Read-only with respect to Core/RQ1/Fig.3 and the historical recovery run.
# Rscript --vanilla scripts/diagnose_rq2_information.R audit|experiment [run_dir]
suppressPackageStartupMessages({library(data.table)})
source("scripts/utils/parallel_runtime.R")

rq2_diag_source <- function(path = "") {
  if (!nzchar(path)) {
    paths <- Sys.glob("results/rq2/recovery/*/*/recovery_manifest.rds")
    paths <- paths[vapply(paths, function(p) {
      m <- readRDS(p)
      isTRUE(m$complete) && identical(m$provenance$estimator_version, "anchored_shared_split_candidate_library_v2")
    }, logical(1))]
    if (length(paths) != 1L) stop("Specify one completed source run explicitly")
    path <- dirname(paths)
  }
  m <- readRDS(file.path(path, "recovery_manifest.rds"))
  stopifnot(isTRUE(m$complete), !any(m$statuses$status == "failed"))
  list(path = path, manifest = m)
}

# Fractional inclusion of tied scores avoids arbitrary ordering of null predictions.
# This is a retrospective batch risk-ranking diagnostic, not a deployable threshold.
rq2_diag_weight <- function(score, coverage) {
  rank <- rank(score, ties.method = "min") - 1
  ties <- ave(score, score, FUN = length)
  pmax(0, pmin(1, (coverage * length(score) - rank) / ties))
}

rq2_diag_audit <- function(src, out) {
  rows <- list(); selection <- list()
  status <- as.data.table(src$manifest$statuses)[status == "complete"]
  for (j in seq_len(nrow(status))) {
    cp <- file.path(src$path, "checkpoints", sprintf("task_%04d.rds", status$task_index[j]))
    z <- readRDS(cp); d <- as.data.table(z$predictions)[estimand == "observability"]
    d[, score_rank := frank(prediction, ties.method = "average") / .N, by = .(fold, state)]
    ans <- d[, .(rho = if (sd(prediction) > 0 && sd(raw_distortion) > 0)
      cor(prediction, raw_distortion, method = "spearman") else NA_real_), by = state]
    curves <- rbindlist(lapply(c(.2,.4,.6,.8,1), function(q) {
      tmp <- copy(d); tmp[, w := rq2_diag_weight(prediction, q), by = .(fold,state)]
      tmp[, .(coverage = q, risk = sum(w * raw_distortion) / sum(w), raw_A = mean(raw_distortion)), by = state]
    }))
    ans <- merge(curves, ans, by = "state")
    for (key in c("task_index","metric","metric_class","dimension","comparison_pair_id")) ans[, (key) := status[[key]][j]]
    rows[[j]] <- ans
    selection[[j]] <- rbindlist(lapply(z$models, function(m) {
      if (is.null(m$selected_kind)) return(NULL)
      data.table(task_index = status$task_index[j], estimand = m$estimand, state = m$state,
        selected = m$selected_kind, inner_gain = 1 - m$inner_selected_loss / m$inner_baseline_loss)
    }))
  }
  d <- rbindlist(rows); fwrite(d, file.path(out, "historical_ranking_tasks.csv"))
  fwrite(d[, .(risk = mean(risk), raw_A = mean(raw_A), median_rho = median(rho, na.rm=TRUE)),
    by = .(comparison_pair_id,coverage,state)], file.path(out, "historical_ranking_summary.csv"))
  fwrite(rbindlist(selection)[, .N, by = .(estimand,state,selected)], file.path(out,"historical_selection.csv"))
}

source("scripts/utils/rq2_risk_models.R")
rq2_diag_design <- rq2_risk_design
rq2_diag_predict <- rq2_risk_predict
rq2_diag_increment <- rq2_risk_increment

rq2_diag_task <- function(task) {
  x<-as.data.table(readRDS(task$input))[eligible == TRUE]
  if(nrow(x)<20 || uniqueN(x$participant_key)<4) return(NULL)
  circular<-unique(x$metric_geometry)=="circular_time"
  x[, low_1 := if(circular)sin(candidate_value*2*pi/86400) else candidate_value]
  x[, low_2 := if(circular)cos(candidate_value*2*pi/86400) else 0]
  states<-list(context=task$context, measurement=c("low_1","low_2",task$signature),
    joint=c("low_1","low_2",task$signature,task$context))
  ans<-list(); k<-0L
  for(f in sort(unique(x$fold))) {
    tr<-x[fold!=f];te<-x[fold==f]
    stopifnot(!any(tr$participant_key %in% te$participant_key))
    d<-abs(tr$z); scl<-mean(d); if(scl<=1e-12)scl<-1
    targets<-cbind(mean_risk=d, log_risk=log1p(d/scl), signed=tr$z,
      sapply(task$epsilon,function(e)as.numeric(d>e)))
    colnames(targets)[4:ncol(targets)]<-paste0("exceed_",task$epsilon)
    designs <- lapply(states,function(features)rq2_diag_design(tr,te,features))
    state_names <- if(isTRUE(task$increment))c("orthogonal_joint") else c("null",names(states))
    for(state in state_names) {
      if(state=="null")pred<-matrix(colMeans(targets),nrow(te),ncol(targets),byrow=TRUE)
      else if(state=="orthogonal_joint")pred<-rq2_diag_increment(designs$measurement,designs$context,targets,task$edf)
      else pred<-rq2_diag_predict(designs[[state]],targets,task$edf)
      for(t in seq_len(ncol(targets))) {
        prediction<-pred[,t]
        if(t>=4)prediction<-pmax(0,pmin(1,prediction))
        if(t<=2)prediction<-pmax(0,prediction)
        truth<-if(t==1)abs(te$z) else if(t==2)log1p(abs(te$z)/scl) else if(t==3)te$z else as.numeric(abs(te$z)>task$epsilon[t-3])
        k<-k+1L
        ans[[k]]<-data.table(task_index=task$index,metric=unique(x$metric),metric_class=unique(x$metric_class),
          comparison_pair_id=unique(x$comparison_pair_id),dimension=unique(x$dimension),
          site=te$site,participant_key=te$participant_key,Date=te$Date,fold=f,state=state,
          target=colnames(targets)[t],prediction=prediction,truth=truth,D=abs(te$z))
      }
    }
  }
  z<-rbindlist(ans);saveRDS(z,task$output,compress=TRUE)
  task$output
}

rq2_diag_experiment <- function(src,out,increment=FALSE) {
  prov<-list(version="fixed_additive_risk_v1",upstream=src$manifest$provenance,
    edf=10,epsilon=c(.05,.1,.2,.3,.5,1),rule="three-df natural spline per feature; ridge effective df=10; no tuning",
    dictionary=src$manifest$provenance[c("predictors","signature_predictors","temporal_predictors")],
    code_md5=tools::md5sum("scripts/diagnose_rq2_information.R"),orthogonal_increment=increment)
  if(increment){out<-file.path(out,"orthogonal_increment");dir.create(out,showWarnings=FALSE)}
  saveRDS(prov,file.path(out,"experiment_provenance.rds"))
  dir.create(file.path(out,"predictions"),showWarnings=FALSE)
  catalog<-as.data.table(src$manifest$statuses)[status=="complete"]
  tasks<-lapply(catalog$task_index,function(i)list(index=i,
    input=file.path(src$path,"inputs",sprintf("task_%04d.rds",i)),
    output=file.path(out,"predictions",sprintf("task_%04d.rds",i)),
    context=c(prov$dictionary$predictors,prov$dictionary$temporal_predictors),
    signature=prov$dictionary$signature_predictors,epsilon=prov$epsilon,edf=prov$edf,increment=increment))
  workers<-ms_resolve_workers("RQ2_DIAGNOSTIC_WORKERS",12,12)
  message("Fixed-rule risk experiment: ",length(tasks)," tasks; ",workers," workers")
  paths<-ms_parallel_map(tasks,rq2_diag_task,workers,packages="data.table",
    exports=c("rq2_diag_task","rq2_diag_predict","rq2_diag_design","rq2_diag_increment","rq2_risk_design","rq2_risk_predict","rq2_risk_increment"))
  metrics<-rbindlist(lapply(paths,function(p){
    z<-readRDS(p)
    z[, .(mse=mean((prediction-truth)^2),mae=mean(abs(prediction-truth)),
      rho=if(sd(prediction)>0 && sd(truth)>0)cor(prediction,truth,method="spearman") else NA_real_),
      by=.(task_index,metric,metric_class,comparison_pair_id,dimension,state,target)]
  }))
  fwrite(metrics,file.path(out,"experiment_scores.csv"))
  fwrite(metrics[,.(mse=mean(mse),mae=mean(mae),median_rho=median(rho,na.rm=TRUE)),
    by=.(comparison_pair_id,state,target)],file.path(out,"experiment_summary.csv"))
}

if(sys.nframe()==0L){
  args<-commandArgs(TRUE);if(!length(args)||!args[1]%in%c("audit","experiment","increment"))stop("Use audit|experiment|increment [source_run]")
  src<-rq2_diag_source(if(length(args)>1)args[2] else "")
  out<-file.path("results/rq2/information_diagnostics",basename(src$path));dir.create(out,recursive=TRUE,showWarnings=FALSE)
  if(args[1]=="audit")rq2_diag_audit(src,out) else rq2_diag_experiment(src,out,args[1]=="increment")
  message("Diagnostics written to ",out)
}
