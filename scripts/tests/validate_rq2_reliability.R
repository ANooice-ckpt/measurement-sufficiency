suppressPackageStartupMessages(library(data.table))
source("scripts/utils/rq2_risk_models.R")
# Boundary/tied knots, missing features and unseen extremes must be handled using
# training data only; test values must not alter the fitted training design.
tr<-data.frame(x=c(rep(0,15),1:5),binary=rep(0:1,10),missing=c(rep(NA,10),1:10),empty=NA_real_)
te<-data.frame(x=c(-10,100),binary=c(1,0),missing=c(NA,100),empty=NA_real_)
a<-rq2_risk_design(tr,te,names(tr));b<-rq2_risk_design(tr,transform(te,x=c(1e6,-1e6)),names(tr))
stopifnot(identical(a$train,b$train),all(is.finite(a$train)),all(is.finite(a$test)))
y<-cbind(rep(3,nrow(tr)),seq_len(nrow(tr)))
pr<-rq2_risk_predict(a,y,10,TRUE)
stopifnot(max(abs(pr$test[,1]-3))<1e-8)
zero<-rq2_risk_predict(list(train=matrix(0,20,0),test=matrix(0,2,0)),y,10,TRUE)
stopifnot(identical(dim(zero$train),c(20L,2L)),all(zero$test[,1]==3))
# Known isotonic solutions and the nonexpansion property for a valid CDF target.
p<-rbind(c(.8,.2,.6),c(.1,.2,.3),c(1,.5,0))
q<-rq2_risk_monotone(p)
stopifnot(max(abs(q[1,]-c(.8,.4,.4)))<1e-8,max(abs(q[2,]-.2))<1e-8,
  all(q[,1]>=q[,2]),all(q[,2]>=q[,3]),identical(q[3,],p[3,]))
truth<-rbind(c(1,0,0),c(1,1,0),c(1,0,0))
stopifnot(all(rowSums((q-truth)^2)<=rowSums((p-truth)^2)+1e-10))
# No context columns must reproduce the measurement baseline exactly.
base<-rq2_risk_predict(a,y,10)
joint<-rq2_risk_increment(a,list(train=matrix(0,20,0),test=matrix(0,2,0)),y,10)
stopifnot(identical(base,joint))
# Redundant context must add no information, including a rank-deficient low block.
x <- matrix(seq(-1,1,length.out=100), ncol=1)
low <- list(train=cbind(x,x), test=cbind(x,x))
ctx <- list(train=x, test=x)
stopifnot(identical(rq2_risk_predict(low,x,10), rq2_risk_increment(low,ctx,x,10)))
# Load only the pure display helper: never execute the plotting entrypoint.
plot_code<-parse("scripts/13b_plot_fig4.R",encoding="UTF-8")
display_helper<-Filter(function(e)is.call(e) && identical(e[[1]],as.name("<-")) &&
  identical(e[[2]],as.name("fig4_increment_display")),as.list(plot_code))
stopifnot(length(display_helper)==1L)
eval(display_helper[[1]])
scores<-CJ(task_index=1:2,repeat_id=1:3,target=c("exceed_0.1","exceed_0.5"),
  state=c("measurement","measurement_capacity","joint"))
scores[,`:=`(metric=paste0("metric_",task_index),metric_class="level",dimension="placement",
  comparison_pair_id="chest_vs_eye",support_id="eye_chest")]
scores[,mse:=ifelse(task_index==1,0,ifelse(state=="measurement",.05,
  ifelse(state=="measurement_capacity",.035,ifelse(repeat_id==2,.06,.04))))]
scores[target=="exceed_0.5",mse:=mse/2]
unchanged<-copy(scores)
value<-fig4_increment_display(scores)
stopifnot(identical(scores,unchanged),nrow(value)==24L,
  all(value[task_index==1]$brier_reduction==0),
  abs(value[task_index==2 & repeat_id==1 & epsilon==.1 & baseline=="measurement"]$brier_reduction-.01)<1e-12,
  abs(value[task_index==2 & repeat_id==2 & epsilon==.1 & baseline=="measurement"]$brier_reduction+.01)<1e-12,
  abs(value[task_index==2 & repeat_id==1 & epsilon==.5 & baseline=="measurement"]$brier_reduction-.005)<1e-12,
  all(value[task_index==2 & baseline=="measurement_capacity"]$brier_reduction<0),
  inherits(try(fig4_increment_display(rbind(scores,scores[1])),silent=TRUE),"try-error"))
# Compile just the new atlas with synthetic frozen scores, without sourcing the
# plotting entrypoint or creating any image/result file.
local({
  suppressPackageStartupMessages(library(ggplot2))
  source("scripts/utils/figure_style.R",local=TRUE)
  z<-list(task_scores=scores)
  ORDER<-c("chest_vs_eye","wrist_vs_eye","LIGHT_vs_MEDI","20s_vs_10s",
    "30s_vs_10s","40s_vs_10s","60s_vs_10s","120s_vs_10s")
  theme_risk<-function()theme_minimal()
  assigned<-function(e,name)is.call(e) && identical(e[[1]],as.name("<-")) &&
    identical(e[[2]],as.name(name))
  start<-which(vapply(plot_code,assigned,logical(1),name="increment"))
  end<-which(vapply(plot_code,function(e)assigned(e,"pc") && "atlas"%in%all.names(e),logical(1)))
  stopifnot(length(start)==1L,length(end)==1L)
  eval(plot_code[start:end])
  built<-ggplot_build(pc)
  stopifnot(nrow(built$data[[1]])==4L,nrow(built$data[[2]])==2L,
    all(!is.na(built$data[[1]]$fill)),all(is.finite(built$data[[1]]$x)))
})
# Exercise only the legacy cache reader and its in-memory summaries, never the
# RQ2 entrypoint, model fitting, CSV writes or downstream context stage.
check_legacy_cache <- function() {
  suppressPackageStartupMessages({library(dplyr);library(purrr)})
  code<-parse("scripts/12_rq2_analysis.R",encoding="UTF-8")
  assigned<-function(e,names)is.call(e) && identical(e[[1]],as.name("<-")) &&
    is.symbol(e[[2]]) && as.character(e[[2]])%in%names
  selected<-vapply(code,function(e) {
    assigned(e,c("sanitize_task","model_tasks","model_results","model_coefficients",
      "model_performance","model_manifest")) ||
      (is.call(e) && identical(e[[1]],as.name("for")) &&
        identical(e[[2]],as.name("task")) && identical(e[[3]],as.name("model_tasks"))) ||
      (is.call(e) && identical(e[[1]],as.name("if")) &&
        (identical(e[[2]],quote(!ncol(model_coefficients))) ||
         identical(e[[2]],quote(!ncol(model_performance)))))
  },logical(1))
  stopifnot(sum(selected)==10L)
  cache_code<-code[selected]
  CHECKPOINTS<-tempfile("rq2_legacy_",tmpdir=tempdir());dir.create(CHECKPOINTS)
  stopifnot(identical(dirname(normalizePath(CHECKPOINTS,winslash="/")),normalizePath(tempdir(),winslash="/")))
  on.exit(unlink(CHECKPOINTS,recursive=TRUE),add=TRUE)
  PROGRESS_LOG<-file.path(CHECKPOINTS,"unused_progress.tsv")
  RQ2_VERSION<-"rq2_fixture";CORE_VERSION<-"core_fixture";RQ1_VERSION<-"rq1_fixture"
  RQ2_CV_FOLDS<-5L;MODEL_SEED<-100L;RUN_MODELS<-FALSE
  task_catalog<-tibble(task_index=c(7L,4L,6L,1L,5L,2L,3L),dimension="placement",
    comparison_pair_id="chest_vs_eye",metric=paste0("metric_",task_index),metric_class="level")
  shard_manifest<-tibble(task_index=c(7L,4L,1L,5L,2L,3L),
    shard_path=paste0("unused_shard_",task_index),bytes=100)
  expected_version<-"rq2_fixture__core__core_fixture__participant_cv__5"
  fixture<-function(i)list(checkpoint_version=expected_version,complete=TRUE,
    coefficients=tibble(dimension="placement",comparison_pair_id="chest_vs_eye",
      metric=paste0("metric_",i),outcome=c("signed","magnitude"),model_family="joint",
      random_structure="participant",term=c("z_b","z_a"),estimate=c(i+.2,i+.1),
      std_error=.2,df=10,t_value=2,p_value=.1),
    performance=tibble(dimension="placement",comparison_pair_id="chest_vs_eye",
      metric=paste0("metric_",i),outcome=c("signed","magnitude"),model_family="joint",
      validation_scheme="participant_grouped",n_participants=4L,n_sites=2L,
      n_test=20L,rmse=c(.2,.3),mae=c(.1,.2),r2=c(.3,.4)))
  for(i in c(1L,2L,3L,6L,7L)) {
    obj<-fixture(i)
    if(i==2L)obj$checkpoint_version<-"obsolete"
    if(i==3L)obj$complete<-FALSE
    saveRDS(obj,file.path(CHECKPOINTS,sprintf("task_%04d.rds",i)))
  }
  writeLines("not an RDS checkpoint",file.path(CHECKPOINTS,"task_0004.rds"))
  frozen_hashes<-tools::md5sum(list.files(CHECKPOINTS,full.names=TRUE))
  eval(cache_code)
  stopifnot(identical(vapply(model_tasks,`[[`,integer(1),"index"),c(7L,4L,1L,5L,2L,3L)),
    all(vapply(model_tasks,`[[`,character(1),"checkpoint_version")==expected_version),
    identical(which(!vapply(model_results,is.null,logical(1))),c(1L,7L)),
    identical(model_coefficients,bind_rows(fixture(1L)$coefficients,fixture(7L)$coefficients)),
    identical(model_performance,bind_rows(fixture(1L)$performance,fixture(7L)$performance)),
    identical(model_manifest$task_index,task_catalog$task_index),
    identical(model_manifest$checkpoint_complete,c(TRUE,FALSE,FALSE,TRUE,FALSE,FALSE,FALSE)),
    identical(model_manifest$checkpoint_present,c(TRUE,TRUE,TRUE,TRUE,FALSE,TRUE,TRUE)),
    all(!model_manifest$run_models),!file.exists(PROGRESS_LOG))
  # With no compatible cache, preserve the public empty-table column contracts.
  RQ2_VERSION<-"unmatched_fixture"
  eval(cache_code)
  stopifnot(nrow(model_coefficients)==0L,nrow(model_performance)==0L,
    identical(names(model_coefficients),c("dimension","comparison_pair_id","metric","outcome",
      "model_family","random_structure","term","estimate","std_error","df","t_value","p_value")),
    identical(names(model_performance),c("dimension","comparison_pair_id","metric","outcome",
      "model_family","validation_scheme","n_participants","n_sites","n_test","rmse","mae","r2")),
    all(!model_manifest$checkpoint_complete),
    identical(frozen_hashes,tools::md5sum(list.files(CHECKPOINTS,full.names=TRUE))))
}
check_legacy_cache()
cat("Conditional reliability numerical contracts passed\n")
if("--frozen"%in%commandArgs(TRUE)){
  a<-readRDS("results/rq2/rq2_conditional_reliability.rds");stopifnot(isTRUE(a$complete))
  complete<-unavailable<-0L
  for(path in a$checkpoints){
    o<-readRDS(path);stopifnot(isTRUE(o$complete),identical(o$run_id,a$run_id))
    if(o$status!="complete"){unavailable<-unavailable+1L;next}
    complete<-complete+1L;d<-o$predictions
    stopifnot(!anyDuplicated(d[,.(participant_key,Date,repeat_id,state,target)]),
      all(is.finite(d$prediction)),all(is.finite(d$truth)),
      all(d[,uniqueN(fold),by=.(participant_key,repeat_id)]$V1==1L))
    counts<-d[,.(n=.N),by=.(repeat_id,state,target)]
    stopifnot(uniqueN(counts$n)==1L,uniqueN(d$repeat_id)==3L)
    p<-d[startsWith(target,"exceed_")]
    stopifnot(all(p$prediction>=0 & p$prediction<=1),
      all(p$truth==as.numeric(p$D>as.numeric(sub("exceed_","",p$target)))))
    wide<-dcast(p,participant_key+Date+repeat_id+state~target,value.var="prediction")
    mat<-as.matrix(wide[,paste0("exceed_",a$provenance$epsilon),with=FALSE])
    stopifnot(all(mat[,-ncol(mat),drop=FALSE]>=mat[,-1,drop=FALSE]-1e-10))
    stopifnot(all(d[,uniqueN(context_group),by=.(participant_key,Date,repeat_id)]$V1==1L))
  }
  stopifnot(complete==414L,unavailable==2L)
  cat("Frozen audit passed: 414 complete, 2 unavailable; paired supports, grouped folds, target definitions and monotone probabilities\n")
}
