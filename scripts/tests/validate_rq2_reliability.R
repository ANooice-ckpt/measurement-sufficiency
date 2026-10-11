suppressPackageStartupMessages(library(data.table))
source("scripts/utils/rq2_conditional_reliability.R")
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
# Bootstrap replicates keep a fixed metric set: if one metric has no sampled
# support, invalidate the whole replicate rather than silently reweight metrics.
num<-matrix(c(.1,0,0,.2),nrow=2,byrow=TRUE);den<-diag(2)
weights<-cbind(c(1,1),c(1,0));fixed<-reliability_equal_metric_boot(num,den,weights)
stopifnot(abs(fixed[1]-.15)<1e-12,is.na(fixed[2]),
  is.na(reliability_improvement(0,0)),
  abs(reliability_improvement(.1,.2)-50)<1e-12)
# Load only the pure display helpers: never execute the plotting entrypoint.
plot_code<-parse("scripts/13b_plot_fig4.R",encoding="UTF-8")
for(helper_name in c("fig4_increment_display","fig4_risk_scatter_summary")){
  helper<-Filter(function(e)is.call(e) && identical(e[[1]],as.name("<-")) &&
    identical(e[[2]],as.name(helper_name)),as.list(plot_code))
  stopifnot(length(helper)==1L);eval(helper[[1]])
}
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
risk_profiles<-data.table(
  task_index=c(1L,1L,2L),metric=c("m1","m1","m2"),dimension="placement",
  comparison_pair_id="chest_vs_eye",target="exceed_0.1",
  context_group=c("Lower","Higher","Lower"),observed=c(.1,.3,.2))
risk<-fig4_risk_scatter_summary(risk_profiles)
stopifnot(all(c("Lower","Higher")%in%names(risk$cloud)),nrow(risk$spread)==1L,
  risk$spread$n_paired==1L,abs(risk$spread$x-.1)<1e-12,abs(risk$spread$y-.3)<1e-12)
lower_only<-fig4_risk_scatter_summary(risk_profiles[context_group=="Lower"])
stopifnot("Higher"%in%names(lower_only$cloud),all(is.na(lower_only$cloud$Higher)),
  nrow(lower_only$spread)==0L)
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
# Execute the streamed distribution producer and its existing pooled summary.
# The context-model stage remains the sole writer of model input shards.
local({
  suppressPackageStartupMessages(library(tidyverse))
  root <- tempfile("rq2_fragments_"); dir.create(root)
  on.exit(unlink(root,recursive=TRUE),add=TRUE)
  CONDITIONAL_DIR <- file.path(root,"conditional"); dir.create(CONDITIONAL_DIR)
  CORE_VERSION <- RQ1_VERSION <- RQ2_VERSION <- "synthetic"
  code <- parse("scripts/12_rq2_analysis.R")
  assignment <- function(name) {
    e <- Filter(function(x)is.call(x) && identical(x[[1]],as.name("<-")) &&
      identical(x[[2]],as.name(name)),as.list(code))
    stopifnot(length(e)==1L); e[[1]]
  }
  for (name in c("normalize_primary","part_token","safe_q","build_conditional_part")) eval(assignment(name))
  rows <- tibble(dimension="temporal",comparison_pair_id="20s_vs_10s",pair_available=TRUE,
    adjacent_transition=TRUE,Date=as.Date("2025-01-01"),n_days_a=NA_integer_,n_days_b=NA_integer_,
    config_a_id="eye__MEDI__20s",config_b_id="eye__MEDI__10s",config_a_label="20 s",config_b_label="10 s",
    support_id="eye_medi",site="test",Id=c("a","b","a","b"),
    analysis_unit_id_a=letters[1:4],analysis_unit_id_b=letters[1:4],metric="synthetic",
    metric_class="level",metric_geometry="linear",available=c(TRUE,TRUE,TRUE,FALSE),z=c(1,-3,5,999))
  unit_features <- normalize_primary(rows) |>
    transmute(transition_unit_key,participant_key=paste(site,Id),primary_state_name="test",
      primary_state_raw=c(1,2,3,4),state_bin=c(1L,1L,2L,1L),state_bin_label=c("Low","Low","High","Low"))
  input <- file.path(root,"pairs.rds");saveRDS(rows,input)
  path <- build_conditional_part(input,1L); hash <- tools::md5sum(path)
  stopifnot(identical(build_conditional_part(input,1L),path),identical(tools::md5sum(path),hash),
    setequal(list.files(CONDITIONAL_DIR),c("part_000.rds","part_000.rds.ok")))
  conditional_parts <- list(readRDS(path),readRDS(path))
  eval(assignment("conditional_geometry"))
  low <- conditional_geometry |> filter(state_bin_label=="Low")
  stopifnot(nrow(low)==1L,low$n_units==4L,low$n_participants==2L,
    low$A_conditional==2,low$B_conditional == -1,
    conditional_geometry$A_conditional[conditional_geometry$state_bin_label=="High"]==5)
  # Disabling models preserves empty schemas and current provenance, not old fits.
  OUT <- file.path(root,"out");dir.create(OUT); RUN_MODELS <- FALSE
  task_catalog <- tibble(task_index=1L,dimension="temporal",comparison_pair_id="20s_vs_10s",
    metric="synthetic",metric_class="level")
  source("scripts/12c_rq2_context_models.R",local=TRUE)
  stopifnot(nrow(readr::read_csv(file.path(OUT,"rq2_model_coefficients.csv"),show_col_types=FALSE))==0L)
  m <- readr::read_csv(file.path(OUT,"rq2_model_artifact_manifest.csv"),show_col_types=FALSE)
  stopifnot(!m$run_models,!m$checkpoint_complete,m$rq1_analysis_version==RQ1_VERSION)
})
cat("Conditional reliability and streamed distribution contracts passed\n")
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
