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
