# Frozen diagnostic predictions only; does not fit predictive models.
suppressPackageStartupMessages(library(data.table))
source("scripts/diagnose_rq2_information.R")
src<-rq2_diag_source(); out<-file.path("results/rq2/information_diagnostics",basename(src$path))
paths<-Sys.glob(file.path(out,"predictions","*.rds"))
rows<-list(); people<-list(); cal<-list(); hetero<-list()
for(j in seq_along(paths)) {
  z<-as.data.table(readRDS(paths[j])); keep<-unique(z[,.(task_index,metric,metric_class,comparison_pair_id,dimension)])
  p<-file.path(out,"orthogonal_increment","predictions",basename(paths[j]))
  if(file.exists(p))z<-rbind(z,readRDS(p))
  d<-z[target %in% c("mean_risk","log_risk")]
  rows[[j]]<-rbindlist(lapply(c(.2,.4,.6,.8,1),function(q){
    d[,w:=rq2_diag_weight(prediction,q),by=.(state,target,fold)]
    d[,.(coverage=q,risk=sum(w*D)/sum(w),raw_A=mean(D)),
      by=.(task_index,metric,metric_class,comparison_pair_id,dimension,state,target)]
  }))
  d<-z[startsWith(target,"exceed_")]
  d[,error:=(prediction-truth)^2]
  # Average across the prespecified tolerance grid, then retain participant sums
  # for paired site-stratified cluster bootstrap with metric-specific supports.
  people[[j]]<-d[,.(sse=sum(error)/uniqueN(target),n=.N/uniqueN(target)),
    by=.(task_index,comparison_pair_id,metric_class,state,site,participant_key)]
  cal[[j]]<-d[,.(n=.N,predicted=mean(prediction),observed=mean(truth)),
    by=.(comparison_pair_id,state,bin=pmin(9L,floor(prediction*10)))]
}
fwrite(rbindlist(rows),file.path(out,"new_ranking_tasks.csv"))
fwrite(rbindlist(rows)[,.(risk=mean(risk),raw_A=mean(raw_A)),by=.(comparison_pair_id,state,target,coverage)],file.path(out,"new_ranking_summary.csv"))
per<-rbindlist(people);fwrite(per,file.path(out,"participant_brier.csv"))
cc<-rbindlist(cal);fwrite(cc[,.(n=sum(n),predicted=weighted.mean(predicted,n),observed=weighted.mean(observed,n)),by=.(comparison_pair_id,state,bin)],file.path(out,"probability_calibration.csv"))
set.seed(20260914); pm<-unique(per[,.(site,participant_key)]);B<-1000L
w<-matrix(0,nrow(pm),B)
for(s in unique(pm$site)){ix<-which(pm$site==s);for(b in seq_len(B))w[ix,b]<-tabulate(sample.int(length(ix),length(ix),TRUE),nbins=length(ix))}
ans<-list();k<-0L
for(pair in unique(per$comparison_pair_id)){
 d<-per[comparison_pair_id==pair]; tasks<-sort(unique(d$task_index)); pindex<-match(d$participant_key,pm$participant_key)
 scores<-list();observed<-numeric()
 for(state_name in unique(d$state)){
  zz<-d[state==state_name]; num<-den<-matrix(0,length(tasks),nrow(pm));idx<-cbind(match(zz$task_index,tasks),match(zz$participant_key,pm$participant_key));num[idx]<-zz$sse;den[idx]<-zz$n
  scores[[state_name]]<-colMeans((num%*%w)/(den%*%w),na.rm=TRUE)
  observed[state_name]<-mean(rowSums(num)/rowSums(den))
 }
 for(cmp in list(c("context","null"),c("measurement","null"),c("joint","measurement"),c("orthogonal_joint","measurement"))){
  if(!all(cmp%in%names(scores)))next
  v<-100*(1-scores[[cmp[1]]]/scores[[cmp[2]]]);k<-k+1L
  ans[[k]]<-data.table(comparison_pair_id=pair,state=cmp[1],baseline=cmp[2],
    skill=100*(1-observed[cmp[1]]/observed[cmp[2]]),lo=quantile(v,.025),hi=quantile(v,.975))
 }
}
fwrite(rbindlist(ans),file.path(out,"brier_bootstrap.csv"))
print(rbindlist(ans))
