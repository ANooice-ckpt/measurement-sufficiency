# Context-stratified cadence policy diagnostic from frozen OOS predictions.
# This targets agreement with 10 s, NOT RQ3 all-higher observed sufficiency.
suppressPackageStartupMessages(library(data.table));setDTthreads(1L)
source("scripts/diagnose_rq2_information.R")
src<-rq2_diag_source();out<-file.path("results/rq2/information_diagnostics",basename(src$path))
paths<-Sys.glob(file.path(out,"predictions","*.rds"))
all<-rbindlist(lapply(paths,function(p){
 z<-readRDS(p);z[dimension=="temporal" & target=="mean_risk" & state%in%c("null","context")]
}))
all[,cadence:=as.numeric(sub("s_vs_10s","",comparison_pair_id))]
keys<-c("metric","metric_class","site","participant_key","Date","fold","state")
support<-all[,.(n_cadences=uniqueN(cadence)),by=keys]
fwrite(support,file.path(out,"cadence_policy_support.csv"))
# Explicit metric-specific complete temporal support for this policy comparison.
# No placement intersection and no pooling metrics as independent replicates.
all<-merge(all,support[n_cadences==5L],by=keys)
ans<-list()
for(e in c(0,.025,.05,.075,.1,.15,.2,.3,.5,.75,1)){
 d<-copy(all);d[,accepted:=prediction<=e]
 z<-d[,{
  ok<-which(accepted)
  if(length(ok)){j<-ok[which.max(cadence[ok])];list(cadence=cadence[j],D=D[j],escalated=FALSE)}
  else list(cadence=10,D=0,escalated=TRUE)
 },by=keys]
 z[,`:=`(epsilon=e,sample_fraction=10/cadence)]
 ans[[length(ans)+1L]]<-z
}
d<-rbindlist(ans);saveRDS(d,file.path(out,"cadence_policy_days.rds"))
metric<-d[,.(A=mean(D),sample_fraction=mean(sample_fraction),escalated=mean(escalated)),by=.(metric,metric_class,state,epsilon)]
fwrite(metric,file.path(out,"cadence_policy_metrics.csv"))
print(metric[,.(A=mean(A),sample_fraction=mean(sample_fraction),escalated=mean(escalated)),by=.(state,epsilon)])
fwrite(metric[,.(A=mean(A),sample_fraction=mean(sample_fraction),escalated=mean(escalated)),by=.(state,epsilon)],file.path(out,"cadence_policy_summary.csv"))
