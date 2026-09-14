# Descriptive context composition, paired profile uncertainty and a fixed
# one-day participant-calibration diagnostic. Reads frozen fits only.
suppressPackageStartupMessages(library(data.table));setDTthreads(1L)
z<-readRDS("results/rq2/rq2_conditional_reliability.rds");out<-z$run_dir
rows<-calibration<-list();features<-z$provenance$context
for(i in seq_along(z$checkpoints)){
 o<-readRDS(z$checkpoints[i]);if(o$status!="complete")next
 x<-as.data.table(readRDS(file.path(z$provenance$source_run,"inputs",sprintf("task_%04d.rds",o$meta$task_index))))[eligible==TRUE]
 p<-o$predictions[repeat_id==1 & state=="context" & target=="mean_risk"]
 x<-merge(x,p[,.(participant_key,Date,context_group,prediction)],by=c("participant_key","Date"))
 comp<-rbindlist(lapply(features,function(f){
  sd_all<-sd(x[[f]],na.rm=TRUE)
  x[,.(feature=f,n_observed=sum(is.finite(get(f))),mean=mean(get(f),na.rm=TRUE),scale=sd_all),by=context_group]
 }))
 comp[,`:=`(task_index=o$meta$task_index,comparison_pair_id=o$meta$comparison_pair_id)]
 rows[[i]]<-comp
 # Exactly the first available paired day calibrates later days. This changes
 # information availability and is NOT the zero-calibration primary estimand.
 setorder(x,participant_key,Date)
 x[,`:=`(first_z=z[1],first_residual=abs(z[1])-prediction[1],later=seq_len(.N)>1L),by=participant_key]
 y<-x[later==TRUE]
 if(nrow(y)){
  e<-y$z-y$first_z
  if(o$meta$metric_geometry=="circular_time")e<-(((e*y$standardizer+43200)%%86400)-43200)/y$standardizer
  calibration[[i]]<-data.table(task_index=o$meta$task_index,comparison_pair_id=o$meta$comparison_pair_id,
    n_days=nrow(y),raw_A=mean(abs(y$z)),one_day_calibrated_A=mean(abs(e)),
    context_risk_mse=mean((abs(y$z)-y$prediction)^2),
    one_day_risk_mse=mean((abs(y$z)-pmax(0,y$prediction+y$first_residual))^2))
 }
}
fwrite(rbindlist(rows),file.path(out,"context_composition_tasks.csv"))
cc<-rbindlist(rows);fwrite(cc[,.(mean=mean(mean,na.rm=TRUE),n_tasks=.N),by=.(comparison_pair_id,feature,context_group)],file.path(out,"context_composition.csv"))
wide<-dcast(cc,task_index+comparison_pair_id+feature+scale~context_group,value.var="mean")
wide[,standardized_difference:=(Higher-Lower)/scale]
fwrite(wide[,.(standardized_difference=mean(standardized_difference[is.finite(standardized_difference)])),by=.(comparison_pair_id,feature)],file.path(out,"context_composition_contrasts.csv"))
fwrite(rbindlist(calibration),file.path(out,"one_day_calibration_tasks.csv"))
fwrite(rbindlist(calibration)[,lapply(.SD,mean),by=comparison_pair_id,.SDcols=c("raw_A","one_day_calibrated_A","context_risk_mse","one_day_risk_mse")],file.path(out,"one_day_calibration_summary.csv"))

d<-fread(file.path(out,"participant_profiles.csv"));pm<-unique(d[,.(site,participant_key)]);set.seed(20260914);B<-1000
w<-matrix(0,nrow(pm),B)
for(s in unique(pm$site)){ix<-which(pm$site==s);for(b in seq_len(B))w[ix,b]<-tabulate(sample.int(length(ix),length(ix),TRUE),nbins=length(ix))}
ci<-list();j<-0L
for(pair in unique(d$comparison_pair_id))for(target_name in unique(d$target)){
 dd<-d[comparison_pair_id==pair & target==target_name];tasks<-sort(unique(dd$task_index));boot<-list();point<-numeric()
 for(g in c("Lower","Higher")){
  xx<-dd[context_group==g];num<-den<-matrix(0,length(tasks),nrow(pm));idx<-cbind(match(xx$task_index,tasks),match(xx$participant_key,pm$participant_key))
  num[idx]<-xx$events;den[idx]<-xx$n;boot[[g]]<-colMeans((num%*%w)/(den%*%w),na.rm=TRUE)
  point[g]<-mean(rowSums(num)/rowSums(den),na.rm=TRUE)
 }
 diff<-boot$Higher-boot$Lower;j<-j+1L
 ci[[j]]<-data.table(comparison_pair_id=pair,target=target_name,difference=point['Higher']-point['Lower'],lo=quantile(diff,.025),hi=quantile(diff,.975))
}
fwrite(rbindlist(ci),file.path(out,"context_profile_contrast_ci.csv"))
cat("Frozen context composition, profile intervals and one-day calibration audit complete\n")
