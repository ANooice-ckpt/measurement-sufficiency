# Canonical Fig.4: frozen conditional-reliability results only. No refitting.
# Pair already-frozen losses within each metric, tolerance and validation split.
# Absolute Brier reduction remains defined when the baseline loss is zero.
fig4_increment_display <- function(scores) {
  d<-data.table::as.data.table(scores)[startsWith(target,"exceed_")]
  keys<-c("task_index","metric","metric_class","dimension","comparison_pair_id",
    "support_id","target","repeat_id")
  stopifnot(!anyDuplicated(d[,c(keys,"state"),with=FALSE]))
  w<-data.table::dcast(d,task_index+metric+metric_class+dimension+comparison_pair_id+
    support_id+target+repeat_id~state,value.var="mse")
  out<-data.table::melt(w,id.vars=c(keys,"joint"),
    measure.vars=c("measurement","measurement_capacity"),
    variable.name="baseline",value.name="baseline_brier")
  out[,`:=`(epsilon=as.numeric(sub("exceed_","",target)),
    brier_reduction=baseline_brier-joint)]
  out[]
}

options(encoding="UTF-8")
if(.Platform$OS.type=="windows")invisible(suppressWarnings(Sys.setlocale("LC_CTYPE","English_United States.utf8")))
suppressPackageStartupMessages({library(data.table);library(ggplot2);library(cowplot)})
source("scripts/utils/figure_style.R");source("scripts/utils/plot_contracts.R")
args<-commandArgs(TRUE);path<-if(length(args))args[1] else "results/rq2/rq2_conditional_reliability.rds"
ms_plot_require_files(path,"Conditional reliability")
z<-readRDS(path);stopifnot(isTRUE(z$complete),identical(z$provenance$rq2_analysis_version,"conditional_reliability_v1"))
ORDER<-c("chest_vs_eye","wrist_vs_eye","LIGHT_vs_MEDI","20s_vs_10s","30s_vs_10s","40s_vs_10s","60s_vs_10s","120s_vs_10s")
LABELS<-c("Chest → eye","Wrist → eye","LIGHT → MEDI","20 → 10 s","30 → 10 s","40 → 10 s","60 → 10 s","120 → 10 s")
pair_factor<-function(x)factor(x,levels=rev(ORDER),labels=rev(LABELS))
theme_risk<-function()theme_ms_axes(base_size=7,legend_position="bottom")+
  theme(panel.grid.minor=element_blank(),panel.grid.major=element_line(colour="#E7EBED",linewidth=.2),
    strip.text=element_text(size=6.5,face="bold"),axis.text=element_text(size=6),
    legend.title=element_blank(),legend.text=element_text(size=6),
    legend.margin=margin(0,0,0,0),legend.box.margin=margin(0,0,0,0),
    legend.box.spacing=unit(1,"mm"),legend.key.height=unit(3,"mm"),
    legend.key.width=unit(4,"mm"),legend.spacing.x=unit(1,"mm"),
    plot.margin=margin(4,5,2,4))
header<-function(p,title,subtitle,h=.14,left=0)ggdraw()+draw_plot(p,left,0,1-left,1-h)+
  draw_label(title,x=if(left>0).00735 else .015,y=.995,hjust=0,vjust=1,size=7.4,fontface="bold",fontfamily=MS_FONT)+
  draw_label(subtitle,x=if(left>0).00735 else .015,y=1-h*.48,hjust=0,vjust=1,size=5.2,colour="#656D72",fontfamily=MS_FONT)

# Training-only context-risk cutpoints define the held-out groups.
mp<-as.data.table(z$metric_profiles)[target=="mean_risk"]
raw<-mp[,.(raw_A=weighted.mean(observed,n)),by=.(task_index,comparison_pair_id)]
mp<-merge(mp,raw,by=c("task_index","comparison_pair_id"))
display<-mp[,.(conditional_A=mean(observed),raw_A=mean(raw_A),n_metrics=.N,n_days=sum(n)),by=.(comparison_pair_id,context_group)]
display[,`:=`(ratio=conditional_A/raw_A,pair=pair_factor(comparison_pair_id))]
metric<-mp[context_group%in%c("Lower","Higher") & raw_A>1e-8]
metric[,`:=`(ratio=observed/raw_A,pair=pair_factor(comparison_pair_id))]
group_colors<-c(Lower="#3A7C88",Middle="#A5ADB2",Higher="#B16C42")
pa<-ggplot(display,aes(ratio,pair,colour=context_group,group=pair))+
  geom_vline(xintercept=1,colour="#7D858A",linetype=2,linewidth=.35)+
  geom_line(colour="#BBC3C7",linewidth=.5)+geom_point(size=2.25)+
  scale_colour_manual(values=group_colors,breaks=c("Lower","Middle","Higher"),labels=c("Lower risk","Middle","Higher risk"))+
  labs(x="Observed distortion / unstratified distortion",y=NULL)+theme_risk()
pa_body<-pa

value_kinds<-c("context_skill","context_increment","capacity_control")
kind_labels<-c("Context vs mean","Joint vs measurement (10 df)","Joint vs measurement (20 df)")
ci<-as.data.table(z$information_value)[contrast%in%value_kinds]
ci[,`:=`(pair=pair_factor(comparison_pair_id),kind=factor(contrast,levels=value_kinds,labels=kind_labels))]
reps<-melt(as.data.table(z$repeat_scores),id.vars=c("comparison_pair_id","repeat_id"),
  measure.vars=value_kinds,variable.name="contrast",value.name="estimate")
reps[,`:=`(pair=pair_factor(comparison_pair_id),kind=factor(contrast,levels=value_kinds,labels=kind_labels))]
pd<-position_dodge(width=.65)
pb<-ggplot(ci,aes(estimate,pair,colour=kind,group=kind))+
  geom_vline(xintercept=0,colour="#7D858A",linetype=2,linewidth=.35)+
  geom_errorbar(aes(xmin=lo,xmax=hi),orientation="y",width=.12,position=pd,linewidth=.5)+geom_point(size=2,position=pd)+
  geom_point(data=reps[repeat_id!=1],aes(shape=factor(repeat_id)),size=1.3,position=pd,alpha=.6,show.legend=FALSE)+
  scale_colour_manual(values=c("#2F5D7E","#B16C42","#52958B"))+
  labs(x="Held-out Brier score improvement (%)",y=NULL)+theme_risk()+guides(colour=guide_legend(ncol=1))
top_aligned<-align_plots(pa_body,pb,align="h",axis="tb")
pa<-header(top_aligned[[1]],"a  Context separates reliability regimes","New participants; training-only risk groups")
pb<-header(top_aligned[[2]],"b  Context value and decoder-capacity control","Pooled loss ratios; fixed-prediction bootstrap bars + repeat splits")

prof<-as.data.table(z$context_profiles)[startsWith(target,"exceed_") & context_group%in%c("Lower","Higher")]
prof[,`:=`(epsilon=as.numeric(sub("exceed_","",target)),pair=factor(comparison_pair_id,levels=ORDER,labels=LABELS))]
curves<-melt(prof,id.vars=c("pair","context_group","epsilon"),measure.vars=c("observed","predicted"),variable.name="curve",value.name="probability")
pc<-ggplot(curves,aes(epsilon,probability,colour=context_group,linetype=curve))+
  geom_line(linewidth=.5)+geom_point(data=curves[curve=="observed"],size=.85)+facet_wrap(~pair,ncol=4)+
  scale_colour_manual(values=group_colors,breaks=c("Lower","Higher"),labels=c("Lower context risk","Higher context risk"))+
  scale_linetype_manual(values=c(observed="solid",predicted="dashed"),labels=c(observed="Observed",predicted="Predicted"))+
  scale_x_log10(breaks=c(.05,.2,1),labels=c("0.05","0.2","1"))+
  scale_y_continuous(limits=c(0,1),breaks=c(0,.5,1),labels=c("0","50%","100%"))+
  labs(x="Tolerance ε (frozen RQ1 standardized units)",y="Probability that daily distortion exceeds ε")+theme_risk()
pc<-header(pc,"c  A given tolerance implies different reliability across contexts",
  "Identical groups across tolerances; solid = actual held-out frequency; dashed = predicted",.12)
foot<-ggdraw()+draw_label(
  "52 daily targets; eight contrasts; unavailable targets excluded. Summaries weight metrics equally.\nContext: 18 daily + 32 daypart fields. Measurement: candidate target + 16 observed-configuration signatures.\nContext groups do not redefine the cohort or certify RQ3 sufficiency. No universal tolerance is imposed.",
  x=.012,hjust=0,size=5.6,colour="#626A70",fontfamily=MS_FONT)
# Preserve the established supplementary output; no new supplementary figure.
previous_figure<-plot_grid(plot_grid(pa,pb,nrow=1,rel_widths=c(.49,.51)),pc,foot,ncol=1,rel_heights=c(.47,.45,.08))
# Keep the existing risk-scatter audit tables and their output contracts.
tail<-as.data.table(z$metric_profiles)[startsWith(target,"exceed_") & context_group %in% c("Lower","Higher")]
cloud<-dcast(tail,task_index+metric+dimension+comparison_pair_id+target~context_group,value.var="observed")
cloud[,`:=`(epsilon=as.numeric(sub("exceed_","",target)),
  axis=factor(dimension,levels=c("placement","optical","temporal"),labels=c("Placement","Optical","Temporal")))]
cloud[,slice:=factor(epsilon,levels=sort(unique(epsilon)),labels=paste0("ε = ",sort(unique(epsilon))))]
spread<-cloud[,.(x=median(Lower),y=median(Higher),
  xlo=quantile(Lower,.25),xhi=quantile(Lower,.75),
  ylo=quantile(Higher,.25),yhi=quantile(Higher,.75)),by=.(slice,axis)]

# The main atlas reveals the incremental-value structure before any pooling.
# Every frozen split remains in `increment`; choose the declared primary split,
# and mark sign reversals across splits without introducing a hypothesis test.
increment<-fig4_increment_display(z$task_scores)
repeat_range<-increment[baseline=="measurement",.(split_min=min(brier_reduction),
  split_max=max(brier_reduction)),by=.(task_index,target)]
atlas<-merge(increment[baseline=="measurement" & repeat_id==1L],repeat_range,by=c("task_index","target"))
metric_order<-unique(atlas[,.(metric,metric_class)])
metric_order[,class_order:=match(metric_class,MS_METRIC_CLASSES)]
setorder(metric_order,class_order,metric)
eps<-sort(unique(increment$epsilon))
atlas[,`:=`(metric=factor(metric,levels=rev(metric_order$metric)),
  metric_class=factor(metric_class,levels=MS_METRIC_CLASSES),
  pair=factor(comparison_pair_id,levels=ORDER,
    labels=c("Chest","Wrist","LIGHT","20 s","30 s","40 s","60 s","120 s")),
  epsilon=factor(epsilon,levels=eps),gain=100*brier_reduction,
  split_reversal=split_min<0 & split_max>0)]
fill_limit<-max(abs(atlas$gain))
if(fill_limit==0)fill_limit<-1
pc<-ggplot(atlas,aes(epsilon,metric,fill=gain))+
  geom_tile(width=.94,height=.94)+
  geom_point(data=atlas[split_reversal==TRUE],shape=4,size=.65,stroke=.2,colour="#30363A")+
  facet_grid(metric_class~pair,scales="free_y",space="free_y",switch="y")+
  scale_fill_gradient2(low="#B16C42",mid="#FAFAF8",high="#2F5D7E",midpoint=0,
    limits=c(-fill_limit,fill_limit),trans=scales::pseudo_log_trans(sigma=.1),
    name="Brier reduction × 100")+
  labs(x="Tolerance ε within each contrast (frozen RQ1 standardized units)",y=NULL)+theme_risk()+
  theme(panel.grid=element_blank(),panel.background=element_rect(fill="#E0E3E5",colour=NA),
    panel.spacing.x=unit(.6,"mm"),panel.spacing.y=unit(.6,"mm"),
    strip.placement="outside",strip.text.y.left=element_text(angle=0,size=5.2),
    axis.text.y=element_text(size=4.8),axis.text.x=element_text(size=4.8,angle=90,hjust=1,vjust=.5),
    axis.ticks=element_blank(),legend.title=element_text(size=6))+
  guides(fill=guide_colourbar(barwidth=unit(40,"mm"),barheight=unit(2,"mm"),title.position="top"))
pc<-header(pc,"c  Where context adds information beyond candidate measurements",
  "Primary split: measurement (10 df) loss − joint loss; blue = improvement · × = sign changes across splits · grey = unavailable",.07)
foot<-ggdraw()+draw_label(
  "52 daily targets; eight contrasts; participant-grouped out-of-sample evaluation. a–b: equal metric weights.\nb: relative improvement over each baseline; c: absolute loss reduction, with no metric or tolerance averaging.\nSplit markers describe validation variability, not significance. Daily risk does not redefine RQ3 sufficiency.",
  x=.012,hjust=0,size=5.4,colour="#626A70",fontfamily=MS_FONT)
figure<-plot_grid(plot_grid(pa,pb,nrow=1,rel_widths=c(.49,.51)),pc,foot,
  ncol=1,rel_heights=c(.29,.66,.05))
ms_plot_save(figure,"results/rq2/Fig4_RQ2.png",8.6,8.4)
ms_plot_save(previous_figure,"results/rq2/FigS_RQ2_reliability_profiles.png",7.4,7.7)
if (!ms_plot_prep_only()) {
  fwrite(cloud,"results/rq2/fig4_tolerance_scatter_display.csv")
  fwrite(spread,"results/rq2/fig4_tolerance_scatter_iqr.csv")
}

# Strong nuisance controls and metric-level detail are retained as supplements.
sens<-as.data.table(z$information_value)
sens[,`:=`(pair=pair_factor(comparison_pair_id),contrast=factor(contrast,
  levels=c("context_skill","context_increment","site_control","capacity_control"),
  labels=c("Context vs configuration mean","Context beyond measurement","Context vs site mean","Joint vs higher-capacity measurement")))]
ps<-ggplot(sens,aes(estimate,pair))+geom_vline(xintercept=0,linetype=2,colour="#888888")+
  geom_errorbar(aes(xmin=lo,xmax=hi),orientation="y",width=.12,linewidth=.4)+geom_point(size=1.8,colour=MS_PRIMARY)+
  facet_wrap(~contrast,ncol=2,scales="free_x")+labs(x="Brier score improvement (%)",y=NULL)+theme_risk()
ms_plot_save(ps,"results/rq2/FigS_RQ2_reliability_controls.png",7.4,5.6)
pm<-ggplot(metric,aes(ratio,pair,colour=metric_class))+geom_vline(xintercept=1,linetype=2,colour="#888888")+
  geom_point(size=.8,alpha=.65,position=position_jitter(height=.16,width=0,seed=14))+
  facet_wrap(~context_group,ncol=2)+scale_colour_manual(values=MS_METRIC_COLORS)+
  scale_x_continuous(trans=scales::pseudo_log_trans(sigma=.1),breaks=c(0,.5,1,2,5))+
  labs(x="Metric-specific conditional / unstratified distortion",y=NULL)+theme_risk()
ms_plot_save(pm,"results/rq2/FigS_RQ2_reliability_metrics.png",7.4,4.8)
if (!ms_plot_prep_only()) {
  fwrite(display,"results/rq2/fig4_conditional_risk_display.csv");fwrite(prof,"results/rq2/fig4_tolerance_reliability_display.csv")
}
ms_plot_write_manifest("results/rq2/figure_artifact_manifest.csv",data.frame(figure="Fig4_RQ2",input_artifact=path,
  core_artifact_version=z$provenance$core_artifact_version,rq1_analysis_version=z$provenance$rq1_analysis_version,
  rq2_analysis_version=z$provenance$rq2_analysis_version,rq3_analysis_version=NA_character_,source_md5=unname(tools::md5sum(path))))
message(if (ms_plot_prep_only()) "Fig.4 conditional reliability and controls prepared in memory" else
  "Fig.4 conditional reliability and controls written")
