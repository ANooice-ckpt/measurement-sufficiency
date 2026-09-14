# Canonical Fig.4: frozen conditional-reliability results only. No refitting.
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
    legend.title=element_blank(),legend.text=element_text(size=6),plot.margin=margin(4,5,4,4))
header<-function(p,title,subtitle,h=.14,left=0)ggdraw()+draw_plot(p,left,0,1-left,1-h)+
  draw_label(title,x=if(left>0).00735 else .015,y=.995,hjust=0,vjust=1,size=8.4,fontface="bold",fontfamily=MS_FONT)+
  draw_label(subtitle,x=if(left>0).00735 else .015,y=1-h*.48,hjust=0,vjust=1,size=6,colour="#656D72",fontfamily=MS_FONT)

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
pa<-header(pa,"a  Context separates reliability regimes","New participants; training-only risk groups")

ci<-as.data.table(z$information_value)[contrast%in%c("context_skill","context_increment")]
kind_labels<-c("Context vs configuration mean","Context beyond measurement")
ci[,`:=`(pair=pair_factor(comparison_pair_id),kind=factor(contrast,levels=c("context_skill","context_increment"),labels=kind_labels))]
reps<-melt(as.data.table(z$repeat_scores),id.vars=c("comparison_pair_id","repeat_id"),
  measure.vars=c("context_skill","context_increment"),variable.name="contrast",value.name="estimate")
reps[,`:=`(pair=pair_factor(comparison_pair_id),kind=factor(contrast,levels=c("context_skill","context_increment"),labels=kind_labels))]
pd<-position_dodge(width=.55)
pb<-ggplot(ci,aes(estimate,pair,colour=kind,group=kind))+
  geom_vline(xintercept=0,colour="#7D858A",linetype=2,linewidth=.35)+
  geom_errorbar(aes(xmin=lo,xmax=hi),orientation="y",width=.12,position=pd,linewidth=.5)+geom_point(size=2,position=pd)+
  geom_point(data=reps[repeat_id!=1],aes(shape=factor(repeat_id)),size=1.3,position=pd,alpha=.6,show.legend=FALSE)+
  scale_colour_manual(values=c("#2F5D7E","#B16C42"))+
  labs(x="Held-out Brier score improvement (%)",y=NULL)+theme_risk()+guides(colour=guide_legend(ncol=1))
top_aligned<-align_plots(pa_body,pb,align="h",axis="tb")
pa<-header(top_aligned[[1]],"a  Context separates reliability regimes","New participants; training-only risk groups")
pb<-header(top_aligned[[2]],"b  Information value depends on what is known","All tolerance slices; bootstrap bars and repeat splits")

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
# Preserve the earlier pooled view; the main figure exposes all analytical units.
previous_figure<-plot_grid(plot_grid(pa,pb,nrow=1,rel_widths=c(.49,.51)),pc,foot,ncol=1,rel_heights=c(.47,.45,.08))
# Each point is one metric/contrast; facets retain every prespecified tolerance.
# The identity line makes successful and reversed risk ordering immediately visible.
tail<-as.data.table(z$metric_profiles)[startsWith(target,"exceed_") & context_group %in% c("Lower","Higher")]
cloud<-dcast(tail,task_index+metric+dimension+comparison_pair_id+target~context_group,value.var="observed")
cloud[,`:=`(epsilon=as.numeric(sub("exceed_","",target)),
  axis=factor(dimension,levels=c("placement","optical","temporal"),labels=c("Placement","Optical","Temporal")))]
cloud[,slice:=factor(epsilon,levels=sort(unique(epsilon)),labels=paste0("ε = ",sort(unique(epsilon))))]
spread<-cloud[,.(x=median(Lower),y=median(Higher),
  xlo=quantile(Lower,.25),xhi=quantile(Lower,.75),
  ylo=quantile(Higher,.25),yhi=quantile(Higher,.75)),by=.(slice,axis)]
pc<-ggplot(cloud,aes(Lower,Higher,colour=axis))+
  geom_abline(slope=1,intercept=0,colour="#899499",linetype=2,linewidth=.35)+
  geom_point(size=.5,alpha=.22)+
  geom_segment(data=spread,aes(x=xlo,xend=xhi,y=y,yend=y),inherit.aes=FALSE,colour="white",linewidth=1.6)+
  geom_segment(data=spread,aes(x=x,xend=x,y=ylo,yend=yhi),inherit.aes=FALSE,colour="white",linewidth=1.6)+
  geom_segment(data=spread,aes(x=xlo,xend=xhi,y=y,yend=y,colour=axis),inherit.aes=FALSE,linewidth=.65)+
  geom_segment(data=spread,aes(x=x,xend=x,y=ylo,yend=yhi,colour=axis),inherit.aes=FALSE,linewidth=.65)+
  geom_point(data=spread,aes(x,y,fill=axis),shape=23,size=2.2,colour="white",stroke=.4)+
  facet_wrap(~slice,ncol=3)+coord_cartesian(xlim=c(0,1),ylim=c(0,1))+
  scale_colour_manual(values=c(Placement="#2F5D7E",Optical="#B16C42",Temporal="#52958B"))+
  scale_fill_manual(values=c(Placement="#2F5D7E",Optical="#B16C42",Temporal="#52958B"),guide="none")+
  scale_x_continuous(breaks=c(0,.5,1),labels=c("0","50","100"))+
  scale_y_continuous(breaks=c(0,.5,1),labels=c("0","50","100"))+
  labs(x="Exceedance probability in lower-risk context (%)",
    y="Exceedance probability in higher-risk context (%)")+theme_risk()+
  guides(colour=guide_legend(override.aes=list(alpha=1,size=2)))
pc<-header(pc,"c  Context changes tolerance-exceedance risk",
  "Above diagonal: higher risk as predicted · dots: metric–contrast pairs · diamonds / bars: median / IQR",.10,left=.047)
foot<-ggdraw()+draw_label(
  "52 daily targets; eight contrasts; participant-grouped out-of-sample evaluation. a–b: equal metric weights.\nRisk groups use training-only cutpoints. c: IQRs describe heterogeneity, not confidence intervals.\nDaily exceedance risk informs context of use; RQ3 retains its separate observed-stability criterion.",
  x=.012,hjust=0,size=5.4,colour="#626A70",fontfamily=MS_FONT)
figure<-plot_grid(plot_grid(pa,pb,nrow=1,rel_widths=c(.49,.51)),pc,foot,
  ncol=1,rel_heights=c(.40,.54,.06))
ms_fig3_atlas_refine_main<-function(...)NULL;ms_fig3_refine_main<-function(...)NULL
ms_polish_main_figure<-function(plot,path,caller_env,width,height)list(plot=plot,width=width,height=height)
ms_plot_save(figure,"results/rq2/Fig4_RQ2.png",7.4,6.56)
ms_plot_save(previous_figure,"results/rq2/FigS_RQ2_reliability_profiles.png",7.4,7.7)
fwrite(cloud,"results/rq2/fig4_tolerance_scatter_display.csv")
fwrite(spread,"results/rq2/fig4_tolerance_scatter_iqr.csv")

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
fwrite(display,"results/rq2/fig4_conditional_risk_display.csv");fwrite(prof,"results/rq2/fig4_tolerance_reliability_display.csv")
ms_plot_write_manifest("results/rq2/figure_artifact_manifest.csv",data.frame(figure="Fig4_RQ2",input_artifact=path,
  core_artifact_version=z$provenance$core_artifact_version,rq1_analysis_version=z$provenance$rq1_analysis_version,
  rq2_analysis_version=z$provenance$rq2_analysis_version,rq3_analysis_version=NA_character_,source_md5=unname(tools::md5sum(path))))
message("Fig.4 conditional reliability and controls written")
