# Visual composition only. Scientific rows, summaries and decisions are frozen.
# The dispatcher receives legacy identities from figure_registry.R exactly once.

ms_polish_env_get <- function(env, name, default = NULL) {
  if (is.environment(env) && exists(name, envir = env, inherits = FALSE))
    get(name, envir = env, inherits = FALSE) else default
}

ms_polish_fig1 <- function(plot, env, width, height) {
  names <- c("p1a_core", "p1b_top", "p1b_bottom", "p1c_temporal", "p1c_duration", "p1b_shape_legend")
  z <- lapply(names, function(nm) ms_polish_env_get(env, nm)); names(z) <- names
  if (any(vapply(z, is.null, logical(1)))) return(list(plot=plot,width=width,height=height))
  a <- ms_style_plot(z$p1a_core)
  l1 <- ms_style_plot(z$p1b_top, subpanel=TRUE) + ggplot2::labs(title="Chest / wrist to eye", y="Absolute distortion, A")
  l2 <- ms_style_plot(z$p1b_bottom, subpanel=TRUE) + ggplot2::labs(title="LIGHT to MEDI", y="Absolute distortion, A")
  r1 <- ms_style_plot(z$p1c_temporal, subpanel=TRUE)
  r2 <- ms_style_plot(z$p1c_duration, subpanel=TRUE)
  # Align the actual plot fields before nesting them inside labelled panels.
  left <- cowplot::align_plots(a,l1,l2,align="v",axis="l")
  right <- cowplot::align_plots(r1,r2,align="v",axis="lr")
  row1 <- cowplot::align_plots(left[[2]],right[[1]],align="h",axis="tb")
  row2 <- cowplot::align_plots(left[[3]],right[[2]],align="h",axis="tb")
  b <- cowplot::plot_grid(row1[[1]],row2[[1]],z$p1b_shape_legend,ncol=1,rel_heights=c(1,1,.13))
  c <- cowplot::plot_grid(row1[[2]],row2[[2]],NULL,ncol=1,rel_heights=c(1,1,.13))
  bottom <- cowplot::plot_grid(ms_panel_frame(b,"b  Magnitude and direction",style=FALSE),
    ms_panel_frame(c,"c  Adjacent configuration changes",style=FALSE),nrow=1)
  body <- cowplot::plot_grid(ms_panel_frame(left[[1]],"a  Absolute and relational preservation"),
    bottom,ncol=1,rel_heights=c(.72,1.28))
  final <- cowplot::plot_grid(ms_metric_legend(),body,ncol=1,rel_heights=c(.045,1))
  list(plot=final,width=MS_FIG_WIDTH_IN,height=5.8)
}

ms_polish_fig4 <- function(plot, env, width, height) {
  z <- lapply(c("p4a","p4b","p4c"),function(nm) ms_polish_env_get(env,nm))
  if (any(vapply(z,is.null,logical(1)))) return(list(plot=plot,width=width,height=height))
  a <- ms_style_plot(z[[1]],dense=TRUE) + ggplot2::labs(title=NULL)
  b <- ms_style_plot(z[[2]]) + ggplot2::labs(title=NULL,
    x="Sampling interval (s) / monitoring days",y=expression(R[obs]))
  c <- ms_style_plot(z[[3]]) + ggplot2::labs(title=NULL)
  ac <- cowplot::align_plots(a,b,align="v",axis="l")
  bc <- cowplot::align_plots(ac[[2]],c,align="h",axis="tb")
  bottom <- cowplot::plot_grid(ms_panel_frame(bc[[1]],"b  Residual instability"),
    ms_panel_frame(bc[[2]],"c  Placement / optical substitutability"),nrow=1,rel_widths=c(1.06,.94))
  body <- cowplot::plot_grid(ms_panel_frame(ac[[1]],"a  Minimum requirements across tolerances"),
    bottom,ncol=1,rel_heights=c(1.18,.82))
  final <- cowplot::plot_grid(ms_metric_legend(),body,ncol=1,rel_heights=c(.045,1))
  list(plot=final,width=MS_FIG_WIDTH_IN,height=6.5)
}

ms_polish_fig5 <- function(plot, env, width, height) {
  # Current Fig.6 already has the common frame in its canonical helper.
  list(plot=plot,width=width,height=height)
}

ms_polish_main_figure <- function(plot, path, env, width, height) {
  name <- basename(path)
  if (identical(name,"Fig1_RQ1.png")) return(ms_polish_fig1(plot,env,width,height))
  if (identical(name,"Fig4_RQ3.png")) return(ms_polish_fig4(plot,env,width,height))
  if (identical(name,"Fig5_RQ3.png")) return(ms_polish_fig5(plot,env,width,height))
  # Current Figs.2/3/4/6 are fully composed in their canonical entrypoints.
  list(plot=plot,width=width,height=height)
}
