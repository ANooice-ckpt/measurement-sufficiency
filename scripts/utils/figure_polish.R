# Final publication-layout pass for main-text figures.
#
# This file is intentionally visual only. It receives already-built plot objects
# from the main figure scripts immediately before export and may change only
# composition, spacing, typography and export dimensions. It must not filter
# rows, recompute summaries, refit models or redefine a scientific estimand.

MS_PANEL_TITLE_SIZE <- 6.25
MS_PANEL_SUBTITLE_SIZE <- 4.05
MS_PANEL_TITLE_COLOUR <- "#151515"
MS_PANEL_SUBTITLE_COLOUR <- "#666A6D"

ms_polish_env_get <- function(env, name, default = NULL) {
  if (is.environment(env) && exists(name, envir = env, inherits = FALSE)) {
    get(name, envir = env, inherits = FALSE)
  } else {
    default
  }
}

ms_polish_text <- function(x) {
  if (is.null(x) || !length(x) || is.na(x[[1]])) return("")
  x <- as.character(x[[1]])
  x <- gsub("[\r\n]+", " ", x)
  trimws(gsub("[[:space:]]+", " ", x))
}

ms_polish_ggplot <- function(plot,
                             title_size = MS_PANEL_TITLE_SIZE,
                             subtitle_size = MS_PANEL_SUBTITLE_SIZE,
                             margin = ggplot2::margin(2, 3, 2, 3),
                             legend_compact = FALSE) {
  if (is.null(plot) || !inherits(plot, "ggplot")) return(plot)
  out <- plot +
    ggplot2::theme(
      plot.title = ggplot2::element_text(
        size = title_size, face = "bold", colour = MS_PANEL_TITLE_COLOUR,
        hjust = 0, lineheight = .96, margin = ggplot2::margin(b = 2)
      ),
      plot.subtitle = ggplot2::element_text(
        size = subtitle_size, colour = MS_PANEL_SUBTITLE_COLOUR,
        hjust = 0, lineheight = .96, margin = ggplot2::margin(t = -1, b = 2)
      ),
      plot.margin = margin
    )
  if (isTRUE(legend_compact)) {
    out <- out + ggplot2::theme(
      legend.margin = ggplot2::margin(0, 0, 0, 0),
      legend.box.margin = ggplot2::margin(0, 0, 0, 0),
      legend.spacing.x = grid::unit(1.2, "mm"),
      legend.spacing.y = grid::unit(.5, "mm"),
      legend.title = ggplot2::element_text(size = 4.05, lineheight = .95),
      legend.text = ggplot2::element_text(size = 3.90)
    )
  }
  out
}

ms_polish_take_labels <- function(plot) {
  if (is.null(plot) || !inherits(plot, "ggplot")) {
    return(list(plot = plot, title = "", subtitle = ""))
  }
  title <- ms_polish_text(plot$labels$title)
  subtitle <- ms_polish_text(plot$labels$subtitle)
  list(
    plot = plot + ggplot2::labs(title = NULL, subtitle = NULL),
    title = title,
    subtitle = subtitle
  )
}

ms_polish_panel_frame <- function(plot, title, subtitle = "",
                                  title_size = MS_PANEL_TITLE_SIZE,
                                  subtitle_size = MS_PANEL_SUBTITLE_SIZE,
                                  body_height = NULL,
                                  title_x = .004) {
  title <- ms_polish_text(title)
  subtitle <- ms_polish_text(subtitle)
  has_subtitle <- nzchar(subtitle)
  if (is.null(body_height)) body_height <- if (has_subtitle) .875 else .915

  out <- cowplot::ggdraw() +
    cowplot::draw_plot(plot, x = 0, y = 0, width = 1, height = body_height) +
    cowplot::draw_label(
      title, x = title_x, y = .995, hjust = 0, vjust = 1,
      fontfamily = MS_FONT, fontface = "bold", size = title_size,
      colour = MS_PANEL_TITLE_COLOUR
    )
  if (has_subtitle) {
    out <- out + cowplot::draw_label(
      subtitle, x = title_x, y = .955, hjust = 0, vjust = 1,
      fontfamily = MS_FONT, size = subtitle_size,
      colour = MS_PANEL_SUBTITLE_COLOUR
    )
  }
  out
}

ms_polish_fig1 <- function(plot, env, width, height) {
  p1a_core <- ms_polish_env_get(env, "p1a_core")
  p1b_core <- ms_polish_env_get(env, "p1b_core")
  p1b_shape_legend <- ms_polish_env_get(env, "p1b_shape_legend")
  right_core <- ms_polish_env_get(env, "right_core")
  metric_legend <- ms_polish_env_get(env, "metric_legend")
  assoc_text <- ms_polish_env_get(env, "assoc_text", "")

  if (any(vapply(list(p1a_core, p1b_core, right_core, metric_legend), is.null, logical(1)))) {
    return(list(plot = plot, width = width, height = height))
  }

  p1a_core <- ms_polish_ggplot(
    p1a_core, margin = ggplot2::margin(1, 3, 2, 3)
  )
  p1a <- ms_polish_panel_frame(
    p1a_core,
    "a  Absolute and relational preservation",
    assoc_text,
    title_size = 6.35, subtitle_size = 3.85, body_height = .855
  )

  p1b_body <- if (!is.null(p1b_shape_legend)) {
    cowplot::plot_grid(
      p1b_core, p1b_shape_legend, ncol = 1,
      rel_heights = c(.91, .09), align = "v", axis = "lr", greedy = TRUE
    )
  } else {
    p1b_core
  }
  p1b_axis <- cowplot::ggdraw() +
    cowplot::draw_plot(p1b_body, x = .060, y = 0, width = .940, height = .965) +
    cowplot::draw_label(
      "Absolute distortion, A", x = .012, y = .49, angle = 90,
      hjust = .5, vjust = .5, size = 5.65,
      colour = MS_PANEL_TITLE_COLOUR, fontfamily = MS_FONT
    )
  p1b <- ms_polish_panel_frame(
    p1b_axis, "b  Magnitude and directional coherence",
    title_size = 6.35, body_height = .905
  )

  p1c <- ms_polish_panel_frame(
    right_core, "c  Where ordered-axis distortion accrues",
    title_size = 6.35, body_height = .905
  )

  bottom <- cowplot::plot_grid(
    p1b, p1c, ncol = 2, rel_widths = c(1, 1),
    align = "hv", axis = "tblr", greedy = TRUE
  )
  body <- cowplot::plot_grid(
    p1a, bottom, ncol = 1, rel_heights = c(.92, 1.08),
    align = "v", axis = "lr", greedy = TRUE
  )
  final <- cowplot::plot_grid(
    metric_legend, body, ncol = 1, rel_heights = c(.050, 1),
    align = "v", axis = "l", greedy = TRUE
  )
  list(plot = final, width = 7.40, height = 5.90)
}

ms_polish_fig2 <- function(plot, env, width, height) {
  # Fig. 2 is the current layout reference. Preserve its composition and only
  # normalize the export box so it remains the visual anchor for the other RQs.
  list(plot = plot, width = 8.20, height = 4.64)
}

ms_polish_fig3 <- function(plot, env, width, height) {
  p3a <- ms_polish_env_get(env, "p3a")
  p3b_po <- ms_polish_env_get(env, "p3b_po")
  p3b_ot <- ms_polish_env_get(env, "p3b_ot")
  p3b_pt <- ms_polish_env_get(env, "p3b_pt")
  p3c <- ms_polish_env_get(env, "p3c")
  metric_legend <- ms_polish_env_get(
    env, "metric_legend_main", ms_polish_env_get(env, "metric_legend")
  )
  if (any(vapply(list(p3a, p3b_po, p3b_ot, p3b_pt, p3c, metric_legend), is.null, logical(1)))) {
    return(list(plot = plot, width = width, height = height))
  }

  a_info <- ms_polish_take_labels(p3a)
  p3a_body <- ms_polish_ggplot(
    a_info$plot, margin = ggplot2::margin(1, 2.5, 1, 2.5)
  )
  p3a_panel <- ms_polish_panel_frame(
    p3a_body, a_info$title, a_info$subtitle,
    body_height = .870
  )

  p3b_po <- ms_polish_ggplot(p3b_po, title_size = 5.10,
                             margin = ggplot2::margin(0, 2.5, 0, 2.5))
  p3b_ot <- ms_polish_ggplot(p3b_ot, title_size = 5.10,
                             margin = ggplot2::margin(0, 2.5, 0, 2.5))
  p3b_body <- cowplot::plot_grid(
    p3b_po, p3b_ot, p3b_pt, ncol = 1,
    rel_heights = c(.88, 1.18, 1.42),
    align = "v", axis = "lr", greedy = TRUE
  )
  p3b_panel <- ms_polish_panel_frame(
    p3b_body,
    "b  Ordered-transition backbone with class overlays",
    "overall = median + IQR; coloured marks = class medians + IQR; faint points = metric-level Q",
    body_height = .865
  )

  c_info <- ms_polish_take_labels(p3c)
  p3c_body <- ms_polish_ggplot(
    c_info$plot, title_size = MS_PANEL_TITLE_SIZE,
    margin = ggplot2::margin(1, 2.5, 1, 2.5)
  )
  p3c_panel <- ms_polish_panel_frame(
    p3c_body, c_info$title, c_info$subtitle,
    body_height = .865
  )

  bottom <- cowplot::plot_grid(
    p3b_panel, p3c_panel, ncol = 2, rel_widths = c(.70, .30),
    align = "hv", axis = "tblr", greedy = TRUE
  )
  body <- cowplot::plot_grid(
    p3a_panel, bottom, ncol = 1, rel_heights = c(.98, 1.02),
    align = "v", axis = "lr", greedy = TRUE
  )
  final <- cowplot::plot_grid(
    metric_legend, body, ncol = 1, rel_heights = c(.045, 1),
    align = "v", axis = "l", greedy = TRUE
  )
  list(plot = final, width = 7.20, height = 6.80)
}

ms_polish_fig4 <- function(plot, env, width, height) {
  p4a <- ms_polish_env_get(env, "p4a")
  p4b <- ms_polish_env_get(env, "p4b")
  p4c <- ms_polish_env_get(env, "p4c")
  metric_legend <- ms_polish_env_get(env, "metric_legend")
  if (any(vapply(list(p4a, p4b, p4c, metric_legend), is.null, logical(1)))) {
    return(list(plot = plot, width = width, height = height))
  }

  p4a <- ms_polish_ggplot(p4a, title_size = 6.25, subtitle_size = 4.10,
                          margin = ggplot2::margin(2, 3, 1, 3), legend_compact = TRUE)
  p4b <- ms_polish_ggplot(p4b, title_size = 6.25, subtitle_size = 4.10,
                          margin = ggplot2::margin(1, 3, 2, 3))
  p4c <- ms_polish_ggplot(p4c, title_size = 6.25, subtitle_size = 4.10,
                          margin = ggplot2::margin(1, 3, 2, 3), legend_compact = TRUE)

  bottom <- cowplot::plot_grid(
    p4b, p4c, ncol = 2, rel_widths = c(1.06, .94),
    align = "hv", axis = "tblr", greedy = TRUE
  )
  body <- cowplot::plot_grid(
    p4a, bottom, ncol = 1, rel_heights = c(1.18, .82),
    align = "v", axis = "lr", greedy = TRUE
  )
  final <- cowplot::plot_grid(
    metric_legend, body, ncol = 1, rel_heights = c(.042, 1),
    align = "v", axis = "l", greedy = TRUE
  )
  list(plot = final, width = 9.00, height = 6.20)
}

ms_polish_fig5 <- function(plot, env, width, height) {
  p5a <- ms_polish_env_get(env, "p5a")
  p5b <- ms_polish_env_get(env, "p5b")
  p5c <- ms_polish_env_get(env, "p5c")
  if (any(vapply(list(p5a, p5b, p5c), is.null, logical(1)))) {
    return(list(plot = plot, width = width, height = height))
  }

  p5a <- ms_polish_ggplot(
    p5a, title_size = 6.25, subtitle_size = 4.05,
    margin = ggplot2::margin(2, 2.5, 1, 2.5), legend_compact = TRUE
  )
  p5b <- ms_polish_ggplot(
    p5b, title_size = 6.25, subtitle_size = 4.05,
    margin = ggplot2::margin(2, 2.5, 1, 2.5), legend_compact = TRUE
  )
  p5c <- ms_polish_ggplot(
    p5c, title_size = 6.25, subtitle_size = 4.05,
    margin = ggplot2::margin(1, 2.5, 2, 2.5), legend_compact = TRUE
  )

  # Panel b contains three tolerance facets and needs more horizontal room than
  # the single 6 x 6 landscape in panel a. Their outer edges still share exactly
  # the same figure width as the four-facet panel c below.
  top <- cowplot::plot_grid(
    p5a, p5b, ncol = 2, rel_widths = c(.40, .60),
    align = "hv", axis = "tblr", greedy = TRUE
  )
  final <- cowplot::plot_grid(
    top, p5c, ncol = 1, rel_heights = c(.82, 1.00),
    align = "v", axis = "lr", greedy = TRUE
  )
  list(plot = final, width = 7.40, height = 6.10)
}

ms_polish_main_figure <- function(plot, path, env, width, height) {
  name <- basename(path)
  if (identical(name, "Fig1_RQ1.png")) return(ms_polish_fig1(plot, env, width, height))
  if (identical(name, "Fig2_RQ2.png")) return(ms_polish_fig2(plot, env, width, height))
  if (identical(name, "Fig3_RQ2.png")) return(ms_polish_fig3(plot, env, width, height))
  if (identical(name, "Fig4_RQ3.png")) return(ms_polish_fig4(plot, env, width, height))
  if (identical(name, "Fig5_RQ3.png")) return(ms_polish_fig5(plot, env, width, height))
  list(plot = plot, width = width, height = height)
}
