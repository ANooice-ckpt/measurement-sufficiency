# Final display refinement for Fig. 3.
#
# This helper uses only the already-computed RQ2 display summaries created by
# scripts/13b_plot_fig3.R. It does not refit models, recompute gamma/Q/C, or
# change the frozen interaction scope. The refinement is intentionally visual:
#   - top block: distribution-first summaries, with raw metric scatter removed;
#   - bottom block: summary-first transition backbones, with raw scatter removed;
#   - panel c adopts the same density + interval grammar as panel a;
#   - the bottom block receives more vertical space.

ms_fig3_env_get <- function(env, name, default = NULL) {
  if (is.environment(env) && exists(name, envir = env, inherits = FALSE)) {
    get(name, envir = env, inherits = FALSE)
  } else {
    default
  }
}

ms_fig3_refine_main <- function(env) {
  required <- c(
    "a_density", "a_stats", "fig3_q_axis", "fig3_q_breaks", "fig3_q_labels",
    "FIG3_CLASS_COLORS", "FIG3_DISPLAY_CLASSES", "coherence_polygons",
    "coherence_class", "coherence_overall", "b_class_stats", "b_overall_stats",
    "fig3_transition_order", "PAIR_CODES", "theme_rq2", "metric_legend_main"
  )
  objects <- lapply(required, function(nm) ms_fig3_env_get(env, nm))
  names(objects) <- required
  if (any(vapply(objects, is.null, logical(1)))) return(NULL)

  a_density <- objects$a_density
  a_stats <- objects$a_stats
  q_axis <- objects$fig3_q_axis
  q_breaks <- objects$fig3_q_breaks
  q_labels <- objects$fig3_q_labels
  class_colours <- objects$FIG3_CLASS_COLORS
  display_classes <- objects$FIG3_DISPLAY_CLASSES
  coherence_polygons <- objects$coherence_polygons
  coherence_class <- objects$coherence_class
  coherence_overall <- objects$coherence_overall
  b_class <- objects$b_class_stats
  b_overall <- objects$b_overall_stats
  transition_order <- objects$fig3_transition_order
  pair_codes <- objects$PAIR_CODES
  theme_rq2_fn <- objects$theme_rq2
  legend_main <- objects$metric_legend_main

  if (!nrow(a_density) || !nrow(a_stats) || !nrow(coherence_overall) ||
      !nrow(b_class) || !nrow(b_overall)) return(NULL)

  # ---------------------------------------------------------------------------
  # a. Distribution atlas — density + marginal interval, no raw scatter.
  # ---------------------------------------------------------------------------
  p3a <- ggplot2::ggplot() +
    {if (isTRUE(q_axis$use_tail)) ggplot2::geom_vline(
      xintercept = q_axis$focus, colour = "#C5C9CB",
      linewidth = .22, linetype = "22", alpha = .90
    ) else NULL} +
    ggplot2::geom_ribbon(
      data = a_density,
      ggplot2::aes(
        x = x, ymin = .050, ymax = density_y, fill = atlas_class,
        group = interaction(atlas_class, atlas_row, dimension_pair)
      ),
      alpha = .16, colour = NA
    ) +
    ggplot2::geom_line(
      data = a_density,
      ggplot2::aes(
        x = x, y = density_y, colour = atlas_class,
        group = interaction(atlas_class, atlas_row, dimension_pair)
      ),
      linewidth = .32, alpha = .64
    ) +
    ggplot2::geom_segment(
      data = a_stats,
      ggplot2::aes(
        x = Q_q10, xend = Q_q90, y = .008, yend = .008,
        colour = atlas_class
      ),
      linewidth = .25, alpha = .40, lineend = "round"
    ) +
    ggplot2::geom_segment(
      data = a_stats,
      ggplot2::aes(
        x = Q_q25, xend = Q_q75, y = .008, yend = .008,
        colour = atlas_class
      ),
      linewidth = .86, alpha = .90, lineend = "round"
    ) +
    ggplot2::geom_point(
      data = a_stats,
      ggplot2::aes(Q_median, .008, fill = atlas_class),
      shape = 21, size = 1.42, colour = "#30383C", stroke = .20
    ) +
    ggplot2::scale_colour_manual(values = class_colours, guide = "none") +
    ggplot2::scale_fill_manual(values = class_colours, guide = "none") +
    ggplot2::scale_x_continuous(
      limits = c(0, q_axis$display_max), breaks = q_breaks, labels = q_labels,
      expand = ggplot2::expansion(mult = c(0, .010))
    ) +
    ggplot2::scale_y_continuous(
      limits = c(-.045, .52), breaks = NULL,
      expand = ggplot2::expansion(mult = c(0, 0))
    ) +
    ggplot2::facet_grid(
      atlas_row ~ dimension_pair, scales = "fixed", drop = FALSE, switch = "y"
    ) +
    ggplot2::labs(
      title = "a  Non-additivity across representation classes",
      subtitle = "Class distributions of metric-level median Q",
      x = "Median Q per metric", y = NULL
    ) +
    theme_rq2_fn(base_size = 5.65) +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      panel.spacing = grid::unit(.72, "mm"),
      strip.background = ggplot2::element_blank(),
      strip.placement = "outside",
      strip.text.x = ggplot2::element_text(
        size = 4.72, face = "bold", lineheight = .86
      ),
      strip.text.y.left = ggplot2::element_text(
        size = 4.38, angle = 0, hjust = 1, lineheight = .86,
        margin = ggplot2::margin(r = 2)
      ),
      axis.text.x = ggplot2::element_text(size = 3.92),
      axis.title.x = ggplot2::element_text(size = 4.30),
      axis.line.x = ggplot2::element_line(colour = "#505457", linewidth = .27),
      axis.ticks.x = ggplot2::element_line(colour = "#505457", linewidth = .21),
      plot.title = ggplot2::element_text(size = 6.20, hjust = 0, margin = ggplot2::margin(b = 2)),
      plot.subtitle = ggplot2::element_text(
        size = 3.92, colour = "#666A6D", hjust = 0,
        margin = ggplot2::margin(t = -1, b = 2)
      ),
      plot.margin = ggplot2::margin(1, 2.2, 1, 3)
    )

  # ---------------------------------------------------------------------------
  # c. Directional coherence — same density + interval grammar as panel a.
  # Raw metric points are intentionally omitted; the class summaries provide
  # the second-level detail beneath the neutral overall density.
  # ---------------------------------------------------------------------------
  p3c <- ggplot2::ggplot() +
    ggplot2::geom_vline(xintercept = 0, linewidth = .34, colour = "#788186") +
    ggplot2::geom_polygon(
      data = coherence_polygons,
      ggplot2::aes(x, y, group = pair_code),
      fill = "#E9ECEE", colour = "#AAB1B5", linewidth = .25, alpha = .96
    ) +
    ggplot2::geom_segment(
      data = coherence_class,
      ggplot2::aes(
        x = C_q25, xend = C_q75, y = y_summary, yend = y_summary,
        colour = metric_class
      ),
      linewidth = .55, alpha = .84, lineend = "round"
    ) +
    ggplot2::geom_point(
      data = coherence_class,
      ggplot2::aes(C_median, y_summary, colour = metric_class),
      shape = 16, size = .88, alpha = .96
    ) +
    ggplot2::geom_segment(
      data = coherence_overall,
      ggplot2::aes(x = C_q25, xend = C_q75, y = pair_y, yend = pair_y),
      linewidth = 1.02, colour = "#343B3F", lineend = "round"
    ) +
    ggplot2::geom_point(
      data = coherence_overall,
      ggplot2::aes(C_median, pair_y),
      shape = 23, size = 1.72, fill = "#343B3F", colour = "white", stroke = .20
    ) +
    ggplot2::scale_colour_manual(values = MS_METRIC_COLORS, guide = "none") +
    ggplot2::scale_x_continuous(
      limits = c(-1, 1), breaks = c(-1, -.5, 0, .5, 1),
      labels = c("−1", "−.5", "0", ".5", "+1"),
      expand = ggplot2::expansion(mult = c(.010, .010))
    ) +
    ggplot2::scale_y_continuous(
      limits = c(.56, 3.38), breaks = 3:1,
      labels = c("Placement ×\noptical", "Optical ×\ntemporal", "Placement ×\ntemporal"),
      expand = ggplot2::expansion(mult = c(0, 0))
    ) +
    ggplot2::labs(
      title = "c  Directional coherence",
      subtitle = "Overall density with class-level IQRs",
      x = "Directional coherence, C", y = NULL
    ) +
    theme_rq2_fn(base_size = 5.20) +
    ggplot2::theme(
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_line(colour = "#EDF0F1", linewidth = .16),
      axis.text.y = ggplot2::element_text(size = 3.72, lineheight = .84),
      axis.text.x = ggplot2::element_text(size = 3.72),
      axis.title.x = ggplot2::element_text(size = 4.02),
      axis.ticks.y = ggplot2::element_blank(),
      axis.line.y = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(size = 6.20, hjust = 0, margin = ggplot2::margin(b = 2)),
      plot.subtitle = ggplot2::element_text(
        size = 3.92, colour = "#666A6D", hjust = 0,
        margin = ggplot2::margin(t = -1, b = 2)
      ),
      plot.margin = ggplot2::margin(1, 2.5, 1, 2.0)
    )

  # ---------------------------------------------------------------------------
  # b. Transition-specific non-additivity — no raw scatter. Overall median/IQR
  # defines the backbone; coloured class medians/IQRs are the secondary layer.
  # ---------------------------------------------------------------------------
  class_x_offsets <- setNames(
    seq(-.17, .17, length.out = length(display_classes)), display_classes
  )

  make_backbone_panel <- function(pair_name, title,
                                  type = c("categorical", "ordered"),
                                  show_y = TRUE, subtitle = NULL) {
    type <- match.arg(type)
    cls <- b_class |> dplyr::filter(pair_code == pair_name)
    overall <- b_overall |> dplyr::filter(pair_code == pair_name)
    steps <- transition_order |>
      dplyr::filter(pair_code == pair_name) |>
      dplyr::distinct(step_index, x_label) |>
      dplyr::arrange(step_index)
    if (!nrow(cls) || !nrow(overall)) {
      stop("Missing Fig. 3 transition summaries for ", pair_name, call. = FALSE)
    }

    p <- ggplot2::ggplot() +
      {if (isTRUE(q_axis$use_tail)) ggplot2::geom_hline(
        yintercept = q_axis$focus, colour = "#C5C9CB",
        linewidth = .22, linetype = "22", alpha = .90
      ) else NULL}

    if (identical(type, "categorical")) {
      cls <- cls |>
        dplyr::mutate(x_class = x_plot + unname(class_x_offsets[metric_class]))
      p <- p +
        ggplot2::geom_errorbar(
          data = cls,
          ggplot2::aes(
            x = x_class, ymin = Q_q25_plot, ymax = Q_q75_plot,
            colour = metric_class
          ),
          width = .040, linewidth = .42, alpha = .72
        ) +
        ggplot2::geom_point(
          data = cls,
          ggplot2::aes(x_class, Q_median_plot, colour = metric_class),
          shape = 16, size = .96, alpha = .96
        ) +
        ggplot2::geom_errorbar(
          data = overall,
          ggplot2::aes(x = x_plot, ymin = Q_q25_plot, ymax = Q_q75_plot),
          width = .072, linewidth = .92, colour = "#343B3F"
        ) +
        ggplot2::geom_point(
          data = overall,
          ggplot2::aes(x_plot, Q_median_plot),
          shape = 21, size = 1.65, fill = "#343B3F", colour = "white", stroke = .20
        )
    } else {
      p <- p +
        ggplot2::geom_ribbon(
          data = overall,
          ggplot2::aes(
            x = x_plot, ymin = Q_q25_plot, ymax = Q_q75_plot,
            group = placement
          ),
          fill = "#6F777B", alpha = .11, colour = NA
        ) +
        ggplot2::geom_errorbar(
          data = cls,
          ggplot2::aes(
            x = x_plot, ymin = Q_q25_plot, ymax = Q_q75_plot,
            colour = metric_class
          ),
          width = .022, linewidth = .27, alpha = .34
        ) +
        ggplot2::geom_line(
          data = cls,
          ggplot2::aes(
            x_plot, Q_median_plot, colour = metric_class,
            linetype = placement, group = interaction(metric_class, placement)
          ),
          linewidth = .58, alpha = .82
        ) +
        ggplot2::geom_point(
          data = cls,
          ggplot2::aes(x_plot, Q_median_plot, colour = metric_class),
          shape = 16, size = .92, alpha = .94
        ) +
        ggplot2::geom_line(
          data = overall,
          ggplot2::aes(
            x_plot, Q_median_plot, linetype = placement, group = placement
          ),
          linewidth = 1.00, colour = "#343B3F"
        ) +
        ggplot2::geom_point(
          data = overall,
          ggplot2::aes(x_plot, Q_median_plot),
          shape = 21, size = 1.55, fill = "#343B3F", colour = "white", stroke = .20
        )
    }

    p +
      ggplot2::scale_colour_manual(values = MS_METRIC_COLORS, guide = "none") +
      ggplot2::scale_linetype_manual(
        values = c(all = "solid", chest = "solid", wrist = "22"), guide = "none"
      ) +
      ggplot2::scale_x_continuous(
        limits = c(.62, max(steps$step_index) + .38),
        breaks = steps$step_index, labels = steps$x_label,
        expand = ggplot2::expansion(mult = c(0, 0))
      ) +
      ggplot2::scale_y_continuous(
        limits = c(0, q_axis$display_max), breaks = q_breaks,
        labels = if (show_y) q_labels else NULL,
        expand = ggplot2::expansion(mult = c(0, .010))
      ) +
      ggplot2::labs(
        title = title, subtitle = subtitle, x = NULL,
        y = if (show_y) "Non-additivity, Q" else NULL
      ) +
      theme_rq2_fn(base_size = 5.30) +
      ggplot2::theme(
        panel.grid.major.x = ggplot2::element_blank(),
        panel.grid.major.y = ggplot2::element_line(colour = "#EDF0F1", linewidth = .17),
        axis.text.x = ggplot2::element_text(
          size = 3.88, lineheight = .82, margin = ggplot2::margin(t = 1)
        ),
        axis.text.y = if (show_y) ggplot2::element_text(size = 3.88) else ggplot2::element_blank(),
        axis.title.y = if (show_y) ggplot2::element_text(
          size = 4.25, margin = ggplot2::margin(r = 2)
        ) else ggplot2::element_blank(),
        axis.ticks.y = if (show_y) ggplot2::element_line(
          colour = "#505457", linewidth = .20
        ) else ggplot2::element_blank(),
        axis.line.y = ggplot2::element_blank(),
        axis.line.x = ggplot2::element_line(colour = "#505457", linewidth = .27),
        axis.ticks.x = ggplot2::element_line(colour = "#505457", linewidth = .22),
        plot.title = ggplot2::element_text(
          size = 5.10, face = "bold", hjust = 0, margin = ggplot2::margin(b = 1)
        ),
        plot.subtitle = ggplot2::element_text(
          size = 3.72, colour = "#666A6D", hjust = 0,
          margin = ggplot2::margin(t = -1, b = 1)
        ),
        plot.margin = ggplot2::margin(0, 2.5, 1.5, 2.5)
      )
  }

  p3b_po <- make_backbone_panel(
    pair_codes[[1]], "Placement × optical", type = "categorical", show_y = TRUE
  )
  p3b_ot <- make_backbone_panel(
    pair_codes[[2]], "Optical × temporal", type = "ordered", show_y = FALSE
  )
  p3b_pt <- make_backbone_panel(
    pair_codes[[3]], "Placement × temporal", type = "ordered", show_y = FALSE,
    subtitle = "solid = chest · dashed = wrist"
  )

  p3b_body <- cowplot::plot_grid(
    p3b_po, p3b_ot, p3b_pt, ncol = 3,
    rel_widths = c(.82, 1.14, 1.30),
    align = "hv", axis = "tblr", greedy = TRUE
  )
  p3b <- cowplot::ggdraw() +
    cowplot::draw_plot(p3b_body, x = 0, y = 0, width = 1, height = .925) +
    cowplot::draw_label(
      "b  Transition-specific non-additivity",
      x = .002, y = .996, hjust = 0, vjust = 1,
      size = 6.25, fontface = "bold", colour = "#151515", fontfamily = MS_FONT
    )

  # Top block is intentionally a 3+1-column composition: panel a contains three
  # equal interaction columns and panel c occupies the width of a fourth column.
  p3_top <- cowplot::plot_grid(
    p3a, p3c, ncol = 2, rel_widths = c(.75, .25),
    align = "hv", axis = "tblr", greedy = TRUE
  )
  p3_body <- cowplot::plot_grid(
    p3_top, p3b, ncol = 1, rel_heights = c(.76, 1.24),
    align = "v", axis = "lr", greedy = TRUE
  )
  p3 <- cowplot::plot_grid(
    legend_main, p3_body, ncol = 1, rel_heights = c(.040, 1),
    align = "v", axis = "l", greedy = TRUE
  )

  list(
    plot = p3, p3a = p3a, p3b = p3b, p3c = p3c,
    width = 7.40, height = 7.05
  )
}
