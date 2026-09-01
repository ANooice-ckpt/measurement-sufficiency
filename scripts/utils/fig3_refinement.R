# Final display refinement for Fig. 3.
#
# This helper uses only the already-computed RQ2 display summaries created by
# scripts/13b_plot_fig3.R. It does not refit models, recompute gamma/Q/C, or
# change the frozen interaction scope. The refinement is intentionally visual:
#   - top block: distribution-first summaries, with raw metric scatter removed;
#   - bottom block: three profile small multiples with independent raw-Q y scales;
#   - panel c adopts the same density + interval grammar as panel a;
#   - all transition panels use one common profile grammar.

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
    "coherence_class", "coherence_overall", "b_class_stats_raw",
    "b_overall_stats_raw", "fig3_transition_order", "PAIR_CODES",
    "theme_rq2", "metric_legend_main"
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
  b_class <- objects$b_class_stats_raw
  b_overall <- objects$b_overall_stats_raw
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
  # b. Transition-specific non-additivity.
  #
  # The three interaction pairs do not share an ordered x semantics, so they are
  # treated as profile small multiples rather than one common coordinate field.
  # Each panel keeps Q in its original units but receives its own 0-to-IQR-range
  # y window. This removes the empty upper field created by the largest pair while
  # preserving within-pair shape, class ordering and uncertainty. All three use
  # the same grammar: neutral overall ribbon/backbone + coloured class profiles.
  # ---------------------------------------------------------------------------
  make_profile_panel <- function(pair_name, title, subtitle = NULL,
                                 placement_linetypes = FALSE) {
    cls <- b_class |> dplyr::filter(pair_code == pair_name)
    overall <- b_overall |> dplyr::filter(pair_code == pair_name)
    steps <- transition_order |>
      dplyr::filter(pair_code == pair_name) |>
      dplyr::distinct(step_index, x_label) |>
      dplyr::arrange(step_index)
    if (!nrow(cls) || !nrow(overall)) {
      stop("Missing Fig. 3 transition summaries for ", pair_name, call. = FALSE)
    }

    if (isTRUE(placement_linetypes)) {
      cls <- cls |>
        dplyr::mutate(
          profile_group = interaction(metric_class, placement, drop = TRUE),
          profile_linetype = placement
        )
      overall <- overall |>
        dplyr::mutate(
          profile_group = placement,
          profile_linetype = placement
        )
    } else {
      cls <- cls |>
        dplyr::mutate(
          profile_group = metric_class,
          profile_linetype = "all"
        )
      overall <- overall |>
        dplyr::mutate(
          profile_group = "overall",
          profile_linetype = "all"
        )
    }

    y_top_raw <- max(
      c(cls$Q_q75, cls$Q_median, overall$Q_q75, overall$Q_median),
      na.rm = TRUE
    )
    if (!is.finite(y_top_raw) || y_top_raw <= 0) y_top_raw <- .10
    y_max <- y_top_raw * 1.14
    y_acc <- if (y_max <= .15) .02 else if (y_max <= .35) .05 else .10
    y_breaks <- scales::breaks_extended(n = 4)(c(0, y_max))
    y_breaks <- sort(unique(c(
      0,
      y_breaks[is.finite(y_breaks) & y_breaks >= 0 & y_breaks <= y_max * 1.001]
    )))

    ggplot2::ggplot() +
      ggplot2::geom_ribbon(
        data = overall,
        ggplot2::aes(
          x = x_plot, ymin = Q_q25, ymax = Q_q75,
          group = profile_group
        ),
        fill = "#7A8286", alpha = .10, colour = NA
      ) +
      ggplot2::geom_errorbar(
        data = cls,
        ggplot2::aes(
          x = x_plot, ymin = Q_q25, ymax = Q_q75,
          colour = metric_class
        ),
        width = .028, linewidth = .30, alpha = .42
      ) +
      ggplot2::geom_line(
        data = cls,
        ggplot2::aes(
          x = x_plot, y = Q_median, colour = metric_class,
          linetype = profile_linetype, group = profile_group
        ),
        linewidth = .60, alpha = .84
      ) +
      ggplot2::geom_point(
        data = cls,
        ggplot2::aes(x_plot, Q_median, colour = metric_class),
        shape = 16, size = .96, alpha = .96
      ) +
      ggplot2::geom_line(
        data = overall,
        ggplot2::aes(
          x = x_plot, y = Q_median,
          linetype = profile_linetype, group = profile_group
        ),
        linewidth = 1.02, colour = "#343B3F"
      ) +
      ggplot2::geom_point(
        data = overall,
        ggplot2::aes(x_plot, Q_median),
        shape = 21, size = 1.58,
        fill = "#343B3F", colour = "white", stroke = .20
      ) +
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
        limits = c(0, y_max), breaks = y_breaks,
        labels = scales::label_number(accuracy = y_acc)(y_breaks),
        expand = ggplot2::expansion(mult = c(0, .012))
      ) +
      ggplot2::labs(title = title, subtitle = subtitle, x = NULL, y = NULL) +
      theme_rq2_fn(base_size = 5.35) +
      ggplot2::theme(
        panel.grid.major.x = ggplot2::element_blank(),
        panel.grid.major.y = ggplot2::element_line(colour = "#EDF0F1", linewidth = .17),
        axis.text.x = ggplot2::element_text(
          size = 3.88, lineheight = .82, margin = ggplot2::margin(t = 1)
        ),
        axis.text.y = ggplot2::element_text(size = 3.82),
        axis.ticks.y = ggplot2::element_line(colour = "#505457", linewidth = .19),
        axis.line.y = ggplot2::element_line(colour = "#505457", linewidth = .25),
        axis.line.x = ggplot2::element_line(colour = "#505457", linewidth = .27),
        axis.ticks.x = ggplot2::element_line(colour = "#505457", linewidth = .22),
        plot.title = ggplot2::element_text(
          size = 5.10, face = "bold", hjust = 0, margin = ggplot2::margin(b = 1)
        ),
        plot.subtitle = ggplot2::element_text(
          size = 3.68, colour = "#666A6D", hjust = 0,
          margin = ggplot2::margin(t = -1, b = 1)
        ),
        plot.margin = ggplot2::margin(0, 2.4, 1.2, 2.4)
      )
  }

  p3b_po <- make_profile_panel(
    pair_codes[[1]], "Placement × optical",
    subtitle = "chest ↔ wrist profile"
  )
  p3b_ot <- make_profile_panel(
    pair_codes[[2]], "Optical × temporal"
  )
  p3b_pt <- make_profile_panel(
    pair_codes[[3]], "Placement × temporal",
    subtitle = "solid = chest · dashed = wrist",
    placement_linetypes = TRUE
  )

  p3b_body <- cowplot::plot_grid(
    p3b_po, p3b_ot, p3b_pt, ncol = 3,
    rel_widths = c(.86, 1.10, 1.26),
    align = "hv", axis = "tblr", greedy = TRUE
  )
  p3b <- cowplot::ggdraw() +
    cowplot::draw_plot(p3b_body, x = .032, y = 0, width = .968, height = .885) +
    cowplot::draw_label(
      "b  Transition-specific non-additivity",
      x = .002, y = .997, hjust = 0, vjust = 1,
      size = 6.25, fontface = "bold", colour = "#151515", fontfamily = MS_FONT
    ) +
    cowplot::draw_label(
      "Independent y scales; profiles compare within each interaction pair",
      x = .032, y = .958, hjust = 0, vjust = 1,
      size = 3.75, colour = "#666A6D", fontfamily = MS_FONT
    ) +
    cowplot::draw_label(
      "Non-additivity, Q",
      x = .007, y = .44, angle = 90, hjust = .5, vjust = .5,
      size = 4.55, colour = "#252B2E", fontfamily = MS_FONT
    )

  # Top block is intentionally a 3+1-column composition: panel a contains three
  # equal interaction columns and panel c occupies the width of a fourth column.
  p3_top <- cowplot::plot_grid(
    p3a, p3c, ncol = 2, rel_widths = c(.75, .25),
    align = "hv", axis = "tblr", greedy = TRUE
  )
  p3_body <- cowplot::plot_grid(
    p3_top, p3b, ncol = 1, rel_heights = c(.82, 1.18),
    align = "v", axis = "lr", greedy = TRUE
  )
  p3 <- cowplot::plot_grid(
    legend_main, p3_body, ncol = 1, rel_heights = c(.040, 1),
    align = "v", axis = "l", greedy = TRUE
  )

  list(
    plot = p3, p3a = p3a, p3b = p3b, p3c = p3c,
    width = 7.40, height = 6.90
  )
}
