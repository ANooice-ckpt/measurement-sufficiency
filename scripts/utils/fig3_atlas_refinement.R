# Current Fig. 4 (historical Fig. 3): distributions, transition hotspots, and
# the joint magnitude/coherence relationship. Uses frozen display summaries only.
# C_metric remains median_t(R_t / Q_t), never median(R_t) / median(Q_t).
ms_fig3_atlas_refine_main <- function(env) {
  if (!exists("ms_fig3_refine_main", mode = "function")) return(NULL)
  legacy <- ms_fig3_refine_main(env)
  if (!is.list(legacy) || is.null(legacy$p3a)) return(NULL)
  required <- c("b_class_stats_raw", "b_overall_stats_raw", "fig3_transition_order",
                "PAIR_CODES", "FIG3_DISPLAY_CLASSES", "gamma_metric",
                "theme_rq2", "metric_legend_main", "a_metric_plot")
  inputs <- setNames(lapply(required, function(n) ms_fig3_env_get(env, n)), required)
  if (any(vapply(inputs, is.null, logical(1)))) return(NULL)
  pairs <- inputs$PAIR_CODES
  classes <- inputs$FIG3_DISPLAY_CLASSES
  theme_fn <- inputs$theme_rq2
  blocks <- c("Placement × optical", "Optical × temporal",
              "Placement × temporal · chest", "Placement × temporal · wrist")
  order <- inputs$fig3_transition_order |>
    dplyr::mutate(
      block = factor(dplyr::case_when(
        pair_code == pairs[[1]] ~ blocks[[1]],
        pair_code == pairs[[2]] ~ blocks[[2]],
        placement == "chest" ~ blocks[[3]], TRUE ~ blocks[[4]]
      ), levels = blocks),
      column = ifelse(pair_code == pairs[[1]], placement, sub(" s$", "", x_label)),
      column = factor(column, levels = c("chest", "wrist", "120→60", "60→40",
                                        "40→30", "30→20", "20→10"))
    ) |>
    dplyr::select(pair_code, transition, block, column)
  # Existing medians: no pooling across pairs, row normalization or thresholds.
  cells <- dplyr::bind_rows(
    inputs$b_overall_stats_raw |> dplyr::mutate(metric_class = "Overall"),
    inputs$b_class_stats_raw
  ) |>
    dplyr::inner_join(order, by = c("pair_code", "transition")) |>
    dplyr::mutate(row = factor(metric_class, levels = rev(c("Overall", classes))))
  stopifnot(nrow(cells) == nrow(inputs$b_overall_stats_raw) + nrow(inputs$b_class_stats_raw),
            !anyDuplicated(cells[c("block", "column", "row")]))
  q_max <- max(cells$Q_median, na.rm = TRUE)
  p3b <- ggplot2::ggplot(cells, ggplot2::aes(column, row)) +
    ggplot2::geom_tile(ggplot2::aes(fill = Q_median), colour = "white", linewidth = .65) +
    ggplot2::geom_text(ggplot2::aes(
      label = ifelse(!is.finite(Q_median), "NA",
        ifelse(Q_median > 0 & Q_median < .005, "<.01", sprintf("%.2f", Q_median))),
      colour = Q_median > .58 * q_max
    ), size = 1.65, family = MS_FONT) +
    ggplot2::scale_colour_manual(values = c("FALSE" = "#34434B", "TRUE" = "white"), guide = "none") +
    ggplot2::scale_fill_gradient(low = "#F3F6F6", high = "#28576B", limits = c(0, q_max),
      na.value = "#D8DCDD", name = "Median Q", breaks = scales::breaks_extended(4)) +
    ggplot2::facet_grid(. ~ block, scales = "free_x", space = "free_x") +
    ggplot2::scale_x_discrete(expand = c(0, 0)) +
    ggplot2::scale_y_discrete(expand = c(0, 0), labels = function(x) {
      sub("Temporal dynamics", "Temporal\ndynamics", stringr::str_to_sentence(x), fixed = TRUE)
    }) +
    ggplot2::labs(title = "b  Where non-additivity concentrates",
      subtitle = "Existing transition medians · shared Q scale · temporal steps in seconds; optical change: LIGHT → MEDI",
      x = NULL, y = NULL) +
    theme_fn(base_size = 6) +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(), axis.line = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(), axis.text = ggplot2::element_text(size = 4.4),
      strip.background = ggplot2::element_blank(), strip.text = ggplot2::element_text(size = 4.7, face = "bold"),
      panel.spacing.x = grid::unit(2.4, "mm"),
      plot.title = ggplot2::element_text(size = 6.4, face = "bold"),
      plot.subtitle = ggplot2::element_text(size = 4.1, colour = "#687277"),
      legend.position = "right", legend.title = ggplot2::element_text(size = 4.4),
      legend.text = ggplot2::element_text(size = 4),
      legend.key.height = grid::unit(5, "mm"), legend.key.width = grid::unit(2, "mm"),
      plot.margin = ggplot2::margin(5, 4, 5, 4))

  points <- inputs$gamma_metric |>
    dplyr::filter(is.finite(Q_metric), is.finite(C_metric)) |>
    dplyr::mutate(pair = factor(pair_code, levels = pairs,
      labels = c("Placement × optical", "Optical × temporal", "Placement × temporal")),
      display_class = ifelse(as.character(metric_class) %in% classes,
                              as.character(metric_class), "Other"))
  # Joint geometry complements panel a's margins. Metrics/classes are not
  # independent inferential replicates; no regression or correlation is fitted.
  p3c <- ggplot2::ggplot(points, ggplot2::aes(Q_metric, C_metric)) +
    ggplot2::geom_hline(yintercept = 0, linewidth = .3, colour = "#919A9F") +
    ggplot2::geom_point(ggplot2::aes(colour = display_class), shape = 21, fill = "white", size = 1.25, alpha = .78, stroke = .45) +
    ggplot2::scale_colour_manual(values = c(MS_METRIC_COLORS, Other = "#90999E"), guide = "none") +
    ggplot2::scale_x_continuous(limits = c(0, NA), breaks = scales::breaks_extended(5),
                               expand = ggplot2::expansion(mult = c(.015, .04))) +
    ggplot2::scale_y_continuous(trans = scales::pseudo_log_trans(sigma = .02), limits = c(-1, 1),
      breaks = c(-1, -.1, 0, .1, 1), labels = c("−1", "−.1", "0", ".1", "1")) +
    ggplot2::facet_wrap(~pair, nrow = 1) +
    ggplot2::labs(title = "c  Does stronger non-additivity have a consistent direction?",
      subtitle = "One point per representation · C = median across transitions of R/Q; zero indicates cancellation · grey: other classes",
      x = "Metric-level median Q", y = "Directional coherence, C\n(pseudo-log scale)") +
    theme_fn(base_size = 6) +
    ggplot2::theme(panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_line(colour = "#F0F2F3", linewidth = .14),
      strip.background = ggplot2::element_blank(), strip.text = ggplot2::element_text(size = 5, face = "bold"),
      axis.text = ggplot2::element_text(size = 4.5), axis.title = ggplot2::element_text(size = 5),
      plot.title = ggplot2::element_text(size = 6.4, face = "bold"),
      plot.subtitle = ggplot2::element_text(size = 4.1, colour = "#687277"),
      panel.spacing = grid::unit(3, "mm"), plot.margin = ggplot2::margin(5, 4, 2, 4))
  p3a <- legacy$p3a + ggplot2::geom_point(data = inputs$a_metric_plot,
    ggplot2::aes(Q_plot, raw_y, colour = atlas_class), size = .62, alpha = .55)
  body <- cowplot::plot_grid(p3a, p3b, p3c, ncol = 1, rel_heights = c(.46, .24, .30),
                             align = "v", axis = "lr", greedy = FALSE)
  plot <- cowplot::plot_grid(inputs$metric_legend_main, body, ncol = 1, rel_heights = c(.035, 1))
  list(plot = plot, p3a = p3a, p3b = p3b, p3c = p3c, width = 7.4, height = 7.3)
}
