# Fig. 6 display only. Inputs are the unchanged, mature Fig. 6 display grids.
# No new pooling, sufficiency classification, Pareto calculation or fitting.
ms_fig6_redesign <- function(entry, pareto, classes, resolution_labels, days) {
  ink <- "#233B48"
  muted <- "#657078"
  unresolved <- "#E1E5E7"
  base <- theme_minimal(base_size = 7, base_family = MS_FONT) +
    theme(
      panel.grid = element_blank(), axis.ticks = element_blank(),
      axis.text = element_text(size = 6.5, colour = ink),
      axis.title = element_text(size = 7, colour = ink),
      axis.title.x = element_text(margin = margin(t = 5)),
      axis.title.y = element_text(margin = margin(r = 5)),
      strip.text = element_text(size = 7, face = "bold", colour = ink, margin = margin(b = 5)),
      strip.background = element_blank(),
      panel.spacing.x = grid::unit(2.4, "mm"),
      legend.position = "bottom", legend.title = element_text(size = 6.5),
      legend.text = element_text(size = 6), legend.margin = margin(0, 0, 0, 0),
      legend.box.margin = margin(0, 0, 0, 0),
      plot.margin = margin(3, 5, 3, 3)
    )
  lattice_axes <- list(
    scale_x_continuous(breaks = seq_along(resolution_labels), labels = resolution_labels,
                       limits = c(.5, length(resolution_labels) + .5), expand = expansion(0)),
    scale_y_continuous(breaks = days, limits = c(min(days) - .5, max(days) + .5), expand = expansion(0))
  )

  # a: global joint geometry. Use the range of the displayed medians, not the
  # unplotted individual-metric tail; numbers retain exact reading precision.
  entry$display_label <- ifelse(entry$cell_unresolved, "U",
                                 ifelse(is.finite(entry$epsilon_entry_median), sprintf("%.2f", entry$epsilon_entry_median), "NA"))
  entry$heading <- "Median entry tolerance"
  entry_max <- max(entry$epsilon_entry_median[!entry$cell_unresolved], na.rm = TRUE)
  a <- ggplot(entry, aes(resolution_rank, n_days)) +
    geom_tile(aes(fill = epsilon_entry_median), width = .97, height = .97, colour = "white", linewidth = .35) +
    geom_tile(data = entry[entry$cell_unresolved, ], fill = unresolved, width = .97, height = .97) +
    geom_text(aes(label = display_label,
                  colour = !cell_unresolved & is.finite(epsilon_entry_median) & epsilon_entry_median > .63 * entry_max),
              size = 2.25, show.legend = FALSE) +
    scale_colour_manual(values = c(`FALSE` = ink, `TRUE` = "white")) +
    scale_fill_gradientn(colours = c("#F5F1E9", "#DCC8A3", "#AA7E45", "#704B2B"),
                          limits = c(0, entry_max), na.value = unresolved,
                          breaks = c(0, .25, .50, .75), name = "Entry tolerance") +
    facet_wrap(~heading) + lattice_axes +
    labs(x = "Sampling interval", y = "Monitoring duration (days)") + base +
    guides(fill = guide_colorbar(title.position = "top", barwidth = grid::unit(32, "mm"),
                                 barheight = grid::unit(2, "mm")))

  # b: occupancy is an absolute fraction of the SAME available metric-facets.
  # Area, not radius, is proportional to the fraction. Zero and U are distinct.
  b <- ggplot(pareto, aes(resolution_rank, n_days)) +
    geom_tile(fill = "#F6F8F9", colour = "white", width = .98, height = .98, linewidth = .25) +
    geom_tile(data = pareto[pareto$cell_unresolved, ], fill = unresolved, width = .98, height = .98) +
    geom_point(data = pareto[is.finite(pareto$pareto_fraction) & pareto$pareto_fraction == 0, ],
               size = .35, colour = "#CFD7DB") +
    geom_point(data = pareto[is.finite(pareto$pareto_fraction) & pareto$pareto_fraction > 0, ],
               aes(size = pareto_fraction), shape = 21, stroke = .25, colour = "#31586A", fill = "#5F8B9E", alpha = .92) +
    geom_text(data = pareto[pareto$cell_unresolved, ], label = "U", size = 2.1, colour = muted) +
    scale_size_area(max_size = 5.2, limits = c(0, max(pareto$pareto_fraction, na.rm = TRUE)),
                     breaks = c(.01, .10, .20, .30), labels = scales::label_percent(accuracy = 1),
                     name = "Pareto occupancy") +
    facet_wrap(~epsilon_label, nrow = 1) + lattice_axes +
    labs(x = "Sampling interval", y = NULL) + base +
    guides(size = guide_legend(title.position = "top", nrow = 1,
                                override.aes = list(alpha = 1)))

  # c: re-express all four original 6 x 6 class fields as discrete-day profiles.
  # Heavy endpoint cadences reveal sampling sensitivity; middle cadences remain
  # visible without four more redundant heatmaps. Segments only connect observed
  # states. No envelope, fitted trend or new class statistic is calculated.
  classes$cadence <- ifelse(classes$resolution_rank == 1, "120 s",
                            ifelse(classes$resolution_rank == length(resolution_labels), "10 s", "20-60 s"))
  classes$cadence <- factor(classes$cadence, levels = c("120 s", "10 s", "20-60 s"))
  c <- ggplot(classes, aes(n_days, suff_fraction, group = resolution_rank, colour = metric_class)) +
    geom_hline(yintercept = c(0, .5, 1), colour = "#DDE2E5", linewidth = .3) +
    geom_line(data = classes[classes$cadence == "20-60 s", ], linewidth = .35, alpha = .32, na.rm = TRUE) +
    geom_line(data = classes[classes$cadence != "20-60 s", ], aes(linetype = cadence), linewidth = .7, na.rm = TRUE) +
    geom_point(data = classes[classes$cadence != "20-60 s", ], aes(shape = cadence), size = 1.65, stroke = .6, fill = "white", na.rm = TRUE) +
    facet_wrap(~class_label, nrow = 1) +
    scale_colour_manual(values = MS_METRIC_COLORS, guide = "none") +
    scale_linetype_manual(values = c(`120 s` = "solid", `10 s` = "22"), breaks = c("120 s", "10 s"), name = "Sampling interval") +
    scale_shape_manual(values = c(`120 s` = 16, `10 s` = 21), breaks = c("120 s", "10 s"), name = "Sampling interval") +
    scale_x_continuous(breaks = days, limits = range(days) + c(-.15, .15), expand = expansion(0)) +
    scale_y_continuous(breaks = c(0, .5, 1), labels = scales::label_percent(accuracy = 1),
                       limits = c(0, 1), expand = expansion(add = .04)) +
    labs(x = "Monitoring duration (days)", y = "Metrics sufficient") + base +
    guides(linetype = guide_legend(title.position = "left", nrow = 1),
           shape = guide_legend(title.position = "left", nrow = 1))

  # Align the top plotting fields themselves, not fixed-aspect outer boxes.
  # Titles occupy an independent, identical-height row above both fields.
  aligned <- cowplot::align_plots(a, b, align = "h", axis = "tb")
  class_grob <- ggplotGrob(c)
  left_a <- min(aligned[[1]]$layout$l[grepl("^panel", aligned[[1]]$layout$name)]) - 1L
  left_c <- min(class_grob$layout$l[grepl("^panel", class_grob$layout$name)]) - 1L
  if (left_a == left_c) {
    common_left <- grid::unit.pmax(aligned[[1]]$widths[seq_len(left_a)],
                                   class_grob$widths[seq_len(left_c)])
    aligned[[1]]$widths[seq_len(left_a)] <- common_left
    class_grob$widths[seq_len(left_c)] <- common_left
  }
  header <- function(title, subtitle) {
    cowplot::ggdraw() +
      cowplot::draw_label(title, x = .005, y = .94, hjust = 0, vjust = 1,
                          size = 9, fontfamily = MS_FONT, fontface = "bold", colour = ink) +
      cowplot::draw_label(subtitle, x = .005, y = .36, hjust = 0, vjust = 1,
                          size = 6.5, fontfamily = MS_FONT, colour = muted)
  }
  headers <- cowplot::plot_grid(
    header("a  Joint stability landscape", "Lower entry tolerance = greater stability"),
    header("b  Where minimum-burden solutions occur", "Circle area = occupancy on the frozen Pareto set"),
    nrow = 1, rel_widths = c(.36, .64)
  )
  top <- cowplot::plot_grid(plotlist = aligned, nrow = 1, rel_widths = c(.36, .64))
  c_header <- header("c  Which metric classes need more measurement?",
                     "Shared tolerance = 0.50; faint lines retain the four intermediate sampling intervals")
  note <- cowplot::ggdraw() + cowplot::draw_label(
    "Class heading: share of resolved cells with at least 50% of metrics sufficient.  U: 10 s / 6 d has no higher observed state.",
    x = .005, y = .8, hjust = 0, vjust = 1, fontfamily = MS_FONT, size = 6, colour = muted)
  body <- cowplot::plot_grid(headers, top, c_header, class_grob, note, ncol = 1,
                               rel_heights = c(.10, .51, .10, .32, .04))
  final <- cowplot::ggdraw() + cowplot::draw_plot(body, x = .012, y = .008, width = .976, height = .980)
  list(plot = final, a = a, b = b, c = c)
}
