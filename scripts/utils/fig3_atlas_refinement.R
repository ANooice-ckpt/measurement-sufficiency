# Alternative final display refinement for Fig. 3b.
#
# This layer deliberately reuses the accepted Fig. 3a/3c refinement and only
# replaces panel b. It does not recompute gamma, Q, C, or any RQ2 estimand.
# Panel b is expressed as a Fig. 2a-style transition fingerprint atlas:
#   - one row per transition;
#   - graphite IQR/median = overall transition summary;
#   - coloured IQR/median marks = metric-class fingerprints;
#   - no raw scatter, ribbons, or trajectory lines;
#   - each interaction-pair block uses its own zero-anchored Q scale.

ms_fig3_atlas_refine_main <- function(env) {
  if (!exists("ms_fig3_refine_main", mode = "function")) return(NULL)
  legacy <- ms_fig3_refine_main(env)
  if (!is.list(legacy) || is.null(legacy$p3a) || is.null(legacy$p3c)) return(NULL)

  get_env <- function(name, default = NULL) {
    if (is.environment(env) && exists(name, envir = env, inherits = FALSE)) {
      get(name, envir = env, inherits = FALSE)
    } else {
      default
    }
  }

  b_class <- get_env("b_class_stats_raw")
  b_overall <- get_env("b_overall_stats_raw")
  transition_order <- get_env("fig3_transition_order")
  pair_codes <- get_env("PAIR_CODES")
  display_classes <- get_env("FIG3_DISPLAY_CLASSES")
  theme_rq2_fn <- get_env("theme_rq2")
  legend_main <- get_env("metric_legend_main")

  if (any(vapply(
    list(b_class, b_overall, transition_order, pair_codes,
         display_classes, theme_rq2_fn, legend_main),
    is.null, logical(1)
  ))) return(NULL)
  if (!nrow(b_class) || !nrow(b_overall)) return(NULL)

  # Small deterministic vertical offsets create the secondary class fingerprint
  # around each overall row without giving classes their own primary axis.
  class_offsets <- setNames(
    seq(.22, -.22, length.out = length(display_classes)),
    display_classes
  )

  format_row_label <- function(pair_name, placement, x_label) {
    if (identical(pair_name, pair_codes[[3]])) {
      prefix <- dplyr::recode(
        as.character(placement), chest = "Chest", wrist = "Wrist", .default = ""
      )
      paste0(prefix, " · ", x_label)
    } else {
      x_label
    }
  }

  make_atlas_block <- function(pair_name, title) {
    cls <- b_class |>
      dplyr::filter(pair_code == pair_name) |>
      dplyr::mutate(metric_class = as.character(metric_class))
    overall <- b_overall |>
      dplyr::filter(pair_code == pair_name)
    order <- transition_order |>
      dplyr::filter(pair_code == pair_name) |>
      dplyr::distinct(transition, step_index, x_label, placement) |>
      dplyr::arrange(step_index, match(placement, c("chest", "wrist", "all")))

    if (!nrow(cls) || !nrow(overall) || !nrow(order)) {
      stop("Missing Fig. 3 atlas rows for ", pair_name, call. = FALSE)
    }

    order <- order |>
      dplyr::mutate(
        row_index = rev(seq_len(dplyr::n())),
        row_label = mapply(
          format_row_label,
          pair_name = pair_name,
          placement = placement,
          x_label = x_label,
          USE.NAMES = FALSE
        )
      )

    overall <- overall |>
      dplyr::inner_join(
        order |> dplyr::select(transition, row_index, row_label),
        by = "transition"
      )
    cls <- cls |>
      dplyr::inner_join(
        order |> dplyr::select(transition, row_index, row_label),
        by = "transition"
      ) |>
      dplyr::mutate(
        class_offset = unname(class_offsets[metric_class]),
        y_class = row_index + class_offset
      )

    x_top <- max(
      c(overall$Q_q75, overall$Q_median, cls$Q_q75, cls$Q_median),
      na.rm = TRUE
    )
    if (!is.finite(x_top) || x_top <= 0) x_top <- .10
    x_max <- x_top * 1.13
    x_breaks <- scales::breaks_extended(n = 4)(c(0, x_max))
    x_breaks <- sort(unique(c(
      0,
      x_breaks[
        is.finite(x_breaks) & x_breaks >= 0 & x_breaks <= x_max * 1.001
      ]
    )))
    x_acc <- if (x_max <= .12) .01 else if (x_max <= .30) .02 else .05

    # A faint row guide helps the eye scan across the overall backbone and the
    # vertically offset fingerprints; it is intentionally weaker than any data.
    row_guides <- order |>
      dplyr::transmute(row_index)

    p <- ggplot2::ggplot() +
      ggplot2::geom_hline(
        data = row_guides,
        ggplot2::aes(yintercept = row_index),
        colour = "#F1F3F4", linewidth = .18
      ) +
      # Overall: first-level graphite backbone.
      ggplot2::geom_segment(
        data = overall,
        ggplot2::aes(
          x = Q_q25, xend = Q_q75,
          y = row_index, yend = row_index
        ),
        colour = "#3E464A", linewidth = 1.05, lineend = "round"
      ) +
      ggplot2::geom_point(
        data = overall,
        ggplot2::aes(Q_median, row_index),
        shape = 23, size = 1.70,
        fill = "#343B3F", colour = "white", stroke = .22
      ) +
      # Classes: secondary fingerprint, vertically offset within each row.
      ggplot2::geom_segment(
        data = cls,
        ggplot2::aes(
          x = Q_q25, xend = Q_q75,
          y = y_class, yend = y_class,
          colour = metric_class
        ),
        linewidth = .43, alpha = .72, lineend = "round"
      ) +
      ggplot2::geom_point(
        data = cls,
        ggplot2::aes(Q_median, y_class, colour = metric_class),
        shape = 16, size = .86, alpha = .96
      ) +
      ggplot2::scale_colour_manual(values = MS_METRIC_COLORS, guide = "none") +
      ggplot2::scale_x_continuous(
        limits = c(0, x_max), breaks = x_breaks,
        labels = scales::label_number(accuracy = x_acc)(x_breaks),
        expand = ggplot2::expansion(mult = c(0, .012))
      ) +
      ggplot2::scale_y_continuous(
        limits = c(.48, max(order$row_index) + .52),
        breaks = order$row_index, labels = order$row_label,
        expand = ggplot2::expansion(mult = c(0, 0))
      ) +
      ggplot2::labs(title = title, x = "Non-additivity, Q", y = NULL) +
      theme_rq2_fn(base_size = 5.25) +
      ggplot2::theme(
        panel.grid = ggplot2::element_blank(),
        axis.text.y = ggplot2::element_text(
          size = 3.75, lineheight = .84, hjust = 1,
          margin = ggplot2::margin(r = 2)
        ),
        axis.text.x = ggplot2::element_text(size = 3.78),
        axis.title.x = ggplot2::element_text(size = 4.05, margin = ggplot2::margin(t = 2)),
        axis.ticks.y = ggplot2::element_blank(),
        axis.line.y = ggplot2::element_blank(),
        axis.line.x = ggplot2::element_line(colour = "#505457", linewidth = .27),
        axis.ticks.x = ggplot2::element_line(colour = "#505457", linewidth = .20),
        plot.title = ggplot2::element_text(
          size = 5.15, face = "bold", hjust = 0,
          margin = ggplot2::margin(b = 2)
        ),
        plot.margin = ggplot2::margin(1, 2.0, 1.5, 2.0)
      )

    # Placement × temporal has two five-row groups. A subtle separator makes the
    # chest/wrist structure explicit without restoring linetype as a second code.
    if (identical(pair_name, pair_codes[[3]]) && nrow(order) >= 10L) {
      sep_y <- mean(c(order$row_index[5], order$row_index[6]))
      p <- p + ggplot2::geom_hline(
        yintercept = sep_y, colour = "#D8DCDE", linewidth = .26
      )
    }

    p
  }

  p3b_po <- make_atlas_block(pair_codes[[1]], "Placement × optical")
  p3b_ot <- make_atlas_block(pair_codes[[2]], "Optical × temporal")
  p3b_pt <- make_atlas_block(pair_codes[[3]], "Placement × temporal")

  # The first block has only two transitions, so it is intentionally narrower;
  # the dense placement × temporal block gets the most horizontal room.
  p3b_body <- cowplot::plot_grid(
    p3b_po, p3b_ot, p3b_pt, ncol = 3,
    rel_widths = c(.78, 1.08, 1.42),
    align = "hv", axis = "tblr", greedy = TRUE
  )
  p3b <- cowplot::ggdraw() +
    cowplot::draw_plot(p3b_body, x = .010, y = 0, width = .990, height = .900) +
    cowplot::draw_label(
      "b  Transition-specific non-additivity",
      x = .002, y = .997, hjust = 0, vjust = 1,
      size = 6.25, fontface = "bold",
      colour = "#151515", fontfamily = MS_FONT
    ) +
    cowplot::draw_label(
      "Graphite = overall IQR/median; colours = class fingerprints; Q scales are independent by interaction pair",
      x = .010, y = .958, hjust = 0, vjust = 1,
      size = 3.70, colour = "#666A6D", fontfamily = MS_FONT
    )

  p3_top <- cowplot::plot_grid(
    legacy$p3a, legacy$p3c, ncol = 2, rel_widths = c(.75, .25),
    align = "hv", axis = "tblr", greedy = TRUE
  )
  p3_body <- cowplot::plot_grid(
    p3_top, p3b, ncol = 1, rel_heights = c(.78, 1.22),
    align = "v", axis = "lr", greedy = TRUE
  )
  p3 <- cowplot::plot_grid(
    legend_main, p3_body, ncol = 1, rel_heights = c(.040, 1),
    align = "v", axis = "l", greedy = TRUE
  )

  list(
    plot = p3, p3a = legacy$p3a, p3b = p3b, p3c = legacy$p3c,
    width = 7.40, height = 7.05
  )
}
