# Accepted predictor atlas for current Fig. 3a.
# Uses display objects from scripts/13a_plot_fig3.R; no model fitting.
# The legacy function name is retained for its existing p2a return interface.

ms_fig2_env_get <- function(env, name, default = NULL) {
  if (is.environment(env) && exists(name, envir = env, inherits = FALSE)) {
    get(name, envir = env, inherits = FALSE)
  } else {
    default
  }
}

ms_fig2_add_family_guides <- function(plot, boundaries) {
  if (length(boundaries)) {
    plot + ggplot2::geom_hline(
      yintercept = boundaries, linewidth = .24, colour = "#DDE0E2"
    )
  } else plot
}

ms_fig2_refine_main <- function(env) {
  required <- c(
    "p_labels", "p_strength",
    "coef_summary_all_plot", "coef_summary_dim_plot",
    "status_grid", "predictor_y_limits", "family_boundaries",
    "PREDICTOR_COLORS", "DIMENSION_SHAPES", "coef_window_global"
  )
  objects <- lapply(required, function(nm) ms_fig2_env_get(env, nm))
  names(objects) <- required
  if (any(vapply(objects, is.null, logical(1)))) return(NULL)

  p_labels <- objects$p_labels
  # Narrow strength column: avoid a terminal tick label intruding into signed effects.
  p_strength <- objects$p_strength + ggplot2::theme(axis.text.x = ggplot2::element_text(size = 3.1))
  coef_summary_all_plot <- objects$coef_summary_all_plot
  coef_summary_dim_plot <- objects$coef_summary_dim_plot
  status_grid <- objects$status_grid
  predictor_y_limits <- objects$predictor_y_limits
  family_boundaries <- objects$family_boundaries
  PREDICTOR_COLORS <- objects$PREDICTOR_COLORS
  DIMENSION_SHAPES <- objects$DIMENSION_SHAPES
  coef_window_global <- objects$coef_window_global

  # ---------------------------------------------------------------------------
  # a. Overall coefficient backbone + clearly separated dimension fingerprint
  # ---------------------------------------------------------------------------
  # The barely visible metric/task point cloud is intentionally omitted here.
  # Fig. 3a has two visual levels: the foreground overall distribution
  # and the secondary dimension fingerprint. Full coefficient detail remains in
  # the exported audit tables.
  refined_offsets <- c(
    placement = -.32, optical = -.16,
    temporal = .16, duration = .32
  )
  dim <- coef_summary_dim_plot |>
    dplyr::mutate(y_refined = y + unname(refined_offsets[dimension]))
  miss <- status_grid |>
    dplyr::mutate(y_refined = y + unname(refined_offsets[dimension]))

  make_effect_panel <- function(outcome_name, panel_title) {
    overall <- coef_summary_all_plot |>
      dplyr::filter(outcome_label == outcome_name)
    dim_i <- dim |>
      dplyr::filter(outcome_label == outcome_name)
    miss_i <- miss |>
      dplyr::filter(outcome_label == outcome_name, !is.na(status_label))

    p <- ggplot2::ggplot() +
      ggplot2::geom_vline(xintercept = 0, linewidth = .27, colour = "#A1A6A9") +
      ggplot2::geom_segment(
        data = overall,
        ggplot2::aes(x = estimate_q05_plot, xend = estimate_q95_plot, y = y, yend = y),
        linewidth = .18, alpha = .18, colour = "#687075", lineend = "round"
      ) +
      ggplot2::geom_segment(
        data = overall,
        ggplot2::aes(x = estimate_q25_plot, xend = estimate_q75_plot, y = y, yend = y),
        linewidth = .40, alpha = .56, colour = "#4A5256", lineend = "round"
      ) +
      ggplot2::geom_point(
        data = overall,
        ggplot2::aes(estimate_q50_plot, y, colour = predictor_family),
        shape = 18, size = .90, alpha = 1
      ) +
      ggplot2::geom_segment(
        data = dim_i,
        ggplot2::aes(x = estimate_q25_plot, xend = estimate_q75_plot,
                     y = y_refined, yend = y_refined),
        linewidth = .18, alpha = .40, colour = "#747C80", lineend = "round"
      ) +
      ggplot2::geom_point(
        data = dim_i,
        ggplot2::aes(estimate_q50_plot, y_refined, shape = dimension_label),
        size = .76, stroke = .26, colour = "#39464D", fill = "white", alpha = .98
      ) +
      ggplot2::geom_text(
        data = miss_i,
        ggplot2::aes(x = 0, y = y_refined, label = status_label),
        size = 1.05, colour = "#BEC2C4"
      ) +
      ggplot2::scale_colour_manual(
        values = PREDICTOR_COLORS, drop = FALSE, guide = "none"
      ) +
      ggplot2::scale_shape_manual(
        values = DIMENSION_SHAPES, drop = FALSE, guide = "none"
      ) +
      ggplot2::scale_x_continuous(
        limits = coef_window_global,
        trans = scales::pseudo_log_trans(sigma = .002),
        breaks = c(-.1, -.01, 0, .01, .1),
        labels = c("−.1", "−.01", "0", ".01", ".1")
      ) +
      ggplot2::scale_y_continuous(
        limits = predictor_y_limits, expand = ggplot2::expansion(mult = c(0, 0))
      ) +
      ggplot2::labs(title = panel_title, x = "standardized β", y = NULL) +
      theme_rq2(base_size = 5.30) +
      ggplot2::theme(
        panel.grid.major.y = ggplot2::element_blank(),
        axis.line.y = ggplot2::element_blank(),
        axis.ticks.y = ggplot2::element_blank(),
        axis.text.y = ggplot2::element_blank(),
        axis.text.x = ggplot2::element_text(size = 3.55),
        axis.title.x = ggplot2::element_text(size = 4.15),
        plot.title = ggplot2::element_text(
          size = 5.25, hjust = .5, face = "bold",
          margin = ggplot2::margin(b = 1.2)
        ),
        plot.margin = ggplot2::margin(1.2, .7, 1.0, .7)
      )
    ms_fig2_add_family_guides(p, family_boundaries)
  }

  p_signed <- make_effect_panel("Signed", "Signed effect")
  p_absolute <- make_effect_panel("Absolute", "Absolute effect")
  p2a_core <- cowplot::plot_grid(
    p_labels, p_strength, p_signed, p_absolute,
    ncol = 4, rel_widths = c(.23, .12, .325, .325),
    align = "hv", axis = "tblr", greedy = TRUE
  )
  p2a <- cowplot::ggdraw() +
    cowplot::draw_plot(p2a_core, x = 0, y = .018, width = 1, height = .912) +
    cowplot::draw_label(
      "a  Contextual predictor hierarchy",
      x = .002, y = .998, hjust = 0, vjust = 1,
      fontface = "bold", size = 7.0
    ) +
    cowplot::draw_label(
      "Overall median / IQR with dimension fingerprints · shared pseudo-log axes expand near-zero effects",
      x = .002, y = .968, hjust = 0, vjust = 1,
      colour = "#666A6D", size = 4.25
    )

  list(p2a = p2a)
}
