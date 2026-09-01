# Final display refinement for Fig. 2.
#
# This helper uses only display objects and already-aggregated frozen RQ2 outputs
# constructed by scripts/13a_plot_fig2.R. It does not refit models or redefine
# any RQ2 estimand. The refinements are deliberately local:
#   a) separate overall coefficient summaries from the dimension fingerprint;
#   b) give each measurement dimension its own symmetric viewing window for
#      context-induced geometry displacement;
#   c) replace the full CV distribution in the main figure with a descriptive
#      ranked view of the most context-recoverable representation tasks.
# The complete participant-grouped CV distribution remains in the canonical
# fig2_joint_context_cv.csv audit output.

ms_fig2_env_get <- function(env, name, default = NULL) {
  if (is.environment(env) && exists(name, envir = env, inherits = FALSE)) {
    get(name, envir = env, inherits = FALSE)
  } else {
    default
  }
}

ms_fig2_robust_symmetric_limit <- function(values, foreground = numeric(),
                                           prob = .95, pad = 1.14,
                                           fallback = .05, hard_cap = NULL) {
  values <- suppressWarnings(as.numeric(values))
  values <- values[is.finite(values)]
  foreground <- suppressWarnings(as.numeric(foreground))
  foreground <- foreground[is.finite(foreground)]
  core <- if (length(values)) {
    as.numeric(stats::quantile(abs(values), prob, na.rm = TRUE,
                               names = FALSE, type = 8))
  } else NA_real_
  fg <- if (length(foreground)) max(abs(foreground), na.rm = TRUE) else NA_real_
  lim <- suppressWarnings(max(c(core, fg, fallback / pad), na.rm = TRUE)) * pad
  if (!is.finite(lim) || lim <= 0) lim <- fallback
  if (!is.null(hard_cap) && is.finite(hard_cap)) lim <- min(lim, hard_cap)
  lim
}

ms_fig2_add_family_guides <- function(plot, boundaries) {
  if (length(boundaries)) {
    plot + ggplot2::geom_hline(
      yintercept = boundaries, linewidth = .24, colour = "#DDE0E2"
    )
  } else plot
}

ms_fig2_refine_main <- function(env, top_n = 8L) {
  required <- c(
    "p_labels", "p_strength", "predictor_legend", "dimension_legend",
    "coef_metric_plot", "coef_summary_all_plot", "coef_summary_dim_plot",
    "status_grid", "predictor_y_limits", "family_boundaries",
    "PREDICTOR_COLORS", "DIMENSION_SHAPES", "coef_window_global",
    "conditional_shift_path_metric", "conditional_shift_class_path",
    "conditional_shift_overall_path", "DIMENSIONS", "DIM_TITLES",
    "joint_cv_metric_panel", "metric_legend_right"
  )
  objects <- lapply(required, function(nm) ms_fig2_env_get(env, nm))
  names(objects) <- required
  if (any(vapply(objects, is.null, logical(1)))) return(NULL)

  p_labels <- objects$p_labels
  p_strength <- objects$p_strength
  predictor_legend <- objects$predictor_legend
  dimension_legend <- objects$dimension_legend
  coef_metric_plot <- objects$coef_metric_plot
  coef_summary_all_plot <- objects$coef_summary_all_plot
  coef_summary_dim_plot <- objects$coef_summary_dim_plot
  status_grid <- objects$status_grid
  predictor_y_limits <- objects$predictor_y_limits
  family_boundaries <- objects$family_boundaries
  PREDICTOR_COLORS <- objects$PREDICTOR_COLORS
  DIMENSION_SHAPES <- objects$DIMENSION_SHAPES
  coef_window_global <- objects$coef_window_global
  DIMENSIONS <- objects$DIMENSIONS
  DIM_TITLES <- objects$DIM_TITLES

  # ---------------------------------------------------------------------------
  # a. Overall coefficient backbone + clearly separated dimension fingerprint
  # ---------------------------------------------------------------------------
  # The four secondary dimension summaries are spread farther from the central
  # overall row. White underlays act as a halo where thin dimension IQRs cross
  # the graphite overall IQR, preventing the secondary marks from disappearing.
  refined_offsets <- c(
    placement = -.19, optical = -.063,
    temporal = .063, duration = .19
  )
  dim <- coef_summary_dim_plot |>
    dplyr::mutate(y_refined = y + unname(refined_offsets[dimension]))
  miss <- status_grid |>
    dplyr::mutate(y_refined = y + unname(refined_offsets[dimension]))

  make_effect_panel <- function(outcome_name, panel_title) {
    raw <- coef_metric_plot |>
      dplyr::filter(outcome_label == outcome_name)
    overall <- coef_summary_all_plot |>
      dplyr::filter(outcome_label == outcome_name)
    dim_i <- dim |>
      dplyr::filter(outcome_label == outcome_name)
    miss_i <- miss |>
      dplyr::filter(outcome_label == outcome_name, !is.na(status_label))

    p <- ggplot2::ggplot() +
      ggplot2::geom_vline(xintercept = 0, linewidth = .27, colour = "#A1A6A9") +
      ggplot2::geom_point(
        data = raw,
        ggplot2::aes(estimate_plot, y, colour = predictor_family),
        position = ggplot2::position_jitter(width = 0, height = .055, seed = 72),
        shape = 16, size = .15, alpha = .022
      ) +
      ggplot2::geom_segment(
        data = overall,
        ggplot2::aes(x = estimate_q05_plot, xend = estimate_q95_plot, y = y, yend = y),
        linewidth = .26, alpha = .29, colour = "#596166", lineend = "round"
      ) +
      ggplot2::geom_segment(
        data = overall,
        ggplot2::aes(x = estimate_q25_plot, xend = estimate_q75_plot, y = y, yend = y),
        linewidth = .70, alpha = .82, colour = "#3F474B", lineend = "round"
      ) +
      ggplot2::geom_point(
        data = overall,
        ggplot2::aes(estimate_q50_plot, y, colour = predictor_family),
        shape = 18, size = .92, alpha = 1
      ) +
      # White halo behind dimension-specific IQRs.
      ggplot2::geom_segment(
        data = dim_i,
        ggplot2::aes(x = estimate_q25_plot, xend = estimate_q75_plot,
                     y = y_refined, yend = y_refined),
        linewidth = .62, alpha = .98, colour = "white", lineend = "round"
      ) +
      ggplot2::geom_segment(
        data = dim_i,
        ggplot2::aes(x = estimate_q25_plot, xend = estimate_q75_plot,
                     y = y_refined, yend = y_refined),
        linewidth = .18, alpha = .58, colour = "#70787C", lineend = "round"
      ) +
      # White point underlay keeps open dimension glyphs legible on the backbone.
      ggplot2::geom_point(
        data = dim_i,
        ggplot2::aes(estimate_q50_plot, y_refined, shape = dimension_label),
        size = .96, stroke = .72, colour = "white", fill = "white", alpha = 1
      ) +
      ggplot2::geom_point(
        data = dim_i,
        ggplot2::aes(estimate_q50_plot, y_refined, shape = dimension_label),
        size = .60, stroke = .25, colour = "#616A6F", fill = "white", alpha = .96
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
        breaks = scales::breaks_extended(n = 3)
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
        plot.margin = ggplot2::margin(1.5, .7, 1.5, .7)
      )
    ms_fig2_add_family_guides(p, family_boundaries)
  }

  p_signed <- make_effect_panel("Signed", "Signed effect")
  p_absolute <- make_effect_panel("Absolute", "Absolute effect")
  p2a_core <- cowplot::plot_grid(
    p_labels, p_strength, p_signed, p_absolute,
    ncol = 4, rel_widths = c(.23, .12, .325, .325),
    align = "hv", axis = "tb", greedy = TRUE
  )
  p2a_legends <- cowplot::plot_grid(
    predictor_legend, dimension_legend,
    ncol = 2, rel_widths = c(.48, .52),
    align = "h", axis = "b", greedy = TRUE
  )
  p2a_body <- cowplot::plot_grid(
    p2a_core, p2a_legends,
    ncol = 1, rel_heights = c(.935, .065),
    align = "v", axis = "l", greedy = TRUE
  )
  p2a <- cowplot::ggdraw() +
    cowplot::draw_plot(p2a_body, x = 0, y = 0, width = 1, height = .965) +
    cowplot::draw_label(
      "a  Contextual predictor hierarchy",
      x = .002, y = .998, hjust = 0, vjust = 1,
      fontface = "bold", size = 7.0
    ) +
    cowplot::draw_label(
      "overall coefficient distributions are foreground; dimension-specific estimates form the secondary fingerprint",
      x = .002, y = .972, hjust = 0, vjust = 1,
      colour = "#666A6D", size = 4.25
    )

  # ---------------------------------------------------------------------------
  # b. Dimension-specific displacement windows
  # ---------------------------------------------------------------------------
  raw_shift <- objects$conditional_shift_path_metric
  class_shift <- objects$conditional_shift_class_path
  overall_shift <- objects$conditional_shift_overall_path

  make_shift_panel <- function(dim_name, show_y = TRUE, show_x = TRUE) {
    raw <- raw_shift |>
      dplyr::filter(dimension == dim_name)
    cls <- class_shift |>
      dplyr::filter(dimension == dim_name)
    ov <- overall_shift |>
      dplyr::filter(dimension == dim_name)

    x_lim <- ms_fig2_robust_symmetric_limit(
      raw$delta_A[raw$state_num > 1L],
      c(cls$delta_A, ov$delta_A),
      prob = .95, pad = 1.16, fallback = .035
    )
    y_lim <- ms_fig2_robust_symmetric_limit(
      raw$delta_direction[raw$state_num > 1L],
      c(cls$delta_direction, ov$delta_direction),
      prob = .95, pad = 1.16, fallback = .06, hard_cap = 2.05
    )
    ov <- ov |>
      dplyr::mutate(
        state_label = dplyr::case_when(
          state_num == 2L ~ "M",
          state_num == 3L ~ "H",
          TRUE ~ ""
        )
      )

    p <- ggplot2::ggplot() +
      ggplot2::geom_hline(yintercept = 0, linewidth = .24, colour = "#C5C9CB") +
      ggplot2::geom_vline(xintercept = 0, linewidth = .24, colour = "#C5C9CB") +
      ggplot2::geom_path(
        data = raw,
        ggplot2::aes(delta_A, delta_direction,
                     group = interaction(metric, metric_class), colour = metric_class),
        linewidth = .09, alpha = .032
      ) +
      ggplot2::geom_point(
        data = raw |> dplyr::filter(state_num > 1L),
        ggplot2::aes(delta_A, delta_direction, colour = metric_class),
        size = .20, alpha = .070
      ) +
      ggplot2::geom_path(
        data = cls,
        ggplot2::aes(delta_A, delta_direction, group = metric_class, colour = metric_class),
        linewidth = .50, alpha = .74
      ) +
      ggplot2::geom_point(
        data = cls |> dplyr::filter(state_num > 1L),
        ggplot2::aes(delta_A, delta_direction, colour = metric_class, shape = state),
        size = .86, alpha = .96
      ) +
      ggplot2::geom_path(
        data = ov,
        ggplot2::aes(delta_A, delta_direction, group = 1),
        linewidth = 1.00, colour = "#343B3F"
      ) +
      ggplot2::geom_point(
        data = ov,
        ggplot2::aes(delta_A, delta_direction, shape = state),
        size = 1.34, colour = "#343B3F", fill = "white", stroke = .30
      ) +
      ggplot2::geom_text(
        data = ov |> dplyr::filter(state_num > 1L),
        ggplot2::aes(delta_A, delta_direction, label = state_label),
        nudge_x = .045 * x_lim, nudge_y = .055 * y_lim,
        size = 1.38, colour = "#343B3F", fontface = "bold"
      ) +
      scale_colour_ms_metric(guide = "none") +
      ggplot2::scale_shape_manual(
        values = c("Low" = 1, "Middle" = 16, "High" = 18), guide = "none"
      ) +
      ggplot2::scale_x_continuous(
        limits = c(-x_lim, x_lim), breaks = scales::breaks_extended(n = 3),
        expand = ggplot2::expansion(mult = c(0, 0))
      ) +
      ggplot2::scale_y_continuous(
        limits = c(-y_lim, y_lim), breaks = scales::breaks_extended(n = 3),
        expand = ggplot2::expansion(mult = c(0, 0))
      ) +
      ggplot2::labs(title = unname(DIM_TITLES[[dim_name]]), x = NULL, y = NULL) +
      theme_rq2(base_size = 5.15) +
      ggplot2::theme(
        panel.grid = ggplot2::element_blank(),
        strip.text = ggplot2::element_blank(),
        plot.title = ggplot2::element_text(
          size = 4.75, hjust = .5, face = "bold", margin = ggplot2::margin(b = 1.0)
        ),
        axis.text = ggplot2::element_text(size = 3.35),
        axis.text.y = if (show_y) ggplot2::element_text(size = 3.35) else ggplot2::element_blank(),
        axis.ticks.y = if (show_y) ggplot2::element_line(linewidth = .22) else ggplot2::element_blank(),
        axis.line.y = if (show_y) ggplot2::element_line(colour = "#505457", linewidth = .30) else ggplot2::element_blank(),
        axis.text.x = if (show_x) ggplot2::element_text(size = 3.35) else ggplot2::element_blank(),
        axis.ticks.x = if (show_x) ggplot2::element_line(linewidth = .22) else ggplot2::element_blank(),
        plot.margin = ggplot2::margin(1.0, 1.2, 1.0, 1.2)
      )
    p
  }

  p_b1 <- make_shift_panel("placement", show_y = TRUE,  show_x = FALSE)
  p_b2 <- make_shift_panel("optical",   show_y = FALSE, show_x = FALSE)
  p_b3 <- make_shift_panel("temporal",  show_y = TRUE,  show_x = TRUE)
  p_b4 <- make_shift_panel("duration",  show_y = FALSE, show_x = TRUE)
  p2b_grid <- cowplot::plot_grid(
    p_b1, p_b2, p_b3, p_b4,
    ncol = 2, rel_widths = c(1, 1), rel_heights = c(1, 1),
    align = "hv", axis = "tblr", greedy = TRUE
  )
  p2b <- cowplot::ggdraw() +
    cowplot::draw_plot(p2b_grid, x = .075, y = .085, width = .920, height = .795) +
    cowplot::draw_label(
      "b  Context-induced geometry shifts",
      x = .004, y = .998, hjust = 0, vjust = 1,
      fontface = "bold", size = 6.25
    ) +
    cowplot::draw_label(
      "Low = origin; M/H = Middle/High displacement · dimension-specific symmetric viewing windows",
      x = .004, y = .963, hjust = 0, vjust = 1,
      colour = "#666A6D", size = 3.72
    ) +
    cowplot::draw_label(
      expression(Delta * " distortion magnitude, A"),
      x = .535, y = .014, hjust = .5, vjust = 0,
      size = 4.05, colour = "#34383B"
    ) +
    cowplot::draw_label(
      expression(Delta * " directional coherence, B/A"),
      x = .017, y = .485, angle = 90, hjust = .5, vjust = .5,
      size = 4.05, colour = "#34383B"
    )

  # ---------------------------------------------------------------------------
  # c. Ranked recoverability showcase
  # ---------------------------------------------------------------------------
  cv <- objects$joint_cv_metric_panel |>
    dplyr::filter(is.finite(r2)) |>
    dplyr::mutate(
      metric_class = factor(metric_class, levels = MS_METRIC_CLASSES),
      dimension = as.character(dimension),
      outcome_label = as.character(outcome_label)
    )
  positive <- cv |>
    dplyr::filter(r2 > 0) |>
    dplyr::arrange(dplyr::desc(r2)) |>
    dplyr::slice_head(n = as.integer(top_n))
  if (!nrow(positive)) {
    positive <- cv |>
      dplyr::arrange(dplyr::desc(r2)) |>
      dplyr::slice_head(n = as.integer(top_n))
  }

  dim_short <- c(
    placement = "P", optical = "O", temporal = "T", duration = "D"
  )
  positive <- positive |>
    dplyr::mutate(
      metric_text = stringr::str_replace_all(as.character(metric), "_", " "),
      metric_text = stringr::str_trunc(metric_text, width = 25, side = "right", ellipsis = "…"),
      outcome_code = dplyr::if_else(outcome_label == "Absolute distortion", "|z|", "z"),
      row_label = paste0(metric_text, " · ", unname(dim_short[dimension]), " · ", outcome_code),
      row_key = paste(row_label, dimension, outcome_label, row_number(), sep = "|||"),
      row_key = forcats::fct_reorder(row_key, r2)
    )
  r2_max <- if (nrow(positive)) max(positive$r2, na.rm = TRUE) else .1
  if (!is.finite(r2_max) || r2_max <= 0) r2_max <- .1

  p2c <- ggplot2::ggplot(positive, ggplot2::aes(r2, row_key, colour = metric_class)) +
    ggplot2::geom_vline(xintercept = 0, linewidth = .25, colour = "#A8ADB0") +
    ggplot2::geom_segment(
      ggplot2::aes(x = 0, xend = r2, yend = row_key),
      linewidth = .55, alpha = .35, lineend = "round"
    ) +
    ggplot2::geom_point(shape = 18, size = 1.28, alpha = .98) +
    ggplot2::geom_text(
      ggplot2::aes(label = sprintf("%.2f", r2)),
      hjust = -.18, size = 1.45, colour = "#4E5559", show.legend = FALSE
    ) +
    scale_colour_ms_metric(guide = "none") +
    ggplot2::scale_x_continuous(
      limits = c(0, r2_max * 1.18),
      breaks = scales::breaks_extended(n = 4),
      expand = ggplot2::expansion(mult = c(0, 0))
    ) +
    ggplot2::scale_y_discrete(
      labels = function(x) sub("\\|\\|\\|.*$", "", x)
    ) +
    ggplot2::labs(
      title = "c  Most context-recoverable representations",
      subtitle = paste0("top ", nrow(positive),
                        " positive participant-grouped CV R² tasks; complete distribution retained in audit output"),
      x = "participant-grouped CV R²", y = NULL
    ) +
    theme_rq2(base_size = 5.05) +
    ggplot2::theme(
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      axis.line.y = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_text(size = 3.55, colour = "#3F4447"),
      axis.text.x = ggplot2::element_text(size = 3.45),
      axis.title.x = ggplot2::element_text(size = 3.95),
      plot.title = ggplot2::element_text(
        size = 6.05, hjust = 0, face = "bold", margin = ggplot2::margin(b = 1.4)
      ),
      plot.subtitle = ggplot2::element_text(
        size = 3.55, colour = "#666A6D", hjust = 0,
        margin = ggplot2::margin(t = -1, b = 1.5)
      ),
      plot.margin = ggplot2::margin(1.2, 4.0, 1.0, 1.0)
    )

  # ---------------------------------------------------------------------------
  # Final composition
  # ---------------------------------------------------------------------------
  metric_legend_right <- objects$metric_legend_right
  right_top <- cowplot::plot_grid(
    metric_legend_right, p2b,
    ncol = 1, rel_heights = c(.080, .920),
    align = "v", axis = "l", greedy = TRUE
  )
  right_column <- cowplot::plot_grid(
    right_top, p2c,
    ncol = 1, rel_heights = c(.65, .35),
    align = "v", axis = "l", greedy = TRUE
  )
  final <- cowplot::plot_grid(
    p2a, right_column,
    ncol = 2, rel_widths = c(.60, .40),
    align = "hv", axis = "tb", greedy = TRUE
  )

  list(plot = final, p2a = p2a, p2b = p2b, p2c = p2c,
       top_recoverable = positive)
}
