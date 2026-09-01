# Final display refinement for Fig. 1a.
#
# This helper reuses only the already-computed RQ1 display summaries created by
# scripts/11_plot_fig1.R. It does not refit models, filter metrics, or redefine
# any RQ1 estimand. The former nested marginal-quantile lozenges are replaced by
# a direct cross grammar that matches the available statistics:
#   - raw points: individual metric representations;
#   - coloured crosses: metric-class marginal IQRs;
#   - graphite crosses: dimension-level 10–90% ranges plus IQRs.

ms_fig1_env_get <- function(env, name, default = NULL) {
  if (is.environment(env) && exists(name, envir = env, inherits = FALSE)) {
    get(name, envir = env, inherits = FALSE)
  } else {
    default
  }
}

ms_fig1_refine_main <- function(env) {
  required <- c(
    "dimension_metric_plot_a", "class_summary_plot_a", "dimension_overall_plot_a",
    "a_x_axis", "a_y_axis", "a_x_breaks_raw", "a_y_breaks_raw",
    "a_x_labels", "a_y_labels", "theme_fig1"
  )
  objects <- lapply(required, function(nm) ms_fig1_env_get(env, nm))
  names(objects) <- required
  if (any(vapply(objects, is.null, logical(1)))) return(NULL)

  metric <- objects$dimension_metric_plot_a
  cls <- objects$class_summary_plot_a
  overall <- objects$dimension_overall_plot_a
  x_axis <- objects$a_x_axis
  y_axis <- objects$a_y_axis
  theme_fig1_fn <- objects$theme_fig1

  if (!nrow(metric) || !nrow(cls) || !nrow(overall)) return(NULL)

  overall <- overall |>
    dplyr::mutate(
      dimension_chr = as.character(dimension),
      short_label = dplyr::recode(
        dimension_chr,
        "Optical representation" = "Optical",
        "Temporal resolution" = "Temporal",
        "Monitoring duration" = "Duration",
        .default = dimension_chr
      ),
      # Data-fixed offsets keep the four dimension labels close to their anchors
      # without implying a trajectory between dimensions.
      label_dx = dplyr::case_when(
        dimension_chr == "Temporal resolution" ~ .020,
        dimension_chr == "Optical representation" ~ .018,
        dimension_chr == "Monitoring duration" ~ .018,
        TRUE ~ .018
      ),
      label_dy = dplyr::case_when(
        dimension_chr == "Temporal resolution" ~ .040,
        dimension_chr == "Optical representation" ~ .050,
        dimension_chr == "Monitoring duration" ~ .045,
        TRUE ~ .045
      ),
      x_label = A_median_plot + label_dx * x_axis$display_max,
      y_label = rank_loss_median_plot + label_dy * y_axis$display_max
    )

  p <- ggplot2::ggplot() +
    # Compression boundaries remain visible but deliberately subordinate; the
    # old shaded gutter is removed so the display device is not a focal object.
    {if (isTRUE(x_axis$use_tail)) ggplot2::geom_vline(
      xintercept = x_axis$focus, colour = "#C6CACC",
      linewidth = .24, linetype = "22", alpha = .90
    ) else NULL} +
    {if (isTRUE(y_axis$use_tail)) ggplot2::geom_hline(
      yintercept = y_axis$focus, colour = "#C6CACC",
      linewidth = .24, linetype = "22", alpha = .90
    ) else NULL} +

    # Individual representations: tertiary texture only.
    ggplot2::geom_point(
      data = metric,
      ggplot2::aes(A_plot, rank_loss_plot, colour = metric_class),
      size = .38, alpha = .10, shape = 16
    ) +

    # Metric-class summaries: the marginal IQR in each coordinate is shown
    # directly as an orthogonal cross rather than an artificial polygon.
    ggplot2::geom_segment(
      data = cls,
      ggplot2::aes(
        x = A_q25_plot, xend = A_q75_plot,
        y = rank_loss_median_plot, yend = rank_loss_median_plot,
        colour = metric_class
      ),
      linewidth = .48, alpha = .72, lineend = "round"
    ) +
    ggplot2::geom_segment(
      data = cls,
      ggplot2::aes(
        x = A_median_plot, xend = A_median_plot,
        y = rank_loss_q25_plot, yend = rank_loss_q75_plot,
        colour = metric_class
      ),
      linewidth = .48, alpha = .72, lineend = "round"
    ) +
    ggplot2::geom_point(
      data = cls,
      ggplot2::aes(A_median_plot, rank_loss_median_plot, colour = metric_class),
      size = 1.02, alpha = .98, shape = 16
    ) +

    # Dimension summaries: thin 10–90% cross, thick IQR cross, and a strong
    # centre marker. This is the first visual layer and establishes the global
    # measurement-sensitivity geometry at a glance.
    ggplot2::geom_segment(
      data = overall,
      ggplot2::aes(
        x = A_q10_plot, xend = A_q90_plot,
        y = rank_loss_median_plot, yend = rank_loss_median_plot
      ),
      inherit.aes = FALSE, colour = "#5E666A", linewidth = .38,
      alpha = .48, lineend = "round"
    ) +
    ggplot2::geom_segment(
      data = overall,
      ggplot2::aes(
        x = A_median_plot, xend = A_median_plot,
        y = rank_loss_q10_plot, yend = rank_loss_q90_plot
      ),
      inherit.aes = FALSE, colour = "#5E666A", linewidth = .38,
      alpha = .48, lineend = "round"
    ) +
    ggplot2::geom_segment(
      data = overall,
      ggplot2::aes(
        x = A_q25_plot, xend = A_q75_plot,
        y = rank_loss_median_plot, yend = rank_loss_median_plot
      ),
      inherit.aes = FALSE, colour = "#252B2E", linewidth = .92,
      alpha = .94, lineend = "round"
    ) +
    ggplot2::geom_segment(
      data = overall,
      ggplot2::aes(
        x = A_median_plot, xend = A_median_plot,
        y = rank_loss_q25_plot, yend = rank_loss_q75_plot
      ),
      inherit.aes = FALSE, colour = "#252B2E", linewidth = .92,
      alpha = .94, lineend = "round"
    ) +
    ggplot2::geom_point(
      data = overall,
      ggplot2::aes(A_median_plot, rank_loss_median_plot),
      inherit.aes = FALSE, shape = 23, size = 2.15,
      fill = "#252B2E", colour = "white", stroke = .30
    ) +
    ggplot2::geom_text(
      data = overall,
      ggplot2::aes(x_label, y_label, label = short_label),
      inherit.aes = FALSE, family = MS_FONT, fontface = "bold",
      size = 1.70, colour = "#252B2E", hjust = 0, vjust = 0
    ) +

    scale_color_ms_metric(guide = "none") +
    ggplot2::scale_x_continuous(
      limits = c(0, x_axis$display_max),
      breaks = x_axis$map(objects$a_x_breaks_raw), labels = objects$a_x_labels,
      expand = ggplot2::expansion(mult = c(0, .012))
    ) +
    ggplot2::scale_y_continuous(
      limits = c(0, y_axis$display_max),
      breaks = y_axis$map(objects$a_y_breaks_raw), labels = objects$a_y_labels,
      expand = ggplot2::expansion(mult = c(0, .018))
    ) +
    ggplot2::labs(
      x = "Absolute distortion, A",
      y = "Rank loss, 1 − Spearman ρ"
    ) +
    theme_fig1_fn(base_size = 6.75) +
    ggplot2::theme(
      panel.grid.major = ggplot2::element_line(colour = "#EFF1F2", linewidth = .20),
      panel.grid.minor = ggplot2::element_blank(),
      axis.title = ggplot2::element_text(size = 5.25),
      axis.text = ggplot2::element_text(size = 4.45),
      plot.margin = ggplot2::margin(1.0, 2.0, 1.5, 2.0)
    )

  subtitle <- paste0(
    "points = individual representations · coloured crosses = class IQR · ",
    "graphite crosses = dimension 10–90% / IQR",
    if (isTRUE(x_axis$use_tail) || isTRUE(y_axis$use_tail)) " · extreme tails compressed" else ""
  )

  list(p1a_core = p, assoc_text = subtitle)
}
