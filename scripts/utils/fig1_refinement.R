# Final display refinement for Fig. 1.
#
# This helper reuses only already-computed RQ1 display summaries created by
# scripts/11_plot_fig1.R. It does not refit models, filter metrics, or redefine
# any RQ1 estimand. Panel a uses a direct marginal-cross grammar; panel b keeps
# the frozen target-aligned geometry but removes automatic extreme-metric labels
# that compete with the data cloud.

ms_fig1_env_get <- function(env, name, default = NULL) {
  if (is.environment(env) && exists(name, envir = env, inherits = FALSE)) {
    get(name, envir = env, inherits = FALSE)
  } else {
    default
  }
}

ms_fig1_make_pseudolog_axis <- function(values, n_breaks = 6L) {
  values <- suppressWarnings(as.numeric(values))
  values <- values[is.finite(values) & values >= 0]
  if (!length(values)) {
    return(list(
      sigma = 1, raw_max = 1, display_max = 1,
      breaks = c(0, 1), labels = c("0", "1"),
      map = function(x) suppressWarnings(as.numeric(x))
    ))
  }

  raw_max <- max(values)
  positive <- values[values > 0]
  if (!length(positive) || raw_max <= 0) {
    return(list(
      sigma = 1, raw_max = raw_max, display_max = 1,
      breaks = 0, labels = "0",
      map = function(x) rep(0, length(x))
    ))
  }

  # Use a low positive quantile as the approximately linear neighbourhood.
  # Compared with the previous 15th-percentile setting, the 10th percentile
  # expands the crowded near-zero field a little more while still preserving 0.
  sigma <- as.numeric(stats::quantile(
    positive, .10, na.rm = TRUE, names = FALSE, type = 8
  ))
  sigma <- max(sigma, raw_max * .003, .Machine$double.eps)

  mapper <- function(x) {
    x <- suppressWarnings(as.numeric(x))
    out <- rep(NA_real_, length(x))
    ok <- is.finite(x)
    out[ok] <- asinh(pmax(x[ok], 0) / sigma)
    out
  }

  mapped_max <- mapper(raw_max)
  display_max <- mapped_max * 1.030

  # Build log-like nice raw-unit candidates, then retain a compact subset that
  # is approximately evenly spaced after transformation. Zero is always shown.
  lo_exp <- floor(log10(min(positive))) - 1L
  hi_exp <- ceiling(log10(raw_max)) + 1L
  exponents <- seq.int(lo_exp, hi_exp)
  candidates <- sort(unique(c(
    0,
    as.vector(outer(c(1, 2, 5), 10 ^ exponents))
  )))
  candidates <- candidates[
    is.finite(candidates) & candidates >= 0 & candidates <= raw_max * 1.000001
  ]
  if (length(candidates) < 3L) {
    candidates <- sort(unique(c(0, scales::breaks_extended(n = n_breaks)(c(0, raw_max)))))
    candidates <- candidates[
      is.finite(candidates) & candidates >= 0 & candidates <= raw_max * 1.000001
    ]
  }

  mapped_candidates <- mapper(candidates)
  targets <- seq(0, mapped_max, length.out = max(3L, as.integer(n_breaks)))
  keep <- unique(vapply(
    targets,
    function(z) which.min(abs(mapped_candidates - z)),
    integer(1)
  ))
  breaks <- candidates[sort(keep)]
  breaks <- sort(unique(c(0, breaks)))

  format_raw <- function(x) {
    vapply(x, function(v) {
      if (!is.finite(v)) return("")
      if (abs(v) < .Machine$double.eps) return("0")
      if (v < .01) {
        txt <- sprintf("%.3f", v)
      } else if (v < .1) {
        txt <- sprintf("%.2f", v)
      } else if (v < 1) {
        txt <- sprintf("%.1f", v)
      } else {
        txt <- sprintf("%.1f", v)
      }
      sub("\\.?0+$", "", txt)
    }, character(1))
  }

  list(
    sigma = sigma,
    raw_max = raw_max,
    display_max = display_max,
    breaks = breaks,
    labels = format_raw(breaks),
    map = mapper
  )
}

ms_fig1_refine_main <- function(env) {
  required <- c(
    "dimension_metric_a", "class_summary_a", "dimension_overall_a",
    "target_geometry", "theme_fig1"
  )
  objects <- lapply(required, function(nm) ms_fig1_env_get(env, nm))
  names(objects) <- required
  if (any(vapply(objects, is.null, logical(1)))) return(NULL)

  metric_raw <- objects$dimension_metric_a
  cls_raw <- objects$class_summary_a
  overall_raw <- objects$dimension_overall_a
  target_geometry <- objects$target_geometry
  theme_fig1_fn <- objects$theme_fig1
  if (!nrow(metric_raw) || !nrow(cls_raw) || !nrow(overall_raw) || !nrow(target_geometry)) {
    return(NULL)
  }

  # ---------------------------------------------------------------------------
  # a. Absolute and relational preservation
  # ---------------------------------------------------------------------------
  x_axis <- ms_fig1_make_pseudolog_axis(metric_raw$A_typical, n_breaks = 6L)
  y_axis <- ms_fig1_make_pseudolog_axis(metric_raw$rank_loss_typical, n_breaks = 6L)

  metric <- metric_raw |>
    dplyr::mutate(
      A_plot = x_axis$map(A_typical),
      rank_loss_plot = y_axis$map(rank_loss_typical)
    )

  cls <- cls_raw |>
    dplyr::mutate(
      A_q25_plot = x_axis$map(A_q25),
      A_median_plot = x_axis$map(A_median),
      A_q75_plot = x_axis$map(A_q75),
      rank_loss_q25_plot = y_axis$map(rank_loss_q25),
      rank_loss_median_plot = y_axis$map(rank_loss_median),
      rank_loss_q75_plot = y_axis$map(rank_loss_q75)
    )

  overall <- overall_raw |>
    dplyr::mutate(
      A_q10_plot = x_axis$map(A_q10),
      A_q25_plot = x_axis$map(A_q25),
      A_median_plot = x_axis$map(A_median),
      A_q75_plot = x_axis$map(A_q75),
      A_q90_plot = x_axis$map(A_q90),
      rank_loss_q10_plot = y_axis$map(rank_loss_q10),
      rank_loss_q25_plot = y_axis$map(rank_loss_q25),
      rank_loss_median_plot = y_axis$map(rank_loss_median),
      rank_loss_q75_plot = y_axis$map(rank_loss_q75),
      rank_loss_q90_plot = y_axis$map(rank_loss_q90),
      dimension_chr = as.character(dimension),
      short_label = dplyr::recode(
        dimension_chr,
        "Optical representation" = "Optical",
        "Temporal resolution" = "Temporal",
        "Monitoring duration" = "Duration",
        .default = dimension_chr
      ),
      label_dx = .018,
      label_dy = dplyr::case_when(
        dimension_chr == "Temporal resolution" ~ .034,
        dimension_chr == "Optical representation" ~ .040,
        dimension_chr == "Monitoring duration" ~ .038,
        TRUE ~ .038
      ),
      x_label = A_median_plot + label_dx * x_axis$display_max,
      y_label = rank_loss_median_plot + label_dy * y_axis$display_max
    )

  p1a_core <- ggplot2::ggplot() +
    # Individual representations are tertiary texture only.
    ggplot2::geom_point(
      data = metric,
      ggplot2::aes(A_plot, rank_loss_plot, colour = metric_class),
      size = .34, alpha = .080, shape = 16
    ) +

    # Metric-class summaries stay visible on close inspection but no longer
    # compete with the dimension-level graphite backbone.
    ggplot2::geom_segment(
      data = cls,
      ggplot2::aes(
        x = A_q25_plot, xend = A_q75_plot,
        y = rank_loss_median_plot, yend = rank_loss_median_plot,
        colour = metric_class
      ),
      linewidth = .34, alpha = .50, lineend = "round"
    ) +
    ggplot2::geom_segment(
      data = cls,
      ggplot2::aes(
        x = A_median_plot, xend = A_median_plot,
        y = rank_loss_q25_plot, yend = rank_loss_q75_plot,
        colour = metric_class
      ),
      linewidth = .34, alpha = .50, lineend = "round"
    ) +
    ggplot2::geom_point(
      data = cls,
      ggplot2::aes(A_median_plot, rank_loss_median_plot, colour = metric_class),
      size = .86, alpha = .94, shape = 16
    ) +

    # Dimension summaries are the primary visual layer.
    ggplot2::geom_segment(
      data = overall,
      ggplot2::aes(
        x = A_q10_plot, xend = A_q90_plot,
        y = rank_loss_median_plot, yend = rank_loss_median_plot
      ),
      inherit.aes = FALSE, colour = "#5E666A", linewidth = .34,
      alpha = .44, lineend = "round"
    ) +
    ggplot2::geom_segment(
      data = overall,
      ggplot2::aes(
        x = A_median_plot, xend = A_median_plot,
        y = rank_loss_q10_plot, yend = rank_loss_q90_plot
      ),
      inherit.aes = FALSE, colour = "#5E666A", linewidth = .34,
      alpha = .44, lineend = "round"
    ) +
    ggplot2::geom_segment(
      data = overall,
      ggplot2::aes(
        x = A_q25_plot, xend = A_q75_plot,
        y = rank_loss_median_plot, yend = rank_loss_median_plot
      ),
      inherit.aes = FALSE, colour = "#252B2E", linewidth = .90,
      alpha = .96, lineend = "round"
    ) +
    ggplot2::geom_segment(
      data = overall,
      ggplot2::aes(
        x = A_median_plot, xend = A_median_plot,
        y = rank_loss_q25_plot, yend = rank_loss_q75_plot
      ),
      inherit.aes = FALSE, colour = "#252B2E", linewidth = .90,
      alpha = .96, lineend = "round"
    ) +
    ggplot2::geom_point(
      data = overall,
      ggplot2::aes(A_median_plot, rank_loss_median_plot),
      inherit.aes = FALSE, shape = 23, size = 2.08,
      fill = "#252B2E", colour = "white", stroke = .30
    ) +
    ggplot2::geom_text(
      data = overall,
      ggplot2::aes(x_label, y_label, label = short_label),
      inherit.aes = FALSE, family = MS_FONT, fontface = "bold",
      size = 1.68, colour = "#252B2E", hjust = 0, vjust = 0
    ) +

    scale_color_ms_metric(guide = "none") +
    ggplot2::scale_x_continuous(
      limits = c(0, x_axis$display_max),
      breaks = x_axis$map(x_axis$breaks), labels = x_axis$labels,
      expand = ggplot2::expansion(mult = c(0, .010))
    ) +
    ggplot2::scale_y_continuous(
      limits = c(0, y_axis$display_max),
      breaks = y_axis$map(y_axis$breaks), labels = y_axis$labels,
      expand = ggplot2::expansion(mult = c(0, .015))
    ) +
    ggplot2::labs(
      x = "Absolute distortion, A",
      y = "Rank loss, 1 − Spearman ρ"
    ) +
    theme_fig1_fn(base_size = 6.70) +
    ggplot2::theme(
      panel.grid.major = ggplot2::element_line(colour = "#F0F2F3", linewidth = .18),
      panel.grid.minor = ggplot2::element_blank(),
      axis.title = ggplot2::element_text(size = 5.15),
      axis.text = ggplot2::element_text(size = 4.35),
      plot.margin = ggplot2::margin(.8, 2.0, 1.2, 2.0)
    )

  assoc_text <- "Crosses show marginal ranges across representations; axes use zero-preserving pseudo-log scaling."

  # ---------------------------------------------------------------------------
  # b. Target-aligned magnitude and directional coherence
  # ---------------------------------------------------------------------------
  # The old automatic labels selected the maximum-A and maximum-|B/A| metric in
  # each facet. They often landed against plot boundaries or produced long raw
  # metric strings. The frozen data are unchanged; the main panel now lets the
  # distribution itself carry the message and leaves metric identities to audit
  # outputs / supplementary views.
  target_geometry_panel <- function(panel_name, show_x_title = TRUE) {
    d <- target_geometry |> dplyr::filter(facet_label == panel_name)
    if (!nrow(d)) stop("No target geometry rows for panel: ", panel_name)
    y_lim <- dplyr::first(d$y_limit)
    panel_title <- dplyr::case_when(
      panel_name == "Placement · chest/wrist → eye" ~ "Placement ·\nchest/wrist → eye",
      panel_name == "Optical representation · LIGHT → MEDI" ~ "Optical representation ·\nLIGHT → MEDI",
      TRUE ~ panel_name
    )

    ggplot2::ggplot() +
      ggplot2::geom_vline(xintercept = 0, linewidth = .28, colour = "#A8ADB0") +
      ggplot2::geom_point(
        data = d |> dplyr::filter(!offscale),
        ggplot2::aes(coherence, A_display, colour = metric_class, shape = transition),
        size = 1.24, alpha = .80
      ) +
      ggplot2::geom_point(
        data = d |> dplyr::filter(offscale),
        ggplot2::aes(coherence, A_display),
        inherit.aes = FALSE, shape = 4, size = 1.42, stroke = .40,
        colour = "#303437"
      ) +
      scale_color_ms_metric(guide = "none") +
      ggplot2::scale_shape_discrete(name = NULL) +
      ggplot2::scale_x_continuous(
        limits = c(-1, 1),
        breaks = c(-1, -.5, 0, .5, 1),
        expand = ggplot2::expansion(mult = c(.012, .012))
      ) +
      ggplot2::scale_y_continuous(
        limits = c(0, y_lim),
        breaks = scales::breaks_extended(n = 4),
        expand = ggplot2::expansion(mult = c(0, .025))
      ) +
      ggplot2::labs(
        title = panel_title,
        x = if (show_x_title) "Directional coherence, B/A" else NULL,
        y = NULL
      ) +
      theme_fig1_fn(base_size = 7.0) +
      ggplot2::theme(
        panel.grid.major = ggplot2::element_blank(),
        plot.title = ggplot2::element_text(
          size = 5.75, lineheight = .90, face = "bold", hjust = .5,
          margin = ggplot2::margin(b = 1)
        ),
        axis.text.x = ggplot2::element_text(size = 5.0),
        plot.margin = ggplot2::margin(1.5, 2.5, 1.5, 2.5)
      )
  }

  p1b_top <- target_geometry_panel("Placement · chest/wrist → eye", show_x_title = FALSE)
  p1b_bottom <- target_geometry_panel("Optical representation · LIGHT → MEDI", show_x_title = TRUE)
  p1b_core <- cowplot::plot_grid(
    p1b_top, p1b_bottom,
    ncol = 1, rel_heights = c(1, 1),
    align = "v", axis = "lr", greedy = TRUE
  )

  p1b_shape_legend <- cowplot::get_legend(
    ggplot2::ggplot(
      target_geometry |> dplyr::filter(!offscale),
      ggplot2::aes(coherence, A_display, shape = transition)
    ) +
      ggplot2::geom_point(size = 1.45, colour = "#3B3B3B") +
      ggplot2::scale_shape_discrete(name = NULL) +
      ggplot2::theme_void(base_family = MS_FONT) +
      ggplot2::theme(
        legend.position = "bottom",
        legend.text = ggplot2::element_text(size = 4.10),
        legend.key.width = grid::unit(2.4, "mm"),
        legend.spacing.x = grid::unit(.32, "mm"),
        legend.margin = ggplot2::margin(0, 0, 0, 0)
      )
  )

  list(
    p1a_core = p1a_core,
    assoc_text = assoc_text,
    p1b_core = p1b_core,
    p1b_shape_legend = p1b_shape_legend
  )
}
