# Final display refinement for Fig. 1a.
#
# This helper reuses only the already-computed RQ1 display summaries created by
# scripts/11_plot_fig1.R. It does not refit models, filter metrics, or redefine
# any RQ1 estimand. The former nested marginal-quantile lozenges are replaced by
# a direct cross grammar that matches the available statistics:
#   - raw points: individual metric representations;
#   - coloured crosses: metric-class marginal IQRs;
#   - graphite crosses: dimension-level 10–90% ranges plus IQRs.
#
# Both coordinates use a zero-preserving pseudo-log display transform. This
# expands the crowded near-zero region while compressing large values smoothly;
# tick labels remain in the original A / rank-loss units.

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

  # A low positive quantile defines the approximately-linear neighbourhood.
  # The floor prevents a single near-zero numerical value from making the
  # transformation excessively aggressive.
  sigma <- as.numeric(stats::quantile(
    positive, .15, na.rm = TRUE, names = FALSE, type = 8
  ))
  sigma <- max(sigma, raw_max * .004, .Machine$double.eps)

  mapper <- function(x) {
    x <- suppressWarnings(as.numeric(x))
    out <- rep(NA_real_, length(x))
    ok <- is.finite(x)
    out[ok] <- asinh(pmax(x[ok], 0) / sigma)
    out
  }

  mapped_max <- mapper(raw_max)
  display_max <- mapped_max * 1.035

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
    "theme_fig1"
  )
  objects <- lapply(required, function(nm) ms_fig1_env_get(env, nm))
  names(objects) <- required
  if (any(vapply(objects, is.null, logical(1)))) return(NULL)

  metric_raw <- objects$dimension_metric_a
  cls_raw <- objects$class_summary_a
  overall_raw <- objects$dimension_overall_a
  theme_fig1_fn <- objects$theme_fig1
  if (!nrow(metric_raw) || !nrow(cls_raw) || !nrow(overall_raw)) return(NULL)

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
      label_dx = dplyr::case_when(
        dimension_chr == "Temporal resolution" ~ .018,
        dimension_chr == "Optical representation" ~ .018,
        dimension_chr == "Monitoring duration" ~ .018,
        TRUE ~ .018
      ),
      label_dy = dplyr::case_when(
        dimension_chr == "Temporal resolution" ~ .036,
        dimension_chr == "Optical representation" ~ .042,
        dimension_chr == "Monitoring duration" ~ .040,
        TRUE ~ .040
      ),
      x_label = A_median_plot + label_dx * x_axis$display_max,
      y_label = rank_loss_median_plot + label_dy * y_axis$display_max
    )

  p <- ggplot2::ggplot() +
    # Individual representations: tertiary texture only.
    ggplot2::geom_point(
      data = metric,
      ggplot2::aes(A_plot, rank_loss_plot, colour = metric_class),
      size = .38, alpha = .11, shape = 16
    ) +

    # Metric-class summaries: marginal IQR cross.
    ggplot2::geom_segment(
      data = cls,
      ggplot2::aes(
        x = A_q25_plot, xend = A_q75_plot,
        y = rank_loss_median_plot, yend = rank_loss_median_plot,
        colour = metric_class
      ),
      linewidth = .46, alpha = .70, lineend = "round"
    ) +
    ggplot2::geom_segment(
      data = cls,
      ggplot2::aes(
        x = A_median_plot, xend = A_median_plot,
        y = rank_loss_q25_plot, yend = rank_loss_q75_plot,
        colour = metric_class
      ),
      linewidth = .46, alpha = .70, lineend = "round"
    ) +
    ggplot2::geom_point(
      data = cls,
      ggplot2::aes(A_median_plot, rank_loss_median_plot, colour = metric_class),
      size = .98, alpha = .98, shape = 16
    ) +

    # Dimension summaries: thin 10–90% cross, thick IQR cross, strong centre.
    ggplot2::geom_segment(
      data = overall,
      ggplot2::aes(
        x = A_q10_plot, xend = A_q90_plot,
        y = rank_loss_median_plot, yend = rank_loss_median_plot
      ),
      inherit.aes = FALSE, colour = "#5E666A", linewidth = .36,
      alpha = .46, lineend = "round"
    ) +
    ggplot2::geom_segment(
      data = overall,
      ggplot2::aes(
        x = A_median_plot, xend = A_median_plot,
        y = rank_loss_q10_plot, yend = rank_loss_q90_plot
      ),
      inherit.aes = FALSE, colour = "#5E666A", linewidth = .36,
      alpha = .46, lineend = "round"
    ) +
    ggplot2::geom_segment(
      data = overall,
      ggplot2::aes(
        x = A_q25_plot, xend = A_q75_plot,
        y = rank_loss_median_plot, yend = rank_loss_median_plot
      ),
      inherit.aes = FALSE, colour = "#252B2E", linewidth = .88,
      alpha = .94, lineend = "round"
    ) +
    ggplot2::geom_segment(
      data = overall,
      ggplot2::aes(
        x = A_median_plot, xend = A_median_plot,
        y = rank_loss_q25_plot, yend = rank_loss_q75_plot
      ),
      inherit.aes = FALSE, colour = "#252B2E", linewidth = .88,
      alpha = .94, lineend = "round"
    ) +
    ggplot2::geom_point(
      data = overall,
      ggplot2::aes(A_median_plot, rank_loss_median_plot),
      inherit.aes = FALSE, shape = 23, size = 2.12,
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
      breaks = x_axis$map(x_axis$breaks), labels = x_axis$labels,
      expand = ggplot2::expansion(mult = c(0, .012))
    ) +
    ggplot2::scale_y_continuous(
      limits = c(0, y_axis$display_max),
      breaks = y_axis$map(y_axis$breaks), labels = y_axis$labels,
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
    "zero-preserving pseudo-log axes · points = individual representations · ",
    "coloured crosses = class IQR · graphite crosses = dimension 10–90% / IQR"
  )

  list(p1a_core = p, assoc_text = subtitle)
}
