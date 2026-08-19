suppressPackageStartupMessages({
  library(tidyverse)
  library(cowplot)
})

# Fig. 1 only. This script reads frozen RQ1 outputs and never recomputes
# representation distortion, standardizers, bootstrap intervals, or duration windows.

DISTORTION_RDS <- "data/derived/rq1/rq1_distortion_long.rds"
SUMMARY_CSV <- "results/rq1/rq1_summary.csv"
MANIFEST_CSV <- "results/rq1/rq1_configuration_manifest.csv"
FIG_DIR <- "results/figures"

for (p in c(DISTORTION_RDS, SUMMARY_CSV, MANIFEST_CSV)) {
  if (!file.exists(p)) {
    stop("Missing RQ1 artifact: ", p, ". Run scripts/10_rq1_analysis.R first.")
  }
}
dir.create(FIG_DIR, recursive = TRUE, showWarnings = FALSE)

METRIC_CLASSES <- c(
  "duration", "exposure history", "level",
  "spectrum", "temporal dynamics", "timing"
)
DIMENSIONS <- c("placement", "optical", "temporal", "duration")
DIM_TITLES <- c(
  placement = "Placement",
  optical = "Optical proxy",
  temporal = "Temporal resolution",
  duration = "Monitoring duration"
)
PANEL_LETTERS <- matrix(letters[1:8], nrow = 2, byrow = TRUE)
TRAJECTORY_DIMS <- c("temporal", "duration")

# Equal-size plotting frames keep the 2 x 4 grammar stable. Configuration rows in
# the top panels are rescaled to a common vertical canvas below so dimensions with
# one, two, or many observed levels use comparable visual area.
base_panel_theme <- theme_minimal(base_size = 8.3) +
  theme(
    aspect.ratio = 1,
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(linewidth = .22, color = "grey92"),
    axis.text = element_text(color = "grey25"),
    axis.title = element_text(color = "grey20"),
    plot.title = element_text(face = "plain", size = 9.2, margin = margin(b = 4)),
    plot.margin = margin(4, 5, 4, 5),
    legend.position = "none"
  )

# A/B panels use one display transform across all four dimensions. Because the
# same monotone transform is applied to B and A, A = |B| remains an exact visual
# boundary while extreme metrics no longer flatten the rest of a panel.
asinh_display <- scales::transform_asinh()

dist <- readRDS(DISTORTION_RDS) |>
  filter(available, is.finite(e)) |>
  mutate(metric_class = factor(metric_class, levels = METRIC_CLASSES))

summary <- readr::read_csv(SUMMARY_CSV, show_col_types = FALSE) |>
  mutate(metric_class = factor(metric_class, levels = METRIC_CLASSES))

manifest <- readr::read_csv(MANIFEST_CSV, show_col_types = FALSE)

# Equal representation weighting: within each metric x configuration, all
# smallest units together carry weight 1. Thus a metric represented by many
# participant-days does not dominate a metric represented by fewer units.
density_input <- dist |>
  group_by(dimension, configuration, metric) |>
  mutate(unit_weight = 1 / n()) |>
  ungroup()

weighted_quantile <- function(x, w, probs = c(.05, .25, .50, .75, .95)) {
  ok <- is.finite(x) & is.finite(w) & w > 0
  x <- x[ok]
  w <- w[ok]
  if (!length(x)) return(rep(NA_real_, length(probs)))
  ord <- order(x)
  x <- x[ord]
  w <- w[ord]
  w <- w / sum(w)
  cw <- cumsum(w)
  vapply(probs, function(p) x[which(cw >= p)[1]], numeric(1))
}

weighted_density <- function(x, w, n = 512L) {
  ok <- is.finite(x) & is.finite(w) & w > 0
  x <- x[ok]
  w <- w[ok]
  if (length(x) < 2L || length(unique(x)) < 2L) {
    return(tibble(x = if (length(x)) x[1] else 0, density = 0))
  }
  w <- w / sum(w)
  d <- stats::density(x, weights = w, n = n, na.rm = TRUE)
  tibble(x = d$x, density = d$y)
}

# The old full-range ridges were visually dominated by a handful of extreme e
# values and collapsed into near-vertical needles. Fig. 1 now shows the central
# 90% of each configuration distribution. q5-q95, IQR, and median are retained
# explicitly, so the display window is auditable rather than an arbitrary zoom.
ridge_stats <- density_input |>
  group_by(dimension, configuration, configuration_label, configuration_order) |>
  group_modify(~{
    q <- weighted_quantile(.x$e, .x$unit_weight)
    tibble(q05 = q[1], q25 = q[2], q50 = q[3], q75 = q[4], q95 = q[5])
  }) |>
  ungroup()

readr::write_csv(
  ridge_stats,
  "results/rq1/fig1_distribution_quantiles.csv",
  na = ""
)

# Dimension-specific robust windows preserve genuine directional asymmetry while
# guaranteeing that zero and every configuration's q5-q95 interval remain visible.
top_limits <- ridge_stats |>
  group_by(dimension) |>
  summarise(
    xmin0 = min(c(0, q05), na.rm = TRUE),
    xmax0 = max(c(0, q95), na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    span = xmax0 - xmin0,
    span = if_else(is.finite(span) & span > 0, span, 1),
    xmin = xmin0 - .045 * span,
    xmax = xmax0 + .045 * span
  )

ridge_data <- density_input |>
  group_by(dimension, configuration, configuration_label, configuration_order) |>
  group_modify(~weighted_density(.x$e, .x$unit_weight)) |>
  ungroup() |>
  left_join(
    ridge_stats,
    by = c("dimension", "configuration", "configuration_label", "configuration_order")
  ) |>
  filter(x >= q05, x <= q95) |>
  group_by(dimension, configuration) |>
  mutate(
    density_scaled = if (any(is.finite(density)) && max(density, na.rm = TRUE) > 0) {
      density / max(density, na.rm = TRUE)
    } else 0
  ) |>
  ungroup()

readr::write_csv(
  ridge_data,
  "results/rq1/fig1_pooled_distribution_density.csv",
  na = ""
)

configuration_layout <- function(labels) {
  n_cfg <- nrow(labels)
  if (!n_cfg) return(labels)
  if (n_cfg == 1L) {
    return(labels |> mutate(y0 = .34, ridge_height = .34))
  }
  y0 <- seq(.10, .76, length.out = n_cfg)
  gap <- min(diff(y0))
  labels |>
    mutate(y0 = y0, ridge_height = min(.24, .72 * gap))
}

top_panel <- function(dim, letter) {
  labels <- manifest |>
    filter(dimension == dim) |>
    distinct(configuration, configuration_label, configuration_order) |>
    arrange(configuration_order) |>
    configuration_layout()
  if (!nrow(labels)) stop("No configuration manifest rows for Fig. 1 dimension: ", dim)

  d <- ridge_data |>
    filter(dimension == dim) |>
    left_join(
      labels |> select(configuration, y0, ridge_height),
      by = "configuration"
    ) |>
    mutate(y1 = y0 + ridge_height * density_scaled)

  qs <- ridge_stats |>
    filter(dimension == dim) |>
    left_join(labels |> select(configuration, y0), by = "configuration")

  lim <- top_limits |> filter(dimension == dim)
  if (nrow(lim) != 1L) stop("Missing top-row display limits for dimension: ", dim)

  ggplot(d, aes(x = x, group = configuration)) +
    geom_vline(xintercept = 0, linewidth = .32, color = "grey45") +
    geom_ribbon(aes(ymin = y0, ymax = y1), fill = "grey82", alpha = .80) +
    geom_line(aes(y = y1), linewidth = .42, color = "grey30") +
    geom_segment(
      data = qs,
      aes(x = q05, xend = q95, y = y0, yend = y0),
      inherit.aes = FALSE,
      linewidth = .55, color = "grey52", lineend = "round"
    ) +
    geom_segment(
      data = qs,
      aes(x = q25, xend = q75, y = y0, yend = y0),
      inherit.aes = FALSE,
      linewidth = 1.45, color = "grey22", lineend = "round"
    ) +
    geom_point(
      data = qs,
      aes(x = q50, y = y0),
      inherit.aes = FALSE,
      shape = 21, fill = "white", color = "grey15",
      size = 1.18, stroke = .38
    ) +
    scale_x_continuous(
      breaks = scales::breaks_extended(n = 4),
      expand = expansion(mult = c(0, 0))
    ) +
    scale_y_continuous(
      breaks = labels$y0,
      labels = labels$configuration_label,
      limits = c(.02, 1.02),
      expand = c(0, 0)
    ) +
    coord_cartesian(
      xlim = c(lim$xmin, lim$xmax),
      ylim = c(.02, 1.02),
      clip = "off"
    ) +
    labs(
      title = paste0(letter, "  ", DIM_TITLES[[dim]]),
      x = "standardized signed distortion, e",
      y = NULL
    ) +
    base_panel_theme +
    theme(
      panel.grid.major.y = element_blank(),
      axis.text.y = element_text(size = 6.9),
      axis.ticks.y = element_blank()
    )
}

extreme_metric_rows <- function(d, n = 3L) {
  d |>
    group_by(metric) |>
    slice_max(A_mean_absolute, n = 1, with_ties = FALSE) |>
    ungroup() |>
    slice_max(A_mean_absolute, n = n, with_ties = FALSE)
}

last_observed_configuration <- function(d) {
  d |>
    group_by(metric) |>
    slice_max(configuration_order, n = 1, with_ties = FALSE) |>
    ungroup()
}

bottom_panel <- function(dim, letter) {
  d <- summary |>
    filter(
      dimension == dim,
      is.finite(A_mean_absolute),
      is.finite(B_mean_signed)
    ) |>
    arrange(metric, configuration_order)
  if (!nrow(d)) stop("No summary rows for Fig. 1 dimension: ", dim)

  if (dim %in% TRAJECTORY_DIMS) {
    endpoint_d <- last_observed_configuration(d)
    label_data <- endpoint_d |>
      slice_max(A_mean_absolute, n = 3L, with_ties = FALSE)
  } else {
    endpoint_d <- tibble()
    label_data <- extreme_metric_rows(d, n = 3L)
  }

  display_values <- c(
    d$A_mean_absolute,
    abs(d$B_mean_signed),
    d$A_ci_high,
    abs(d$B_ci_low),
    abs(d$B_ci_high)
  )
  display_values <- display_values[is.finite(display_values)]
  lim <- if (length(display_values)) max(display_values) else 1
  lim <- ifelse(is.finite(lim) && lim > 0, lim * 1.06, 1)

  p <- ggplot(d, aes(B_mean_signed, A_mean_absolute, color = metric_class)) +
    geom_vline(xintercept = 0, linewidth = .26, color = "grey82") +
    geom_abline(
      slope = c(-1, 1), intercept = 0,
      linetype = 2, linewidth = .34, color = "grey62"
    )

  if (dim %in% TRAJECTORY_DIMS) {
    # Ordered dimensions are trajectories. Thin class-colored paths preserve the
    # progression without making the intermediate points dominate the endpoint.
    p <- p +
      geom_path(
        aes(group = metric),
        alpha = .32, linewidth = .40,
        lineend = "round", linejoin = "round"
      ) +
      geom_point(size = .92, alpha = .58) +
      geom_point(
        data = endpoint_d,
        shape = 21, fill = "white",
        size = 2.05, stroke = .65, alpha = .98,
        show.legend = FALSE
      )
  } else if (dim == "placement") {
    ci_d <- d |> filter(bootstrap_supported)
    p <- p +
      geom_errorbar(
        data = ci_d,
        aes(ymin = A_ci_low, ymax = A_ci_high),
        alpha = .13, linewidth = .24
      ) +
      geom_errorbar(
        data = ci_d,
        aes(xmin = B_ci_low, xmax = B_ci_high),
        orientation = "y",
        alpha = .13, linewidth = .24
      ) +
      geom_point(
        aes(shape = configuration_label),
        size = 1.48, alpha = .82
      ) +
      scale_shape_manual(values = c("Chest" = 16, "Wrist" = 17))
  } else {
    ci_d <- d |> filter(bootstrap_supported)
    p <- p +
      geom_errorbar(
        data = ci_d,
        aes(ymin = A_ci_low, ymax = A_ci_high),
        alpha = .13, linewidth = .24
      ) +
      geom_errorbar(
        data = ci_d,
        aes(xmin = B_ci_low, xmax = B_ci_high),
        orientation = "y",
        alpha = .13, linewidth = .24
      ) +
      geom_point(size = 1.48, alpha = .82)
  }

  p +
    geom_text(
      data = label_data,
      aes(
        x = B_mean_signed,
        y = A_mean_absolute,
        label = metric
      ),
      inherit.aes = FALSE,
      size = 1.95, color = "grey18",
      check_overlap = TRUE, vjust = -0.68
    ) +
    scale_color_discrete(drop = FALSE) +
    scale_x_continuous(
      transform = asinh_display,
      limits = c(-lim, lim),
      breaks = scales::breaks_extended(n = 4),
      expand = expansion(mult = .025)
    ) +
    scale_y_continuous(
      transform = asinh_display,
      limits = c(0, lim),
      breaks = scales::breaks_extended(n = 4),
      expand = expansion(mult = .025)
    ) +
    labs(
      title = paste0(letter, "  ", DIM_TITLES[[dim]]),
      x = "B: mean signed distortion",
      y = "A: mean absolute distortion"
    ) +
    base_panel_theme
}

# Global legends only. No individual panel surrenders plotting area to a guide.
metric_legend_source <- ggplot(
  tibble(
    metric_class = factor(METRIC_CLASSES, levels = METRIC_CLASSES),
    x = seq_along(METRIC_CLASSES),
    y = 1
  ),
  aes(x, y, color = metric_class)
) +
  geom_point(size = 2) +
  scale_color_discrete(drop = FALSE) +
  guides(
    color = guide_legend(
      title = "metric class", nrow = 1, byrow = TRUE,
      override.aes = list(size = 2, alpha = 1)
    )
  ) +
  theme_void(base_size = 8) +
  theme(legend.position = "bottom")
metric_legend <- cowplot::get_legend(metric_legend_source)

placement_legend_source <- ggplot(
  tibble(
    configuration = factor(c("Chest", "Wrist"), levels = c("Chest", "Wrist")),
    x = 1:2,
    y = 1
  ),
  aes(x, y, shape = configuration)
) +
  geom_point(size = 2, color = "grey20") +
  scale_shape_manual(values = c("Chest" = 16, "Wrist" = 17)) +
  guides(shape = guide_legend(title = "placement", nrow = 1)) +
  theme_void(base_size = 8) +
  theme(legend.position = "bottom")
placement_legend <- cowplot::get_legend(placement_legend_source)

top <- map2(DIMENSIONS, PANEL_LETTERS[1, ], top_panel)
bottom <- map2(DIMENSIONS, PANEL_LETTERS[2, ], bottom_panel)

panel_grid <- cowplot::plot_grid(
  plotlist = c(top, bottom),
  ncol = 4,
  align = "hv",
  axis = "tblr",
  rel_widths = rep(1, 4),
  rel_heights = c(1, 1)
)

legend_row <- cowplot::plot_grid(
  metric_legend,
  placement_legend,
  nrow = 1,
  rel_widths = c(4.6, 1.4)
)

display_note <- cowplot::ggdraw() +
  cowplot::draw_label(
    "Top row: central 90% of each configuration distribution (q5-q95; thick bar = IQR; point = median).  Bottom row: common asinh display transform; underlying A and B values are unchanged.",
    x = .5, y = .5, hjust = .5, vjust = .5,
    size = 7.2, color = "grey38"
  )

fig1 <- cowplot::plot_grid(
  panel_grid,
  legend_row,
  display_note,
  ncol = 1,
  rel_heights = c(1, .072, .035)
)

ggsave(
  file.path(FIG_DIR, "Fig1_RQ1.pdf"),
  plot = fig1,
  width = 15.6, height = 8.65,
  units = "in", device = cairo_pdf
)

ggsave(
  file.path(FIG_DIR, "Fig1_RQ1.png"),
  plot = fig1,
  width = 15.6, height = 8.65,
  units = "in", dpi = 220
)

message("Fig. 1 complete:")
message("  ", file.path(FIG_DIR, "Fig1_RQ1.pdf"))
message("  ", file.path(FIG_DIR, "Fig1_RQ1.png"))
message("  top-row density display is restricted to configuration-specific q5-q95")
message("  all four A-B panels use the same asinh display transform; underlying RQ1 values are unchanged")
