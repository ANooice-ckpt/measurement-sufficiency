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

# Keep every plotting panel geometrically comparable. theme(aspect.ratio = 1)
# fixes the actual panel rectangle to a square; cowplot then aligns the eight
# outer grobs so axis/title differences do not shift the panel frames.
base_panel_theme <- theme_minimal(base_size = 8) +
  theme(
    aspect.ratio = 1,
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "plain", size = 9, margin = margin(b = 3)),
    plot.margin = margin(4, 5, 4, 5),
    legend.position = "none"
  )

# Only the two ordered dimensions need dynamic-range compression in the A-B
# panels. The inverse-hyperbolic-sine transform is monotone, defined through 0,
# and applied identically to x and y, so the A = |B| boundary remains exact.
asinh_display <- scales::transform_asinh()

dist <- readRDS(DISTORTION_RDS) |>
  filter(available, is.finite(e)) |>
  mutate(metric_class = factor(metric_class, levels = METRIC_CLASSES))

summary <- readr::read_csv(SUMMARY_CSV, show_col_types = FALSE) |>
  mutate(metric_class = factor(metric_class, levels = METRIC_CLASSES))

manifest <- readr::read_csv(MANIFEST_CSV, show_col_types = FALSE)

# Equal representation weighting: within each metric x configuration, all
# smallest units together carry weight 1. The top row is therefore a descriptive
# pooled distortion landscape across target representations, not class inference.
density_input <- dist |>
  group_by(dimension, configuration, metric) |>
  mutate(unit_weight = 1 / n()) |>
  ungroup()

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

ridge_data <- density_input |>
  group_by(dimension, configuration, configuration_label, configuration_order) |>
  group_modify(~weighted_density(.x$e, .x$unit_weight)) |>
  ungroup() |>
  group_by(dimension, configuration) |>
  mutate(
    density_scaled = if (max(density, na.rm = TRUE) > 0) {
      density / max(density, na.rm = TRUE)
    } else 0
  ) |>
  ungroup()

readr::write_csv(
  ridge_data,
  "results/rq1/fig1_pooled_distribution_density.csv",
  na = ""
)

top_panel <- function(dim, letter) {
  d <- ridge_data |>
    filter(dimension == dim) |>
    mutate(
      y0 = configuration_order,
      y1 = configuration_order + 0.72 * density_scaled
    )
  labels <- manifest |>
    filter(dimension == dim) |>
    arrange(configuration_order)

  ggplot(d, aes(x = x, group = configuration)) +
    geom_vline(xintercept = 0, linewidth = .32, color = "grey30") +
    geom_ribbon(aes(ymin = y0, ymax = y1), fill = "grey75", alpha = .82) +
    geom_line(aes(y = y1), linewidth = .38, color = "grey22") +
    scale_y_continuous(
      breaks = labels$configuration_order + .05,
      labels = labels$configuration_label,
      expand = expansion(mult = c(.04, .10))
    ) +
    labs(
      title = paste0(letter, "  ", DIM_TITLES[[dim]]),
      x = "standardized signed distortion (e)",
      y = NULL
    ) +
    base_panel_theme +
    theme(
      panel.grid.major.y = element_blank(),
      axis.text.y = element_text(size = 7)
    )
}

extreme_metric_rows <- function(d, n = 4L) {
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
      slice_max(A_mean_absolute, n = 4L, with_ties = FALSE)
  } else {
    endpoint_d <- tibble()
    label_data <- extreme_metric_rows(d, n = 4L)
  }

  lim <- max(c(d$A_mean_absolute, abs(d$B_mean_signed)), na.rm = TRUE)
  if (dim %in% c("placement", "optical")) {
    lim <- max(
      c(lim, d$A_ci_high, abs(d$B_ci_low), abs(d$B_ci_high)),
      na.rm = TRUE
    )
  }
  lim <- ifelse(is.finite(lim) && lim > 0, lim * 1.06, 1)

  p <- ggplot(d, aes(B_mean_signed, A_mean_absolute, color = metric_class)) +
    geom_abline(
      slope = c(-1, 1), intercept = 0,
      linetype = 2, linewidth = .32, color = "grey62"
    )

  if (dim %in% TRAJECTORY_DIMS) {
    # Trajectory is the primary mark: every metric is connected in the observed
    # requirement order, while points remain deliberately small. The last
    # available configuration for each metric is emphasized separately so
    # structurally unavailable coarse levels do not create false endpoints.
    p <- p +
      geom_path(
        aes(group = metric),
        color = "grey40", alpha = .58, linewidth = .52,
        lineend = "round", linejoin = "round"
      ) +
      geom_point(size = 1.05, alpha = .72) +
      geom_point(
        data = endpoint_d,
        size = 1.95, alpha = .96,
        show.legend = FALSE
      )
  } else if (dim == "placement") {
    ci_d <- d |> filter(bootstrap_supported)
    p <- p +
      geom_errorbar(
        data = ci_d,
        aes(ymin = A_ci_low, ymax = A_ci_high),
        alpha = .20, linewidth = .25
      ) +
      geom_errorbar(
        data = ci_d,
        aes(xmin = B_ci_low, xmax = B_ci_high),
        orientation = "y",
        alpha = .20, linewidth = .25
      ) +
      geom_point(
        aes(shape = configuration_label),
        size = 1.55, alpha = .86
      ) +
      scale_shape_manual(values = c("Chest" = 16, "Wrist" = 17))
  } else {
    ci_d <- d |> filter(bootstrap_supported)
    p <- p +
      geom_errorbar(
        data = ci_d,
        aes(ymin = A_ci_low, ymax = A_ci_high),
        alpha = .20, linewidth = .25
      ) +
      geom_errorbar(
        data = ci_d,
        aes(xmin = B_ci_low, xmax = B_ci_high),
        orientation = "y",
        alpha = .20, linewidth = .25
      ) +
      geom_point(size = 1.55, alpha = .86)
  }

  p <- p +
    geom_text(
      data = label_data,
      aes(label = metric),
      size = 2.0, check_overlap = TRUE, vjust = -0.65,
      show.legend = FALSE
    ) +
    scale_color_discrete(drop = FALSE) +
    labs(
      title = paste0(letter, "  ", DIM_TITLES[[dim]]),
      x = "B: mean signed distortion",
      y = "A: mean absolute distortion"
    ) +
    base_panel_theme

  if (dim %in% TRAJECTORY_DIMS) {
    p <- p +
      scale_x_continuous(
        transform = asinh_display,
        limits = c(-lim, lim),
        expand = expansion(mult = .025)
      ) +
      scale_y_continuous(
        transform = asinh_display,
        limits = c(0, lim),
        expand = expansion(mult = .025)
      ) +
      annotate(
        "text", x = Inf, y = Inf,
        label = "asinh axes", hjust = 1.06, vjust = 1.35,
        size = 2.05, color = "grey45"
      )
  } else {
    p <- p +
      coord_cartesian(
        xlim = c(-lim, lim),
        ylim = c(0, lim),
        expand = TRUE
      )
  }

  p
}

# Global legends only. No individual panel is allowed to surrender plotting area
# to its own guide, which keeps all eight square frames aligned.
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
      override.aes = list(size = 2)
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

# One aligned 2 x 4 matrix. All cells have equal width and equal height, and each
# ggplot panel itself is square. The two legends live in a separate bottom strip.
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

fig1 <- cowplot::plot_grid(
  panel_grid,
  legend_row,
  ncol = 1,
  rel_heights = c(1, .075)
)

ggsave(
  file.path(FIG_DIR, "Fig1_RQ1.pdf"),
  plot = fig1,
  width = 15.6, height = 8.4,
  units = "in", device = cairo_pdf
)

ggsave(
  file.path(FIG_DIR, "Fig1_RQ1.png"),
  plot = fig1,
  width = 15.6, height = 8.4,
  units = "in", dpi = 200
)

message("Fig. 1 complete:")
message("  ", file.path(FIG_DIR, "Fig1_RQ1.pdf"))
message("  ", file.path(FIG_DIR, "Fig1_RQ1.png"))
message("  temporal/duration A-B panels use asinh display axes; underlying RQ1 values are unchanged")
