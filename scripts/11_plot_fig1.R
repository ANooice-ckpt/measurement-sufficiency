suppressPackageStartupMessages({
  library(tidyverse)
  library(grid)
})

# Fig. 1 only. This script reads frozen RQ1 outputs and never recomputes
# representation distortion, standardizers, bootstrap intervals, or duration windows.

DISTORTION_RDS <- "data/derived/rq1/rq1_distortion_long.rds"
SUMMARY_CSV <- "results/rq1/rq1_summary.csv"
MANIFEST_CSV <- "results/rq1/rq1_configuration_manifest.csv"
FIG_DIR <- "results/figures"

for (p in c(DISTORTION_RDS, SUMMARY_CSV, MANIFEST_CSV)) {
  if (!file.exists(p)) stop("Missing RQ1 artifact: ", p, ". Run scripts/10_rq1_analysis.R first.")
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

dist <- readRDS(DISTORTION_RDS) |>
  filter(available, is.finite(e)) |>
  mutate(metric_class = factor(metric_class, levels = METRIC_CLASSES))
summary <- readr::read_csv(SUMMARY_CSV, show_col_types = FALSE) |>
  mutate(metric_class = factor(metric_class, levels = METRIC_CLASSES))
manifest <- readr::read_csv(MANIFEST_CSV, show_col_types = FALSE)

# Equal representation weighting: within each metric x configuration, all smallest
# units together carry weight 1. The top row is therefore a descriptive pooled
# distortion landscape across target representations, not a class-level inference.
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
    geom_vline(xintercept = 0, linewidth = .3, color = "grey35") +
    geom_ribbon(aes(ymin = y0, ymax = y1), fill = "grey72", alpha = .82) +
    geom_line(aes(y = y1), linewidth = .35, color = "grey25") +
    scale_y_continuous(
      breaks = labels$configuration_order + .05,
      labels = labels$configuration_label,
      expand = expansion(mult = c(.03, .12))
    ) +
    labs(
      title = paste0(letter, "  ", DIM_TITLES[[dim]]),
      x = "standardized signed distortion (e)",
      y = NULL
    ) +
    theme_minimal(base_size = 8) +
    theme(
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank(),
      axis.text.y = element_text(size = 7),
      plot.title = element_text(face = "plain")
    )
}

extreme_labels <- function(d, n = 4L) {
  d |>
    arrange(desc(A_mean_absolute)) |>
    slice_head(n = n) |>
    pull(metric)
}

bottom_panel <- function(dim, letter) {
  d <- summary |>
    filter(dimension == dim, is.finite(A_mean_absolute), is.finite(B_mean_signed)) |>
    arrange(metric, configuration_order)
  if (!nrow(d)) stop("No summary rows for Fig. 1 dimension: ", dim)

  if (dim %in% c("temporal", "duration")) {
    endpoint <- d |> filter(configuration_order == max(configuration_order))
    label_metrics <- extreme_labels(endpoint)
    d <- d |>
      mutate(label = if_else(
        configuration_order == max(configuration_order) & metric %in% label_metrics,
        metric, NA_character_
      ))
  } else {
    label_metrics <- extreme_labels(d)
    d <- d |> mutate(label = if_else(metric %in% label_metrics, metric, NA_character_))
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
      linetype = 2, linewidth = .3, color = "grey60"
    )

  if (dim %in% c("temporal", "duration")) {
    p <- p +
      geom_path(
        aes(group = metric),
        color = "grey55", alpha = .33, linewidth = .35,
        arrow = arrow(length = unit(1.0, "mm"), type = "open")
      ) +
      geom_point(aes(size = configuration_order), alpha = .82) +
      guides(size = "none")
  } else if (dim == "placement") {
    p <- p +
      geom_errorbar(
        data = d |> filter(bootstrap_supported),
        aes(ymin = A_ci_low, ymax = A_ci_high),
        alpha = .20, linewidth = .25
      ) +
      geom_errorbarh(
        data = d |> filter(bootstrap_supported),
        aes(xmin = B_ci_low, xmax = B_ci_high),
        alpha = .20, linewidth = .25
      ) +
      geom_point(aes(shape = configuration_label), size = 1.6, alpha = .86) +
      guides(
        color = "none",
        shape = guide_legend(title = NULL, nrow = 1)
      )
  } else {
    p <- p +
      geom_errorbar(
        data = d |> filter(bootstrap_supported),
        aes(ymin = A_ci_low, ymax = A_ci_high),
        alpha = .20, linewidth = .25
      ) +
      geom_errorbarh(
        data = d |> filter(bootstrap_supported),
        aes(xmin = B_ci_low, xmax = B_ci_high),
        alpha = .20, linewidth = .25
      ) +
      geom_point(size = 1.6, alpha = .86) +
      guides(color = "none")
  }

  p <- p +
    geom_text(
      aes(label = label),
      size = 2.0, check_overlap = TRUE, vjust = -0.6,
      show.legend = FALSE
    ) +
    scale_color_discrete(drop = FALSE) +
    coord_cartesian(xlim = c(-lim, lim), ylim = c(0, lim)) +
    labs(
      title = paste0(letter, "  ", DIM_TITLES[[dim]]),
      x = "B: mean signed distortion",
      y = "A: mean absolute distortion",
      color = "metric class",
      shape = NULL
    ) +
    theme_minimal(base_size = 8) +
    theme(
      panel.grid.minor = element_blank(),
      plot.title = element_text(face = "plain")
    )

  if (dim != "placement") p <- p + theme(legend.position = "none")
  p
}

get_legend <- function(p) {
  g <- ggplotGrob(p)
  idx <- which(vapply(g$grobs, function(x) x$name, character(1)) == "guide-box")
  if (!length(idx)) return(nullGrob())
  g$grobs[[idx[1]]]
}

legend_source <- ggplot(
  tibble(
    metric_class = factor(METRIC_CLASSES, levels = METRIC_CLASSES),
    x = seq_along(METRIC_CLASSES),
    y = 1
  ),
  aes(x, y, color = metric_class)
) +
  geom_point(size = 2) +
  scale_color_discrete(drop = FALSE) +
  guides(color = guide_legend(title = "metric class", nrow = 1, byrow = TRUE)) +
  theme_void(base_size = 8) +
  theme(legend.position = "bottom")
metric_legend <- get_legend(legend_source)

top <- map2(DIMENSIONS, PANEL_LETTERS[1, ], top_panel)
bottom <- map2(DIMENSIONS, PANEL_LETTERS[2, ], bottom_panel)

draw_figure <- function(device) {
  device()
  grid.newpage()
  pushViewport(viewport(layout = grid.layout(
    nrow = 3, ncol = 4,
    heights = unit(c(.78, 1.18, .10), "null"),
    widths = unit(rep(1, 4), "null")
  )))
  for (j in seq_along(DIMENSIONS)) {
    print(top[[j]], vp = viewport(layout.pos.row = 1, layout.pos.col = j))
    print(bottom[[j]], vp = viewport(layout.pos.row = 2, layout.pos.col = j))
  }
  pushViewport(viewport(layout.pos.row = 3, layout.pos.col = 1:4))
  grid.draw(metric_legend)
  popViewport()
  dev.off()
}

draw_figure(function() {
  pdf(
    file.path(FIG_DIR, "Fig1_RQ1.pdf"),
    width = 15.5, height = 8.7, useDingbats = FALSE
  )
})
draw_figure(function() {
  png(
    file.path(FIG_DIR, "Fig1_RQ1.png"),
    width = 3100, height = 1740, res = 200
  )
})

message("Fig. 1 complete:")
message("  ", file.path(FIG_DIR, "Fig1_RQ1.pdf"))
message("  ", file.path(FIG_DIR, "Fig1_RQ1.png"))
