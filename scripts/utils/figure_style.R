# Shared publication-style visual system for all manuscript figures.
# Purely visual: no data filtering, statistics, or figure-specific transformations.

MS_FONT_REQUESTED <- Sys.getenv("MS_FIG_FONT", unset = "Source Sans 3")
MS_FONT <- if (requireNamespace("systemfonts", quietly = TRUE) &&
               MS_FONT_REQUESTED %in% unique(systemfonts::system_fonts()$family)) {
  MS_FONT_REQUESTED
} else {
  "sans"
}

MS_PRIMARY <- "#2F5D7E"
MS_SECONDARY <- "#C67C2E"
MS_NEUTRAL <- "#6E7479"
MS_TWO_COLORS <- c(primary = MS_PRIMARY, secondary = MS_SECONDARY)
MS_THREE_COLORS <- c(MS_PRIMARY, MS_SECONDARY, "#5F8F84")

MS_METRIC_CLASSES <- c(
  "duration", "exposure history", "level",
  "spectrum", "temporal dynamics", "timing"
)
MS_METRIC_COLORS <- c(
  "duration" = "#4E79A7",
  "exposure history" = "#59A6A6",
  "level" = "#5B9E4D",
  "spectrum" = "#D39B2A",
  "temporal dynamics" = "#D65F4A",
  "timing" = "#8E6BAF"
)

MS_STATE_COLORS <- c(
  "Low" = "#A9C7DB",
  "Middle" = "#6F9CBD",
  "High" = MS_PRIMARY
)
MS_SEQUENTIAL <- c("#F7FBFF", "#DDEAF4", "#A9C7DB", "#6F9CBD", MS_PRIMARY)
MS_DIVERGING <- c("#27577F", "#7FA8C7", "#F7F7F4", "#E3AE79", "#B96828")

scale_color_ms_metric <- function(...) {
  ggplot2::scale_color_manual(values = MS_METRIC_COLORS, limits = MS_METRIC_CLASSES, drop = FALSE, ...)
}
scale_fill_ms_metric <- function(...) {
  ggplot2::scale_fill_manual(values = MS_METRIC_COLORS, limits = MS_METRIC_CLASSES, drop = FALSE, ...)
}
scale_fill_ms_sequential <- function(...) {
  ggplot2::scale_fill_gradientn(colours = MS_SEQUENTIAL, ...)
}
scale_color_ms_sequential <- function(...) {
  ggplot2::scale_color_gradientn(colours = MS_SEQUENTIAL, ...)
}

# Diverging scales are always symmetric around zero; pass the largest absolute value.
scale_fill_ms_diverging <- function(max_abs, ...) {
  max_abs <- abs(as.numeric(max_abs)[1])
  if (!is.finite(max_abs) || max_abs <= 0) stop("max_abs must be a positive finite number")
  ggplot2::scale_fill_gradientn(
    colours = MS_DIVERGING,
    values = c(0, .275, .5, .725, 1),
    limits = c(-max_abs, max_abs), ...
  )
}
scale_color_ms_diverging <- function(max_abs, ...) {
  max_abs <- abs(as.numeric(max_abs)[1])
  if (!is.finite(max_abs) || max_abs <= 0) stop("max_abs must be a positive finite number")
  ggplot2::scale_color_gradientn(
    colours = MS_DIVERGING,
    values = c(0, .275, .5, .725, 1),
    limits = c(-max_abs, max_abs), ...
  )
}

theme_ms <- function(base_size = 8.3, aspect_ratio = NULL, legend_position = "bottom") {
  ggplot2::theme_minimal(base_family = MS_FONT, base_size = base_size) +
    ggplot2::theme(
      panel.background = ggplot2::element_rect(fill = "white", colour = NA),
      plot.background = ggplot2::element_rect(fill = "white", colour = NA),
      panel.border = ggplot2::element_rect(fill = NA, colour = "black", linewidth = .48),
      panel.grid.major = ggplot2::element_line(colour = "#ECECEC", linewidth = .24, linetype = "22"),
      panel.grid.minor = ggplot2::element_blank(),
      axis.text = ggplot2::element_text(colour = "#303030"),
      axis.title = ggplot2::element_text(colour = "#202020"),
      axis.ticks = ggplot2::element_line(colour = "black", linewidth = .28),
      axis.ticks.length = grid::unit(1.3, "mm"),
      strip.background = ggplot2::element_rect(fill = "white", colour = NA),
      strip.text = ggplot2::element_text(colour = "#202020", margin = ggplot2::margin(2, 2, 2, 2)),
      plot.title = ggplot2::element_text(size = base_size + .8, colour = "#151515", margin = ggplot2::margin(b = 4)),
      plot.margin = ggplot2::margin(4, 5, 4, 5),
      legend.position = legend_position,
      legend.key = ggplot2::element_blank(),
      legend.background = ggplot2::element_blank(),
      aspect.ratio = aspect_ratio
    )
}

theme_ms_blank <- function(base_size = 8.3, aspect_ratio = NULL) {
  theme_ms(base_size = base_size, aspect_ratio = aspect_ratio, legend_position = "none") +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      axis.text = ggplot2::element_blank(),
      axis.title = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank()
    )
}
