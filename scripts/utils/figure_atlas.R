# Shared helpers for information-dense manuscript atlases.
# These functions only reorganize already-estimated quantities for plotting.
# No inferential estimand is changed here.

ms_metric_order <- function(rq1_summary, classes = MS_METRIC_CLASSES) {
  required <- c("metric", "metric_class", "A_mean_absolute")
  missing <- setdiff(required, names(rq1_summary))
  if (length(missing)) stop("RQ1 summary missing metric-order columns: ", paste(missing, collapse = ", "))

  rq1_summary |>
    dplyr::filter(is.finite(A_mean_absolute)) |>
    dplyr::group_by(metric, metric_class) |>
    dplyr::summarise(
      rq1_sensitivity = stats::median(A_mean_absolute, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::mutate(metric_class = factor(metric_class, levels = classes)) |>
    dplyr::arrange(metric_class, dplyr::desc(rq1_sensitivity), metric) |>
    dplyr::mutate(metric_order = dplyr::row_number())
}

ms_add_metric_order <- function(data, order_table) {
  data |>
    dplyr::left_join(
      order_table |>
        dplyr::select(metric, metric_class_order = metric_class, rq1_sensitivity, metric_order),
      by = "metric"
    ) |>
    dplyr::mutate(
      metric_class = dplyr::coalesce(as.character(metric_class), as.character(metric_class_order)),
      metric_class = factor(metric_class, levels = MS_METRIC_CLASSES),
      metric = factor(metric, levels = rev(order_table$metric))
    ) |>
    dplyr::select(-metric_class_order)
}

ms_direction_ratio <- function(signed, magnitude) {
  out <- rep(NA_real_, length(magnitude))
  ok <- is.finite(signed) & is.finite(magnitude) & magnitude > sqrt(.Machine$double.eps)
  out[ok] <- signed[ok] / magnitude[ok]
  zero <- is.finite(signed) & is.finite(magnitude) & abs(magnitude) <= sqrt(.Machine$double.eps)
  out[zero] <- 0
  pmax(-1, pmin(1, out))
}

ms_direction_scale <- function(name = "directionality\n(signed / absolute)", ...) {
  scale_fill_ms_diverging(
    1,
    name = name,
    breaks = c(-1, -.5, 0, .5, 1),
    labels = c("-1", "-0.5", "0", "0.5", "1"),
    ...
  )
}

ms_magnitude_size_scale <- function(name = "absolute distortion", range = c(.25, 3.0), ...) {
  ggplot2::scale_size_continuous(
    transform = scales::transform_asinh(),
    range = range,
    name = name,
    ...
  )
}

ms_atlas_theme <- function(base_size = 6.5, legend_position = "bottom", x_angle = 45) {
  theme_ms(base_size = base_size, legend_position = legend_position) +
    ggplot2::theme(
      panel.border = ggplot2::element_rect(fill = NA, colour = "#B7B7B7", linewidth = .28),
      panel.grid = ggplot2::element_blank(),
      panel.spacing.x = grid::unit(.7, "mm"),
      panel.spacing.y = grid::unit(.55, "mm"),
      strip.placement = "outside",
      strip.background = ggplot2::element_rect(fill = "white", colour = NA),
      strip.text.x = ggplot2::element_text(size = base_size, face = "bold"),
      strip.text.y.left = ggplot2::element_text(size = base_size - .3, face = "bold", angle = 0),
      axis.text.y = ggplot2::element_text(size = base_size - 1.0, lineheight = .9),
      axis.text.x = ggplot2::element_text(size = base_size - .8, angle = x_angle, hjust = 1, vjust = 1),
      axis.ticks = ggplot2::element_blank(),
      plot.margin = ggplot2::margin(4, 4, 4, 4)
    )
}

ms_context_state_table <- function() {
  tibble::tribble(
    ~context_family, ~context_state, ~context_state_label, ~context_order,
    "photoperiod", "day", "Day", 1L,
    "photoperiod", "night", "Night", 2L,
    "environment", "indoor", "Indoor", 3L,
    "environment", "outdoor", "Outdoor", 4L,
    "activity", "home", "Home", 5L,
    "activity", "working", "Working", 6L,
    "activity", "vehicle", "Vehicle", 7L,
    "activity", "outdoors", "Outdoors", 8L
  )
}

ms_short_dimension <- function(x) {
  dplyr::recode(
    as.character(x),
    placement = "Placement",
    optical = "Optical",
    temporal = "Temporal",
    duration = "Duration",
    .default = as.character(x)
  )
}
