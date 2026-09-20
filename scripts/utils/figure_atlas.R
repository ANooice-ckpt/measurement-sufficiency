# Shared helpers for information-dense manuscript atlases.
# These functions only reorganize already-estimated quantities for plotting.
# No inferential estimand is changed here.

ms_metric_order <- function(rq1_summary, classes = MS_METRIC_CLASSES) {
  required <- c("metric", "metric_class", "dimension", "A_mean_absolute")
  missing <- setdiff(required, names(rq1_summary))
  if (length(missing)) stop("RQ1 summary missing metric-order columns: ", paste(missing, collapse = ", "))

  # Configuration counts differ strongly by measurement dimension (e.g. one
  # optical alternative versus several temporal/duration levels). Build a
  # dimension-balanced display score so the row order is not implicitly weighted
  # by how densely a dimension happened to be sampled in the configuration grid.
  rq1_summary |>
    dplyr::filter(is.finite(A_mean_absolute)) |>
    dplyr::group_by(metric, metric_class, dimension) |>
    dplyr::summarise(
      dimension_median_A = stats::median(A_mean_absolute, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::group_by(metric, metric_class) |>
    dplyr::summarise(
      rq1_sensitivity = stats::median(dimension_median_A, na.rm = TRUE),
      n_dimensions_in_order = dplyr::n(),
      .groups = "drop"
    ) |>
    dplyr::mutate(metric_class = factor(metric_class, levels = classes)) |>
    dplyr::arrange(metric_class, dplyr::desc(rq1_sensitivity), metric) |>
    dplyr::mutate(metric_order = dplyr::row_number())
}

ms_add_metric_order <- function(data, order_table) {
  # Some plotting summaries (e.g. a per-metric range table) intentionally carry
  # only `metric`. Reconstruct metric class from the frozen order table in that
  # case instead of requiring every derived plotting table to retain it.
  if (!"metric_class" %in% names(data)) data$metric_class <- NA_character_

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
