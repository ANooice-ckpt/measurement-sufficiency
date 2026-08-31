# Fig. 5 only — joint temporal × duration sufficiency geometry.
.ms_file <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
if (length(.ms_file)) {
  .ms_script <- normalizePath(sub("^--file=", "", .ms_file[[1]]), winslash = "/", mustWork = TRUE)
  .ms_root <- normalizePath(file.path(dirname(.ms_script), ".."), winslash = "/", mustWork = TRUE)
  if (!file.exists(file.path(.ms_root, "scripts", "utils", "figure_style.R"))) {
    stop("Could not resolve measurement-sufficiency repository root from ", .ms_script, call. = FALSE)
  }
  setwd(.ms_root)
}
rm(.ms_file)
if (exists(".ms_script")) rm(.ms_script)
if (exists(".ms_root")) rm(.ms_root)
source("scripts/utils/plot_rq3_common.R")

# =============================================================================
# Fig. 5 — joint temporal × duration sufficiency geometry
# =============================================================================

metric_class_lookup5 <- rq1_summary |> distinct(metric, metric_class)

joint_plot_base <- joint |>
  left_join(metric_class_lookup5, by = "metric", suffix = c("", ".lookup")) |>
  mutate(
    metric_class = coalesce(metric_class, metric_class.lookup),
    resolution_s = as.numeric(resolution_s),
    n_days = as.numeric(n_days)
  ) |>
  filter(is.finite(resolution_s), is.finite(n_days))

fig5_res_levels <- RES_LEVELS
fig5_days <- DURATION_LEVELS
if (!setequal(unique(joint_plot_base$resolution_s), fig5_res_levels) ||
    !setequal(unique(joint_plot_base$n_days), fig5_days)) {
  stop("RQ3 joint artifact does not contain the frozen 6 x 6 primary lattice", call. = FALSE)
}

format_resolution5 <- function(x) {
  x <- as.numeric(x)
  ifelse(
    x >= 60 & abs(x / 60 - round(x / 60)) < 1e-9,
    paste0(format(round(x / 60), trim = TRUE), " min"),
    paste0(format(x, trim = TRUE), " s")
  )
}
fig5_res_labels <- format_resolution5(fig5_res_levels)
joint_plot_base <- joint_plot_base |> mutate(resolution_rank = match(resolution_s, fig5_res_levels))

entry_metric_surface <- joint_plot_base |>
  filter(status == "resolved", is.finite(epsilon_entry), !is.na(metric_class)) |>
  group_by(metric, metric_class, resolution_s, resolution_rank, n_days) |>
  summarise(epsilon_metric = median(epsilon_entry, na.rm = TRUE), n_facets = n(), .groups = "drop")

entry_surface <- entry_metric_surface |>
  group_by(resolution_s, resolution_rank, n_days) |>
  summarise(
    n_metrics = n_distinct(metric),
    epsilon_entry_median = median(epsilon_metric, na.rm = TRUE),
    epsilon_entry_q25 = quantile(epsilon_metric, .25, na.rm = TRUE, names = FALSE),
    epsilon_entry_q75 = quantile(epsilon_metric, .75, na.rm = TRUE, names = FALSE),
    .groups = "drop"
  )

joint_cell_status <- joint_plot_base |>
  group_by(resolution_rank, n_days) |>
  summarise(cell_unresolved = any(status == "boundary_unresolved"), .groups = "drop")

entry_grid <- tidyr::crossing(
  resolution_rank = seq_along(fig5_res_levels),
  n_days = fig5_days
) |>
  left_join(
    entry_surface |>
      select(resolution_rank, n_days, n_metrics,
             epsilon_entry_median, epsilon_entry_q25, epsilon_entry_q75),
    by = c("resolution_rank", "n_days")
  ) |>
  left_join(joint_cell_status, by = c("resolution_rank", "n_days")) |>
  mutate(cell_unresolved = replace_na(cell_unresolved, FALSE))

entry_fill_max5 <- max(c(entry_grid$epsilon_entry_median, entry_metric_surface$epsilon_metric), na.rm = TRUE)
if (!is.finite(entry_fill_max5) || entry_fill_max5 <= 0) entry_fill_max5 <- 1
entry_fill_limits5 <- c(0, entry_fill_max5)

FIG5_TOLERANCE_SLICES <- c(.10, .25, .50, .75)
build_step_boundaries5 <- function(g, eps_levels, value_column,
                                   half_width = .46, facet_value = NULL) {
  is_sufficient <- function(x, y, eps) {
    z <- g[g$resolution_rank == x & g$n_days == y, , drop = FALSE]
    value <- z[[value_column]][[1]]
    nrow(z) == 1L && isTRUE(is.finite(value)) && isTRUE(value <= eps + NUMERIC_TOL)
  }
  segments <- list()
  for (eps in eps_levels) {
    eps_label <- paste0("epsilon = ", sprintf("%.2f", eps))
    for (x in seq_along(fig5_res_levels)) {
      for (y in fig5_days) {
        if (!is_sufficient(x, y, eps)) next
        x0 <- x; y0 <- y
        if (x == 1L || !is_sufficient(x - 1L, y, eps)) {
          segments[[length(segments) + 1L]] <- tibble(
            epsilon_label = eps_label, x = x0 - half_width, xend = x0 - half_width,
            y = y0 - half_width, yend = y0 + half_width)
        }
        if (x == length(fig5_res_levels) || !is_sufficient(x + 1L, y, eps)) {
          segments[[length(segments) + 1L]] <- tibble(
            epsilon_label = eps_label, x = x0 + half_width, xend = x0 + half_width,
            y = y0 - half_width, yend = y0 + half_width)
        }
        if (y == min(fig5_days) || !is_sufficient(x, y - 1L, eps)) {
          segments[[length(segments) + 1L]] <- tibble(
            epsilon_label = eps_label, x = x0 - half_width, xend = x0 + half_width,
            y = y0 - half_width, yend = y0 - half_width)
        }
        if (y == max(fig5_days) || !is_sufficient(x, y + 1L, eps)) {
          segments[[length(segments) + 1L]] <- tibble(
            epsilon_label = eps_label, x = x0 - half_width, xend = x0 + half_width,
            y = y0 + half_width, yend = y0 + half_width)
        }
      }
    }
  }
  out <- bind_rows(segments) |>
    mutate(epsilon_label = factor(
      epsilon_label, levels = paste0("epsilon = ", sprintf("%.2f", eps_levels))))
  if (!is.null(facet_value)) out$metric <- facet_value
  out
}

build_region_boundaries5 <- function(g, thresholds, value_column,
                                     group_column, facet_column,
                                     half_width = .46) {
  threshold_labels <- paste0(">=", sprintf("%d%%", round(100 * thresholds)))
  segments <- list()
  group_values <- unique(g[[group_column]])
  group_values <- group_values[!is.na(group_values)]
  for (group_value in group_values) {
    gs <- g[!is.na(g[[group_column]]) & g[[group_column]] == group_value, , drop = FALSE]
    facet_values <- as.character(gs[[facet_column]])
    facet_values <- facet_values[!is.na(facet_values)]
    facet_label <- if (length(facet_values)) facet_values[[1]] else as.character(group_value)
    for (j in seq_along(thresholds)) {
      threshold <- thresholds[[j]]
      is_region <- function(x, y) {
        z <- gs[gs$resolution_rank == x & gs$n_days == y, , drop = FALSE]
        if (nrow(z) != 1L) return(FALSE)
        value <- z[[value_column]][[1]]
        unresolved <- if ("cell_unresolved" %in% names(z)) isTRUE(z$cell_unresolved[[1]]) else FALSE
        !unresolved && isTRUE(is.finite(value)) && isTRUE(value >= threshold - NUMERIC_TOL)
      }
      for (x in seq_along(fig5_res_levels)) {
        for (y in fig5_days) {
          if (!is_region(x, y)) next
          x0 <- x; y0 <- y
          if (x == 1L || !is_region(x - 1L, y)) {
            segments[[length(segments) + 1L]] <- tibble(
              facet_label = facet_label, region_level = threshold_labels[[j]],
              x = x0 - half_width, xend = x0 - half_width,
              y = y0 - half_width, yend = y0 + half_width)
          }
          if (x == length(fig5_res_levels) || !is_region(x + 1L, y)) {
            segments[[length(segments) + 1L]] <- tibble(
              facet_label = facet_label, region_level = threshold_labels[[j]],
              x = x0 + half_width, xend = x0 + half_width,
              y = y0 - half_width, yend = y0 + half_width)
          }
          if (y == min(fig5_days) || !is_region(x, y - 1L)) {
            segments[[length(segments) + 1L]] <- tibble(
              facet_label = facet_label, region_level = threshold_labels[[j]],
              x = x0 - half_width, xend = x0 + half_width,
              y = y0 - half_width, yend = y0 - half_width)
          }
          if (y == max(fig5_days) || !is_region(x, y + 1L)) {
            segments[[length(segments) + 1L]] <- tibble(
              facet_label = facet_label, region_level = threshold_labels[[j]],
              x = x0 - half_width, xend = x0 + half_width,
              y = y0 + half_width, yend = y0 + half_width)
          }
        }
      }
    }
  }
  if (!length(segments)) {
    return(tibble(
      facet_label = character(),
      region_level = factor(character(), levels = threshold_labels),
      x = numeric(), xend = numeric(), y = numeric(), yend = numeric()))
  }
  bind_rows(segments) |> mutate(region_level = factor(region_level, levels = threshold_labels))
}

make_region_cells5 <- function(g, thresholds, value_column) {
  threshold_labels <- paste0(">=", sprintf("%d%%", round(100 * thresholds)))
  bind_rows(lapply(seq_along(thresholds), function(j) {
    g |>
      filter(!cell_unresolved, is.finite(.data[[value_column]]),
             .data[[value_column]] >= thresholds[[j]] - NUMERIC_TOL) |>
      mutate(region_level = threshold_labels[[j]])
  })) |>
    mutate(region_level = factor(region_level, levels = threshold_labels))
}

threshold_boundaries5 <- build_step_boundaries5(entry_grid, FIG5_TOLERANCE_SLICES, "epsilon_entry_median")

p5a <- ggplot(entry_grid, aes(resolution_rank, n_days, fill = epsilon_entry_median)) +
  geom_tile(width = .92, height = .92, color = "white", linewidth = .34) +
  geom_tile(
    data = entry_grid |> filter(cell_unresolved),
    aes(resolution_rank, n_days), inherit.aes = FALSE,
    fill = "#D8DCDE", color = "white", linewidth = .34
  ) +
  geom_text(
    aes(label = if_else(is.finite(epsilon_entry_median),
                        formatC(epsilon_entry_median, format = "f", digits = 2), "")),
    size = 1.48, color = "#4A5459", na.rm = TRUE
  ) +
  geom_text(
    data = entry_grid |> filter(cell_unresolved),
    aes(resolution_rank, n_days, label = "U"), inherit.aes = FALSE,
    size = 2.2, fontface = "bold", color = "#596064"
  ) +
  geom_segment(
    data = threshold_boundaries5,
    aes(x = x, y = y, xend = xend, yend = yend, linetype = epsilon_label),
    inherit.aes = FALSE, color = "#29383F", linewidth = .34, lineend = "butt"
  ) +
  scale_fill_ms_sequential(
    limits = entry_fill_limits5, oob = scales::squish,
    trans = scales::transform_asinh(), na.value = "#F0F2F2",
    name = "entry tolerance epsilon\n(R_obs)"
  ) +
  scale_linetype_manual(
    values = c("solid", "dashed", "dotdash", "dotted"),
    breaks = paste0("epsilon = ", sprintf("%.2f", FIG5_TOLERANCE_SLICES)),
    labels = sprintf("%.2f", FIG5_TOLERANCE_SLICES), name = "epsilon"
  ) +
  scale_x_continuous(
    breaks = seq_along(fig5_res_levels), labels = fig5_res_labels,
    expand = expansion(add = .30)
  ) +
  scale_y_continuous(
    breaks = fig5_days, labels = paste0(fig5_days, " d"),
    expand = expansion(add = .30)
  ) +
  coord_fixed(ratio = .86, clip = "off") +
  labs(
    title = "a  Joint entry-tolerance landscape",
    subtitle = "darker = more permissive tolerance required; U = boundary unresolved",
    x = "temporal resolution  (low to high burden)", y = "monitoring duration"
  ) +
  theme_rq3(base_size = 6.1, legend_position = "bottom") +
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_text(angle = 30, hjust = 1, size = 4.85),
    axis.text.y = element_text(size = 4.75),
    plot.subtitle = element_text(size = 4.45, colour = "#666A6D", margin = margin(t = -1, b = 2)),
    legend.text = element_text(size = 4.15), legend.title = element_text(size = 4.25),
    legend.key.width = grid::unit(5.8, "mm"), legend.key.height = grid::unit(2.8, "mm"),
    legend.box = "horizontal", legend.box.just = "left",
    legend.spacing.x = grid::unit(3, "mm")
  ) +
  guides(
    fill = guide_colorbar(order = 1, title.position = "top", barwidth = grid::unit(20, "mm")),
    linetype = guide_legend(order = 2, nrow = 1, byrow = TRUE)
  )

FIG5_PARETO_SLICES <- c(.10, .25, .50)
pareto_occupancy5 <- pareto_occupancy |>
  mutate(
    resolution_s = as.numeric(resolution_s), n_days = as.numeric(n_days),
    epsilon_interval_start = as.numeric(epsilon_interval_start),
    epsilon_interval_end = as.numeric(epsilon_interval_end),
    pareto = replace_na(as.logical(pareto), FALSE)
  )

select_pareto_interval5 <- function(g, eps) {
  g |>
    group_by(support_id, placement, optical, metric, resolution_s, n_days) |>
    filter({
      inside <- epsilon_interval_start <= eps + NUMERIC_TOL & epsilon_interval_end > eps + NUMERIC_TOL
      if (any(inside)) inside else epsilon_interval_end <= eps + NUMERIC_TOL
    }) |>
    slice_max(epsilon_interval_start, n = 1, with_ties = FALSE) |>
    ungroup()
}
pareto_slice5 <- bind_rows(lapply(FIG5_PARETO_SLICES, function(eps) {
  select_pareto_interval5(pareto_occupancy5, eps) |> mutate(epsilon = eps)
}))
joint_available5 <- joint_plot_base |>
  group_by(resolution_rank, n_days) |>
  summarise(n_available = n_distinct(paste(support_id, placement, optical, metric, sep = "|")), .groups = "drop")
pareto_slice_counts5 <- pareto_slice5 |>
  group_by(epsilon, resolution_s, n_days) |>
  summarise(n_available = n_distinct(paste(support_id, placement, optical, metric, sep = "|")), .groups = "drop") |>
  mutate(resolution_rank = match(resolution_s, fig5_res_levels)) |>
  left_join(joint_available5, by = c("resolution_rank", "n_days"))
if (any(pareto_slice_counts5$n_available.x != pareto_slice_counts5$n_available.y, na.rm = TRUE)) {
  stop("Frozen Pareto occupancy slices do not cover the available joint metrics", call. = FALSE)
}
pareto_slice_summary5 <- pareto_slice5 |>
  group_by(epsilon, resolution_s, n_days) |>
  summarise(pareto_fraction = mean(pareto), .groups = "drop")
pareto_grid5 <- tidyr::crossing(
  epsilon = FIG5_PARETO_SLICES,
  resolution_rank = seq_along(fig5_res_levels), n_days = fig5_days
) |>
  left_join(
    pareto_slice_summary5 |>
      mutate(
        resolution_rank = match(resolution_s, fig5_res_levels),
        epsilon_label = factor(
          paste0("epsilon = ", sprintf("%.2f", epsilon)),
          levels = paste0("epsilon = ", sprintf("%.2f", FIG5_PARETO_SLICES)))) |>
      select(epsilon, epsilon_label, resolution_rank, n_days, pareto_fraction),
    by = c("epsilon", "resolution_rank", "n_days")
  ) |>
  left_join(joint_cell_status, by = c("resolution_rank", "n_days")) |>
  mutate(
    cell_unresolved = replace_na(cell_unresolved, FALSE),
    pareto_fraction = if_else(cell_unresolved, NA_real_, pareto_fraction)
  )

pareto_region_thresholds5 <- c(.25, .50, .75)
pareto_region_cells5 <- make_region_cells5(pareto_grid5, pareto_region_thresholds5, "pareto_fraction")
pareto_presence_cells5 <- pareto_grid5 |>
  filter(!cell_unresolved, is.finite(pareto_fraction), pareto_fraction > 0)
pareto_region_boundaries5 <- build_region_boundaries5(
  pareto_grid5, pareto_region_thresholds5, "pareto_fraction",
  group_column = "epsilon", facet_column = "epsilon_label"
) |>
  mutate(epsilon_label = factor(facet_label, levels = levels(pareto_grid5$epsilon_label)))

p5b <- ggplot(pareto_grid5, aes(resolution_rank, n_days)) +
  geom_tile(width = .92, height = .92, fill = "#F7F9F9", color = "#E5EAEB", linewidth = .16) +
  geom_tile(
    data = pareto_presence_cells5,
    aes(resolution_rank, n_days), inherit.aes = FALSE,
    width = .92, height = .92, fill = "#D7E8EA", color = NA
  ) +
  geom_tile(
    data = pareto_region_cells5,
    aes(resolution_rank, n_days, fill = region_level), inherit.aes = FALSE,
    width = .92, height = .92, alpha = .42, color = NA
  ) +
  geom_tile(
    data = pareto_grid5 |> filter(cell_unresolved),
    aes(resolution_rank, n_days), inherit.aes = FALSE,
    width = .92, height = .92, fill = "#D8DCDE", color = NA
  ) +
  geom_text(
    data = pareto_grid5 |> filter(cell_unresolved),
    aes(resolution_rank, n_days, label = "U"), inherit.aes = FALSE,
    size = 1.55, fontface = "bold", color = "#596064"
  ) +
  geom_segment(
    data = pareto_region_boundaries5,
    aes(x = x, y = y, xend = xend, yend = yend, linewidth = region_level),
    inherit.aes = FALSE, color = "#355A6C", lineend = "butt"
  ) +
  facet_wrap(~epsilon_label, nrow = 1) +
  scale_fill_manual(
    values = c(">=25%" = "#A8C6CC", ">=50%" = "#6F9CAD", ">=75%" = "#365F74"),
    name = "Pareto occupancy region",
    limits = names(c(">=25%" = 1, ">=50%" = 1, ">=75%" = 1)),
    drop = FALSE,
    guide = guide_legend(title.position = "top", nrow = 1, byrow = TRUE)
  ) +
  scale_linewidth_manual(values = c(">=25%" = .30, ">=50%" = .43, ">=75%" = .58), guide = "none") +
  scale_x_continuous(
    breaks = seq_along(fig5_res_levels), labels = fig5_res_labels, expand = expansion(add = .16)
  ) +
  scale_y_continuous(
    breaks = fig5_days, labels = paste0(fig5_days, " d"), expand = expansion(add = .16)
  ) +
  coord_fixed(ratio = .86, clip = "off") +
  labs(
    title = "b  Pareto occupancy shifts as tolerance relaxes",
    subtitle = "pale footprint = occupancy > 0; darker bands and boundaries = >= 25%, 50%, 75%",
    x = "temporal resolution", y = NULL
  ) +
  theme_rq3(base_size = 5.5, legend_position = "bottom") +
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 3.35),
    axis.text.y = element_text(size = 4.0), axis.title.y = element_blank(),
    strip.text = element_text(size = 4.7), plot.title = element_text(size = 6.0),
    plot.subtitle = element_text(size = 3.95, colour = "#666A6D", margin = margin(t = -1, b = 1)),
    panel.spacing = grid::unit(.6, "mm"), legend.text = element_text(size = 3.9),
    legend.title = element_text(size = 4.0), legend.key.width = grid::unit(5.8, "mm"),
    legend.key.height = grid::unit(2.6, "mm"), plot.margin = margin(2.5, 1.5, 1.5, 1.5)
  ) +
  guides(fill = guide_legend(title.position = "top", nrow = 1, byrow = TRUE))

class_order5 <- c("timing", "duration", "level", "temporal dynamics")
class_name5 <- c(
  "timing" = "Timing", "duration" = "Duration", "level" = "Level",
  "temporal dynamics" = "Temporal dynamics"
)
class_threshold5 <- .50
class_metric_counts5 <- metric_class_lookup5 |>
  filter(metric_class %in% class_order5) |>
  group_by(metric_class) |>
  summarise(n_class_metrics = n_distinct(metric), .groups = "drop")
metric_cell_status_all5 <- joint_plot_base |>
  group_by(metric, resolution_rank, n_days) |>
  summarise(cell_unresolved = any(status == "boundary_unresolved"), .groups = "drop")
class_surface5 <- entry_metric_surface |>
  filter(metric_class %in% class_order5) |>
  group_by(metric_class, resolution_rank, n_days) |>
  summarise(
    n_resolved_metrics = n_distinct(metric),
    suff_fraction = mean(epsilon_metric <= class_threshold5 + NUMERIC_TOL), .groups = "drop"
  )
class_status5 <- metric_cell_status_all5 |>
  left_join(metric_class_lookup5, by = "metric") |>
  filter(metric_class %in% class_order5) |>
  group_by(metric_class, resolution_rank, n_days) |>
  summarise(class_unresolved = any(cell_unresolved), .groups = "drop")
class_grid5 <- tidyr::crossing(
  metric_class = class_order5,
  resolution_rank = seq_along(fig5_res_levels), n_days = fig5_days
) |>
  left_join(class_surface5, by = c("metric_class", "resolution_rank", "n_days")) |>
  left_join(class_metric_counts5, by = "metric_class") |>
  left_join(class_status5, by = c("metric_class", "resolution_rank", "n_days")) |>
  mutate(
    class_unresolved = replace_na(class_unresolved, FALSE),
    cell_unresolved = class_unresolved,
    suff_fraction = if_else(class_unresolved, NA_real_, suff_fraction),
    class_label = paste0(unname(class_name5[metric_class]), " (n = ", n_class_metrics, ")"),
    class_label = factor(
      class_label,
      levels = paste0(unname(class_name5[class_order5]), " (n = ",
                      class_metric_counts5$n_class_metrics[match(class_order5, class_metric_counts5$metric_class)], ")"))
  )
class_region_thresholds5 <- c(.25, .50, .75)
class_region_cells5 <- make_region_cells5(class_grid5, class_region_thresholds5, "suff_fraction")
class_region_boundaries5 <- build_region_boundaries5(
  class_grid5, class_region_thresholds5, "suff_fraction",
  group_column = "metric_class", facet_column = "class_label"
) |>
  mutate(class_label = factor(facet_label, levels = levels(class_grid5$class_label)))
class_region_fill_alpha5 <- c(">=25%" = .08, ">=50%" = .16, ">=75%" = .25)
class_region_line_types5 <- c(">=25%" = "dotted", ">=50%" = "solid", ">=75%" = "dashed")
class_region_line_widths5 <- c(">=25%" = .25, ">=50%" = .45, ">=75%" = .58)

p5c <- ggplot(class_grid5, aes(resolution_rank, n_days)) +
  geom_tile(width = .92, height = .92, fill = "#FAFBFB", color = "#E5EAEB", linewidth = .16) +
  geom_tile(
    data = class_region_cells5,
    aes(resolution_rank, n_days, alpha = region_level), inherit.aes = FALSE,
    width = .92, height = .92, fill = "#5A879B", color = NA
  ) +
  geom_tile(
    data = class_grid5 |> filter(class_unresolved),
    aes(resolution_rank, n_days), inherit.aes = FALSE,
    width = .92, height = .92, fill = "#D8DCDE", color = NA
  ) +
  geom_text(
    data = class_grid5 |> filter(class_unresolved),
    aes(resolution_rank, n_days, label = "U"), inherit.aes = FALSE,
    size = 1.65, fontface = "bold", color = "#596064"
  ) +
  geom_segment(
    data = class_region_boundaries5,
    aes(x = x, y = y, xend = xend, yend = yend,
        linetype = region_level, linewidth = region_level),
    inherit.aes = FALSE, color = "#365A6A", lineend = "butt"
  ) +
  facet_wrap(~class_label, ncol = 4) +
  scale_alpha_manual(values = class_region_fill_alpha5, guide = "none") +
  scale_linetype_manual(
    values = class_region_line_types5, name = "class sufficient fraction",
    breaks = names(class_region_line_types5), labels = names(class_region_line_types5)
  ) +
  scale_linewidth_manual(values = class_region_line_widths5, guide = "none") +
  scale_x_continuous(
    breaks = seq_along(fig5_res_levels), labels = fig5_res_labels, expand = expansion(add = .16)
  ) +
  scale_y_continuous(
    breaks = fig5_days, labels = paste0(fig5_days, " d"), expand = expansion(add = .16)
  ) +
  coord_fixed(ratio = .86, clip = "off") +
  labs(
    title = "c  Class-specific sufficient-region geometry",
    subtitle = paste0(
      "shared epsilon = ", sprintf("%.2f", class_threshold5),
      "; shaded regions = class fraction sufficient; solid = 50%; grey U = unresolved"
    ),
    x = "temporal resolution", y = "monitoring duration"
  ) +
  theme_rq3(base_size = 5.5, legend_position = "bottom") +
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_text(angle = 50, hjust = 1, size = 3.05),
    axis.text.y = element_text(size = 3.55), strip.text = element_text(size = 4.3),
    plot.title = element_text(size = 6.2),
    plot.subtitle = element_text(size = 3.55, colour = "#666A6D", margin = margin(t = -1, b = 1)),
    panel.spacing = grid::unit(1.2, "mm"), legend.text = element_text(size = 3.9),
    legend.title = element_text(size = 4.0), legend.key.width = grid::unit(5.8, "mm"),
    legend.key.height = grid::unit(2.6, "mm"), plot.margin = margin(2.5, 2, 1.5, 2)
  ) +
  guides(linetype = guide_legend(title.position = "top", nrow = 1, byrow = TRUE))

fig5_top <- cowplot::plot_grid(
  p5a, p5b, ncol = 2, rel_widths = c(.55, .45),
  align = "hv", axis = "tblr", greedy = TRUE
)
fig5_body <- cowplot::plot_grid(
  fig5_top, p5c, ncol = 1, rel_heights = c(.68, .82),
  align = "v", axis = "l", greedy = TRUE
)
ms_plot_save(fig5_body, file.path(OUT_DIR, "Fig5_RQ3.png"), 7.2, 5.8)
message("Fig. 5 complete: joint temporal-duration sufficiency geometry.")
