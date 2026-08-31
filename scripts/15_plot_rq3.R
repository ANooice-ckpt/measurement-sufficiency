# Canonical RQ3 plotting source. All accepted display refinements are consolidated here.
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
suppressPackageStartupMessages({library(tidyverse); library(cowplot)})
source("scripts/utils/figure_style.R")
source("scripts/utils/figure_atlas.R")
source("scripts/utils/plot_contracts.R")
source("scripts/utils/analysis_design.R")

RQ1_SUMMARY_CSV <- file.path("results", "rq1", "rq1_pairwise_summary.csv")
OBSERVED_RDS <- file.path("results", "rq3", "rq3_sufficiency_long.rds")
SUFFICIENCY_CSV <- file.path("results", "rq3", "rq3_sufficiency_long.csv")
REQUIREMENT_CSV <- file.path("results", "rq3", "rq3_single_dimension_requirement.csv")
UNORDERED_CSV <- file.path("results", "rq3", "rq3_unordered_substitutability.csv")
COVERAGE_CSV <- file.path("results", "rq3", "rq3_unordered_coverage_curves.csv")
CONVERGENCE_CSV <- file.path("results", "rq3", "rq3_convergence_profile.csv")
JOINT_CSV <- file.path("results", "rq3", "rq3_joint_summary.csv")
PARETO_OCCUPANCY_CSV <- file.path("results", "rq3", "rq3_pareto_occupancy.csv")
OUT_DIR <- file.path("results", "rq3", "figures")
ms_plot_require_files(c(RQ1_SUMMARY_CSV, OBSERVED_RDS, SUFFICIENCY_CSV, REQUIREMENT_CSV,
                        UNORDERED_CSV, COVERAGE_CSV, CONVERGENCE_CSV, JOINT_CSV,
                        PARETO_OCCUPANCY_CSV),
                        "RQ3 v5 plotting inputs")
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

METRIC_CLASSES <- MS_METRIC_CLASSES
ORDERED_DIMS <- c("temporal", "duration")
ORDERED_TITLES <- c(temporal = "Temporal resolution", duration = "Monitoring duration")
RES_LEVELS <- rev(ms_primary_temporal_s())
RES_LABELS <- ms_temporal_label(RES_LEVELS)
DURATION_LEVELS <- ms_primary_duration_days()
ORDERED_MAX_RANK <- max(length(RES_LEVELS), length(DURATION_LEVELS))
NUMERIC_TOL <- 1e-12

rq1_summary <- readr::read_csv(RQ1_SUMMARY_CSV, show_col_types = FALSE, progress = FALSE)
observed <- readRDS(OBSERVED_RDS)
sufficiency <- readr::read_csv(SUFFICIENCY_CSV, show_col_types = FALSE, progress = FALSE)
requirement <- readr::read_csv(REQUIREMENT_CSV, show_col_types = FALSE, progress = FALSE)
unordered <- readr::read_csv(UNORDERED_CSV, show_col_types = FALSE, progress = FALSE)
coverage <- readr::read_csv(COVERAGE_CSV, show_col_types = FALSE, progress = FALSE)
convergence <- readr::read_csv(CONVERGENCE_CSV, show_col_types = FALSE, progress = FALSE)
joint <- readr::read_csv(JOINT_CSV, show_col_types = FALSE, progress = FALSE)
pareto_occupancy <- readr::read_csv(PARETO_OCCUPANCY_CSV, show_col_types = FALSE, progress = FALSE)

ms_plot_require_columns(rq1_summary, c("metric", "metric_class", "dimension", "A_mean_absolute"),
                        "rq1_pairwise_summary.csv")
ms_plot_require_columns(observed,
  c("core_artifact_version", "rq1_analysis_version", "rq3_analysis_version", "dimension",
    "metric", "metric_class", "state_label", "requirement_rank", "R_obs", "status"),
  "rq3_sufficiency_long.rds")
ms_plot_require_columns(sufficiency,
  c("dimension", "metric", "metric_class", "epsilon", "sufficient", "status"),
  "rq3_sufficiency_long.csv")
ms_plot_require_columns(requirement,
  c("dimension", "metric", "epsilon", "sufficient_states", "sufficient_set_threshold_like"),
  "rq3_single_dimension_requirement.csv")
ms_plot_require_columns(unordered,
  c("dimension", "comparison_pair_id", "config_a_label", "config_b_label", "metric", "metric_class",
    "orientation_type", "epsilon_entry", "A", "B"),
  "rq3_unordered_substitutability.csv")
ms_plot_require_columns(coverage,
  c("dimension", "comparison_pair_id", "epsilon", "fraction_metrics_substitutable"),
  "rq3_unordered_coverage_curves.csv")
ms_plot_require_columns(convergence,
  c("dimension", "metric", "metric_class", "G", "requirement_position", "boundary_proximity"),
  "rq3_convergence_profile.csv")
ms_plot_require_columns(joint,
  c("core_artifact_version", "rq1_analysis_version", "rq3_analysis_version", "support_id", "placement",
    "optical", "resolution_s", "n_days", "metric", "status", "epsilon_entry",
    "worst_higher_config"),
  "rq3_joint_summary.csv")
ms_plot_require_columns(pareto_occupancy,
  c("support_id", "placement", "optical", "resolution_s", "n_days", "metric",
    "epsilon_interval_start", "epsilon_interval_end", "pareto"),
  "rq3_pareto_occupancy.csv")

RQ1_VERSION <- ms_plot_one_version(c(observed$rq1_analysis_version, joint$rq1_analysis_version),
                                   "rq1_analysis_version")
RQ3_VERSION <- ms_plot_one_version(c(observed$rq3_analysis_version, joint$rq3_analysis_version),
                                   "rq3_analysis_version")
CORE_VERSION <- ms_plot_assert_core(c(observed$core_artifact_version, joint$core_artifact_version))
ms_plot_assert_prefix(RQ1_VERSION, "rq1_v5_", "rq1_analysis_version")
ms_plot_assert_prefix(RQ3_VERSION, "rq3_v5_", "rq3_analysis_version")
if (!all(sort(unique(joint$resolution_s)) %in% sort(ms_primary_temporal_s()))) {
  stop("RQ3 joint artifact contains temporal states outside the frozen primary design", call. = FALSE)
}
if (!all(sort(unique(joint$n_days)) %in% DURATION_LEVELS)) {
  stop("RQ3 joint artifact contains duration states outside the frozen primary design", call. = FALSE)
}
if (!grepl(ms_analysis_design_id(), RQ3_VERSION, fixed = TRUE)) {
  stop("RQ3 plotting inputs do not match the current frozen analysis design", call. = FALSE)
}
metric_order <- ms_metric_order(rq1_summary)

safe_median <- function(x) {
  x <- x[is.finite(x)]
  if (length(x)) median(x) else NA_real_
}
safe_q <- function(x, p) {
  x <- x[is.finite(x)]
  if (length(x)) unname(quantile(x, p, names = FALSE)) else NA_real_
}
safe_mean <- function(x) {
  x <- x[is.finite(x)]
  if (length(x)) mean(x) else NA_real_
}

theme_rq3 <- function(base_size = 6.7, legend_position = "none") {
  theme_ms_axes(base_size = base_size, legend_position = legend_position)
}

metric_legend <- ms_metric_legend(text_size = 5.35, point_size = 1.5, key_width_mm = 3.5)

# =============================================================================
# Fig. 4 — tolerance determines the minimum sufficient burden
# =============================================================================

observed_display <- observed |>
  filter(dimension %in% ORDERED_DIMS, status == "resolved", is.finite(R_obs), is.finite(requirement_rank)) |>
  mutate(
    metric_class = factor(metric_class, levels = METRIC_CLASSES),
    dimension = factor(dimension, levels = ORDERED_DIMS, labels = unname(ORDERED_TITLES[ORDERED_DIMS])),
    class_offset = ms_class_offset(metric_class, span = .50, classes = METRIC_CLASSES),
    x_pos = requirement_rank + class_offset
  )

# a. Re-evaluate each metric on the pooled observed epsilon breakpoints within
# dimension. This is a display inversion of the existing R_obs thresholds only;
# it does not create a new sufficiency estimand.
requirement_grid <- bind_rows(lapply(ORDERED_DIMS, function(dim) {
  d <- observed |>
    filter(dimension == dim, status == "resolved", is.finite(R_obs), is.finite(requirement_rank))
  eps <- sort(unique(c(0, d$R_obs[is.finite(d$R_obs)])))
  metrics <- d |> distinct(metric, metric_class)
  bind_rows(lapply(seq_len(nrow(metrics)), function(i) {
    m <- metrics$metric[[i]]
    mc <- metrics$metric_class[[i]]
    g <- d |> filter(metric == m)
    rank_at <- vapply(eps, function(e) {
      ok <- g$requirement_rank[g$R_obs <= e + NUMERIC_TOL]
      if (length(ok)) min(ok) else NA_real_
    }, numeric(1))
    tibble(
      dimension = dim, metric = m, metric_class = mc,
      epsilon = eps, least_rank = rank_at
    )
  }))
})) |>
  mutate(metric_class = factor(metric_class, levels = METRIC_CLASSES))

requirement_summary <- requirement_grid |>
  group_by(dimension, metric_class, epsilon) |>
  summarise(
    n_metrics = n_distinct(metric),
    coverage = mean(is.finite(least_rank)),
    rank_median = safe_median(least_rank),
    rank_q25 = safe_q(least_rank, .25),
    rank_q75 = safe_q(least_rank, .75),
    .groups = "drop"
  ) |>
  mutate(
    dimension = factor(dimension, levels = ORDERED_DIMS,
                       labels = unname(ORDERED_TITLES[ORDERED_DIMS]))
  )

epsilon_values <- c(requirement_summary$epsilon, unordered$epsilon_entry)
epsilon_values <- epsilon_values[is.finite(epsilon_values) & epsilon_values >= 0]
epsilon_limit <- if (length(epsilon_values)) max(epsilon_values) * 1.02 else 1
if (!is.finite(epsilon_limit) || epsilon_limit <= 0) epsilon_limit <- 1

resolved_coverage <- requirement_grid |>
  group_by(dimension, epsilon) |>
  summarise(
    n_metrics = n_distinct(metric),
    coverage = mean(is.finite(least_rank)),
    .groups = "drop"
  ) |>
  mutate(
    dimension = factor(dimension, levels = ORDERED_DIMS,
                       labels = unname(ORDERED_TITLES[ORDERED_DIMS]))
  )

# b. Distribution of the empirical entry threshold R_obs at each ordered state.
observed_summary <- observed_display |>
  group_by(dimension, requirement_rank, metric_class) |>
  summarise(
    n_metrics = n_distinct(metric),
    R_median = median(R_obs, na.rm = TRUE),
    R_q25 = quantile(R_obs, .25, na.rm = TRUE, names = FALSE),
    R_q75 = quantile(R_obs, .75, na.rm = TRUE, names = FALSE),
    .groups = "drop"
  ) |>
  mutate(
    metric_class = factor(metric_class, levels = METRIC_CLASSES),
    class_offset = ms_class_offset(metric_class, span = .50, classes = METRIC_CLASSES),
    x_pos = requirement_rank + class_offset
  )
FIG4B_MAX_RESOLVED_RANK <- max(observed_summary$requirement_rank, na.rm = TRUE)

# c. Unordered dimensions are empirical substitutability curves. Reconstruct the
# all-metric ECDF directly from epsilon_entry so metric-class-specific source
# curves do not get accidentally connected together.
pair_ecdf <- unordered |>
  filter(dimension %in% c("placement", "optical"), is.finite(epsilon_entry)) |>
  mutate(pair = paste(config_a_label, "→", config_b_label)) |>
  group_by(dimension, comparison_pair_id, pair) |>
  group_modify(function(g, key) {
    eps <- sort(unique(c(0, g$epsilon_entry[is.finite(g$epsilon_entry)])))
    tibble(
      epsilon = eps,
      fraction_metrics_substitutable = vapply(
        eps, function(e) mean(g$epsilon_entry <= e + NUMERIC_TOL, na.rm = TRUE), numeric(1)
      )
    )
  }) |>
  ungroup() |>
  mutate(dimension = factor(dimension, levels = c("placement", "optical"),
                            labels = c("Placement", "Optical representation")))

pair_levels <- unique(pair_ecdf$pair)
pair_palette <- if (length(pair_levels) <= length(MS_THREE_COLORS)) {
  setNames(MS_THREE_COLORS[seq_along(pair_levels)], pair_levels)
} else {
  setNames(grDevices::hcl.colors(length(pair_levels), palette = "Dark 3"), pair_levels)
}
readr::write_csv(
  resolved_coverage |>
    mutate(dimension = as.character(dimension)),
  file.path("results", "rq3", "fig4_sufficient_metric_coverage.csv"), na = ""
)

readr::write_csv(
  requirement_summary |>
    mutate(metric_class = as.character(metric_class), dimension = as.character(dimension)),
  file.path("results", "rq3", "fig4_minimum_requirement_summary.csv"), na = ""
)
readr::write_csv(observed_display,
                 file.path("results", "rq3", "fig4_observed_stability.csv"), na = "")
readr::write_csv(pair_ecdf,
                 file.path("results", "rq3", "fig4_unordered_substitutability_ecdf.csv"), na = "")

# Supplement: retain the original detailed ordered-axis trajectories as audit views.
convergence_display <- convergence |>
  filter(dimension %in% ORDERED_DIMS, is.finite(G), is.finite(requirement_position)) |>
  group_by(dimension, metric, metric_class, requirement_position) |>
  summarise(G_display = median(G, na.rm = TRUE), .groups = "drop") |>
  mutate(metric_class = factor(metric_class, levels = METRIC_CLASSES))

p4s_convergence <- ggplot(
  convergence_display,
  aes(requirement_position, G_display, group = metric, color = metric_class)
) +
  geom_line(alpha = .46, linewidth = .36) + geom_point(size = .45, alpha = .58) +
  facet_wrap(~dimension, nrow = 1, scales = "free_x") +
  scale_color_ms_metric() +
  scale_y_continuous(trans = scales::transform_asinh(), breaks = scales::breaks_extended(n = 4)) +
  labs(title = "Adjacent-transition change by metric",
       x = "requirement position", y = "G = mean |z|") +
  theme_ms(base_size = 6.1, legend_position = "bottom")

sufficiency_plot <- sufficiency |>
  filter(dimension %in% ORDERED_DIMS) |>
  group_by(dimension, metric, metric_class, epsilon) |>
  summarise(
    resolved_fraction = mean(status == "resolved", na.rm = TRUE),
    fraction_sufficient = if (any(status == "resolved")) {
      mean(sufficient[status == "resolved"], na.rm = TRUE)
    } else NA_real_,
    .groups = "drop"
  ) |>
  mutate(metric_class = factor(metric_class, levels = METRIC_CLASSES))

p4s_sufficiency <- ggplot(
  sufficiency_plot,
  aes(epsilon, fraction_sufficient, group = metric, color = metric_class)
) +
  geom_line(alpha = .46, linewidth = .36) +
  facet_wrap(~dimension, nrow = 1, scales = "free_x") +
  scale_color_ms_metric() +
  scale_x_continuous(trans = scales::transform_asinh(), breaks = scales::breaks_extended(n = 4)) +
  scale_y_continuous(limits = c(0, 1), labels = scales::label_percent(accuracy = 25)) +
  labs(title = "Observed sufficiency projection by metric",
       x = "tolerance ε", y = "fraction of resolved states sufficient") +
  theme_ms(base_size = 6.1, legend_position = "bottom")

fig4s <- cowplot::plot_grid(p4s_convergence, p4s_sufficiency, ncol = 1, rel_heights = c(1, 1))
ms_plot_save(fig4s, file.path(OUT_DIR, "FigS_RQ3_single_dimension_detail.pdf"), 12.5, 8.0)
ms_plot_save(fig4s, file.path(OUT_DIR, "FigS_RQ3_single_dimension_detail.png"), 12.5, 8.0)


# =============================================================================
# Fig. 5 — joint temporal × duration sufficiency geometry
# =============================================================================

# Fig. 5 is restricted to the temporal-resolution × monitoring-duration joint
# design. Entry tolerance keeps the existing metric-equal pooling: support,
# placement and optical facets are collapsed within metric first, followed by
# the median across metrics. Pareto occupancy is read from the frozen
# epsilon-interval output; no Pareto rule is refit here.

metric_class_lookup5 <- rq1_summary |>
  distinct(metric, metric_class)

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
joint_plot_base <- joint_plot_base |>
  mutate(resolution_rank = match(resolution_s, fig5_res_levels))

# Equal-weight metric entry surface: facet pooling is performed within metric
# before the metric-level median, matching the previous joint landscape.
entry_metric_surface <- joint_plot_base |>
  filter(status == "resolved", is.finite(epsilon_entry), !is.na(metric_class)) |>
  group_by(metric, metric_class, resolution_s, resolution_rank, n_days) |>
  summarise(
    epsilon_metric = median(epsilon_entry, na.rm = TRUE),
    n_facets = n(),
    .groups = "drop"
  )

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
  summarise(
    cell_unresolved = any(status == "boundary_unresolved"),
    .groups = "drop"
  )

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

# Use one shared fill range for Panel a and the target-specific landscapes.
entry_fill_max5 <- max(
  c(entry_grid$epsilon_entry_median, entry_metric_surface$epsilon_metric),
  na.rm = TRUE
)
if (!is.finite(entry_fill_max5) || entry_fill_max5 <= 0) entry_fill_max5 <- 1
entry_fill_limits5 <- c(0, entry_fill_max5)

# Discrete tolerance boundaries: draw only the exposed edges of cells with
# median entry tolerance <= epsilon. This is a stepped decision aid, not a
# continuous contour or a new sufficiency estimate.
FIG5_TOLERANCE_SLICES <- c(.10, .25, .50, .75)
build_step_boundaries5 <- function(g, eps_levels, value_column,
                                   half_width = .46, facet_value = NULL) {
  is_sufficient <- function(x, y, eps) {
    z <- g[g$resolution_rank == x & g$n_days == y, , drop = FALSE]
    value <- z[[value_column]][[1]]
    nrow(z) == 1L && isTRUE(is.finite(value)) &&
      isTRUE(value <= eps + NUMERIC_TOL)
  }

  segments <- list()
  for (eps in eps_levels) {
    eps_label <- paste0("epsilon = ", sprintf("%.2f", eps))
    for (x in seq_along(fig5_res_levels)) {
      for (y in fig5_days) {
        if (!is_sufficient(x, y, eps)) next
        x0 <- x
        y0 <- y
        if (x == 1L || !is_sufficient(x - 1L, y, eps)) {
          segments[[length(segments) + 1L]] <- tibble(
            epsilon_label = eps_label, x = x0 - half_width, xend = x0 - half_width,
            y = y0 - half_width, yend = y0 + half_width
          )
        }
        if (x == length(fig5_res_levels) || !is_sufficient(x + 1L, y, eps)) {
          segments[[length(segments) + 1L]] <- tibble(
            epsilon_label = eps_label, x = x0 + half_width, xend = x0 + half_width,
            y = y0 - half_width, yend = y0 + half_width
          )
        }
        if (y == min(fig5_days) || !is_sufficient(x, y - 1L, eps)) {
          segments[[length(segments) + 1L]] <- tibble(
            epsilon_label = eps_label, x = x0 - half_width, xend = x0 + half_width,
            y = y0 - half_width, yend = y0 - half_width
          )
        }
        if (y == max(fig5_days) || !is_sufficient(x, y + 1L, eps)) {
          segments[[length(segments) + 1L]] <- tibble(
            epsilon_label = eps_label, x = x0 - half_width, xend = x0 + half_width,
            y = y0 + half_width, yend = y0 + half_width
          )
        }
      }
    }
  }
  out <- bind_rows(segments) |>
    mutate(
      epsilon_label = factor(
        epsilon_label,
        levels = paste0("epsilon = ", sprintf("%.2f", eps_levels))
      )
    )
  if (!is.null(facet_value)) out$metric <- facet_value
  out
}

# Exposed stepped edges for high-value regions on the discrete lattice. This is
# used for the Pareto-occupancy and class-level sufficiency atlases; it only
# summarizes already frozen cell values and does not interpolate between cells.
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
          x0 <- x
          y0 <- y
          if (x == 1L || !is_region(x - 1L, y)) {
            segments[[length(segments) + 1L]] <- tibble(
              facet_label = facet_label, region_level = threshold_labels[[j]],
              x = x0 - half_width, xend = x0 - half_width,
              y = y0 - half_width, yend = y0 + half_width
            )
          }
          if (x == length(fig5_res_levels) || !is_region(x + 1L, y)) {
            segments[[length(segments) + 1L]] <- tibble(
              facet_label = facet_label, region_level = threshold_labels[[j]],
              x = x0 + half_width, xend = x0 + half_width,
              y = y0 - half_width, yend = y0 + half_width
            )
          }
          if (y == min(fig5_days) || !is_region(x, y - 1L)) {
            segments[[length(segments) + 1L]] <- tibble(
              facet_label = facet_label, region_level = threshold_labels[[j]],
              x = x0 - half_width, xend = x0 + half_width,
              y = y0 - half_width, yend = y0 - half_width
            )
          }
          if (y == max(fig5_days) || !is_region(x, y + 1L)) {
            segments[[length(segments) + 1L]] <- tibble(
              facet_label = facet_label, region_level = threshold_labels[[j]],
              x = x0 - half_width, xend = x0 + half_width,
              y = y0 + half_width, yend = y0 + half_width
            )
          }
        }
      }
    }
  }

  if (!length(segments)) {
    return(tibble(
      facet_label = character(),
      region_level = factor(character(), levels = threshold_labels),
      x = numeric(), xend = numeric(), y = numeric(), yend = numeric()
    ))
  }
  bind_rows(segments) |>
    mutate(region_level = factor(region_level, levels = threshold_labels))
}

make_region_cells5 <- function(g, thresholds, value_column) {
  threshold_labels <- paste0(">=", sprintf("%d%%", round(100 * thresholds)))
  bind_rows(lapply(seq_along(thresholds), function(j) {
    g |>
      filter(
        !cell_unresolved,
        is.finite(.data[[value_column]]),
        .data[[value_column]] >= thresholds[[j]] - NUMERIC_TOL
      ) |>
      mutate(region_level = threshold_labels[[j]])
  })) |>
    mutate(region_level = factor(region_level, levels = threshold_labels))
}
threshold_boundaries5 <- build_step_boundaries5(
  entry_grid, FIG5_TOLERANCE_SLICES, "epsilon_entry_median"
)

# a. Overall joint entry-tolerance landscape. Unresolved boundary cells are
# explicitly marked U and use a separate neutral grey.
p5a <- ggplot(entry_grid, aes(resolution_rank, n_days, fill = epsilon_entry_median)) +
  geom_tile(width = .92, height = .92, color = "white", linewidth = .34) +
  geom_tile(
    data = entry_grid |> filter(cell_unresolved),
    aes(resolution_rank, n_days),
    inherit.aes = FALSE, fill = "#D8DCDE", color = "white", linewidth = .34
  ) +
  geom_text(
    aes(label = if_else(
      is.finite(epsilon_entry_median),
      formatC(epsilon_entry_median, format = "f", digits = 2), ""
    )),
    size = 1.48, color = "#4A5459", na.rm = TRUE
  ) +
  geom_text(
    data = entry_grid |> filter(cell_unresolved),
    aes(resolution_rank, n_days, label = "U"),
    inherit.aes = FALSE, size = 2.2, fontface = "bold", color = "#596064"
  ) +
  geom_segment(
    data = threshold_boundaries5,
    aes(x = x, y = y, xend = xend, yend = yend, linetype = epsilon_label),
    inherit.aes = FALSE, color = "#29383F", linewidth = .34, lineend = "butt"
  ) +
  scale_fill_ms_sequential(
    limits = entry_fill_limits5, oob = scales::squish,
    trans = scales::transform_asinh(),
    na.value = "#F0F2F2",
    name = "entry tolerance epsilon\n(R_obs)"
  ) +
  scale_linetype_manual(
    values = c("solid", "dashed", "dotdash", "dotted"),
    breaks = paste0("epsilon = ", sprintf("%.2f", FIG5_TOLERANCE_SLICES)),
    labels = sprintf("%.2f", FIG5_TOLERANCE_SLICES),
    name = "epsilon"
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
    legend.text = element_text(size = 4.15),
    legend.title = element_text(size = 4.25),
    legend.key.width = grid::unit(5.8, "mm"),
    legend.key.height = grid::unit(2.8, "mm"),
    legend.box = "horizontal",
    legend.box.just = "left",
    legend.spacing.x = grid::unit(3, "mm")
  ) +
  guides(
    fill = guide_colorbar(order = 1, title.position = "top", barwidth = grid::unit(20, "mm")),
    linetype = guide_legend(order = 2, nrow = 1, byrow = TRUE)
  )

# b. Pareto occupancy at explicit tolerance slices. These are the frozen
# interval-level Pareto flags, not tolerance-domain mean persistence. For a
# selected epsilon beyond a metric facet's last observed breakpoint, the last
# frozen interval is carried forward because the Pareto set is piecewise
# constant after the final R_obs breakpoint.
FIG5_PARETO_SLICES <- c(.10, .25, .50)
pareto_occupancy5 <- pareto_occupancy |>
  mutate(
    resolution_s = as.numeric(resolution_s),
    n_days = as.numeric(n_days),
    epsilon_interval_start = as.numeric(epsilon_interval_start),
    epsilon_interval_end = as.numeric(epsilon_interval_end),
    pareto = replace_na(as.logical(pareto), FALSE)
  )

select_pareto_interval5 <- function(g, eps) {
  g |>
    group_by(support_id, placement, optical, metric, resolution_s, n_days) |>
    filter({
      inside <- epsilon_interval_start <= eps + NUMERIC_TOL &
        epsilon_interval_end > eps + NUMERIC_TOL
      if (any(inside)) inside else epsilon_interval_end <= eps + NUMERIC_TOL
    }) |>
    slice_max(epsilon_interval_start, n = 1, with_ties = FALSE) |>
    ungroup()
}

pareto_slice5 <- bind_rows(lapply(FIG5_PARETO_SLICES, function(eps) {
  select_pareto_interval5(pareto_occupancy5, eps) |>
    mutate(epsilon = eps)
}))

joint_available5 <- joint_plot_base |>
  group_by(resolution_rank, n_days) |>
  summarise(
    n_available = n_distinct(paste(support_id, placement, optical, metric, sep = "|")),
    .groups = "drop"
  )

pareto_slice_counts5 <- pareto_slice5 |>
  group_by(epsilon, resolution_s, n_days) |>
  summarise(
    n_available = n_distinct(paste(support_id, placement, optical, metric, sep = "|")),
    .groups = "drop"
  ) |>
  mutate(resolution_rank = match(resolution_s, fig5_res_levels)) |>
  left_join(joint_available5, by = c("resolution_rank", "n_days"))
if (any(pareto_slice_counts5$n_available.x != pareto_slice_counts5$n_available.y,
        na.rm = TRUE)) {
  stop("Frozen Pareto occupancy slices do not cover the available joint metrics", call. = FALSE)
}

pareto_slice_summary5 <- pareto_slice5 |>
  group_by(epsilon, resolution_s, n_days) |>
  summarise(
    pareto_fraction = mean(pareto),
    .groups = "drop"
  )

pareto_grid5 <- tidyr::crossing(
  epsilon = FIG5_PARETO_SLICES,
  resolution_rank = seq_along(fig5_res_levels),
  n_days = fig5_days
) |>
  left_join(
    pareto_slice_summary5 |>
      mutate(
        resolution_rank = match(resolution_s, fig5_res_levels),
        epsilon_label = factor(
          paste0("epsilon = ", sprintf("%.2f", epsilon)),
          levels = paste0("epsilon = ", sprintf("%.2f", FIG5_PARETO_SLICES))
        )
      ) |>
      select(epsilon, epsilon_label, resolution_rank, n_days, pareto_fraction),
    by = c("epsilon", "resolution_rank", "n_days")
  ) |>
  left_join(joint_cell_status, by = c("resolution_rank", "n_days")) |>
  mutate(
    cell_unresolved = replace_na(cell_unresolved, FALSE),
    pareto_fraction = if_else(cell_unresolved, NA_real_, pareto_fraction)
  )

pareto_region_thresholds5 <- c(.25, .50, .75)
pareto_region_cells5 <- make_region_cells5(
  pareto_grid5, pareto_region_thresholds5, "pareto_fraction"
)
pareto_presence_cells5 <- pareto_grid5 |>
  filter(!cell_unresolved, is.finite(pareto_fraction), pareto_fraction > 0)
pareto_region_boundaries5 <- build_region_boundaries5(
  pareto_grid5, pareto_region_thresholds5, "pareto_fraction",
  group_column = "epsilon", facet_column = "epsilon_label"
) |>
  mutate(epsilon_label = factor(
    facet_label, levels = levels(pareto_grid5$epsilon_label)
  ))

p5b <- ggplot(pareto_grid5, aes(resolution_rank, n_days)) +
  geom_tile(
    width = .92, height = .92, fill = "#F7F9F9", color = "#E5EAEB", linewidth = .16
  ) +
  geom_tile(
    data = pareto_presence_cells5,
    aes(resolution_rank, n_days),
    inherit.aes = FALSE, width = .92, height = .92,
    fill = "#D7E8EA", color = NA
  ) +
  geom_tile(
    data = pareto_region_cells5,
    aes(resolution_rank, n_days, fill = region_level),
    inherit.aes = FALSE,
    width = .92, height = .92, alpha = .42, color = NA
  ) +
  geom_tile(
    data = pareto_grid5 |> filter(cell_unresolved),
    aes(resolution_rank, n_days),
    inherit.aes = FALSE, width = .92, height = .92,
    fill = "#D8DCDE", color = NA
  ) +
  geom_text(
    data = pareto_grid5 |> filter(cell_unresolved),
    aes(resolution_rank, n_days, label = "U"),
    inherit.aes = FALSE, size = 1.55, fontface = "bold", color = "#596064"
  ) +
  geom_segment(
    data = pareto_region_boundaries5,
    aes(
      x = x, y = y, xend = xend, yend = yend,
      linewidth = region_level
    ),
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
  scale_linewidth_manual(
    values = c(">=25%" = .30, ">=50%" = .43, ">=75%" = .58),
    guide = "none"
  ) +
  scale_x_continuous(
    breaks = seq_along(fig5_res_levels), labels = fig5_res_labels,
    expand = expansion(add = .16)
  ) +
  scale_y_continuous(
    breaks = fig5_days, labels = paste0(fig5_days, " d"),
    expand = expansion(add = .16)
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
    axis.text.y = element_text(size = 4.0),
    axis.title.y = element_blank(),
    strip.text = element_text(size = 4.7),
    plot.title = element_text(size = 6.0),
    plot.subtitle = element_text(size = 3.95, colour = "#666A6D", margin = margin(t = -1, b = 1)),
    panel.spacing = grid::unit(.6, "mm"),
    legend.text = element_text(size = 3.9),
    legend.title = element_text(size = 4.0),
    legend.key.width = grid::unit(5.8, "mm"),
    legend.key.height = grid::unit(2.6, "mm"),
    plot.margin = margin(2.5, 1.5, 1.5, 1.5)
  ) +
  guides(fill = guide_legend(title.position = "top", nrow = 1, byrow = TRUE))

# c. Class-level sufficient-region geometry. A common epsilon = 0.50 keeps
# all four requested classes structured rather than saturated in the current
# frozen surface. Class fractions are computed after metric-level facet pooling.
class_order5 <- c("timing", "duration", "level", "temporal dynamics")
class_name5 <- c(
  "timing" = "Timing",
  "duration" = "Duration",
  "level" = "Level",
  "temporal dynamics" = "Temporal dynamics"
)
class_threshold5 <- .50
class_metric_counts5 <- metric_class_lookup5 |>
  filter(metric_class %in% class_order5) |>
  group_by(metric_class) |>
  summarise(n_class_metrics = n_distinct(metric), .groups = "drop")

metric_cell_status_all5 <- joint_plot_base |>
  group_by(metric, resolution_rank, n_days) |>
  summarise(
    cell_unresolved = any(status == "boundary_unresolved"),
    .groups = "drop"
  )

class_surface5 <- entry_metric_surface |>
  filter(metric_class %in% class_order5) |>
  group_by(metric_class, resolution_rank, n_days) |>
  summarise(
    n_resolved_metrics = n_distinct(metric),
    suff_fraction = mean(epsilon_metric <= class_threshold5 + NUMERIC_TOL),
    .groups = "drop"
  )

class_status5 <- metric_cell_status_all5 |>
  left_join(metric_class_lookup5, by = "metric") |>
  filter(metric_class %in% class_order5) |>
  group_by(metric_class, resolution_rank, n_days) |>
  summarise(class_unresolved = any(cell_unresolved), .groups = "drop")

class_grid5 <- tidyr::crossing(
  metric_class = class_order5,
  resolution_rank = seq_along(fig5_res_levels),
  n_days = fig5_days
) |>
  left_join(class_surface5, by = c("metric_class", "resolution_rank", "n_days")) |>
  left_join(class_metric_counts5, by = "metric_class") |>
  left_join(class_status5, by = c("metric_class", "resolution_rank", "n_days")) |>
  mutate(
    class_unresolved = replace_na(class_unresolved, FALSE),
    cell_unresolved = class_unresolved,
    suff_fraction = if_else(class_unresolved, NA_real_, suff_fraction),
    class_label = paste0(
      unname(class_name5[metric_class]),
      " (n = ", n_class_metrics, ")"
    ),
    class_label = factor(
      class_label,
      levels = paste0(unname(class_name5[class_order5]),
                      " (n = ", class_metric_counts5$n_class_metrics[match(
                        class_order5, class_metric_counts5$metric_class
                      )], ")")
    )
  )

class_region_thresholds5 <- c(.25, .50, .75)
class_region_cells5 <- make_region_cells5(
  class_grid5, class_region_thresholds5, "suff_fraction"
)
class_region_boundaries5 <- build_region_boundaries5(
  class_grid5, class_region_thresholds5, "suff_fraction",
  group_column = "metric_class", facet_column = "class_label"
) |>
  mutate(class_label = factor(
    facet_label, levels = levels(class_grid5$class_label)
  ))

class_region_fill_alpha5 <- c(">=25%" = .08, ">=50%" = .16, ">=75%" = .25)
class_region_line_types5 <- c(">=25%" = "dotted", ">=50%" = "solid", ">=75%" = "dashed")
class_region_line_widths5 <- c(">=25%" = .25, ">=50%" = .45, ">=75%" = .58)

p5c <- ggplot(class_grid5, aes(resolution_rank, n_days)) +
  geom_tile(
    width = .92, height = .92, fill = "#FAFBFB", color = "#E5EAEB", linewidth = .16
  ) +
  geom_tile(
    data = class_region_cells5,
    aes(resolution_rank, n_days, alpha = region_level),
    inherit.aes = FALSE, width = .92, height = .92,
    fill = "#5A879B", color = NA
  ) +
  geom_tile(
    data = class_grid5 |> filter(class_unresolved),
    aes(resolution_rank, n_days),
    inherit.aes = FALSE, width = .92, height = .92,
    fill = "#D8DCDE", color = NA
  ) +
  geom_text(
    data = class_grid5 |> filter(class_unresolved),
    aes(resolution_rank, n_days, label = "U"),
    inherit.aes = FALSE, size = 1.65, fontface = "bold", color = "#596064"
  ) +
  geom_segment(
    data = class_region_boundaries5,
    aes(
      x = x, y = y, xend = xend, yend = yend,
      linetype = region_level, linewidth = region_level
    ),
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
    breaks = seq_along(fig5_res_levels), labels = fig5_res_labels,
    expand = expansion(add = .16)
  ) +
  scale_y_continuous(
    breaks = fig5_days, labels = paste0(fig5_days, " d"),
    expand = expansion(add = .16)
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
    axis.text.y = element_text(size = 3.55),
    strip.text = element_text(size = 4.3),
    plot.title = element_text(size = 6.2),
    plot.subtitle = element_text(size = 3.55, colour = "#666A6D", margin = margin(t = -1, b = 1)),
    panel.spacing = grid::unit(1.2, "mm"),
    legend.text = element_text(size = 3.9),
    legend.title = element_text(size = 4.0),
    legend.key.width = grid::unit(5.8, "mm"),
    legend.key.height = grid::unit(2.6, "mm"),
    plot.margin = margin(2.5, 2, 1.5, 2)
  ) +
  guides(
    linetype = guide_legend(
      title.position = "top", nrow = 1, byrow = TRUE
    )
  )

fig5_top <- cowplot::plot_grid(
  p5a, p5b, ncol = 2, rel_widths = c(.55, .45),
  align = "hv", axis = "tblr", greedy = TRUE
)
fig5_body <- cowplot::plot_grid(
  fig5_top, p5c, ncol = 1, rel_heights = c(.68, .82),
  align = "v", axis = "l", greedy = TRUE
)
ms_plot_save(fig5_body, file.path(OUT_DIR, "Fig5_RQ3.png"), 7.2, 5.8)

ms_plot_write_manifest(
  file.path(OUT_DIR, "figure_artifact_manifest.csv"),
  tibble(
    figure = c(
      "Fig4_RQ3", "Fig5_RQ3",
      "FigS_RQ3_single_dimension_detail"
    ),
    input_artifact = c(
      "rq3_observed_stability+sufficiency+unordered_substitutability",
      "rq3_joint_summary+rq3_pareto_occupancy",
      "rq3_convergence_profile+sufficiency"
    ),
    core_artifact_version = CORE_VERSION,
    rq1_analysis_version = RQ1_VERSION,
    rq2_analysis_version = NA_character_,
    rq3_analysis_version = RQ3_VERSION
  )
)
# -----------------------------------------------------------------------------
# Main-text display refinement for Fig. 4.
# Tolerance spans close to an order of magnitude and includes zero. A log1p axis
# preserves zero and uses familiar logarithmic compression. Each tolerance facet
# uses its own observed x-range so the shorter placement/optical and duration
# domains do not inherit the temporal long tail. Breaks remain explicit so the
# decision-relevant low-tolerance region is not left with only a few automatic
# ticks.
# -----------------------------------------------------------------------------
epsilon_log1p <- scales::trans_new(
  name = "log1p",
  transform = base::log1p,
  inverse = base::expm1,
  domain = c(0, Inf)
)
epsilon_tick_candidates <- c(0, .1, .2, .5, 1, 2, 3, 5, 7, 10)
epsilon_tick_labels <- c("0", "0.1", "0.2", "0.5", "1", "2", "3", "5", "7", "10")
epsilon_tick_keep <- epsilon_tick_candidates <= epsilon_limit + NUMERIC_TOL
epsilon_ticks <- epsilon_tick_candidates[epsilon_tick_keep]
epsilon_labels <- epsilon_tick_labels[epsilon_tick_keep]

FIG4_DIM_LABELS <- c(
  "Temporal resolution" = paste0("Temporal resolution · ", RES_LABELS[[1]], " → ", tail(RES_LABELS, 1)),
  "Monitoring duration" = paste0("Monitoring duration · ", min(DURATION_LEVELS), " d → ", max(DURATION_LEVELS), " d")
)
fig5_slice_guides <- FIG5_TOLERANCE_SLICES[FIG5_TOLERANCE_SLICES <= epsilon_limit + NUMERIC_TOL]

# a. Class-wise distribution ribbons. Each metric contributes one minimum
# sufficient state at each pooled observed R_obs breakpoint. The threshold-like
# check is retained from the frozen RQ3 requirement rule: a non-threshold-like
# sufficient set is displayed as unresolved rather than being forced into a
# least-demanding state. This is a display inversion of frozen R_obs values only;
# it does not create a new sufficiency estimand.
p4a_class_order <- c(
  "timing", "duration", "level", "temporal dynamics", "exposure history", "spectrum"
)
p4a_metric_inventory <- rq1_summary |>
  filter(dimension %in% ORDERED_DIMS) |>
  distinct(dimension, metric, metric_class) |>
  mutate(metric_class = as.character(metric_class))

p4a_class_counts <- p4a_metric_inventory |>
  group_by(metric_class) |>
  summarise(n_metrics = n_distinct(metric), .groups = "drop")
if (!setequal(p4a_class_counts$metric_class, p4a_class_order)) {
  stop("Panel a metric classes do not match the frozen six-class inventory", call. = FALSE)
}

p4a_row_map <- tibble(
  row_key = c("overall", p4a_class_order),
  row_y = c(7.20, 6:1),
  row_label = c(
    "Overall",
    paste0(
      str_to_sentence(p4a_class_order),
      " (n=",
      p4a_class_counts$n_metrics[match(p4a_class_order, p4a_class_counts$metric_class)],
      ")"
    )
  )
)

p4a_state_labels <- list(
  temporal = RES_LABELS,
  duration = paste0(DURATION_LEVELS, " d")
)
p4a_state_key <- function(dimension, rank) {
  if (!is.finite(rank)) return("unresolved / no resolved sufficient state")
  prefix <- if (identical(dimension, "temporal")) "Temporal: " else "Duration: "
  paste0(prefix, p4a_state_labels[[dimension]][[as.integer(rank)]])
}

p4a_min_rank <- function(g, epsilon) {
  z <- g |>
    filter(status == "resolved", is.finite(R_obs), is.finite(requirement_rank)) |>
    arrange(requirement_rank)
  if (!nrow(z)) return(NA_real_)

  is_sufficient <- z$R_obs <= epsilon + NUMERIC_TOL
  # The RQ3 estimand reports a least-demanding state only when the resolved
  # sufficient set is threshold-like in increasing burden order.
  if (!any(is_sufficient) || any(diff(as.integer(is_sufficient)) < 0L)) {
    return(NA_real_)
  }
  min(z$requirement_rank[is_sufficient])
}

p4a_epsilon_limit <- max(
  c(.75, observed$R_obs[observed$dimension %in% ORDERED_DIMS & is.finite(observed$R_obs)]),
  na.rm = TRUE
)
if (!is.finite(p4a_epsilon_limit) || p4a_epsilon_limit <= 0) p4a_epsilon_limit <- 1
p4a_epsilon_limit <- p4a_epsilon_limit * 1.02

p4a_epsilon_intervals <- bind_rows(lapply(ORDERED_DIMS, function(dim) {
  d <- observed |>
    filter(dimension == dim, status == "resolved", is.finite(R_obs))
  tibble(
    dimension = dim,
    epsilon = sort(unique(c(0, d$R_obs, p4a_epsilon_limit)))
  )
})) |>
  group_by(dimension) |>
  arrange(epsilon, .by_group = TRUE) |>
  mutate(epsilon_end = lead(epsilon)) |>
  ungroup() |>
  filter(is.finite(epsilon_end), epsilon_end > epsilon + NUMERIC_TOL)

p4a_metric_states <- bind_rows(lapply(ORDERED_DIMS, function(dim) {
  metrics <- p4a_metric_inventory |>
    filter(dimension == dim) |>
    arrange(match(metric_class, p4a_class_order), metric)
  eps <- p4a_epsilon_intervals |>
    filter(dimension == dim) |>
    pull(epsilon)
  bind_rows(lapply(seq_len(nrow(metrics)), function(i) {
    m <- metrics$metric[[i]]
    g <- observed |>
      filter(dimension == dim, metric == m)
    tibble(
      dimension = dim,
      metric = m,
      metric_class = metrics$metric_class[[i]],
      epsilon = eps,
      minimum_rank = vapply(eps, function(e) p4a_min_rank(g, e), numeric(1))
    )
  }))
})) |>
  mutate(row_key = metric_class)
p4a_metric_states$state_key <- vapply(
  seq_len(nrow(p4a_metric_states)),
  function(i) p4a_state_key(
    p4a_metric_states$dimension[[i]], p4a_metric_states$minimum_rank[[i]]
  ),
  character(1)
)

# Reuse the same per-metric state assignments for the pooled Overall row. This
# preserves equal metric weighting while keeping the six class rows separate.
p4a_metric_states <- bind_rows(
  p4a_metric_states,
  p4a_metric_states |>
    mutate(metric_class = NA_character_, row_key = "overall")
)

p4a_state_order <- c(
  paste0("Temporal: ", p4a_state_labels$temporal),
  paste0("Duration: ", p4a_state_labels$duration),
  "unresolved / no resolved sufficient state"
)

p4a_ribbon_data <- p4a_metric_states |>
  left_join(p4a_row_map |> select(row_key, row_y), by = "row_key") |>
  left_join(p4a_epsilon_intervals, by = c("dimension", "epsilon")) |>
  mutate(state_key = factor(state_key, levels = p4a_state_order)) |>
  group_by(dimension, row_key, row_y, epsilon, epsilon_end, state_key) |>
  summarise(n_metrics = n_distinct(metric), .groups = "drop") |>
  group_by(dimension, row_key, row_y, epsilon, epsilon_end) |>
  mutate(
    fraction = n_metrics / sum(n_metrics),
    ribbon_ymin = row_y - .39 + .78 * lag(cumsum(fraction), default = 0),
    ribbon_ymax = ribbon_ymin + .78 * fraction
  ) |>
  ungroup() |>
  mutate(
    dimension = factor(
      dimension, levels = ORDERED_DIMS,
      labels = unname(c(temporal = "Temporal resolution", duration = "Monitoring duration"))
    )
  )

p4a_burden_palette <- c("#E8F0F3", "#C8DCE4", "#A5C5D1", "#7FA8B9", "#568BA1", "#2F5D7E")
p4a_fill_values <- c(
  setNames(p4a_burden_palette, paste0("Temporal: ", p4a_state_labels$temporal)),
  setNames(p4a_burden_palette, paste0("Duration: ", p4a_state_labels$duration)),
  "unresolved / no resolved sufficient state" = "#BFC5C8"
)
p4a_fill_breaks <- c(
  paste0("Temporal: ", p4a_state_labels$temporal),
  paste0("Duration: ", p4a_state_labels$duration),
  "unresolved / no resolved sufficient state"
)

p4a_ticks <- c(0, .10, .25, .50, .75, 1, 2, 3, 5)
p4a_tick_keep <- p4a_ticks <= p4a_epsilon_limit + NUMERIC_TOL
p4a_ticks <- p4a_ticks[p4a_tick_keep]
p4a_tick_labels <- c("0", "0.10", "0.25", "0.50", "0.75", "1", "2", "3", "5")[p4a_tick_keep]

p4a <- ggplot() +
  geom_rect(
    data = p4a_ribbon_data,
    aes(
      xmin = epsilon, xmax = epsilon_end,
      ymin = ribbon_ymin, ymax = ribbon_ymax, fill = state_key
    ),
    colour = NA
  ) +
  geom_vline(
    xintercept = c(.10, .25, .50, .75),
    linewidth = .20, linetype = 3, colour = "#BFC5C8", alpha = .72
  ) +
  facet_grid(. ~ dimension, scales = "fixed", space = "fixed") +
  scale_fill_manual(
    values = p4a_fill_values, breaks = p4a_fill_breaks,
    drop = FALSE, name = "requirement state\n(darker = higher burden)"
  ) +
  scale_x_continuous(
    trans = epsilon_log1p,
    limits = c(0, p4a_epsilon_limit),
    breaks = p4a_ticks, labels = p4a_tick_labels,
    expand = expansion(mult = c(0, 0))
  ) +
  scale_y_continuous(
    breaks = p4a_row_map$row_y, labels = p4a_row_map$row_label,
    limits = c(.48, 7.62), expand = expansion(mult = c(0, 0))
  ) +
  labs(
    title = "a  Tolerance sets the minimum sufficient measurement burden",
    subtitle = "100% stacked distribution of minimum sufficient states within each metric group; grey = unresolved / no resolved sufficient state",
    x = "tolerance epsilon", y = NULL
  ) +
  theme_rq3(base_size = 6.0, legend_position = "bottom") +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(colour = "#F0F1F2", linewidth = .18),
    panel.spacing = grid::unit(4.5, "mm"),
    axis.text.x = element_text(size = 4.65),
    axis.text.y = element_text(size = 4.75, hjust = 1),
    axis.ticks.x = element_line(colour = "#505457", linewidth = .25),
    strip.text = element_text(size = 6.0),
    plot.subtitle = element_text(size = 4.45, colour = "#666A6D", margin = margin(t = -1, b = 2)),
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.text = element_text(size = 4.15),
    legend.title = element_text(size = 4.25),
    legend.key.width = grid::unit(6.3, "mm"),
    legend.key.height = grid::unit(2.7, "mm"),
    legend.spacing.x = grid::unit(2.3, "mm"),
    plot.margin = margin(2, 3, 0, 3)
  ) +
  guides(fill = guide_legend(nrow = 2, byrow = TRUE, override.aes = list(colour = NA)))

# R_obs stays on its original linear scale; only the background raw points carry
# the long tail. The highest observed boundary is unresolved and omitted, so the
# rank axis ends at the highest resolved observed requirement.
p4b <- ggplot() +
  geom_point(
    data = observed_display,
    aes(x_pos, R_obs, color = metric_class),
    position = position_jitter(width = .018, height = 0, seed = 91),
    size = .52, alpha = .16
  ) +
  geom_linerange(
    data = observed_summary,
    aes(x_pos, ymin = R_q25, ymax = R_q75, color = metric_class),
    linewidth = .42, alpha = .46
  ) +
  geom_point(
    data = observed_summary,
    aes(x_pos, R_median, color = metric_class),
    shape = 18, size = 1.6
  ) +
  facet_wrap(~dimension, nrow = 1, labeller = as_labeller(FIG4_DIM_LABELS)) +
  scale_color_ms_metric(guide = "none") +
  scale_x_continuous(
    breaks = seq_len(FIG4B_MAX_RESOLVED_RANK),
    limits = c(.65, FIG4B_MAX_RESOLVED_RANK + .35),
    labels = as.character(seq_len(FIG4B_MAX_RESOLVED_RANK))
  ) +
  scale_y_continuous(breaks = scales::breaks_extended(n = 5)) +
  labs(
    title = "b  Residual instability contracts as measurement burden increases",
    subtitle = "highest observed boundary is unresolved and omitted",
    x = "requirement rank (low → high burden)", y = "R_obs = max A to higher observed states"
  ) +
  theme_rq3(base_size = 6.35) +
  theme(
    panel.grid.major.x = element_blank(), strip.text = element_text(size = 5.9),
    plot.subtitle = element_text(size = 4.8, colour = "#666A6D", margin = margin(t = -1, b = 2))
  )

pair_e50 <- pair_ecdf |>
  group_by(dimension, comparison_pair_id, pair) |>
  summarise(
    epsilon50 = if (any(fraction_metrics_substitutable >= .5)) {
      min(epsilon[fraction_metrics_substitutable >= .5], na.rm = TRUE)
    } else NA_real_,
    .groups = "drop"
  ) |>
  filter(is.finite(epsilon50))

p4c <- ggplot(
  pair_ecdf,
  aes(
    epsilon, fraction_metrics_substitutable,
    color = pair,
    group = interaction(dimension, comparison_pair_id, drop = TRUE)
  )
) +
  geom_vline(xintercept = fig5_slice_guides, linewidth = .22, linetype = 3, color = "#C5C9CC") +
  geom_hline(yintercept = .5, linewidth = .24, linetype = 3, color = "#B4B8BB") +
  geom_step(linewidth = .76, alpha = .94, lineend = "butt", linejoin = "mitre") +
  geom_point(
    data = pair_e50, aes(epsilon50, .5, color = pair),
    inherit.aes = FALSE, shape = 21, fill = "white", size = 1.35, stroke = .45
  ) +
  facet_wrap(~dimension, nrow = 1, scales = "free_x") +
  scale_color_manual(values = pair_palette, breaks = pair_levels, name = NULL) +
  scale_x_continuous(
    trans = epsilon_log1p,
    breaks = epsilon_ticks,
    labels = epsilon_labels,
    expand = expansion(mult = c(0, .01))
  ) +
  scale_y_continuous(limits = c(0, 1), labels = scales::label_percent(accuracy = 25)) +
  labs(
    title = "c  Target-aligned alternatives become substitutable as tolerance relaxes",
    subtitle = "open points = ε50; faint vertical guides = Fig. 5 tolerance slices",
    x = "tolerance ε", y = "fraction of metrics substitutable"
  ) +
  theme_rq3(base_size = 6.3, legend_position = "bottom") +
  theme(
    panel.grid.major.x = element_line(colour = "#ECEFF0", linewidth = .20),
    strip.text = element_text(size = 5.8),
    plot.subtitle = element_text(size = 4.45, colour = "#666A6D", margin = margin(t = -1, b = 1.5)),
    legend.text = element_text(size = 5.0),
    legend.key.width = grid::unit(5.0, "mm")
  )

fig4_bottom <- cowplot::plot_grid(
  p4b, p4c, ncol = 2, rel_widths = c(1.08, .92),
  align = "hv", axis = "tblr", greedy = TRUE
)
fig4_body <- cowplot::plot_grid(
  p4a, fig4_bottom, ncol = 1, rel_heights = c(1.20, .80),
  align = "v", axis = "l", greedy = TRUE
)
fig4 <- cowplot::plot_grid(
  metric_legend, fig4_body, ncol = 1,
  rel_heights = c(.042, 1), align = "v", axis = "l", greedy = TRUE
)
ms_plot_save(fig4, file.path(OUT_DIR, "Fig4_RQ3.pdf"), 9.0, 6.2)
ms_plot_save(fig4, file.path(OUT_DIR, "Fig4_RQ3.png"), 9.0, 6.2)
readr::write_csv(pair_e50, file.path("results", "rq3", "fig4_unordered_epsilon50.csv"), na = "")

message("RQ3 v5 figures complete: single-dimension sufficiency and joint tolerance landscapes")
