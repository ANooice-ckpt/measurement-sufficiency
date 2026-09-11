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
  x <- as.numeric(x)
  x <- x[is.finite(x)]
  if (length(x)) mean(x) else NA_real_
}

theme_rq3 <- function(base_size = 6.7, legend_position = "none") {
  theme_ms_axes(base_size = base_size, legend_position = legend_position)
}

metric_legend <- ms_metric_legend(text_size = 5.35, point_size = 1.5, key_width_mm = 3.5)

# =============================================================================
# Fig. 5 — joint temporal × duration sufficiency phase diagrams
# =============================================================================

# The measurement lattice is intrinsically discrete (6 temporal states × 6
# monitoring durations). All fields below therefore retain discrete cells and
# stepped boundaries. No interpolation, smoothing or monotonicity is imposed.
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
format_resolution_compact5 <- function(x) {
  x <- as.numeric(x)
  ifelse(
    x >= 60 & abs(x / 60 - round(x / 60)) < 1e-9,
    paste0(format(round(x / 60), trim = TRUE), "m"),
    paste0(format(x, trim = TRUE), "s")
  )
}
fig5_res_labels <- format_resolution5(fig5_res_levels)
fig5_res_labels_compact <- format_resolution_compact5(fig5_res_levels)
joint_plot_base <- joint_plot_base |>
  mutate(resolution_rank = match(resolution_s, fig5_res_levels))

# Equal-weight metric entry surface: support / placement / optical facets are
# collapsed within metric first, followed by the metric-level distribution.
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

entry_fill_max5 <- max(
  c(entry_grid$epsilon_entry_median, entry_metric_surface$epsilon_metric),
  na.rm = TRUE
)
if (!is.finite(entry_fill_max5) || entry_fill_max5 <= 0) entry_fill_max5 <- 1
entry_fill_limits5 <- c(0, entry_fill_max5)

# Build exposed cell edges for one scientifically interpretable threshold. The
# helper supports both <= thresholds (entry tolerance) and >= thresholds
# (occupancy / sufficient fractions), optionally within facets.
build_stepped_boundary5 <- function(g, threshold, value_column,
                                    direction = c("ge", "le"),
                                    facet_column = NULL,
                                    half_width = .46) {
  direction <- match.arg(direction)
  facet_values <- if (is.null(facet_column)) {
    NA_character_
  } else {
    unique(as.character(g[[facet_column]]))
  }
  facet_values <- facet_values[is.na(facet_values) | nzchar(facet_values)]
  segments <- list()

  for (facet_value in facet_values) {
    gs <- if (is.null(facet_column)) {
      g
    } else {
      g[as.character(g[[facet_column]]) == facet_value, , drop = FALSE]
    }
    if (!nrow(gs)) next

    is_region <- function(x, y) {
      z <- gs[gs$resolution_rank == x & gs$n_days == y, , drop = FALSE]
      if (nrow(z) != 1L) return(FALSE)
      unresolved <- if ("cell_unresolved" %in% names(z)) {
        isTRUE(z$cell_unresolved[[1]])
      } else {
        FALSE
      }
      value <- z[[value_column]][[1]]
      if (unresolved || !isTRUE(is.finite(value))) return(FALSE)
      if (direction == "ge") {
        value >= threshold - NUMERIC_TOL
      } else {
        value <= threshold + NUMERIC_TOL
      }
    }

    add_edge <- function(x, xend, y, yend) {
      row <- tibble(x = x, xend = xend, y = y, yend = yend)
      if (!is.null(facet_column)) row[[facet_column]] <- facet_value
      segments[[length(segments) + 1L]] <<- row
    }

    for (x in seq_along(fig5_res_levels)) {
      for (y in fig5_days) {
        if (!is_region(x, y)) next
        if (x == 1L || !is_region(x - 1L, y)) {
          add_edge(x - half_width, x - half_width, y - half_width, y + half_width)
        }
        if (x == length(fig5_res_levels) || !is_region(x + 1L, y)) {
          add_edge(x + half_width, x + half_width, y - half_width, y + half_width)
        }
        if (y == min(fig5_days) || !is_region(x, y - 1L)) {
          add_edge(x - half_width, x + half_width, y - half_width, y - half_width)
        }
        if (y == max(fig5_days) || !is_region(x, y + 1L)) {
          add_edge(x - half_width, x + half_width, y + half_width, y + half_width)
        }
      }
    }
  }

  if (!length(segments)) {
    out <- tibble(x = numeric(), xend = numeric(), y = numeric(), yend = numeric())
    if (!is.null(facet_column)) out[[facet_column]] <- character()
    return(out)
  }
  bind_rows(segments)
}

FIG5_TOLERANCE_COLORS <- c(
  "#FBFAF7", "#F0E4CC", "#DEC08B", "#BC8C4D", "#80562C"
)
FIG5_FRACTION_COLORS <- c(
  "#FAFBFB", "#E4ECEE", "#BCD1D7", "#7FA8B7", "#365F74"
)
FIG5_UNRESOLVED <- "#D8DCDE"
FIG5_CELL_BORDER <- "#F0F2F2"

# -----------------------------------------------------------------------------
# a. Entry-tolerance phase diagram
# -----------------------------------------------------------------------------
# The fill carries the continuous entry-tolerance information. Only two decision
# contours remain: epsilon = .50 is the primary frontier; .25 is a weak reference.
entry_boundary25 <- build_stepped_boundary5(
  entry_grid, .25, "epsilon_entry_median", direction = "le"
)
entry_boundary50 <- build_stepped_boundary5(
  entry_grid, .50, "epsilon_entry_median", direction = "le"
)

# -----------------------------------------------------------------------------
# b. Pareto occupancy as a continuous phase field
# -----------------------------------------------------------------------------
# Pareto flags are read from the frozen epsilon-interval artifact. At each
# tolerance slice, the cell fill is the fraction of available metric-facet
# combinations occupying the frozen Pareto set; the only contour is 50%.
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
  summarise(pareto_fraction = mean(pareto), .groups = "drop")

pareto_grid5 <- tidyr::crossing(
  epsilon = FIG5_PARETO_SLICES,
  resolution_rank = seq_along(fig5_res_levels),
  n_days = fig5_days
) |>
  left_join(
    pareto_slice_summary5 |>
      mutate(resolution_rank = match(resolution_s, fig5_res_levels)) |>
      select(epsilon, resolution_rank, n_days, pareto_fraction),
    by = c("epsilon", "resolution_rank", "n_days")
  ) |>
  left_join(joint_cell_status, by = c("resolution_rank", "n_days")) |>
  mutate(
    epsilon_label = factor(
      paste0("ε = ", sprintf("%.2f", epsilon)),
      levels = paste0("ε = ", sprintf("%.2f", FIG5_PARETO_SLICES))
    ),
    cell_unresolved = replace_na(cell_unresolved, FALSE),
    pareto_fraction = if_else(cell_unresolved, NA_real_, pareto_fraction)
  )

pareto_boundary50 <- build_stepped_boundary5(
  pareto_grid5, .50, "pareto_fraction", direction = "ge",
  facet_column = "epsilon_label"
) |>
  mutate(epsilon_label = factor(
    epsilon_label, levels = levels(pareto_grid5$epsilon_label)
  ))

# -----------------------------------------------------------------------------
# c. Class-specific sufficient-fraction phase diagrams
# -----------------------------------------------------------------------------
# At the shared epsilon = .50 slice, each cell is the fraction of metrics in the
# class whose pooled entry tolerance is <= .50. Facets are ordered stringent to
# permissive by the share of resolved lattice cells with >= 50% metrics sufficient.
class_candidates5 <- c("timing", "duration", "level", "temporal dynamics")
class_name5 <- c(
  "timing" = "Timing",
  "duration" = "Duration",
  "level" = "Level",
  "temporal dynamics" = "Temporal dynamics"
)
class_threshold5 <- .50
class_metric_counts5 <- metric_class_lookup5 |>
  filter(metric_class %in% class_candidates5) |>
  group_by(metric_class) |>
  summarise(n_class_metrics = n_distinct(metric), .groups = "drop")

metric_cell_status_all5 <- joint_plot_base |>
  group_by(metric, resolution_rank, n_days) |>
  summarise(
    cell_unresolved = any(status == "boundary_unresolved"),
    .groups = "drop"
  )

class_surface5 <- entry_metric_surface |>
  filter(metric_class %in% class_candidates5) |>
  group_by(metric_class, resolution_rank, n_days) |>
  summarise(
    n_resolved_metrics = n_distinct(metric),
    suff_fraction = mean(epsilon_metric <= class_threshold5 + NUMERIC_TOL),
    .groups = "drop"
  )

class_status5 <- metric_cell_status_all5 |>
  left_join(metric_class_lookup5, by = "metric") |>
  filter(metric_class %in% class_candidates5) |>
  group_by(metric_class, resolution_rank, n_days) |>
  summarise(class_unresolved = any(cell_unresolved), .groups = "drop")

class_grid_raw5 <- tidyr::crossing(
  metric_class = class_candidates5,
  resolution_rank = seq_along(fig5_res_levels),
  n_days = fig5_days
) |>
  left_join(class_surface5, by = c("metric_class", "resolution_rank", "n_days")) |>
  left_join(class_metric_counts5, by = "metric_class") |>
  left_join(class_status5, by = c("metric_class", "resolution_rank", "n_days")) |>
  mutate(
    class_unresolved = replace_na(class_unresolved, FALSE),
    cell_unresolved = class_unresolved,
    suff_fraction = if_else(class_unresolved, NA_real_, suff_fraction)
  )

class_rank5 <- class_grid_raw5 |>
  group_by(metric_class) |>
  summarise(
    region_share = safe_mean(as.numeric(suff_fraction >= .50)),
    mean_sufficient_fraction = safe_mean(suff_fraction),
    .groups = "drop"
  ) |>
  arrange(region_share, mean_sufficient_fraction, metric_class) |>
  mutate(
    class_rank = row_number(),
    class_label = paste0(
      unname(class_name5[metric_class]), " · ",
      if_else(is.finite(region_share), sprintf("%.0f%%", 100 * region_share), "NA")
    )
  )
class_order5 <- class_rank5$metric_class
class_label_levels5 <- class_rank5$class_label

class_grid5 <- class_grid_raw5 |>
  left_join(class_rank5, by = "metric_class") |>
  mutate(class_label = factor(class_label, levels = class_label_levels5))

class_boundary50 <- build_stepped_boundary5(
  class_grid5, .50, "suff_fraction", direction = "ge",
  facet_column = "class_label"
) |>
  mutate(class_label = factor(
    class_label, levels = levels(class_grid5$class_label)
  ))

# Fig. 6 redesign consumes the same display grids without changing their values.
source("scripts/utils/fig6_redesign.R")
fig6_display <- ms_fig6_redesign(entry_grid, pareto_grid5, class_grid5,
                                fig5_res_labels_compact, fig5_days)
fig6_redesigned <- fig6_display$plot
p5a <- fig6_display$a; p5b <- fig6_display$b; p5c <- fig6_display$c
ms_plot_save(fig6_redesigned, file.path(OUT_DIR, "Fig5_RQ3.png"), 7.40, 6.10)

ms_plot_write_manifest(
  file.path(OUT_DIR, "figure_artifact_manifest.csv"),
  tibble(
    figure = "Fig5_RQ3",
    input_artifact = "rq3_joint_summary+rq3_pareto_occupancy",
    core_artifact_version = CORE_VERSION,
    rq1_analysis_version = RQ1_VERSION,
    rq2_analysis_version = NA_character_,
    rq3_analysis_version = RQ3_VERSION
  )
)

message("Fig. 6 complete: joint stability, Pareto occupancy and class-specific profiles.")