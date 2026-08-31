.ms_file <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
if (length(.ms_file)) {
  .ms_script <- normalizePath(sub("^--file=", "", .ms_file[[1]]), winslash = "/", mustWork = TRUE)
  .ms_candidates <- unique(c(file.path(dirname(.ms_script), ".."), getwd()))
  .ms_ok <- vapply(
    .ms_candidates,
    function(x) file.exists(file.path(x, "scripts", "utils", "figure_style.R")),
    logical(1)
  )
  if (!any(.ms_ok)) {
    stop("Could not resolve measurement-sufficiency repository root from ", .ms_script, call. = FALSE)
  }
  setwd(normalizePath(.ms_candidates[which(.ms_ok)[1]], winslash = "/", mustWork = TRUE))
}
rm(.ms_file)
if (exists(".ms_script")) rm(.ms_script)
if (exists(".ms_candidates")) rm(.ms_candidates)
if (exists(".ms_ok")) rm(.ms_ok)

suppressPackageStartupMessages({
  library(tidyverse)
  library(cowplot)
})
source("scripts/utils/analysis_design.R")
source("scripts/utils/figure_style.R")
source("scripts/utils/figure_atlas.R")
source("scripts/utils/plot_contracts.R")

SUMMARY_CSV <- file.path("results", "rq1", "rq1_pairwise_summary.csv")
AVAILABILITY_CSV <- file.path("results", "rq1", "rq1_metric_availability.csv")
LOCAL_CSV <- file.path("results", "rq1", "rq1_local_transition_summary.csv")
RANK_METRIC_CSV <- file.path("results", "rq1", "rq1_relational_preservation_dimension_metric.csv")
RANK_SUMMARY_CSV <- file.path("results", "rq1", "rq1_relational_preservation_dimension_summary.csv")
RANK_ASSOC_CSV <- file.path("results", "rq1", "rq1_distortion_rank_association.csv")
OUT_DIR <- file.path("results", "rq1", "figures")
FIG1_WIDTH_IN <- 7.2
FIG1_HEIGHT_IN <- 3.325  # half of the previous 6.65-in production height
ms_plot_require_files(
  c(SUMMARY_CSV, AVAILABILITY_CSV, LOCAL_CSV, RANK_METRIC_CSV, RANK_SUMMARY_CSV, RANK_ASSOC_CSV),
  "RQ1 plotting inputs"
)
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

DIMENSIONS <- c("placement", "optical", "temporal", "duration")
DIM_TITLES <- c(
  placement = "Placement", optical = "Optical representation",
  temporal = "Temporal resolution", duration = "Monitoring duration"
)
METRIC_CLASSES <- MS_METRIC_CLASSES
FIG1_PANEL_TITLE_SIZE <- 8.0
FIG1_SUBPANEL_TITLE_SIZE <- 7.0
TEMPORAL_TRANSITION_ORDER <- ms_temporal_transition_labels()
NUMERIC_TOL <- 1e-12

summary <- readr::read_csv(SUMMARY_CSV, show_col_types = FALSE, progress = FALSE)
availability <- readr::read_csv(AVAILABILITY_CSV, show_col_types = FALSE, progress = FALSE)
local <- readr::read_csv(LOCAL_CSV, show_col_types = FALSE, progress = FALSE)
dimension_metric <- readr::read_csv(RANK_METRIC_CSV, show_col_types = FALSE, progress = FALSE)
dimension_summary <- readr::read_csv(RANK_SUMMARY_CSV, show_col_types = FALSE, progress = FALSE)
dimension_assoc <- readr::read_csv(RANK_ASSOC_CSV, show_col_types = FALSE, progress = FALSE)

ms_plot_require_columns(
  summary,
  c("core_artifact_version", "rq1_analysis_version", "dimension", "comparison_lattice",
    "comparison_pair_id", "config_a_id", "config_b_id", "config_a_label", "config_b_label",
    "orientation_type", "orientation_basis", "metric", "metric_class", "median_z", "q25_z",
    "q75_z", "p025_z", "p975_z", "B_mean_signed", "A_mean_absolute"),
  "rq1_pairwise_summary.csv"
)
ms_plot_require_columns(
  availability,
  c("core_artifact_version", "rq1_analysis_version", "dimension", "comparison_pair_id",
    "metric", "metric_class", "representation_available"),
  "rq1_metric_availability.csv"
)
ms_plot_require_columns(
  local,
  c("dimension", "metric", "metric_class", "lower_level", "higher_level",
    "orientation_type", "orientation_basis", "G", "A", "B"),
  "rq1_local_transition_summary.csv"
)
ms_plot_require_columns(
  dimension_metric,
  c("core_artifact_version", "rq1_analysis_version", "dimension", "metric", "metric_class",
    "n_oriented_pairs", "A_typical", "rank_loss_typical"),
  "rq1_relational_preservation_dimension_metric.csv"
)
ms_plot_require_columns(
  dimension_summary,
  c("core_artifact_version", "rq1_analysis_version", "dimension", "metric_class", "n_metrics",
    "A_median", "A_q25", "A_q75", "rank_loss_median", "rank_loss_q25", "rank_loss_q75"),
  "rq1_relational_preservation_dimension_summary.csv"
)
ms_plot_require_columns(
  dimension_assoc,
  c("core_artifact_version", "rq1_analysis_version", "dimension", "n_metrics", "rho_A_rank"),
  "rq1_distortion_rank_association.csv"
)

RQ1_VERSION <- ms_plot_one_version(
  c(summary$rq1_analysis_version, availability$rq1_analysis_version, local$rq1_analysis_version,
    dimension_metric$rq1_analysis_version, dimension_summary$rq1_analysis_version,
    dimension_assoc$rq1_analysis_version),
  "rq1_analysis_version"
)
CORE_VERSION <- ms_plot_assert_core(
  c(summary$core_artifact_version, availability$core_artifact_version, local$core_artifact_version,
    dimension_metric$core_artifact_version, dimension_summary$core_artifact_version,
    dimension_assoc$core_artifact_version)
)
ms_plot_assert_prefix(RQ1_VERSION, "rq1_v5_", "rq1_analysis_version")
if (!grepl(ms_analysis_design_id(), RQ1_VERSION, fixed = TRUE)) {
  stop("Fig. 1 inputs do not match the current frozen analysis design", call. = FALSE)
}

safe_q <- function(x, p) {
  x <- as.numeric(x)
  x <- x[is.finite(x)]
  if (length(x)) unname(stats::quantile(x, p, names = FALSE, type = 8)) else NA_real_
}

pretty_transition <- function(x) {
  stringr::str_replace_all(as.character(x), "\\s+to\\s+", " → ")
}

theme_fig1 <- function(base_size = 7.1, legend_position = "none") {
  theme_ms_axes(
    base_size = base_size,
    legend_position = legend_position,
    plot_title_size = FIG1_PANEL_TITLE_SIZE,
    plot_margin = margin(2, 3, 2, 3)
  )
}

summary <- summary |>
  mutate(
    metric_class = factor(metric_class, levels = METRIC_CLASSES),
    dimension = factor(dimension, levels = DIMENSIONS),
    pair_label = paste(config_a_label, "to", config_b_label),
    direction_ratio = ms_direction_ratio(B_mean_signed, A_mean_absolute)
  )
availability <- availability |>
  mutate(
    metric_class = factor(metric_class, levels = METRIC_CLASSES),
    dimension = factor(dimension, levels = DIMENSIONS)
  ) |>
  left_join(
    summary |> distinct(dimension, comparison_pair_id, pair_label),
    by = c("dimension", "comparison_pair_id")
  ) |>
  mutate(pair_label = coalesce(pair_label, as.character(comparison_pair_id))) |>
  distinct(dimension, pair_label, metric, metric_class, representation_available)
local <- local |>
  mutate(
    metric_class = factor(metric_class, levels = METRIC_CLASSES),
    dimension = factor(dimension, levels = DIMENSIONS)
  )

metric_order <- ms_metric_order(summary |> mutate(dimension = as.character(dimension)))
readr::write_csv(metric_order, file.path("results", "rq1", "figure_metric_order.csv"), na = "")
summary_plot <- summary |> mutate(dimension = as.character(dimension)) |> ms_add_metric_order(metric_order)
availability_plot <- availability |> mutate(dimension = as.character(dimension)) |> ms_add_metric_order(metric_order)

# -----------------------------------------------------------------------------
# a. Absolute versus relational preservation across measurement dimensions
# -----------------------------------------------------------------------------
dimension_metric <- dimension_metric |>
  mutate(
    dimension = factor(dimension, levels = DIMENSIONS, labels = unname(DIM_TITLES[DIMENSIONS])),
    metric_class = factor(metric_class, levels = METRIC_CLASSES)
  )
dimension_summary <- dimension_summary |>
  mutate(
    dimension = factor(dimension, levels = DIMENSIONS, labels = unname(DIM_TITLES[DIMENSIONS])),
    metric_class = factor(metric_class, levels = METRIC_CLASSES)
  )
dimension_assoc <- dimension_assoc |>
  mutate(
    dimension = factor(dimension, levels = DIMENSIONS, labels = unname(DIM_TITLES[DIMENSIONS])),
    label = if_else(is.finite(rho_A_rank), sprintf("rₛ = %.2f", rho_A_rank), "rₛ = NA")
  )
if (!nrow(dimension_summary)) stop("No non-circular RQ1 rows available for Fig. 1a")

# Fig. 1a uses one common bivariate coordinate system. The visual hierarchy is
# deliberately three-level: four dimension-level summaries form the foreground,
# metric-class marginal-quantile glyphs form the middle layer, and individual
# metric points remain visible at low opacity. Hollow lozenges are descriptive
# marginal-quantile glyphs, not covariance/confidence ellipses: horizontal extent
# encodes A quantiles and vertical extent encodes rank-loss quantiles.
dimension_metric_a <- dimension_metric |>
  filter(is.finite(A_typical), is.finite(rank_loss_typical))
if (!nrow(dimension_metric_a)) stop("No finite metric-level rows available for Fig. 1a")

dimension_summary_a <- dimension_summary |>
  filter(
    is.finite(A_median), is.finite(A_q25), is.finite(A_q75),
    is.finite(rank_loss_median), is.finite(rank_loss_q25), is.finite(rank_loss_q75)
  )
if (!nrow(dimension_summary_a)) stop("No finite class summaries available for Fig. 1a")

class_outer_a <- dimension_metric_a |>
  group_by(dimension, metric_class) |>
  summarise(
    A_q10 = safe_q(A_typical, .10),
    A_q90 = safe_q(A_typical, .90),
    rank_loss_q10 = safe_q(rank_loss_typical, .10),
    rank_loss_q90 = safe_q(rank_loss_typical, .90),
    .groups = "drop"
  )

class_summary_a <- dimension_summary_a |>
  left_join(class_outer_a, by = c("dimension", "metric_class")) |>
  filter(
    is.finite(A_q10), is.finite(A_q90),
    is.finite(rank_loss_q10), is.finite(rank_loss_q90)
  )

dimension_overall_a <- dimension_metric_a |>
  group_by(dimension) |>
  summarise(
    n_metrics = n_distinct(metric),
    A_q10 = safe_q(A_typical, .10),
    A_q25 = safe_q(A_typical, .25),
    A_median = safe_q(A_typical, .50),
    A_q75 = safe_q(A_typical, .75),
    A_q90 = safe_q(A_typical, .90),
    rank_loss_q10 = safe_q(rank_loss_typical, .10),
    rank_loss_q25 = safe_q(rank_loss_typical, .25),
    rank_loss_median = safe_q(rank_loss_typical, .50),
    rank_loss_q75 = safe_q(rank_loss_typical, .75),
    rank_loss_q90 = safe_q(rank_loss_typical, .90),
    .groups = "drop"
  )

# Focus + tail display mapping. The dense central field stays linear. Only when a
# small extreme tail is materially separated from the foreground IQR structure is
# that tail compressed into a narrow shaded gutter. Relative order within the
# gutter is retained by a log1p mapping; no metric is discarded.
make_focus_tail_axis <- function(values, foreground, prob = .95,
                                 tail_ratio = 1.30, gutter_fraction = .12,
                                 fallback = 1) {
  values <- suppressWarnings(as.numeric(values))
  values <- values[is.finite(values) & values >= 0]
  foreground <- suppressWarnings(as.numeric(foreground))
  foreground <- foreground[is.finite(foreground) & foreground >= 0]
  if (!length(values)) {
    return(list(
      use_tail = FALSE, focus = fallback, raw_max = fallback,
      display_max = fallback, tick_max = fallback,
      map = function(x) suppressWarnings(as.numeric(x))
    ))
  }

  raw_max <- max(values)
  robust_focus <- safe_q(values, prob)
  foreground_max <- if (length(foreground)) max(foreground) else 0
  focus <- max(c(robust_focus, foreground_max, raw_max * .15), na.rm = TRUE)
  focus <- min(focus, raw_max)
  use_tail <- raw_max > focus + NUMERIC_TOL &&
    (focus <= NUMERIC_TOL || raw_max / focus >= tail_ratio)

  if (!use_tail) {
    display_max <- max(raw_max * 1.06, fallback * .02)
    mapper <- function(x) {
      x <- suppressWarnings(as.numeric(x))
      ifelse(is.finite(x), pmax(0, pmin(x, display_max)), NA_real_)
    }
    return(list(
      use_tail = FALSE, focus = raw_max, raw_max = raw_max,
      display_max = display_max, tick_max = raw_max, map = mapper
    ))
  }

  gutter <- max(focus * gutter_fraction, raw_max * .018, fallback * .015)
  display_max <- focus + gutter
  denom <- log1p(raw_max - focus)
  mapper <- function(x) {
    x <- suppressWarnings(as.numeric(x))
    out <- rep(NA_real_, length(x))
    ok <- is.finite(x)
    low <- ok & x <= focus
    high <- ok & x > focus
    out[low] <- pmax(0, x[low])
    out[high] <- focus + gutter *
      log1p(pmin(x[high], raw_max) - focus) / denom
    out
  }
  list(
    use_tail = TRUE, focus = focus, raw_max = raw_max,
    display_max = display_max, tick_max = focus, map = mapper
  )
}

a_x_axis <- make_focus_tail_axis(
  dimension_metric_a$A_typical,
  c(class_summary_a$A_q75, dimension_overall_a$A_q75),
  prob = .95, tail_ratio = 1.30, gutter_fraction = .12, fallback = .25
)
a_y_axis <- make_focus_tail_axis(
  dimension_metric_a$rank_loss_typical,
  c(class_summary_a$rank_loss_q75, dimension_overall_a$rank_loss_q75),
  prob = .95, tail_ratio = 1.30, gutter_fraction = .12, fallback = .05
)

dimension_metric_plot_a <- dimension_metric_a |>
  mutate(
    A_plot = a_x_axis$map(A_typical),
    rank_loss_plot = a_y_axis$map(rank_loss_typical)
  )

class_summary_plot_a <- class_summary_a |>
  mutate(
    A_q10_plot = a_x_axis$map(A_q10),
    A_q25_plot = a_x_axis$map(A_q25),
    A_median_plot = a_x_axis$map(A_median),
    A_q75_plot = a_x_axis$map(A_q75),
    A_q90_plot = a_x_axis$map(A_q90),
    rank_loss_q10_plot = a_y_axis$map(rank_loss_q10),
    rank_loss_q25_plot = a_y_axis$map(rank_loss_q25),
    rank_loss_median_plot = a_y_axis$map(rank_loss_median),
    rank_loss_q75_plot = a_y_axis$map(rank_loss_q75),
    rank_loss_q90_plot = a_y_axis$map(rank_loss_q90)
  )

dimension_overall_plot_a <- dimension_overall_a |>
  mutate(
    A_q10_plot = a_x_axis$map(A_q10),
    A_q25_plot = a_x_axis$map(A_q25),
    A_median_plot = a_x_axis$map(A_median),
    A_q75_plot = a_x_axis$map(A_q75),
    A_q90_plot = a_x_axis$map(A_q90),
    rank_loss_q10_plot = a_y_axis$map(rank_loss_q10),
    rank_loss_q25_plot = a_y_axis$map(rank_loss_q25),
    rank_loss_median_plot = a_y_axis$map(rank_loss_median),
    rank_loss_q75_plot = a_y_axis$map(rank_loss_q75),
    rank_loss_q90_plot = a_y_axis$map(rank_loss_q90)
  )

# Nested marginal-quantile lozenges: outer = 10–90%, inner = IQR. Closing the
# path at the first vertex yields a contour-like glyph without implying a fitted
# bivariate probability region.
class_outer_path_a <- bind_rows(
  class_summary_plot_a |> transmute(dimension, metric_class, vertex = 1L, x = A_q10_plot, y = rank_loss_median_plot),
  class_summary_plot_a |> transmute(dimension, metric_class, vertex = 2L, x = A_median_plot, y = rank_loss_q90_plot),
  class_summary_plot_a |> transmute(dimension, metric_class, vertex = 3L, x = A_q90_plot, y = rank_loss_median_plot),
  class_summary_plot_a |> transmute(dimension, metric_class, vertex = 4L, x = A_median_plot, y = rank_loss_q10_plot),
  class_summary_plot_a |> transmute(dimension, metric_class, vertex = 5L, x = A_q10_plot, y = rank_loss_median_plot)
) |>
  arrange(dimension, metric_class, vertex) |>
  mutate(glyph = interaction(dimension, metric_class, drop = TRUE))

class_inner_path_a <- bind_rows(
  class_summary_plot_a |> transmute(dimension, metric_class, vertex = 1L, x = A_q25_plot, y = rank_loss_median_plot),
  class_summary_plot_a |> transmute(dimension, metric_class, vertex = 2L, x = A_median_plot, y = rank_loss_q75_plot),
  class_summary_plot_a |> transmute(dimension, metric_class, vertex = 3L, x = A_q75_plot, y = rank_loss_median_plot),
  class_summary_plot_a |> transmute(dimension, metric_class, vertex = 4L, x = A_median_plot, y = rank_loss_q25_plot),
  class_summary_plot_a |> transmute(dimension, metric_class, vertex = 5L, x = A_q25_plot, y = rank_loss_median_plot)
) |>
  arrange(dimension, metric_class, vertex) |>
  mutate(glyph = interaction(dimension, metric_class, drop = TRUE))

overall_outer_path_a <- bind_rows(
  dimension_overall_plot_a |> transmute(dimension, vertex = 1L, x = A_q10_plot, y = rank_loss_median_plot),
  dimension_overall_plot_a |> transmute(dimension, vertex = 2L, x = A_median_plot, y = rank_loss_q90_plot),
  dimension_overall_plot_a |> transmute(dimension, vertex = 3L, x = A_q90_plot, y = rank_loss_median_plot),
  dimension_overall_plot_a |> transmute(dimension, vertex = 4L, x = A_median_plot, y = rank_loss_q10_plot),
  dimension_overall_plot_a |> transmute(dimension, vertex = 5L, x = A_q10_plot, y = rank_loss_median_plot)
) |>
  arrange(dimension, vertex)

overall_inner_path_a <- bind_rows(
  dimension_overall_plot_a |> transmute(dimension, vertex = 1L, x = A_q25_plot, y = rank_loss_median_plot),
  dimension_overall_plot_a |> transmute(dimension, vertex = 2L, x = A_median_plot, y = rank_loss_q75_plot),
  dimension_overall_plot_a |> transmute(dimension, vertex = 3L, x = A_q75_plot, y = rank_loss_median_plot),
  dimension_overall_plot_a |> transmute(dimension, vertex = 4L, x = A_median_plot, y = rank_loss_q25_plot),
  dimension_overall_plot_a |> transmute(dimension, vertex = 5L, x = A_q25_plot, y = rank_loss_median_plot)
) |>
  arrange(dimension, vertex)

class_spokes_a <- class_summary_plot_a |>
  select(dimension, metric_class, x = A_median_plot, y = rank_loss_median_plot) |>
  left_join(
    dimension_overall_plot_a |>
      select(dimension, x0 = A_median_plot, y0 = rank_loss_median_plot),
    by = "dimension"
  )

dimension_labels_a <- dimension_overall_plot_a |>
  mutate(
    short_label = recode(
      as.character(dimension),
      "Optical representation" = "Optical",
      "Temporal resolution" = "Temporal",
      "Monitoring duration" = "Duration",
      .default = as.character(dimension)
    ),
    hjust = if_else(A_median_plot > .72 * a_x_axis$display_max, 1, 0),
    vjust = if_else(rank_loss_median_plot > .78 * a_y_axis$display_max, 1, 0),
    x_label = A_median_plot + if_else(
      hjust == 1, -.018 * a_x_axis$display_max, .018 * a_x_axis$display_max
    ),
    y_label = rank_loss_median_plot + if_else(
      vjust == 1, -.030 * a_y_axis$display_max, .030 * a_y_axis$display_max
    )
  )

assoc_lookup <- setNames(dimension_assoc$rho_A_rank, as.character(dimension_assoc$dimension))
assoc_text <- sprintf(
  "tail gutters compress extreme metrics · A–rank-loss rₛ: P %.2f · O %.2f · T %.2f · D %.2f",
  assoc_lookup[["Placement"]], assoc_lookup[["Optical representation"]],
  assoc_lookup[["Temporal resolution"]], assoc_lookup[["Monitoring duration"]]
)
assoc_text_compact <- sprintf(
  "tail gutters compress extremes\nrₛ: P %.2f · O %.2f · T %.2f · D %.2f",
  assoc_lookup[["Placement"]], assoc_lookup[["Optical representation"]],
  assoc_lookup[["Temporal resolution"]], assoc_lookup[["Monitoring duration"]]
)

a_x_breaks_raw <- scales::breaks_extended(n = 5)(c(0, a_x_axis$tick_max))
a_x_breaks_raw <- sort(unique(c(
  0, a_x_breaks_raw[is.finite(a_x_breaks_raw) & a_x_breaks_raw >= 0 & a_x_breaks_raw <= a_x_axis$tick_max]
)))
a_y_breaks_raw <- scales::breaks_extended(n = 5)(c(0, a_y_axis$tick_max))
a_y_breaks_raw <- sort(unique(c(
  0, a_y_breaks_raw[is.finite(a_y_breaks_raw) & a_y_breaks_raw >= 0 & a_y_breaks_raw <= a_y_axis$tick_max]
)))
a_x_labels <- scales::label_number(
  accuracy = if (a_x_axis$tick_max <= .5) .05 else .1
)(a_x_breaks_raw)
a_y_labels <- scales::label_number(
  accuracy = if (a_y_axis$tick_max <= .2) .01 else .05
)(a_y_breaks_raw)

tail_rects_a <- bind_rows(
  if (a_x_axis$use_tail) tibble(
    xmin = a_x_axis$focus, xmax = a_x_axis$display_max,
    ymin = 0, ymax = a_y_axis$display_max
  ) else tibble(),
  if (a_y_axis$use_tail) tibble(
    xmin = 0, xmax = a_x_axis$display_max,
    ymin = a_y_axis$focus, ymax = a_y_axis$display_max
  ) else tibble()
)

p1a_core <- ggplot() +
  geom_rect(
    data = tail_rects_a,
    aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
    inherit.aes = FALSE, fill = "#F5F6F6", colour = NA
  ) +
  {if (a_x_axis$use_tail) geom_vline(
    xintercept = a_x_axis$focus, colour = "#B9BEC1",
    linewidth = .28, linetype = "22"
  ) else NULL} +
  {if (a_y_axis$use_tail) geom_hline(
    yintercept = a_y_axis$focus, colour = "#B9BEC1",
    linewidth = .28, linetype = "22"
  ) else NULL} +
  geom_segment(
    data = class_spokes_a,
    aes(x = x0, y = y0, xend = x, yend = y),
    inherit.aes = FALSE, colour = "#AEB4B7", linewidth = .30, alpha = .16
  ) +
  geom_point(
    data = dimension_metric_plot_a,
    aes(A_plot, rank_loss_plot, colour = metric_class),
    size = .48, alpha = .13, shape = 16
  ) +
  geom_path(
    data = class_outer_path_a,
    aes(x, y, group = glyph, colour = metric_class),
    linewidth = .30, alpha = .28, lineend = "round", linejoin = "round"
  ) +
  geom_path(
    data = class_inner_path_a,
    aes(x, y, group = glyph, colour = metric_class),
    linewidth = .70, alpha = .82, lineend = "round", linejoin = "round"
  ) +
  geom_point(
    data = class_summary_plot_a,
    aes(A_median_plot, rank_loss_median_plot, colour = metric_class),
    shape = 16, size = 1.28, alpha = .98
  ) +
  geom_path(
    data = overall_outer_path_a,
    aes(x, y, group = dimension),
    inherit.aes = FALSE, colour = "#40474B", linewidth = .52,
    alpha = .46, lineend = "round", linejoin = "round"
  ) +
  geom_path(
    data = overall_inner_path_a,
    aes(x, y, group = dimension),
    inherit.aes = FALSE, colour = "#252B2E", linewidth = 1.05,
    alpha = .92, lineend = "round", linejoin = "round"
  ) +
  geom_point(
    data = dimension_overall_plot_a,
    aes(A_median_plot, rank_loss_median_plot),
    inherit.aes = FALSE, shape = 23, size = 2.65,
    fill = "#252B2E", colour = "white", stroke = .32
  ) +
  geom_text(
    data = dimension_labels_a,
    aes(x_label, y_label, label = short_label, hjust = hjust, vjust = vjust),
    inherit.aes = FALSE, family = MS_FONT, fontface = "bold",
    size = 1.78, colour = "#252B2E"
  ) +
  scale_color_ms_metric(guide = "none") +
  scale_x_continuous(
    limits = c(0, a_x_axis$display_max),
    breaks = a_x_axis$map(a_x_breaks_raw), labels = a_x_labels,
    expand = expansion(mult = c(0, .012))
  ) +
  scale_y_continuous(
    limits = c(0, a_y_axis$display_max),
    breaks = a_y_axis$map(a_y_breaks_raw), labels = a_y_labels,
    expand = expansion(mult = c(0, .018))
  ) +
  labs(
    x = "Absolute distortion, A",
    y = "Rank loss, 1 − Spearman ρ"
  ) +
  theme_fig1(base_size = 7.15, legend_position = "none") +
  theme(
    panel.grid.major = element_line(colour = "#EFF1F2", linewidth = .20),
    panel.grid.minor = element_blank(),
    axis.text = element_text(size = 5.7),
    axis.title = element_text(size = 6.65),
    plot.margin = margin(0, 3, 3, 3)
  )

if (a_x_axis$use_tail) {
  p1a_core <- p1a_core + annotate(
    "text",
    x = a_x_axis$focus + .50 * (a_x_axis$display_max - a_x_axis$focus),
    y = .975 * a_y_axis$display_max,
    label = "compressed A tail", family = MS_FONT,
    size = 1.42, colour = "#8A8F92", hjust = .5, vjust = 1
  )
}
if (a_y_axis$use_tail) {
  p1a_core <- p1a_core + annotate(
    "text",
    x = .018 * a_x_axis$display_max,
    y = a_y_axis$focus + .78 * (a_y_axis$display_max - a_y_axis$focus),
    label = "compressed rank-loss tail", family = MS_FONT,
    size = 1.42, colour = "#8A8F92", hjust = 0, vjust = .5
  )
}

# The lower b/c block has an intentional left inset for the shared y-axis and
# its plot frames. Apply the same inset to panel a's inner plot, while keeping
# the panel title and association line on the full-figure left alignment.
p1a <- cowplot::ggdraw() +
  cowplot::draw_plot(p1a_core, x = .025, y = 0, width = .975, height = .93) +
  cowplot::draw_label(
    "a  Absolute and\nrelational preservation",
    x = .002, y = .998, hjust = 0, vjust = 1,
    size = 6.8, lineheight = .92, fontface = "bold",
    colour = "#151515", fontfamily = MS_FONT
  ) +
  cowplot::draw_label(
    assoc_text_compact,
    x = .025, y = .895, hjust = 0, vjust = 1,
    size = 4.05, lineheight = .90, colour = "#666A6D", fontfamily = MS_FONT
  )

# -----------------------------------------------------------------------------
# b. Target-aligned distortion magnitude and directional coherence
# -----------------------------------------------------------------------------
target_geometry <- summary |>
  filter(
    dimension %in% c("placement", "optical"),
    is.finite(A_mean_absolute), is.finite(B_mean_signed),
    A_mean_absolute > NUMERIC_TOL
  ) |>
  transmute(
    dimension = as.character(dimension), metric, metric_class,
    A_mean_absolute, B_mean_signed,
    coherence = pmax(-1, pmin(1, B_mean_signed / A_mean_absolute)),
    transition = pretty_transition(pair_label),
    facet_label = case_when(
      dimension == "placement" ~ "Placement · chest/wrist → eye",
      dimension == "optical" ~ "Optical representation · LIGHT → MEDI",
      TRUE ~ dimension
    )
  ) |>
  mutate(
    facet_label = factor(
      facet_label,
      levels = c("Placement · chest/wrist → eye", "Optical representation · LIGHT → MEDI")
    ),
    metric_class = factor(metric_class, levels = METRIC_CLASSES)
  )
if (!nrow(target_geometry)) stop("No target-aligned rows available for Fig. 1b")

target_y_limits <- target_geometry |>
  group_by(facet_label) |>
  summarise(
    y_limit = max(.12, safe_q(A_mean_absolute, .98), na.rm = TRUE) * 1.08,
    .groups = "drop"
  )

target_geometry <- target_geometry |>
  left_join(target_y_limits, by = "facet_label") |>
  mutate(
    offscale = A_mean_absolute > y_limit + NUMERIC_TOL,
    A_display = pmin(A_mean_absolute, y_limit * .975)
  )

target_label_max_a <- target_geometry |>
  group_by(facet_label, metric) |>
  slice_max(A_mean_absolute, n = 1, with_ties = FALSE) |>
  ungroup() |>
  group_by(facet_label) |>
  slice_max(A_mean_absolute, n = 1, with_ties = FALSE) |>
  ungroup()
target_label_max_coh <- target_geometry |>
  group_by(facet_label, metric) |>
  slice_max(abs(coherence), n = 1, with_ties = FALSE) |>
  ungroup() |>
  group_by(facet_label) |>
  slice_max(abs(coherence), n = 1, with_ties = FALSE) |>
  ungroup()
target_labels <- bind_rows(target_label_max_a, target_label_max_coh) |>
  distinct(facet_label, metric, .keep_all = TRUE)

target_geometry_panel <- function(panel_name, show_x_title = TRUE) {
  d <- target_geometry |> filter(facet_label == panel_name)
  if (!nrow(d)) stop("No target geometry rows for panel: ", panel_name)
  y_lim <- dplyr::first(d$y_limit)
  panel_title <- dplyr::case_when(
    panel_name == "Placement · chest/wrist → eye" ~ "Placement ·\nchest/wrist → eye",
    panel_name == "Optical representation · LIGHT → MEDI" ~ "Optical representation ·\nLIGHT → MEDI",
    TRUE ~ panel_name
  )

  ggplot() +
    geom_vline(xintercept = 0, linewidth = .30, color = "#A8ADB0") +
    geom_point(
      data = d |> filter(!offscale),
      aes(coherence, A_display, color = metric_class, shape = transition),
      size = 1.28, alpha = .82
    ) +
    geom_point(
      data = d |> filter(offscale),
      aes(coherence, A_display),
      inherit.aes = FALSE, shape = 4, size = 1.50, stroke = .42, color = "#303437"
    ) +
    geom_text(
      data = target_labels |> filter(facet_label == panel_name),
      aes(coherence, A_display, label = metric),
      inherit.aes = FALSE, size = 1.80, color = "#303030",
      check_overlap = TRUE, vjust = -.62
    ) +
    scale_color_ms_metric(guide = "none") +
    scale_shape_discrete(name = NULL) +
    scale_x_continuous(
      limits = c(-1, 1),
      breaks = c(-1, -.5, 0, .5, 1),
      expand = expansion(mult = c(.01, .01))
    ) +
    scale_y_continuous(
      limits = c(0, y_lim),
      breaks = scales::breaks_extended(n = 4),
      expand = expansion(mult = c(0, .03))
    ) +
    labs(
      title = panel_title,
      x = if (show_x_title) "Directional coherence, B/A" else NULL,
      y = NULL
    ) +
    theme_fig1(base_size = 7.0) +
    theme(
      panel.grid.major = element_blank(),
      plot.title = element_text(
        size = 5.8, lineheight = .90, face = "bold", hjust = .5,
        margin = margin(b = 1)
      ),
      axis.text.x = element_text(size = 5.1),
      plot.margin = margin(2, 3, 2, 3)
    )
}

p1b_top <- target_geometry_panel("Placement · chest/wrist → eye", show_x_title = FALSE)
p1b_bottom <- target_geometry_panel("Optical representation · LIGHT → MEDI", show_x_title = TRUE)

p1b_shape_legend <- cowplot::get_legend(
  ggplot(target_geometry |> filter(!offscale), aes(coherence, A_display, shape = transition)) +
    geom_point(size = 1.5, color = "#3B3B3B") +
    scale_shape_discrete(name = NULL) +
    theme_void() +
    theme(
      legend.position = "bottom",
      legend.text = element_text(size = 4.25),
      legend.key.width = grid::unit(2.5, "mm"),
      legend.spacing.x = grid::unit(.35, "mm")
    )
)

p1b_core <- cowplot::plot_grid(
  p1b_top, p1b_bottom,
  ncol = 1, rel_heights = c(1, 1),
  align = "v", axis = "lr", greedy = TRUE
)
p1b <- cowplot::ggdraw() +
  cowplot::draw_plot(p1b_core, x = .08, y = .075, width = .92, height = .86) +
  cowplot::draw_plot(p1b_shape_legend, x = .16, y = 0, width = .70, height = .09) +
  cowplot::draw_label(
    "b  Magnitude and\ndirectional coherence",
    x = .002, y = .998, hjust = 0, vjust = 1,
    size = 6.8, lineheight = .92, fontface = "bold",
    colour = "#151515", fontfamily = MS_FONT
  ) +
  cowplot::draw_label(
    "Absolute distortion, A",
    x = .012, y = .50, angle = 90,
    hjust = .5, vjust = .5,
    size = 6.3, colour = "#151515", fontfamily = MS_FONT
  )

# -----------------------------------------------------------------------------
# c. Where local response accrues along ordered measurement-burden axes
# -----------------------------------------------------------------------------
local_display <- local |>
  mutate(
    dimension = as.character(dimension),
    from_days = if_else(
      dimension == "duration",
      suppressWarnings(as.integer(str_extract(lower_level, "^\\d+"))), NA_integer_
    ),
    to_days = if_else(
      dimension == "duration",
      suppressWarnings(as.integer(str_extract(higher_level, "^\\d+"))), NA_integer_
    ),
    transition = if_else(
      dimension == "duration",
      paste0(from_days, " d → ", to_days, " d"),
      pretty_transition(paste(lower_level, "to", higher_level))
    ),
    step_order = case_when(
      dimension == "duration" ~ as.numeric(from_days),
      dimension == "temporal" ~ match(transition, pretty_transition(TEMPORAL_TRANSITION_ORDER)),
      TRUE ~ NA_real_
    )
  ) |>
  filter(
    dimension %in% c("temporal", "duration"),
    is.finite(G), is.finite(step_order)
  ) |>
  group_by(dimension, metric, metric_class, transition, step_order) |>
  summarise(G_display = median(G, na.rm = TRUE), .groups = "drop") |>
  group_by(dimension, metric, metric_class) |>
  mutate(
    G_total = sum(G_display, na.rm = TRUE),
    G_share = if_else(is.finite(G_total) & G_total > 0, G_display / G_total, NA_real_)
  ) |>
  ungroup() |>
  filter(is.finite(G_share)) |>
  mutate(
    metric_class = factor(metric_class, levels = METRIC_CLASSES),
    row_offset = ms_class_offset(metric_class, span = .44, classes = METRIC_CLASSES),
    y_point = step_order + row_offset
  )

local_summary <- local_display |>
  group_by(dimension, metric_class, transition, step_order) |>
  summarise(
    n_metrics = n_distinct(metric),
    share_median = median(G_share, na.rm = TRUE),
    share_q25 = quantile(G_share, .25, na.rm = TRUE, names = FALSE),
    share_q75 = quantile(G_share, .75, na.rm = TRUE, names = FALSE),
    .groups = "drop"
  ) |>
  mutate(
    metric_class = factor(metric_class, levels = METRIC_CLASSES),
    row_offset = ms_class_offset(metric_class, span = .44, classes = METRIC_CLASSES),
    y_summary = step_order + row_offset
  )

local_overall <- local_display |>
  group_by(dimension, transition, step_order) |>
  summarise(
    n_metrics = n_distinct(metric),
    share_median = median(G_share, na.rm = TRUE),
    share_q25 = quantile(G_share, .25, na.rm = TRUE, names = FALSE),
    share_q75 = quantile(G_share, .75, na.rm = TRUE, names = FALSE),
    .groups = "drop"
  )

readr::write_csv(
  local_summary |>
    mutate(metric_class = as.character(metric_class)) |>
    select(
      dimension, metric_class, transition, step_order, n_metrics,
      share_median, share_q25, share_q75
    ),
  file.path("results", "rq1", "fig1_local_response_aggregated.csv"), na = ""
)
readr::write_csv(
  local_overall,
  file.path("results", "rq1", "fig1_local_response_overall.csv"), na = ""
)

local_xmax <- max(c(local_summary$share_q75, local_summary$share_median,
                    local_overall$share_q75, local_overall$share_median), na.rm = TRUE) * 1.15
local_xmax <- min(1, max(.40, local_xmax))

local_distribution_panel <- function(dim, subpanel_title = DIM_TITLES[[dim]], show_x_title = TRUE) {
  d <- local_display |> filter(dimension == dim)
  ds <- local_summary |> filter(dimension == dim)
  do <- local_overall |> filter(dimension == dim)
  if (!nrow(d) || !nrow(ds) || !nrow(do)) stop("No local response rows for Fig. 1 dimension: ", dim)

  labels <- d |> distinct(step_order, transition) |> arrange(step_order)
  equal_share <- 1 / n_distinct(labels$transition)

  ggplot() +
    geom_vline(
      xintercept = equal_share, linetype = 3,
      linewidth = .30, color = "#A9AEB1"
    ) +
    geom_segment(
      data = do,
      aes(x = share_q25, xend = share_q75, y = step_order, yend = step_order),
      inherit.aes = FALSE, linewidth = .62, color = "#AEB3B6", alpha = .38,
      lineend = "round"
    ) +
    geom_point(
      data = d,
      aes(G_share, y_point, color = metric_class),
      position = position_jitter(width = 0, height = .025, seed = 73),
      size = .56, alpha = .22
    ) +
    geom_segment(
      data = ds,
      aes(
        x = share_q25, xend = share_q75,
        y = y_summary, yend = y_summary, color = metric_class
      ),
      linewidth = .78, alpha = .50, lineend = "round"
    ) +
    geom_point(
      data = ds,
      aes(share_median, y_summary, color = metric_class),
      shape = 18, size = 1.55, alpha = .98
    ) +
    scale_color_ms_metric(guide = "none") +
    scale_x_continuous(
      limits = c(0, local_xmax),
      labels = scales::label_percent(accuracy = 1),
      breaks = scales::breaks_extended(n = 5),
      expand = expansion(mult = c(.01, .02))
    ) +
    scale_y_reverse(
      breaks = labels$step_order,
      labels = labels$transition,
      expand = expansion(add = c(.38, .38))
    ) +
    labs(
      title = subpanel_title,
      x = if (show_x_title) "Share of cumulative local response" else NULL,
      y = NULL
    ) +
    theme_fig1(base_size = 7.0) +
    theme(
      panel.grid.major.y = element_blank(),
      axis.line.y = element_blank(), axis.ticks.y = element_blank(),
      axis.text.y = element_text(size = 5.8),
      plot.title = element_text(
        size = FIG1_SUBPANEL_TITLE_SIZE, face = "bold", hjust = .5,
        margin = margin(b = 2)
      ),
      plot.margin = margin(2, 3, 2, 3)
    )
}

p1c_temporal <- local_distribution_panel("temporal", "Temporal resolution", show_x_title = FALSE)
p1c_duration <- local_distribution_panel("duration", "Monitoring duration", show_x_title = TRUE)

metric_legend <- ms_metric_legend(text_size = 6.05, point_size = 1.7, key_width_mm = 3.8)

right_core <- cowplot::plot_grid(
  p1c_temporal, p1c_duration,
  ncol = 1, rel_heights = c(1, 1),
  align = "v", axis = "lr", greedy = TRUE
)
right_column <- cowplot::ggdraw() +
  cowplot::draw_plot(right_core, x = 0, y = 0, width = 1, height = .90) +
  cowplot::draw_label(
    "c  Where ordered-axis\ndistortion accrues",
    x = .002, y = .998, hjust = 0, vjust = 1,
    size = 6.8, lineheight = .92, fontface = "bold",
    colour = "#151515", fontfamily = MS_FONT
  )

fig1body <- cowplot::plot_grid(
  p1a, p1b, right_column,
  ncol = 3, rel_widths = c(.98, 1.14, 1.14),
  align = "hv", axis = "tblr", greedy = TRUE
)
fig1 <- cowplot::plot_grid(
  metric_legend, fig1body,
  ncol = 1, rel_heights = c(.045, 1),
  align = "v", axis = "l", greedy = TRUE
)

ms_plot_save(fig1, file.path(OUT_DIR, "Fig1_RQ1.pdf"), FIG1_WIDTH_IN, FIG1_HEIGHT_IN)
ms_plot_save(fig1, file.path(OUT_DIR, "Fig1_RQ1.png"), FIG1_WIDTH_IN, FIG1_HEIGHT_IN)


ms_plot_write_manifest(
  file.path(OUT_DIR, "figure_artifact_manifest.csv"),
  tibble(
    figure = 'Fig1_RQ1',
    input_artifact = 'rq1_pairwise_change_long (derived Spearman) + rq1_pairwise_summary + rq1_local_transition_summary',
    core_artifact_version = CORE_VERSION,
    rq1_analysis_version = RQ1_VERSION,
    rq2_analysis_version = NA_character_,
    rq3_analysis_version = NA_character_
  )
)

message("Fig. 1 complete: common preservation landscape, target-aligned magnitude/coherence geometry, and ordered-axis local-response distributions.")