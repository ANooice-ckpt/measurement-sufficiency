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

# The main panel is summary-only. Keep every dimension × representation-class
# object in one common (A, rank-loss) space so the four measurement dimensions
# remain directly comparable. Crosshairs show the class-level median and IQR;
# deterministic leader lines label the dimension groups without adding a second
# statistical layer. Raw metric observations remain in the supplementary atlas.
DIM_SHAPES_A <- c(
  "Placement" = 16,
  "Optical representation" = 17,
  "Temporal resolution" = 15,
  "Monitoring duration" = 18
)

dimension_summary_a <- dimension_summary |>
  filter(
    is.finite(A_median), is.finite(A_q25), is.finite(A_q75),
    is.finite(rank_loss_median), is.finite(rank_loss_q25), is.finite(rank_loss_q75)
  )
if (!nrow(dimension_summary_a)) stop("No finite class summaries available for Fig. 1a")

# Display domains are driven by the foreground summaries rather than raw metric
# extremes, so the joint structure occupies the plotting field without hiding
# any median or IQR shown in the main panel.
a_display_limit <- max(.25, max(dimension_summary_a$A_q75, na.rm = TRUE) * 1.08)
rank_loss_limit <- max(.05, max(dimension_summary_a$rank_loss_q75, na.rm = TRUE) * 1.08)

assoc_lookup <- setNames(dimension_assoc$rho_A_rank, as.character(dimension_assoc$dimension))
assoc_text <- sprintf(
  "A–rank-loss association, rₛ: Placement %.2f · Optical %.2f · Temporal %.2f · Duration %.2f",
  assoc_lookup[["Placement"]], assoc_lookup[["Optical representation"]],
  assoc_lookup[["Temporal resolution"]], assoc_lookup[["Monitoring duration"]]
)
assoc_text_compact <- sprintf(
  "A–rank-loss rₛ\nP %.2f · O %.2f · T %.2f · D %.2f",
  assoc_lookup[["Placement"]], assoc_lookup[["Optical representation"]],
  assoc_lookup[["Temporal resolution"]], assoc_lookup[["Monitoring duration"]]
)

# Dimension identity is shown with a compact in-panel legend. It uses the same
# shape grammar as the summaries and occupies the otherwise empty upper-left
# region, avoiding leader lines that compete with the crosshairs.
a_dimension_legend <- tibble(
  dimension = factor(
    c("Placement", "Optical representation", "Temporal resolution", "Monitoring duration"),
    levels = levels(dimension_summary_a$dimension)
  ),
  label = c("Placement", "Optical", "Temporal", "Duration"),
  x = .025,
  x_text = .045,
  y = rank_loss_limit * c(.78, .65, .52, .39)
)
a_dimension_legend_bg <- tibble(
  xmin = .010, xmax = .155,
  ymin = rank_loss_limit * .31, ymax = rank_loss_limit * .85
)

p1a_core <- ggplot() +
  geom_hline(yintercept = 0, linewidth = .24, color = "#D7DADD") +
  geom_segment(
    data = dimension_summary_a,
    aes(
      x = A_q25, xend = A_q75,
      y = rank_loss_median, yend = rank_loss_median,
      color = metric_class
    ),
    inherit.aes = FALSE, linewidth = .72, alpha = .54, lineend = "round"
  ) +
  geom_segment(
    data = dimension_summary_a,
    aes(
      x = A_median, xend = A_median,
      y = rank_loss_q25, yend = rank_loss_q75,
      color = metric_class
    ),
    inherit.aes = FALSE, linewidth = .72, alpha = .54, lineend = "round"
  ) +
  geom_rect(
    data = a_dimension_legend_bg,
    aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
    inherit.aes = FALSE, fill = scales::alpha("white", .88),
    color = "#D7DADD", linewidth = .24
  ) +
  geom_text(
    data = tibble(x = .017, y = rank_loss_limit * .825, label = "Dimension"),
    aes(x, y, label = label),
    inherit.aes = FALSE, hjust = 0, vjust = .5,
    size = 1.60, family = MS_FONT, fontface = "bold", color = "#565B5E"
  ) +
  geom_point(
    data = a_dimension_legend,
    aes(x, y, shape = dimension),
    inherit.aes = FALSE, size = 1.95, color = "#4E5559", stroke = 0
  ) +
  geom_text(
    data = a_dimension_legend,
    aes(x_text, y, label = label),
    inherit.aes = FALSE, hjust = 0, vjust = .5,
    size = 1.55, family = MS_FONT, color = "#4E5559"
  ) +
  geom_point(
    data = dimension_summary_a,
    aes(A_median, rank_loss_median, color = metric_class, shape = dimension),
    inherit.aes = FALSE, size = 2.35, stroke = 0, alpha = .98
  ) +
  scale_shape_manual(values = DIM_SHAPES_A, guide = "none") +
  scale_color_ms_metric(guide = "none") +
  scale_x_continuous(
    # A pseudo-log display expands the dense low-distortion region while
    # retaining a well-defined zero and the original A values in tick labels.
    trans = scales::transform_pseudo_log(sigma = .02),
    limits = c(0, a_display_limit),
    breaks = c(0, .2, .4, .6),
    labels = scales::label_number(accuracy = .1),
    expand = expansion(mult = c(0, .02))
  ) +
  scale_y_continuous(
    # Apply the same monotone zero-preserving display logic to rank loss;
    # otherwise the many near-zero temporal/optical summaries collapse onto
    # the baseline while A is already shown on a pseudo-log scale.
    trans = scales::transform_pseudo_log(sigma = .01),
    limits = c(0, rank_loss_limit),
    breaks = c(0, .01, .05, .1, .2, .4),
    labels = scales::label_number(accuracy = .01),
    expand = expansion(mult = c(0, .04))
  ) +
  labs(
    x = "Absolute distortion, A (pseudo-log scale)",
    y = "Rank loss, 1 − Spearman ρ (pseudo-log scale)"
  ) +
  theme_fig1(base_size = 7.15, legend_position = "bottom") +
  theme(
    panel.grid.major = element_blank(),
    axis.text = element_text(size = 5.7),
    axis.title = element_text(size = 6.65),
    plot.margin = margin(0, 3, 3, 3)
  )

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
