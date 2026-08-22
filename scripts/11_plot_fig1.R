suppressPackageStartupMessages({
  library(tidyverse)
  library(cowplot)
})
source("scripts/utils/figure_style.R")
source("scripts/utils/figure_atlas.R")
source("scripts/utils/rq1_pairwise_artifacts.R")
source("scripts/utils/plot_contracts.R")

# Fig. 1 uses a compact distribution-led grammar. Metric-level observations are
# visible, but the foreground is always a class-level location/interval summary.
# The four measurement dimensions are never connected as if they formed one
# continuous axis; ordered local transitions are shown as distribution strips.
RQ1_LONG <- file.path("results", "rq1", "rq1_pairwise_change_long.rds")
SUMMARY_CSV <- file.path("results", "rq1", "rq1_pairwise_summary.csv")
AVAILABILITY_CSV <- file.path("results", "rq1", "rq1_metric_availability.csv")
LOCAL_CSV <- file.path("results", "rq1", "rq1_local_transition_summary.csv")
OUT_DIR <- file.path("results", "rq1", "figures")
ms_plot_require_files(c(RQ1_LONG, SUMMARY_CSV, AVAILABILITY_CSV, LOCAL_CSV), "RQ1 plotting inputs")
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

DIMENSIONS <- c("placement", "optical", "temporal", "duration")
DIM_TITLES <- c(
  placement = "Placement", optical = "Optical representation",
  temporal = "Temporal resolution", duration = "Monitoring duration"
)
METRIC_CLASSES <- MS_METRIC_CLASSES
FIG1_PANEL_TITLE_SIZE <- 7.6
FIG1_SUBPANEL_TITLE_SIZE <- 6.5

pairwise_artifact <- readRDS(RQ1_LONG)
summary <- readr::read_csv(SUMMARY_CSV, show_col_types = FALSE, progress = FALSE)
availability <- readr::read_csv(AVAILABILITY_CSV, show_col_types = FALSE, progress = FALSE)
local <- readr::read_csv(LOCAL_CSV, show_col_types = FALSE, progress = FALSE)
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
  c("dimension", "comparison_pair_id", "metric", "metric_class", "representation_available"),
  "rq1_metric_availability.csv"
)
ms_plot_require_columns(
  local,
  c("dimension", "metric", "metric_class", "lower_level", "higher_level",
    "orientation_type", "orientation_basis", "G", "A", "B"),
  "rq1_local_transition_summary.csv"
)

RQ1_VERSION <- rq1_pairwise_version(pairwise_artifact)
CORE_VERSION <- ms_plot_assert_core(c(pairwise_artifact$core_artifact_version, summary$core_artifact_version))
ms_plot_assert_prefix(RQ1_VERSION, "rq1_v5_", "rq1_analysis_version")
if (any(!is.na(summary$rq1_analysis_version) & summary$rq1_analysis_version != RQ1_VERSION)) {
  stop("rq1_pairwise_summary contains a different rq1_analysis_version", call. = FALSE)
}

pretty_transition <- function(x) {
  stringr::str_replace_all(as.character(x), "\\s+to\\s+", " to ")
}

theme_fig1 <- function(base_size = 6.8, legend_position = "none") {
  theme_ms(base_size = base_size, legend_position = legend_position) +
    theme(
      panel.border = element_blank(),
      axis.line.x = element_line(colour = "#4F5356", linewidth = .34),
      axis.line.y = element_line(colour = "#4F5356", linewidth = .34),
      panel.grid.major = element_line(colour = "#ECEFF0", linewidth = .22),
      panel.grid.minor = element_blank(),
      axis.ticks = element_line(colour = "#4F5356", linewidth = .28),
      strip.background = element_blank(),
      strip.text = element_text(face = "bold", colour = "#25282A", margin = margin(1, 2, 2, 2)),
      plot.title = element_text(size = FIG1_PANEL_TITLE_SIZE, face = "bold", margin = margin(b = 3)),
      plot.margin = margin(2, 3, 2, 3)
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
# a. Configuration sensitivity as four parallel distribution strips
# -----------------------------------------------------------------------------
# Each metric is first collapsed within a measurement dimension and normalized by
# its largest observed RQ1 A. Points therefore preserve metric-level heterogeneity
# while the class median/IQR supplies a compact cross-class comparison.
pair_display <- summary |>
  filter(is.finite(A_mean_absolute)) |>
  mutate(dimension = as.character(dimension)) |>
  group_by(dimension, metric, metric_class, pair_label, config_a_label, config_b_label) |>
  summarise(A_display = median(A_mean_absolute, na.rm = TRUE), .groups = "drop") |>
  group_by(metric) |>
  mutate(
    A_metric_max = max(A_display, na.rm = TRUE),
    A_relative = if_else(
      is.finite(A_metric_max) & A_metric_max > 0,
      A_display / A_metric_max, NA_real_
    )
  ) |>
  ungroup()

dimension_metric <- pair_display |>
  filter(is.finite(A_relative)) |>
  group_by(dimension, metric, metric_class) |>
  summarise(
    n_oriented_pairs = n(),
    A_relative = median(A_relative, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    dimension = factor(dimension, levels = DIMENSIONS, labels = unname(DIM_TITLES[DIMENSIONS])),
    metric_class = factor(metric_class, levels = METRIC_CLASSES)
  )

dimension_summary <- dimension_metric |>
  group_by(dimension, metric_class) |>
  summarise(
    n_metrics = n_distinct(metric),
    A_relative_median = median(A_relative, na.rm = TRUE),
    A_relative_q25 = quantile(A_relative, .25, na.rm = TRUE, names = FALSE),
    A_relative_q75 = quantile(A_relative, .75, na.rm = TRUE, names = FALSE),
    .groups = "drop"
  )
if (!nrow(dimension_summary)) stop("No RQ1 rows available for Fig. 1a")

readr::write_csv(
  dimension_summary |>
    mutate(dimension = as.character(dimension), metric_class = as.character(metric_class)),
  file.path("results", "rq1", "fig1_panel_a_aggregated.csv"), na = ""
)

p1a <- ggplot(dimension_metric, aes(A_relative, metric_class, color = metric_class)) +
  geom_point(
    position = position_jitter(width = 0, height = .105, seed = 41),
    size = .72, alpha = .34
  ) +
  geom_segment(
    data = dimension_summary,
    aes(
      x = A_relative_q25, xend = A_relative_q75,
      y = metric_class, yend = metric_class, color = metric_class
    ),
    inherit.aes = FALSE, linewidth = 1.10, alpha = .46, lineend = "round"
  ) +
  geom_point(
    data = dimension_summary,
    aes(A_relative_median, metric_class, color = metric_class),
    inherit.aes = FALSE, shape = 18, size = 2.05, alpha = .98
  ) +
  facet_grid(. ~ dimension) +
  scale_color_ms_metric(guide = "none") +
  scale_x_continuous(
    limits = c(0, 1.02), breaks = c(0, .25, .5, .75, 1),
    labels = scales::label_number(accuracy = .01),
    expand = expansion(mult = c(.01, .015))
  ) +
  labs(
    title = "a  Sensitivity of representation classes to measurement configuration",
    x = "relative distortion A  (within-metric maximum = 1)", y = NULL
  ) +
  theme_fig1(base_size = 6.8) +
  theme(
    panel.grid.major.y = element_blank(),
    axis.line.y = element_blank(), axis.ticks.y = element_blank(),
    axis.text.y = element_text(size = 6.0, face = "bold"),
    strip.text.x = element_text(size = FIG1_SUBPANEL_TITLE_SIZE, hjust = .5),
    panel.spacing.x = grid::unit(2.4, "mm"),
    # Reserve the same left-side visual gutter used by panel b's y-axis
    # title/ticks so the first panel-a axis endpoint sits over panel b.
    plot.margin = margin(2, 3, 2, 18)
  )

# -----------------------------------------------------------------------------
# Supplementary complete metric-level atlas retained unchanged in meaning
# -----------------------------------------------------------------------------
atlas <- summary_plot |> filter(is.finite(A_mean_absolute), is.finite(B_mean_signed))
atlas_bg <- availability_plot
p_atlas <- ggplot(atlas, aes(pair_label, metric)) +
  geom_tile(
    data = atlas_bg |> filter(representation_available),
    fill = "#F3F3F3", color = "white", linewidth = .10
  ) +
  geom_point(
    data = atlas_bg |> filter(!representation_available), shape = 4,
    size = .52, stroke = .24, color = "#B5B5B5"
  ) +
  geom_point(
    aes(size = A_mean_absolute, fill = direction_ratio), shape = 21,
    color = "#3B3B3B", stroke = .14, alpha = .94
  ) +
  facet_grid(metric_class ~ dimension, scales = "free", space = "free", switch = "y") +
  ms_direction_scale(name = "B / A") +
  ms_magnitude_size_scale(name = "A = mean |z|", range = c(.25, 3.0)) +
  labs(
    title = "Complete oriented configuration-response atlas",
    x = "scientifically oriented comparison pair", y = NULL
  ) +
  ms_atlas_theme(base_size = 6.1, x_angle = 52) +
  theme(axis.text.x = element_text(size = 5.1))
readr::write_csv(
  atlas |> mutate(dimension = as.character(dimension), metric_class = as.character(metric_class)),
  file.path("results", "rq1", "fig1_pairwise_atlas.csv"), na = ""
)

# -----------------------------------------------------------------------------
# b. Target-aligned signed-vs-absolute distortion geometry
# -----------------------------------------------------------------------------
# Placement and optical geometry use the same grammar and therefore share one
# panel. Free facet scales retain each dimension's usable dynamic range.
target_geometry <- summary |>
  filter(
    dimension %in% c("placement", "optical"),
    is.finite(A_mean_absolute), is.finite(B_mean_signed)
  ) |>
  transmute(
    dimension = as.character(dimension), metric, metric_class,
    A_mean_absolute, B_mean_signed, transition = pair_label,
    A_boot_q025, A_boot_q975, B_boot_q025, B_boot_q975,
    facet_label = case_when(
      dimension == "placement" ~ "Placement | Chest/Wrist to Eye",
      dimension == "optical" ~ "Optical | LIGHT to MEDI",
      TRUE ~ dimension
    )
  ) |>
  mutate(
    facet_label = factor(
      facet_label,
      levels = c("Placement | Chest/Wrist to Eye", "Optical | LIGHT to MEDI")
    ),
    metric_class = factor(metric_class, levels = METRIC_CLASSES)
  )
if (!nrow(target_geometry)) stop("No target-aligned rows available for Fig. 1b")

target_ci <- target_geometry |>
  filter(
    is.finite(A_boot_q025), is.finite(A_boot_q975),
    is.finite(B_boot_q025), is.finite(B_boot_q975)
  )
target_labels <- target_geometry |>
  group_by(facet_label, metric) |>
  slice_max(A_mean_absolute, n = 1, with_ties = FALSE) |>
  ungroup() |>
  group_by(facet_label) |>
  slice_max(A_mean_absolute, n = 2, with_ties = FALSE) |>
  ungroup()

p1b <- ggplot(target_geometry, aes(B_mean_signed, A_mean_absolute, color = metric_class)) +
  geom_vline(xintercept = 0, linewidth = .24, color = "#D7DADD") +
  geom_abline(
    slope = c(-1, 1), intercept = 0, linetype = 2,
    linewidth = .27, color = "#9BA0A3"
  ) +
  geom_segment(
    data = target_ci,
    aes(
      x = B_boot_q025, xend = B_boot_q975,
      y = A_mean_absolute, yend = A_mean_absolute
    ),
    inherit.aes = FALSE, alpha = .10, linewidth = .20, color = "#8E9396"
  ) +
  geom_segment(
    data = target_ci,
    aes(
      x = B_mean_signed, xend = B_mean_signed,
      y = A_boot_q025, yend = A_boot_q975
    ),
    inherit.aes = FALSE, alpha = .10, linewidth = .20, color = "#8E9396"
  ) +
  geom_point(aes(shape = transition), size = 1.28, alpha = .84) +
  geom_text(
    data = target_labels,
    aes(B_mean_signed, A_mean_absolute, label = metric),
    inherit.aes = FALSE, size = 1.70, color = "#303030",
    check_overlap = TRUE, vjust = -.60
  ) +
  facet_wrap(~facet_label, nrow = 1, scales = "free") +
  scale_color_ms_metric(guide = "none") +
  scale_x_continuous(
    trans = scales::transform_asinh(),
    breaks = scales::breaks_extended(n = 6),
    expand = expansion(mult = c(.05, .06))
  ) +
  scale_y_continuous(
    trans = scales::transform_asinh(),
    limits = c(0, NA),
    breaks = scales::breaks_extended(n = 6),
    expand = expansion(mult = c(0, .06))
  ) +
  labs(
    title = "b  Directionality and magnitude of target-aligned distortion",
    x = "B: mean signed change", y = "A: mean absolute change"
  ) +
  theme_fig1(base_size = 6.7) +
  theme(
    panel.grid.major = element_blank(),
    strip.text.x = element_text(size = FIG1_SUBPANEL_TITLE_SIZE, hjust = .5),
    panel.spacing.x = grid::unit(2.6, "mm"),
    # Pull the plotting field left so its vertical axis shares panel a's
    # first-axis endpoint; the tick labels remain inside the page gutter.
    plot.margin = margin(2, 3, 2, -20)
  )

# -----------------------------------------------------------------------------
# c-d. Distribution of where ordered-axis distortion accrues
# -----------------------------------------------------------------------------
# Each metric is normalized over its adjacent steps. Unlike a trajectory plot,
# the horizontal strips do not imply smoothness; they show the full metric-level
# distribution together with class medians and IQRs at every transition.
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
      paste0(from_days, " d to ", to_days, " d"),
      paste(lower_level, "to", higher_level)
    ),
    step_order = case_when(
      dimension == "duration" ~ as.numeric(from_days),
      dimension == "temporal" ~ match(
        transition,
        c(
          "30 min to 15 min", "15 min to 5 min", "5 min to 1 min",
          "1 min to 30 s", "30 s to 20 s", "20 s to 10 s"
        )
      ),
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
  filter(is.finite(G_share))

CLASS_ROW_OFFSETS <- setNames(seq(-.22, .22, length.out = length(METRIC_CLASSES)), METRIC_CLASSES)
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
    row_offset = unname(CLASS_ROW_OFFSETS[as.character(metric_class)]),
    y_summary = step_order + row_offset
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

local_distribution_panel <- function(dim, subpanel_title = DIM_TITLES[[dim]]) {
  d <- local_display |> filter(dimension == dim)
  ds <- local_summary |> filter(dimension == dim)
  if (!nrow(d) || !nrow(ds)) stop("No local response rows for Fig. 1 dimension: ", dim)

  labels <- d |> distinct(step_order, transition) |> arrange(step_order)
  xmax <- max(ds$share_q75, ds$share_median, na.rm = TRUE) * 1.18
  xmax <- min(1, max(.32, xmax))

  ggplot() +
    geom_point(
      data = d,
      aes(G_share, step_order, color = metric_class),
      position = position_jitter(width = 0, height = .070, seed = 73),
      size = .48, alpha = .18
    ) +
    geom_segment(
      data = ds,
      aes(
        x = share_q25, xend = share_q75,
        y = y_summary, yend = y_summary, color = metric_class
      ),
      linewidth = .74, alpha = .47, lineend = "round"
    ) +
    geom_point(
      data = ds,
      aes(share_median, y_summary, color = metric_class),
      shape = 18, size = 1.48, alpha = .98
    ) +
    scale_color_ms_metric(guide = "none") +
    scale_x_continuous(
      limits = c(0, xmax),
      labels = scales::label_percent(accuracy = 1),
      breaks = scales::breaks_extended(n = 4),
      expand = expansion(mult = c(.01, .03))
    ) +
    scale_y_reverse(
      breaks = labels$step_order,
      labels = pretty_transition(labels$transition),
      expand = expansion(add = c(.38, .38))
    ) +
    labs(
      title = subpanel_title,
      x = "share of local response", y = NULL
    ) +
    theme_fig1(base_size = 6.55) +
    theme(
      panel.grid.major.y = element_blank(),
      axis.line.y = element_blank(), axis.ticks.y = element_blank(),
      axis.text.y = element_text(size = 5.35),
      plot.title = element_text(
        size = FIG1_SUBPANEL_TITLE_SIZE, face = "bold", hjust = .5,
        margin = margin(b = 2)
      ),
      plot.margin = margin(2, 3, 2, 3)
    )
}

p1c <- local_distribution_panel("temporal", "Temporal resolution") +
  theme(
    axis.title.x = element_blank(),
    # Match the title baseline to panel b while preserving the equal c/d rows.
    plot.margin = margin(12, 3, .5, 3)
  )
p1d <- local_distribution_panel("duration", "Monitoring duration") +
  theme(
    plot.margin = margin(.5, 3, .5, 3)
  )

# One compact legend governs the main figure. Panel a also labels every class,
# so the legend is a cross-panel color key rather than the only class identifier.
metric_legend_source <- ggplot(
  tibble(
    metric_class = factor(METRIC_CLASSES, levels = METRIC_CLASSES),
    x = seq_along(METRIC_CLASSES), y = 1
  ),
  aes(x, y, color = metric_class)
) +
  geom_point(size = 1.55) +
  scale_color_ms_metric() +
  guides(color = guide_legend(
    title = NULL, nrow = 1, byrow = TRUE,
    override.aes = list(size = 1.55)
  )) +
  theme_void(base_family = MS_FONT, base_size = 7) +
  theme(
    legend.position = "bottom", legend.direction = "horizontal",
    legend.margin = margin(0, 0, 0, 0),
    legend.box.margin = margin(0, 0, 0, 0),
    legend.text = element_text(size = 5.45),
    legend.key.width = grid::unit(3.4, "mm")
  )
metric_legend <- cowplot::get_legend(metric_legend_source)

# Asymmetric composition: the distribution atlas is a compact header; the A/B
# geometry occupies the main lower-left field; ordered-axis distributions form a
# dense diagnostic column at right. This avoids equal-size dashboard panels.
panel_c_spacer <- cowplot::ggdraw()
right_column_core <- cowplot::plot_grid(
  panel_c_spacer, p1c, p1d, ncol = 1, rel_heights = c(.14, 1.05, .95),
  align = "v", axis = "l", greedy = TRUE
)
right_column <- cowplot::ggdraw() +
  # A tiny common downward nudge closes the remaining raster-pixel gap
  # between the c-top/d-bottom frames and panel b's corresponding edges.
  cowplot::draw_plot(right_column_core, x = 0, y = -.004, width = 1, height = 1) +
  cowplot::draw_label(
    "c  Ordered-axis local response",
    x = 0, y = .92, hjust = 0, vjust = .5,
    size = FIG1_PANEL_TITLE_SIZE, fontface = "bold",
    colour = "#151515", fontfamily = MS_FONT
  )
lower <- cowplot::plot_grid(
  p1b, right_column, ncol = 2, rel_widths = c(1.43, .87),
  # Expand the right column around the lower-row center so its actual upper
  # and lower plotting-frame boundaries track panel b together.
  align = "h", axis = "b", greedy = TRUE, scale = c(1, 1.134)
)
fig1body <- cowplot::plot_grid(
  p1a, lower, ncol = 1, rel_heights = c(.72, 1.58),
  align = "v", axis = "l", greedy = TRUE
)
fig1 <- cowplot::plot_grid(
  metric_legend, fig1body, ncol = 1,
  rel_heights = c(.040, 1), align = "v", greedy = TRUE
)
ms_plot_save(fig1, file.path(OUT_DIR, "Fig1_RQ1.pdf"), 8.6, 5.7)
ms_plot_save(fig1, file.path(OUT_DIR, "Fig1_RQ1.png"), 8.6, 5.7)

# -----------------------------------------------------------------------------
# Supplementary figures
# -----------------------------------------------------------------------------
ms_plot_save(p_atlas, file.path(OUT_DIR, "FigS_RQ1_pairwise_atlas.pdf"), 16, 10)
ms_plot_save(p_atlas, file.path(OUT_DIR, "FigS_RQ1_pairwise_atlas.png"), 16, 10)

distribution_panel <- function(dim, letter) {
  d <- summary_plot |>
    filter(dimension == dim, is.finite(median_z)) |>
    mutate(metric = forcats::fct_rev(metric))
  if (!nrow(d)) stop("No RQ1 distribution rows for dimension: ", dim)
  ggplot(d, aes(y = metric, color = metric_class)) +
    geom_vline(xintercept = 0, linewidth = .28, color = "#B8B8B8") +
    geom_segment(aes(x = p025_z, xend = p975_z, yend = metric), alpha = .30, linewidth = .35) +
    geom_segment(aes(x = q25_z, xend = q75_z, yend = metric), alpha = .72, linewidth = 1.05) +
    geom_point(aes(x = median_z), size = .72, alpha = .90) +
    facet_grid(metric_class ~ ., scales = "free_y", space = "free_y", switch = "y") +
    scale_color_ms_metric() +
    scale_x_continuous(trans = scales::transform_asinh(), breaks = scales::breaks_extended(n = 4)) +
    labs(
      title = paste0(letter, "  ", DIM_TITLES[[dim]]),
      x = "standardized representation change, z", y = NULL
    ) +
    theme_ms(base_size = 6.0, legend_position = "none") +
    theme(
      panel.grid.major.y = element_blank(),
      axis.text.y = element_text(size = 4.8),
      axis.ticks.y = element_blank(),
      strip.text.y.left = element_text(size = 5.1)
    )
}

distribution_grid <- cowplot::plot_grid(
  plotlist = map2(DIMENSIONS, letters[1:4], distribution_panel),
  ncol = 4, align = "hv", axis = "tblr"
)
ms_plot_save(distribution_grid, file.path(OUT_DIR, "FigS_RQ1_pairwise_distributions.pdf"), 16, 9.2)
ms_plot_save(distribution_grid, file.path(OUT_DIR, "FigS_RQ1_pairwise_distributions.png"), 16, 9.2)

p_availability <- ggplot(availability_plot, aes(pair_label, metric, fill = representation_available)) +
  geom_tile(color = "white", linewidth = .12) +
  facet_grid(metric_class ~ dimension, scales = "free", space = "free", switch = "y") +
  scale_fill_manual(
    values = c(`TRUE` = MS_PRIMARY, `FALSE` = "#D9D9D9"),
    labels = c(`TRUE` = "available", `FALSE` = "unavailable"), name = NULL
  ) +
  labs(
    title = "RQ1 representation availability by oriented comparison pair",
    x = NULL, y = NULL
  ) +
  ms_atlas_theme(base_size = 6.1, x_angle = 52)
ms_plot_save(p_availability, file.path(OUT_DIR, "FigS_RQ1_availability_atlas.pdf"), 16, 10)
ms_plot_save(p_availability, file.path(OUT_DIR, "FigS_RQ1_availability_atlas.png"), 16, 10)

ms_plot_write_manifest(
  file.path(OUT_DIR, "figure_artifact_manifest.csv"),
  tibble(
    figure = c(
      "Fig1_RQ1", "FigS_RQ1_pairwise_atlas",
      "FigS_RQ1_pairwise_distributions", "FigS_RQ1_availability_atlas"
    ),
    input_artifact = c(
      "rq1_pairwise_summary + rq1_local_transition_summary",
      "rq1_pairwise_summary + rq1_metric_availability",
      "rq1_pairwise_summary",
      "rq1_metric_availability"
    ),
    core_artifact_version = CORE_VERSION,
    rq1_analysis_version = RQ1_VERSION,
    rq2_analysis_version = NA_character_,
    rq3_analysis_version = NA_character_
  )
)
message("Fig. 1 complete: compact distribution-led overview, combined A/B geometry, and local-response distribution strips; full metric atlas retained as supplement.")
