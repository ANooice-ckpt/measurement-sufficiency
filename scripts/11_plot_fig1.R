suppressPackageStartupMessages({
  library(tidyverse)
  library(cowplot)
})
source("scripts/utils/figure_style.R")
source("scripts/utils/figure_atlas.R")
source("scripts/utils/rq1_pairwise_artifacts.R")
source("scripts/utils/plot_contracts.R")

# Fig. 1 is deliberately hierarchical rather than panel-symmetric. A full-width
# response fingerprint carries the overview; four compact panels underneath
# explain directionality and where ordered-axis distortion accumulates.
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
  stringr::str_replace_all(as.character(x), "\\s+to\\s+", " → ")
}

# Main-text panels use open axes rather than boxed analytical panels. The shared
# manuscript palette remains unchanged; only visual hierarchy is figure-specific.
theme_fig1 <- function(base_size = 6.4, legend_position = "none") {
  theme_ms(base_size = base_size, legend_position = legend_position) +
    theme(
      panel.border = element_blank(),
      axis.line.x = element_line(colour = "#4B4F52", linewidth = .34),
      axis.line.y = element_line(colour = "#4B4F52", linewidth = .34),
      panel.grid.major = element_line(colour = "#ECEFF0", linewidth = .22, linetype = 1),
      panel.grid.minor = element_blank(),
      axis.ticks = element_line(colour = "#4B4F52", linewidth = .28),
      plot.title = element_text(size = base_size + .8, face = "bold", margin = margin(b = 3)),
      plot.margin = margin(3, 5, 3, 4)
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
  left_join(summary |> distinct(dimension, comparison_pair_id, pair_label),
            by = c("dimension", "comparison_pair_id")) |>
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
# a. Full-width measurement-response fingerprint
# -----------------------------------------------------------------------------
# First collapse duplicate display rows within an oriented pair, then normalize
# A within each metric by its largest observed RQ1 distortion. A dimension-level
# score is the median relative A across that dimension's oriented comparisons.
# This makes the four dimensions comparable without allowing raw metric scale or
# a denser comparison lattice to dominate the overview.
pair_display <- summary |>
  filter(is.finite(A_mean_absolute)) |>
  mutate(dimension = as.character(dimension)) |>
  group_by(dimension, metric, metric_class, pair_label, config_a_label, config_b_label) |>
  summarise(A_display = median(A_mean_absolute, na.rm = TRUE), .groups = "drop") |>
  group_by(metric) |>
  mutate(
    A_metric_max = max(A_display, na.rm = TRUE),
    A_relative = if_else(is.finite(A_metric_max) & A_metric_max > 0,
                         A_display / A_metric_max, NA_real_)
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
    dimension_rank = as.integer(dimension),
    metric_class = factor(metric_class, levels = METRIC_CLASSES)
  )

CLASS_OFFSETS <- setNames(seq(-.105, .105, length.out = length(METRIC_CLASSES)), METRIC_CLASSES)
dimension_summary <- dimension_metric |>
  group_by(dimension, dimension_rank, metric_class) |>
  summarise(
    n_metrics = n_distinct(metric),
    A_relative_median = median(A_relative, na.rm = TRUE),
    A_relative_q25 = quantile(A_relative, .25, na.rm = TRUE, names = FALSE),
    A_relative_q75 = quantile(A_relative, .75, na.rm = TRUE, names = FALSE),
    .groups = "drop"
  ) |>
  mutate(
    class_offset = unname(CLASS_OFFSETS[as.character(metric_class)]),
    x_summary = dimension_rank + class_offset
  )
if (!nrow(dimension_summary)) stop("No RQ1 rows available for Fig. 1a response fingerprint")

readr::write_csv(
  dimension_summary |>
    mutate(dimension = as.character(dimension), metric_class = as.character(metric_class)) |>
    select(dimension, dimension_rank, metric_class, n_metrics,
           A_relative_median, A_relative_q25, A_relative_q75),
  file.path("results", "rq1", "fig1_panel_a_aggregated.csv"), na = ""
)

p1a <- ggplot() +
  geom_line(
    data = dimension_metric,
    aes(dimension_rank, A_relative, group = metric, color = metric_class),
    linewidth = .25, alpha = .075
  ) +
  geom_point(
    data = dimension_metric,
    aes(dimension_rank, A_relative, color = metric_class),
    size = .52, alpha = .17
  ) +
  geom_linerange(
    data = dimension_summary,
    aes(x_summary, ymin = A_relative_q25, ymax = A_relative_q75, color = metric_class),
    linewidth = .48, alpha = .52
  ) +
  geom_line(
    data = dimension_summary,
    aes(x_summary, A_relative_median, group = metric_class, color = metric_class),
    linewidth = .90, alpha = .96
  ) +
  geom_point(
    data = dimension_summary,
    aes(x_summary, A_relative_median, color = metric_class),
    size = 1.35, alpha = .98
  ) +
  scale_color_ms_metric(guide = "none") +
  scale_x_continuous(
    breaks = 1:4, labels = unname(DIM_TITLES[DIMENSIONS]),
    limits = c(.72, 4.30), expand = c(0, 0)
  ) +
  scale_y_continuous(
    limits = c(0, 1.03), breaks = c(0, .25, .5, .75, 1),
    expand = expansion(mult = c(0, .015))
  ) +
  labs(
    title = "a  Measurement-response fingerprint across metric classes",
    x = NULL,
    y = "relative distortion A\n(within-metric max = 1)"
  ) +
  theme_fig1(base_size = 6.65) +
  theme(
    panel.grid.major.x = element_blank(),
    axis.text.x = element_text(size = 6.0, face = "bold", margin = margin(t = 3)),
    plot.margin = margin(2, 8, 5, 5)
  )

# -----------------------------------------------------------------------------
# Supplementary complete metric-level atlas retained unchanged in meaning
# -----------------------------------------------------------------------------
atlas <- summary_plot |> filter(is.finite(A_mean_absolute), is.finite(B_mean_signed))
atlas_bg <- availability_plot
p_atlas <- ggplot(atlas, aes(pair_label, metric)) +
  geom_tile(data = atlas_bg |> filter(representation_available), fill = "#F3F3F3",
            color = "white", linewidth = .10) +
  geom_point(data = atlas_bg |> filter(!representation_available), shape = 4,
             size = .52, stroke = .24, color = "#B5B5B5") +
  geom_point(aes(size = A_mean_absolute, fill = direction_ratio), shape = 21,
             color = "#3B3B3B", stroke = .14, alpha = .94) +
  facet_grid(metric_class ~ dimension, scales = "free", space = "free", switch = "y") +
  ms_direction_scale(name = "B / A") +
  ms_magnitude_size_scale(name = "A = mean |z|", range = c(.25, 3.0)) +
  labs(title = "Complete oriented configuration-response atlas",
       x = "scientifically oriented comparison pair", y = NULL) +
  ms_atlas_theme(base_size = 6.1, x_angle = 52) +
  theme(axis.text.x = element_text(size = 5.1))
readr::write_csv(
  atlas |> mutate(dimension = as.character(dimension), metric_class = as.character(metric_class)),
  file.path("results", "rq1", "fig1_pairwise_atlas.csv"), na = ""
)

# -----------------------------------------------------------------------------
# b-c. Target-aligned A/B geometry
# -----------------------------------------------------------------------------
target_geometry_panel <- function(dim, letter) {
  d <- summary |>
    filter(dimension == dim, is.finite(A_mean_absolute), is.finite(B_mean_signed)) |>
    transmute(
      metric, metric_class, A_mean_absolute, B_mean_signed,
      transition = pair_label, A_boot_q025, A_boot_q975, B_boot_q025, B_boot_q975
    )
  if (!nrow(d)) stop("No target-aligned A/B rows for Fig. 1 dimension: ", dim)
  d <- d |> mutate(metric_class = factor(metric_class, levels = METRIC_CLASSES))

  lab <- d |>
    group_by(metric) |>
    slice_max(A_mean_absolute, n = 1, with_ties = FALSE) |>
    ungroup() |>
    slice_max(A_mean_absolute, n = 2, with_ties = FALSE)

  max_display <- max(c(
    d$A_mean_absolute, abs(d$B_mean_signed), d$A_boot_q025, d$A_boot_q975,
    d$B_boot_q025, d$B_boot_q975
  ), na.rm = TRUE)
  if (!is.finite(max_display) || max_display <= 0) max_display <- 1

  ci <- d |>
    filter(is.finite(A_boot_q025), is.finite(A_boot_q975),
           is.finite(B_boot_q025), is.finite(B_boot_q975))
  transition_note <- if (dim == "placement") {
    "circle: Chest → Eye; triangle: Wrist → Eye"
  } else {
    "LIGHT → MEDI"
  }

  ggplot(d, aes(B_mean_signed, A_mean_absolute, color = metric_class)) +
    geom_vline(xintercept = 0, linewidth = .24, color = "#D7DADD") +
    geom_abline(slope = c(-1, 1), intercept = 0, linetype = 2,
                linewidth = .28, color = "#9BA0A3") +
    geom_segment(
      data = ci,
      aes(x = B_boot_q025, xend = B_boot_q975,
          y = A_mean_absolute, yend = A_mean_absolute),
      inherit.aes = FALSE, alpha = .10, linewidth = .22, color = "#8E9396"
    ) +
    geom_segment(
      data = ci,
      aes(x = B_mean_signed, xend = B_mean_signed,
          y = A_boot_q025, yend = A_boot_q975),
      inherit.aes = FALSE, alpha = .10, linewidth = .22, color = "#8E9396"
    ) +
    geom_point(aes(shape = transition), size = 1.28, alpha = .88) +
    geom_text(
      data = lab,
      aes(x = B_mean_signed, y = A_mean_absolute, label = metric),
      inherit.aes = FALSE, size = 1.78, color = "#303030",
      check_overlap = TRUE, vjust = -.65
    ) +
    scale_color_ms_metric(guide = "none") +
    scale_x_continuous(
      trans = scales::transform_asinh(),
      limits = c(-max_display * 1.055, max_display * 1.055),
      breaks = scales::breaks_extended(n = 4)
    ) +
    scale_y_continuous(
      trans = scales::transform_asinh(), limits = c(0, max_display * 1.055),
      breaks = scales::breaks_extended(n = 4)
    ) +
    labs(
      title = paste0(letter, "  ", DIM_TITLES[[dim]], ": signed vs absolute distortion"),
      subtitle = transition_note,
      x = "B: mean signed change", y = "A: mean absolute change"
    ) +
    theme_fig1(base_size = 6.2) +
    theme(
      panel.grid.major = element_blank(),
      plot.subtitle = element_text(size = 4.8, colour = "#666A6D", margin = margin(t = -1, b = 1)),
      plot.margin = margin(2, 6, 2, 4)
    )
}

# -----------------------------------------------------------------------------
# d-e. Where ordered-axis distortion accrues
# -----------------------------------------------------------------------------
# Raw G mixes overall metric sensitivity with response shape and was dominated by
# a few extreme metrics. For the main figure each metric is therefore normalized
# over adjacent steps: G_share = G_step / sum(G_steps). The panels answer where
# along the ordered axis a metric's local distortion is concentrated. Raw G is
# unchanged upstream and remains available in rq1_local_transition_summary.csv.
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
        c("30 min to 15 min", "15 min to 5 min", "5 min to 1 min",
          "1 min to 30 s", "30 s to 20 s", "20 s to 10 s")
      ),
      TRUE ~ NA_real_
    )
  ) |>
  filter(dimension %in% c("temporal", "duration"), is.finite(G), is.finite(step_order)) |>
  group_by(dimension, metric, metric_class, transition, step_order) |>
  summarise(G_display = median(G, na.rm = TRUE), .groups = "drop") |>
  group_by(dimension, metric, metric_class) |>
  mutate(
    G_total = sum(G_display, na.rm = TRUE),
    G_share = if_else(is.finite(G_total) & G_total > 0, G_display / G_total, NA_real_)
  ) |>
  ungroup() |>
  filter(is.finite(G_share))

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
    class_offset = unname(CLASS_OFFSETS[as.character(metric_class)]),
    x_summary = step_order + class_offset
  )

readr::write_csv(
  local_summary |>
    mutate(metric_class = as.character(metric_class)) |>
    select(dimension, metric_class, transition, step_order, n_metrics,
           share_median, share_q25, share_q75),
  file.path("results", "rq1", "fig1_local_response_aggregated.csv"), na = ""
)

local_response_panel <- function(dim, letter) {
  d <- local_display |> filter(dimension == dim)
  ds <- local_summary |> filter(dimension == dim)
  if (!nrow(d) || !nrow(ds)) stop("No local G rows for Fig. 1 dimension: ", dim)

  labels <- d |> distinct(step_order, transition) |> arrange(step_order)
  ymax <- max(c(ds$share_q75, ds$share_median, d$G_share), na.rm = TRUE)
  ymax <- min(1, max(.36, ymax * 1.08))

  ggplot() +
    geom_line(
      data = d,
      aes(step_order, G_share, group = metric, color = metric_class),
      linewidth = .24, alpha = .065
    ) +
    geom_linerange(
      data = ds,
      aes(x_summary, ymin = share_q25, ymax = share_q75, color = metric_class),
      linewidth = .44, alpha = .48
    ) +
    geom_line(
      data = ds,
      aes(x_summary, share_median, group = metric_class, color = metric_class),
      linewidth = .82, alpha = .96
    ) +
    geom_point(
      data = ds,
      aes(x_summary, share_median, color = metric_class),
      size = 1.08, alpha = .98
    ) +
    scale_color_ms_metric(guide = "none") +
    scale_x_continuous(
      breaks = labels$step_order,
      labels = pretty_transition(labels$transition),
      expand = expansion(mult = c(.035, .045))
    ) +
    scale_y_continuous(
      limits = c(0, ymax),
      labels = scales::label_percent(accuracy = 1),
      breaks = scales::breaks_extended(n = 4),
      expand = expansion(mult = c(0, .02))
    ) +
    labs(
      title = paste0(letter, "  Where ", tolower(DIM_TITLES[[dim]]), " distortion accrues"),
      x = NULL, y = "share of local response"
    ) +
    theme_fig1(base_size = 6.2) +
    theme(
      panel.grid.major.x = element_blank(),
      axis.text.x = element_text(angle = 27, hjust = 1, size = 4.9),
      plot.margin = margin(2, 6, 2, 4)
    )
}

p1b <- target_geometry_panel("placement", "b")
p1c <- target_geometry_panel("optical", "c")
p1d <- local_response_panel("temporal", "d")
p1e <- local_response_panel("duration", "e")

# One compact legend governs the full main figure.
metric_legend_source <- ggplot(
  tibble(
    metric_class = factor(METRIC_CLASSES, levels = METRIC_CLASSES),
    x = seq_along(METRIC_CLASSES), y = 1
  ),
  aes(x, y, color = metric_class)
) +
  geom_line(aes(group = metric_class), linewidth = .8) +
  geom_point(size = 1.7) +
  scale_color_ms_metric() +
  guides(color = guide_legend(
    title = NULL, nrow = 1, byrow = TRUE,
    override.aes = list(size = 1.45, linewidth = .75)
  )) +
  theme_void(base_family = MS_FONT, base_size = 7) +
  theme(
    legend.position = "bottom", legend.direction = "horizontal",
    legend.margin = margin(0, 0, 0, 0), legend.box.margin = margin(0, 0, 0, 0),
    legend.text = element_text(size = 5.35),
    legend.key.width = grid::unit(4.0, "mm")
  )
metric_legend <- cowplot::get_legend(metric_legend_source)

# Hierarchical composition: a is the visual anchor; b-e are compact explanatory
# panels. The open-axis grammar keeps the five panels visually connected.
middle <- cowplot::plot_grid(
  p1b, p1c, ncol = 2, rel_widths = c(1.03, .97),
  align = "hv", axis = "tblr", greedy = TRUE
)
bottom <- cowplot::plot_grid(
  p1d, p1e, ncol = 2, rel_widths = c(1.03, .97),
  align = "hv", axis = "tblr", greedy = TRUE
)
fig1body <- cowplot::plot_grid(
  p1a, middle, bottom, ncol = 1,
  rel_heights = c(1.18, .83, .92),
  align = "v", axis = "l", greedy = TRUE
)
fig1 <- cowplot::plot_grid(
  metric_legend, fig1body, ncol = 1,
  rel_heights = c(.042, 1), align = "v", greedy = TRUE
)
ms_plot_save(fig1, file.path(OUT_DIR, "Fig1_RQ1.pdf"), 12.4, 7.9)
ms_plot_save(fig1, file.path(OUT_DIR, "Fig1_RQ1.png"), 12.4, 7.9)

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
    labs(title = paste0(letter, "  ", DIM_TITLES[[dim]]),
         x = "standardized representation change, z", y = NULL) +
    theme_ms(base_size = 6.0, legend_position = "none") +
    theme(
      panel.grid.major.y = element_blank(), axis.text.y = element_text(size = 4.8),
      axis.ticks.y = element_blank(), strip.text.y.left = element_text(size = 5.1)
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
  labs(title = "RQ1 representation availability by oriented comparison pair", x = NULL, y = NULL) +
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
message("Fig. 1 complete: full-width response fingerprint with compact A/B geometry and normalized local-response profiles; full metric atlas retained as supplement.")