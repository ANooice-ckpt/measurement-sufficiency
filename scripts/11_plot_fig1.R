suppressPackageStartupMessages({
  library(tidyverse)
  library(cowplot)
})
source("scripts/utils/figure_style.R")
source("scripts/utils/figure_atlas.R")
source("scripts/utils/rq1_pairwise_artifacts.R")
source("scripts/utils/plot_contracts.R")

# Fig. 1 follows the RQ1 logic: prespecified scientific orientation ->
# representation response -> local change along ordered measurement axes.
# Complete metric-level pairwise detail remains available in supplementary figures.
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
  left_join(summary |> distinct(dimension, comparison_pair_id, pair_label), by = c("dimension", "comparison_pair_id")) |>
  mutate(pair_label = coalesce(pair_label, as.character(comparison_pair_id))) |>
  distinct(dimension, pair_label, metric, metric_class, representation_available)
local <- local |>
  mutate(metric_class = factor(metric_class, levels = METRIC_CLASSES), dimension = factor(dimension, levels = DIMENSIONS))

metric_order <- ms_metric_order(summary |> mutate(dimension = as.character(dimension)))
readr::write_csv(metric_order, file.path("results", "rq1", "figure_metric_order.csv"), na = "")
summary_plot <- summary |> mutate(dimension = as.character(dimension)) |> ms_add_metric_order(metric_order)
availability_plot <- availability |> mutate(dimension = as.character(dimension)) |> ms_add_metric_order(metric_order)

# a. Aggregated configuration-response structure. The main panel should expose
# topology rather than ask the reader to decode all 54 metrics simultaneously.
# A is first collapsed to one display value per metric x oriented pair, then
# divided by that metric's maximum A across the complete RQ1 configuration
# space. Thus relative A retains within-metric effect structure while preventing
# naturally large-scale metrics from dominating class-level summaries.
pair_display <- summary |>
  filter(is.finite(A_mean_absolute)) |>
  mutate(dimension = as.character(dimension)) |>
  group_by(dimension, metric, metric_class, pair_label, config_a_label, config_b_label) |>
  summarise(A_display = median(A_mean_absolute, na.rm = TRUE), .groups = "drop") |>
  group_by(metric) |>
  mutate(
    A_metric_max = max(A_display, na.rm = TRUE),
    A_relative = if_else(is.finite(A_metric_max) & A_metric_max > 0, A_display / A_metric_max, NA_real_)
  ) |>
  ungroup()

alignment_summary <- pair_display |>
  filter(dimension %in% c("placement", "optical"), is.finite(A_relative)) |>
  group_by(dimension, pair_label, metric_class) |>
  summarise(
    n_metrics = n_distinct(metric),
    A_relative_median = median(A_relative, na.rm = TRUE),
    A_relative_q25 = quantile(A_relative, .25, na.rm = TRUE, names = FALSE),
    A_relative_q75 = quantile(A_relative, .75, na.rm = TRUE, names = FALSE),
    .groups = "drop"
  ) |>
  mutate(
    metric_class = factor(metric_class, levels = METRIC_CLASSES),
    dimension = factor(dimension, levels = c("placement", "optical"),
                       labels = c("Placement", "Optical representation"))
  )
if (!nrow(alignment_summary)) stop("No target-alignment rows available for aggregated Fig. 1a")

p1a_alignment <- ggplot(alignment_summary, aes(pair_label, metric_class, fill = A_relative_median)) +
  geom_tile(color = "white", linewidth = .18) +
  facet_grid(. ~ dimension, scales = "free_x", space = "free_x") +
  scale_fill_ms_sequential(
    limits = c(0, 1), oob = scales::squish,
    labels = scales::label_number(accuracy = .1), name = "median relative A"
  ) +
  labs(title = "a  Target-aligned response", x = "oriented comparison", y = NULL) +
  theme_ms(base_size = 6.1, legend_position = "bottom") +
  theme(
    axis.text.x = element_text(angle = 34, hjust = 1, size = 4.7),
    axis.ticks.y = element_blank(), panel.grid = element_blank(),
    plot.margin = margin(2, 2, 2, 2),
    legend.margin = margin(0, 0, 0, 0),
    legend.box.margin = margin(0, 0, 0, 0),
    legend.key.height = grid::unit(2.4, "mm"),
    legend.text = element_text(size = 4.7),
    legend.title = element_text(size = 5.0)
  )

parse_temporal_seconds <- function(x) {
  x <- str_to_lower(str_trim(as.character(x)))
  value <- suppressWarnings(as.numeric(str_extract(x, "[0-9.]+")))
  case_when(
    str_detect(x, "min") ~ value * 60,
    str_detect(x, "s") ~ value,
    TRUE ~ NA_real_
  )
}
parse_duration_days <- function(x) suppressWarnings(as.numeric(str_extract(as.character(x), "[0-9.]+")))

ordered_candidates <- pair_display |>
  filter(dimension %in% c("temporal", "duration"), is.finite(A_relative)) |>
  mutate(
    state_a_value = if_else(dimension == "temporal", parse_temporal_seconds(config_a_label), parse_duration_days(config_a_label)),
    state_b_value = if_else(dimension == "temporal", parse_temporal_seconds(config_b_label), parse_duration_days(config_b_label)),
    state_rank_a = case_when(
      dimension == "temporal" ~ match(state_a_value, c(1800, 900, 300, 60, 30, 20, 10)),
      dimension == "duration" ~ match(state_a_value, 1:6),
      TRUE ~ NA_integer_
    ),
    state_rank_b = case_when(
      dimension == "temporal" ~ match(state_b_value, c(1800, 900, 300, 60, 30, 20, 10)),
      dimension == "duration" ~ match(state_b_value, 1:6),
      TRUE ~ NA_integer_
    )
  )
if (any(ordered_candidates$dimension == "temporal" & (!is.finite(ordered_candidates$state_rank_a) | !is.finite(ordered_candidates$state_rank_b)))) {
  stop("Fig. 1a could not parse one or more temporal configuration labels")
}
if (any(ordered_candidates$dimension == "duration" & (!is.finite(ordered_candidates$state_rank_a) | !is.finite(ordered_candidates$state_rank_b)))) {
  stop("Fig. 1a could not parse one or more duration configuration labels")
}

ordered_metric <- ordered_candidates |>
  filter(state_rank_a == 1L, state_rank_b > 1L) |>
  group_by(dimension, metric, metric_class, state_rank = state_rank_b, state_label = config_b_label) |>
  summarise(A_relative = median(A_relative, na.rm = TRUE), .groups = "drop")
ordered_baseline <- ordered_candidates |>
  filter(state_rank_a == 1L) |>
  distinct(dimension, metric, metric_class, state_label = config_a_label) |>
  mutate(state_rank = 1L, A_relative = 0)
ordered_metric <- bind_rows(ordered_baseline, ordered_metric) |>
  group_by(dimension, metric, metric_class, state_rank) |>
  summarise(state_label = first(state_label), A_relative = median(A_relative, na.rm = TRUE), .groups = "drop")
if (!nrow(ordered_metric)) stop("No ordered-axis rows available for aggregated Fig. 1a")

ordered_summary <- ordered_metric |>
  group_by(dimension, state_rank, state_label, metric_class) |>
  summarise(
    n_metrics = n_distinct(metric),
    A_relative_median = median(A_relative, na.rm = TRUE),
    A_relative_q25 = quantile(A_relative, .25, na.rm = TRUE, names = FALSE),
    A_relative_q75 = quantile(A_relative, .75, na.rm = TRUE, names = FALSE),
    .groups = "drop"
  )

ordered_response_panel <- function(dim, title) {
  dm <- ordered_metric |> filter(dimension == dim)
  ds <- ordered_summary |>
    filter(dimension == dim) |>
    mutate(metric_class = factor(metric_class, levels = METRIC_CLASSES))
  if (!nrow(dm) || !nrow(ds)) stop("No aggregated ordered response rows for Fig. 1a dimension: ", dim)
  labels <- dm |>
    group_by(state_rank) |>
    summarise(state_label = first(state_label), .groups = "drop") |>
    arrange(state_rank)
  ggplot() +
    geom_line(
      data = dm,
      aes(state_rank, A_relative, group = metric),
      color = "#AFAFAF", alpha = .12, linewidth = .24
    ) +
    geom_ribbon(
      data = ds,
      aes(state_rank, ymin = A_relative_q25, ymax = A_relative_q75,
          fill = metric_class, group = metric_class),
      alpha = .10, color = NA
    ) +
    geom_line(
      data = ds,
      aes(state_rank, A_relative_median, color = metric_class, group = metric_class),
      linewidth = .62, alpha = .92
    ) +
    geom_point(
      data = ds,
      aes(state_rank, A_relative_median, color = metric_class),
      size = .70, alpha = .92
    ) +
    scale_color_ms_metric() +
    scale_fill_ms_metric(guide = "none") +
    scale_x_continuous(breaks = labels$state_rank, labels = labels$state_label) +
    scale_y_continuous(limits = c(0, 1.02), breaks = c(0, .25, .5, .75, 1)) +
    labs(title = title, x = "measurement state", y = "relative A\n(within-metric max = 1)") +
    theme_ms(base_size = 6.1, legend_position = "none") +
    theme(axis.text.x = element_text(angle = 38, hjust = 1, size = 5.0))
}

p1a_temporal <- ordered_response_panel("temporal", "Temporal resolution")
p1a_duration <- ordered_response_panel("duration", "Monitoring duration")
p1a <- cowplot::plot_grid(
  p1a_alignment, p1a_temporal, p1a_duration,
  ncol = 3, rel_widths = c(.98, 1.12, 1.02), align = "hv", axis = "tblr", greedy = TRUE
)

panel_a_export <- bind_rows(
  alignment_summary |>
    transmute(
      display = "target_alignment", dimension = as.character(dimension), pair_label,
      state_rank = NA_integer_, state_label = NA_character_, metric_class = as.character(metric_class),
      n_metrics, A_relative_median, A_relative_q25, A_relative_q75
    ),
  ordered_summary |>
    transmute(
      display = "measurement_requirement", dimension, pair_label = NA_character_,
      state_rank, state_label, metric_class = as.character(metric_class),
      n_metrics, A_relative_median, A_relative_q25, A_relative_q75
    )
)
readr::write_csv(panel_a_export, file.path("results", "rq1", "fig1_panel_a_aggregated.csv"), na = "")

# The former main-panel atlas is retained verbatim as supplementary detail.
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
  labs(title = "Complete oriented configuration-response atlas", x = "scientifically oriented comparison pair", y = NULL) +
  ms_atlas_theme(base_size = 6.1, x_angle = 52) +
  theme(axis.text.x = element_text(size = 5.1))
readr::write_csv(atlas |> mutate(dimension = as.character(dimension), metric_class = as.character(metric_class)),
                 file.path("results", "rq1", "fig1_pairwise_atlas.csv"), na = "")

# c-d. Target-aligned A/B geometry. These panels quantify the consequence of
# moving from an alternative state toward the target-aligned state.
target_geometry_panel <- function(dim, letter) {
  d <- summary |>
    filter(dimension == dim, is.finite(A_mean_absolute), is.finite(B_mean_signed)) |>
    transmute(metric, metric_class, A_mean_absolute, B_mean_signed,
              transition = pair_label, A_boot_q025, A_boot_q975, B_boot_q025, B_boot_q975)
  if (!nrow(d)) stop("No target-aligned A/B rows for Fig. 1 dimension: ", dim)
  d <- d |> mutate(metric_class = factor(metric_class, levels = METRIC_CLASSES))
  lab <- d |> group_by(metric) |> slice_max(A_mean_absolute, n = 1, with_ties = FALSE) |> ungroup() |>
    slice_max(A_mean_absolute, n = 4, with_ties = FALSE)
  max_display <- max(c(d$A_mean_absolute, abs(d$B_mean_signed), d$A_boot_q025, d$A_boot_q975,
                       d$B_boot_q025, d$B_boot_q975), na.rm = TRUE)
  if (!is.finite(max_display) || max_display <= 0) max_display <- 1
  ci <- d |> filter(is.finite(A_boot_q025), is.finite(A_boot_q975), is.finite(B_boot_q025), is.finite(B_boot_q975))
  transition_note <- if (dim == "placement") {
    "shape: circle = Chest to Eye; triangle = Wrist to Eye"
  } else {
    "LIGHT to MEDI"
  }
  ggplot(d, aes(B_mean_signed, A_mean_absolute, color = metric_class)) +
    geom_vline(xintercept = 0, linewidth = .26, color = "#D8D8D8") +
    geom_abline(slope = c(-1, 1), intercept = 0, linetype = 2, linewidth = .32, color = "#8A8A8A") +
    geom_segment(data = ci, aes(x = B_boot_q025, xend = B_boot_q975, y = A_mean_absolute, yend = A_mean_absolute),
                 inherit.aes = FALSE, alpha = .16, linewidth = .25) +
    geom_segment(data = ci, aes(x = B_mean_signed, xend = B_mean_signed, y = A_boot_q025, yend = A_boot_q975),
                 inherit.aes = FALSE, alpha = .16, linewidth = .25) +
    geom_point(aes(shape = transition), size = 1.45, alpha = .86) +
    geom_text(data = lab, aes(x = B_mean_signed, y = A_mean_absolute, label = metric), inherit.aes = FALSE,
              size = 1.85, color = "#252525", check_overlap = TRUE, vjust = -.65) +
    scale_color_ms_metric() +
    scale_x_continuous(trans = scales::transform_asinh(), limits = c(-max_display * 1.06, max_display * 1.06),
                       breaks = scales::breaks_extended(n = 4)) +
    scale_y_continuous(trans = scales::transform_asinh(), limits = c(0, max_display * 1.06),
                       breaks = scales::breaks_extended(n = 4)) +
    labs(title = paste0(letter, "  ", DIM_TITLES[[dim]], " target-aligned change"),
         subtitle = transition_note,
         x = "B: mean signed change", y = "A: mean absolute change") +
    theme_ms(base_size = 6.3, legend_position = "none") +
    theme(
      plot.margin = margin(2, 2, 2, 2),
      plot.subtitle = element_text(size = 5.0, colour = "#555555", margin = margin(t = -1, b = 2))
    )
}

# d-e. Local response along ordered axes. For duration, multiple nested windows
# can represent the same d -> d+1 step; medians here are display summaries of
# the already-computed pair-level G values, not new estimands from raw data.
local_display <- local |>
  mutate(
    dimension = as.character(dimension),
    from_days = if_else(dimension == "duration", suppressWarnings(as.integer(str_extract(lower_level, "^\\d+"))), NA_integer_),
    to_days = if_else(dimension == "duration", suppressWarnings(as.integer(str_extract(higher_level, "^\\d+"))), NA_integer_),
    transition = if_else(
      dimension == "duration",
      paste0(from_days, " d to ", to_days, " d"),
      paste(lower_level, "to", higher_level)
    ),
    step_order = case_when(
      dimension == "duration" ~ as.numeric(from_days),
      dimension == "temporal" ~ match(
        transition,
        c("30 min to 15 min", "15 min to 5 min", "5 min to 1 min", "1 min to 30 s", "30 s to 20 s", "20 s to 10 s")
      ),
      TRUE ~ NA_real_
    )
  ) |>
  filter(dimension %in% c("temporal", "duration"), is.finite(G), is.finite(step_order)) |>
  group_by(dimension, metric, metric_class, transition, step_order) |>
  summarise(G_display = median(G, na.rm = TRUE), .groups = "drop")

local_response_panel <- function(dim, letter) {
  d <- local_display |>
    filter(dimension == dim) |>
    mutate(
      metric_class = factor(metric_class, levels = METRIC_CLASSES),
      transition = forcats::fct_reorder(transition, step_order)
    )
  if (!nrow(d)) stop("No local G rows for Fig. 1 dimension: ", dim)
  ggplot(d, aes(transition, G_display, group = metric, color = metric_class)) +
    geom_line(alpha = .38, linewidth = .35) +
    geom_point(size = .62, alpha = .72) +
    facet_wrap(~metric_class, scales = "free_y", ncol = 1) +
    scale_color_ms_metric() +
    scale_y_continuous(trans = scales::transform_asinh(), breaks = scales::breaks_extended(n = 4)) +
    labs(title = paste0(letter, "  ", DIM_TITLES[[dim]], " local response"),
         x = "adjacent oriented transition", y = "G = mean |z|") +
    theme_ms(base_size = 6.1, legend_position = "none") +
    theme(
      axis.text.x = element_text(angle = 42, hjust = 1, size = 5.0),
      strip.text = element_blank(),
      strip.background = element_blank(),
      strip.switch.pad.grid = grid::unit(0, "mm"),
      panel.spacing.y = grid::unit(.45, "mm"),
      plot.margin = margin(1.5, 2, 1.5, 2)
    )
}

p1b <- target_geometry_panel("placement", "b")
p1c <- target_geometry_panel("optical", "c")
p1d <- local_response_panel("temporal", "d")
p1e <- local_response_panel("duration", "e")

metric_legend_source <- ggplot(
  tibble(metric_class = factor(METRIC_CLASSES, levels = METRIC_CLASSES), x = seq_along(METRIC_CLASSES), y = 1),
  aes(x, y, color = metric_class)
) +
  geom_point(size = 1.8) + scale_color_ms_metric() +
  guides(color = guide_legend(
    title = "metric class", nrow = 1, byrow = TRUE,
    override.aes = list(size = 1.45)
  )) +
  theme_void(base_family = MS_FONT, base_size = 7) +
  theme(
    legend.position = "bottom", legend.direction = "horizontal",
    legend.margin = margin(0, 0, 0, 0), legend.box.margin = margin(0, 0, 0, 0),
    legend.title = element_text(size = 5.6, face = "bold"),
    legend.text = element_text(size = 5.2),
    legend.key.width = grid::unit(3.2, "mm")
  )
metric_legend <- cowplot::get_legend(metric_legend_source)

middle <- cowplot::plot_grid(p1b, p1c, ncol = 2, rel_widths = c(1, 1),
                             align = "hv", axis = "tblr", greedy = TRUE)
bottom <- cowplot::plot_grid(p1d, p1e, ncol = 2, rel_widths = c(1, 1),
                             align = "hv", axis = "tblr", greedy = TRUE)
fig1body <- cowplot::plot_grid(p1a, middle, bottom, ncol = 1,
                               rel_heights = c(.96, 1.10, 1.30),
                               align = "v", axis = "l", greedy = TRUE)
fig1 <- cowplot::plot_grid(metric_legend, fig1body, ncol = 1,
                           rel_heights = c(.035, 1), align = "v", greedy = TRUE)
ms_plot_save(fig1, file.path(OUT_DIR, "Fig1_RQ1.pdf"), 12.4, 9.8)
ms_plot_save(fig1, file.path(OUT_DIR, "Fig1_RQ1.png"), 12.4, 9.8)

# Supplement: complete metric-level atlas and empirical z distributions.
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
    labs(title = paste0(letter, "  ", DIM_TITLES[[dim]]), x = "standardized representation change, z", y = NULL) +
    theme_ms(base_size = 6.0, legend_position = "none") +
    theme(panel.grid.major.y = element_blank(), axis.text.y = element_text(size = 4.8),
          axis.ticks.y = element_blank(), strip.text.y.left = element_text(size = 5.1))
}
distribution_grid <- cowplot::plot_grid(
  plotlist = map2(DIMENSIONS, letters[1:4], distribution_panel), ncol = 4, align = "hv", axis = "tblr"
)
ms_plot_save(distribution_grid, file.path(OUT_DIR, "FigS_RQ1_pairwise_distributions.pdf"), 16, 9.2)
ms_plot_save(distribution_grid, file.path(OUT_DIR, "FigS_RQ1_pairwise_distributions.png"), 16, 9.2)

p_availability <- ggplot(availability_plot, aes(pair_label, metric, fill = representation_available)) +
  geom_tile(color = "white", linewidth = .12) +
  facet_grid(metric_class ~ dimension, scales = "free", space = "free", switch = "y") +
  scale_fill_manual(values = c(`TRUE` = MS_PRIMARY, `FALSE` = "#D9D9D9"),
                    labels = c(`TRUE` = "available", `FALSE` = "unavailable"), name = NULL) +
  labs(title = "RQ1 representation availability by oriented comparison pair", x = NULL, y = NULL) +
  ms_atlas_theme(base_size = 6.1, x_angle = 52)
ms_plot_save(p_availability, file.path(OUT_DIR, "FigS_RQ1_availability_atlas.pdf"), 16, 10)
ms_plot_save(p_availability, file.path(OUT_DIR, "FigS_RQ1_availability_atlas.png"), 16, 10)

ms_plot_write_manifest(
  file.path(OUT_DIR, "figure_artifact_manifest.csv"),
  tibble(
    figure = c("Fig1_RQ1", "FigS_RQ1_pairwise_atlas", "FigS_RQ1_pairwise_distributions", "FigS_RQ1_availability_atlas"),
    input_artifact = c(
      "rq1_pairwise_summary + rq1_metric_availability + rq1_local_transition_summary",
      "rq1_pairwise_summary + rq1_metric_availability",
      "rq1_pairwise_summary",
      "rq1_metric_availability"
    ),
    core_artifact_version = CORE_VERSION, rq1_analysis_version = RQ1_VERSION,
    rq2_analysis_version = NA_character_, rq3_analysis_version = NA_character_
  )
)
message("Fig. 1 complete: aggregated configuration-response structure, target-aligned A/B geometry, and local G response; full metric atlas retained as supplement.")
