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
# Complete pairwise z distributions remain available as a supplementary figure.
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
    pair_label = paste(config_a_label, "→", config_b_label),
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

# a. Scientific orientation is prespecified by the measurement task. Placement
# and optical comparisons use target alignment; temporal and duration use
# measurement refinement / accumulation.
orientation <- tribble(
  ~orientation_family, ~dimension, ~path,
  "Target alignment", "Placement", "Chest / Wrist  →  Eye",
  "Target alignment", "Optical representation", "LIGHT  →  MEDI",
  "Measurement requirement", "Temporal resolution", "30 min  →  15 min  →  5 min  →  1 min  →  30 s  →  20 s  →  10 s",
  "Measurement requirement", "Monitoring duration", "1 d  →  2 d  →  3 d  →  4 d  →  5 d  →  6 d"
) |>
  mutate(
    orientation_family = factor(orientation_family, levels = c("Target alignment", "Measurement requirement")),
    dimension = factor(dimension, levels = rev(c("Placement", "Optical representation", "Temporal resolution", "Monitoring duration")))
  )
p1a <- ggplot(orientation, aes(x = 0, y = dimension, label = path)) +
  geom_text(hjust = 0, size = 2.35) +
  facet_wrap(~orientation_family, ncol = 1, scales = "free_y") +
  xlim(0, 1) +
  labs(title = "a  Scientific orientation of configuration change", x = NULL, y = NULL) +
  theme_ms(base_size = 6.5, legend_position = "none") +
  theme(
    panel.grid = element_blank(), axis.text.x = element_blank(), axis.ticks = element_blank(),
    strip.text = element_text(face = "bold", hjust = 0), panel.border = element_blank()
  )

# b. Complete oriented pairwise map. Bubble area is A and fill is B/A;
# unavailable cells remain visible as crosses rather than being treated as zero.
atlas <- summary_plot |> filter(is.finite(A_mean_absolute), is.finite(B_mean_signed))
atlas_bg <- availability_plot
p1b <- ggplot(atlas, aes(pair_label, metric)) +
  geom_tile(data = atlas_bg |> filter(representation_available), fill = "#F3F3F3",
            color = "white", linewidth = .10) +
  geom_point(data = atlas_bg |> filter(!representation_available), shape = 4,
             size = .52, stroke = .24, color = "#B5B5B5") +
  geom_point(aes(size = A_mean_absolute, fill = direction_ratio), shape = 21,
             color = "#3B3B3B", stroke = .14, alpha = .94) +
  facet_grid(metric_class ~ dimension, scales = "free", space = "free", switch = "y") +
  ms_direction_scale(name = "B / A") +
  ms_magnitude_size_scale(name = "A = mean |z|", range = c(.25, 3.0)) +
  labs(title = "b  Oriented configuration-response atlas", x = "scientifically oriented comparison pair", y = NULL) +
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
         x = "B: mean signed change", y = "A: mean absolute change") +
    theme_ms(base_size = 6.3, legend_position = "bottom")
}

# e-f. Local response along ordered axes. For duration, multiple nested windows
# can represent the same d -> d+1 step; medians here are display summaries of
# the already-computed pair-level G values, not new estimands from raw data.
local_display <- local |>
  mutate(
    dimension = as.character(dimension),
    from_days = if_else(dimension == "duration", suppressWarnings(as.integer(str_extract(lower_level, "^\\d+"))), NA_integer_),
    to_days = if_else(dimension == "duration", suppressWarnings(as.integer(str_extract(higher_level, "^\\d+"))), NA_integer_),
    transition = if_else(
      dimension == "duration",
      paste0(from_days, " d → ", to_days, " d"),
      paste(lower_level, "→", higher_level)
    ),
    step_order = case_when(
      dimension == "duration" ~ as.numeric(from_days),
      dimension == "temporal" ~ match(
        transition,
        c("30 min → 15 min", "15 min → 5 min", "5 min → 1 min", "1 min → 30 s", "30 s → 20 s", "20 s → 10 s")
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
    theme(axis.text.x = element_text(angle = 42, hjust = 1, size = 5.0))
}

p1c <- target_geometry_panel("placement", "c")
p1d <- target_geometry_panel("optical", "d")
p1e <- local_response_panel("temporal", "e")
p1f <- local_response_panel("duration", "f")

metric_legend_source <- ggplot(
  tibble(metric_class = factor(METRIC_CLASSES, levels = METRIC_CLASSES), x = seq_along(METRIC_CLASSES), y = 1),
  aes(x, y, color = metric_class)
) +
  geom_point(size = 1.8) + scale_color_ms_metric() +
  guides(color = guide_legend(title = "metric class", nrow = 1, byrow = TRUE)) +
  theme_void(base_family = MS_FONT, base_size = 8) + theme(legend.position = "bottom")
metric_legend <- cowplot::get_legend(metric_legend_source)

middle <- cowplot::plot_grid(p1c, p1d, ncol = 2, align = "hv", axis = "tblr")
bottom <- cowplot::plot_grid(p1e, p1f, ncol = 2, align = "hv", axis = "tblr")
fig1body <- cowplot::plot_grid(p1a, p1b, middle, bottom, ncol = 1, rel_heights = c(.75, 2.25, 1.1, 1.15))
fig1 <- cowplot::plot_grid(fig1body, metric_legend, ncol = 1, rel_heights = c(1, .04))
ms_plot_save(fig1, file.path(OUT_DIR, "Fig1_RQ1.pdf"), 15.8, 13.2)
ms_plot_save(fig1, file.path(OUT_DIR, "Fig1_RQ1.png"), 15.8, 13.2)

# Supplement: complete empirical z distributions for every oriented pair.
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
    figure = c("Fig1_RQ1", "FigS_RQ1_pairwise_distributions", "FigS_RQ1_availability_atlas"),
    input_artifact = c(
      "rq1_pairwise_summary + rq1_metric_availability + rq1_local_transition_summary",
      "rq1_pairwise_summary",
      "rq1_metric_availability"
    ),
    core_artifact_version = CORE_VERSION, rq1_analysis_version = RQ1_VERSION,
    rq2_analysis_version = NA_character_, rq3_analysis_version = NA_character_
  )
)
message("Fig. 1 complete: scientific orientation, configuration-response atlas, target-aligned A/B geometry, and local G response.")
