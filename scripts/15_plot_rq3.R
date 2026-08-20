suppressPackageStartupMessages({
  library(tidyverse)
  library(cowplot)
})
source("scripts/utils/figure_style.R")
source("scripts/utils/figure_atlas.R")

# RQ3 plotting only: inverse single-dimension sufficiency + multidimensional
# entry tolerance/Pareto. The main plots retain the 54 target representations
# rather than collapsing them prematurely to class-level coverage curves.
RQ1_SUMMARY_CSV <- "results/rq1/rq1_summary.csv"
SINGLE_RDS <- "data/derived/rq3/rq3_single_dimension_sufficiency.rds"
REQ_CSV <- "results/rq3/rq3_single_dimension_requirement.csv"
UNORDERED_CSV <- "results/rq3/rq3_unordered_sufficiency_thresholds.csv"
COVERAGE_CSV <- "results/rq3/rq3_unordered_coverage_curves.csv"
JOINT_SUMMARY_CSV <- "results/rq3/rq3_joint_summary.csv"
PARETO_EVER_CSV <- "results/rq3/rq3_pareto_ever.csv"
PARETO_FREQ_CSV <- "results/rq3/rq3_pareto_frequency.csv"
REP_CSV <- "results/rq3/rq3_fig5_representative_metrics.csv"
SCOPE_CSV <- "results/rq3/rq3_scope.csv"
FIG_DIR <- "results/figures"

reqfiles <- c(
  RQ1_SUMMARY_CSV, SINGLE_RDS, REQ_CSV, UNORDERED_CSV, COVERAGE_CSV,
  JOINT_SUMMARY_CSV, PARETO_EVER_CSV, PARETO_FREQ_CSV, REP_CSV, SCOPE_CSV
)
for (p in reqfiles) if (!file.exists(p)) stop("Missing RQ3 artifact: ", p, ". Run scripts/14_rq3_analysis.R first.")
dir.create(FIG_DIR, recursive = TRUE, showWarnings = FALSE)

METRIC_CLASSES <- MS_METRIC_CLASSES
TEMPORAL_LEVELS <- c("10 s", "20 s", "30 s", "1 min", "5 min", "15 min", "30 min")
DURATION_LEVELS <- paste0(7:1, " d")
base_square_theme <- theme_ms(aspect_ratio = 1, legend_position = "none")
asinh_display <- scales::transform_asinh()
tlabel <- function(x) case_when(x < 60 ~ paste0(x, " s"), x %% 60 == 0 ~ paste0(x %/% 60, " min"), TRUE ~ paste0(x, " s"))

rq1_summary <- readr::read_csv(RQ1_SUMMARY_CSV, show_col_types = FALSE)
metric_order <- ms_metric_order(rq1_summary)
single <- readRDS(SINGLE_RDS) |> mutate(metric_class = factor(metric_class, levels = METRIC_CLASSES))
req <- readr::read_csv(REQ_CSV, show_col_types = FALSE) |> mutate(metric_class = factor(metric_class, levels = METRIC_CLASSES))
unordered <- readr::read_csv(UNORDERED_CSV, show_col_types = FALSE) |> mutate(metric_class = factor(metric_class, levels = METRIC_CLASSES))
coverage <- readr::read_csv(COVERAGE_CSV, show_col_types = FALSE)
js <- readr::read_csv(JOINT_SUMMARY_CSV, show_col_types = FALSE)
pe <- readr::read_csv(PARETO_EVER_CSV, show_col_types = FALSE)
pf <- readr::read_csv(PARETO_FREQ_CSV, show_col_types = FALSE)
reps <- readr::read_csv(REP_CSV, show_col_types = FALSE)
scope <- readr::read_csv(SCOPE_CSV, show_col_types = FALSE)

extend_req <- function(d) {
  if (!nrow(d)) return(d)
  mx <- max(d$epsilon, na.rm = TRUE)
  bind_rows(
    d,
    d |>
      group_by(metric) |>
      slice_max(epsilon, n = 1, with_ties = FALSE) |>
      mutate(epsilon = mx * 1.035 + 1e-9)
  ) |>
    arrange(metric, epsilon)
}

all_eps <- c(req$epsilon, unordered$epsilon_entry)
all_eps <- all_eps[is.finite(all_eps) & all_eps >= 0]
EPS_MAX <- if (length(all_eps)) max(all_eps) * 1.04 + 1e-9 else 1

# =============================================================================
# Fig. 4 | Inverse single-dimension sufficiency maps
# =============================================================================

# Ordered dimensions are rendered as metric-level requirement strips over epsilon.
# Rank 1 is the high-information reference (10 s or 7 d); larger ranks are
# progressively less demanding observed levels. Hollow marks flag epsilon states
# whose empirical sufficient set is not threshold-like.
ordered_atlas <- function(dim, letter, title, level_labels) {
  d <- req |>
    filter(dimension == dim, is.finite(epsilon)) |>
    extend_req() |>
    group_by(metric) |>
    arrange(epsilon, .by_group = TRUE) |>
    mutate(epsilon_next = lead(epsilon)) |>
    ungroup() |>
    filter(is.finite(least_demanding_rank)) |>
    ms_add_metric_order(metric_order)

  if (!nrow(d)) {
    return(
      ggplot() + annotate("text", x = .5, y = .5, label = "Not estimable") +
        xlim(0, 1) + ylim(0, 1) + labs(title = paste0(letter, "  ", title)) + theme_ms_blank(aspect_ratio = 1)
    )
  }

  pal <- setNames(
    grDevices::colorRampPalette(c(MS_PRIMARY, "#DDEAF4"))(length(level_labels)),
    seq_along(level_labels)
  )
  bad <- d |> filter(!replace_na(minimum_requirement_interpretable, FALSE), is.finite(epsilon))

  ggplot(d |> filter(is.finite(epsilon_next)), aes(y = metric)) +
    geom_segment(
      aes(
        x = epsilon, xend = epsilon_next,
        yend = metric, color = factor(least_demanding_rank)
      ),
      linewidth = 3.2, lineend = "butt"
    ) +
    geom_point(
      data = bad, aes(x = epsilon, y = metric), inherit.aes = FALSE,
      shape = 1, size = .72, stroke = .28, color = "#303030"
    ) +
    facet_grid(metric_class ~ ., scales = "free_y", space = "free_y", switch = "y") +
    scale_color_manual(values = pal, breaks = seq_along(level_labels), labels = level_labels, name = "least-demanding\nsufficient level") +
    scale_x_continuous(
      transform = asinh_display, limits = c(0, EPS_MAX),
      breaks = scales::breaks_extended(n = 5), expand = expansion(mult = c(0, .01))
    ) +
    labs(
      title = paste0(letter, "  ", title),
      x = "acceptable mean absolute standardized distortion, ε", y = NULL
    ) +
    ms_atlas_theme(base_size = 6.2, x_angle = 0) +
    theme(axis.text.x = element_text(angle = 0, hjust = .5), legend.text = element_text(size = 5.8)) +
    guides(color = guide_legend(nrow = 1, byrow = TRUE, title.position = "top"))
}

p4a <- ordered_atlas("temporal", "a", "Temporal resolution", TEMPORAL_LEVELS)
p4b <- ordered_atlas("duration", "b", "Monitoring duration", DURATION_LEVELS)

# Unordered dimensions: x-position is epsilon_entry=A(c); bubble fill retains
# signed-direction structure through B/A. Placement is shown as paired points.
unordered_plot <- unordered |>
  filter(is.finite(epsilon_entry), is.finite(B_at_entry)) |>
  mutate(direction_ratio = ms_direction_ratio(B_at_entry, epsilon_entry)) |>
  ms_add_metric_order(metric_order)

optical_d <- unordered_plot |> filter(dimension == "optical")
p4c <- ggplot(optical_d, aes(y = metric)) +
  geom_segment(aes(x = 0, xend = epsilon_entry, yend = metric), linewidth = .28, color = "#C8C8C8") +
  geom_point(aes(x = epsilon_entry, fill = direction_ratio), shape = 21, size = 1.75, color = "#343434", stroke = .20) +
  facet_grid(metric_class ~ ., scales = "free_y", space = "free_y", switch = "y") +
  scale_x_continuous(transform = asinh_display, limits = c(0, EPS_MAX), breaks = scales::breaks_extended(n = 5), expand = expansion(mult = c(0, .01))) +
  ms_direction_scale(name = "B / A") +
  labs(title = "c  Optical proxy", x = "entry tolerance, εentry = A(c)", y = NULL) +
  ms_atlas_theme(base_size = 6.2, x_angle = 0) +
  theme(axis.text.x = element_text(angle = 0, hjust = .5)) +
  guides(fill = "none")

placement_d <- unordered_plot |> filter(dimension == "placement")
placement_range <- placement_d |>
  group_by(metric) |>
  summarise(xmin = min(epsilon_entry), xmax = max(epsilon_entry), .groups = "drop") |>
  ms_add_metric_order(metric_order)
p4d <- ggplot(placement_d, aes(y = metric)) +
  geom_segment(
    data = placement_range,
    aes(x = xmin, xend = xmax, y = metric, yend = metric), inherit.aes = FALSE,
    linewidth = .35, color = "#BDBDBD"
  ) +
  geom_point(
    aes(x = epsilon_entry, fill = direction_ratio, shape = configuration_label),
    size = 1.85, color = "#343434", stroke = .22
  ) +
  facet_grid(metric_class ~ ., scales = "free_y", space = "free_y", switch = "y") +
  scale_x_continuous(transform = asinh_display, limits = c(0, EPS_MAX), breaks = scales::breaks_extended(n = 5), expand = expansion(mult = c(0, .01))) +
  ms_direction_scale(name = "B / A") +
  scale_shape_manual(values = c(Chest = 21, Wrist = 24), name = "placement") +
  labs(title = "d  Placement", x = "entry tolerance, εentry = A(c)", y = NULL) +
  ms_atlas_theme(base_size = 6.2, x_angle = 0) +
  theme(axis.text.x = element_text(angle = 0, hjust = .5), legend.text = element_text(size = 5.8)) +
  guides(fill = guide_colorbar(order = 1, title.position = "top"), shape = guide_legend(order = 2, title.position = "top"))

fig4 <- plot_grid(p4a, p4b, p4c, p4d, ncol = 2, align = "hv", axis = "tblr")
ggsave(file.path(FIG_DIR, "Fig4_RQ3.pdf"), fig4, width = 13.2, height = 12.4, useDingbats = FALSE, bg = "white")
ggsave(file.path(FIG_DIR, "Fig4_RQ3.png"), fig4, width = 13.2, height = 12.4, dpi = 240, bg = "white")

# Preserve the original class/all-metric coverage projection as a supplementary
# summary. It is useful, but it is downstream of the metric-level decision map.
oc <- coverage |>
  filter(dimension == "optical", metric_class != "All") |>
  mutate(metric_class = factor(metric_class, levels = METRIC_CLASSES))
oa <- coverage |> filter(dimension == "optical", metric_class == "All")
ps4a <- ggplot(oc, aes(epsilon, fraction_metrics_sufficient, color = metric_class)) +
  geom_step(linewidth = .46, alpha = .65, na.rm = TRUE) +
  geom_step(data = oa, aes(epsilon, fraction_metrics_sufficient), inherit.aes = FALSE, linewidth = .85, color = "#202020") +
  scale_x_continuous(transform = asinh_display) +
  scale_y_continuous(limits = c(0, 1), labels = scales::percent_format(accuracy = 1)) +
  scale_color_ms_metric() +
  labs(title = "a  Optical coverage summary", x = "acceptable distortion, ε", y = "fraction sufficient") +
  theme_ms(aspect_ratio = .7, legend_position = "bottom")

pc <- coverage |> filter(dimension == "placement", metric_class == "All")
placement_levels <- unique(pc$configuration_label)
placement_colors <- setNames(rep(MS_TWO_COLORS, length.out = length(placement_levels)), placement_levels)
ps4b <- ggplot(pc, aes(epsilon, fraction_metrics_sufficient, color = configuration_label, linetype = configuration_label)) +
  geom_step(linewidth = .82) +
  scale_color_manual(values = placement_colors) +
  scale_x_continuous(transform = asinh_display) +
  scale_y_continuous(limits = c(0, 1), labels = scales::percent_format(accuracy = 1)) +
  labs(title = "b  Placement coverage summary", x = "acceptable distortion, ε", y = "fraction sufficient", color = NULL, linetype = NULL) +
  theme_ms(aspect_ratio = .7, legend_position = "bottom")

figs4 <- plot_grid(ps4a, ps4b, nrow = 1, align = "hv", axis = "tblr")
ggsave(file.path(FIG_DIR, "FigS_RQ3_single_dimension_coverage.pdf"), figs4, width = 10.5, height = 4.7, bg = "white")
ggsave(file.path(FIG_DIR, "FigS_RQ3_single_dimension_coverage.png"), figs4, width = 10.5, height = 4.7, dpi = 240, bg = "white")

# =============================================================================
# Fig. 5 | Multidimensional entry-tolerance landscapes and Pareto efficiency
# =============================================================================
est <- scope |> filter(object == "multidimensional_joint") |> pull(estimable)
estimable <- length(est) && isTRUE(est[[1]]) && nrow(js) > 0

if (!estimable) {
  p5 <- ggplot() +
    annotate("text", x = .5, y = .56, label = "Multidimensional frontier not estimable\non facet-specific protocol-anchored supports", size = 4, lineheight = 1.1) +
    annotate("text", x = .5, y = .36, label = "No supported seven-day joint configuration lattice was available.", size = 2.8, color = "#555555") +
    xlim(0, 1) + ylim(0, 1) + theme_ms_blank()
  ggsave(file.path(FIG_DIR, "Fig5_RQ3.pdf"), p5, width = 10, height = 6, useDingbats = FALSE, bg = "white")
  ggsave(file.path(FIG_DIR, "Fig5_RQ3.png"), p5, width = 10, height = 6, dpi = 240, bg = "white")
} else {
  to <- tibble(
    resolution_s = c(10L, 20L, 30L, 60L, 300L, 900L, 1800L),
    temporal_label = TEMPORAL_LEVELS,
    temporal_rank = seq_along(TEMPORAL_LEVELS)
  )
  pe <- pe |>
    left_join(to, by = c("resolution_s", "temporal_label")) |>
    mutate(
      temporal_label = factor(temporal_label, levels = TEMPORAL_LEVELS),
      placement = factor(placement, levels = c("eye", "chest", "wrist")),
      optical = factor(optical, levels = c("MEDI", "LIGHT")),
      metric_class = factor(metric_class, levels = METRIC_CLASSES)
    )

  # a. Empirical global recurrence map: size = recurrence across target metrics;
  # fill = median tolerance at which the joint configuration enters sufficiency.
  global_cfg <- pe |>
    group_by(placement, optical, resolution_s, temporal_label, n_days) |>
    summarise(
      n_metrics_available = n_distinct(metric),
      fraction_metrics_ever_pareto = mean(ever_pareto),
      median_epsilon_entry = median(epsilon_entry, na.rm = TRUE),
      .groups = "drop"
    )

  p5a <- ggplot(global_cfg, aes(temporal_label, factor(n_days, levels = 1:7))) +
    geom_point(
      aes(size = fraction_metrics_ever_pareto, fill = median_epsilon_entry),
      shape = 21, color = "#303030", stroke = .24, alpha = .90
    ) +
    facet_grid(placement ~ optical, drop = FALSE) +
    scale_fill_ms_sequential(name = "median ε entry", transform = asinh_display) +
    scale_size_continuous(range = c(.35, 3.3), limits = c(0, 1), labels = scales::percent_format(accuracy = 1), name = "metrics ever Pareto") +
    labs(title = "a  Pareto recurrence and entry tolerance across all target representations", x = "temporal resolution", y = "monitoring duration (days)") +
    theme_ms(legend_position = "bottom") +
    theme(axis.text.x = element_text(angle = 35, hjust = 1, size = 5.8), strip.text = element_text(size = 6.0), legend.text = element_text(size = 6.0))

  requested <- Sys.getenv("RQ3_FIG5_METRICS", unset = "")
  rmets <- if (nzchar(requested)) trimws(strsplit(requested, ",", fixed = TRUE)[[1]]) else reps$metric
  rmets <- unique(rmets[rmets %in% pe$metric])
  rmets <- rmets[seq_len(min(4L, length(rmets)))]
  if (!length(rmets)) stop("No representative metrics for Fig. 5")

  pdata <- pe |> filter(metric %in% rmets)
  make_metric_panel <- function(m, letter) {
    d <- pdata |> filter(metric == m)
    ggplot(d, aes(temporal_label, factor(n_days, levels = 1:7))) +
      geom_point(aes(fill = epsilon_entry, shape = optical), size = 2.25, alpha = .82, color = "#2A2A2A", stroke = .28) +
      geom_point(data = d |> filter(ever_pareto), aes(shape = optical), fill = NA, color = "black", size = 3.05, stroke = .85) +
      facet_grid(placement ~ optical, drop = FALSE) +
      scale_fill_ms_sequential(name = "ε entry", transform = asinh_display) +
      scale_shape_manual(values = c(MEDI = 21, LIGHT = 24), guide = "none") +
      labs(title = paste0(letter, "  ", m), x = "temporal resolution", y = "monitoring duration (days)") +
      theme_ms(legend_position = "bottom") +
      theme(axis.text.x = element_text(angle = 35, hjust = 1, size = 5.7), strip.text = element_text(size = 5.7), legend.text = element_text(size = 6.0))
  }

  panel_letters <- c("b", "c", "d", "e")
  panels <- map2(rmets, panel_letters[seq_along(rmets)], make_metric_panel)
  while (length(panels) < 4) panels[[length(panels) + 1]] <- ggplot() + theme_void(base_family = MS_FONT)

  # f. Class-specific protocol recurrence. Select the globally most recurrent
  # observed joint configurations, then show how often each is Pareto-efficient
  # within each representation class. This separates global recurrence from
  # class heterogeneity without adding another configuration axis to b-e.
  top_cfg <- global_cfg |>
    arrange(desc(fraction_metrics_ever_pareto), median_epsilon_entry, desc(resolution_s), n_days) |>
    slice_head(n = min(12L, n())) |>
    mutate(
      config_id = paste(placement, optical, resolution_s, n_days, sep = "|"),
      config_label = paste0(
        stringr::str_to_title(as.character(placement)), "/", as.character(optical), "\n",
        as.character(temporal_label), " · ", n_days, " d"
      ),
      config_order = row_number()
    )

  class_rec <- pe |>
    group_by(metric_class, placement, optical, resolution_s, temporal_label, n_days) |>
    summarise(
      n_metrics_available = n_distinct(metric),
      fraction_metrics_ever_pareto = mean(ever_pareto),
      .groups = "drop"
    ) |>
    mutate(config_id = paste(placement, optical, resolution_s, n_days, sep = "|")) |>
    inner_join(top_cfg |> select(config_id, config_label, config_order), by = "config_id") |>
    mutate(
      config_label = factor(config_label, levels = top_cfg$config_label),
      metric_class = factor(metric_class, levels = METRIC_CLASSES)
    )

  p5f <- ggplot(class_rec, aes(config_label, metric_class, fill = fraction_metrics_ever_pareto)) +
    geom_tile(color = "white", linewidth = .35) +
    geom_text(aes(label = scales::percent(fraction_metrics_ever_pareto, accuracy = 1)), size = 1.72, color = "#202020") +
    scale_fill_gradientn(colours = MS_SEQUENTIAL, limits = c(0, 1), labels = scales::percent_format(accuracy = 1), name = "class metrics\never Pareto") +
    labs(title = "f  Class-specific recurrence of Pareto-efficient protocols", x = NULL, y = NULL) +
    theme_ms(base_size = 6.5, legend_position = "bottom") +
    theme(
      panel.grid = element_blank(),
      axis.text.x = element_text(angle = 42, hjust = 1, size = 5.5),
      axis.text.y = element_text(size = 6.0),
      legend.text = element_text(size = 5.8)
    )

  fig5top <- plot_grid(p5a, panels[[1]], panels[[2]], nrow = 1, align = "hv", axis = "tblr", rel_widths = c(1.15, 1, 1))
  fig5bot <- plot_grid(panels[[3]], panels[[4]], p5f, nrow = 1, align = "hv", axis = "tblr", rel_widths = c(1, 1, 1.25))
  fig5 <- plot_grid(fig5top, fig5bot, ncol = 1, rel_heights = c(1, 1))
  ggsave(file.path(FIG_DIR, "Fig5_RQ3.pdf"), fig5, width = 14.2, height = 9.8, useDingbats = FALSE, bg = "white")
  ggsave(file.path(FIG_DIR, "Fig5_RQ3.png"), fig5, width = 14.2, height = 9.8, dpi = 240, bg = "white")
}

message("RQ3 figures complete: metric-level inverse sufficiency atlas + empirical Pareto recurrence and class heterogeneity.")
