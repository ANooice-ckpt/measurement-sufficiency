suppressPackageStartupMessages({
  library(tidyverse)
  library(cowplot)
})
source("scripts/utils/figure_style.R")

# RQ3 plotting only: Fig. 4 inverse sufficiency + Fig. 5 multidimensional entry tolerance/Pareto.
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

reqfiles <- c(SINGLE_RDS, REQ_CSV, UNORDERED_CSV, COVERAGE_CSV, JOINT_SUMMARY_CSV, PARETO_EVER_CSV, PARETO_FREQ_CSV, REP_CSV, SCOPE_CSV)
for (p in reqfiles) if (!file.exists(p)) stop("Missing RQ3 artifact: ", p, ". Run scripts/14_rq3_analysis.R first.")
dir.create(FIG_DIR, recursive = TRUE, showWarnings = FALSE)

METRIC_CLASSES <- MS_METRIC_CLASSES
TEMPORAL_LEVELS <- c("10 s", "20 s", "30 s", "1 min", "5 min", "15 min", "30 min")
DURATION_LEVELS <- paste0(7:1, " d")
base_theme <- theme_ms()
base_square_theme <- theme_ms(aspect_ratio = 1, legend_position = "none")
asinh_display <- scales::transform_asinh()
tlabel <- function(x) case_when(x < 60 ~ paste0(x, " s"), x %% 60 == 0 ~ paste0(x %/% 60, " min"), TRUE ~ paste0(x, " s"))

single <- readRDS(SINGLE_RDS) |> mutate(metric_class = factor(metric_class, levels = METRIC_CLASSES))
req <- readr::read_csv(REQ_CSV, show_col_types = FALSE) |> mutate(metric_class = factor(metric_class, levels = METRIC_CLASSES))
unordered <- readr::read_csv(UNORDERED_CSV, show_col_types = FALSE) |> mutate(metric_class = factor(metric_class, levels = METRIC_CLASSES))
coverage <- readr::read_csv(COVERAGE_CSV, show_col_types = FALSE)
js <- readr::read_csv(JOINT_SUMMARY_CSV, show_col_types = FALSE)
pe <- readr::read_csv(PARETO_EVER_CSV, show_col_types = FALSE)
pf <- readr::read_csv(PARETO_FREQ_CSV, show_col_types = FALSE)
reps <- readr::read_csv(REP_CSV, show_col_types = FALSE)
scope <- readr::read_csv(SCOPE_CSV, show_col_types = FALSE)

# Fig. 4a: inverse-problem schematic.
p4a <- ggplot() +
  annotate("segment", x = .08, xend = .92, y = .66, yend = .66, linewidth = .62, color = MS_PRIMARY) +
  annotate("point", x = c(.18, .38, .58, .78), y = .66, size = c(1.6, 1.8, 2, 2.2), color = MS_PRIMARY) +
  annotate("segment", x = .15, xend = .82, y = .28, yend = .28, linewidth = .62, color = MS_PRIMARY) +
  annotate("segment", x = .55, xend = .55, y = .16, yend = .4, linetype = 2, linewidth = .45, color = MS_SECONDARY) +
  annotate("text", x = .5, y = .84, label = "RQ1: configuration → distortion", size = 3.1) +
  annotate("text", x = .5, y = .48, label = "acceptable distortion ε", size = 2.9, color = MS_SECONDARY) +
  annotate("text", x = .5, y = .1, label = "RQ3: ε → sufficient measurement requirement", size = 3.1) +
  annotate("text", x = .05, y = .97, label = "a", hjust = 0, vjust = 1, size = 3.2) +
  coord_cartesian(xlim = c(0, 1), ylim = c(0, 1), clip = "off") +
  theme_ms_blank(aspect_ratio = 1)

extend_req <- function(d) {
  if (!nrow(d)) return(d)
  mx <- max(d$epsilon, na.rm = TRUE)
  bind_rows(
    d,
    d |> group_by(metric) |> slice_max(epsilon, n = 1, with_ties = FALSE) |> mutate(epsilon = mx * 1.025 + 1e-9)
  ) |> arrange(metric, epsilon)
}

ordered_panel <- function(dim, letter, title, ylab) {
  d <- req |>
    filter(dimension == dim, is.finite(epsilon), is.finite(least_demanding_rank)) |>
    extend_req()
  if (!nrow(d)) return(ggplot() + annotate("text", x = .5, y = .5, label = "Not estimable") + theme_ms_blank(aspect_ratio = 1))

  ggplot(d, aes(epsilon, least_demanding_rank, group = metric, color = metric_class)) +
    geom_step(alpha = .38, linewidth = .44, direction = "hv") +
    geom_point(
      data = d |> filter(!minimum_requirement_interpretable),
      shape = 1, size = .85, stroke = .25, alpha = .45, show.legend = FALSE
    ) +
    scale_x_continuous(transform = asinh_display, expand = expansion(mult = c(.01, .03))) +
    scale_y_continuous(breaks = seq_along(ylab), labels = ylab, limits = c(.75, length(ylab) + .25), expand = c(0, 0)) +
    scale_color_ms_metric() +
    labs(
      title = paste0(letter, "  ", title),
      x = "acceptable mean absolute standardized distortion, ε",
      y = "least-demanding sufficient observed level"
    ) +
    theme_ms(aspect_ratio = 1, legend_position = "none") +
    theme(panel.grid.major.y = element_line(colour = "#E8E8E8", linewidth = .24, linetype = "22"))
}

p4b <- ordered_panel("temporal", "b", "Temporal resolution", TEMPORAL_LEVELS)
p4c <- ordered_panel("duration", "c", "Monitoring duration", DURATION_LEVELS)

oc <- coverage |>
  filter(dimension == "optical", metric_class != "All") |>
  mutate(metric_class = factor(metric_class, levels = METRIC_CLASSES))
oa <- coverage |> filter(dimension == "optical", metric_class == "All")
p4d <- ggplot(oc, aes(epsilon, fraction_metrics_sufficient, color = metric_class)) +
  geom_step(linewidth = .46, alpha = .65, na.rm = TRUE) +
  geom_step(data = oa, aes(epsilon, fraction_metrics_sufficient), inherit.aes = FALSE, linewidth = .85, color = MS_PRIMARY) +
  scale_x_continuous(transform = asinh_display, expand = expansion(mult = c(.01, .03))) +
  scale_y_continuous(limits = c(0, 1), labels = scales::percent_format(accuracy = 1)) +
  scale_color_ms_metric() +
  labs(title = "d  Optical proxy", x = "acceptable distortion, ε", y = "observable target representations sufficient") +
  theme_ms(aspect_ratio = 1, legend_position = "none")

pc <- coverage |> filter(dimension == "placement", metric_class == "All")
placement_levels <- unique(pc$configuration_label)
placement_colors <- setNames(rep(MS_TWO_COLORS, length.out = length(placement_levels)), placement_levels)
p4e <- ggplot(pc, aes(epsilon, fraction_metrics_sufficient, color = configuration_label, linetype = configuration_label)) +
  geom_step(linewidth = .82) +
  scale_color_manual(values = placement_colors) +
  scale_x_continuous(transform = asinh_display, expand = expansion(mult = c(.01, .03))) +
  scale_y_continuous(limits = c(0, 1), labels = scales::percent_format(accuracy = 1)) +
  labs(title = "e  Placement", x = "acceptable distortion, ε", y = "observable target representations sufficient", color = NULL, linetype = NULL) +
  theme_ms(aspect_ratio = 1, legend_position = "bottom") +
  theme(legend.text = element_text(size = 6.5))

metric_legend <- cowplot::get_legend(
  ggplot(
    tibble(metric_class = factor(METRIC_CLASSES, levels = METRIC_CLASSES), x = seq_along(METRIC_CLASSES), y = 1),
    aes(x, y, color = metric_class)
  ) +
    geom_point(size = 1.8) +
    scale_color_ms_metric() +
    guides(color = guide_legend(title = "metric class", nrow = 1, byrow = TRUE)) +
    theme_void(base_family = MS_FONT, base_size = 8) + theme(legend.position = "bottom")
)

fig4body <- plot_grid(p4a, p4b, p4c, p4d, p4e, NULL, ncol = 3, align = "hv", axis = "tblr")
fig4 <- plot_grid(fig4body, metric_legend, ncol = 1, rel_heights = c(1, .08))
ggsave(file.path(FIG_DIR, "Fig4_RQ3.pdf"), fig4, width = 12.2, height = 8.2, useDingbats = FALSE)
ggsave(file.path(FIG_DIR, "Fig4_RQ3.png"), fig4, width = 12.2, height = 8.2, dpi = 240)

# Fig. 5: epsilon_entry=A(c) compresses all tolerances; Pareto within fixed placement/optical facets.
est <- scope |> filter(object == "multidimensional_joint") |> pull(estimable)
estimable <- length(est) && isTRUE(est[[1]]) && nrow(js) > 0

if (!estimable) {
  p5 <- ggplot() +
    annotate("text", x = .5, y = .56, label = "Multidimensional frontier not estimable\non facet-specific protocol-anchored supports", size = 4, lineheight = 1.1) +
    annotate("text", x = .5, y = .36, label = "No supported seven-day joint configuration lattice was available.", size = 2.8, color = "#555555") +
    xlim(0, 1) + ylim(0, 1) + theme_ms_blank()
  ggsave(file.path(FIG_DIR, "Fig5_RQ3.pdf"), p5, width = 10, height = 6, useDingbats = FALSE)
  ggsave(file.path(FIG_DIR, "Fig5_RQ3.png"), p5, width = 10, height = 6, dpi = 240)
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
      optical = factor(optical, levels = c("MEDI", "LIGHT"))
    )

  requested <- Sys.getenv("RQ3_FIG5_METRICS", unset = "")
  rmets <- if (nzchar(requested)) trimws(strsplit(requested, ",", fixed = TRUE)[[1]]) else reps$metric
  rmets <- unique(rmets[rmets %in% pe$metric])
  rmets <- rmets[seq_len(min(4L, length(rmets)))]
  if (!length(rmets)) stop("No representative metrics for Fig. 5")

  schem <- tibble(
    x = c(1, 2, 3, 4, 2, 3, 4), y = c(4, 4, 4, 4, 3, 3, 2),
    sufficient = c(FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE),
    pareto = c(FALSE, FALSE, FALSE, TRUE, FALSE, TRUE, TRUE)
  )
  p5a <- ggplot(schem, aes(x, y)) +
    geom_point(aes(alpha = sufficient), size = 2.5, color = MS_PRIMARY) +
    geom_step(data = schem |> filter(pareto) |> arrange(x), aes(x, y), direction = "vh", linewidth = .78, color = MS_PRIMARY) +
    geom_point(data = schem |> filter(pareto), shape = 21, fill = "white", color = MS_PRIMARY, size = 3, stroke = .8) +
    annotate("text", x = 2.55, y = 1.25, label = "less demanding →", size = 2.7) +
    annotate("text", x = .72, y = 3, label = "more days", angle = 90, size = 2.7) +
    scale_alpha_manual(values = c(`FALSE` = .18, `TRUE` = .72), guide = "none") +
    labs(title = "a  Sufficient region and Pareto boundary", x = "temporal requirement", y = "duration requirement") +
    base_square_theme

  pdata <- pe |> filter(metric %in% rmets)
  make_metric_panel <- function(m, letter) {
    d <- pdata |> filter(metric == m)
    ggplot(d, aes(temporal_label, factor(n_days, levels = 1:7))) +
      geom_point(aes(fill = epsilon_entry, shape = optical), size = 2.25, alpha = .82, color = "#2A2A2A", stroke = .28) +
      geom_point(data = d |> filter(ever_pareto), aes(shape = optical), fill = NA, color = "black", size = 3.05, stroke = .85) +
      facet_grid(placement ~ optical, drop = FALSE) +
      scale_fill_ms_sequential(name = "ε entry") +
      scale_shape_manual(values = c(MEDI = 21, LIGHT = 24), guide = "none") +
      labs(title = paste0(letter, "  ", m), x = "temporal resolution", y = "monitoring duration (days)") +
      theme_ms(legend_position = "bottom") +
      theme(axis.text.x = element_text(angle = 35, hjust = 1, size = 6), strip.text = element_text(size = 6), legend.text = element_text(size = 6.5))
  }

  panel_letters <- c("b", "c", "d", "e")
  panels <- map2(rmets, panel_letters[seq_along(rmets)], make_metric_panel)
  while (length(panels) < 4) panels[[length(panels) + 1]] <- ggplot() + theme_void(base_family = MS_FONT)

  pf <- pf |> mutate(optical = factor(optical, levels = c("MEDI", "LIGHT")))
  optical_colors <- c(MEDI = MS_PRIMARY, LIGHT = MS_SECONDARY)
  p5f <- ggplot(
    pf,
    aes(
      factor(tlabel(resolution_s), levels = TEMPORAL_LEVELS), factor(n_days, levels = 1:7),
      size = fraction_metrics_ever_pareto, shape = optical, color = optical
    )
  ) +
    geom_point(alpha = .68) +
    facet_wrap(~placement, nrow = 1) +
    scale_color_manual(values = optical_colors) +
    scale_shape_manual(values = c(MEDI = 16, LIGHT = 17)) +
    scale_size_continuous(range = c(.4, 3.2), labels = scales::percent_format(accuracy = 1)) +
    labs(
      title = "f  Frontier recurrence across target representations",
      x = "temporal resolution", y = "monitoring duration (days)",
      size = "metrics ever Pareto", shape = "optical", color = "optical"
    ) +
    theme_ms(legend_position = "bottom") +
    theme(axis.text.x = element_text(angle = 35, hjust = 1, size = 6), legend.text = element_text(size = 6.3))

  fig5top <- plot_grid(p5a, panels[[1]], panels[[2]], nrow = 1, align = "hv", axis = "tblr")
  fig5bot <- plot_grid(panels[[3]], panels[[4]], p5f, nrow = 1, align = "hv", axis = "tblr")
  fig5 <- plot_grid(fig5top, fig5bot, ncol = 1, rel_heights = c(1, 1))
  ggsave(file.path(FIG_DIR, "Fig5_RQ3.pdf"), fig5, width = 13, height = 8.6, useDingbats = FALSE)
  ggsave(file.path(FIG_DIR, "Fig5_RQ3.png"), fig5, width = 13, height = 8.6, dpi = 240)
}

message("RQ3 figures complete with shared publication style: Fig4_RQ3 + Fig5_RQ3")
