suppressPackageStartupMessages({
  library(tidyverse)
  library(cowplot)
})

# RQ3 plotting only. Reads frozen outputs from scripts/14_rq3_analysis.R.
# Fig. 4 is the single-dimension inverse decision map.
# Fig. 5 compresses the full multidimensional epsilon family by plotting each
# configuration's entry tolerance A and marking configurations that lie on a
# Pareto frontier for at least one observed tolerance.

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

required <- c(
  SINGLE_RDS, REQ_CSV, UNORDERED_CSV, COVERAGE_CSV,
  JOINT_SUMMARY_CSV, PARETO_EVER_CSV, PARETO_FREQ_CSV,
  REP_CSV, SCOPE_CSV
)
for (p in required) {
  if (!file.exists(p)) stop("Missing RQ3 artifact: ", p, ". Run scripts/14_rq3_analysis.R first.")
}
dir.create(FIG_DIR, recursive = TRUE, showWarnings = FALSE)

METRIC_CLASSES <- c(
  "duration", "exposure history", "level",
  "spectrum", "temporal dynamics", "timing"
)
TEMPORAL_LEVELS <- c("10 s", "15 s", "20 s", "30 s", "1 min", "5 min", "15 min", "30 min")
DURATION_LEVELS <- paste0(7:1, " d")

base_theme <- theme_minimal(base_size = 8) +
  theme(
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "plain", size = 9, margin = margin(b = 3)),
    plot.margin = margin(4, 5, 4, 5)
  )
asinh_display <- scales::transform_asinh()

single <- readRDS(SINGLE_RDS) |>
  mutate(metric_class = factor(metric_class, levels = METRIC_CLASSES))
req <- readr::read_csv(REQ_CSV, show_col_types = FALSE) |>
  mutate(metric_class = factor(metric_class, levels = METRIC_CLASSES))
unordered <- readr::read_csv(UNORDERED_CSV, show_col_types = FALSE) |>
  mutate(metric_class = factor(metric_class, levels = METRIC_CLASSES))
coverage <- readr::read_csv(COVERAGE_CSV, show_col_types = FALSE)
joint_summary <- readr::read_csv(JOINT_SUMMARY_CSV, show_col_types = FALSE)
pareto_ever <- readr::read_csv(PARETO_EVER_CSV, show_col_types = FALSE)
pareto_freq <- readr::read_csv(PARETO_FREQ_CSV, show_col_types = FALSE)
representatives <- readr::read_csv(REP_CSV, show_col_types = FALSE)
scope <- readr::read_csv(SCOPE_CSV, show_col_types = FALSE)

# -----------------------------------------------------------------------------
# Fig. 4 | Single-dimension measurement sufficiency.
# -----------------------------------------------------------------------------

p4a <- ggplot() +
  annotate("segment", x = .08, xend = .92, y = .66, yend = .66,
           linewidth = .55, color = "grey35") +
  annotate("point", x = c(.18, .38, .58, .78), y = .66,
           size = c(1.6, 1.8, 2.0, 2.2), color = "grey35") +
  annotate("segment", x = .15, xend = .82, y = .28, yend = .28,
           linewidth = .55, color = "grey35") +
  annotate("segment", x = .55, xend = .55, y = .16, yend = .40,
           linetype = 2, linewidth = .45, color = "grey45") +
  annotate("text", x = .50, y = .84, label = "RQ1: configuration  →  distortion", size = 3.1) +
  annotate("text", x = .50, y = .48, label = "threshold by acceptable distortion ε", size = 2.9) +
  annotate("text", x = .50, y = .10, label = "RQ3: ε  →  sufficient measurement requirement", size = 3.1) +
  annotate("text", x = .05, y = .97, label = "a", hjust = 0, vjust = 1, size = 3.2) +
  coord_cartesian(xlim = c(0, 1), ylim = c(0, 1), clip = "off") +
  theme_void(base_size = 8) +
  theme(plot.margin = margin(5, 7, 5, 7), aspect.ratio = 1)

extend_requirement <- function(d) {
  if (!nrow(d)) return(d)
  global_max <- max(d$epsilon, na.rm = TRUE)
  tails <- d |>
    group_by(metric) |>
    slice_max(epsilon, n = 1, with_ties = FALSE) |>
    mutate(epsilon = global_max * 1.025 + 1e-9)
  bind_rows(d, tails) |>
    arrange(metric, epsilon)
}

ordered_requirement_panel <- function(dim, letter, title, y_labels) {
  d <- req |>
    filter(dimension == dim, is.finite(epsilon), is.finite(least_demanding_rank)) |>
    extend_requirement()
  if (!nrow(d)) {
    return(ggplot() + annotate("text", x = 0.5, y = 0.5, label = "Not estimable") + theme_void())
  }
  n_rank <- length(y_labels)

  ggplot(d, aes(epsilon, least_demanding_rank, group = metric, color = metric_class)) +
    geom_step(alpha = .34, linewidth = .42, direction = "hv") +
    geom_point(
      data = d |> filter(!minimum_requirement_interpretable),
      shape = 1, size = .85, stroke = .25, alpha = .45,
      show.legend = FALSE
    ) +
    scale_x_continuous(transform = asinh_display, expand = expansion(mult = c(.01, .03))) +
    scale_y_continuous(
      breaks = seq_len(n_rank), labels = y_labels,
      limits = c(.75, n_rank + .25), expand = c(0, 0)
    ) +
    scale_color_discrete(drop = FALSE) +
    labs(
      title = paste0(letter, "  ", title),
      x = "acceptable mean absolute standardized distortion (ε)",
      y = "least-demanding sufficient observed level",
      color = "metric class"
    ) +
    base_theme +
    theme(
      aspect.ratio = 1,
      legend.position = "none",
      panel.grid.major.y = element_line(linewidth = .25, color = "grey90")
    )
}

p4b <- ordered_requirement_panel(
  "temporal", "b", "Temporal resolution",
  TEMPORAL_LEVELS
)
p4c <- ordered_requirement_panel(
  "duration", "c", "Monitoring duration",
  DURATION_LEVELS
)

# Optical has one alternative state, so its entire sufficiency function is the
# empirical CDF of metric-specific entry tolerances. Thin class curves retain
# representation structure; the black curve is the descriptive all-metric summary.
optical_threshold <- unordered |> filter(dimension == "optical", is.finite(epsilon_entry))
opt_eps <- sort(unique(c(0, optical_threshold$epsilon_entry)))
opt_class <- tidyr::crossing(
  epsilon = opt_eps,
  metric_class = factor(METRIC_CLASSES, levels = METRIC_CLASSES)
) |>
  rowwise() |>
  mutate(
    fraction = {
      z <- optical_threshold |> filter(metric_class == .env$metric_class)
      if (nrow(z)) mean(z$epsilon_entry <= epsilon + 1e-12) else NA_real_
    }
  ) |>
  ungroup()
opt_all <- tibble(epsilon = opt_eps) |>
  mutate(fraction = map_dbl(epsilon, ~mean(optical_threshold$epsilon_entry <= .x + 1e-12)))

p4d <- ggplot(opt_class, aes(epsilon, fraction, color = metric_class)) +
  geom_step(linewidth = .45, alpha = .62, na.rm = TRUE) +
  geom_step(data = opt_all, aes(epsilon, fraction), inherit.aes = FALSE,
            linewidth = .8, color = "black") +
  scale_x_continuous(transform = asinh_display, expand = expansion(mult = c(.01, .03))) +
  scale_y_continuous(limits = c(0, 1), labels = scales::percent_format(accuracy = 1)) +
  scale_color_discrete(drop = FALSE) +
  labs(
    title = "d  Optical proxy",
    x = "acceptable distortion (ε)",
    y = "target representations sufficient",
    color = "metric class"
  ) +
  base_theme +
  theme(aspect.ratio = 1, legend.position = "none")

placement_threshold <- unordered |> filter(dimension == "placement", is.finite(epsilon_entry))
place_eps <- sort(unique(c(0, placement_threshold$epsilon_entry)))
place_curve <- tidyr::crossing(
  epsilon = place_eps,
  configuration_label = sort(unique(placement_threshold$configuration_label))
) |>
  rowwise() |>
  mutate(
    fraction = {
      z <- placement_threshold |> filter(configuration_label == .env$configuration_label)
      if (nrow(z)) mean(z$epsilon_entry <= epsilon + 1e-12) else NA_real_
    }
  ) |>
  ungroup()

p4e <- ggplot(place_curve, aes(epsilon, fraction, linetype = configuration_label)) +
  geom_step(linewidth = .78) +
  scale_x_continuous(transform = asinh_display, expand = expansion(mult = c(.01, .03))) +
  scale_y_continuous(limits = c(0, 1), labels = scales::percent_format(accuracy = 1)) +
  scale_linetype_discrete(name = NULL) +
  labs(
    title = "e  Placement",
    x = "acceptable distortion (ε)",
    y = "target representations sufficient"
  ) +
  base_theme +
  theme(aspect.ratio = 1, legend.position = c(.73, .20), legend.background = element_blank())

legend_source <- ggplot(
  tibble(
    metric_class = factor(METRIC_CLASSES, levels = METRIC_CLASSES),
    x = seq_along(METRIC_CLASSES), y = 1
  ), aes(x, y, color = metric_class)
) +
  geom_line(linewidth = .8) +
  scale_color_discrete(drop = FALSE) +
  guides(color = guide_legend(title = "metric class", nrow = 1, byrow = TRUE)) +
  theme_void(base_size = 8) +
  theme(legend.position = "bottom")
metric_legend <- cowplot::get_legend(legend_source)

fig4_body <- plot_grid(
  p4a, p4b, p4c,
  p4d, p4e, NULL,
  ncol = 3,
  align = "hv", axis = "tblr",
  rel_widths = c(1, 1, 1), rel_heights = c(1, 1)
)
fig4 <- plot_grid(fig4_body, metric_legend, ncol = 1, rel_heights = c(1, .08))

ggsave(file.path(FIG_DIR, "Fig4_RQ3.pdf"), fig4,
       width = 12.2, height = 8.2, device = cairo_pdf)
ggsave(file.path(FIG_DIR, "Fig4_RQ3.png"), fig4,
       width = 12.2, height = 8.2, dpi = 240)

# -----------------------------------------------------------------------------
# Fig. 5 | Multidimensional sufficient regions and Pareto frontiers.
# -----------------------------------------------------------------------------

joint_estimable <- scope |>
  filter(object == "multidimensional_joint") |>
  pull(estimable)
joint_estimable <- length(joint_estimable) && isTRUE(joint_estimable[[1]]) && nrow(joint_summary) > 0

if (!joint_estimable) {
  p5 <- ggplot() +
    annotate(
      "text", x = .5, y = .56,
      label = "Multidimensional frontier not estimable\nunder the frozen strict joint-support rule",
      size = 4.0, lineheight = 1.1
    ) +
    annotate(
      "text", x = .5, y = .36,
      label = "No exact seven-consecutive-day cohort was available on eye_chest_wrist_full.",
      size = 2.8, color = "grey35"
    ) +
    xlim(0, 1) + ylim(0, 1) + theme_void()
  ggsave(file.path(FIG_DIR, "Fig5_RQ3.pdf"), p5,
         width = 10, height = 6, device = cairo_pdf)
  ggsave(file.path(FIG_DIR, "Fig5_RQ3.png"), p5,
         width = 10, height = 6, dpi = 240)
} else {
  temporal_order <- tibble(
    resolution_s = c(10L, 15L, 20L, 30L, 60L, 300L, 900L, 1800L),
    temporal_label = TEMPORAL_LEVELS,
    temporal_rank = seq_along(TEMPORAL_LEVELS)
  )

  pe <- pareto_ever |>
    left_join(temporal_order, by = c("resolution_s", "temporal_label")) |>
    mutate(
      temporal_label = factor(temporal_label, levels = TEMPORAL_LEVELS),
      placement = factor(placement, levels = c("eye", "chest", "wrist")),
      optical = factor(optical, levels = c("MEDI", "LIGHT"))
    )

  requested_metrics <- Sys.getenv("RQ3_FIG5_METRICS", unset = "")
  if (nzchar(requested_metrics)) {
    rep_metrics <- trimws(strsplit(requested_metrics, ",", fixed = TRUE)[[1]])
    rep_metrics <- rep_metrics[rep_metrics %in% pe$metric]
  } else {
    rep_metrics <- representatives$metric
  }
  rep_metrics <- unique(rep_metrics)[seq_len(min(4L, length(unique(rep_metrics))))]
  if (!length(rep_metrics)) stop("No representative metrics available for Fig. 5")

  # Panel a is a conceptual dominance map only; empirical panels b-e use the
  # actual joint configuration results below.
  schem <- tibble(
    x = c(1, 2, 3, 4, 2, 3, 4),
    y = c(4, 4, 4, 4, 3, 3, 2),
    sufficient = c(FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE),
    pareto = c(FALSE, FALSE, FALSE, TRUE, FALSE, TRUE, TRUE)
  )
  p5a <- ggplot(schem, aes(x, y)) +
    geom_point(aes(alpha = sufficient), size = 2.5, color = "grey35") +
    geom_step(
      data = schem |> filter(pareto) |> arrange(x),
      aes(x, y), direction = "vh", linewidth = .75, color = "black"
    ) +
    geom_point(data = schem |> filter(pareto), shape = 21, fill = "white",
               size = 3.0, stroke = .8) +
    annotate("text", x = 2.55, y = 1.25, label = "less demanding →", size = 2.7) +
    annotate("text", x = .72, y = 3.0, label = "more days", angle = 90, size = 2.7) +
    scale_alpha_manual(values = c(`FALSE` = .18, `TRUE` = .72), guide = "none") +
    labs(title = "a  Sufficient region and Pareto boundary", x = "temporal requirement", y = "duration requirement") +
    base_theme + theme(aspect.ratio = 1, legend.position = "none")

  selected_pe <- pe |> filter(metric %in% rep_metrics)
  fill_lim <- range(selected_pe$A_mean_absolute, na.rm = TRUE)
  if (!all(is.finite(fill_lim)) || diff(fill_lim) <= 0) fill_lim <- c(0, 1)

  rep_panel <- function(metric_name, letter) {
    d <- selected_pe |> filter(metric == metric_name)
    ggplot(d, aes(temporal_label, n_days)) +
      geom_point(
        aes(shape = optical, fill = A_mean_absolute),
        position = position_dodge(width = .42),
        size = 1.75, color = "grey35", stroke = .25
      ) +
      geom_point(
        data = d |> filter(ever_pareto),
        aes(shape = optical, fill = A_mean_absolute),
        position = position_dodge(width = .42),
        size = 2.7, color = "black", stroke = .85,
        show.legend = FALSE
      ) +
      facet_wrap(~placement, nrow = 1) +
      scale_shape_manual(values = c(MEDI = 21, LIGHT = 24), drop = FALSE) +
      scale_fill_viridis_c(
        option = "C", transform = "asinh",
        limits = fill_lim,
        name = "entry tolerance\nA = εentry"
      ) +
      scale_y_continuous(breaks = 1:7, limits = c(.6, 7.4)) +
      labs(
        title = paste0(letter, "  ", metric_name),
        x = "temporal resolution",
        y = "monitoring duration (d)",
        shape = "optical state"
      ) +
      base_theme +
      theme(
        aspect.ratio = .82,
        axis.text.x = element_text(angle = 55, hjust = 1, size = 6.2),
        strip.text = element_text(size = 6.7),
        legend.position = "none"
      )
  }

  letters_rep <- letters[2:(length(rep_metrics) + 1L)]
  rep_plots <- map2(rep_metrics, letters_rep, rep_panel)

  freq <- pareto_freq |>
    mutate(
      temporal_label = factor(temporal_label, levels = TEMPORAL_LEVELS),
      placement = factor(placement, levels = c("eye", "chest", "wrist")),
      optical = factor(optical, levels = c("MEDI", "LIGHT"))
    )
  p5f <- ggplot(freq, aes(temporal_label, n_days, fill = fraction_metrics_ever_pareto)) +
    geom_tile(color = "white", linewidth = .15) +
    facet_grid(optical ~ placement) +
    scale_fill_viridis_c(
      option = "C", limits = c(0, 1),
      labels = scales::percent_format(accuracy = 1),
      name = "metrics ever\non frontier"
    ) +
    scale_y_continuous(breaks = 1:7, limits = c(.5, 7.5), expand = c(0, 0)) +
    labs(
      title = "f  Frontier recurrence across target representations",
      x = "temporal resolution", y = "monitoring duration (d)"
    ) +
    base_theme +
    theme(
      aspect.ratio = 1,
      axis.text.x = element_text(angle = 55, hjust = 1, size = 6),
      strip.text = element_text(size = 6.5),
      legend.position = "right"
    )

  # Fill any missing representative slots with blanks so the 2x3 grammar stays stable.
  while (length(rep_plots) < 4L) rep_plots[[length(rep_plots) + 1L]] <- NULL
  fig5 <- plot_grid(
    p5a, rep_plots[[1]], rep_plots[[2]],
    rep_plots[[3]], rep_plots[[4]], p5f,
    ncol = 3,
    align = "hv", axis = "tblr",
    rel_widths = c(1, 1.15, 1.15),
    rel_heights = c(1, 1)
  )

  ggsave(file.path(FIG_DIR, "Fig5_RQ3.pdf"), fig5,
         width = 14.5, height = 9.1, device = cairo_pdf)
  ggsave(file.path(FIG_DIR, "Fig5_RQ3.png"), fig5,
         width = 14.5, height = 9.1, dpi = 240)
}

message("RQ3 figures complete:")
message("  ", file.path(FIG_DIR, "Fig4_RQ3.pdf"))
message("  ", file.path(FIG_DIR, "Fig4_RQ3.png"))
message("  ", file.path(FIG_DIR, "Fig5_RQ3.pdf"))
message("  ", file.path(FIG_DIR, "Fig5_RQ3.png"))
