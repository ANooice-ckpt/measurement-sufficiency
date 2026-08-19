suppressPackageStartupMessages({
  library(tidyverse)
  library(cowplot)
})

# RQ2 plotting only. Reads frozen outputs from scripts/12_rq2_analysis.R and does
# not refit mixed models, recompute condition bins, bootstrap gamma, or rebuild
# joint configuration cells.

COND_RDS <- "data/derived/rq2/rq2_condition_long.rds"
GAMMA_RDS <- "data/derived/rq2/rq2_gamma_long.rds"
COND_GEOM_CSV <- "results/rq2/rq2_conditional_geometry.csv"
ANCHOR_CSV <- "results/rq2/rq2_anchor_configurations.csv"
COND_EXAMPLE_CSV <- "results/rq2/rq2_conditional_examples.csv"
MODEL_PERF_CSV <- "results/rq2/rq2_model_performance.csv"
GAMMA_SUMMARY_CSV <- "results/rq2/rq2_gamma_summary.csv"
GAMMA_PAIR_CSV <- "results/rq2/rq2_gamma_pair_summary.csv"
GAMMA_EXAMPLE_CSV <- "results/rq2/rq2_gamma_examples.csv"
STRONG_EXAMPLE_CSV <- "results/rq2/rq2_strong_coupling_example.csv"
FIG_DIR <- "results/figures"

required <- c(
  COND_RDS, GAMMA_RDS, COND_GEOM_CSV, ANCHOR_CSV, COND_EXAMPLE_CSV,
  MODEL_PERF_CSV, GAMMA_SUMMARY_CSV, GAMMA_PAIR_CSV,
  GAMMA_EXAMPLE_CSV, STRONG_EXAMPLE_CSV
)
for (p in required) {
  if (!file.exists(p)) stop("Missing RQ2 artifact: ", p, ". Run scripts/12_rq2_analysis.R first.")
}
dir.create(FIG_DIR, recursive = TRUE, showWarnings = FALSE)

METRIC_CLASSES <- c(
  "duration", "exposure history", "level",
  "spectrum", "temporal dynamics", "timing"
)
DIMENSIONS <- c("placement", "optical", "temporal", "duration")
DIM_TITLES <- c(
  placement = "Placement",
  optical = "Optical proxy",
  temporal = "Temporal resolution",
  duration = "Monitoring duration"
)
PAIR_LEVELS <- c(
  "placement × optical", "placement × temporal", "optical × temporal"
)
STATE_LEVELS <- c("Low", "Middle", "High")
MODEL_LEVELS <- c("external_context", "exposure_state", "joint")
MODEL_LABELS <- c(
  external_context = "External",
  exposure_state = "Exposure state",
  joint = "Joint"
)
VALIDATION_LABELS <- c(
  participant_grouped = "Grouped participant CV",
  leave_site_out = "Leave-site-out"
)

base_square_theme <- theme_minimal(base_size = 8) +
  theme(
    aspect.ratio = 1,
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "plain", size = 9, margin = margin(b = 3)),
    plot.margin = margin(4, 5, 4, 5),
    legend.position = "none"
  )
asinh_display <- scales::transform_asinh()

cond <- readRDS(COND_RDS) |>
  mutate(
    metric_class = factor(metric_class, levels = METRIC_CLASSES),
    state_bin_label = factor(as.character(state_bin_label), levels = STATE_LEVELS)
  )
gamma <- readRDS(GAMMA_RDS) |>
  mutate(metric_class = factor(metric_class, levels = METRIC_CLASSES))
cond_geom <- readr::read_csv(COND_GEOM_CSV, show_col_types = FALSE) |>
  mutate(
    metric_class = factor(metric_class, levels = METRIC_CLASSES),
    state_bin_label = factor(state_bin_label, levels = STATE_LEVELS)
  )
anchors <- readr::read_csv(ANCHOR_CSV, show_col_types = FALSE)
cond_examples <- readr::read_csv(COND_EXAMPLE_CSV, show_col_types = FALSE)
model_perf <- readr::read_csv(MODEL_PERF_CSV, show_col_types = FALSE) |>
  mutate(
    metric_class = factor(metric_class, levels = METRIC_CLASSES),
    model_family = factor(model_family, levels = MODEL_LEVELS),
    model_label = factor(MODEL_LABELS[as.character(model_family)], levels = unname(MODEL_LABELS)),
    validation_label = factor(VALIDATION_LABELS[validation_scheme], levels = unname(VALIDATION_LABELS))
  )
gamma_summary <- readr::read_csv(GAMMA_SUMMARY_CSV, show_col_types = FALSE) |>
  mutate(
    metric_class = factor(metric_class, levels = METRIC_CLASSES),
    dimension_pair = factor(dimension_pair, levels = PAIR_LEVELS)
  )
gamma_pair <- readr::read_csv(GAMMA_PAIR_CSV, show_col_types = FALSE) |>
  mutate(dimension_pair = factor(dimension_pair, levels = PAIR_LEVELS))
gamma_examples <- readr::read_csv(GAMMA_EXAMPLE_CSV, show_col_types = FALSE)
strong_example <- readr::read_csv(STRONG_EXAMPLE_CSV, show_col_types = FALSE)

get_legend <- function(p) {
  g <- ggplotGrob(p)
  idx <- which(vapply(g$grobs, function(x) x$name, character(1)) == "guide-box")
  if (!length(idx)) return(grid::nullGrob())
  g$grobs[[idx[1]]]
}

metric_legend_source <- ggplot(
  tibble(
    metric_class = factor(METRIC_CLASSES, levels = METRIC_CLASSES),
    x = seq_along(METRIC_CLASSES), y = 1
  ),
  aes(x, y, color = metric_class)
) +
  geom_point(size = 2) +
  scale_color_discrete(drop = FALSE) +
  guides(color = guide_legend(title = "metric class", nrow = 1, byrow = TRUE)) +
  theme_void(base_size = 8) + theme(legend.position = "bottom")
metric_legend <- get_legend(metric_legend_source)

# -----------------------------------------------------------------------------
# Fig. 2: conditional structure and external predictability
# -----------------------------------------------------------------------------

# a: one algorithmically selected state-dependent example per measurement dimension.
example_ids <- cond_examples |>
  select(dimension, configuration, metric) |>
  mutate(example_id = paste(dimension, configuration, metric, sep = "|"))

cond_example_data <- cond |>
  mutate(example_id = paste(dimension, configuration, metric, sep = "|")) |>
  semi_join(example_ids, by = "example_id") |>
  filter(available, is.finite(e), !is.na(state_bin_label)) |>
  left_join(
    cond_examples |>
      transmute(
        example_id = paste(dimension, configuration, metric, sep = "|"),
        example_label = paste0(DIM_TITLES[dimension], ": ", metric)
      ),
    by = "example_id"
  )

p2a <- ggplot(cond_example_data, aes(e, color = state_bin_label, fill = state_bin_label)) +
  geom_density(alpha = .14, linewidth = .45, adjust = .9) +
  geom_vline(xintercept = 0, linewidth = .28, color = "grey45") +
  facet_wrap(~example_label, scales = "free", ncol = 2) +
  labs(
    title = "a  Conditional distortion distributions",
    x = "standardized signed distortion (e)", y = "density",
    color = "reference state", fill = "reference state"
  ) +
  theme_minimal(base_size = 7.5) +
  theme(
    aspect.ratio = 1,
    panel.grid.minor = element_blank(),
    strip.text = element_text(size = 6.6),
    plot.title = element_text(size = 9),
    legend.position = "bottom",
    legend.title = element_text(size = 7),
    legend.text = element_text(size = 7),
    plot.margin = margin(4, 5, 4, 5)
  )

conditional_ab_panel <- function(dim, letter) {
  anchor_d <- anchors |>
    filter(dimension == dim)
  d <- cond_geom |>
    filter(dimension == dim) |>
    semi_join(anchor_d, by = c("dimension", "configuration")) |>
    filter(is.finite(A_conditional), is.finite(B_conditional)) |>
    arrange(metric, configuration, state_bin)
  if (!nrow(d)) {
    return(
      ggplot() +
        annotate("text", x = 0, y = 0, label = "No estimable conditional geometry", size = 3) +
        xlim(-1, 1) + ylim(-1, 1) +
        labs(title = paste0(letter, "  ", DIM_TITLES[[dim]])) +
        base_square_theme
    )
  }

  high <- d |> filter(state_bin == max(state_bin, na.rm = TRUE))
  label_metrics <- high |>
    slice_max(A_conditional, n = 3L, with_ties = FALSE) |>
    pull(metric)
  label_d <- high |> filter(metric %in% label_metrics)

  lim <- max(c(d$A_conditional, abs(d$B_conditional)), na.rm = TRUE)
  lim <- ifelse(is.finite(lim) && lim > 0, lim * 1.08, 1)

  ggplot(d, aes(B_conditional, A_conditional, color = metric_class)) +
    geom_abline(slope = c(-1, 1), intercept = 0, linetype = 2, linewidth = .30, color = "grey65") +
    geom_path(
      aes(group = interaction(metric, configuration)),
      color = "grey42", alpha = .55, linewidth = .48,
      lineend = "round", linejoin = "round"
    ) +
    geom_point(aes(shape = state_bin_label), size = 1.15, alpha = .78) +
    geom_point(
      data = high,
      aes(B_conditional, A_conditional, color = metric_class),
      size = 1.75, alpha = .95, show.legend = FALSE
    ) +
    geom_text(
      data = label_d,
      aes(label = metric),
      size = 1.9, vjust = -0.65, check_overlap = TRUE,
      show.legend = FALSE
    ) +
    scale_color_discrete(drop = FALSE) +
    scale_shape_manual(values = c(Low = 1, Middle = 16, High = 17)) +
    scale_x_continuous(transform = asinh_display, limits = c(-lim, lim), expand = expansion(mult = .025)) +
    scale_y_continuous(transform = asinh_display, limits = c(0, lim), expand = expansion(mult = .025)) +
    labs(
      title = paste0(letter, "  ", DIM_TITLES[[dim]]),
      x = "conditional B", y = "conditional A"
    ) +
    base_square_theme
}

p2b <- conditional_ab_panel("placement", "b")
p2c <- conditional_ab_panel("optical", "c")
p2d <- conditional_ab_panel("temporal", "d")
p2e <- conditional_ab_panel("duration", "e")

# f: out-of-sample predictability. R2 remains metric/configuration specific; the
# boxplots summarize those independent target-representation fits descriptively.
if (nrow(model_perf)) {
  p2f <- ggplot(
    model_perf |> filter(is.finite(r2)),
    aes(model_label, r2, fill = validation_label)
  ) +
    geom_hline(yintercept = 0, linewidth = .3, color = "grey55") +
    geom_boxplot(
      position = position_dodge(width = .72),
      width = .62, outlier.shape = NA, alpha = .34, linewidth = .35
    ) +
    geom_point(
      aes(color = validation_label),
      position = position_jitterdodge(jitter.width = .09, dodge.width = .72),
      size = .65, alpha = .28, show.legend = FALSE
    ) +
    facet_wrap(~outcome, ncol = 1, scales = "free_y") +
    labs(
      title = "f  External predictability",
      x = NULL, y = "out-of-sample R²", fill = NULL
    ) +
    theme_minimal(base_size = 7.5) +
    theme(
      aspect.ratio = 1,
      panel.grid.minor = element_blank(),
      strip.text = element_text(size = 7),
      axis.text.x = element_text(angle = 22, hjust = 1),
      plot.title = element_text(size = 9),
      legend.position = "bottom",
      legend.text = element_text(size = 6.7),
      plot.margin = margin(4, 5, 4, 5)
    )
} else {
  p2f <- ggplot() +
    annotate("text", x = 0, y = 0, label = "Prediction models not run / not estimable", size = 3) +
    xlim(-1, 1) + ylim(-1, 1) +
    labs(title = "f  External predictability") +
    base_square_theme
}

fig2_top <- plot_grid(p2a, p2b, p2c, nrow = 1, align = "hv", axis = "tblr")
fig2_bottom <- plot_grid(p2d, p2e, p2f, nrow = 1, align = "hv", axis = "tblr")
fig2_body <- plot_grid(fig2_top, fig2_bottom, ncol = 1, align = "v", rel_heights = c(1, 1))
fig2 <- plot_grid(fig2_body, metric_legend, ncol = 1, rel_heights = c(1, .07))

ggsave(file.path(FIG_DIR, "Fig2_RQ2.pdf"), fig2, width = 12.2, height = 8.4, useDingbats = FALSE)
ggsave(file.path(FIG_DIR, "Fig2_RQ2.png"), fig2, width = 12.2, height = 8.4, dpi = 220)

# -----------------------------------------------------------------------------
# Fig. 3: cross-dimensional separability
# -----------------------------------------------------------------------------

schematic_cells <- tribble(
  ~x, ~y, ~label,
  0, 0, "reference\nM00",
  1, 0, "a changed\nMa0",
  0, 1, "b changed\nM0b",
  1, 1, "a + b\nMab"
)
p3a <- ggplot(schematic_cells, aes(x, y)) +
  geom_tile(width = .62, height = .42, fill = "grey94", color = "grey40", linewidth = .4) +
  geom_text(aes(label = label), size = 2.5) +
  annotate("segment", x = .30, xend = .70, y = 0, yend = 0, arrow = grid::arrow(length = grid::unit(1.3, "mm")), linewidth = .35) +
  annotate("segment", x = .30, xend = .70, y = 1, yend = 1, arrow = grid::arrow(length = grid::unit(1.3, "mm")), linewidth = .35) +
  annotate("text", x = .5, y = .53, label = "γ = Δa|b − Δa|0", size = 3) +
  coord_equal(xlim = c(-.45, 1.45), ylim = c(-.45, 1.45), clip = "off") +
  labs(title = "a  Second-order interaction distortion") +
  theme_void(base_size = 8) +
  theme(
    aspect.ratio = 1,
    plot.title = element_text(size = 9),
    plot.margin = margin(4, 5, 4, 5)
  )

# b: four algorithmic D(gamma) archetypes.
gamma_example_ids <- gamma_examples |>
  transmute(
    id = paste(dimension_pair, a_configuration, b_configuration, metric, sep = " | "),
    example_type
  )
gamma_example_data <- gamma |>
  filter(available, is.finite(gamma)) |>
  mutate(id = paste(dimension_pair, a_configuration, b_configuration, metric, sep = " | ")) |>
  inner_join(gamma_example_ids, by = "id")

p3b <- ggplot(gamma_example_data, aes(gamma, fill = example_type)) +
  geom_density(alpha = .38, color = NA, adjust = .85) +
  geom_vline(xintercept = 0, linewidth = .28, color = "grey40") +
  facet_wrap(~example_type, scales = "free", ncol = 2) +
  guides(fill = "none") +
  labs(title = "b  Empirical D(γ)", x = "standardized interaction distortion (γ)", y = "density") +
  theme_minimal(base_size = 7.2) +
  theme(
    aspect.ratio = 1,
    panel.grid.minor = element_blank(),
    strip.text = element_text(size = 6.3),
    plot.title = element_text(size = 9),
    plot.margin = margin(4, 5, 4, 5)
  )

# c: R-Q geometry. Dynamic range is displayed with identical asinh transforms.
if (nrow(gamma_summary)) {
  lim3 <- max(c(gamma_summary$Q_mean_absolute, abs(gamma_summary$R_mean_signed)), na.rm = TRUE)
  lim3 <- ifelse(is.finite(lim3) && lim3 > 0, lim3 * 1.06, 1)
  label3 <- gamma_summary |>
    slice_max(Q_mean_absolute, n = 5L, with_ties = FALSE)
  p3c <- ggplot(gamma_summary, aes(R_mean_signed, Q_mean_absolute, color = metric_class)) +
    geom_abline(slope = c(-1, 1), intercept = 0, linetype = 2, linewidth = .3, color = "grey63") +
    geom_point(aes(shape = dimension_pair), size = 1.05, alpha = .67) +
    geom_text(
      data = label3, aes(label = metric),
      size = 1.8, vjust = -0.6, check_overlap = TRUE, show.legend = FALSE
    ) +
    scale_color_discrete(drop = FALSE) +
    scale_x_continuous(transform = asinh_display, limits = c(-lim3, lim3), expand = expansion(mult = .025)) +
    scale_y_continuous(transform = asinh_display, limits = c(0, lim3), expand = expansion(mult = .025)) +
    labs(title = "c  Interaction geometry", x = "R: mean signed γ", y = "Q: mean absolute γ") +
    base_square_theme
} else {
  p3c <- ggplot() + annotate("text", x = 0, y = 0, label = "No gamma summaries", size = 3) +
    xlim(-1, 1) + ylim(-1, 1) + labs(title = "c  Interaction geometry") + base_square_theme
}

# d: dimension-pair summary; metrics remain descriptive units, not replicate inference.
p3d <- ggplot(gamma_pair, aes(dimension_pair, median_Q)) +
  geom_errorbar(aes(ymin = q25_Q, ymax = q75_Q), width = .12, linewidth = .45, color = "grey45") +
  geom_point(size = 2.2) +
  coord_flip() +
  labs(title = "d  Overall cross-dimensional dependence", x = NULL, y = "median Q across metric-configurations") +
  base_square_theme +
  theme(axis.text.y = element_text(size = 7))

# e: strongest observed coupling, showing how the marginal effect of dimension a
# changes after dimension b is moved away from reference.
if (nrow(strong_example)) {
  strong_id <- strong_example |>
    transmute(id = paste(dimension_pair, a_configuration, b_configuration, metric, sep = " | ")) |>
    pull(id)
  strong_d <- gamma |>
    filter(available, is.finite(gamma)) |>
    mutate(id = paste(dimension_pair, a_configuration, b_configuration, metric, sep = " | ")) |>
    filter(id == strong_id) |>
    select(marginal_a_ref, marginal_a_at_b) |>
    pivot_longer(everything(), names_to = "b_state", values_to = "marginal_effect") |>
    mutate(
      b_state = recode(
        b_state,
        marginal_a_ref = "b at reference",
        marginal_a_at_b = "b changed"
      )
    )
  strong_stats <- strong_d |>
    group_by(b_state) |>
    summarise(
      mean = mean(marginal_effect, na.rm = TRUE),
      q25 = quantile(marginal_effect, .25, na.rm = TRUE),
      q75 = quantile(marginal_effect, .75, na.rm = TRUE),
      .groups = "drop"
    )
  strong_title <- paste0(
    "e  Strong coupling: ", strong_example$metric[1], "\n",
    strong_example$a_configuration_label[1], " × ", strong_example$b_configuration_label[1]
  )
  p3e <- ggplot(strong_stats, aes(b_state, mean, group = 1)) +
    geom_hline(yintercept = 0, linewidth = .3, color = "grey60") +
    geom_line(linewidth = .55, color = "grey40") +
    geom_errorbar(aes(ymin = q25, ymax = q75), width = .10, linewidth = .42) +
    geom_point(size = 2.0) +
    labs(title = strong_title, x = NULL, y = "marginal effect of dimension a") +
    base_square_theme +
    theme(axis.text.x = element_text(angle = 15, hjust = 1))
} else {
  p3e <- ggplot() + annotate("text", x = 0, y = 0, label = "No strong-coupling example", size = 3) +
    xlim(-1, 1) + ylim(-1, 1) + labs(title = "e  Strong coupling") + base_square_theme
}

blank <- ggplot() + theme_void()
fig3_top <- plot_grid(p3a, p3b, p3c, nrow = 1, align = "hv", axis = "tblr")
fig3_bottom <- plot_grid(p3d, p3e, blank, nrow = 1, align = "hv", axis = "tblr")
fig3_body <- plot_grid(fig3_top, fig3_bottom, ncol = 1, rel_heights = c(1, 1))
fig3 <- plot_grid(fig3_body, metric_legend, ncol = 1, rel_heights = c(1, .07))

ggsave(file.path(FIG_DIR, "Fig3_RQ2.pdf"), fig3, width = 12.2, height = 8.4, useDingbats = FALSE)
ggsave(file.path(FIG_DIR, "Fig3_RQ2.png"), fig3, width = 12.2, height = 8.4, dpi = 220)

message("RQ2 figures complete:")
message("  ", file.path(FIG_DIR, "Fig2_RQ2.pdf"))
message("  ", file.path(FIG_DIR, "Fig2_RQ2.png"))
message("  ", file.path(FIG_DIR, "Fig3_RQ2.pdf"))
message("  ", file.path(FIG_DIR, "Fig3_RQ2.png"))
