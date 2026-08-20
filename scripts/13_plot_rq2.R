suppressPackageStartupMessages({
  library(tidyverse)
  library(cowplot)
})
source("scripts/utils/figure_style.R")

# Plot-only RQ2 entry point. Reads frozen outputs from scripts/12_rq2_analysis.R.
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
SCOPE_CSV <- "results/rq2/rq2_interaction_scope.csv"
FIG_DIR <- "results/figures"

reqfiles <- c(
  COND_RDS, GAMMA_RDS, COND_GEOM_CSV, ANCHOR_CSV, COND_EXAMPLE_CSV,
  MODEL_PERF_CSV, GAMMA_SUMMARY_CSV, GAMMA_PAIR_CSV, GAMMA_EXAMPLE_CSV,
  STRONG_EXAMPLE_CSV, SCOPE_CSV
)
for (p in reqfiles) if (!file.exists(p)) stop("Missing RQ2 artifact: ", p, ". Run scripts/12_rq2_analysis.R first.")
dir.create(FIG_DIR, recursive = TRUE, showWarnings = FALSE)

METRIC_CLASSES <- MS_METRIC_CLASSES
DIM_TITLES <- c(
  placement = "Placement", optical = "Optical proxy",
  temporal = "Temporal resolution", duration = "Monitoring duration"
)
PAIR_LEVELS <- c("placement × optical", "placement × temporal", "optical × temporal")
STATE_LEVELS <- c("Low", "Middle", "High")
MODEL_LEVELS <- c("external_context", "exposure_state", "joint")
MODEL_LABELS <- c(external_context = "External", exposure_state = "Exposure state", joint = "Joint")
MODEL_COLORS <- setNames(MS_THREE_COLORS, unname(MODEL_LABELS))
base_square_theme <- theme_ms(aspect_ratio = 1, legend_position = "none")
asinh_display <- scales::transform_asinh()

cond <- readRDS(COND_RDS) |>
  mutate(
    metric_class = factor(metric_class, levels = METRIC_CLASSES),
    state_bin_label = factor(as.character(state_bin_label), levels = STATE_LEVELS)
  )
gamma <- readRDS(GAMMA_RDS) |>
  mutate(metric_class = factor(metric_class, levels = METRIC_CLASSES))
cg <- readr::read_csv(COND_GEOM_CSV, show_col_types = FALSE) |>
  mutate(
    metric_class = factor(metric_class, levels = METRIC_CLASSES),
    state_bin_label = factor(state_bin_label, levels = STATE_LEVELS)
  )
anchors <- readr::read_csv(ANCHOR_CSV, show_col_types = FALSE)
examples <- readr::read_csv(COND_EXAMPLE_CSV, show_col_types = FALSE)
perf <- readr::read_csv(MODEL_PERF_CSV, show_col_types = FALSE) |>
  mutate(
    metric_class = factor(metric_class, levels = METRIC_CLASSES),
    model_family = factor(model_family, levels = MODEL_LEVELS),
    model_label = factor(MODEL_LABELS[as.character(model_family)], levels = unname(MODEL_LABELS))
  )
gs <- readr::read_csv(GAMMA_SUMMARY_CSV, show_col_types = FALSE) |>
  mutate(
    metric_class = factor(metric_class, levels = METRIC_CLASSES),
    dimension_pair = factor(dimension_pair, levels = PAIR_LEVELS)
  )
gp <- readr::read_csv(GAMMA_PAIR_CSV, show_col_types = FALSE) |>
  mutate(dimension_pair = factor(dimension_pair, levels = PAIR_LEVELS))
ge <- readr::read_csv(GAMMA_EXAMPLE_CSV, show_col_types = FALSE)
strong <- readr::read_csv(STRONG_EXAMPLE_CSV, show_col_types = FALSE)
scope <- readr::read_csv(SCOPE_CSV, show_col_types = FALSE)

metric_legend <- cowplot::get_legend(
  ggplot(
    tibble(metric_class = factor(METRIC_CLASSES, levels = METRIC_CLASSES), x = seq_along(METRIC_CLASSES), y = 1),
    aes(x, y, color = metric_class)
  ) +
    geom_point(size = 2) +
    scale_color_ms_metric() +
    guides(color = guide_legend(title = "metric class", nrow = 1, byrow = TRUE)) +
    theme_void(base_family = MS_FONT, base_size = 8) +
    theme(legend.position = "bottom")
)

# Fig. 2a: conditional distributions; ordered state uses one primary-color gradient.
ids <- examples |>
  transmute(
    example_id = paste(dimension, configuration, metric, sep = "|"),
    example_label = paste0(DIM_TITLES[dimension], ": ", metric)
  )
exd <- cond |>
  mutate(example_id = paste(dimension, configuration, metric, sep = "|")) |>
  inner_join(ids, by = "example_id") |>
  filter(available, is.finite(e), !is.na(state_bin_label))

p2a <- ggplot(exd, aes(e, color = state_bin_label, fill = state_bin_label)) +
  geom_density(alpha = .12, linewidth = .48, adjust = .9) +
  geom_vline(xintercept = 0, linewidth = .28, color = "#737373") +
  facet_wrap(~example_label, scales = "free", ncol = 2) +
  scale_color_manual(values = MS_STATE_COLORS, drop = FALSE) +
  scale_fill_manual(values = MS_STATE_COLORS, drop = FALSE) +
  labs(
    title = "a  Conditional distortion distributions",
    x = "standardized signed distortion, e", y = "density",
    color = "reference state", fill = "reference state"
  ) +
  theme_ms(base_size = 7.4, aspect_ratio = 1, legend_position = "bottom") +
  theme(strip.text = element_text(size = 6.4), legend.text = element_text(size = 6.5))

conditional_ab_panel <- function(dim, letter) {
  d <- cg |>
    filter(dimension == dim) |>
    semi_join(anchors |> filter(dimension == dim), by = c("dimension", "configuration")) |>
    filter(is.finite(A_conditional), is.finite(B_conditional)) |>
    arrange(metric, configuration, state_bin)
  if (!nrow(d)) {
    return(
      ggplot() + annotate("text", x = 0, y = 0, label = "Not estimable", size = 3) +
        xlim(-1, 1) + ylim(-1, 1) + labs(title = paste0(letter, "  ", DIM_TITLES[[dim]])) + base_square_theme
    )
  }
  hi <- d |> group_by(metric, configuration) |> slice_max(state_bin, n = 1, with_ties = FALSE) |> ungroup()
  lab <- hi |> slice_max(A_conditional, n = 3, with_ties = FALSE)
  lim <- max(c(d$A_conditional, abs(d$B_conditional)), na.rm = TRUE) * 1.08

  ggplot(d, aes(B_conditional, A_conditional, color = metric_class)) +
    geom_vline(xintercept = 0, linewidth = .24, color = "#D8D8D8") +
    geom_abline(slope = c(-1, 1), intercept = 0, linetype = 2, linewidth = .3, color = "#8A8A8A") +
    geom_path(aes(group = interaction(metric, configuration)), color = "#6F7478", alpha = .40, linewidth = .45, lineend = "round") +
    geom_point(aes(shape = state_bin_label), size = 1.12, alpha = .76) +
    geom_point(data = hi, shape = 21, fill = "white", size = 1.85, stroke = .55, show.legend = FALSE) +
    geom_text(data = lab, aes(label = metric), size = 1.85, color = "#252525", vjust = -.65, check_overlap = TRUE, show.legend = FALSE) +
    scale_color_ms_metric() +
    scale_shape_manual(values = c(Low = 1, Middle = 16, High = 17)) +
    scale_x_continuous(transform = asinh_display, limits = c(-lim, lim), expand = expansion(mult = .025)) +
    scale_y_continuous(transform = asinh_display, limits = c(0, lim), expand = expansion(mult = .025)) +
    labs(title = paste0(letter, "  ", DIM_TITLES[[dim]]), x = "conditional B", y = "conditional A") +
    base_square_theme
}

p2b <- conditional_ab_panel("placement", "b")
p2c <- conditional_ab_panel("optical", "c")
p2d <- conditional_ab_panel("temporal", "d")
p2e <- conditional_ab_panel("duration", "e")

# Fig. 2f: grouped-CV predictability versus leave-site-out transportability.
perfwide <- perf |>
  filter(is.finite(r2), validation_scheme %in% c("participant_grouped", "leave_site_out")) |>
  select(dimension, configuration, metric, metric_class, outcome, model_family, model_label, validation_scheme, r2) |>
  pivot_wider(names_from = validation_scheme, values_from = r2) |>
  filter(is.finite(participant_grouped), is.finite(leave_site_out))

if (nrow(perfwide)) {
  lims <- range(c(perfwide$participant_grouped, perfwide$leave_site_out), finite = TRUE)
  pad <- .05 * diff(lims); if (!is.finite(pad) || pad == 0) pad <- .1
  lims <- c(lims[1] - pad, lims[2] + pad)
  p2f <- ggplot(perfwide, aes(participant_grouped, leave_site_out, color = model_label, shape = model_label)) +
    geom_abline(slope = 1, intercept = 0, linetype = 2, linewidth = .35, color = "#777777") +
    geom_point(size = 1.0, alpha = .56) +
    facet_wrap(~outcome, ncol = 1) +
    scale_color_manual(values = MODEL_COLORS, drop = FALSE) +
    scale_shape_manual(values = c(External = 16, `Exposure state` = 17, Joint = 15)) +
    coord_equal(xlim = lims, ylim = lims) +
    labs(
      title = "f  Predictability → transportability",
      x = "grouped-participant CV R²", y = "leave-site-out R²", color = NULL, shape = NULL
    ) +
    theme_ms(base_size = 7.4, aspect_ratio = 1, legend_position = "bottom") +
    theme(legend.text = element_text(size = 6.5))
} else {
  p2f <- ggplot() + annotate("text", x = 0, y = 0, label = "Prediction models not estimable", size = 3) +
    xlim(-1, 1) + ylim(-1, 1) + labs(title = "f  Predictability → transportability") + base_square_theme
}

fig2body <- plot_grid(p2a, p2b, p2c, p2d, p2e, p2f, ncol = 3, align = "hv", axis = "tblr")
fig2 <- plot_grid(fig2body, metric_legend, ncol = 1, rel_heights = c(1, .07))
ggsave(file.path(FIG_DIR, "Fig2_RQ2.pdf"), fig2, width = 12.2, height = 8.4, useDingbats = FALSE)
ggsave(file.path(FIG_DIR, "Fig2_RQ2.png"), fig2, width = 12.2, height = 8.4, dpi = 220)

# Fig. 3a: second-order contrast schematic; schematic keeps the common frame but no grid/axes.
scells <- tribble(
  ~x, ~y, ~label,
  0, 0, "reference\nM00", 1, 0, "a changed\nMa0",
  0, 1, "b changed\nM0b", 1, 1, "a + b\nMab"
)
p3a <- ggplot(scells, aes(x, y)) +
  geom_tile(width = .62, height = .42, fill = "white", color = MS_PRIMARY, linewidth = .55) +
  geom_text(aes(label = label), size = 2.5) +
  annotate("segment", x = .3, xend = .7, y = 0, yend = 0, arrow = grid::arrow(length = grid::unit(1.3, "mm")), linewidth = .35, color = MS_PRIMARY) +
  annotate("segment", x = .3, xend = .7, y = 1, yend = 1, arrow = grid::arrow(length = grid::unit(1.3, "mm")), linewidth = .35, color = MS_PRIMARY) +
  annotate("text", x = .5, y = .53, label = "γ = Δa|b − Δa|0", size = 3) +
  coord_equal(xlim = c(-.45, 1.45), ylim = c(-.45, 1.45), clip = "off") +
  labs(title = "a  Second-order interaction distortion") + theme_ms_blank(aspect_ratio = 1)

gids <- ge |> transmute(id = paste(dimension_pair, a_configuration, b_configuration, metric, sep = " | "), example_type)
ged <- gamma |>
  filter(available, is.finite(gamma)) |>
  mutate(id = paste(dimension_pair, a_configuration, b_configuration, metric, sep = " | ")) |>
  inner_join(gids, by = "id")
p3b <- ggplot(ged, aes(gamma)) +
  geom_density(fill = MS_PRIMARY, color = MS_PRIMARY, alpha = .18, linewidth = .48, adjust = .85) +
  geom_vline(xintercept = 0, linewidth = .28, color = "#707070") +
  facet_wrap(~example_type, scales = "free", ncol = 2) +
  labs(title = "b  Empirical D(γ)", x = "standardized interaction distortion, γ", y = "density") +
  theme_ms(base_size = 7.2, aspect_ratio = 1, legend_position = "none") +
  theme(strip.text = element_text(size = 6.2))

if (nrow(gs)) {
  lim <- max(c(gs$Q_mean_absolute, abs(gs$R_mean_signed)), na.rm = TRUE) * 1.06
  lab <- gs |> slice_max(Q_mean_absolute, n = 5, with_ties = FALSE)
  p3c <- ggplot(gs, aes(R_mean_signed, Q_mean_absolute, color = metric_class)) +
    geom_vline(xintercept = 0, linewidth = .24, color = "#D8D8D8") +
    geom_abline(slope = c(-1, 1), intercept = 0, linetype = 2, linewidth = .3, color = "#8A8A8A") +
    geom_point(aes(shape = dimension_pair), size = 1.05, alpha = .67) +
    geom_text(data = lab, aes(label = metric), size = 1.8, color = "#252525", vjust = -.6, check_overlap = TRUE, show.legend = FALSE) +
    scale_color_ms_metric() +
    scale_x_continuous(transform = asinh_display, limits = c(-lim, lim), expand = expansion(mult = .025)) +
    scale_y_continuous(transform = asinh_display, limits = c(0, lim), expand = expansion(mult = .025)) +
    labs(title = "c  Interaction geometry", x = "R: mean signed γ", y = "Q: mean absolute γ") + base_square_theme
} else {
  p3c <- ggplot() + annotate("text", x = 0, y = 0, label = "No gamma summaries") +
    xlim(-1, 1) + ylim(-1, 1) + labs(title = "c  Interaction geometry") + base_square_theme
}

p3d <- ggplot(gp, aes(dimension_pair, median_Q)) +
  geom_errorbar(aes(ymin = q25_Q, ymax = q75_Q), width = .12, linewidth = .5, color = MS_PRIMARY, alpha = .65) +
  geom_point(size = 2.2, color = MS_PRIMARY) +
  coord_flip() +
  labs(title = "d  Overall cross-dimensional dependence", x = NULL, y = "median Q across metric-configurations") +
  base_square_theme + theme(axis.text.y = element_text(size = 7))

if (nrow(strong)) {
  sid <- strong |> transmute(id = paste(dimension_pair, a_configuration, b_configuration, metric, sep = " | ")) |> pull(id)
  sd <- gamma |>
    filter(available, is.finite(gamma)) |>
    mutate(id = paste(dimension_pair, a_configuration, b_configuration, metric, sep = " | ")) |>
    filter(id == sid) |>
    select(marginal_a_ref, marginal_a_at_b) |>
    pivot_longer(everything(), names_to = "b_state", values_to = "marginal_effect") |>
    mutate(b_state = recode(b_state, marginal_a_ref = "b at reference", marginal_a_at_b = "b changed"))
  ss <- sd |> group_by(b_state) |>
    summarise(mean = mean(marginal_effect), q25 = quantile(marginal_effect, .25), q75 = quantile(marginal_effect, .75), .groups = "drop")
  p3e <- ggplot(ss, aes(b_state, mean, group = 1)) +
    geom_hline(yintercept = 0, linewidth = .3, color = "#A0A0A0") +
    geom_line(linewidth = .6, color = MS_PRIMARY) +
    geom_errorbar(aes(ymin = q25, ymax = q75), width = .1, linewidth = .46, color = MS_PRIMARY, alpha = .7) +
    geom_point(size = 2, color = MS_PRIMARY) +
    labs(title = paste0("e  Strong coupling: ", strong$metric[1]), x = NULL, y = "marginal effect of dimension a") +
    base_square_theme + theme(axis.text.x = element_text(angle = 15, hjust = 1))
} else {
  p3e <- ggplot() + annotate("text", x = 0, y = 0, label = "No strong-coupling example") +
    xlim(-1, 1) + ylim(-1, 1) + labs(title = "e  Strong coupling") + base_square_theme
}

scope <- scope |>
  mutate(
    a = sub(" × .*", "", dimension_pair),
    b = sub(".* × ", "", dimension_pair),
    status = factor(status, levels = c("estimated", "not population-estimated", "unavailable"))
  )
SCOPE_COLORS <- c("estimated" = MS_PRIMARY, "not population-estimated" = "#DDEAF4", "unavailable" = "white")
p3f <- ggplot(scope, aes(a, b)) +
  geom_tile(aes(fill = status), color = "black", linewidth = .35) +
  geom_text(aes(label = case_when(status == "estimated" ~ "estimated", status == "not population-estimated" ~ "limited", TRUE ~ "unavailable")), size = 2.2) +
  scale_fill_manual(values = SCOPE_COLORS, drop = FALSE) +
  labs(title = "f  Empirical interaction scope", x = NULL, y = NULL, fill = NULL) +
  theme_ms(base_size = 7.2, aspect_ratio = 1, legend_position = "bottom") +
  theme(panel.grid = element_blank(), axis.text.x = element_text(angle = 25, hjust = 1), legend.text = element_text(size = 6.2))

fig3body <- plot_grid(p3a, p3b, p3c, p3d, p3e, p3f, ncol = 3, align = "hv", axis = "tblr")
fig3 <- plot_grid(fig3body, metric_legend, ncol = 1, rel_heights = c(1, .07))
ggsave(file.path(FIG_DIR, "Fig3_RQ2.pdf"), fig3, width = 12.2, height = 8.4, useDingbats = FALSE)
ggsave(file.path(FIG_DIR, "Fig3_RQ2.png"), fig3, width = 12.2, height = 8.4, dpi = 220)
message("RQ2 figures complete with shared publication style: Fig2_RQ2 + Fig3_RQ2")
