# Canonical RQ2 plotting entrypoint. Plotting consumes frozen v5 outputs only;
# unlike the analysis runtime, the plot source requires no deployment-time patch.
.ms_file <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
if (length(.ms_file)) {
  .ms_script <- normalizePath(sub("^--file=", "", .ms_file[[1]]), winslash = "/", mustWork = TRUE)
  .ms_root <- normalizePath(file.path(dirname(.ms_script), ".."), winslash = "/", mustWork = TRUE)
  if (!file.exists(file.path(.ms_root, "scripts", "13_plot_rq2_v5.R"))) {
    stop("Could not resolve measurement-sufficiency repository root from ", .ms_script, call. = FALSE)
  }
  setwd(.ms_root)
}
rm(.ms_file)
if (exists(".ms_script")) rm(.ms_script)
if (exists(".ms_root")) rm(.ms_root)
source(file.path("scripts", "13_plot_rq2_v5.R"), local = .GlobalEnv)
source(file.path("scripts", "utils", "analysis_design.R"), local = .GlobalEnv)

# Guard against plotting stale RQ2 artifacts after a measurement-lattice change.
if (is.list(condition) && !is.null(condition$analysis_design_id) &&
    !identical(as.character(condition$analysis_design_id[[1]]), ms_analysis_design_id())) {
  stop("RQ2 plotting inputs do not match the current frozen analysis design", call. = FALSE)
}

# Main Fig. 2a deliberately keeps a linear y scale. Facets retain their own
# ranges because the scientific comparison is exposure-state modulation within
# each measurement dimension, not transformed cross-dimension magnitude.
p2a <- ggplot() +
  geom_point(
    data = conditional_metric_state,
    aes(x_pos, A_state, color = metric_class),
    position = position_jitter(width = .018, height = 0, seed = 41),
    size = .48, alpha = .20
  ) +
  geom_linerange(
    data = conditional_profile_summary,
    aes(x_pos, ymin = A_q25, ymax = A_q75, color = metric_class),
    linewidth = .68, alpha = .50
  ) +
  geom_point(
    data = conditional_profile_summary,
    aes(x_pos, A_median, color = metric_class),
    shape = 18, size = 1.55
  ) +
  facet_wrap(
    ~factor(dimension, levels = DIMENSIONS, labels = unname(DIM_TITLES[DIMENSIONS])),
    ncol = 2, scales = "free_y"
  ) +
  scale_color_ms_metric(guide = "none") +
  scale_x_continuous(
    breaks = 1:3, labels = c("Low", "Middle", "High"),
    limits = c(.72, 3.28), expand = expansion(mult = c(0, 0))
  ) +
  scale_y_continuous(breaks = scales::breaks_extended(n = 4)) +
  labs(
    title = "a  Conditional distortion magnitude across exposure state",
    x = "transition-local exposure state", y = "conditional A = mean |z|"
  ) +
  theme_rq2(base_size = 6.45) +
  theme(
    panel.grid.major.x = element_blank(), strip.text = element_text(size = 5.9),
    panel.spacing = grid::unit(2.2, "mm")
  )

# -----------------------------------------------------------------------------
# Main-text display refinement for Fig. 2b.
# Keep the coefficient axis linear and shared across dimensions, but prevent a
# very small number of extreme metric-level coefficients from determining the
# whole display window. Class summaries are always computed from all estimates.
# -----------------------------------------------------------------------------
if (exists("coef_metric") && nrow(coef_metric) && exists("coef_summary") && nrow(coef_summary)) {
  coef_abs_cutoff <- as.numeric(
    stats::quantile(abs(coef_metric$estimate), probs = .99, na.rm = TRUE, names = FALSE, type = 8)
  )
  if (!is.finite(coef_abs_cutoff) || coef_abs_cutoff <= 0) {
    coef_abs_cutoff <- max(abs(coef_metric$estimate), na.rm = TRUE)
  }

  coef_metric_display <- coef_metric |>
    mutate(
      abs_estimate = abs(estimate),
      display_cutoff = coef_abs_cutoff,
      displayed_in_fig2b = is.finite(estimate) & abs_estimate <= display_cutoff
    )

  summary_extent <- max(abs(c(
    coef_summary$estimate_median,
    coef_summary$estimate_q25,
    coef_summary$estimate_q75
  )), na.rm = TRUE)
  coef_limit_display <- max(coef_abs_cutoff, summary_extent, na.rm = TRUE) * 1.04
  if (!is.finite(coef_limit_display) || coef_limit_display <= 0) coef_limit_display <- 1

  coef_metric_visible <- coef_metric_display |> filter(displayed_in_fig2b)

  p2b <- ggplot(
    coef_metric_visible,
    aes(estimate, y_pos, color = predictor_family, shape = outcome_label)
  ) +
    geom_vline(xintercept = 0, linewidth = .30, color = "#9DA2A5") +
    geom_point(
      position = position_jitter(width = 0, height = .035, seed = 54),
      size = .52, alpha = .18
    ) +
    geom_segment(
      data = coef_summary,
      aes(
        x = estimate_q25, xend = estimate_q75,
        y = y_pos, yend = y_pos, color = predictor_family
      ),
      inherit.aes = FALSE, linewidth = .90, alpha = .58, lineend = "round"
    ) +
    geom_point(
      data = coef_summary,
      aes(estimate_median, y_pos, color = predictor_family, shape = outcome_label),
      inherit.aes = FALSE, size = 1.45
    ) +
    facet_wrap(~dimension, ncol = 2) +
    scale_color_manual(values = PREDICTOR_COLORS, drop = FALSE) +
    scale_shape_manual(values = OUTCOME_SHAPES, drop = FALSE) +
    scale_y_continuous(
      breaks = seq_along(levels(coef_metric$predictor)),
      labels = levels(coef_metric$predictor),
      limits = c(.55, length(levels(coef_metric$predictor)) + .45)
    ) +
    scale_x_continuous(
      limits = c(-coef_limit_display, coef_limit_display),
      breaks = scales::breaks_extended(n = 5)
    ) +
    guides(
      color = guide_legend(
        title = NULL, nrow = 1, order = 1,
        override.aes = list(alpha = 1, size = 1.15)
      ),
      shape = guide_legend(
        title = NULL, nrow = 1, order = 2,
        override.aes = list(alpha = 1, size = 1.15)
      )
    ) +
    labs(
      title = "b  Contextual predictors of distortion",
      subtitle = "raw points: central 99% of |β|; summaries use all estimates",
      x = "standardized joint-model coefficient", y = NULL
    ) +
    theme_rq2(base_size = 6.1, legend_position = "bottom") +
    theme(
      panel.grid.major.y = element_blank(),
      axis.line.y = element_blank(), axis.ticks.y = element_blank(),
      axis.text.y = element_text(size = 4.5), strip.text = element_text(size = 5.35),
      plot.subtitle = element_text(
        size = 4.45, colour = "#666A6D", margin = margin(t = -1, b = 2)
      ),
      legend.text = element_text(size = 4.45),
      legend.key.width = grid::unit(2.8, "mm"),
      legend.spacing.x = grid::unit(.8, "mm"),
      panel.spacing = grid::unit(1.8, "mm")
    )

  p2bottom <- cowplot::plot_grid(
    p2b, p2c, ncol = 2, rel_widths = c(.56, .44),
    align = "hv", axis = "tblr", greedy = TRUE
  )
  p2body <- cowplot::plot_grid(
    p2a, p2bottom, ncol = 1, rel_heights = c(1.10, .90),
    align = "v", axis = "l", greedy = TRUE
  )
  p2 <- cowplot::plot_grid(
    metric_legend, p2body, ncol = 1, rel_heights = c(.042, 1),
    align = "v", greedy = TRUE
  )
  ms_plot_save(p2, file.path(OUT_DIR, "Fig2_RQ2.png"), 9.0, 6.6)

  readr::write_csv(
    coef_metric_display |>
      mutate(
        dimension = as.character(dimension),
        predictor = as.character(predictor),
        predictor_family = as.character(predictor_family),
        outcome_label = as.character(outcome_label)
      ),
    file.path("results", "rq2", "fig2_context_predictor_display_diagnostics.csv"),
    na = ""
  )

  message(
    "Fig. 2 display refinement: ",
    sum(!coef_metric_display$displayed_in_fig2b), " / ", nrow(coef_metric_display),
    " raw coefficient points omitted beyond pooled q99(|beta|)=",
    signif(coef_abs_cutoff, 4), "; summaries retain all estimates."
  )
}
