# Centralized supplementary plotting entrypoint.
# Main-text figures live in 11_plot_fig1.R, 13a_plot_fig2.R, 13b_plot_fig3.R,
# 15a_plot_fig4.R, and 15b_plot_fig5.R. This script contains every FigS_* plot.

.ms_file <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
if (length(.ms_file)) {
  .ms_script <- normalizePath(sub("^--file=", "", .ms_file[[1]]), winslash = "/", mustWork = TRUE)
  .ms_root <- normalizePath(file.path(dirname(.ms_script), ".."), winslash = "/", mustWork = TRUE)
  if (!file.exists(file.path(.ms_root, "scripts", "utils", "figure_style.R"))) {
    stop("Could not resolve measurement-sufficiency repository root from ", .ms_script, call. = FALSE)
  }
  setwd(.ms_root)
}
rm(.ms_file)
if (exists(".ms_script")) rm(.ms_script)
if (exists(".ms_root")) rm(.ms_root)

# =============================================================================
# RQ1 supplementary figures
# =============================================================================
local({
  source("scripts/utils/plot_rq1_common.R", local = TRUE)

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
        "rq1_pairwise_change_long (derived Spearman) + rq1_pairwise_summary + rq1_local_transition_summary",
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
})

# =============================================================================
# RQ2 supplementary figures
# =============================================================================
local({
  source("scripts/utils/plot_rq2_common.R", local = TRUE)

  transition_state <- conditional |>
    mutate(
      metric = as.character(metric), metric_class = as.character(metric_class),
      state_bin_label = as.character(state_bin_label)
    ) |>
    filter(is.finite(A_conditional), is.finite(direction_ratio),
           state_bin_label %in% c("Low", "Middle", "High")) |>
    group_by(dimension, comparison_pair_id, pair_label, metric, metric_class, state_bin_label) |>
    summarise(
      A_state = median(A_conditional, na.rm = TRUE),
      direction_state = median(direction_ratio, na.rm = TRUE), .groups = "drop"
    ) |>
    pivot_wider(names_from = state_bin_label,
                values_from = c(A_state, direction_state), names_sep = "_") |>
    rowwise() |>
    mutate(
      n_A_states = sum(is.finite(c_across(starts_with("A_state_")))),
      A_span = if (n_A_states >= 2L) diff(range(c_across(starts_with("A_state_")), na.rm = TRUE)) else NA_real_,
      delta_A_HL = A_state_High - A_state_Low,
      delta_direction_HL = direction_state_High - direction_state_Low
    ) |>
    ungroup()
  transition_spread <- transition_state |>
    filter(is.finite(A_span)) |>
    group_by(dimension, comparison_pair_id, pair_label) |>
    summarise(
      n_metrics = n_distinct(metric),
      span_median = median(A_span, na.rm = TRUE),
      span_q25 = quantile(A_span, .25, na.rm = TRUE, names = FALSE),
      span_q75 = quantile(A_span, .75, na.rm = TRUE, names = FALSE), .groups = "drop"
    ) |>
    group_by(dimension) |>
    slice_max(span_median, n = 3, with_ties = FALSE) |>
    ungroup() |>
    mutate(
      dimension = factor(dimension, levels = DIMENSIONS, labels = unname(DIM_TITLES[DIMENSIONS])),
      transition_key = paste(as.character(dimension), pair_label, sep = "|||"),
      transition_key = forcats::fct_reorder(transition_key, span_median)
    )

  context_task <- performance |>
    filter(str_detect(validation_scheme, "^participant_grouped"),
           model_family %in% c("external_context", "exposure_state", "joint")) |>
    group_by(dimension, comparison_pair_id, metric, outcome, model_family) |>
    summarise(r2 = median(r2, na.rm = TRUE), n_test = max(n_test, na.rm = TRUE), .groups = "drop") |>
    pivot_wider(names_from = model_family, values_from = c(r2, n_test), names_sep = "__")
  for (nm in c("r2__external_context", "r2__exposure_state", "r2__joint",
               "n_test__external_context", "n_test__exposure_state", "n_test__joint")) {
    if (!nm %in% names(context_task)) context_task[[nm]] <- NA_real_
  }
  context_task <- context_task |>
    mutate(
      external_beyond_state = if_else(
        is.finite(r2__joint) & is.finite(r2__exposure_state) & n_test__joint == n_test__exposure_state,
        r2__joint - r2__exposure_state, NA_real_
      ),
      state_beyond_external = if_else(
        is.finite(r2__joint) & is.finite(r2__external_context) & n_test__joint == n_test__external_context,
        r2__joint - r2__external_context, NA_real_
      )
    ) |>
    select(dimension, comparison_pair_id, metric, outcome,
           external_beyond_state, state_beyond_external) |>
    pivot_longer(c(external_beyond_state, state_beyond_external),
                 names_to = "information", values_to = "delta_r2") |>
    mutate(
      information = recode(information,
        external_beyond_state = "External beyond state",
        state_beyond_external = "State beyond external"
      )
    ) |>
    filter(is.finite(delta_r2)) |>
    group_by(dimension, metric, outcome, information) |>
    summarise(delta_r2 = median(delta_r2, na.rm = TRUE), .groups = "drop") |>
    mutate(
      dimension = factor(dimension, levels = DIMENSIONS, labels = unname(DIM_TITLES[DIMENSIONS])),
      outcome_label = recode(outcome, signed = "Signed distortion", magnitude = "Absolute distortion", .default = outcome),
      information = factor(information, levels = c("External beyond state", "State beyond external"))
    )
  context_summary <- context_task |>
    group_by(dimension, outcome_label, information) |>
    summarise(
      n_metrics = n_distinct(metric),
      delta_median = median(delta_r2, na.rm = TRUE),
      delta_q25 = quantile(delta_r2, .25, na.rm = TRUE, names = FALSE),
      delta_q75 = quantile(delta_r2, .75, na.rm = TRUE, names = FALSE), .groups = "drop"
    )

  p2_atlas <- ggplot(conditional, aes(interaction(pair_label, state_bin_label, sep = "\n"), metric)) +
    geom_point(aes(size = A_conditional, fill = direction_ratio), shape = 21,
               color = "#3B3B3B", stroke = .14, alpha = .92) +
    facet_grid(rows = vars(metric_class), cols = vars(transition_family, dimension),
               scales = "free", space = "free", switch = "y") +
    ms_direction_scale(name = "B / A") +
    ms_magnitude_size_scale(name = "A = conditional mean |z|", range = c(.25, 3.0)) +
    labs(title = "Complete conditional geometry atlas",
         x = "oriented transition × transition-local exposure state", y = NULL) +
    ms_atlas_theme(base_size = 6.0, x_angle = 52) +
    theme(axis.text.x = element_text(size = 4.5))
  ms_plot_save(p2_atlas, file.path(OUT_DIR, "FigS_RQ2_conditional_atlas.pdf"), 16, 11.5)
  ms_plot_save(p2_atlas, file.path(OUT_DIR, "FigS_RQ2_conditional_atlas.png"), 16, 11.5)
  readr::write_csv(conditional |>
    mutate(metric = as.character(metric), metric_class = as.character(metric_class),
           dimension = as.character(dimension), transition_family = as.character(transition_family)),
    file.path("results", "rq2", "fig2_conditional_geometry_atlas.csv"), na = "")

  p2_spread_s <- ggplot(transition_spread, aes(span_median, transition_key)) +
    geom_segment(aes(x = span_q25, xend = span_q75, yend = transition_key),
                 linewidth = 1.0, color = "#9FB7C6", alpha = .58, lineend = "round") +
    geom_point(shape = 18, size = 2.0, color = MS_PRIMARY) +
    facet_wrap(~dimension, ncol = 2, scales = "free_y") +
    scale_y_discrete(labels = function(x) sub("^.*\\|\\|\\|", "", x)) +
    scale_x_continuous(trans = scales::transform_asinh(), breaks = scales::breaks_extended(n = 4)) +
    labs(title = "Transitions with the largest state-dependent spread",
         x = "median state span in A", y = NULL) +
    theme_rq2(base_size = 6.5) +
    theme(panel.grid.major.y = element_blank(), axis.line.y = element_blank(), axis.ticks.y = element_blank())
  ms_plot_save(p2_spread_s, file.path(OUT_DIR, "FigS_RQ2_state_spread.pdf"), 7.4, 4.6)
  ms_plot_save(p2_spread_s, file.path(OUT_DIR, "FigS_RQ2_state_spread.png"), 7.4, 4.6)

  CONTEXT_COLORS <- c("External beyond state" = MS_PRIMARY, "State beyond external" = MS_SECONDARY)
  CONTEXT_SHAPES <- c("External beyond state" = 16, "State beyond external" = 17)
  if (nrow(context_task)) {
    context_s_plot <- context_task |>
      mutate(dimension_num = as.integer(dimension),
             y_pos = dimension_num + if_else(information == "External beyond state", -.11, .11))
    context_s_summary <- context_summary |>
      mutate(dimension_num = as.integer(dimension),
             y_pos = dimension_num + if_else(information == "External beyond state", -.11, .11))
    p2_context_s <- ggplot(context_s_plot, aes(delta_r2, y_pos, color = information, shape = information)) +
      geom_vline(xintercept = 0, linewidth = .30, color = "#9DA2A5") +
      geom_point(position = position_jitter(width = 0, height = .035, seed = 56), size = .58, alpha = .18) +
      geom_segment(data = context_s_summary,
                   aes(x = delta_q25, xend = delta_q75, y = y_pos, yend = y_pos, color = information),
                   inherit.aes = FALSE, linewidth = 1.0, alpha = .58, lineend = "round") +
      geom_point(data = context_s_summary,
                 aes(delta_median, y_pos, color = information, shape = information),
                 inherit.aes = FALSE, size = 1.55) +
      facet_wrap(~outcome_label, nrow = 1) +
      scale_color_manual(values = CONTEXT_COLORS, drop = FALSE) +
      scale_shape_manual(values = CONTEXT_SHAPES, drop = FALSE) +
      scale_x_continuous(trans = scales::transform_asinh(), breaks = scales::breaks_extended(n = 4)) +
      scale_y_continuous(breaks = seq_along(DIMENSIONS), labels = unname(DIM_TITLES[DIMENSIONS]),
                         limits = c(.55, length(DIMENSIONS) + .45)) +
      labs(title = "Independent contextual information",
           x = "incremental participant-grouped CV R²", y = NULL) +
      theme_rq2(base_size = 6.4, legend_position = "bottom") +
      theme(panel.grid.major.y = element_blank(), axis.line.y = element_blank(), axis.ticks.y = element_blank(),
            axis.text.y = element_text(size = 5.0), strip.text = element_text(size = 5.7),
            legend.text = element_text(size = 4.7))
  } else {
    p2_context_s <- ggplot() + theme_void(base_family = MS_FONT) +
      annotate("text", x = 0, y = 0, label = "No paired grouped-CV model results", size = 2.2)
  }
  ms_plot_save(p2_context_s, file.path(OUT_DIR, "FigS_RQ2_context_increment.pdf"), 7.6, 4.8)
  ms_plot_save(p2_context_s, file.path(OUT_DIR, "FigS_RQ2_context_increment.png"), 7.6, 4.8)

  format_gamma_transition <- function(x) {
    x |>
      str_replace_all("_LIGHT_to_MEDI", " · LIGHT → MEDI") |>
      str_replace_all("([0-9]+)to([0-9]+)", "\\1 → \\2 s") |>
      str_replace_all("_", " · ")
  }
  PAIR_LEVELS <- c("Placement × optical", "Optical × temporal", "Placement × temporal")
  gamma_plot <- gamma_summary |>
    mutate(
      dimension_pair = case_when(
        dimension_a == "placement" & dimension_b == "optical" ~ PAIR_LEVELS[[1]],
        dimension_a == "placement" & dimension_b == "temporal" ~ PAIR_LEVELS[[3]],
        dimension_a == "optical" & dimension_b == "temporal" ~ PAIR_LEVELS[[2]],
        TRUE ~ paste(dimension_a, "×", dimension_b)
      ),
      dimension_pair = factor(dimension_pair, levels = PAIR_LEVELS),
      metric = as.character(metric), metric_class = factor(metric_class, levels = METRIC_CLASSES),
      transition_display = format_gamma_transition(transition), Q = abs(as.numeric(Q)), R = as.numeric(R)
    )
  gamma_atlas <- gamma_plot |>
    mutate(
      dimension = as.character(dimension_a),
      transition_display = factor(transition_display, levels = unique(transition_display))
    ) |>
    ms_add_metric_order(metric_order)
  gamma_limit <- max(abs(gamma_atlas$R), na.rm = TRUE)
  if (!is.finite(gamma_limit) || gamma_limit <= 0) gamma_limit <- 1
  p3_atlas <- ggplot(gamma_atlas, aes(transition_display, R, color = metric_class)) +
    geom_hline(yintercept = 0, linewidth = .28, color = "#8A8A8A") +
    geom_segment(aes(xend = transition_display, y = 0, yend = R), alpha = .34, linewidth = .40) +
    geom_point(aes(size = Q), alpha = .90) +
    facet_grid(metric_class ~ dimension_pair, scales = "free_x", space = "free", switch = "y") +
    scale_color_ms_metric() +
    scale_size_continuous(range = c(.35, 2.8), name = "Q = mean |gamma|") +
    scale_y_continuous(limits = c(-gamma_limit * 1.05, gamma_limit * 1.05),
                       breaks = scales::breaks_extended(n = 5)) +
    labs(title = "Complete cross-dimensional interaction atlas",
         x = "oriented local transition", y = "R = mean γ") +
    ms_atlas_theme(base_size = 6.2, x_angle = 48)
  ms_plot_save(p3_atlas, file.path(OUT_DIR, "FigS_RQ2_gamma_atlas.pdf"), 14.5, 10.5)
  ms_plot_save(p3_atlas, file.path(OUT_DIR, "FigS_RQ2_gamma_atlas.png"), 14.5, 10.5)
  readr::write_csv(gamma_atlas |>
    mutate(metric = as.character(metric), metric_class = as.character(metric_class),
           transition_display = as.character(transition_display), dimension_pair = as.character(dimension_pair)),
    file.path("results", "rq2", "fig3_gamma_atlas.csv"), na = "")

  if (nrow(performance)) {
    perf_plot <- performance |>
      filter(is.finite(rmse) | is.finite(mae) | is.finite(r2)) |>
      group_by(dimension, model_family, outcome, validation_scheme) |>
      summarise(rmse = median(rmse, na.rm = TRUE), mae = median(mae, na.rm = TRUE),
                r2 = median(r2, na.rm = TRUE), .groups = "drop") |>
      pivot_longer(c(rmse, mae, r2), names_to = "measure", values_to = "value") |>
      mutate(dimension = factor(dimension, levels = DIMENSIONS))
    p_perf <- ggplot(perf_plot, aes(interaction(model_family, validation_scheme, sep = "\n"), outcome, fill = value)) +
      geom_tile(color = "white", linewidth = .12) +
      facet_grid(measure ~ dimension, scales = "free", space = "free", switch = "y") +
      scale_fill_ms_sequential(name = "median value") +
      labs(title = "RQ2 model validation diagnostics", x = "model family × validation scheme", y = NULL) +
      ms_atlas_theme(base_size = 6.1, x_angle = 48)
  } else {
    p_perf <- ggplot() + theme_void() +
      annotate("text", x = 0, y = 0,
               label = "No model-performance rows; RQ2_RUN_MODELS=0 or no eligible tasks.")
  }
  ms_plot_save(p_perf, file.path(OUT_DIR, "FigS_RQ2_model_performance.pdf"), 13, 8.5)
  ms_plot_save(p_perf, file.path(OUT_DIR, "FigS_RQ2_model_performance.png"), 13, 8.5)

  ms_plot_write_manifest(
    file.path(OUT_DIR, "figure_artifact_manifest.csv"),
    tibble(
      figure = c(
        "Fig2_RQ2", "Fig3_RQ2", "FigS_RQ2_conditional_atlas", "FigS_RQ2_state_spread",
        "FigS_RQ2_context_increment", "FigS_RQ2_gamma_atlas", "FigS_RQ2_model_performance"
      ),
      input_artifact = c(
        "rq2_conditional_geometry+rq2_model_coefficients", "rq2_gamma_summary",
        "rq2_conditional_geometry", "rq2_conditional_geometry", "rq2_model_performance",
        "rq2_gamma_summary", "rq2_model_performance"
      ),
      core_artifact_version = CORE_VERSION,
      rq1_analysis_version = RQ1_VERSION,
      rq2_analysis_version = RQ2_VERSION,
      rq3_analysis_version = NA_character_
    )
  )
})

# =============================================================================
# RQ3 supplementary figures
# =============================================================================
local({
  source("scripts/utils/plot_rq3_common.R", local = TRUE)

  convergence_display <- convergence |>
    filter(dimension %in% ORDERED_DIMS, is.finite(G), is.finite(requirement_position)) |>
    group_by(dimension, metric, metric_class, requirement_position) |>
    summarise(G_display = median(G, na.rm = TRUE), .groups = "drop") |>
    mutate(metric_class = factor(metric_class, levels = METRIC_CLASSES))

  p4s_convergence <- ggplot(
    convergence_display,
    aes(requirement_position, G_display, group = metric, color = metric_class)
  ) +
    geom_line(alpha = .46, linewidth = .36) + geom_point(size = .45, alpha = .58) +
    facet_wrap(~dimension, nrow = 1, scales = "free_x") +
    scale_color_ms_metric() +
    scale_y_continuous(trans = scales::transform_asinh(), breaks = scales::breaks_extended(n = 4)) +
    labs(title = "Adjacent-transition change by metric",
         x = "requirement position", y = "G = mean |z|") +
    theme_ms(base_size = 6.1, legend_position = "bottom")

  sufficiency_plot <- sufficiency |>
    filter(dimension %in% ORDERED_DIMS) |>
    group_by(dimension, metric, metric_class, epsilon) |>
    summarise(
      resolved_fraction = mean(status == "resolved", na.rm = TRUE),
      fraction_sufficient = if (any(status == "resolved")) {
        mean(sufficient[status == "resolved"], na.rm = TRUE)
      } else NA_real_,
      .groups = "drop"
    ) |>
    mutate(metric_class = factor(metric_class, levels = METRIC_CLASSES))

  p4s_sufficiency <- ggplot(
    sufficiency_plot,
    aes(epsilon, fraction_sufficient, group = metric, color = metric_class)
  ) +
    geom_line(alpha = .46, linewidth = .36) +
    facet_wrap(~dimension, nrow = 1, scales = "free_x") +
    scale_color_ms_metric() +
    scale_x_continuous(trans = scales::transform_asinh(), breaks = scales::breaks_extended(n = 4)) +
    scale_y_continuous(limits = c(0, 1), labels = scales::label_percent(accuracy = 25)) +
    labs(title = "Observed sufficiency projection by metric",
         x = "tolerance ε", y = "fraction of resolved states sufficient") +
    theme_ms(base_size = 6.1, legend_position = "bottom")

  fig4s <- cowplot::plot_grid(p4s_convergence, p4s_sufficiency, ncol = 1, rel_heights = c(1, 1))
  ms_plot_save(fig4s, file.path(OUT_DIR, "FigS_RQ3_single_dimension_detail.pdf"), 12.5, 8.0)
  ms_plot_save(fig4s, file.path(OUT_DIR, "FigS_RQ3_single_dimension_detail.png"), 12.5, 8.0)

  ms_plot_write_manifest(
    file.path(OUT_DIR, "figure_artifact_manifest.csv"),
    tibble(
      figure = c("Fig4_RQ3", "Fig5_RQ3", "FigS_RQ3_single_dimension_detail"),
      input_artifact = c(
        "rq3_observed_stability+sufficiency+unordered_substitutability",
        "rq3_joint_summary+rq3_pareto_occupancy",
        "rq3_convergence_profile+sufficiency"
      ),
      core_artifact_version = CORE_VERSION,
      rq1_analysis_version = RQ1_VERSION,
      rq2_analysis_version = NA_character_,
      rq3_analysis_version = RQ3_VERSION
    )
  )
})

message("Supplementary figures complete: RQ1, RQ2, and RQ3 FigS outputs centralized in one entrypoint.")
