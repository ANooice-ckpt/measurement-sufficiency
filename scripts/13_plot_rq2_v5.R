suppressPackageStartupMessages({library(tidyverse); library(cowplot)})
source("scripts/utils/figure_style.R")
source("scripts/utils/figure_atlas.R")
source("scripts/utils/plot_contracts.R")

RQ1_SUMMARY_CSV <- file.path("results", "rq1", "rq1_pairwise_summary.csv")
CONDITION_RDS <- file.path("results", "rq2", "rq2_condition_long.rds")
COND_GEOM_CSV <- file.path("results", "rq2", "rq2_conditional_geometry.csv")
MODEL_PERF_CSV <- file.path("results", "rq2", "rq2_model_performance.csv")
MODEL_MANIFEST_CSV <- file.path("results", "rq2", "rq2_model_artifact_manifest.csv")
GAMMA_RDS <- file.path("results", "rq2", "rq2_gamma_long.rds")
GAMMA_SUMMARY_CSV <- file.path("results", "rq2", "rq2_gamma_summary.csv")
SCOPE_CSV <- file.path("results", "rq2", "rq2_interaction_scope.csv")
OUT_DIR <- file.path("results", "rq2", "figures")
ms_plot_require_files(c(RQ1_SUMMARY_CSV, CONDITION_RDS, COND_GEOM_CSV, MODEL_PERF_CSV,
                        MODEL_MANIFEST_CSV, GAMMA_RDS, GAMMA_SUMMARY_CSV, SCOPE_CSV),
                      "RQ2 v5 plotting inputs")
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

METRIC_CLASSES <- MS_METRIC_CLASSES
DIMENSIONS <- c("placement", "optical", "temporal", "duration")
DIM_TITLES <- c(
  placement = "Placement", optical = "Optical representation",
  temporal = "Temporal resolution", duration = "Monitoring duration"
)

rq1_summary <- readr::read_csv(RQ1_SUMMARY_CSV, show_col_types = FALSE, progress = FALSE)
condition <- readRDS(CONDITION_RDS)
conditional <- readr::read_csv(COND_GEOM_CSV, show_col_types = FALSE, progress = FALSE)
performance <- readr::read_csv(MODEL_PERF_CSV, show_col_types = FALSE, progress = FALSE)
model_manifest <- readr::read_csv(MODEL_MANIFEST_CSV, show_col_types = FALSE, progress = FALSE)
gamma_long <- readRDS(GAMMA_RDS)
gamma_summary <- readr::read_csv(GAMMA_SUMMARY_CSV, show_col_types = FALSE, progress = FALSE)
scope <- readr::read_csv(SCOPE_CSV, show_col_types = FALSE, progress = FALSE)

ms_plot_require_columns(rq1_summary, c("metric", "metric_class", "dimension", "A_mean_absolute"),
                        "rq1_pairwise_summary.csv")
ms_plot_require_columns(conditional,
  c("core_artifact_version", "rq1_analysis_version", "rq2_analysis_version", "dimension",
    "comparison_pair_id", "config_a_label", "config_b_label", "metric", "metric_class",
    "state_bin_label", "A_conditional", "B_conditional"), "rq2_conditional_geometry.csv")
ms_plot_require_columns(performance,
  c("dimension", "comparison_pair_id", "metric", "outcome", "model_family", "validation_scheme",
    "n_test", "rmse", "mae", "r2"), "rq2_model_performance.csv")
ms_plot_require_columns(model_manifest,
  c("core_artifact_version", "rq1_analysis_version", "rq2_analysis_version"),
  "rq2_model_artifact_manifest.csv")
ms_plot_require_columns(gamma_summary,
  c("dimension_a", "dimension_b", "comparison_lattice", "transition", "metric", "metric_class", "R", "Q"),
  "rq2_gamma_summary.csv")
ms_plot_require_columns(scope, c("dimension_pair", "primary_scope"), "rq2_interaction_scope.csv")

condition_core <- if (is.list(condition)) condition$core_artifact_version else NULL
condition_rq1 <- if (is.list(condition)) condition$rq1_analysis_version else NULL
condition_rq2 <- if (is.list(condition)) condition$rq2_analysis_version else NULL
RQ1_VERSION <- ms_plot_one_version(c(condition_rq1, conditional$rq1_analysis_version,
                                     model_manifest$rq1_analysis_version, gamma_long$rq1_analysis_version),
                                   "rq1_analysis_version")
RQ2_VERSION <- ms_plot_one_version(c(condition_rq2, conditional$rq2_analysis_version,
                                     model_manifest$rq2_analysis_version, gamma_long$rq2_analysis_version),
                                   "rq2_analysis_version")
CORE_VERSION <- ms_plot_assert_core(c(condition_core, conditional$core_artifact_version,
                                     model_manifest$core_artifact_version, gamma_long$core_artifact_version))
ms_plot_assert_prefix(RQ1_VERSION, "rq1_v5_", "rq1_analysis_version")
ms_plot_assert_prefix(RQ2_VERSION, "rq2_v5_", "rq2_analysis_version")

metric_order <- ms_metric_order(rq1_summary)
metric_class_lookup <- rq1_summary |> distinct(metric, metric_class)
conditional <- conditional |>
  mutate(
    metric_class = factor(metric_class, levels = METRIC_CLASSES),
    transition_family = factor(
      if_else(dimension %in% c("placement", "optical"),
              "Target alignment", "Measurement requirement"),
      levels = c("Target alignment", "Measurement requirement")
    ),
    dimension = factor(dimension, levels = DIMENSIONS),
    pair_label = paste(config_a_label, "→", config_b_label),
    state_bin_label = factor(state_bin_label, levels = c("Low", "Middle", "High")),
    direction_ratio = ms_direction_ratio(B_conditional, A_conditional)
  ) |>
  mutate(dimension = as.character(dimension)) |>
  ms_add_metric_order(metric_order)

theme_rq2 <- function(base_size = 6.7, legend_position = "none") {
  theme_ms(base_size = base_size, legend_position = legend_position) +
    theme(
      panel.border = element_blank(),
      axis.line.x = element_line(colour = "#505457", linewidth = .34),
      axis.line.y = element_line(colour = "#505457", linewidth = .34),
      panel.grid.major = element_line(colour = "#ECEFF0", linewidth = .22),
      panel.grid.minor = element_blank(),
      axis.ticks = element_line(colour = "#505457", linewidth = .28),
      strip.background = element_blank(),
      strip.text = element_text(face = "bold", colour = "#25282A", margin = margin(1, 2, 2, 2)),
      plot.title = element_text(size = base_size + .8, face = "bold", margin = margin(b = 3)),
      plot.margin = margin(3, 4, 3, 4)
    )
}

metric_legend_source <- ggplot(
  tibble(metric_class = factor(METRIC_CLASSES, levels = METRIC_CLASSES),
         x = seq_along(METRIC_CLASSES), y = 1),
  aes(x, y, color = metric_class)
) +
  geom_point(size = 1.7) +
  scale_color_ms_metric() +
  guides(color = guide_legend(title = NULL, nrow = 1, byrow = TRUE,
                              override.aes = list(size = 1.5))) +
  theme_void(base_family = MS_FONT, base_size = 7) +
  theme(
    legend.position = "bottom", legend.direction = "horizontal",
    legend.margin = margin(0, 0, 0, 0), legend.box.margin = margin(0, 0, 0, 0),
    legend.text = element_text(size = 5.35), legend.key.width = grid::unit(3.5, "mm")
  )
metric_legend <- cowplot::get_legend(metric_legend_source)

# =============================================================================
# Fig. 2 — context dependence of distortion
# =============================================================================

# a. Exposure-state dependence of distortion magnitude.
conditional_metric_state <- conditional |>
  mutate(
    metric = as.character(metric),
    metric_class = factor(as.character(metric_class), levels = METRIC_CLASSES),
    state_num = as.integer(state_bin_label)
  ) |>
  filter(is.finite(A_conditional), is.finite(state_num)) |>
  group_by(dimension, metric, metric_class, state_bin_label, state_num) |>
  summarise(A_state = median(A_conditional, na.rm = TRUE), .groups = "drop")

conditional_profile_summary <- conditional_metric_state |>
  group_by(dimension, metric_class, state_bin_label, state_num) |>
  summarise(
    n_metrics = n_distinct(metric),
    A_median = median(A_state, na.rm = TRUE),
    A_q25 = quantile(A_state, .25, na.rm = TRUE, names = FALSE),
    A_q75 = quantile(A_state, .75, na.rm = TRUE, names = FALSE),
    .groups = "drop"
  )

p2a <- ggplot() +
  geom_line(
    data = conditional_metric_state,
    aes(state_num, A_state, group = interaction(metric, dimension), color = metric_class),
    linewidth = .24, alpha = .065
  ) +
  geom_linerange(
    data = conditional_profile_summary,
    aes(state_num, ymin = A_q25, ymax = A_q75, color = metric_class),
    linewidth = .42, alpha = .42
  ) +
  geom_line(
    data = conditional_profile_summary,
    aes(state_num, A_median, group = metric_class, color = metric_class),
    linewidth = .76, alpha = .96
  ) +
  geom_point(
    data = conditional_profile_summary,
    aes(state_num, A_median, color = metric_class),
    size = 1.0, alpha = .98
  ) +
  facet_grid(. ~ factor(dimension, levels = DIMENSIONS, labels = unname(DIM_TITLES[DIMENSIONS]))) +
  scale_color_ms_metric(guide = "none") +
  scale_x_continuous(breaks = 1:3, labels = c("Low", "Middle", "High"),
                     expand = expansion(mult = c(.08, .08))) +
  scale_y_continuous(trans = scales::transform_asinh(), breaks = scales::breaks_extended(n = 4)) +
  labs(title = "a  Conditional distortion magnitude across exposure state",
       x = "transition-local exposure state", y = "conditional A = mean |z|") +
  theme_rq2(base_size = 6.6) +
  theme(panel.grid.major.x = element_blank(), strip.text.x = element_text(size = 6.2))

# Transition-resolved state geometry. The compact spread summary is retained as
# a supplement; the direction shift remains Fig. 2c.
transition_state <- conditional |>
  mutate(
    metric = as.character(metric),
    metric_class = as.character(metric_class),
    state_bin_label = as.character(state_bin_label)
  ) |>
  filter(is.finite(A_conditional), is.finite(direction_ratio),
         state_bin_label %in% c("Low", "Middle", "High")) |>
  group_by(dimension, comparison_pair_id, pair_label, metric, metric_class, state_bin_label) |>
  summarise(
    A_state = median(A_conditional, na.rm = TRUE),
    direction_state = median(direction_ratio, na.rm = TRUE),
    .groups = "drop"
  ) |>
  pivot_wider(
    names_from = state_bin_label,
    values_from = c(A_state, direction_state),
    names_sep = "_"
  ) |>
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
    span_q75 = quantile(A_span, .75, na.rm = TRUE, names = FALSE),
    .groups = "drop"
  ) |>
  group_by(dimension) |>
  slice_max(span_median, n = 3, with_ties = FALSE) |>
  ungroup() |>
  mutate(
    dimension = factor(dimension, levels = DIMENSIONS, labels = unname(DIM_TITLES[DIMENSIONS])),
    transition_key = paste(as.character(dimension), pair_label, sep = "|||"),
    transition_key = forcats::fct_reorder(transition_key, span_median)
  )

# b. External context enters through the frozen predictor-family models. The
# runtime uses one participant-grouped fold map per scientific task. R2
# differences are retained only when the compared families have the same test
# support, then collapsed across transitions within each metric for display.
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
      is.finite(r2__joint) & is.finite(r2__exposure_state) &
        n_test__joint == n_test__exposure_state,
      r2__joint - r2__exposure_state, NA_real_
    ),
    state_beyond_external = if_else(
      is.finite(r2__joint) & is.finite(r2__external_context) &
        n_test__joint == n_test__external_context,
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
  left_join(metric_class_lookup, by = "metric") |>
  mutate(
    metric_class = factor(metric_class, levels = METRIC_CLASSES),
    dimension_num = match(dimension, DIMENSIONS),
    outcome_label = recode(outcome, signed = "Signed distortion", magnitude = "Absolute distortion", .default = outcome),
    information = factor(information, levels = c("External beyond state", "State beyond external")),
    y_pos = dimension_num + if_else(information == "External beyond state", -.11, .11)
  )

context_summary <- context_task |>
  group_by(dimension, dimension_num, outcome, outcome_label, information) |>
  summarise(
    n_metrics = n_distinct(metric),
    delta_median = median(delta_r2, na.rm = TRUE),
    delta_q25 = quantile(delta_r2, .25, na.rm = TRUE, names = FALSE),
    delta_q75 = quantile(delta_r2, .75, na.rm = TRUE, names = FALSE),
    .groups = "drop"
  ) |>
  mutate(y_pos = dimension_num + if_else(information == "External beyond state", -.11, .11))

CONTEXT_COLORS <- c("External beyond state" = MS_PRIMARY, "State beyond external" = MS_SECONDARY)
CONTEXT_SHAPES <- c("External beyond state" = 16, "State beyond external" = 17)
if (nrow(context_task)) {
  p2b <- ggplot(context_task, aes(delta_r2, y_pos, color = information, shape = information)) +
    geom_vline(xintercept = 0, linewidth = .30, color = "#9DA2A5") +
    geom_point(position = position_jitter(width = 0, height = .035, seed = 54),
               size = .58, alpha = .18) +
    geom_segment(
      data = context_summary,
      aes(x = delta_q25, xend = delta_q75, y = y_pos, yend = y_pos, color = information),
      inherit.aes = FALSE, linewidth = 1.0, alpha = .58, lineend = "round"
    ) +
    geom_point(
      data = context_summary,
      aes(delta_median, y_pos, color = information, shape = information),
      inherit.aes = FALSE, size = 1.55
    ) +
    facet_wrap(~outcome_label, nrow = 1) +
    scale_color_manual(values = CONTEXT_COLORS, drop = FALSE) +
    scale_shape_manual(values = CONTEXT_SHAPES, drop = FALSE) +
    scale_x_continuous(trans = scales::transform_asinh(), breaks = scales::breaks_extended(n = 4)) +
    scale_y_continuous(breaks = seq_along(DIMENSIONS), labels = unname(DIM_TITLES[DIMENSIONS]),
                       limits = c(.55, length(DIMENSIONS) + .45)) +
    guides(
      color = guide_legend(title = NULL, nrow = 1, override.aes = list(alpha = 1, size = 1.2)),
      shape = guide_legend(title = NULL, nrow = 1, override.aes = list(alpha = 1, size = 1.2))
    ) +
    labs(title = "b  Independent contextual information",
         x = "incremental grouped-CV R²", y = NULL) +
    theme_rq2(base_size = 6.35, legend_position = "bottom") +
    theme(
      panel.grid.major.y = element_blank(), axis.line.y = element_blank(), axis.ticks.y = element_blank(),
      axis.text.y = element_text(size = 5.0), strip.text = element_text(size = 5.7),
      legend.text = element_text(size = 4.7), legend.key.width = grid::unit(3.2, "mm"),
      legend.spacing.x = grid::unit(1.2, "mm"), panel.spacing.x = grid::unit(2.2, "mm")
    )
} else {
  p2b <- ggplot() + theme_void(base_family = MS_FONT) +
    annotate("text", x = 0, y = 0, label = "b  Independent contextual information\nNo paired grouped-CV model results",
             size = 2.2, colour = "#55595C")
}

# c. Exposure-state dependence of distortion direction.
metric_direction_shift <- transition_state |>
  filter(is.finite(delta_direction_HL)) |>
  group_by(dimension, metric, metric_class) |>
  summarise(delta_direction_HL = median(delta_direction_HL, na.rm = TRUE), .groups = "drop") |>
  mutate(metric_class = factor(metric_class, levels = METRIC_CLASSES))

direction_shift_summary <- metric_direction_shift |>
  group_by(dimension, metric_class) |>
  summarise(
    n_metrics = n_distinct(metric),
    shift_median = median(delta_direction_HL, na.rm = TRUE),
    shift_q25 = quantile(delta_direction_HL, .25, na.rm = TRUE, names = FALSE),
    shift_q75 = quantile(delta_direction_HL, .75, na.rm = TRUE, names = FALSE),
    .groups = "drop"
  )

dir_limit <- max(abs(metric_direction_shift$delta_direction_HL), na.rm = TRUE)
if (!is.finite(dir_limit) || dir_limit <= 0) dir_limit <- 1

p2c <- ggplot(metric_direction_shift,
              aes(delta_direction_HL, metric_class, color = metric_class)) +
  geom_vline(xintercept = 0, linewidth = .30, color = "#9DA2A5") +
  geom_point(position = position_jitter(width = 0, height = .09, seed = 52),
             size = .68, alpha = .26) +
  geom_segment(
    data = direction_shift_summary,
    aes(x = shift_q25, xend = shift_q75, y = metric_class, yend = metric_class,
        color = metric_class),
    inherit.aes = FALSE, linewidth = 1.0, alpha = .46, lineend = "round"
  ) +
  geom_point(
    data = direction_shift_summary,
    aes(shift_median, metric_class, color = metric_class),
    inherit.aes = FALSE, shape = 18, size = 1.85
  ) +
  facet_wrap(~factor(dimension, levels = DIMENSIONS, labels = unname(DIM_TITLES[DIMENSIONS])), ncol = 2) +
  scale_color_ms_metric(guide = "none") +
  scale_x_continuous(limits = c(-dir_limit * 1.03, dir_limit * 1.03),
                     breaks = scales::breaks_extended(n = 4)) +
  labs(title = "c  Conditional shift in distortion direction",
       x = "Δ(B/A), High − Low", y = NULL) +
  theme_rq2(base_size = 6.4) +
  theme(
    panel.grid.major.y = element_blank(), axis.line.y = element_blank(), axis.ticks.y = element_blank(),
    axis.text.y = element_text(size = 5.1), strip.text = element_text(size = 5.8),
    panel.spacing = grid::unit(2.2, "mm")
  )

# Preserve the existing asymmetric Fig. 2 composition.
p2bottom <- cowplot::plot_grid(p2b, p2c, ncol = 2, rel_widths = c(.54, .46),
                               align = "hv", axis = "tblr", greedy = TRUE)
p2body <- cowplot::plot_grid(p2a, p2bottom, ncol = 1, rel_heights = c(.92, 1.08),
                             align = "v", axis = "l", greedy = TRUE)
p2 <- cowplot::plot_grid(metric_legend, p2body, ncol = 1, rel_heights = c(.045, 1),
                         align = "v", greedy = TRUE)
ms_plot_save(p2, file.path(OUT_DIR, "Fig2_RQ2.pdf"), 9.0, 6.2)
ms_plot_save(p2, file.path(OUT_DIR, "Fig2_RQ2.png"), 9.0, 6.2)

readr::write_csv(conditional_profile_summary |>
  mutate(metric_class = as.character(metric_class), state_bin_label = as.character(state_bin_label)),
  file.path("results", "rq2", "fig2_conditional_profile.csv"), na = "")
readr::write_csv(context_task |>
  mutate(metric_class = as.character(metric_class), information = as.character(information)),
  file.path("results", "rq2", "fig2_context_increment.csv"), na = "")
readr::write_csv(transition_spread |>
  mutate(dimension = as.character(dimension), transition_key = as.character(transition_key)),
  file.path("results", "rq2", "fig2_transition_spread.csv"), na = "")
readr::write_csv(direction_shift_summary |>
  mutate(metric_class = as.character(metric_class)),
  file.path("results", "rq2", "fig2_direction_shift.csv"), na = "")

# Complete conditional atlas retained as supplementary audit view.
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

# Former main-text transition-spread panel retained as a compact supplement.
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

# =============================================================================
# Fig. 3 — cross-dimensional dependence
# =============================================================================

format_gamma_transition <- function(x) {
  x |>
    str_replace_all("_LIGHT_to_MEDI", " · LIGHT → MEDI") |>
    str_replace_all("([0-9]+)to([0-9]+)", "\\1 → \\2") |>
    str_replace_all("_", " · ")
}

gamma_plot <- gamma_summary |>
  mutate(
    dimension_pair = case_when(
      dimension_a == "placement" & dimension_b == "optical" ~ "Placement × optical",
      dimension_a == "placement" & dimension_b == "temporal" ~ "Placement × temporal",
      dimension_a == "optical" & dimension_b == "temporal" ~ "Optical × temporal",
      TRUE ~ paste(dimension_a, "×", dimension_b)
    ),
    metric = as.character(metric),
    metric_class = factor(metric_class, levels = METRIC_CLASSES),
    transition_display = format_gamma_transition(transition),
    Q = abs(Q)
  )

PAIR_LEVELS <- c("Placement × optical", "Optical × temporal", "Placement × temporal")
gamma_plot <- gamma_plot |>
  mutate(dimension_pair = factor(dimension_pair, levels = PAIR_LEVELS))

gamma_metric <- gamma_plot |>
  filter(is.finite(R), is.finite(Q)) |>
  group_by(dimension_pair, metric, metric_class) |>
  summarise(R_metric = median(R, na.rm = TRUE), Q_metric = median(Q, na.rm = TRUE), .groups = "drop")

gamma_r_summary <- gamma_metric |>
  group_by(dimension_pair, metric_class) |>
  summarise(
    n_metrics = n_distinct(metric),
    R_median = median(R_metric, na.rm = TRUE),
    R_q25 = quantile(R_metric, .25, na.rm = TRUE, names = FALSE),
    R_q75 = quantile(R_metric, .75, na.rm = TRUE, names = FALSE),
    .groups = "drop"
  )

gamma_q_summary <- gamma_metric |>
  group_by(dimension_pair, metric_class) |>
  summarise(
    n_metrics = n_distinct(metric),
    Q_median = median(Q_metric, na.rm = TRUE),
    Q_q25 = quantile(Q_metric, .25, na.rm = TRUE, names = FALSE),
    Q_q75 = quantile(Q_metric, .75, na.rm = TRUE, names = FALSE),
    .groups = "drop"
  )

r_limit <- max(abs(gamma_metric$R_metric), na.rm = TRUE)
if (!is.finite(r_limit) || r_limit <= 0) r_limit <- 1
q_limit <- max(gamma_metric$Q_metric, na.rm = TRUE)
if (!is.finite(q_limit) || q_limit <= 0) q_limit <- 1

p3a <- ggplot(gamma_metric, aes(R_metric, metric_class, color = metric_class)) +
  geom_vline(xintercept = 0, linewidth = .30, color = "#969B9E") +
  geom_point(position = position_jitter(width = 0, height = .09, seed = 71), size = .70, alpha = .28) +
  geom_segment(
    data = gamma_r_summary,
    aes(x = R_q25, xend = R_q75, y = metric_class, yend = metric_class, color = metric_class),
    inherit.aes = FALSE, linewidth = 1.05, alpha = .46, lineend = "round"
  ) +
  geom_point(data = gamma_r_summary, aes(R_median, metric_class, color = metric_class),
             inherit.aes = FALSE, shape = 18, size = 1.9) +
  facet_grid(. ~ dimension_pair) +
  scale_color_ms_metric(guide = "none") +
  scale_x_continuous(limits = c(-r_limit * 1.04, r_limit * 1.04), breaks = scales::breaks_extended(n = 5)) +
  labs(title = "a  Signed cross-dimensional interaction",
       x = "R = median signed γ across local transitions", y = NULL) +
  theme_rq2(base_size = 6.5) +
  theme(panel.grid.major.y = element_blank(), axis.line.y = element_blank(), axis.ticks.y = element_blank(),
        axis.text.y = element_text(size = 5.4), strip.text = element_text(size = 6.0),
        panel.spacing.x = grid::unit(2.3, "mm"))

p3b <- ggplot(gamma_metric, aes(Q_metric, metric_class, color = metric_class)) +
  geom_point(position = position_jitter(width = 0, height = .09, seed = 73), size = .70, alpha = .28) +
  geom_segment(
    data = gamma_q_summary,
    aes(x = Q_q25, xend = Q_q75, y = metric_class, yend = metric_class, color = metric_class),
    inherit.aes = FALSE, linewidth = 1.05, alpha = .46, lineend = "round"
  ) +
  geom_point(data = gamma_q_summary, aes(Q_median, metric_class, color = metric_class),
             inherit.aes = FALSE, shape = 18, size = 1.9) +
  facet_grid(. ~ dimension_pair) +
  scale_color_ms_metric(guide = "none") +
  scale_x_continuous(limits = c(0, q_limit * 1.04), breaks = scales::breaks_extended(n = 5)) +
  labs(title = "b  Magnitude of cross-dimensional interaction",
       x = "Q = median |γ| across local transitions", y = NULL) +
  theme_rq2(base_size = 6.5) +
  theme(panel.grid.major.y = element_blank(), axis.line.y = element_blank(), axis.ticks.y = element_blank(),
        axis.text.y = element_text(size = 5.4), strip.text = element_text(size = 6.0),
        panel.spacing.x = grid::unit(2.3, "mm"))

gamma_transition <- gamma_plot |>
  filter(is.finite(R), is.finite(Q)) |>
  group_by(dimension_pair, transition_display) |>
  summarise(
    n_metrics = n_distinct(metric),
    Q_median = median(Q, na.rm = TRUE),
    Q_q25 = quantile(Q, .25, na.rm = TRUE, names = FALSE),
    Q_q75 = quantile(Q, .75, na.rm = TRUE, names = FALSE),
    R_median = median(R, na.rm = TRUE),
    .groups = "drop"
  ) |>
  group_by(dimension_pair) |>
  slice_max(Q_median, n = 4, with_ties = FALSE) |>
  ungroup() |>
  mutate(
    transition_key = paste(as.character(dimension_pair), transition_display, sep = "|||"),
    transition_key = forcats::fct_reorder(transition_key, Q_median)
  )

fill_limit <- max(abs(gamma_transition$R_median), na.rm = TRUE)
if (!is.finite(fill_limit) || fill_limit <= 0) fill_limit <- 1e-6

p3c <- ggplot(gamma_transition, aes(Q_median, transition_key)) +
  geom_segment(aes(x = Q_q25, xend = Q_q75, yend = transition_key),
               color = "#A8ADB0", linewidth = .90, alpha = .62, lineend = "round") +
  geom_point(aes(fill = R_median), shape = 21, size = 2.15, color = "#3E4245", stroke = .22) +
  facet_grid(. ~ dimension_pair, scales = "free_y", space = "free_x") +
  scale_y_discrete(labels = function(x) stringr::str_wrap(sub("^.*\\|\\|\\|", "", x), width = 24)) +
  scale_x_continuous(breaks = scales::breaks_extended(n = 4)) +
  scale_fill_ms_diverging(fill_limit, name = "median R") +
  labs(title = "c  Localization of the strongest cross-dimensional interactions",
       x = "median Q across metrics", y = NULL) +
  theme_rq2(base_size = 6.3, legend_position = "bottom") +
  theme(panel.grid.major.y = element_blank(), axis.line.y = element_blank(), axis.ticks.y = element_blank(),
        axis.text.y = element_text(size = 4.9), strip.text = element_text(size = 5.8),
        legend.title = element_text(size = 5.3), legend.text = element_text(size = 5.0),
        panel.spacing.x = grid::unit(2.4, "mm"))

p3body <- cowplot::plot_grid(p3a, p3b, p3c, ncol = 1, rel_heights = c(.82, .82, 1.0),
                             align = "v", axis = "l", greedy = TRUE)
p3 <- cowplot::plot_grid(metric_legend, p3body, ncol = 1, rel_heights = c(.045, 1),
                         align = "v", greedy = TRUE)
ms_plot_save(p3, file.path(OUT_DIR, "Fig3_RQ2.pdf"), 9.0, 6.3)
ms_plot_save(p3, file.path(OUT_DIR, "Fig3_RQ2.png"), 9.0, 6.3)

readr::write_csv(gamma_r_summary |>
  mutate(metric_class = as.character(metric_class), dimension_pair = as.character(dimension_pair)),
  file.path("results", "rq2", "fig3_signed_interaction_summary.csv"), na = "")
readr::write_csv(gamma_q_summary |>
  mutate(metric_class = as.character(metric_class), dimension_pair = as.character(dimension_pair)),
  file.path("results", "rq2", "fig3_interaction_magnitude_summary.csv"), na = "")
readr::write_csv(gamma_transition |>
  mutate(dimension_pair = as.character(dimension_pair), transition_key = as.character(transition_key)),
  file.path("results", "rq2", "fig3_top_interaction_transitions.csv"), na = "")

# Complete gamma atlas retained as supplementary audit view.
gamma_atlas <- gamma_plot |>
  mutate(
    dimension = as.character(dimension_a),
    transition_display = factor(transition_display, levels = unique(transition_display))
  ) |>
  ms_add_metric_order(metric_order)
gamma_limit <- max(abs(c(gamma_atlas$R, gamma_atlas$Q)), na.rm = TRUE)
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

# =============================================================================
# Supplementary model validation diagnostics
# =============================================================================
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
      "FigS_RQ2_gamma_atlas", "FigS_RQ2_model_performance"
    ),
    input_artifact = c(
      "rq2_conditional_geometry+rq2_model_performance",
      "rq2_gamma_summary",
      "rq2_conditional_geometry",
      "rq2_conditional_geometry",
      "rq2_gamma_summary",
      "rq2_model_performance"
    ),
    core_artifact_version = CORE_VERSION,
    rq1_analysis_version = RQ1_VERSION,
    rq2_analysis_version = RQ2_VERSION,
    rq3_analysis_version = NA_character_
  ))
message("RQ2 v5 figures complete: Fig. 2 integrates exposure-state and external-context dependence; Fig. 3 retains cross-dimensional interaction geometry.")
