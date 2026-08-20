suppressPackageStartupMessages({
  library(tidyverse)
  library(cowplot)
})
source("scripts/utils/figure_style.R")
source("scripts/utils/figure_atlas.R")

# RQ2 plotting only. Fig. 2 covers all three conditionality axes in the research
# question: reference exposure state, real-world context, and external-context
# predictability/transportability. Fig. 3 carries cross-dimensional separability
# at full metric x joint-configuration resolution.
RQ1_SUMMARY_CSV <- "results/rq1/rq1_summary.csv"
COND_RDS <- "data/derived/rq2/rq2_condition_long.rds"
GAMMA_RDS <- "data/derived/rq2/rq2_gamma_long.rds"
COND_GEOM_CSV <- "results/rq2/rq2_conditional_geometry.csv"
ANCHOR_CSV <- "results/rq2/rq2_anchor_configurations.csv"
COND_EXAMPLE_CSV <- "results/rq2/rq2_conditional_examples.csv"
MODEL_PERF_CSV <- "results/rq2/rq2_model_performance.csv"
GAMMA_SUMMARY_CSV <- "results/rq2/rq2_gamma_summary.csv"
GAMMA_PAIR_CSV <- "results/rq2/rq2_gamma_pair_summary.csv"
GAMMA_EXAMPLE_CSV <- "results/rq2/rq2_gamma_examples.csv"
SCOPE_CSV <- "results/rq2/rq2_interaction_scope.csv"
CTX_GEOM_CSV <- "results/rq2/rq2_context_conditional_geometry.csv"
CTX_CONTRAST_CSV <- "results/rq2/rq2_context_binary_contrasts.csv"
CTX_MANIFEST_CSV <- "results/rq2/rq2_context_manifest.csv"
FIG_DIR <- "results/figures"

reqfiles <- c(
  RQ1_SUMMARY_CSV, COND_RDS, GAMMA_RDS, COND_GEOM_CSV, ANCHOR_CSV,
  COND_EXAMPLE_CSV, MODEL_PERF_CSV, GAMMA_SUMMARY_CSV, GAMMA_PAIR_CSV,
  GAMMA_EXAMPLE_CSV, SCOPE_CSV, CTX_GEOM_CSV, CTX_CONTRAST_CSV, CTX_MANIFEST_CSV
)
for (p in reqfiles) {
  if (!file.exists(p)) {
    stop(
      "Missing RQ2 plotting artifact: ", p,
      ". Run scripts/10b_rq1_context_analysis.R, scripts/12_rq2_analysis.R, and scripts/12b_rq2_context_analysis.R before plotting."
    )
  }
}
dir.create(FIG_DIR, recursive = TRUE, showWarnings = FALSE)

METRIC_CLASSES <- MS_METRIC_CLASSES
DIMENSIONS <- c("placement", "optical", "temporal", "duration")
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

rq1_summary <- readr::read_csv(RQ1_SUMMARY_CSV, show_col_types = FALSE)
metric_order <- ms_metric_order(rq1_summary)
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
scope <- readr::read_csv(SCOPE_CSV, show_col_types = FALSE)
ctx <- readr::read_csv(CTX_GEOM_CSV, show_col_types = FALSE)
ctx_contrast <- readr::read_csv(CTX_CONTRAST_CSV, show_col_types = FALSE)
ctx_manifest <- readr::read_csv(CTX_MANIFEST_CSV, show_col_types = FALSE)
ctx_states <- ms_context_state_table()

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

# =============================================================================
# Fig. 2 | Conditional structure and transportability
# =============================================================================

# a. Representative conditional D(e | reference exposure state).
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
    title = "a  Exposure-state conditional distributions",
    x = "standardized signed distortion, e", y = "density",
    color = "reference state", fill = "reference state"
  ) +
  theme_ms(base_size = 7.4, aspect_ratio = 1, legend_position = "bottom") +
  theme(strip.text = element_text(size = 6.4), legend.text = element_text(size = 6.5))

# b-e. All-metric A(X)-B(X) geometry for the predeclared RQ2 anchor configurations.
conditional_panel_data <- function(dim) {
  finite_all <- cg |>
    filter(dimension == dim, is.finite(A_conditional), is.finite(B_conditional))
  anchored <- finite_all |>
    semi_join(anchors |> filter(dimension == dim), by = c("dimension", "configuration"))

  if (nrow(anchored) || dim != "duration" || !nrow(finite_all)) return(anchored)

  # Duration can be population-sparse even when another observed duration level
  # has finite conditional geometry. The fallback is explicit and plot-only.
  fallback <- finite_all |>
    count(configuration, configuration_label, configuration_order, name = "n_finite_cells") |>
    arrange(desc(n_finite_cells), desc(configuration_order)) |>
    slice_head(n = 1)
  message(
    "Fig. 2 duration anchor has no finite conditional geometry; using ",
    fallback$configuration_label[[1]], " (", fallback$n_finite_cells[[1]], " finite cells)."
  )
  finite_all |>
    semi_join(fallback, by = c("configuration", "configuration_label", "configuration_order"))
}

conditional_ab_panel <- function(dim, letter) {
  d <- conditional_panel_data(dim) |>
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
p2d <- conditional_ab_panel("duration", "d")
p2e <- conditional_ab_panel("temporal", "e")

# Shared anchor table for the real-world-context panels. Placement intentionally
# retains chest and wrist as separate anchor facets; ordered dimensions retain
# their predeclared most-degraded primary anchor from 12_rq2_analysis.R.
anchor_table <- anchors |>
  mutate(
    dimension = factor(dimension, levels = DIMENSIONS),
    anchor_label = case_when(
      dimension == "placement" ~ paste0("Placement\n", configuration_label),
      dimension == "optical" ~ paste0("Optical\n", configuration_label),
      dimension == "temporal" ~ paste0("Temporal\n", configuration_label),
      dimension == "duration" ~ paste0("Duration\n", configuration_label),
      TRUE ~ paste(dimension, configuration_label)
    )
  ) |>
  arrange(dimension, configuration_order)
anchor_levels <- unique(anchor_table$anchor_label)
anchor_index <- anchor_table |>
  transmute(
    anchor_row = row_number(), dimension = as.character(dimension),
    configuration, anchor_label
  ) |>
  distinct()
context_index <- ctx_states |>
  transmute(
    context_row = row_number(), context_family, context_state,
    context_state_label, context_order
  )

operator_valid <- ctx_manifest |>
  distinct(context_family, context_state, metric) |>
  mutate(operator_valid = TRUE)

# f. State-specific real-world context geometry.
# Orthogonal visual grammar:
#   y = target representation; x = context state; facet = measurement anchor;
#   bubble area = A(context); bubble fill = B(context)/A(context).
# Grey = operator-valid support; white = operator-invalid context restriction;
# x = operator-valid but no finite estimate on that anchor/support.
ctx_anchor <- ctx |>
  semi_join(anchor_table, by = c("dimension", "configuration")) |>
  left_join(ctx_states, by = c("context_family", "context_state")) |>
  left_join(anchor_table |> select(dimension, configuration, anchor_label), by = c("dimension", "configuration")) |>
  filter(is.finite(A_conditional), is.finite(B_conditional), !is.na(context_state_label)) |>
  mutate(
    anchor_label = factor(anchor_label, levels = anchor_levels),
    context_state_label = factor(context_state_label, levels = ctx_states$context_state_label),
    direction_ratio = ms_direction_ratio(B_conditional, A_conditional)
  ) |>
  ms_add_metric_order(metric_order)

ctx_key <- ctx_anchor |>
  transmute(
    dimension = as.character(dimension), configuration, anchor_label = as.character(anchor_label),
    context_family, context_state, metric = as.character(metric), estimated = TRUE
  ) |>
  distinct()

ctx_grid <- tidyr::crossing(
  metric = metric_order$metric,
  anchor_row = anchor_index$anchor_row,
  context_row = context_index$context_row
) |>
  left_join(anchor_index, by = "anchor_row") |>
  left_join(context_index, by = "context_row") |>
  select(-anchor_row, -context_row) |>
  left_join(operator_valid, by = c("context_family", "context_state", "metric")) |>
  left_join(ctx_key, by = c("dimension", "configuration", "anchor_label", "context_family", "context_state", "metric")) |>
  mutate(
    operator_valid = replace_na(operator_valid, FALSE),
    estimated = replace_na(estimated, FALSE),
    anchor_label = factor(anchor_label, levels = anchor_levels),
    context_state_label = factor(context_state_label, levels = ctx_states$context_state_label)
  ) |>
  ms_add_metric_order(metric_order)

p2f <- ggplot(ctx_anchor, aes(context_state_label, metric)) +
  geom_tile(
    data = ctx_grid |> filter(operator_valid),
    fill = "#F2F2F2", color = "white", linewidth = .10
  ) +
  geom_point(
    data = ctx_grid |> filter(operator_valid, !estimated),
    shape = 4, size = .48, stroke = .22, color = "#B5B5B5"
  ) +
  geom_point(
    aes(size = A_conditional, fill = direction_ratio),
    shape = 21, color = "#393939", stroke = .14, alpha = .94
  ) +
  facet_grid(metric_class ~ anchor_label, scales = "free_y", space = "free_y", switch = "y") +
  ms_direction_scale(name = "conditional B / A") +
  ms_magnitude_size_scale(name = "conditional A", range = c(.22, 2.65)) +
  labs(
    title = "f  Context-specific distortion geometry",
    x = "civil photoperiod · diary environment · diary activity", y = NULL
  ) +
  ms_atlas_theme(base_size = 5.9, x_angle = 55) +
  guides(
    size = guide_legend(order = 1, title.position = "top"),
    fill = guide_colorbar(order = 2, title.position = "top", barwidth = grid::unit(28, "mm"))
  )

# g. Paired contextual shifts for the two naturally paired binary contexts.
# A and B have the same standardized-distortion units, so ΔA and ΔB share one
# diverging fill scale. A small black dot means the site-stratified participant
# bootstrap interval excludes zero; it is an uncertainty cue, not a new estimand.
binary_labels <- tibble::tribble(
  ~context_family, ~contrast_label, ~contrast_order,
  "photoperiod", "Day → Night", 1L,
  "environment", "Indoor → Outdoor", 2L
)
contrast_anchor <- ctx_contrast |>
  semi_join(anchor_table, by = c("dimension", "configuration")) |>
  left_join(anchor_table |> select(dimension, configuration, anchor_label), by = c("dimension", "configuration")) |>
  left_join(binary_labels, by = "context_family") |>
  filter(!is.na(contrast_label))

contrast_long <- bind_rows(
  contrast_anchor |>
    transmute(
      dimension, configuration, anchor_label, metric, metric_class, context_family, contrast_label, contrast_order,
      quantity = "ΔA", shift = mean_delta_absolute,
      ci_low = absolute_ci_low, ci_high = absolute_ci_high, bootstrap_supported
    ),
  contrast_anchor |>
    transmute(
      dimension, configuration, anchor_label, metric, metric_class, context_family, contrast_label, contrast_order,
      quantity = "ΔB", shift = mean_delta_signed,
      ci_low = signed_ci_low, ci_high = signed_ci_high, bootstrap_supported
    )
) |>
  filter(is.finite(shift)) |>
  mutate(
    contrast_col = paste0(contrast_label, "\n", quantity),
    contrast_col = factor(
      contrast_col,
      levels = c("Day → Night\nΔA", "Day → Night\nΔB", "Indoor → Outdoor\nΔA", "Indoor → Outdoor\nΔB")
    ),
    anchor_label = factor(anchor_label, levels = anchor_levels),
    ci_excludes_zero = replace_na(bootstrap_supported, FALSE) & is.finite(ci_low) & is.finite(ci_high) & (ci_low > 0 | ci_high < 0)
  ) |>
  ms_add_metric_order(metric_order)

binary_valid <- ctx_manifest |>
  filter(context_family %in% binary_labels$context_family) |>
  distinct(context_family, context_state, metric) |>
  count(context_family, metric, name = "n_valid_states") |>
  mutate(operator_valid = n_valid_states >= 2L) |>
  select(context_family, metric, operator_valid)

contrast_cols <- tidyr::crossing(
  binary_labels,
  quantity = c("ΔA", "ΔB")
) |>
  arrange(contrast_order, match(quantity, c("ΔA", "ΔB"))) |>
  mutate(
    contrast_row = row_number(),
    contrast_col = factor(
      paste0(contrast_label, "\n", quantity),
      levels = c("Day → Night\nΔA", "Day → Night\nΔB", "Indoor → Outdoor\nΔA", "Indoor → Outdoor\nΔB")
    )
  )
contrast_key <- contrast_long |>
  transmute(
    dimension = as.character(dimension), configuration, anchor_label = as.character(anchor_label),
    context_family, metric = as.character(metric), quantity, estimated = TRUE
  ) |>
  distinct()
contrast_grid <- tidyr::crossing(
  metric = metric_order$metric,
  anchor_row = anchor_index$anchor_row,
  contrast_row = contrast_cols$contrast_row
) |>
  left_join(anchor_index, by = "anchor_row") |>
  left_join(contrast_cols |> select(contrast_row, context_family, quantity, contrast_col), by = "contrast_row") |>
  select(-anchor_row, -contrast_row) |>
  left_join(binary_valid, by = c("context_family", "metric")) |>
  left_join(contrast_key, by = c("dimension", "configuration", "anchor_label", "context_family", "metric", "quantity")) |>
  mutate(
    operator_valid = replace_na(operator_valid, FALSE),
    estimated = replace_na(estimated, FALSE),
    anchor_label = factor(anchor_label, levels = anchor_levels)
  ) |>
  ms_add_metric_order(metric_order)

contrast_lim <- max(abs(contrast_long$shift), na.rm = TRUE)
if (!is.finite(contrast_lim) || contrast_lim <= 0) contrast_lim <- 1

p2g <- ggplot(contrast_long, aes(contrast_col, metric)) +
  geom_tile(
    data = contrast_grid |> filter(operator_valid),
    fill = "#F2F2F2", color = "white", linewidth = .10
  ) +
  geom_point(
    data = contrast_grid |> filter(operator_valid, !estimated),
    shape = 4, size = .44, stroke = .20, color = "#B5B5B5"
  ) +
  geom_tile(aes(fill = shift), color = "white", linewidth = .10) +
  geom_point(
    data = contrast_long |> filter(ci_excludes_zero),
    shape = 16, size = .42, color = "#111111"
  ) +
  facet_grid(metric_class ~ anchor_label, scales = "free_y", space = "free_y", switch = "y") +
  scale_fill_ms_diverging(
    contrast_lim,
    transform = asinh_display,
    name = "mean paired shift\n(standardized e)"
  ) +
  labs(title = "g  Paired contextual shifts", x = "paired context and geometry component", y = NULL) +
  ms_atlas_theme(base_size = 5.9, x_angle = 55) +
  guides(fill = guide_colorbar(title.position = "top", barwidth = grid::unit(28, "mm")))

readr::write_csv(
  ctx_anchor |>
    mutate(metric = as.character(metric), metric_class = as.character(metric_class), anchor_label = as.character(anchor_label)),
  "results/rq2/fig2_context_anchor_atlas.csv", na = ""
)
readr::write_csv(
  contrast_long |>
    mutate(metric = as.character(metric), metric_class = as.character(metric_class), anchor_label = as.character(anchor_label)),
  "results/rq2/fig2_context_binary_shift_atlas.csv", na = ""
)

# h. External/exposure/joint predictability and leave-site-out transportability.
perfwide <- perf |>
  filter(is.finite(r2), validation_scheme %in% c("participant_grouped", "leave_site_out")) |>
  select(dimension, configuration, metric, metric_class, outcome, model_family, model_label, validation_scheme, r2) |>
  pivot_wider(names_from = validation_scheme, values_from = r2) |>
  filter(is.finite(participant_grouped), is.finite(leave_site_out))

if (nrow(perfwide)) {
  lims <- range(c(perfwide$participant_grouped, perfwide$leave_site_out), finite = TRUE)
  pad <- .05 * diff(lims); if (!is.finite(pad) || pad == 0) pad <- .1
  lims <- c(lims[1] - pad, lims[2] + pad)
  perf_summary <- perfwide |>
    group_by(outcome, model_label) |>
    summarise(
      x = median(participant_grouped), x25 = quantile(participant_grouped, .25), x75 = quantile(participant_grouped, .75),
      y = median(leave_site_out), y25 = quantile(leave_site_out, .25), y75 = quantile(leave_site_out, .75),
      .groups = "drop"
    )

  p2h <- ggplot(perfwide, aes(participant_grouped, leave_site_out, color = model_label, shape = model_label)) +
    geom_abline(slope = 1, intercept = 0, linetype = 2, linewidth = .35, color = "#777777") +
    geom_point(size = .90, alpha = .34) +
    geom_segment(
      data = perf_summary, aes(x = x25, xend = x75, y = y, yend = y, color = model_label),
      inherit.aes = FALSE, linewidth = .75
    ) +
    geom_segment(
      data = perf_summary, aes(x = x, xend = x, y = y25, yend = y75, color = model_label),
      inherit.aes = FALSE, linewidth = .75
    ) +
    geom_point(
      data = perf_summary, aes(x = x, y = y, color = model_label, shape = model_label),
      inherit.aes = FALSE, size = 2.35, stroke = .8
    ) +
    facet_wrap(~outcome, nrow = 1) +
    scale_color_manual(values = MODEL_COLORS, drop = FALSE) +
    scale_shape_manual(values = c(External = 16, `Exposure state` = 17, Joint = 15)) +
    coord_equal(xlim = lims, ylim = lims) +
    labs(
      title = "h  Predictability and cross-site transportability",
      x = "grouped-participant CV R²", y = "leave-site-out R²", color = NULL, shape = NULL
    ) +
    theme_ms(base_size = 7.2, aspect_ratio = .52, legend_position = "bottom") +
    theme(legend.text = element_text(size = 6.5))
} else {
  p2h <- ggplot() + annotate("text", x = 0, y = 0, label = "Prediction models not estimable", size = 3) +
    xlim(-1, 1) + ylim(-1, 1) + labs(title = "h  Predictability and cross-site transportability") + base_square_theme
}

state_geometry <- plot_grid(p2b, p2c, p2d, p2e, ncol = 2, align = "hv", axis = "tblr")
fig2top <- plot_grid(p2a, state_geometry, nrow = 1, rel_widths = c(.36, .64), align = "hv", axis = "tblr")
context_block <- plot_grid(p2f, p2g, nrow = 1, rel_widths = c(.66, .34), align = "v", axis = "lr")
fig2body <- plot_grid(fig2top, context_block, p2h, ncol = 1, rel_heights = c(1.0, 2.55, .86))
fig2 <- plot_grid(fig2body, metric_legend, ncol = 1, rel_heights = c(1, .035))
ggsave(file.path(FIG_DIR, "Fig2_RQ2.pdf"), fig2, width = 15.6, height = 14.2, useDingbats = FALSE, bg = "white")
ggsave(file.path(FIG_DIR, "Fig2_RQ2.png"), fig2, width = 15.6, height = 14.2, dpi = 240, bg = "white")

# Full all-configuration context hypercube slices are exported by measurement
# dimension as supplementary zoomable atlases. This preserves completeness
# without forcing all configurations x all states into the main panel.
make_context_hypercube <- function(dim) {
  cfg <- ctx |>
    filter(dimension == dim) |>
    distinct(dimension, configuration, configuration_label, configuration_order) |>
    arrange(configuration_order) |>
    mutate(configuration_label = factor(configuration_label, levels = unique(configuration_label)))
  d <- ctx |>
    filter(dimension == dim) |>
    left_join(ctx_states, by = c("context_family", "context_state")) |>
    filter(is.finite(A_conditional), is.finite(B_conditional), !is.na(context_state_label)) |>
    mutate(
      configuration_label = factor(configuration_label, levels = levels(cfg$configuration_label)),
      context_state_label = factor(context_state_label, levels = ctx_states$context_state_label),
      direction_ratio = ms_direction_ratio(B_conditional, A_conditional)
    ) |>
    ms_add_metric_order(metric_order)

  cfg_index <- cfg |>
    transmute(cfg_row = row_number(), dimension, configuration, configuration_label)
  state_index <- ctx_states |>
    transmute(context_row = row_number(), context_family, context_state, context_state_label)
  grid <- tidyr::crossing(
    metric = metric_order$metric,
    cfg_row = cfg_index$cfg_row,
    context_row = state_index$context_row
  ) |>
    left_join(cfg_index, by = "cfg_row") |>
    left_join(state_index, by = "context_row") |>
    select(-cfg_row, -context_row) |>
    left_join(operator_valid, by = c("context_family", "context_state", "metric")) |>
    mutate(
      operator_valid = replace_na(operator_valid, FALSE),
      context_state_label = factor(context_state_label, levels = ctx_states$context_state_label)
    ) |>
    ms_add_metric_order(metric_order)

  ggplot(d, aes(context_state_label, metric)) +
    geom_tile(data = grid |> filter(operator_valid), fill = "#F2F2F2", color = "white", linewidth = .10) +
    geom_point(aes(size = A_conditional, fill = direction_ratio), shape = 21, color = "#393939", stroke = .12, alpha = .94) +
    facet_grid(metric_class ~ configuration_label, scales = "free_y", space = "free_y", switch = "y") +
    ms_direction_scale(name = "conditional B / A") +
    ms_magnitude_size_scale(name = "conditional A", range = c(.20, 2.55)) +
    labs(title = paste0("Real-world context hypercube: ", DIM_TITLES[[dim]]), x = NULL, y = NULL) +
    ms_atlas_theme(base_size = 5.8, x_angle = 55)
}

walk(DIMENSIONS, function(dim) {
  ps <- make_context_hypercube(dim)
  w <- if (dim %in% c("temporal", "duration")) 18 else 10
  ggsave(file.path(FIG_DIR, paste0("FigS_RQ2_context_", dim, ".pdf")), ps, width = w, height = 11.5, bg = "white")
  ggsave(file.path(FIG_DIR, paste0("FigS_RQ2_context_", dim, ".png")), ps, width = w, height = 11.5, dpi = 240, bg = "white")
})

# =============================================================================
# Fig. 3 | Cross-dimensional separability
# =============================================================================

# a. Full metric x joint-configuration interaction atlas.
gs_atlas <- gs |>
  filter(is.finite(Q_mean_absolute), is.finite(R_mean_signed)) |>
  mutate(
    interaction_label = paste(a_configuration_label, b_configuration_label, sep = " × "),
    direction_ratio = ms_direction_ratio(R_mean_signed, Q_mean_absolute)
  ) |>
  ms_add_metric_order(metric_order)

p3a <- ggplot(gs_atlas, aes(interaction_label, metric)) +
  geom_tile(fill = "#F3F3F3", color = "white", linewidth = .10) +
  geom_point(
    aes(size = Q_mean_absolute, fill = direction_ratio),
    shape = 21, color = "#3A3A3A", stroke = .14, alpha = .94
  ) +
  facet_grid(metric_class ~ dimension_pair, scales = "free", space = "free", switch = "y") +
  ms_direction_scale(name = "R / Q") +
  ms_magnitude_size_scale(name = "Q = mean |γ|", range = c(.22, 2.85)) +
  labs(
    title = "a  Cross-dimensional interaction atlas",
    x = "observed joint configuration", y = NULL
  ) +
  ms_atlas_theme(base_size = 6.0, x_angle = 50)

# b. Representative empirical D(gamma).
gids <- ge |> transmute(id = paste(dimension_pair, a_configuration, b_configuration, metric, sep = " | "), example_type)
ged <- gamma |>
  filter(available, is.finite(gamma)) |>
  mutate(id = paste(dimension_pair, a_configuration, b_configuration, metric, sep = " | ")) |>
  inner_join(gids, by = "id")
p3b <- ggplot(ged, aes(gamma)) +
  geom_density(fill = MS_PRIMARY, color = MS_PRIMARY, alpha = .18, linewidth = .48, adjust = .85) +
  geom_vline(xintercept = 0, linewidth = .28, color = "#707070") +
  facet_wrap(~example_type, scales = "free", ncol = 2) +
  labs(title = "b  Representative empirical D(γ)", x = "standardized interaction distortion, γ", y = "density") +
  theme_ms(base_size = 7.2, aspect_ratio = 1, legend_position = "none") +
  theme(strip.text = element_text(size = 6.2))

# c. R-Q geometry across every estimable metric x joint configuration.
if (nrow(gs)) {
  lim <- max(c(gs$Q_mean_absolute, abs(gs$R_mean_signed)), na.rm = TRUE) * 1.06
  lab <- gs |> slice_max(Q_mean_absolute, n = 5, with_ties = FALSE)
  p3c <- ggplot(gs, aes(R_mean_signed, Q_mean_absolute, color = metric_class)) +
    geom_vline(xintercept = 0, linewidth = .24, color = "#D8D8D8") +
    geom_abline(slope = c(-1, 1), intercept = 0, linetype = 2, linewidth = .3, color = "#8A8A8A") +
    geom_point(aes(shape = dimension_pair), size = 1.05, alpha = .62) +
    geom_text(data = lab, aes(label = metric), size = 1.8, color = "#252525", vjust = -.6, check_overlap = TRUE, show.legend = FALSE) +
    scale_color_ms_metric() +
    scale_x_continuous(transform = asinh_display, limits = c(-lim, lim), expand = expansion(mult = .025)) +
    scale_y_continuous(transform = asinh_display, limits = c(0, lim), expand = expansion(mult = .025)) +
    labs(title = "c  Interaction geometry", x = "R: mean signed γ", y = "Q: mean absolute γ") + base_square_theme
} else {
  p3c <- ggplot() + annotate("text", x = 0, y = 0, label = "No gamma summaries") +
    xlim(-1, 1) + ylim(-1, 1) + labs(title = "c  Interaction geometry") + base_square_theme
}

# d. Pair-level summary preserves total interaction magnitude and directional component.
p3d <- ggplot(gp, aes(y = dimension_pair)) +
  geom_segment(aes(x = q25_Q, xend = q75_Q, yend = dimension_pair), linewidth = .7, color = MS_PRIMARY, alpha = .62) +
  geom_point(aes(x = median_Q), size = 2.25, color = MS_PRIMARY) +
  geom_point(aes(x = median_abs_R), size = 2.0, shape = 1, stroke = .65, color = "#262626") +
  scale_x_continuous(transform = asinh_display, expand = expansion(mult = c(.02, .08))) +
  labs(
    title = "d  Dependence by dimension pair",
    x = "interaction magnitude (filled: median Q; open: median |R|)", y = NULL
  ) +
  theme_ms(base_size = 7.1, aspect_ratio = 1, legend_position = "none") +
  theme(axis.text.y = element_text(size = 7))

# e. Strongest non-separable marginal shifts, expanded beyond one anecdotal example.
top_couplings <- gs |>
  filter(n_participants >= 3, is.finite(Q_mean_absolute)) |>
  arrange(desc(Q_mean_absolute)) |>
  distinct(dimension_pair, a_configuration, b_configuration, metric, .keep_all = TRUE) |>
  slice_head(n = 8) |>
  mutate(
    id = paste(dimension_pair, a_configuration, b_configuration, metric, sep = " | "),
    profile_label = stringr::str_wrap(
      paste0(metric, " | ", dimension_pair, " | ", a_configuration_label, " × ", b_configuration_label),
      width = 28
    )
  )

profile_data <- gamma |>
  filter(available, is.finite(marginal_a_ref), is.finite(marginal_a_at_b)) |>
  mutate(id = paste(dimension_pair, a_configuration, b_configuration, metric, sep = " | ")) |>
  inner_join(top_couplings |> select(id, profile_label), by = "id") |>
  select(profile_label, marginal_a_ref, marginal_a_at_b) |>
  pivot_longer(c(marginal_a_ref, marginal_a_at_b), names_to = "b_state", values_to = "marginal_effect") |>
  mutate(b_state = recode(b_state, marginal_a_ref = "b reference", marginal_a_at_b = "b changed")) |>
  group_by(profile_label, b_state) |>
  summarise(
    mean = mean(marginal_effect), q25 = quantile(marginal_effect, .25), q75 = quantile(marginal_effect, .75),
    .groups = "drop"
  )

if (nrow(profile_data)) {
  p3e <- ggplot(profile_data, aes(b_state, mean, group = profile_label)) +
    geom_hline(yintercept = 0, linewidth = .25, color = "#B0B0B0") +
    geom_line(linewidth = .52, color = MS_PRIMARY) +
    geom_errorbar(aes(ymin = q25, ymax = q75), width = .08, linewidth = .36, color = MS_PRIMARY, alpha = .65) +
    geom_point(size = 1.55, color = MS_PRIMARY) +
    facet_wrap(~profile_label, ncol = 4, scales = "free_y") +
    labs(title = "e  Strongest non-separable marginal shifts", x = NULL, y = "marginal effect of dimension a") +
    theme_ms(base_size = 6.5, legend_position = "none") +
    theme(axis.text.x = element_text(angle = 18, hjust = 1, size = 5.7), strip.text = element_text(size = 5.7))
} else {
  p3e <- ggplot() + annotate("text", x = 0, y = 0, label = "No strong-coupling profiles") +
    xlim(-1, 1) + ylim(-1, 1) + labs(title = "e  Strongest non-separable marginal shifts") + base_square_theme
}

fig3lower <- plot_grid(p3b, p3c, p3d, p3e, ncol = 2, rel_heights = c(1, 1), align = "hv", axis = "tblr")
fig3body <- plot_grid(p3a, fig3lower, ncol = 1, rel_heights = c(2.35, 1.65))
fig3 <- plot_grid(fig3body, metric_legend, ncol = 1, rel_heights = c(1, .035))
ggsave(file.path(FIG_DIR, "Fig3_RQ2.pdf"), fig3, width = 14.4, height = 12.4, useDingbats = FALSE, bg = "white")
ggsave(file.path(FIG_DIR, "Fig3_RQ2.png"), fig3, width = 14.4, height = 12.4, dpi = 240, bg = "white")
message("RQ2 figures complete: exposure-state + real-world-context + external-context conditionality, plus full interaction atlas.")
