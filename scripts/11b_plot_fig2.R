options(encoding = "UTF-8")
if (.Platform$OS.type == "windows") invisible(suppressWarnings(Sys.setlocale("LC_CTYPE", "English_United States.utf8")))
.ms_file <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
if (length(.ms_file)) {
  .ms_script <- normalizePath(sub("^--file=", "", .ms_file[[1]]), winslash = "/", mustWork = TRUE)
  .ms_candidates <- unique(c(file.path(dirname(.ms_script), ".."), getwd()))
  .ms_ok <- vapply(
    .ms_candidates,
    function(x) file.exists(file.path(x, "scripts", "utils", "figure_style.R")),
    logical(1)
  )
  if (!any(.ms_ok)) stop("Could not resolve measurement-sufficiency repository root", call. = FALSE)
  setwd(normalizePath(.ms_candidates[which(.ms_ok)[1]], winslash = "/", mustWork = TRUE))
}
rm(.ms_file)
if (exists(".ms_script")) rm(.ms_script)
if (exists(".ms_candidates")) rm(.ms_candidates)
if (exists(".ms_ok")) rm(.ms_ok)

suppressPackageStartupMessages({
  library(tidyverse)
  library(cowplot)
})
source("scripts/utils/analysis_design.R")
source("scripts/utils/core_artifacts.R")
source("scripts/utils/figure_style.R")
source("scripts/utils/figure_atlas.R")
source("scripts/utils/plot_contracts.R")
source("scripts/utils/artifact_validation.R")
source("scripts/utils/rq1_inference_contract.R")

# Appendix entrypoint stays here so active inference plotting has one owner.
# 16_appendix_figures.R sources this file with MS_FIG2_APPENDIX_ONLY=TRUE,
# then supplies the already-validated frozen artifact; no main figure is run.
ms_fig2_appendix_strength <- function(artifact) {
  tasks <- tibble::as_tibble(artifact$contrast_summary)
  ms_plot_require_columns(tasks, c("status", "outcome", "outcome_label", "outcome_domain",
    "metric_geometry", "signal_reference_r2", "signal_difference_outcome_sd",
    "inference_deviation", "n_matched_days", "n_participants"), "Appendix inference")
  eligible <- tasks |> filter(status == "estimated", is.finite(signal_reference_r2)) |>
    mutate(geometry = if_else(metric_geometry == "circular_time", "Circular", "Linear"),
           domain = factor(outcome_domain, levels = c("Sleep", "Alertness", "Affect")))
  thresholds <- tibble(stratum = c("All", "R² ≥ 0.1%", "R² ≥ 1%", "R² ≥ 5%"),
                       threshold = c(0, .001, .01, .05))
  rows <- tidyr::crossing(eligible, thresholds) |> filter(signal_reference_r2 >= threshold) |>
    tidyr::pivot_longer(c(inference_deviation, signal_difference_outcome_sd),
                       names_to = "quantity", values_to = "value") |>
    filter(is.finite(value))
  summary <- rows |> group_by(domain, geometry, stratum, threshold, quantity) |>
    summarise(n_tasks = n(), median = median(value), q25 = quantile(value, .25),
      q75 = quantile(value, .75), min_participants = min(n_participants),
      max_participants = max(n_participants), min_matched_days = min(n_matched_days),
      max_matched_days = max(n_matched_days), .groups = "drop")
  # Empty signal-strength strata remain explicit, rather than disappearing.
  grid <- tidyr::crossing(domain = factor(c("Sleep", "Alertness", "Affect"),
    levels = c("Sleep", "Alertness", "Affect")), geometry = c("Linear", "Circular"),
    thresholds, quantity = c("inference_deviation", "signal_difference_outcome_sd"))
  summary <- grid |> left_join(summary, by = c("domain", "geometry", "stratum", "threshold", "quantity")) |>
    mutate(n_tasks = coalesce(n_tasks, 0L),
      stratum = factor(stratum, levels = rev(thresholds$stratum)),
      quantity_label = if_else(quantity == "inference_deviation",
        "Coefficient displacement / reference uncertainty", "Signal RMS / within-outcome RMS"))
  strength <- eligible |> group_by(outcome, outcome_label, geometry) |>
    summarise(n_tasks = n(), median_r2 = median(signal_reference_r2),
      q25 = quantile(signal_reference_r2, .25), q75 = quantile(signal_reference_r2, .75),
      min_participants = min(n_participants), max_participants = max(n_participants),
      min_matched_days = min(n_matched_days), max_matched_days = max(n_matched_days), .groups = "drop")
  p_a <- ggplot(eligible, aes(100 * signal_reference_r2, outcome_label)) +
    geom_point(position = position_jitter(height = .16, width = 0, seed = 19),
               size = .55, alpha = .22, colour = MS_NEUTRAL) +
    geom_errorbar(data = strength, aes(x = 100 * median_r2, xmin = 100 * q25, xmax = 100 * q75),
                  orientation = "y", width = .16, colour = MS_PRIMARY, linewidth = .5) +
    geom_point(data = strength, aes(x = 100 * median_r2), size = 1.5, colour = MS_PRIMARY) +
    facet_wrap(~geometry, nrow = 1) +
    scale_x_continuous(trans = scales::pseudo_log_trans(sigma = .03),
                       breaks = c(0, .1, 1, 5, 10, 25)) +
    labs(x = "Reference within-participant R² (%)", y = NULL,
      caption = "Reference strength is evaluated on each contrast's exact matched days; tasks can share observations.") +
    theme_ms_axes(legend_position = "none")
  panels <- lapply(unique(summary$quantity), function(quantity) {
    z <- summary |> filter(.data$quantity == .env$quantity)
    ggplot(z, aes(median, stratum)) +
      geom_errorbar(aes(xmin = q25, xmax = q75), orientation = "y", width = .2,
                    linewidth = .5, colour = MS_PRIMARY, na.rm = TRUE) +
      geom_point(size = 1.5, colour = MS_PRIMARY, na.rm = TRUE) +
      geom_text(aes(x = 0, label = paste0("n=", n_tasks)), hjust = -.1, vjust = -1,
                size = 1.8, colour = MS_NEUTRAL) +
      facet_grid(geometry ~ domain) +
      scale_x_continuous(trans = scales::pseudo_log_trans(sigma = .02), expand = expansion(mult = c(.03, .20))) +
      labs(x = unique(z$quantity_label), y = NULL) + theme_ms_axes(legend_position = "none")
  })
  note <- cowplot::ggdraw() + cowplot::draw_label(
    "Nested reference-strength strata describe the same matched-support association models. Points/bars show task medians/IQR.\nLinear and circular exposure geometries are summarized separately; each stratum retains its task count.",
    x = .02, hjust = 0, size = MS_FIG_DENSE_PT, fontfamily = MS_FONT)
  plot <- cowplot::plot_grid(ms_panel_frame(p_a, "a  Reference signal on matched support"),
    ms_panel_frame(panels[[1]], "b  Coefficient preservation by reference strength"),
    ms_panel_frame(panels[[2]], "c  Signal preservation by reference strength"), note,
    ncol = 1, rel_heights = c(.30, .32, .32, .06))
  list(plot = plot, strength = strength, strata = summary)
}

if (!isTRUE(get0("MS_FIG2_APPENDIX_ONLY", inherits = FALSE, ifnotfound = FALSE))) {
contract <- rq1_inference_contract()
INFERENCE_RDS <- file.path("results", "rq1", "inference", contract$artifact_filename)
RQ1_SUMMARY_CSV <- file.path("results", "rq1", "rq1_pairwise_summary.csv")
OUT_DIR <- file.path("results", "rq1", "figures")
FIG2_WIDTH_IN <- MS_FIG_WIDTH_IN
FIG2_HEIGHT_IN <- 7.6
ms_plot_require_files(c(INFERENCE_RDS, RQ1_SUMMARY_CSV), "Fig. 2 plotting inputs")
if (!ms_plot_prep_only()) {
  dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)
}

a <- readRDS(INFERENCE_RDS)
if (!identical(a$artifact_type, "rq1_inferential_preservation")) {
  stop("Unexpected inferential-preservation artifact type", call. = FALSE)
}
ms_assert_version(a, "analysis_design_id", ms_analysis_design_id())
CORE_VERSION <- ms_plot_assert_core(a$core_artifact_version)
RQ1_VERSION <- ms_plot_one_version(a$rq1_analysis_version, "rq1_analysis_version")
INFERENCE_VERSION <- ms_plot_one_version(a$rq1_inference_version, "rq1_inference_version")
ms_plot_assert_prefix(RQ1_VERSION, "rq1_v6_", "rq1_analysis_version")
ms_plot_assert_prefix(INFERENCE_VERSION, "rq1_inference_v8_geometry_anchor8__", "rq1_inference_version")

rq1_summary <- readr::read_csv(RQ1_SUMMARY_CSV, show_col_types = FALSE, progress = FALSE)
ms_plot_assert_core(rq1_summary$core_artifact_version, CORE_VERSION)
if (!identical(ms_plot_one_version(rq1_summary$rq1_analysis_version, "rq1 summary version"), RQ1_VERSION)) {
  stop("Fig. 2 RQ1 summary version mismatch", call. = FALSE)
}

reference <- tibble::as_tibble(a$reference_summary)
contrast <- tibble::as_tibble(a$contrast_summary)
anchor_map <- tibble::as_tibble(a$anchor_map)
ms_plot_require_columns(
  reference,
  c("metric", "metric_class", "outcome", "outcome_domain", "reference_association_strength", "status"),
  "reference association summary"
)
ms_plot_require_columns(
  contrast,
  c("candidate_config", "contrast_label", "contrast_order", "dimension", "metric", "metric_class",
    "outcome", "outcome_domain", "inference_deviation", "rq1_distortion_A", "status",
    "distortion_between_ms", "distortion_within_ms", "distortion_within_fraction", "D_T", "f_W",
    "signal_difference_outcome_sd", "signal_reference_r2", "signal_candidate_r2", "signal_correlation",
    "rho_within", "within_shape_loss"),
  "inferential preservation summary"
)
if (nrow(anchor_map) != contract$anchor_count || n_distinct(anchor_map$candidate_config) != contract$anchor_count) {
  stop("Fig. 2 anchor map does not match the frozen inferential-preservation contract", call. = FALSE)
}
if (n_distinct(reference$metric) != contract$daily_metric_count ||
    !setequal(unique(reference$outcome), contract$outcomes)) {
  stop("Fig. 2 reference association landscape does not match the frozen daily metric/outcome contract", call. = FALSE)
}

OUTCOME_LEVELS <- contract$outcomes
OUTCOME_LABELS <- contract$outcome_label
DOMAIN_LEVELS <- c("Sleep", "Alertness", "Affect")
metric_order <- ms_metric_order(rq1_summary)

reference_plot <- reference |>
  mutate(
    outcome_domain = factor(outcome_domain, levels = DOMAIN_LEVELS),
    outcome = factor(outcome, levels = OUTCOME_LEVELS, labels = unname(OUTCOME_LABELS[OUTCOME_LEVELS])),
    reference_association_strength = if_else(
      is.finite(reference_association_strength), reference_association_strength, NA_real_
    )
  ) |>
  ms_add_metric_order(metric_order)

contrast_plot <- contrast |>
  mutate(
    outcome_domain = factor(outcome_domain, levels = DOMAIN_LEVELS),
    outcome = factor(outcome, levels = OUTCOME_LEVELS, labels = unname(OUTCOME_LABELS[OUTCOME_LEVELS])),
    contrast_label = factor(contrast_label, levels = anchor_map$contrast_label[order(anchor_map$contrast_order)]),
    metric_class = factor(metric_class, levels = MS_METRIC_CLASSES)
  ) |>
  filter(is.finite(inference_deviation), is.finite(rq1_distortion_A))
if (!nrow(contrast_plot)) stop("No finite inferential-preservation results for Fig. 2", call. = FALSE)

# -----------------------------------------------------------------------------
# a. Canonical high-information reference association landscape across domains
# -----------------------------------------------------------------------------
# Six outcome distributions replace the 52-row metric lookup. Metrics retain
# equal weight within each outcome; classes remain descriptive colours only.
reference_distribution <- reference_plot |>
  filter(is.finite(reference_association_strength)) |>
  arrange(outcome, metric) |>
  group_by(outcome) |>
  mutate(row = 7 - as.integer(outcome),
         point_y = row - .25 + .10 * ((row_number() * .61803398875) %% 1)) |>
  ungroup()
reference_distribution_summary <- reference_distribution |>
  group_by(outcome, row) |>
  summarise(q25 = quantile(reference_association_strength, .25, type = 8),
            centre = median(reference_association_strength),
            q75 = quantile(reference_association_strength, .75, type = 8),
            .groups = "drop")
# Draw one side of the existing violin density. Density estimation and data
# coordinates are unchanged; only the mirrored, redundant polygon is removed.
GeomReferenceHalfViolin <- ggproto("GeomReferenceHalfViolin", GeomViolin,
  draw_group = function(self, data, panel_params, coord, ...) {
    flipped <- data$flipped_aes[1]
    data <- ggplot2::flip_data(data, flipped)
    edge <- data
    edge$x <- edge$x + edge$violinwidth * (edge$xmax - edge$x)
    polygon <- rbind(data[order(data$y), ], edge[order(edge$y, decreasing = TRUE), ])
    polygon <- ggplot2::flip_data(polygon, flipped)
    GeomPolygon$draw_panel(polygon, panel_params, coord)
  })
p2a <- ggplot(reference_distribution, aes(reference_association_strength, row)) +
  geom_hline(yintercept = c(2.5, 3.5), colour = "#DFE4E6", linewidth = .25) +
  layer(geom = GeomReferenceHalfViolin, stat = "ydensity", position = "identity",
        mapping = aes(group = outcome), params = list(orientation = "y", width = .60,
        bounds = c(0, Inf), scale = "width", fill = "#E6ECEF",
        colour = "#A5B1B8", linewidth = .25)) +
  geom_point(aes(y = point_y, colour = metric_class), size = .85, alpha = .62) +
  geom_segment(data = reference_distribution_summary,
    aes(x = q25, xend = q75, y = row + .37, yend = row + .37),
    inherit.aes = FALSE, colour = "#52636C", linewidth = .55, lineend = "round") +
  geom_point(data = reference_distribution_summary,
    aes(x = centre, y = row + .37), inherit.aes = FALSE,
    shape = 21, size = 1.65, stroke = .45, fill = "white", colour = "#354953") +
  scale_color_ms_metric(guide = "none") +
  scale_y_continuous(breaks = 6:1, labels = unname(OUTCOME_LABELS[OUTCOME_LEVELS]),
                     limits = c(.55, 6.5), expand = c(0, 0)) +
  scale_x_continuous(limits = c(0, NA), breaks = 0:5, expand = expansion(mult = c(0, .04))) +
  labs(title = "a  Reference association distributions",
       subtitle = "Eye / MEDI / 10 s · 52 representations per outcome\nDensity and metric dots; open circle / bar: median / IQR",
       x = "Reference association strength\n(bootstrap-uncertainty units)", y = NULL) +
  theme_ms_axes(base_size = 5.9, legend_position = "none", plot_title_size = 6.8) +
  theme(panel.grid.major.y = element_blank(), panel.grid.minor = element_blank(),
        axis.text.y = element_text(size = 5.2), axis.text.x = element_text(size = 4.6),
        axis.title.x = element_text(size = 5),
        plot.subtitle = element_text(size = 4.35, colour = "#666A6D"),
        plot.margin = margin(2, 5, 2, 3))

# -----------------------------------------------------------------------------
# b. Domain-level degradation distributions across metric-outcome combinations
# -----------------------------------------------------------------------------
contrast_summary <- contrast_plot |>
  group_by(outcome_domain, contrast_label, contrast_order, dimension) |>
  summarise(
    n_outcomes = n_distinct(outcome),
    n_metric_outcomes = n(),
    deviation_q25 = quantile(inference_deviation, .25, na.rm = TRUE, names = FALSE, type = 8),
    deviation_median = median(inference_deviation, na.rm = TRUE),
    deviation_q75 = quantile(inference_deviation, .75, na.rm = TRUE, names = FALSE, type = 8),
    .groups = "drop"
  )

# Both consequence panels use the same continuous deviation scale. This is a
# display transform, not an inferential threshold or a change to the estimates.
deviation_limits <- c(0, max(contrast_plot$inference_deviation) * 1.04)
deviation_breaks <- c(0, .1, .3, 1, 3, 5)
contrast_short <- setNames(c("Chest", "Wrist", "LIGHT", "20 s", "30 s", "40 s", "60 s", "120 s"),
                          anchor_map$contrast_label[order(anchor_map$contrast_order)])
contrast_display <- contrast_plot |>
  mutate(display_row = 9 - as.integer(contrast_label))
summary_display <- contrast_summary |>
  mutate(display_row = 9 - as.integer(contrast_label))
p2b <- ggplot(contrast_display, aes(inference_deviation, display_row - .13, color = metric_class)) +
  geom_hline(yintercept = c(5.5, 6.5), linewidth = .23, colour = "#DEE3E5") +
  geom_point(
    position = position_jitter(width = 0, height = .10, seed = 211),
    size = .52, alpha = .25
  ) +
  geom_linerange(
    data = summary_display,
    aes(y = display_row + .18, xmin = deviation_q25, xmax = deviation_q75),
    orientation = "y", inherit.aes = FALSE, linewidth = .45, color = "#52636C"
  ) +
  geom_point(
    data = summary_display,
    aes(y = display_row + .18, x = deviation_median),
    inherit.aes = FALSE, shape = 21, size = 1.45, stroke = .40,
    fill = "white", color = "#354953"
  ) +
  facet_wrap(~outcome_domain, nrow = 1) +
  scale_color_ms_metric(guide = "none") +
  scale_y_continuous(breaks = 8:1, labels = unname(contrast_short),
                     limits = c(.55, 8.5), expand = c(0, 0)) +
  scale_x_continuous(trans = scales::pseudo_log_trans(sigma = .08),
                     limits = deviation_limits, breaks = deviation_breaks) +
  labs(
    title = "b  Which configurations shift inference?",
    subtitle = paste0("Dots: metric–outcome pairs · open circles / bars above: median / IQR\n",
                      "All vs eye / MEDI / 10 s · pseudo-log x · LIGHT excludes MDER/nvRD"),
    x = "Inferential deviation (reference-bootstrap uncertainty units)", y = NULL
  ) +
  theme_ms_axes(base_size = 5.9, legend_position = "none", plot_title_size = 6.8) +
  theme(
    panel.grid.major.y = element_blank(),
    axis.text.x = element_text(size = 4.3),
    axis.text.y = element_text(size = 4.8),
    axis.title.x = element_text(size = 4.7),
    strip.text = element_text(size = 5.0),
    plot.subtitle = element_text(size = 4.35, color = "#666A6D"),
    plot.margin = margin(2, 3, 2, 3)
  )

# -----------------------------------------------------------------------------
# c. Frozen RQ1 representation distortion -> inferential deviation by domain
# -----------------------------------------------------------------------------
link_bins <- contrast_plot |>
  group_by(outcome_domain) |>
  mutate(distortion_bin = ntile(rq1_distortion_A, 5L)) |>
  group_by(outcome_domain, distortion_bin) |>
  summarise(
    rq1_distortion_A = median(rq1_distortion_A, na.rm = TRUE),
    inference_deviation = median(inference_deviation, na.rm = TRUE),
    n = n(), .groups = "drop"
  )

link_assoc <- contrast_plot |>
  group_by(outcome_domain) |>
  summarise(
    n = n(),
    rho = suppressWarnings(cor(rq1_distortion_A, inference_deviation,
                               method = "spearman", use = "complete.obs")),
    .groups = "drop"
  ) |>
  mutate(label = if_else(is.finite(rho), sprintf("Spearman rₛ = %.2f", rho), "Spearman rₛ = NA"))

p2c <- ggplot(contrast_plot, aes(rq1_distortion_A, inference_deviation, color = metric_class)) +
  geom_point(shape = 21, fill = "white", stroke = .24, size = .68, alpha = .43) +
  geom_line(
    data = link_bins,
    aes(rq1_distortion_A, inference_deviation, group = outcome_domain),
    inherit.aes = FALSE, linewidth = .45, color = "#354953"
  ) +
  geom_point(
    data = link_bins,
    aes(rq1_distortion_A, inference_deviation),
    inherit.aes = FALSE, shape = 21, size = 1.50, stroke = .4,
    fill = "white", color = "#354953"
  ) +
  geom_text(
    data = link_assoc,
    aes(x = -Inf, y = Inf, label = label),
    inherit.aes = FALSE, hjust = -.04, vjust = 1.12,
    size = 1.65, color = "#303437"
  ) +
  facet_wrap(~outcome_domain, nrow = 1) +
  scale_color_ms_metric(guide = "none") +
  scale_x_continuous(trans = scales::pseudo_log_trans(sigma = .03), breaks = c(0, .1, .5, 2)) +
  scale_y_continuous(trans = scales::pseudo_log_trans(sigma = .08),
                     limits = deviation_limits, breaks = deviation_breaks) +
  labs(
    title = "c  Distortion and inferential displacement",
    subtitle = "Frozen RQ1 A · connected open circles: quintile medians · axes expand near zero",
    x = "Frozen RQ1 representation distortion, A",
    y = "Inferential deviation"
  ) +
  theme_ms_axes(base_size = 5.9, legend_position = "none", plot_title_size = 6.8) +
  theme(
    axis.text = element_text(size = 4.5),
    strip.text = element_text(size = 5.0),
    axis.title = element_text(size = 4.7),
    plot.subtitle = element_text(size = 4.35, color = "#666A6D"),
    plot.margin = margin(2, 3, 2, 3)
  )

# Preserve the former reference landscape and pooled link as supplementary plots.
p2_reference <- p2a
p2_pooled <- p2c
component_plot <- contrast_plot |>
  filter(is.finite(D_T)) |>
  mutate(geometry_label = factor(if_else(metric_geometry == "linear", "Linear", "Circular sin/cos"),
                                 levels=c("Linear", "Circular sin/cos")),
         display_row = 9 - as.integer(contrast_label))
total_summary <- component_plot |>
  group_by(geometry_label,display_row) |>
  summarise(q25=quantile(D_T,.25),centre=median(D_T),q75=quantile(D_T,.75),.groups="drop")
p2_total <- ggplot(component_plot,aes(D_T,display_row)) +
  geom_point(aes(colour=metric_class),
    position=position_jitter(width=0,height=.11,seed=213),size=.65,alpha=.30) +
  geom_linerange(data=total_summary,aes(y=display_row,xmin=q25,xmax=q75),
    inherit.aes=FALSE,orientation="y",colour="#354953",linewidth=.5) +
  geom_point(data=total_summary,aes(x=centre),shape=21,fill="white",size=1.5,stroke=.4) +
  facet_wrap(~geometry_label,nrow=1,scales="free_x") +
  scale_color_ms_metric(guide="none") +
  scale_x_continuous(trans=scales::pseudo_log_trans(sigma=.1),breaks=c(0,.1,.5,1,2,5,10)) +
  scale_y_continuous(breaks=8:1,labels=unname(contrast_short),limits=c(.55,8.5)) +
  labs(title="a  Distortion magnitude",
    subtitle="Same matched participant-days; dots: tasks; bars: IQR\nLinear: reference-SD units; circular: sin/cos units",
    x=expression(D[T]~"(total RMS; pseudo-log scale)"),y=NULL) +
  theme_ms_axes(base_size=6.3,legend_position="none",plot_title_size=7) +
  theme(panel.grid.major.y=element_blank(),strip.text=element_text(size=5.5),
    axis.text=element_text(size=5.2),plot.subtitle=element_text(size=4.8,colour="#666A6D"))

# Replace the composition forest with frozen pattern/signal pairs. The existing
# component model remains in the analysis artifact; this panel adds no fit.
shape_plot <- contrast_plot |>
  filter(is.finite(within_shape_loss), is.finite(signal_difference_outcome_sd)) |>
  mutate(geometry_label = factor(if_else(metric_geometry == "linear", "Linear", "Circular sin/cos"),
                                 levels = c("Linear", "Circular sin/cos")))
p2c <- ggplot(shape_plot, aes(within_shape_loss, signal_difference_outcome_sd,
                             colour = metric_class, shape = dimension)) +
  geom_point(size = .8, alpha = .4, stroke = .2) +
  scale_color_ms_metric(guide = "none") +
  scale_shape_manual(values = c(placement = 16, optical = 17, temporal = 1), guide = "none") +
  scale_x_continuous(limits = c(0, 1), breaks = c(0, .5, 1)) +
  scale_y_continuous(labels = scales::label_percent()) +
  facet_grid(geometry_label ~ outcome_domain) +
  labs(title="c  Within-person pattern and association signal",
    subtitle="Linear: 1 - rho-within squared; circular: design projection loss\nSame matched days; descriptive points, no fitted relation",
    x="Reference pattern unexplained by candidate", y="Signal difference / within-outcome RMS") +
  theme_ms_axes(base_size=5.9,legend_position="none",plot_title_size=6.8) +
  theme(axis.text=element_text(size=4.8),axis.title=element_text(size=4.7),
        strip.text=element_text(size=5),plot.subtitle=element_text(size=4.35,colour="#666A6D"))

# Coefficient movement and fitted-signal change are different quantities. Keep
# every finite metric/outcome/contrast point; no outcome or significance screen,
# added regression, smoothing, bootstrap or decision threshold is needed here.
p2d <- ggplot(contrast_plot,
              aes(inference_deviation, signal_difference_outcome_sd,
                  colour = metric_class, shape = dimension, size = signal_reference_r2)) +
  geom_hline(yintercept = 0, colour = "#C8CFD3", linewidth = .25) +
  geom_point(alpha = .45, stroke = .25, na.rm = TRUE) +
  facet_wrap(~outcome_domain, nrow = 1) +
  scale_color_ms_metric(guide = "none") +
  scale_shape_manual(values = c(placement = 16, optical = 17, temporal = 1),
                     breaks = c("placement", "optical", "temporal"),
                     labels = c("Placement", "Optical", "Temporal"), name = NULL) +
  scale_size_continuous(range = c(.45, 1.7), breaks = c(.001, .01, .05),
                        labels = scales::label_percent(accuracy = .1), name = "Reference within R²") +
  scale_x_continuous(trans = scales::pseudo_log_trans(sigma = .08),
                     limits = deviation_limits, breaks = deviation_breaks) +
  scale_y_continuous(labels = scales::label_percent(),
                     expand = expansion(mult = c(.02, .06))) +
  labs(title = "d  Does coefficient movement change the association signal?",
       subtitle = paste("Same matched days and paired fits; participant intercepts removed.",
                        "Descriptive in-sample signals, not prediction accuracy."),
       x = "Inferential deviation (reference-bootstrap uncertainty units)",
       y = "Signal difference / within-outcome RMS") +
  theme_ms_axes(base_size = 6.1, legend_position = "bottom", plot_title_size = 7) +
  theme(axis.text = element_text(size = 5), strip.text = element_text(size = 5.5),
        axis.title = element_text(size = 5.2), legend.text = element_text(size = 5.5),
        plot.subtitle = element_text(size = 4.6, colour = "#666A6D"),
        plot.margin = margin(5, 3, 3, 3))

# Four distinct questions: distortion magnitude, coefficient sensitivity,
# pattern/signal correspondence, and coefficient/signal correspondence.
# The two signal panels share domain columns and the complete vertical range.
signal_limits <- c(0, max(contrast_plot$signal_difference_outcome_sd, na.rm=TRUE) * 1.05)
p2_total <- ms_style_plot(p2_total) + labs(title=NULL, x=expression(D[T]~"(total RMS; pseudo-log)")) +
  scale_x_continuous(trans=scales::pseudo_log_trans(sigma=.1),
    breaks=function(limits) if(max(limits)<=1) c(0,.1,.2,.4,.8) else c(0,.3,1,3,10))
p2b <- ms_style_plot(p2b) + labs(title=NULL, x="Inferential deviation (pseudo-log)") +
  scale_x_continuous(trans=scales::pseudo_log_trans(sigma=.08),limits=deviation_limits,breaks=c(0,.3,1,5))
p2c <- ms_style_plot(p2c) + labs(title=NULL, x="Within-person pattern loss", y="Signal difference /\nwithin-outcome RMS") +
  scale_y_continuous(limits=signal_limits,labels=scales::label_percent(),expand=expansion(mult=0))
p2d_display <- ms_style_plot(p2d) + labs(title=NULL, x="Inferential deviation (reference uncertainty units; pseudo-log)",
  y="Signal difference /\nwithin-outcome RMS") +
  scale_y_continuous(limits=signal_limits,labels=scales::label_percent(),expand=expansion(mult=0)) +
  guides(size=guide_legend(order=1),shape=guide_legend(order=2,override.aes=list(size=1.3,alpha=.7))) +
  theme(legend.key.width=grid::unit(5,"mm"),legend.box.spacing=grid::unit(1,"mm"),
    legend.spacing.x=grid::unit(4,"mm"))
upper_fields <- cowplot::align_plots(p2_total,p2b,align="h",axis="tb")
upper <- cowplot::plot_grid(ms_panel_frame(upper_fields[[1]],"a  Distortion magnitude"),
  ms_panel_frame(upper_fields[[2]],"b  Inferential displacement"),nrow=1,rel_widths=c(.40,.60))
# Match left and right gutters so the three domain columns line up in c and d.
signal_fields <- cowplot::align_plots(p2c,p2d_display,align="v",axis="lr")
foot <- cowplot::ggdraw()+cowplot::draw_label(
  "a: matched-day RMS; linear in reference-SD units, circular in sin/cos units. a/b: dots = tasks; rings/bars = median/IQR.\nc/d: same-sample association signals, not prediction accuracy. c: linear correlation loss; circular design projection loss.",
  x=.012,hjust=0,size=MS_FIG_DENSE_PT,colour="#30363B",fontfamily=MS_FONT)
fig2 <- cowplot::plot_grid(ms_metric_legend(key_width_mm=5),upper,
  ms_panel_frame(signal_fields[[1]],"c  Within-person pattern and association signal"),
  ms_panel_frame(signal_fields[[2]],"d  Coefficient displacement and association signal"),foot,
  ncol=1,rel_heights=c(.20,2.50,2.45,2.08,.37))

if (!ms_plot_prep_only()) {
  readr::write_csv(
    reference_plot |>
      mutate(metric = as.character(metric), metric_class = as.character(metric_class),
             outcome = as.character(outcome), outcome_domain = as.character(outcome_domain)),
    file.path("results", "rq1", "inference", "fig2_reference_association_landscape.csv"), na = ""
  )
  readr::write_csv(
    contrast_summary |>
      mutate(outcome_domain = as.character(outcome_domain), contrast_label = as.character(contrast_label)),
    file.path("results", "rq1", "inference", "fig2_inferential_degradation.csv"), na = ""
  )
  readr::write_csv(
    contrast_plot |>
      mutate(metric_class = as.character(metric_class), outcome = as.character(outcome),
             outcome_domain = as.character(outcome_domain), contrast_label = as.character(contrast_label)),
    file.path("results", "rq1", "inference", "fig2_distortion_inference_link.csv"), na = ""
  )
}

ms_plot_save(fig2, file.path(OUT_DIR, "Fig2_RQ1_inferential_preservation.png"), FIG2_WIDTH_IN, FIG2_HEIGHT_IN)
ms_plot_save(p2_reference + labs(title="Reference association distributions"),
  file.path(OUT_DIR,"FigS_RQ1_reference_associations.png"),4.2,6.2)
ms_plot_save(p2_pooled + labs(title="Pooled distortion-displacement relationship (descriptive)"),
  file.path(OUT_DIR,"FigS_RQ1_pooled_distortion_link.png"),7.4,3.8)
link_by_contrast <- tibble::as_tibble(a$component_link$correlations) |>
  left_join(anchor_map |> select(comparison_pair_id,contrast_label,contrast_order),by="comparison_pair_id") |>
  mutate(contrast_label=factor(contrast_label,levels=rev(anchor_map$contrast_label[order(anchor_map$contrast_order)])))
link_by_contrast_plot <- ggplot(link_by_contrast,aes(rho_A,contrast_label)) +
  geom_vline(xintercept=0,colour="#C8CFD3",linewidth=.3) + geom_point(size=1.4,colour="#354953") +
  facet_grid(metric_geometry~outcome_domain) + scale_x_continuous(limits=c(-1,1)) +
  labs(x="Within-contrast Spearman correlation: frozen A vs displacement",y=NULL,
       title="Pooled association need not hold within each measurement contrast") +
  theme_ms_axes(base_size=6,legend_position="none")
ms_plot_save(link_by_contrast_plot,file.path(OUT_DIR,"FigS_RQ1_contrast_distortion_link.png"),7.4,5.5)
ms_plot_write_manifest(
  file.path(OUT_DIR, "figure_artifact_manifest.csv"),
  tibble(
    figure = c("Fig1_RQ1", "Fig2_RQ1_inferential_preservation"),
    input_artifact = c(
      "rq1_pairwise_change_long + rq1_pairwise_summary + rq1_local_transition_summary",
      "rq1_inferential_preservation_domains_anchor8 + rq1_pairwise_summary"
    ),
    core_artifact_version = CORE_VERSION,
    rq1_analysis_version = RQ1_VERSION,
    rq1_inference_version = c(NA_character_, INFERENCE_VERSION),
    rq2_analysis_version = NA_character_,
    rq3_analysis_version = NA_character_
  )
)
message("Fig. 2 complete: distortion magnitude, inferential displacement and association-signal preservation.")
}
