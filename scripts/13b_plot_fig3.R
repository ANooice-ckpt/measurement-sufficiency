# Fig. 3 only — cross-dimensional non-additivity.
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
source("scripts/utils/plot_rq2_common.R")

# =============================================================================
# Fig. 3 — cross-dimensional non-additivity
# =============================================================================

format_gamma_transition <- function(x) {
  x |>
    str_replace_all("_LIGHT_to_MEDI", paste0(" · LIGHT → MEDI")) |>
    str_replace_all("([0-9]+)to([0-9]+)", "\\1 → \\2 s") |>
    str_replace_all("_", " · ")
}

PAIR_LEVELS <- c(
  paste("Placement", "optical", sep = " × "),
  paste("Optical", "temporal", sep = " × "),
  paste("Placement", "temporal", sep = " × ")
)
PAIR_CODES <- c("placement__optical", "optical__temporal", "placement__temporal")
names(PAIR_CODES) <- PAIR_LEVELS
PAIR_CODE_TO_LABEL <- setNames(names(PAIR_CODES), unname(PAIR_CODES))
NUMERIC_TOL <- 1e-12
PAIR_LABELS <- setNames(PAIR_LEVELS, PAIR_LEVELS)

safe_median <- function(x) {
  x <- as.numeric(x); x <- x[is.finite(x)]
  if (length(x)) unname(stats::median(x)) else NA_real_
}
safe_q <- function(x, p) {
  x <- as.numeric(x); x <- x[is.finite(x)]
  if (length(x)) unname(stats::quantile(x, p, names = FALSE)) else NA_real_
}

gamma_plot <- gamma_summary |>
  mutate(
    dimension_pair = case_when(
      dimension_a == "placement" & dimension_b == "optical" ~ PAIR_LEVELS[[1]],
      dimension_a == "placement" & dimension_b == "temporal" ~ PAIR_LEVELS[[3]],
      dimension_a == "optical" & dimension_b == "temporal" ~ PAIR_LEVELS[[2]],
      TRUE ~ paste(dimension_a, "×", dimension_b)
    ),
    pair_code = case_when(
      dimension_a == "placement" & dimension_b == "optical" ~ PAIR_CODES[[1]],
      dimension_a == "optical" & dimension_b == "temporal" ~ PAIR_CODES[[2]],
      dimension_a == "placement" & dimension_b == "temporal" ~ PAIR_CODES[[3]],
      TRUE ~ NA_character_
    ),
    dimension_pair = factor(dimension_pair, levels = PAIR_LEVELS),
    metric = as.character(metric),
    metric_class = factor(metric_class, levels = METRIC_CLASSES),
    transition_display = format_gamma_transition(transition),
    Q = as.numeric(Q), R = as.numeric(R)
  )
if (any(is.finite(gamma_plot$Q) & gamma_plot$Q < -NUMERIC_TOL, na.rm = TRUE)) {
  stop("RQ2 gamma Q contains a negative value", call. = FALSE)
}
gamma_plot <- gamma_plot |> mutate(Q = abs(Q))

gamma_metric <- gamma_plot |>
  filter(is.finite(R) | is.finite(Q)) |>
  group_by(pair_code, dimension_pair, metric, metric_class) |>
  summarise(
    Q_metric = safe_median(Q),
    R_metric = safe_median(R),
    C_metric = safe_median(if_else(
      is.finite(R) & is.finite(Q) & Q > NUMERIC_TOL, R / Q, NA_real_
    )),
    n_transitions = n_distinct(transition), .groups = "drop"
  ) |>
  mutate(
    C_metric = if_else(is.finite(C_metric), pmax(-1, pmin(1, C_metric)), NA_real_),
    dimension_pair = factor(dimension_pair, levels = PAIR_LEVELS)
  )

metric_table <- gamma_plot |>
  distinct(metric, metric_class) |>
  mutate(metric = as.character(metric), metric_class = as.character(metric_class)) |>
  left_join(
    metric_order |> transmute(metric = as.character(metric), rq1_metric_order = metric_order),
    by = "metric"
  ) |>
  distinct(metric, .keep_all = TRUE)

# Panel c — directional coherence. This is the active compact raincloud display
# from the pre-refactor source; retired alternative compositions are omitted.
coherence_points <- gamma_metric |>
  filter(is.finite(C_metric)) |>
  mutate(
    pair_code = as.character(pair_code),
    pair_y = unname(setNames(3:1, unname(PAIR_CODES))[pair_code])
  )
coherence_summary <- coherence_points |>
  group_by(pair_code, pair_y) |>
  summarise(C_median = safe_median(C_metric), n_metrics = n(), .groups = "drop")
coherence_polygon <- function(pair_code_name) {
  values <- coherence_points |> filter(pair_code == pair_code_name) |> pull(C_metric)
  pair_y <- unname(setNames(3:1, unname(PAIR_CODES))[pair_code_name])
  if (!length(values)) return(tibble())
  if (length(unique(values)) < 2L) {
    density_x <- c(values[[1]] - .01, values[[1]] + .01)
    density_h <- c(.01, .01)
  } else {
    density_fit <- stats::density(values, from = -1, to = 1, n = 256, adjust = 1)
    density_x <- density_fit$x
    density_h <- .33 * density_fit$y / max(density_fit$y)
  }
  tibble(
    pair_code = pair_code_name,
    x = c(density_x, rev(density_x)),
    y = c(pair_y + density_h, rep(pair_y, length(density_x)))
  )
}
coherence_polygons <- purrr::map_dfr(unname(PAIR_CODES), coherence_polygon)
coherence_y_labels <- c(
  paste0("Placement ", "×", "\noptical"),
  paste0("Optical ", "×", "\ntemporal"),
  paste0("Placement ", "×", "\ntemporal")
)

p3c <- ggplot() +
  geom_vline(xintercept = 0, linewidth = .32, colour = "#7E878B") +
  geom_polygon(
    data = coherence_polygons,
    aes(x, y, group = pair_code),
    fill = "#A9C7DB", colour = "#6F9CBD", linewidth = .25, alpha = .62
  ) +
  geom_segment(
    data = coherence_summary,
    aes(x = -1, xend = 1, y = pair_y, yend = pair_y),
    colour = "#D1D7DA", linewidth = .32
  ) +
  geom_point(
    data = coherence_points,
    aes(C_metric, pair_y - .10),
    position = position_jitter(width = 0, height = .085, seed = 89),
    shape = 16, size = .70, alpha = .54, colour = "#59666C"
  ) +
  geom_point(
    data = coherence_summary,
    aes(C_median, pair_y - .10),
    shape = 23, size = 2.05, fill = MS_PRIMARY, colour = "#273F50", stroke = .25
  ) +
  scale_x_continuous(
    limits = c(-1, 1), breaks = c(-1, -.5, 0, .5, 1),
    labels = c("-1", "-.5", "0", ".5", "+1"),
    expand = expansion(mult = c(.015, .015))
  ) +
  scale_y_continuous(
    limits = c(.45, 3.55), breaks = 3:1, labels = coherence_y_labels,
    expand = expansion(mult = c(0, 0))
  ) +
  labs(
    title = "c  Directional coherence is\nfrequently weak",
    subtitle = "C = median_t(R_mpt / Q_mpt)", x = "directional coherence C", y = NULL
  ) +
  theme_rq2(base_size = 5.25) +
  theme(
    panel.grid.major.y = element_blank(), panel.grid.major.x = element_line(
      colour = "#ECEFF0", linewidth = .18
    ),
    axis.text.y = element_text(size = 3.85, lineheight = .84),
    axis.text.x = element_text(size = 3.8), axis.title.x = element_text(size = 4.1),
    axis.ticks.y = element_blank(), axis.line.y = element_blank(),
    plot.title = element_text(size = 5.55, hjust = 0, margin = margin(b = 2)),
    plot.subtitle = element_text(size = 4.0, colour = "#666A6D", hjust = 0,
                                 margin = margin(t = -1, b = 2)),
    plot.margin = margin(2, 2, 2, 2)
  )

# Current main-text Fig. 3 composition.
ATLAS3_CLASS_ORDER <- c("Overall", "temporal dynamics", "timing", "duration", "level")
ATLAS3_PAIR_LABELS <- c(
  "Placement ×\noptical", "Optical ×\ntemporal", "Placement ×\ntemporal"
)
names(ATLAS3_PAIR_LABELS) <- PAIR_LEVELS
ATLAS3_CLASS_COLORS <- c("Overall" = "#343B3F", MS_METRIC_COLORS)

atlas3_class_counts <- metric_table |>
  mutate(metric_class = as.character(metric_class)) |>
  count(metric_class, name = "n_metrics")
atlas3_n_lookup <- c(
  Overall = nrow(metric_table),
  setNames(atlas3_class_counts$n_metrics, atlas3_class_counts$metric_class)
)
atlas3_row_labels <- setNames(
  paste0(
    c("Overall", str_to_sentence(ATLAS3_CLASS_ORDER[-1])),
    " (n=", unname(atlas3_n_lookup[ATLAS3_CLASS_ORDER]), ")"
  ),
  ATLAS3_CLASS_ORDER
)

atlas3_metric_q <- gamma_metric |>
  filter(is.finite(Q_metric)) |>
  mutate(
    pair_code = as.character(pair_code),
    metric_class = as.character(metric_class),
    atlas_class = metric_class
  ) |>
  select(pair_code, metric, metric_class, atlas_class, Q_metric)
atlas3_metric_q <- bind_rows(
  atlas3_metric_q |> filter(metric_class %in% ATLAS3_CLASS_ORDER[-1]),
  atlas3_metric_q |> mutate(atlas_class = "Overall")
) |>
  mutate(
    atlas_row = factor(unname(atlas3_row_labels[atlas_class]), levels = unname(atlas3_row_labels)),
    dimension_pair = factor(
      unname(ATLAS3_PAIR_LABELS[PAIR_CODE_TO_LABEL[pair_code]]),
      levels = unname(ATLAS3_PAIR_LABELS)
    )
  )

atlas3_q_limit <- max(atlas3_metric_q$Q_metric, na.rm = TRUE)
if (!is.finite(atlas3_q_limit) || atlas3_q_limit <= 0) atlas3_q_limit <- 1
atlas3_q_limit <- atlas3_q_limit * 1.06
atlas3_cells <- tidyr::expand_grid(atlas_class = ATLAS3_CLASS_ORDER, pair_code = PAIR_CODES) |>
  mutate(
    atlas_row = factor(unname(atlas3_row_labels[atlas_class]), levels = unname(atlas3_row_labels)),
    dimension_pair = factor(
      unname(ATLAS3_PAIR_LABELS[PAIR_CODE_TO_LABEL[pair_code]]),
      levels = unname(ATLAS3_PAIR_LABELS)
    )
  )
atlas3_stats <- atlas3_metric_q |>
  group_by(atlas_class, atlas_row, dimension_pair) |>
  summarise(
    Q_median = safe_median(Q_metric),
    Q_q25 = safe_q(Q_metric, .25),
    Q_q75 = safe_q(Q_metric, .75), .groups = "drop"
  )
atlas3_raw <- atlas3_metric_q |>
  arrange(atlas_class, dimension_pair, metric) |>
  group_by(atlas_class, dimension_pair) |>
  mutate(raw_y = .18 + .14 * ((row_number() * .61803398875) %% 1)) |>
  ungroup()

p3a <- ggplot() +
  geom_blank(data = atlas3_cells, aes(x = 0, y = 0), inherit.aes = FALSE) +
  geom_point(
    data = atlas3_raw,
    aes(Q_metric, raw_y, colour = atlas_class), shape = 16,
    size = .76, alpha = .78
  ) +
  geom_segment(
    data = atlas3_stats,
    aes(x = Q_q25, xend = Q_q75, y = .405, yend = .405, colour = atlas_class),
    linewidth = 1.62, lineend = "round"
  ) +
  geom_point(
    data = atlas3_stats,
    aes(Q_median, .405, fill = atlas_class), shape = 21,
    size = 1.92, colour = "#30383C", stroke = .25
  ) +
  scale_colour_manual(values = ATLAS3_CLASS_COLORS, guide = "none") +
  scale_fill_manual(values = ATLAS3_CLASS_COLORS, guide = "none") +
  scale_x_continuous(
    limits = c(0, atlas3_q_limit), breaks = scales::breaks_extended(n = 4),
    expand = expansion(mult = c(0, .015))
  ) +
  scale_y_continuous(limits = c(-.02, .54), breaks = NULL, expand = expansion(mult = c(0, 0))) +
  facet_grid(atlas_row ~ dimension_pair, scales = "fixed", drop = FALSE, switch = "y") +
  labs(
    title = "a  Class-level non-additivity distribution strips",
    subtitle = "Q_mp = median_t(Q_mpt); dots = metrics, thick line = IQR, point = class median",
    x = "median Q per metric", y = NULL
  ) +
  theme_rq2(base_size = 5.75) +
  theme(
    panel.grid = element_blank(), panel.spacing = grid::unit(.75, "mm"),
    strip.background = element_blank(),
    strip.text.x = element_text(size = 5.0, face = "bold", lineheight = .86),
    strip.text.y.left = element_text(size = 4.55, angle = 0, hjust = 1, lineheight = .86,
                                     margin = margin(r = 2)),
    strip.placement = "outside",
    axis.text.x = element_text(size = 4.0),
    axis.ticks.x = element_line(colour = "#505457", linewidth = .25),
    axis.title.x = element_text(size = 4.35),
    axis.line.x = element_line(colour = "#505457", linewidth = .30),
    plot.title = element_text(size = 6.25, hjust = 0, margin = margin(b = 2)),
    plot.subtitle = element_text(size = 4.15, colour = "#666A6D", hjust = 0,
                                 margin = margin(t = -1, b = 2)),
    plot.margin = margin(1, 3, 1, 3)
  )

b3_transition_pairs <- rep(unname(PAIR_CODES), c(2L, 5L, 10L))
b3_transition_order <- tibble(
  pair_code = b3_transition_pairs,
  transition = c(
    "chest_LIGHT_to_MEDI", "wrist_LIGHT_to_MEDI",
    "120to60", "60to40", "40to30", "30to20", "20to10",
    "chest_120to60", "chest_60to40", "chest_40to30", "chest_30to20", "chest_20to10",
    "wrist_120to60", "wrist_60to40", "wrist_40to30", "wrist_30to20", "wrist_20to10"
  ),
  x_label = c(
    "chest ·\nLIGHT → MEDI", "wrist ·\nLIGHT → MEDI",
    "120 → 60 s", "60 → 40 s", "40 → 30 s", "30 → 20 s", "20 → 10 s",
    rep(c("120 → 60 s", "60 → 40 s", "40 → 30 s", "30 → 20 s", "20 → 10 s"), 2)
  ),
  placement = c(rep("all", 7L), rep("chest", 5L), rep("wrist", 5L))
)

gamma_b3_raw <- gamma_plot |>
  filter(is.finite(Q), metric_class %in% ATLAS3_CLASS_ORDER[-1]) |>
  mutate(pair_code = as.character(pair_code), transition = as.character(transition)) |>
  left_join(b3_transition_order, by = c("pair_code", "transition"))

b3_class_stats <- gamma_plot |>
  filter(is.finite(Q)) |>
  mutate(
    pair_code = as.character(pair_code), transition = as.character(transition),
    metric_class = as.character(metric_class)
  ) |>
  group_by(pair_code, transition, metric_class) |>
  summarise(Q_median = safe_median(Q), Q_q25 = safe_q(Q, .25), Q_q75 = safe_q(Q, .75), .groups = "drop")
b3_overall_stats <- gamma_plot |>
  filter(is.finite(Q)) |>
  mutate(pair_code = as.character(pair_code), transition = as.character(transition)) |>
  group_by(pair_code, transition) |>
  summarise(Q_median = safe_median(Q), Q_q25 = safe_q(Q, .25), Q_q75 = safe_q(Q, .75), .groups = "drop") |>
  mutate(metric_class = "Overall")
b3_transition_stats <- bind_rows(b3_overall_stats, b3_class_stats) |>
  left_join(b3_transition_order, by = c("pair_code", "transition"))

b3_q_limit <- max(c(gamma_b3_raw$Q, b3_transition_stats$Q_q75), na.rm = TRUE)
if (!is.finite(b3_q_limit) || b3_q_limit <= 0) b3_q_limit <- 1
b3_q_limit <- b3_q_limit * 1.08
B3_CLASS_COLORS <- c("Overall" = "#343B3F", MS_METRIC_COLORS)
B3_CLASS_ORDER <- ATLAS3_CLASS_ORDER[-1]
B3_CLASS_OFFSETS <- setNames(seq(-.27, .27, length.out = length(B3_CLASS_ORDER)), B3_CLASS_ORDER)
metric_legend_main <- cowplot::get_legend(
  ggplot(
    tibble(metric_class = factor(B3_CLASS_ORDER, levels = B3_CLASS_ORDER), x = 1, y = 1),
    aes(x, y, colour = metric_class)
  ) +
    geom_point(size = 1.05) +
    scale_colour_manual(values = MS_METRIC_COLORS, limits = B3_CLASS_ORDER) +
    guides(colour = guide_legend(title = NULL, nrow = 1, byrow = TRUE,
                                 override.aes = list(size = 1.05, alpha = 1))) +
    theme_void(base_family = MS_FONT) +
    theme(
      legend.position = "bottom", legend.text = element_text(size = 3.75),
      legend.key.width = grid::unit(2.1, "mm"), legend.spacing.x = grid::unit(.45, "mm"),
      legend.margin = margin(0, 0, 0, 0)
    )
)

b3_quasirandom_offset <- function(n, width = .13) {
  if (n <= 1L) return(0)
  (((seq_len(n) * .61803398875) %% 1) - .5) * 2 * width
}

make_b3_row_plot <- function(pair_name, placement_name = "all", section_title,
                             show_y = TRUE, show_x_title = FALSE) {
  transitions <- b3_transition_order |>
    filter(pair_code == pair_name, placement == placement_name) |>
    mutate(row_y = rev(seq_len(n())))
  raw <- gamma_b3_raw |>
    filter(pair_code == pair_name, placement == placement_name) |>
    left_join(transitions |> select(transition, row_y), by = "transition") |>
    arrange(row_y, Q, metric) |>
    group_by(transition) |>
    mutate(raw_y = row_y + b3_quasirandom_offset(n(), .13)) |>
    ungroup()
  stats <- b3_transition_stats |>
    filter(pair_code == pair_name, placement == placement_name) |>
    left_join(transitions |> select(transition, row_y), by = "transition") |>
    mutate(summary_y = row_y + if_else(metric_class == "Overall", 0, unname(B3_CLASS_OFFSETS[metric_class])))
  stats_class <- stats |> filter(metric_class %in% B3_CLASS_ORDER)
  stats_overall <- stats |> filter(metric_class == "Overall")

  ggplot() +
    geom_point(data = raw, aes(Q, raw_y), shape = 16, size = .34, alpha = .15, colour = "#707B80") +
    geom_segment(
      data = stats_overall,
      aes(x = Q_q25, xend = Q_q75, y = summary_y, yend = summary_y),
      colour = B3_CLASS_COLORS[["Overall"]], linewidth = 2.35, lineend = "round"
    ) +
    geom_point(
      data = stats_overall, aes(Q_median, summary_y), shape = 21, size = 2.35,
      fill = B3_CLASS_COLORS[["Overall"]], colour = "white", stroke = .26
    ) +
    geom_segment(
      data = stats_class,
      aes(x = Q_q25, xend = Q_q75, y = summary_y, yend = summary_y, colour = metric_class),
      linewidth = .56, alpha = .86, lineend = "round"
    ) +
    geom_point(
      data = stats_class, aes(Q_median, summary_y, colour = metric_class),
      shape = 16, size = 1.02, alpha = .96
    ) +
    scale_colour_manual(values = B3_CLASS_COLORS, limits = names(B3_CLASS_COLORS), guide = "none") +
    scale_x_continuous(
      limits = c(0, b3_q_limit), breaks = scales::breaks_extended(n = 4),
      expand = expansion(mult = c(0, .015))
    ) +
    scale_y_continuous(
      limits = c(.34, nrow(transitions) + .66),
      breaks = transitions$row_y, labels = transitions$x_label,
      expand = expansion(mult = c(0, 0))
    ) +
    labs(
      title = section_title,
      x = if (show_x_title) "Q = mean(|gamma|)" else NULL,
      y = if (show_y) "ordered transition" else NULL
    ) +
    theme_rq2(base_size = 5.35) +
    theme(
      panel.grid.major.y = element_blank(),
      panel.grid.major.x = element_line(colour = "#ECEFF0", linewidth = .18),
      axis.text.y = if (show_y) element_text(size = 3.95, lineheight = .82) else element_blank(),
      axis.text.x = element_text(size = 3.85),
      axis.title.y = if (show_y) element_text(size = 4.0, margin = margin(r = 2)) else element_blank(),
      axis.title.x = if (show_x_title) element_text(size = 4.2, margin = margin(t = 2)) else element_blank(),
      axis.ticks.y = if (show_y) element_line(colour = "#505457", linewidth = .20) else element_blank(),
      axis.ticks.x = element_line(colour = "#505457", linewidth = .24),
      axis.line.y = element_blank(), axis.line.x = element_line(colour = "#505457", linewidth = .28),
      plot.title = element_text(size = 5.0, face = "bold", hjust = 0, margin = margin(b = 1)),
      plot.margin = margin(0, 3, 0, 3)
    )
}

p3b_po <- make_b3_row_plot(PAIR_CODES[[1]], section_title = "Placement × optical", show_y = TRUE)
p3b_ot <- make_b3_row_plot(PAIR_CODES[[2]], section_title = "Optical × temporal", show_y = TRUE)
p3b_pt_chest <- make_b3_row_plot(
  PAIR_CODES[[3]], placement_name = "chest", section_title = "Chest",
  show_y = TRUE, show_x_title = TRUE
)
p3b_pt_wrist <- make_b3_row_plot(
  PAIR_CODES[[3]], placement_name = "wrist", section_title = "Wrist",
  show_y = FALSE, show_x_title = FALSE
)
p3b_pt <- cowplot::plot_grid(
  p3b_pt_chest, p3b_pt_wrist, ncol = 2, rel_widths = c(1, 1),
  align = "hv", axis = "tblr", greedy = TRUE
)
p3b_header <- cowplot::ggdraw() +
  cowplot::draw_label(
    "b  Ordered-transition backbone with class overlays",
    x = 0, y = .70, hjust = 0, vjust = .5, fontfamily = MS_FONT,
    fontface = "bold", size = 6.2, colour = "#151515"
  ) +
  cowplot::draw_label(
    "overall = median + IQR; coloured marks = class medians + IQR; faint points = metric-level Q",
    x = 0, y = .20, hjust = 0, vjust = .5, fontfamily = MS_FONT,
    size = 4.0, colour = "#666A6D"
  )
p3b <- cowplot::plot_grid(
  p3b_header, p3b_po, p3b_ot, p3b_pt, ncol = 1,
  rel_heights = c(.28, .90, 1.32, 1.56), align = "v", axis = "l", greedy = TRUE
)

p3_bottom <- cowplot::plot_grid(
  p3b, p3c, ncol = 2, rel_widths = c(.75, .25),
  align = "hv", axis = "tblr", greedy = TRUE
)
p3 <- cowplot::plot_grid(
  metric_legend_main,
  cowplot::plot_grid(
    p3a, p3_bottom, ncol = 1, rel_heights = c(1.08, 1.20),
    align = "v", axis = "l", greedy = TRUE
  ),
  ncol = 1, rel_heights = c(.045, 1), align = "v", axis = "l", greedy = TRUE
)
ms_plot_save(p3, file.path(OUT_DIR, "Fig3_RQ2.png"), 7.20, 7.05)
message("Fig. 3 complete: cross-dimensional non-additivity.")
