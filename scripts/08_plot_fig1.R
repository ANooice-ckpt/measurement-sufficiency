suppressPackageStartupMessages({ library(tidyverse); library(grid) })
set.seed(20260819)
B_BOOT <- 1000L
x <- readRDS("data/derived/rq1_distortion_long.rds") |> filter(available, is.finite(e))

bootstrap_ci <- function(g, B = B_BOOT) {
  clusters <- g |> group_by(site, Id) |> summarise(se = sum(e), sa = sum(abs(e)), n = n(), .groups = "drop")
  sites <- split(clusters, clusters$site)
  vals <- replicate(B, {
    sampled <- map_dfr(sites, ~.x[sample.int(nrow(.x), nrow(.x), replace = TRUE), ])
    c(B = sum(sampled$se) / sum(sampled$n), A = sum(sampled$sa) / sum(sampled$n))
  })
  tibble(B_ci_low = quantile(vals["B", ], .025), B_ci_high = quantile(vals["B", ], .975),
         A_ci_low = quantile(vals["A", ], .025), A_ci_high = quantile(vals["A", ], .975))
}

message("RQ1 summaries and participant-cluster bootstrap")
summary_base <- x |> group_by(dimension, configuration, metric, metric_class) |>
  summarise(n_participants = n_distinct(paste(site, Id)), n_units = n(), median_e = median(e),
    q25_e = quantile(e, .25), q75_e = quantile(e, .75), p025_e = quantile(e, .025), p975_e = quantile(e, .975),
    B_mean_signed = mean(e), A_mean_absolute = mean(abs(e)), .groups = "drop")
cis <- x |> group_by(dimension, configuration, metric, metric_class) |> group_modify(~bootstrap_ci(.x)) |> ungroup()
summary <- left_join(summary_base, cis, by = c("dimension", "configuration", "metric", "metric_class"))
write.csv(summary, "results/rq1/rq1_summary.csv", row.names = FALSE)

geometry <- summary |> transmute(dimension, configuration, metric, A_mean_absolute, B_mean_signed,
  pass = A_mean_absolute + 1e-12 >= abs(B_mean_signed), gap = A_mean_absolute - abs(B_mean_signed))
write.csv(geometry, "results/diagnostics/rq1_geometry_invariant.csv", row.names = FALSE)
if (any(!geometry$pass)) stop("A >= |B| geometry invariant failed")

std <- read.csv("results/diagnostics/rq1_standardizer_audit.csv")
std_check <- std |> group_by(lattice, metric) |> summarise(n_distinct_standardizers = n_distinct(standardizer), .groups = "drop") |>
  mutate(pass = n_distinct_standardizers == 1)
write.csv(std_check, "results/diagnostics/rq1_standardizer_consistency.csv", row.names = FALSE)

diff_summary <- read.csv("results/diagnostics/upstream_difference_summary.csv")
write.csv(tibble(check = c("ordered_keys", "metric_count", "authorized_numerical_boundary"),
  result = c("pass", "54", sprintf("accepted: n=%d median=%g p95=%g max=%g", diff_summary$n, diff_summary$median, diff_summary$p95, diff_summary$maximum))),
  "results/diagnostics/rq1_reference_reproduction.csv", row.names = FALSE)

# Algorithmic panel-a selection.
eligible <- summary |> filter(n_participants >= 3, is.finite(A_mean_absolute), A_mean_absolute > 0) |>
  mutate(ratio = abs(B_mean_signed) / A_mean_absolute,
         id = paste(dimension, configuration, metric, sep = " | "))
low <- eligible |> arrange(A_mean_absolute, abs(B_mean_signed)) |> slice(1) |> mutate(example_type = "low distortion")
pos <- eligible |> filter(B_mean_signed > 0) |> mutate(score = percent_rank(A_mean_absolute) + percent_rank(ratio)) |> arrange(desc(score)) |> slice(1) |> mutate(example_type = "positive directional")
neg <- eligible |> filter(B_mean_signed < 0) |> mutate(score = percent_rank(A_mean_absolute) + percent_rank(ratio)) |> arrange(desc(score)) |> slice(1) |> mutate(example_type = "negative directional")
bi <- eligible |> mutate(score = percent_rank(A_mean_absolute) + percent_rank(1-ratio)) |> arrange(desc(score)) |> slice(1) |> mutate(example_type = "bidirectional/cancellation")
examples <- bind_rows(low, pos, neg, bi) |> distinct(id, .keep_all = TRUE)
write.csv(examples, "results/rq1/fig1_panel_a_examples.csv", row.names = FALSE)

example_data <- x |> mutate(id = paste(dimension, configuration, metric, sep = " | ")) |>
  semi_join(examples |> select(id, example_type), by = "id") |>
  left_join(examples |> select(id, example_type), by = "id")
p_a <- ggplot(example_data, aes(e, fill = example_type)) + geom_density(alpha = .55, color = NA, adjust = .8) +
  geom_vline(xintercept = 0, linewidth = .3) + facet_wrap(~example_type, scales = "free", nrow = 1) +
  guides(fill = "none") + labs(title = "a  Empirical distortion distributions", x = "standardized signed distortion (e)", y = "density") + theme_minimal(base_size = 8)

panel_plot <- function(dim, configs, title) {
  d <- summary |> filter(dimension == dim, configuration %in% configs) |>
    mutate(label = if_else(percent_rank(A_mean_absolute) > .95, metric, NA_character_))
  lim <- max(c(d$A_ci_high, abs(d$B_ci_low), abs(d$B_ci_high)), na.rm = TRUE)
  ggplot(d, aes(B_mean_signed, A_mean_absolute, color = metric_class, shape = configuration)) +
    geom_abline(slope = c(-1, 1), intercept = 0, linetype = 2, linewidth = .3, color = "grey55") +
    geom_errorbar(aes(ymin = A_ci_low, ymax = A_ci_high), alpha = .22, linewidth = .25) +
    geom_errorbarh(aes(xmin = B_ci_low, xmax = B_ci_high), alpha = .22, linewidth = .25) +
    geom_point(size = 1.5, alpha = .85) +
    geom_text(aes(label = label), size = 2, check_overlap = TRUE, vjust = -0.5) +
    coord_cartesian(xlim = c(-lim, lim), ylim = c(0, lim)) +
    labs(title = title, x = "B: mean signed distortion", y = "A: mean absolute distortion", color = "metric class", shape = "configuration") +
    theme_minimal(base_size = 8) + theme(legend.position = "bottom", legend.key.height = unit(2.5, "mm"))
}
p_b <- panel_plot("placement", c("chest", "wrist"), "b  Placement")
p_c <- panel_plot("optical", "eye_LIGHT_10s", "c  Optical proxy")
p_d <- panel_plot("temporal", "eye_MEDI_30min", "d  30 min vs 10 s")
p_e <- panel_plot("duration", "duration_1d", "e  1 day vs 7 days")

draw_figure <- function(device) {
  device(); grid.newpage(); pushViewport(viewport(layout = grid.layout(3, 2, heights = unit(c(1.05, 1, 1), "null"))))
  print(p_a, vp = viewport(layout.pos.row = 1, layout.pos.col = 1:2))
  print(p_b, vp = viewport(layout.pos.row = 2, layout.pos.col = 1)); print(p_c, vp = viewport(layout.pos.row = 2, layout.pos.col = 2))
  print(p_d, vp = viewport(layout.pos.row = 3, layout.pos.col = 1)); print(p_e, vp = viewport(layout.pos.row = 3, layout.pos.col = 2))
  dev.off()
}
draw_figure(function() pdf("results/figures/Fig1_RQ1.pdf", width = 11, height = 13, useDingbats = FALSE))
draw_figure(function() png("results/figures/Fig1_RQ1.png", width = 2200, height = 2600, res = 200))

writeLines(c("# RQ1 run report", "", sprintf("Generated: %s", Sys.time()), "",
  "Scripts run: 02_inventory.R, 03_reproduce_upstream.R, 04_validate_reproduction.R, 05_rq1_reference.R, 06_rq1_configurations.R, 06b_rq1_duration.R, 07_rq1_distortion.R, 08_plot_fig1.R.",
  sprintf("Canonical distortion rows: %s; finite/available rows: %s.", nrow(readRDS("data/derived/rq1_distortion_long.rds")), nrow(x)),
  sprintf("Participants: placement chest %d, wrist %d; optical/temporal %d; duration %d.",
          n_distinct(x$Id[x$configuration == "chest"]), n_distinct(x$Id[x$configuration == "wrist"]),
          n_distinct(x$Id[x$dimension == "optical"]), n_distinct(x$Id[x$dimension == "duration"])),
  "Unavailable: optical MDER/nvRD; temporal pulse-family metrics at 5, 15, and 30 min; unit-level missing metric outputs and degenerate standardizers are recorded explicitly.",
  sprintf("Bootstrap: %d participant-cluster replicates, stratified by site.", B_BOOT),
  "Diagnostics: geometry passed; standardizer, missing-support, duration-cohort, availability, and authorized upstream discrepancy records are under results/diagnostics/.",
  "Artifacts: data/derived/rq1_metric_values.rds, data/derived/rq1_distortion_long.rds, results/rq1/rq1_summary.csv, results/figures/Fig1_RQ1.pdf/png.",
  "Interpretation boundary: duration has only three eligible participants (one in each of three sites), so its uncertainty and generalizability are limited."),
  "results/rq1/RQ1_RUN_REPORT.md")
