#!/usr/bin/env Rscript
# Results-supporting appendix: one table and five orthogonal figures.
# Rscript scripts/14_rq3_analysis.R --appendix
# Rscript scripts/14_rq3_analysis.R --appendix-robustness
# Rscript scripts/14_rq3_analysis.R --appendix-phase
# Rscript scripts/16_appendix_figures.R         # frozen inputs only
# MS_PLOT_PREP_ONLY=1 constructs everything without writing output.
options(encoding = "UTF-8")
if (.Platform$OS.type == "windows") invisible(suppressWarnings(Sys.setlocale("LC_CTYPE", "English_United States.utf8")))
.ms_file <- grep("^--file=", commandArgs(FALSE), value = TRUE)
if (length(.ms_file)) {
  .ms_script <- normalizePath(sub("^--file=", "", .ms_file[[1]]), winslash = "/", mustWork = TRUE)
  setwd(normalizePath(file.path(dirname(.ms_script), ".."), winslash = "/", mustWork = TRUE))
}
suppressPackageStartupMessages({ library(tidyverse); library(cowplot) })
source("scripts/utils/analysis_design.R")
source("scripts/utils/core_artifacts.R")
source("scripts/utils/artifact_validation.R")
source("scripts/utils/figure_style.R")
source("scripts/utils/plot_contracts.R")
source("scripts/utils/rq1_inference_contract.R")
source("scripts/utils/rq2_reliability_contract.R")

FIG_DIR <- "results/figures"
APP_DIR <- "results/appendix"
appendix_registry <- tibble::tribble(
  ~id, ~filename, ~title, ~main_figures, ~evidence_role,
  "Table S1", "TableS1_analysis_support.html", "Analysis support and metric availability", "1–6", "Comparable observations and unavailable targets",
  "Fig. S1", "FigS1_distortion_robustness.png", "Standardization, weighting and joint-decision sensitivity", "1,6", "Standardization, participant weighting and All-54 decision scales",
  "Fig. S2", "FigS2_reference_strength.png", "Association preservation across reference-signal strengths", "2", "Dependence on reference association strength",
  "Fig. S3", "FigS3_context_validation.png", "Out-of-sample validation of contextual reliability", "3–4", "Held-out performance and risk-group calibration",
  "Fig. S4", "FigS4_sampling_phase_sensitivity.png", "Sampling-phase sensitivity of adjacent joint designs", "6", "Both 20-s source-grid phases at the three All-54 coarser neighbours",
  "Fig. S5", "FigS5_joint_target_sensitivity.png", "Target-composition and participant-sampling sensitivity of joint designs", "6", "Leave-one-target-out frontiers and participant-bootstrap sufficiency"
)
stopifnot(nrow(appendix_registry) == 6L, !anyDuplicated(appendix_registry$id))

input_paths <- c(
  summary = "results/rq1/rq1_pairwise_summary.csv",
  robust = "results/rq1/rq1_robust_scale_sensitivity.csv",
  balanced = "results/rq1/rq1_participant_balanced_sensitivity.csv",
  availability = "results/rq1/rq1_metric_availability.csv",
  standardizers = "results/diagnostics/rq1_standardizer_audit.csv",
  upstream = "results/rq1/rq1_pairwise_change_long.rds",
  inference = file.path("results/rq1/inference", rq1_inference_contract()$artifact_filename),
  context = "results/rq2/rq2_layered_context_model_performance.csv",
  context_manifest = "results/rq2/rq2_layered_context_model_manifest.csv",
  reliability = "results/rq2/rq2_conditional_reliability.rds",
  single = "results/rq3/rq3_observed_stability.csv",
  requirement = "results/rq3/rq3_single_dimension_requirement.csv",
  joint = "results/rq3/rq3_joint_summary.csv",
  joint_sensitivity = "results/rq3/rq3_appendix_joint_target_sensitivity.rds",
  robustness = "results/rq3/appendix_robustness/rq3_appendix_robustness.rds",
  source_audit = "results/diagnostics/core_source_sampling_audit.csv",
  cohort = "results/diagnostics/duration_cohort_audit.csv",
  windows = "results/diagnostics/duration_window_manifest_audit.csv"
)
ms_plot_require_files(input_paths, "Appendix input (prepare RQ3 with 14_rq3_analysis.R --appendix)")
read_input <- function(key) readr::read_csv(input_paths[[key]], show_col_types = FALSE, progress = FALSE)
rq1 <- read_input("summary")
CORE_VERSION <- ms_plot_assert_core(rq1$core_artifact_version)
RQ1_VERSION <- ms_plot_one_version(rq1$rq1_analysis_version, "RQ1 appendix input")
ms_plot_assert_prefix(RQ1_VERSION, "rq1_v7_", "RQ1 appendix input")
validate_versions <- function(x, label) {
  ms_plot_assert_core(x$core_artifact_version, CORE_VERSION)
  if (!identical(ms_plot_one_version(x$rq1_analysis_version, label), RQ1_VERSION)) stop(label, " upstream mismatch")
}
upstream <- readRDS(input_paths[["upstream"]])
validate_versions(upstream, "RQ1 manifest")
robust <- read_input("robust"); balanced <- read_input("balanced")
availability <- read_input("availability"); validate_versions(availability, "RQ1 availability")
inference <- readRDS(input_paths[["inference"]])
validate_versions(inference, "RQ1 inference")
ms_assert_version(inference, "analysis_design_id", ms_analysis_design_id())
ms_plot_assert_prefix(inference$rq1_inference_version, "rq1_inference_v8_geometry_anchor8__", "RQ1 inference")
if (nrow(inference$anchor_map) != 8L || n_distinct(inference$contrast_summary$metric) != 52L) stop("Invalid inference scope")
reliability <- readRDS(input_paths[["reliability"]])
rq2_reliability_assert_artifact(reliability, upstream, CORE_VERSION)
context <- read_input("context")
context_manifest <- read_input("context_manifest"); validate_versions(context_manifest, "RQ2 contextual models")
single <- read_input("single"); validate_versions(single, "RQ3 single-axis")
joint <- read_input("joint"); validate_versions(joint, "RQ3 joint")
RQ3_VERSION <- ms_plot_one_version(joint$rq3_analysis_version, "RQ3 appendix input")
if (!identical(RQ3_VERSION, ms_plot_one_version(single$rq3_analysis_version, "RQ3 single-axis")) ||
    !identical(RQ3_VERSION, paste0("rq3_v10_min2day_is__", RQ1_VERSION, "__", ms_analysis_design_id()))) stop("RQ3 version mismatch")
joint_sensitivity <- readRDS(input_paths[["joint_sensitivity"]])
validate_versions(joint_sensitivity, "RQ3 appendix target sensitivity")
ms_assert_version(joint_sensitivity, "rq3_analysis_version", RQ3_VERSION)
ms_assert_version(joint_sensitivity, "analysis_design_id", ms_analysis_design_id())
ms_assert_version(joint_sensitivity, "appendix_analysis_version", "appendix_joint_target_v1")
if (!identical(as.character(tools::md5sum(names(joint_sensitivity$input_md5))),
               as.character(joint_sensitivity$input_md5))) stop("Stale target sensitivity; rerun 14_rq3_analysis.R --appendix")
if (any(single$status != "resolved" & is.finite(single$R_obs)) ||
    any(joint$status != "resolved" & is.finite(joint$R_obs))) stop("Unresolved states must retain NA")
robustness <- readRDS(input_paths[["robustness"]])
validate_versions(robustness,"RQ3 appendix robustness")
ms_assert_version(robustness,"rq3_analysis_version",RQ3_VERSION)
if(!identical(robustness$artifact_version,"appendix_robustness_v1") ||
   !identical(robustness$phase_status,"complete") ||
   !identical(as.character(tools::md5sum(names(robustness$input_md5))),as.character(robustness$input_md5))) {
  stop("Missing/stale robustness analysis; run 14_rq3_analysis.R --appendix-robustness then --appendix-phase")
}
phase_hashes <- do.call(c,robustness$phase_raw_md5)
if(any(vapply(split(as.character(phase_hashes),names(phase_hashes)),function(x) length(unique(x))!=1L,logical(1)))) {
  stop("Inconsistent phase source provenance")
}
phase_hashes <- phase_hashes[!duplicated(names(phase_hashes))]
if(!identical(as.character(tools::md5sum(names(phase_hashes))),as.character(phase_hashes))) stop("Phase source provenance changed")

DIMENSIONS <- c("placement", "optical", "temporal", "duration")
DIM_LABELS <- c("Placement", "Optical", "Sampling interval", "Monitoring days")
dimension_factor <- function(x) factor(x, levels = DIMENSIONS, labels = DIM_LABELS)
qfinite <- function(x, p = .5) {
  x <- x[is.finite(x)]; if (length(x)) unname(quantile(x, p)) else NA_real_
}
fig_note <- function(text) ggdraw() + draw_label(text, x = .02, hjust = 0,
  size = MS_FIG_DENSE_PT, fontfamily = MS_FONT, lineheight = 1.12)
compose <- function(panels, titles, note, heights = rep(1, length(panels))) {
  framed <- Map(function(p, title) ms_panel_frame(p, title), panels, titles)
  plot_grid(plotlist = c(framed, list(fig_note(note))), ncol = 1,
            rel_heights = c(heights, .17))
}
write_display <- function(x, filename) {
  if (!ms_plot_prep_only()) {
    readr::write_csv(x, file.path(APP_DIR, filename), na = "")
  }
}
if (!ms_plot_prep_only()) {
  dir.create(APP_DIR, recursive = TRUE, showWarnings = FALSE)
}

# Table S1: counts, coverage and availability; no effect estimates or pooled N.
cohort <- read_input("cohort") |> filter(support_id %in% c("eye_medi", "eye_full",
  "eye_chest_medi", "eye_chest_full", "eye_wrist_medi", "eye_wrist_full"))
windows <- read_input("windows") |> filter(support_id %in% cohort$support_id)
source_audit <- read_input("source_audit"); ms_plot_assert_core(source_audit$core_artifact_version, CORE_VERSION)
fmt_range <- function(x) {
  x <- x[is.finite(x)]
  if (!length(x)) return("Not recorded")
  r <- range(x); if (r[1] == r[2]) scales::comma(r[1]) else paste(scales::comma(r), collapse = "–")
}
core_rows <- cohort |> transmute(analysis = "Core complete-day eligibility", comparison = "Native 10-s support",
  support = support_id, participants = scales::comma(participants), sites = as.character(sites),
  analysis_units = paste0(scales::comma(complete_days), " participant-days"),
  metric_availability = "Before metric-specific missing values", status = "Eligible support")
six_day_rows <- windows |> filter(n_days == 6L) |> group_by(support_id) |>
  summarise(participants = n_distinct(paste(site, Id)), sites = n_distinct(site),
            analysis_units = paste0(scales::comma(n()), " contiguous 6-d windows"), .groups = "drop") |>
  transmute(analysis = "Core six-day eligibility", comparison = "All legal 6-d windows",
    support = support_id, participants = scales::comma(participants), sites = as.character(sites),
    analysis_units, metric_availability = "Targets retain individual maximal supports", status = "Eligible support")
rq1_rows <- availability |> group_by(dimension, comparison_pair_id) |>
  summarise(participants = fmt_range(n_participants_available),
    analysis_units = paste0(fmt_range(n_available_units), if (first(dimension) == "duration") " nested-window comparisons / target" else " paired participant-days / target"),
    n_targets = n_distinct(metric), n_available = n_distinct(metric[n_available_units > 0]),
    unavailable = paste(sort(unique(na.omit(unavailable_reason))), collapse = "; "), .groups = "drop") |>
  transmute(analysis = "RQ1 representation change", comparison = comparison_pair_id,
    support = case_when(dimension == "placement" & grepl("chest", comparison_pair_id) ~ "eye–chest maximal supports",
      dimension == "placement" ~ "eye–wrist maximal supports", dimension == "optical" ~ "eye_full",
      dimension == "temporal" ~ "eye maximal supports", TRUE ~ "Configuration-specific maximal supports; pooled comparisons"),
    participants, analysis_units,
    metric_availability = paste0(n_available, "/", n_targets, " targets with available pairs",
      if_else(nzchar(unavailable), paste0("; ", unavailable), "")), status = "Metric-specific ranges") |>
  left_join(rq1 |> group_by(comparison_pair_id) |>
    summarise(sites = fmt_range(bootstrap_sites), .groups = "drop"), by = c("comparison" = "comparison_pair_id"))
matched <- as_tibble(inference$contrast_summary)
matched_counts <- as_tibble(inference$support) |>
  group_by(candidate_config, metric, outcome) |>
  summarise(n_sites = n_distinct(site), support_days = n(),
    support_participants = n_distinct(paste(site, Id)), .groups = "drop")
matched <- matched |> left_join(matched_counts,
  by = c("candidate_config", "metric", "outcome"), relationship = "one-to-one")
estimated <- matched |> filter(status == "estimated")
if (any(estimated$n_matched_days != estimated$support_days) ||
    any(estimated$n_participants != estimated$support_participants)) stop("Frozen inference support counts disagree")
inference_rows <- matched |> group_by(contrast_label, outcome_domain, support_id) |>
  summarise(participants = fmt_range(n_participants[status == "estimated"]),
    sites = fmt_range(n_sites[status == "estimated"]),
    analysis_units = paste0(fmt_range(n_matched_days[status == "estimated"]), " matched participant-days / task"),
    n_estimated = sum(status == "estimated"), n_tasks = n(),
    .groups = "drop") |>
  transmute(analysis = paste0("RQ1 inference: ", outcome_domain), comparison = contrast_label,
    support = support_id, participants, sites,
    analysis_units, metric_availability = paste0(n_estimated, "/", n_tasks, " estimated metric–outcome tasks"),
    status = "Exactly paired fitting support")
source_rows <- source_audit |> filter(support_id %in% cohort$support_id) |>
  distinct(support_id, site, Id, support_available) |> group_by(support_id) |>
  summarise(participants = as.character(sum(!support_available)), sites = as.character(n_distinct(site[!support_available])), .groups = "drop") |>
  transmute(analysis = "Native-source exclusions", comparison = "Required source ineligible", support = support_id,
    participants, sites, analysis_units = "Excluded participant/support identities",
    metric_availability = "Unavailable on the affected source support", status = "Source audit")
TableS1 <- bind_rows(core_rows, six_day_rows, rq1_rows, inference_rows, source_rows)
write_display(TableS1, "TableS1_analysis_support.csv")
html_escape <- function(x) {
  x <- gsub("&", "&amp;", as.character(x), fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE); gsub(">", "&gt;", x, fixed = TRUE)
}
table_rows <- apply(as.data.frame(TableS1), 1, function(row)
  paste0("<tr><td>", paste(html_escape(row), collapse = "</td><td>"), "</td></tr>"))
if (!ms_plot_prep_only()) {
  writeLines(c('<!doctype html><html lang="en"><meta charset="utf-8"><title>Table S1</title>',
    '<style>body{font-family:Arial,sans-serif;margin:32px;color:#25313a;font-size:12px}h1{font-size:20px}table{border-collapse:collapse;width:100%}th,td{padding:8px;text-align:left;border-bottom:1px solid #dde3e7;vertical-align:top}th{background:#edf3f7;position:sticky;top:0}tr:nth-child(even){background:#f8fafb}p{line-height:1.5;max-width:1050px}@media print{th{position:static}tr{break-inside:avoid}body{margin:12px}}</style>',
    '<h1>Table S1 · Analysis support and metric availability</h1>',
    '<p>Counts are specific to the stated analysis units, targets, contrasts and windows. Ranges describe metric/outcome-specific availability. Complete-day eligibility, target availability and outcome matching define successive analysis supports. MEDI All-54 designs retain the ordinary-target and spectrum-target maximal supports separately. RQ1 site counts describe the task bootstrap cohort; inference site counts describe the exact matched support.</p>',
    paste0('<table><thead><tr><th>', paste(html_escape(names(TableS1)), collapse = '</th><th>'), '</th></tr></thead><tbody>'),
    table_rows, '</tbody></table></html>'), file.path(APP_DIR, "TableS1_analysis_support.html"), useBytes = TRUE)
}

# Fig. S1: compare the same scientific keys and supports before display pooling.
pair_keys <- c("dimension", "comparison_lattice", "comparison_pair_id", "config_a_id", "config_b_id", "metric")
for (z in list(robust, balanced)) {
  ms_plot_require_columns(z, pair_keys, "RQ1 sensitivity")
  if (all(c("core_artifact_version", "rq1_analysis_version") %in% names(z))) validate_versions(z, "RQ1 sensitivity")
  if (anyDuplicated(z[pair_keys])) stop("Duplicated RQ1 sensitivity keys")
  if (nrow(anti_join(z, rq1, by = pair_keys))) stop("Sensitivity includes pairs outside current RQ1")
}
if (nrow(anti_join(rq1, balanced, by = pair_keys))) stop("Participant-balanced sensitivity lacks primary pairs")
missing_robust <- anti_join(rq1, robust, by = pair_keys) |>
  left_join(read_input("standardizers") |> select(comparison_lattice, metric, robust_standardizer),
    by = c("comparison_lattice", "metric"), relationship = "many-to-one")
if (any(is.finite(missing_robust$robust_standardizer) & missing_robust$robust_standardizer > 0)) {
  stop("Positive robust scales have missing sensitivity summaries")
}
s1_pairs <- rq1 |> select(all_of(pair_keys), metric_class, A_mean_absolute, n_units, n_participants) |>
  left_join(robust |> select(all_of(pair_keys), A_robust, robust_n_units = n_units), by = pair_keys, relationship = "one-to-one") |>
  left_join(balanced |> select(all_of(pair_keys), A_participant_balanced, balanced_n_participants = n_participants), by = pair_keys, relationship = "one-to-one")
if (any(s1_pairs$n_units != s1_pairs$robust_n_units, na.rm = TRUE) ||
    any(s1_pairs$n_participants != s1_pairs$balanced_n_participants)) stop("Sensitivity support counts differ from primary RQ1")
s1_metrics <- s1_pairs |> group_by(dimension, metric, metric_class) |>
  summarise(across(c(A_mean_absolute, A_robust, A_participant_balanced), ~qfinite(.x)),
    n_pair_types = n(), .groups = "drop") |> mutate(dimension = dimension_factor(dimension))
s1_pairs <- s1_pairs |> mutate(robust_available = is.finite(A_robust),
  robust_status = if_else(robust_available, "available", "undefined robust standardizer"))
s1_scale_metrics <- s1_pairs |> filter(robust_available) |>
  group_by(dimension, metric, metric_class) |>
  summarise(across(c(A_mean_absolute, A_robust), ~qfinite(.x)), n_pair_types = n(), .groups = "drop") |>
  mutate(dimension = dimension_factor(dimension))
s1_scale <- s1_scale_metrics |> pivot_longer(c(A_mean_absolute, A_robust), names_to = "standardizer", values_to = "A") |>
  mutate(standardizer = factor(standardizer, levels = c("A_mean_absolute", "A_robust"), labels = c("SD", "IQR / 1.349")))
s1a <- ggplot(s1_scale, aes(standardizer, A, group = metric, colour = metric_class)) +
  geom_line(alpha = .35, linewidth = .3) + geom_point(size = .7, alpha = .65) +
  facet_wrap(~dimension, nrow = 1) + scale_colour_ms_metric() +
  scale_y_continuous(trans = scales::pseudo_log_trans(sigma = .03), breaks = c(0, .1, .3, 1, 3, 10, 30)) +
  labs(x = NULL, y = "Metric median A across pair types", colour = "Metric class") + theme_ms_axes(legend_position = "bottom")
s1_metrics <- s1_metrics |> mutate(weight_change = A_participant_balanced - A_mean_absolute)
s1_labels <- s1_metrics |> group_by(dimension) |> slice_max(abs(weight_change), n = 2, with_ties = FALSE) |> ungroup() |>
  mutate(label = str_wrap(str_replace_all(metric, "_", " "), width = 17),
         label_x = pmin(A_mean_absolute, .75 * max(s1_metrics$A_mean_absolute)))
s1b <- ggplot(s1_metrics, aes(A_mean_absolute, A_participant_balanced, colour = metric_class)) +
  geom_abline(slope = 1, intercept = 0, colour = "#A9B1B7", linewidth = .35) + geom_point(size = 1.1, alpha = .8) +
  geom_text(data = s1_labels, aes(x = label_x, label = label), size = 1.65, vjust = -.25, hjust = .5,
            check_overlap = TRUE, show.legend = FALSE) +
  facet_wrap(~dimension, nrow = 1) + scale_colour_ms_metric(guide = "none") +
  scale_x_continuous(trans = scales::pseudo_log_trans(sigma = .03), expand = expansion(mult = c(.04, .14))) +
  scale_y_continuous(trans = scales::pseudo_log_trans(sigma = .03), expand = expansion(mult = c(.04, .16))) +
  labs(x = "Observation-weighted A", y = "Participant-balanced A") + theme_ms_axes()
s1a <- s1a + geom_text(data=s1_scale |> filter(metric=="dark_mean_level",dimension=="Monitoring days",standardizer=="IQR / 1.349"),
  aes(label="dark mean level"),hjust=1.04,vjust=1.3,size=2.05,show.legend=FALSE)
focal_states <- robustness$bootstrap_summary |> select(placement,resolution_s,n_days,role)
s1_joint <- robustness$robust_joint |> inner_join(focal_states,by=c("placement","resolution_s","n_days")) |>
  group_by(placement,resolution_s,n_days,role) |>
  summarise(primary_R_K=max(primary_R_obs),robust_R_K=if(all(is.finite(R_obs))) max(R_obs) else NA_real_,
    robust_limiting_target=if(all(is.finite(R_obs))) metric[which.max(R_obs)] else NA_character_,.groups="drop") |>
  mutate(state=paste0(resolution_s," s / ",n_days," d"),placement=factor(placement,levels=c("eye","chest","wrist"),labels=c("Eye","Chest","Wrist")))
s1c <- ggplot(s1_joint,aes(y=reorder(state,role))) +
  geom_vline(xintercept=.5,linetype=2,colour="#777777",linewidth=.35) +
  geom_segment(aes(x=primary_R_K,xend=robust_R_K,yend=reorder(state,role)),colour="#B9C0C5",linewidth=.6) +
  geom_point(aes(x=primary_R_K,shape="SD"),colour=MS_PRIMARY,size=2) +
  geom_point(aes(x=robust_R_K,shape="IQR / 1.349"),colour="#B16C42",size=2) +
  facet_wrap(~placement,scales="free_y",nrow=1) + scale_shape_manual(values=c("SD"=16,"IQR / 1.349"=17)) +
  scale_x_continuous(trans=scales::pseudo_log_trans(sigma=.1),breaks=c(.1,.5,1,2,5,10),
    expand=expansion(mult=c(.05,.15))) +
  labs(x="All-54 residual instability R_K",y=NULL,shape="Reference scale") + theme_ms_axes(legend_position="bottom")
FigS1 <- compose(list(s1a,s1b,s1c),c("a  Standardization sensitivity","b  Participant weighting sensitivity",
  "c  Scale dependence of All-54 decisions"),
  "Panel c uses RQ3 support-specific six-day reference scales at the chosen and two adjacent reduced designs. Dashed line: ε = 0.50.\nEqual numerical tolerances express different absolute-change requirements under SD and IQR scaling.",c(1.1,1.1,1))
write_display(s1_joint,"FigS1_joint_scale_sensitivity.csv")
write_display(s1_pairs, "FigS1_paired_sensitivity.csv"); write_display(s1_metrics, "FigS1_metric_sensitivity.csv")

# Fig. S2: inference plotting remains owned by its canonical RQ1 script.
fig2_env <- new.env(parent = globalenv()); fig2_env$MS_FIG2_APPENDIX_ONLY <- TRUE
sys.source("scripts/11b_plot_fig2.R", envir = fig2_env)
s2 <- fig2_env$ms_fig2_appendix_strength(inference)
FigS2 <- s2$plot
write_display(s2$strength, "FigS2_reference_strength.csv"); write_display(s2$strata, "FigS2_strength_strata.csv")
rm(inference); invisible(gc(FALSE))

# Fig. S3: existing held-out fits; negative R² stays visible. Group calibration
# uses frozen outer-training risk groups, not invented individual probability bins.
FAMILIES <- c("external_context", "microenvironment", "behaviour", "exposure_state", "joint")
FAMILY_LABELS <- c("External opportunity", "Micro-environment", "Behaviour", "Exposure state", "Joint")
if (any(context$validation_scheme != "participant_grouped")) stop("Unexpected contextual validation scheme")
s3_cv <- context |> filter(model_family %in% FAMILIES, is.finite(r2)) |>
  mutate(family = factor(model_family, levels = rev(FAMILIES), labels = rev(FAMILY_LABELS)),
         response = factor(outcome, levels = c("signed", "magnitude"), labels = c("Signed z", "Absolute |z|")),
         dimension = dimension_factor(dimension))
if (anyNA(s3_cv$response)) stop("Unexpected contextual response")
s3_summary <- s3_cv |> group_by(family, response, dimension) |>
  summarise(n_tasks = n(), median = median(r2), q25 = quantile(r2, .25), q75 = quantile(r2, .75),
    n_negative = sum(r2 < 0), .groups = "drop")
s3a <- ggplot(s3_cv, aes(r2, family)) +
  geom_vline(xintercept = 0, colour = "#A9B1B7", linewidth = .35) +
  geom_point(position = position_jitter(height = .14, width = 0, seed = 17), size = .5, alpha = .20, colour = MS_NEUTRAL) +
  geom_errorbar(data = s3_summary, aes(x = median, xmin = q25, xmax = q75), orientation = "y", width = .15, colour = MS_PRIMARY) +
  geom_point(data = s3_summary, aes(x = median), size = 1.2, colour = MS_PRIMARY) +
  facet_grid(response ~ dimension) + labs(x = "Held-out R²", y = NULL) + theme_ms_axes()
s3a <- s3a + scale_x_continuous(trans = scales::pseudo_log_trans(sigma = .03),
  breaks = c(-1000, -10, 0, .5), labels = c("−1000", "−10", "0", "0.5"))
calibration <- as_tibble(reliability$context_profiles) |>
  filter(comparison_pair_id %in% c("chest_vs_eye", "LIGHT_vs_MEDI", "120s_vs_10s"), target %in% c("exceed_0.1", "exceed_0.5")) |>
  mutate(contrast = factor(comparison_pair_id, levels = c("chest_vs_eye", "LIGHT_vs_MEDI", "120s_vs_10s"),
                          labels = c("Chest vs eye", "LIGHT vs MEDI", "120 vs 10 s")),
         tolerance = factor(target, levels = c("exceed_0.1", "exceed_0.5"), labels = c("ε = 0.10", "ε = 0.50")),
         context_group = factor(context_group, levels = c("Lower", "Middle", "Higher")))
s3b <- ggplot(calibration, aes(predicted, observed, colour = context_group)) +
  geom_abline(slope = 1, intercept = 0, colour = "#A9B1B7", linewidth = .35) + geom_point(size = 1.9) +
  facet_grid(tolerance ~ contrast) + coord_equal(xlim = c(0, 1), ylim = c(0, 1)) +
  scale_x_continuous(breaks = c(0, .5, 1)) + scale_y_continuous(breaks = c(0, .5, 1)) +
  scale_colour_manual(values = c(Lower = "#3A7C88", Middle = "#A5ADB2", Higher = "#B16C42")) +
  labs(x = "Mean predicted exceedance probability", y = "Observed exceedance fraction", colour = "Training-defined risk group") +
  theme_ms_axes(legend_position = "bottom")
calibration_counts <- calibration |> group_by(contrast) |>
  summarise(count = fmt_range(n_metrics), .groups = "drop")
FigS3 <- compose(list(s3a, s3b), c("a  Participant-grouped prediction across information families", "b  Calibration within held-out context-risk groups"),
  paste0("Panel a: tasks and median/IQR on a pseudo-log axis. Panel b targets: ",
    paste(paste(calibration_counts$contrast, calibration_counts$count), collapse = "; "), ".\n",
    "Risk groups are defined in outer-training data. Validation estimates performance for new participants within the observed site mixture."), c(1.2, 1))
write_display(s3_summary, "FigS3_cv_summary.csv"); write_display(calibration, "FigS3_group_calibration.csv")

# Retain state-coverage audit exports, while the numbered figure tests phase sensitivity.
s4_support <- single |> group_by(dimension, state_id, state_label, requirement_rank, status) |>
  summarise(n_targets = n_distinct(metric), .groups = "drop") |>
  mutate(dimension = dimension_factor(dimension),
    state_label = factor(state_label, levels = unique(single$state_label[order(single$dimension, single$requirement_rank)])),
    status = factor(status, levels = c("resolved", "boundary_unresolved")))
s4a <- ggplot(s4_support, aes(state_label, n_targets, fill = status)) + geom_col(width = .7, show.legend = TRUE) +
  facet_wrap(~dimension, scales = "free_x", nrow = 1) +
  scale_fill_manual(values = c(resolved = MS_PRIMARY, boundary_unresolved = "#CBD1D6"),
    labels = c(resolved = "Resolved", boundary_unresolved = "Boundary unresolved")) +
  labs(x = "Observed requirement state", y = "Published targets", fill = NULL) + theme_ms_axes(legend_position = "bottom")
requirements <- read_input("requirement")
s4_selected <- tidyr::crossing(epsilon_display = c(.05, .10, .20, .30, .50, 1),
  requirements |> distinct(dimension, metric)) |>
  left_join(requirements, by = c("dimension", "metric"), relationship = "many-to-many") |>
  filter(epsilon <= epsilon_display + 1e-12) |> group_by(epsilon_display, dimension, metric) |>
  slice_max(epsilon, n = 1, with_ties = FALSE) |> ungroup() |>
  mutate(decision = case_when(n_resolved_states == 0 ~ "Unresolved",
    sufficient_states == 0 ~ "No observed sufficient state",
    !sufficient_set_threshold_like ~ "Non-threshold sufficient set", TRUE ~ "Threshold-like sufficient set"),
    decision = factor(decision, levels = c("Threshold-like sufficient set", "Non-threshold sufficient set", "No observed sufficient state", "Unresolved")),
    dimension = dimension_factor(dimension))
s4_decisions <- s4_selected |> count(epsilon_display, dimension, decision, name = "n_targets")
s4b <- ggplot(s4_decisions, aes(factor(epsilon_display), n_targets, fill = decision)) + geom_col(width = .7, show.legend = TRUE) +
  facet_wrap(~dimension, nrow = 1) +
  scale_fill_manual(values = c("Threshold-like sufficient set" = "#3A7C88", "Non-threshold sufficient set" = "#B16C42",
    "No observed sufficient state" = "#B9C0C5", "Unresolved" = "#E6E9EB"), drop = FALSE) +
  labs(x = "Descriptive tolerance slice ε", y = "Published targets", fill = NULL) +
  theme_ms_axes(legend_position = "bottom") + guides(fill = guide_legend(ncol = 2))
write_display(s4_support, "FigS4_state_support.csv"); write_display(s4_selected, "FigS4_requirement_audit.csv")
s4_phase <- robustness$phase_targets |> left_join(joint |> distinct(metric,metric_class),by="metric") |>
  mutate(placement=factor(placement,levels=c("eye","chest","wrist"),labels=c("Eye","Chest","Wrist")))
s4pa <- ggplot(s4_phase,aes(primary_R_obs,alternate_R_obs,colour=metric_class)) +
  geom_abline(slope=1,intercept=0,colour="#A9B1B7",linewidth=.4) + geom_point(size=1.1,alpha=.8) +
  geom_hline(yintercept=.5,linetype=2,colour="#A9B1B7",linewidth=.3) +
  geom_vline(xintercept=.5,linetype=2,colour="#A9B1B7",linewidth=.3) +
  facet_wrap(~placement,nrow=1) + scale_colour_ms_metric() +
  scale_x_continuous(trans=scales::pseudo_log_trans(sigma=.03)) +
  scale_y_continuous(trans=scales::pseudo_log_trans(sigma=.03)) +
  labs(x="Target instability R_obs: primary phase (0 s)",y="Target instability R_obs: alternate phase (+10 s)",colour="Metric class") +
  theme_ms_axes(legend_position="bottom")
s4_states <- robustness$phase_summary |> mutate(placement=factor(placement,levels=c("eye","chest","wrist"),labels=c("Eye","Chest","Wrist")),
  state=paste0(resolution_a," s / ",n_days_a," d")) |>
  pivot_longer(c(primary_R_K,alternate_R_K),names_to="phase",values_to="R_K") |>
  mutate(phase=factor(phase,levels=c("primary_R_K","alternate_R_K"),labels=c("Primary (0 s)","Alternate (+10 s)")))
s4pb <- ggplot(s4_states,aes(phase,R_K,group=state)) + geom_hline(yintercept=.5,linetype=2,colour="#777777",linewidth=.35) +
  geom_line(colour="#A9B1B7",linewidth=.45) + geom_point(colour=MS_PRIMARY,size=2) +
  geom_text(aes(label=sprintf("%.3f",R_K)),vjust=-.75,size=2.4) +
  facet_wrap(~placement+state,nrow=1) + scale_y_continuous(expand=expansion(mult=c(.08,.2))) +
  labs(x=NULL,y="All-54 residual instability R_K") + theme_ms_axes()
FigS4 <- compose(list(s4pa,s4pb),c("a  Target-level sensitivity at the coarser neighbour","b  All-54 decisions across both 20-s phases"),
  "MEDI; ε = 0.50. Both possible 20-s phases retain exact 10-s source rows; supports, windows and SD anchors are fixed.\nThe chosen and one-day-shorter 10-s designs have no alternative grid phase. This test concerns the three displayed 20-s neighbours.",c(1.15,1))
write_display(s4_phase,"FigS4_phase_targets.csv");write_display(robustness$phase_summary,"FigS4_phase_states.csv")

# Fig. S5: deterministic target perturbations from the explicit analysis output.
s5_cells <- joint_sensitivity$cells |> mutate(placement = factor(placement, levels = c("eye", "chest", "wrist"),
  labels = c("Eye", "Chest", "Wrist")))
s5_map <- s5_cells |> pivot_longer(c(sufficient_change_fraction, pareto_change_fraction), names_to = "decision", values_to = "change_fraction") |>
  mutate(decision = factor(decision, levels = c("sufficient_change_fraction", "pareto_change_fraction"),
                           labels = c("Sufficient set", "Pareto frontier")),
         interval = factor(resolution_s, levels = ms_primary_temporal_s()))
s5a <- ggplot(s5_map, aes(n_days, interval, fill = change_fraction)) +
  geom_tile(colour = "white", linewidth = .3) +
  geom_point(data = s5_map |> filter(primary_pareto %in% TRUE), shape = 21, size = 1.9, fill = "white", stroke = .5) +
  geom_text(data = s5_map |> filter(is.na(change_fraction)), label = "×", colour = "#6E7479", size = 2.3) +
  geom_text(data = s5_map |> filter(is.finite(change_fraction), change_fraction > 0),
    aes(label = scales::percent(change_fraction, accuracy = .1)), colour = "white", size = 1.9, vjust = -.6) +
  facet_grid(decision ~ placement) +
  scale_fill_gradientn(colours = MS_SEQUENTIAL, limits = c(0, max(.01, s5_map$change_fraction, na.rm = TRUE)),
    labels = function(x) scales::percent(x, accuracy = .1),
    na.value = "#E6E9EB", name = "Changed / resolved omissions") +
  scale_x_continuous(breaks = ms_primary_duration_days()) + labs(x = "Monitoring days", y = "Sampling interval (s)") +
  theme_ms_axes(legend_position = "bottom")
s5_targets <- joint_sensitivity$targets
influential <- s5_targets |> group_by(omitted_target) |>
  summarise(total_changed = sum(n_frontier_cells_changed, na.rm = TRUE), .groups = "drop") |>
  filter(total_changed > 0) |> arrange(desc(total_changed), omitted_target) |> slice_head(n = 8)
if (nrow(influential)) {
  s5_bar <- s5_targets |> filter(omitted_target %in% influential$omitted_target) |>
    mutate(omitted_target = factor(omitted_target, levels = rev(influential$omitted_target)),
      placement = factor(placement, levels = c("eye", "chest", "wrist"), labels = c("Eye", "Chest", "Wrist")))
  s5b <- ggplot(s5_bar, aes(n_frontier_cells_changed, omitted_target, colour = metric_class)) +
    geom_segment(aes(x = 0, xend = n_frontier_cells_changed, yend = omitted_target), linewidth = .5) + geom_point(size = 1.6) +
    facet_wrap(~placement, nrow = 1) + scale_colour_ms_metric(guide = "none") +
    scale_y_discrete(labels=function(x) gsub("_"," ",x,fixed=TRUE)) +
    labs(x = "Number of frontier cells changed", y = "Omitted target") + theme_ms_axes()
} else {
  s5b <- ggdraw() + draw_label("All 54 single-target omissions leave the resolved frontier unchanged.",
    size = MS_FIG_TITLE_PT, fontfamily = MS_FONT)
}
s5_boot <- bind_rows(robustness$bootstrap_summary,robustness$bootstrap_fixed_summary) |>
  mutate(placement=factor(placement,levels=c("eye","chest","wrist"),labels=c("Eye","Chest","Wrist")),
    state=paste0(resolution_s," s / ",n_days," d"),
    policy=factor(scale_policy,levels=c("fixed","reestimated"),labels=c("Fixed SD","Re-estimated SD")),
    y=role+if_else(scale_policy=="fixed",-.12,.12),
    probability_label=paste0(scales::percent(sufficient_probability,accuracy=.1)," (",n_resolved,")"))
s5c <- ggplot(s5_boot,aes(y=y,colour=policy)) +
  geom_vline(xintercept=.5,linetype=2,colour="#777777",linewidth=.35) +
  geom_segment(aes(x=R_K_low,xend=R_K_high,yend=y),linewidth=.65) +
  geom_point(aes(x=R_K_median),size=1.55) +
  geom_point(data=s5_boot |> filter(scale_policy=="reestimated"),aes(x=primary_R_K,y=role),shape=23,fill="white",colour="#333333",size=1.8) +
  geom_text(aes(x=R_K_high,label=probability_label),hjust=-.08,size=1.95,show.legend=FALSE) +
  facet_wrap(~placement,nrow=1,scales="free_y") +
  scale_y_continuous(breaks=1:3,labels=c("Coarser","Shorter","Selected")) +
  scale_colour_manual(values=c("Fixed SD"="#7B858D","Re-estimated SD"=MS_PRIMARY)) +
  scale_x_continuous(trans=scales::pseudo_log_trans(sigma=.1),expand=expansion(mult=c(.05,.62)),breaks=c(.2,.5,1,2,5,10)) +
  labs(x="All-54 R_K: bootstrap median and 95% percentile interval",y=NULL,colour=NULL) + theme_ms_axes(legend_position="bottom")
FigS5 <- compose(list(s5a,s5b,s5c),c("a  Stability across 54 leave-one-target-out inventories",
  "b  Targets with the largest frontier changes","c  Participant-sampling stability at selected and adjacent designs"),
  paste0("MEDI; ε = 0.50. Panel c: ",robustness$n_bootstrap," paired, site-stratified participant draws with fixed or re-estimated SD anchors.\nLabels: sufficient percentage (resolved draws); undefined comparisons/scales remain unresolved. Diamonds: original R_K.\nSelected / shorter / coarser coordinates are shown in Fig. S1c. Frequencies concern fixed states, not whole-frontier selection."),
  c(1.5,.6,1.3))
write_display(s5_boot,"FigS5_participant_bootstrap.csv")
write_display(s5_map, "FigS5_cell_sensitivity.csv"); write_display(s5_targets, "FigS5_target_sensitivity.csv")

# Single six-item manuscript manifest; supplementary formats share their identity.
plots <- list(FigS1, FigS2, FigS3, FigS4, FigS5)
heights <- c(8.3,8.1,7.7,5.8,8.8)
for (i in seq_along(plots)) {
  ms_plot_save(plots[[i]], file.path(FIG_DIR, appendix_registry$filename[[i + 1L]]),
               MS_FIG_WIDTH_IN, heights[[i]])
}
appendix_manifest <- appendix_registry |> mutate(
  path = c(file.path(APP_DIR, filename[[1]]), file.path(FIG_DIR, filename[-1])),
  core_artifact_version = CORE_VERSION, rq1_analysis_version = RQ1_VERSION,
  rq1_inference_version = if_else(id == "Fig. S2", as.character(matched$rq1_inference_version[[1]]), NA_character_),
  rq2_context_version = if_else(id == "Fig. S3",
    ms_plot_one_version(context_manifest$context_model_version, "RQ2 context version"), NA_character_),
  rq2_reliability_version = if_else(id == "Fig. S3", reliability$provenance$rq2_analysis_version, NA_character_),
  rq3_analysis_version = if_else(id %in% c("Fig. S1", "Fig. S4", "Fig. S5"), RQ3_VERSION, NA_character_),
  analysis_design_id = ms_analysis_design_id(), status = "complete")
write_display(appendix_manifest, "appendix_artifact_manifest.csv")
write_display(tibble(path = unname(input_paths), md5 = unname(tools::md5sum(input_paths))), "appendix_input_provenance.csv")
message(if (ms_plot_prep_only()) "Appendix prepared in memory: Table S1 + Figs. S1–S5" else
  "Appendix complete: results/appendix/TableS1_analysis_support.html + results/figures/FigS1–FigS5_*.png")
