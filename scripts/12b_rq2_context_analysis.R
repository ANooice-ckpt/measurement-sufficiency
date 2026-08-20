suppressPackageStartupMessages({
  library(tidyverse)
})
source("scripts/utils/rq_context.R")

# RQ2 extension for real-world context conditionality.
# The existing 12_rq2_analysis.R remains responsible for exposure-state,
# external-context prediction, grouped CV/LOSO transportability, and gamma
# separability. This script uses the frozen primitive context distortions from
# 10b and asks only how their empirical distributions change across context.
# Diary context remains explanatory and is not added to the prediction-model
# competition.

RQ1_CONTEXT <- "data/derived/rq1/rq1_context_distortion_long.rds"
OUT_DATA <- "data/derived/rq2"
OUT_RESULTS <- "results/rq2"
OUT_DIAG <- "results/diagnostics"
B_BOOT <- suppressWarnings(as.integer(Sys.getenv("RQ2_BOOT", unset = "1000")))
if (!is.finite(B_BOOT) || B_BOOT < 0L) B_BOOT <- 1000L
BOOT_SEED <- 20260823L

if (!file.exists(RQ1_CONTEXT)) stop("Missing RQ1 context artifact: ", RQ1_CONTEXT)
for (d in c(OUT_DATA, OUT_RESULTS, OUT_DIAG)) dir.create(d, recursive = TRUE, showWarnings = FALSE)

x <- readRDS(RQ1_CONTEXT) |> filter(available, is.finite(e), is.finite(abs_e))
ctx_versions <- unique(x$rq1_context_analysis_version); ctx_versions <- ctx_versions[!is.na(ctx_versions)]
core_versions <- unique(x$core_artifact_version); core_versions <- core_versions[!is.na(core_versions)]
if (length(ctx_versions) != 1L || length(core_versions) != 1L) stop("RQ1 context/core version mismatch")
RQ1_CONTEXT_VERSION <- ctx_versions[[1]]
CORE_VERSION <- core_versions[[1]]
RQ2_CONTEXT_VERSION <- paste0("rq2_context_v2__", RQ1_CONTEXT_VERSION)

safe_q <- rq_context_safe_quantile

message("RQ2 context: empirical conditional geometry across valid target representations")
conditional_geometry <- x |>
  group_by(
    dimension, configuration, configuration_label, configuration_order,
    comparison_lattice, context_family, context_state,
    metric, metric_class, metric_geometry
  ) |>
  summarise(
    n_participants = n_distinct(paste(site, Id, sep = "|")),
    n_units = n(),
    median_e = median(e),
    q25_e = safe_q(e, .25), q75_e = safe_q(e, .75),
    p025_e = safe_q(e, .025), p975_e = safe_q(e, .975),
    B_conditional = mean(e), A_conditional = mean(abs_e),
    .groups = "drop"
  ) |>
  mutate(
    core_artifact_version = CORE_VERSION,
    rq1_context_analysis_version = RQ1_CONTEXT_VERSION,
    rq2_context_analysis_version = RQ2_CONTEXT_VERSION
  )
readr::write_csv(
  conditional_geometry,
  file.path(OUT_RESULTS, "rq2_context_conditional_geometry.csv"),
  na = ""
)

# Preserve the paired smallest-unit structure for the two naturally binary
# contexts. Activity remains a four-state empirical distribution and is not
# converted into an arbitrary collection of pairwise tests.
base <- x |>
  mutate(
    base_unit_id = case_when(
      analysis_unit_type == "participant_window_context" ~ window_id,
      TRUE ~ paste(site, Id, as.character(Date), sep = "|")
    )
  )

make_binary_contrast <- function(df, family, state0, state1) {
  z <- df |>
    filter(context_family == family, context_state %in% c(state0, state1)) |>
    select(
      dimension, configuration, configuration_label, configuration_order,
      comparison_lattice, site, Id, base_unit_id,
      metric, metric_class, metric_geometry,
      context_state, e, abs_e
    ) |>
    distinct() |>
    pivot_wider(names_from = context_state, values_from = c(e, abs_e))

  e0 <- paste0("e_", state0); e1 <- paste0("e_", state1)
  a0 <- paste0("abs_e_", state0); a1 <- paste0("abs_e_", state1)
  if (!all(c(e0, e1, a0, a1) %in% names(z))) return(tibble())

  z |>
    filter(
      is.finite(.data[[e0]]), is.finite(.data[[e1]]),
      is.finite(.data[[a0]]), is.finite(.data[[a1]])
    ) |>
    mutate(
      context_family = family,
      reference_state = state0,
      comparison_state = state1,
      delta_signed_distortion = .data[[e1]] - .data[[e0]],
      delta_absolute_distortion = .data[[a1]] - .data[[a0]]
    ) |>
    select(
      dimension, configuration, configuration_label, configuration_order,
      comparison_lattice, site, Id, base_unit_id,
      metric, metric_class, metric_geometry,
      context_family, reference_state, comparison_state,
      delta_signed_distortion, delta_absolute_distortion
    )
}

binary_long <- bind_rows(
  make_binary_contrast(base, "photoperiod", "day", "night"),
  make_binary_contrast(base, "environment", "indoor", "outdoor")
) |>
  mutate(
    core_artifact_version = CORE_VERSION,
    rq1_context_analysis_version = RQ1_CONTEXT_VERSION,
    rq2_context_analysis_version = RQ2_CONTEXT_VERSION
  )
saveRDS(
  binary_long,
  file.path(OUT_DATA, "rq2_context_binary_contrasts_long.rds"),
  compress = "xz"
)

bootstrap_contrast <- function(g, B = B_BOOT) {
  clusters <- g |>
    group_by(site, Id) |>
    summarise(
      sum_signed = sum(delta_signed_distortion),
      sum_absolute = sum(delta_absolute_distortion),
      n = n(), .groups = "drop"
    )
  site_counts <- clusters |> count(site, name = "n_participants")
  supported <- B > 0L && nrow(clusters) >= 2L && any(site_counts$n_participants > 1L)
  if (!supported) {
    return(tibble(
      bootstrap_supported = FALSE,
      signed_ci_low = NA_real_, signed_ci_high = NA_real_,
      absolute_ci_low = NA_real_, absolute_ci_high = NA_real_
    ))
  }
  by_site <- split(clusters, clusters$site)
  vals <- replicate(B, {
    sampled <- map_dfr(by_site, ~.x[sample.int(nrow(.x), nrow(.x), replace = TRUE), , drop = FALSE])
    c(
      signed = sum(sampled$sum_signed) / sum(sampled$n),
      absolute = sum(sampled$sum_absolute) / sum(sampled$n)
    )
  })
  tibble(
    bootstrap_supported = TRUE,
    signed_ci_low = safe_q(vals["signed", ], .025),
    signed_ci_high = safe_q(vals["signed", ], .975),
    absolute_ci_low = safe_q(vals["absolute", ], .025),
    absolute_ci_high = safe_q(vals["absolute", ], .975)
  )
}

contrast_vars <- c(
  "dimension", "configuration", "configuration_label", "configuration_order",
  "comparison_lattice", "metric", "metric_class", "metric_geometry",
  "context_family", "reference_state", "comparison_state"
)
if (nrow(binary_long)) {
  contrast_base <- binary_long |>
    group_by(across(all_of(contrast_vars))) |>
    summarise(
      n_participants = n_distinct(paste(site, Id, sep = "|")),
      n_paired_units = n(),
      mean_delta_signed = mean(delta_signed_distortion),
      median_delta_signed = median(delta_signed_distortion),
      mean_delta_absolute = mean(delta_absolute_distortion),
      median_delta_absolute = median(delta_absolute_distortion),
      .groups = "drop"
    )
  set.seed(BOOT_SEED)
  contrast_ci <- binary_long |>
    group_by(across(all_of(contrast_vars))) |>
    group_modify(~bootstrap_contrast(.x, B_BOOT)) |>
    ungroup()
  contrast_summary <- contrast_base |>
    left_join(contrast_ci, by = contrast_vars) |>
    mutate(
      core_artifact_version = CORE_VERSION,
      rq1_context_analysis_version = RQ1_CONTEXT_VERSION,
      rq2_context_analysis_version = RQ2_CONTEXT_VERSION
    )
} else {
  contrast_summary <- tibble()
}
readr::write_csv(
  contrast_summary,
  file.path(OUT_RESULTS, "rq2_context_binary_contrasts.csv"),
  na = ""
)

context_manifest <- conditional_geometry |>
  distinct(context_family, context_state, metric, metric_class, metric_geometry) |>
  arrange(context_family, context_state, metric) |>
  mutate(
    role = case_when(
      context_family == "photoperiod" ~ "civil day/night; continuous-interval-valid target representation",
      context_family == "environment" ~ "diary-derived indoor/outdoor; fragmented-context-valid target representation",
      context_family == "activity" ~ "diary-derived four-state activity; fragmented-context-valid target representation",
      TRUE ~ "context"
    )
  )
readr::write_csv(context_manifest, file.path(OUT_RESULTS, "rq2_context_manifest.csv"), na = "")

geometry_audit <- conditional_geometry |>
  transmute(
    dimension, configuration, context_family, context_state, metric,
    A_conditional, B_conditional,
    pass = A_conditional + 1e-12 >= abs(B_conditional)
  )
if (nrow(geometry_audit) && any(!geometry_audit$pass)) stop("Conditional A >= |B| invariant failed")
readr::write_csv(geometry_audit, file.path(OUT_DIAG, "rq2_context_geometry_invariant.csv"), na = "")

writeLines(c(
  "# RQ2 context run report", "",
  paste0("Core artifact version: ", CORE_VERSION),
  paste0("RQ1 context upstream: ", RQ1_CONTEXT_VERSION),
  paste0("RQ2 context version: ", RQ2_CONTEXT_VERSION),
  paste0("Target representations entering context conditionality: ", n_distinct(x$metric)),
  "Conditionality axes: photoperiod, indoor/outdoor environment, four-state activity.",
  "Photoperiod and environment retain paired smallest-unit contrasts metric by metric; activity is reported as state-specific empirical geometry without arbitrary pairwise testing.",
  "Diary context is explanatory only and is not added to the external/exposure/joint prediction competition in 12_rq2_analysis.R.",
  "No metric-class averaging is performed before metric-level conditional geometry is estimated."
), file.path(OUT_RESULTS, "RQ2_CONTEXT_RUN_REPORT.md"))

message("RQ2 context complete: ", RQ2_CONTEXT_VERSION)
message("  ", file.path(OUT_RESULTS, "rq2_context_conditional_geometry.csv"))
message("  ", file.path(OUT_RESULTS, "rq2_context_binary_contrasts.csv"))
