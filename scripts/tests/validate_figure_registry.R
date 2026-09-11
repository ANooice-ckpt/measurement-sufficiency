# Fast, base-R contract test for manuscript figure identity and routing.
# It uses no project data or optional packages and is safe to run in CI.
source("scripts/utils/figure_registry.R")

registry <- ms_main_figure_registry()
expected_current <- c(
  "Fig1_RQ1",
  "Fig2_RQ1_inferential_preservation",
  "Fig3_RQ2",
  "Fig4_RQ2",
  "Fig5_RQ3",
  "Fig6_RQ3"
)
expected_legacy <- c(
  "Fig1_RQ1",
  "Fig2_RQ1_inferential_preservation",
  "Fig2_RQ2",
  "Fig3_RQ2",
  "Fig4_RQ3",
  "Fig5_RQ3"
)
retired_plot_paths <- c(
  "scripts/13a_plot_fig2.R",
  "scripts/13b_plot_fig3.R",
  "scripts/15a_plot_fig4.R",
  "scripts/15b_plot_fig5.R",
  "scripts/16_plot_supplementary.R"
)

stopifnot(
  identical(registry$current_id, expected_current),
  identical(registry$legacy_id, expected_legacy),
  !anyDuplicated(registry$current_id),
  !anyDuplicated(registry$canonical_script),
  all(file.exists(registry$canonical_script)),
  !any(file.exists(retired_plot_paths)),
  identical(ms_current_main_figure_ids(), expected_current),
  identical(ms_main_figure_from_legacy_id(expected_legacy), expected_current),
  identical(ms_main_figure_to_legacy_id(expected_current), expected_legacy),
  identical(
    ms_main_figure_filename_from_legacy(paste0(expected_legacy, ".png")),
    paste0(expected_current, ".png")
  ),
  identical(ms_main_plot_scripts(), basename(registry$canonical_script))
)

# The overlap is intentional and is why conversion direction is explicit:
# current Fig3_RQ2 is also the legacy identity of current Fig4_RQ2, and current
# Fig5_RQ3 is also the legacy identity of current Fig6_RQ3. Stored current IDs
# therefore must never be fed back through the legacy->current conversion.
stopifnot(
  identical(ms_main_figure_from_legacy_id("Fig3_RQ2"), "Fig4_RQ2"),
  identical(ms_main_figure_from_legacy_id("Fig5_RQ3"), "Fig6_RQ3"),
  identical(ms_main_figure_to_legacy_id("Fig3_RQ2"), "Fig2_RQ2"),
  identical(ms_main_figure_to_legacy_id("Fig5_RQ3"), "Fig4_RQ3")
)

# Orchestration is explicit and must invoke every canonical entrypoint exactly
# through its current numbered path.
runner <- readLines("scripts/run_downstream_server.sh", warn = FALSE)
stopifnot(all(vapply(
  registry$canonical_script,
  function(path) any(grepl(path, runner, fixed = TRUE)),
  logical(1)
)))
stopifnot(!any(grepl("16_plot_supplementary.R", runner, fixed = TRUE)))

cat("PASS: canonical main-figure identities, paths, legacy output mapping and runner entrypoints are consistent\n")
