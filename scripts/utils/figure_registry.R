# Single source of truth for manuscript main-figure identity.
#
# `current_id` is the public manuscript/output identity. `legacy_id` is retained
# only for output/refinement compatibility inside mature plotting code that
# predates insertion of the RQ1 inferential-preservation Fig. 2. Every main
# figure now has exactly one canonical script; there is no separate wrapper /
# implementation layer.

ms_main_figure_registry <- function() {
  data.frame(
    current_id = c(
      "Fig1_RQ1",
      "Fig2_RQ1_inferential_preservation",
      "Fig3_RQ2",
      "Fig4_RQ2",
      "Fig5_RQ3",
      "Fig6_RQ3"
    ),
    legacy_id = c(
      "Fig1_RQ1",
      "Fig2_RQ1_inferential_preservation",
      "Fig2_RQ2",
      "Fig3_RQ2",
      "Fig4_RQ3",
      "Fig5_RQ3"
    ),
    canonical_script = c(
      "scripts/11_plot_fig1.R",
      "scripts/11b_plot_fig2.R",
      "scripts/13a_plot_fig3.R",
      "scripts/13b_plot_fig4.R",
      "scripts/15a_plot_fig5.R",
      "scripts/15b_plot_fig6.R"
    ),
    stringsAsFactors = FALSE
  )
}

ms_current_main_figure_ids <- function() {
  ms_main_figure_registry()$current_id
}

ms_main_figure_from_legacy_id <- function(x) {
  x <- as.character(x)
  registry <- ms_main_figure_registry()
  idx <- match(x, registry$legacy_id)
  out <- x
  out[!is.na(idx)] <- registry$current_id[idx[!is.na(idx)]]
  out
}

ms_main_figure_to_legacy_id <- function(current_id) {
  current_id <- as.character(current_id)
  registry <- ms_main_figure_registry()
  idx <- match(current_id, registry$current_id)
  out <- current_id
  out[!is.na(idx)] <- registry$legacy_id[idx[!is.na(idx)]]
  out
}

ms_main_figure_filename_from_legacy <- function(filename) {
  filename <- as.character(filename)
  ext <- tools::file_ext(filename)
  stem <- tools::file_path_sans_ext(filename)
  current <- ms_main_figure_from_legacy_id(stem)
  ifelse(nzchar(ext), paste0(current, ".", ext), current)
}

ms_main_plot_scripts <- function() {
  unique(basename(ms_main_figure_registry()$canonical_script))
}
