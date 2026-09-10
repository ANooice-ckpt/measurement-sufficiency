# Single source of truth for manuscript main-figure identity.
#
# `current_id` is the public manuscript/output identity. `legacy_id` is the
# identifier still emitted internally by mature plotting implementations that
# predate insertion of the RQ1 inferential-preservation Fig. 2. Numbered wrapper
# scripts are canonical entrypoints; implementation paths are internal details.
# No scientific plotting object or estimand is defined here.

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
    implementation_script = c(
      "scripts/11_plot_fig1.R",
      "scripts/11b_plot_fig2.R",
      "scripts/13a_plot_fig2.R",
      "scripts/13b_plot_fig3.R",
      "scripts/15a_plot_fig4.R",
      "scripts/15b_plot_fig5.R"
    ),
    stringsAsFactors = FALSE
  )
}

ms_current_main_figure_ids <- function() {
  ms_main_figure_registry()$current_id
}

ms_main_figure_resolve_id <- function(x) {
  x <- as.character(x)
  registry <- ms_main_figure_registry()
  idx <- match(x, registry$legacy_id)
  current_idx <- match(x, registry$current_id)
  out <- x
  out[!is.na(idx)] <- registry$current_id[idx[!is.na(idx)]]
  out[!is.na(current_idx)] <- registry$current_id[current_idx[!is.na(current_idx)]]
  out
}

ms_main_figure_legacy_id <- function(x) {
  current <- ms_main_figure_resolve_id(x)
  registry <- ms_main_figure_registry()
  idx <- match(current, registry$current_id)
  out <- as.character(x)
  out[!is.na(idx)] <- registry$legacy_id[idx[!is.na(idx)]]
  out
}

ms_main_figure_resolve_filename <- function(filename) {
  filename <- as.character(filename)
  ext <- tools::file_ext(filename)
  stem <- tools::file_path_sans_ext(filename)
  current <- ms_main_figure_resolve_id(stem)
  ifelse(nzchar(ext), paste0(current, ".", ext), current)
}

ms_main_figure_implementation <- function(current_id) {
  current_id <- ms_main_figure_resolve_id(current_id)
  registry <- ms_main_figure_registry()
  idx <- match(current_id, registry$current_id)
  if (length(current_id) != 1L || is.na(idx)) {
    stop("Unknown main figure: ", paste(current_id, collapse = ", "), call. = FALSE)
  }
  registry$implementation_script[[idx]]
}

ms_main_plot_scripts <- function(include_implementations = TRUE) {
  registry <- ms_main_figure_registry()
  scripts <- registry$canonical_script
  if (isTRUE(include_implementations)) scripts <- c(scripts, registry$implementation_script)
  unique(basename(scripts))
}
