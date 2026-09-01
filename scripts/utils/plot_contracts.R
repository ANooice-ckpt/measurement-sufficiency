# Plot-side contracts for the current core/RQ artifact graph.
# Plot scripts may reshape frozen summaries for display, but must not refit,
# recompute estimands, or silently fall back to legacy data/derived paths.

# Some compact plotting entry points source plot_contracts.R directly but still
# rely on shared atlas helpers such as ms_direction_ratio(). Load those helpers
# here only when they have not already been sourced by the caller.
if (!exists("ms_direction_ratio", mode = "function") &&
    file.exists("scripts/utils/figure_atlas.R")) {
  source("scripts/utils/figure_atlas.R")
}

# Figure-specific final display refinements may reuse already-built display
# summaries immediately before export. They may recompose or filter descriptive
# display rows, but must not refit models or alter canonical RQ estimands.
if (!exists("ms_fig1_refine_main", mode = "function") &&
    file.exists("scripts/utils/fig1_refinement.R")) {
  source("scripts/utils/fig1_refinement.R")
}
if (!exists("ms_fig2_refine_main", mode = "function") &&
    file.exists("scripts/utils/fig2_refinement.R")) {
  source("scripts/utils/fig2_refinement.R")
}
if (!exists("ms_fig3_refine_main", mode = "function") &&
    file.exists("scripts/utils/fig3_refinement.R")) {
  source("scripts/utils/fig3_refinement.R")
}
if (!exists("ms_fig3_atlas_refine_main", mode = "function") &&
    file.exists("scripts/utils/fig3_atlas_refinement.R")) {
  source("scripts/utils/fig3_atlas_refinement.R")
}

# The final composition pass is visual only. Main scripts still construct every
# panel and scientific layer; this helper only regularizes layout immediately
# before the PNG is written.
if (!exists("ms_polish_main_figure", mode = "function") &&
    file.exists("scripts/utils/figure_polish.R")) {
  source("scripts/utils/figure_polish.R")
}

ms_plot_prep_only <- function() {
  # Explicit override remains available for diagnostics.
  if (identical(Sys.getenv("MS_PLOT_PREP_ONLY", unset = "0"), "1")) return(TRUE)

  # The centralized supplementary entrypoint sources main-figure scripts only to
  # reconstruct their frozen display objects. Suppress save/manifest side effects
  # only while one of those five explicit main scripts is present on the call
  # stack. FigS saves executed directly by 16_plot_supplementary.R are unaffected.
  file_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  if (!length(file_arg)) return(FALSE)
  top_script <- basename(sub("^--file=", "", file_arg[[1]]))
  if (!identical(top_script, "16_plot_supplementary.R")) return(FALSE)

  main_plot_scripts <- c(
    "11_plot_fig1.R",
    "13a_plot_fig2.R",
    "13b_plot_fig3.R",
    "15a_plot_fig4.R",
    "15b_plot_fig5.R"
  )
  call_text <- vapply(
    sys.calls(),
    function(cl) paste(deparse(cl, width.cutoff = 500L), collapse = " "),
    character(1)
  )
  any(vapply(
    main_plot_scripts,
    function(script) any(grepl(script, call_text, fixed = TRUE)),
    logical(1)
  ))
}

ms_plot_require_files <- function(paths, artifact = "plot input") {
  paths <- as.character(paths)
  missing <- paths[!file.exists(paths)]
  if (length(missing)) {
    stop(artifact, " missing: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  invisible(paths)
}

ms_plot_require_columns <- function(data, required, artifact = "plot input") {
  missing <- setdiff(required, names(data))
  if (length(missing)) {
    stop(artifact, " missing columns: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  invisible(data)
}

ms_plot_one_version <- function(values, label) {
  values <- unique(as.character(values[!is.na(values) & nzchar(as.character(values))]))
  if (length(values) != 1L) {
    stop(label, " must contain exactly one version; found: ", paste(values, collapse = ", "), call. = FALSE)
  }
  values[[1]]
}

ms_plot_assert_prefix <- function(value, prefix, label) {
  if (!startsWith(value, prefix)) {
    stop(label, " has unsupported version: ", value, "; expected prefix ", prefix, call. = FALSE)
  }
  invisible(value)
}

ms_plot_assert_core <- function(values, expected = NULL) {
  value <- ms_plot_one_version(values, "core_artifact_version")
  if (is.null(expected)) {
    if (!exists("core_artifact_version", mode = "function")) {
      source("scripts/utils/core_artifacts.R")
    }
    expected <- core_artifact_version()
  }
  if (!identical(value, expected)) {
    stop("Plot input core version is ", value, "; expected ", expected, call. = FALSE)
  }
  value
}

ms_plot_pair_label <- function(data) {
  if (all(c("config_a_label", "config_b_label") %in% names(data))) {
    return(paste(data$config_a_label, "→", data$config_b_label))
  }
  if ("comparison_pair_id" %in% names(data)) return(as.character(data$comparison_pair_id))
  rep("pairwise", nrow(data))
}

ms_plot_write_manifest <- function(path, figure_rows) {
  # Supplementary plotting may source a main-figure script only to reconstruct
  # its frozen display objects. In prep-only mode, do not overwrite the main
  # figure manifest; the supplementary entrypoint writes the complete per-RQ
  # manifest after its FigS blocks finish.
  if (ms_plot_prep_only()) return(invisible(path))

  figure_rows <- tibble::as_tibble(figure_rows)
  figure_rows$generated_at_utc <- format(Sys.time(), tz = "UTC", usetz = TRUE)

  # Plot scripts historically place the manifest inside results/rq*/figures.
  # Figures now live centrally, while each RQ keeps its manifest at the RQ root.
  legacy_dir <- dirname(path)
  rq_dir <- dirname(legacy_dir)
  manifest_path <- if (
    identical(basename(legacy_dir), "figures") && grepl("^rq[0-9]+$", basename(rq_dir))
  ) {
    file.path(rq_dir, basename(path))
  } else {
    path
  }

  dir.create(dirname(manifest_path), recursive = TRUE, showWarnings = FALSE)
  readr::write_csv(figure_rows, manifest_path, na = "")

  # Once the manifest has been moved out, the legacy per-RQ figure directory is
  # obsolete. Remove it together with any stale PNG/PDF files from older runs.
  if (!identical(legacy_dir, dirname(manifest_path)) && dir.exists(legacy_dir)) {
    unlink(legacy_dir, recursive = TRUE, force = TRUE)
  }
  invisible(manifest_path)
}

ms_plot_save <- function(plot, path, width, height,
                         dpi = if (exists("MS_RASTER_DPI", inherits = TRUE)) MS_RASTER_DPI else 600) {
  # When a main script is sourced by the supplementary entrypoint for data/object
  # preparation, reconstruct its plot objects without writing the main figure a
  # second time. Standalone main-figure execution is unchanged.
  if (ms_plot_prep_only()) return(invisible(path))

  ext <- tolower(tools::file_ext(path))

  # PDF export is intentionally disabled. Existing plot scripts may retain paired
  # PDF/PNG calls; only the PNG call produces an artifact.
  if (identical(ext, "pdf")) return(invisible(NULL))
  if (!identical(ext, "png")) {
    stop("Figure outputs must be PNG; unsupported path: ", path, call. = FALSE)
  }

  # Capture the script evaluation environment before entering any further helper
  # calls. Figure-specific final display refinements can reuse already-computed
  # display objects without moving canonical estimands out of the RQ script.
  caller_env <- parent.frame()
  if (identical(basename(path), "Fig1_RQ1.png") &&
      exists("ms_fig1_refine_main", mode = "function")) {
    refined <- ms_fig1_refine_main(caller_env)
    if (is.list(refined) && !is.null(refined$p1a_core)) {
      assign("p1a_core", refined$p1a_core, envir = caller_env)
      if (!is.null(refined$assoc_text)) {
        assign("assoc_text", refined$assoc_text, envir = caller_env)
      }
      if (!is.null(refined$p1b_core)) {
        assign("p1b_core", refined$p1b_core, envir = caller_env)
      }
      if (!is.null(refined$p1b_shape_legend)) {
        assign("p1b_shape_legend", refined$p1b_shape_legend, envir = caller_env)
      }
    }
  }

  if (identical(basename(path), "Fig2_RQ2.png") &&
      exists("ms_fig2_refine_main", mode = "function")) {
    refined <- ms_fig2_refine_main(caller_env)
    if (is.list(refined) && !is.null(refined$plot)) {
      plot <- refined$plot
      # Keep the interactive object in sync with the exported main figure.
      assign("p2", refined$plot, envir = caller_env)
      if (!is.null(refined$p2a)) assign("p2a", refined$p2a, envir = caller_env)
      if (!is.null(refined$p2b)) assign("p2b", refined$p2b, envir = caller_env)
      if (!is.null(refined$p2c)) assign("p2c", refined$p2c, envir = caller_env)
      if (!is.null(refined$top_recoverable)) {
        assign("fig2_top_recoverable", refined$top_recoverable, envir = caller_env)
      }
    }
  }

  if (identical(basename(path), "Fig3_RQ2.png") &&
      (exists("ms_fig3_atlas_refine_main", mode = "function") ||
       exists("ms_fig3_refine_main", mode = "function"))) {
    refined <- if (exists("ms_fig3_atlas_refine_main", mode = "function")) {
      ms_fig3_atlas_refine_main(caller_env)
    } else {
      ms_fig3_refine_main(caller_env)
    }
    if (is.list(refined) && !is.null(refined$plot)) {
      plot <- refined$plot
      assign("p3", refined$plot, envir = caller_env)
      if (!is.null(refined$p3a)) assign("p3a", refined$p3a, envir = caller_env)
      if (!is.null(refined$p3b)) assign("p3b", refined$p3b, envir = caller_env)
      if (!is.null(refined$p3c)) assign("p3c", refined$p3c, envir = caller_env)
      if (!is.null(refined$width) && is.finite(refined$width[[1]])) {
        width <- as.numeric(refined$width[[1]])
      }
      if (!is.null(refined$height) && is.finite(refined$height[[1]])) {
        height <- as.numeric(refined$height[[1]])
      }
    }
  }

  if (exists("ms_polish_main_figure", mode = "function")) {
    polished <- ms_polish_main_figure(plot, path, caller_env, width, height)
    if (is.list(polished) && !is.null(polished$plot)) plot <- polished$plot
    if (is.list(polished) && length(polished$width) && is.finite(polished$width[[1]])) {
      width <- as.numeric(polished$width[[1]])
    }
    if (is.list(polished) && length(polished$height) && is.finite(polished$height[[1]])) {
      height <- as.numeric(polished$height[[1]])
    }
  }

  figure_dir <- file.path("results", "figures")
  output_path <- file.path(figure_dir, basename(path))
  dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)

  if (requireNamespace("ragg", quietly = TRUE)) {
    ggplot2::ggsave(output_path, plot, width = width, height = height, units = "in",
                    dpi = dpi, device = ragg::agg_png, bg = "white")
  } else {
    ggplot2::ggsave(output_path, plot, width = width, height = height, units = "in",
                    dpi = dpi, bg = "white")
  }
  invisible(output_path)
}
