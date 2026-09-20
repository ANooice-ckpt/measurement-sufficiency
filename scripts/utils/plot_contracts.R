# Plot-side contracts for the current core/RQ artifact graph.
# Plot scripts may reshape frozen summaries for display, but must not refit,
# recompute estimands, or silently fall back to legacy data/derived paths.

if (!exists("ms_main_figure_registry", mode = "function") &&
    file.exists("scripts/utils/figure_registry.R")) {
  source("scripts/utils/figure_registry.R")
}
if (!exists("ms_direction_ratio", mode = "function") &&
    file.exists("scripts/utils/figure_atlas.R")) {
  source("scripts/utils/figure_atlas.R")
}

if (!exists("ms_fig1_refine_main", mode = "function") &&
    file.exists("scripts/utils/fig1_refinement.R")) {
  source("scripts/utils/fig1_refinement.R")
}
if (!exists("ms_fig2_refine_main", mode = "function") &&
    file.exists("scripts/utils/fig2_refinement.R")) {
  source("scripts/utils/fig2_refinement.R")
}

if (!exists("ms_polish_main_figure", mode = "function") &&
    file.exists("scripts/utils/figure_polish.R")) {
  source("scripts/utils/figure_polish.R")
}

# MS_PLOT_PREP_ONLY=1 constructs plots in memory without exporting images,
# display tables or manifests, creating output directories, or downloading
# appendix inputs. Entry scripts guard direct writers with the same predicate.
ms_plot_prep_only <- function() {
  identical(Sys.getenv("MS_PLOT_PREP_ONLY", unset = "0"), "1")
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
  if (ms_plot_prep_only()) return(invisible(path))

  figure_rows <- tibble::as_tibble(figure_rows)
  # Mature RQ2/RQ3 plotting code may still emit a pre-insertion figure identity.
  # Convert exactly once on entry; a stored manifest is already canonical/current
  # and must never be renumbered a second time.
  if ("figure" %in% names(figure_rows)) {
    figure_rows$figure <- ms_main_figure_from_legacy_id(figure_rows$figure)
  }
  figure_rows$generated_at_utc <- format(Sys.time(), tz = "UTC", usetz = TRUE)

  legacy_dir <- dirname(path)
  rq_dir <- dirname(legacy_dir)
  manifest_path <- if (
    identical(basename(legacy_dir), "figures") && grepl("^rq[0-9]+$", basename(rq_dir))
  ) {
    file.path(rq_dir, basename(path))
  } else {
    path
  }

  # Multiple main-figure writes may update the same per-RQ manifest. Keep earlier
  # canonical main-figure rows only when their PNG still exists; the downstream
  # runner clears results/figures first, so stale main figures cannot survive a
  # complete rerun.
  if (file.exists(manifest_path) && "figure" %in% names(figure_rows)) {
    previous <- suppressMessages(readr::read_csv(manifest_path, show_col_types = FALSE, progress = FALSE))
    if ("figure" %in% names(previous)) {
      current_ids <- ms_current_main_figure_ids()
      existing_ids <- current_ids[
        file.exists(file.path("results", "figures", paste0(current_ids, ".png")))
      ]
      keep_previous <- previous |>
        dplyr::filter(
          figure %in% existing_ids,
          !figure %in% figure_rows$figure
        )
      if (nrow(keep_previous)) figure_rows <- dplyr::bind_rows(keep_previous, figure_rows)
    }
  }

  if ("figure" %in% names(figure_rows)) {
    order_ids <- ms_current_main_figure_ids()
    figure_rows <- figure_rows |>
      dplyr::distinct(figure, .keep_all = TRUE) |>
      dplyr::arrange(match(figure, order_ids), figure)
  }

  dir.create(dirname(manifest_path), recursive = TRUE, showWarnings = FALSE)
  readr::write_csv(figure_rows, manifest_path, na = "")

  if (!identical(legacy_dir, dirname(manifest_path)) && dir.exists(legacy_dir)) {
    unlink(legacy_dir, recursive = TRUE, force = TRUE)
  }
  invisible(manifest_path)
}

ms_plot_save <- function(plot, path, width, height,
                         dpi = if (exists("MS_RASTER_DPI", inherits = TRUE)) MS_RASTER_DPI else 600) {
  if (ms_plot_prep_only()) return(invisible(path))

  # Mature plotting code may still request its pre-insertion filename. Convert
  # that request once to the current public filename before any export.
  resolved_name <- ms_main_figure_filename_from_legacy(basename(path))
  if (!identical(resolved_name, basename(path))) path <- file.path(dirname(path), resolved_name)
  ext <- tolower(tools::file_ext(path))

  if (identical(ext, "pdf")) return(invisible(NULL))
  if (!identical(ext, "png")) {
    stop("Figure outputs must be PNG; unsupported path: ", path, call. = FALSE)
  }

  current_id <- tools::file_path_sans_ext(basename(path))
  legacy_id <- ms_main_figure_to_legacy_id(current_id)
  caller_env <- parent.frame()

  if (identical(current_id, "Fig1_RQ1") &&
      exists("ms_fig1_refine_main", mode = "function")) {
    refined <- ms_fig1_refine_main(caller_env)
    if (is.list(refined) && !is.null(refined$p1a_core)) {
      assign("p1a_core", refined$p1a_core, envir = caller_env)
      if (!is.null(refined$assoc_text)) assign("assoc_text", refined$assoc_text, envir = caller_env)
      if (!is.null(refined$p1b_core)) assign("p1b_core", refined$p1b_core, envir = caller_env)
      if (!is.null(refined$p1b_shape_legend)) {
        assign("p1b_shape_legend", refined$p1b_shape_legend, envir = caller_env)
      }
    }
  }

  # Current Figs. 3/4 are fully composed by their entrypoints; they do not
  # refine again at export. The polish dispatcher preserves that composition
  # without replacing shared functions in the caller's R session.
  if (exists("ms_polish_main_figure", mode = "function")) {
    # figure_polish.R intentionally retains legacy component identities. Passing
    # the registry-derived legacy filename preserves the mature layout without
    # another current-number dispatch table here.
    polish_path <- file.path(dirname(path), paste0(legacy_id, ".png"))
    polished <- ms_polish_main_figure(plot, polish_path, caller_env, width, height)
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
