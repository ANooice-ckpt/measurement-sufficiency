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
  ext <- tolower(tools::file_ext(path))

  # PDF export is intentionally disabled. Existing plot scripts may retain paired
  # PDF/PNG calls; only the PNG call produces an artifact.
  if (identical(ext, "pdf")) return(invisible(NULL))
  if (!identical(ext, "png")) {
    stop("Figure outputs must be PNG; unsupported path: ", path, call. = FALSE)
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
