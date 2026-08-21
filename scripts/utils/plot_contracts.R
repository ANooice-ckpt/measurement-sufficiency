# Plot-side contracts for the current core/RQ artifact graph.
# Plot scripts may reshape frozen summaries for display, but must not refit,
# recompute estimands, or silently fall back to legacy data/derived paths.

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

ms_plot_assert_core <- function(values, expected = "v3_sparse_sampling_complete_days") {
  value <- ms_plot_one_version(values, "core_artifact_version")
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
  readr::write_csv(figure_rows, path, na = "")
  invisible(path)
}

ms_plot_save <- function(plot, path, width, height, dpi = 240) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  if (grepl("\\.pdf$", path, ignore.case = TRUE)) {
    ggplot2::ggsave(path, plot, width = width, height = height, units = "in",
                    device = grDevices::cairo_pdf, bg = "white")
  } else {
    ggplot2::ggsave(path, plot, width = width, height = height, units = "in",
                    dpi = dpi, bg = "white")
  }
  invisible(path)
}
