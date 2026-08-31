# RQ1 partitioned-artifact helpers.
#
# The canonical RQ1 object is allowed to be a manifest rather than one large
# data frame.  Parts are immutable, versioned, and written atomically.  This
# keeps local execution bounded while allowing a server run to process parts
# concurrently or materialise a flat convenience file afterwards.

rq1_pairwise_is_partitioned <- function(x) {
  is.list(x) && identical(x$artifact_type, "partitioned_rq1_pairwise_change") &&
    length(x$parts) > 0L && !is.null(x$part_dir) && nzchar(as.character(x$part_dir[[1]]))
}

rq1_pairwise_part_paths <- function(x) {
  if (!rq1_pairwise_is_partitioned(x)) return(character())
  file.path(x$part_dir, x$parts)
}

rq1_pairwise_version <- function(x) {
  if (rq1_pairwise_is_partitioned(x)) return(as.character(x$rq1_analysis_version[[1]]))
  value <- unique(na.omit(x$rq1_analysis_version))
  if (length(value) != 1L) stop("Expected exactly one RQ1 analysis version")
  value[[1]]
}

rq1_assert_summary_version <- function(x, summary) {
  expected <- rq1_pairwise_version(x)
  if (!"rq1_analysis_version" %in% names(summary)) stop("RQ1 summary lacks rq1_analysis_version")
  observed <- unique(na.omit(as.character(summary$rq1_analysis_version)))
  if (length(observed) != 1L || !identical(observed[[1]], expected)) {
    stop("RQ1 pairwise manifest and summary versions do not match")
  }
  invisible(expected)
}

rq1_pairwise_load <- function(x, columns = NULL, filter_fn = NULL, parts = NULL) {
  if (!rq1_pairwise_is_partitioned(x)) {
    out <- x
    if (!is.null(columns)) out <- dplyr::select(out, dplyr::all_of(columns))
    if (!is.null(filter_fn)) out <- filter_fn(out)
    return(out)
  }
  paths <- rq1_pairwise_part_paths(x)
  if (!is.null(parts)) paths <- paths[parts]
  if (!length(paths)) return(tibble::tibble())
  pieces <- lapply(paths, function(path) {
    z <- readRDS(path)
    if (!is.null(columns)) z <- dplyr::select(z, dplyr::all_of(columns))
    if (!is.null(filter_fn)) z <- filter_fn(z)
    z
  })
  tibble::as_tibble(data.table::rbindlist(pieces, use.names = TRUE, fill = TRUE))
}

rq1_write_part_atomic <- function(x, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  tmp <- paste0(path, ".tmp")
  ok <- paste0(path, ".ok")
  if (file.exists(tmp)) unlink(tmp, force = TRUE)
  compression <- tolower(Sys.getenv("RQ1_PART_COMPRESSION", unset = "gzip"))
  if (!compression %in% c("gzip", "bzip2", "xz", "none")) compression <- "gzip"
  saveRDS(x, tmp, compress = compression)
  if (file.exists(path)) unlink(path, force = TRUE)
  if (!file.rename(tmp, path)) stop("Could not atomically install RQ1 part: ", path)
  writeLines(c("complete", paste0("rows=", nrow(x))), ok, useBytes = TRUE)
  invisible(path)
}

rq1_read_part_manifest <- function(path) {
  if (!file.exists(path)) return(tibble::tibble())
  readr::read_csv(path, show_col_types = FALSE, progress = FALSE)
}
