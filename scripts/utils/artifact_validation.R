# Strict input checks shared by downstream consumers. These do not rewrite data.
ms_assert_version <- function(x, column, expected, label = deparse(substitute(x))) {
  values <- x[[column]]
  if (is.null(values) || !length(values) || anyNA(values) ||
      any(!nzchar(as.character(values))) ||
      !identical(unique(as.character(values)), as.character(expected))) {
    stop(label, " has missing or incompatible ", column, "; expected ", expected)
  }
  invisible(x)
}

ms_assert_unique <- function(x, keys, label = deparse(substitute(x))) {
  if (!all(keys %in% names(x))) stop(label, " is missing key columns")
  if (anyNA(x[keys]) || anyDuplicated(x[keys])) {
    stop(label, " has missing or duplicate scientific keys: ", paste(keys, collapse = ", "))
  }
  invisible(x)
}
