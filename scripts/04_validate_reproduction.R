suppressPackageStartupMessages({ library(dplyr); library(readxl) })

up <- new.env(parent = emptyenv())
ours <- new.env(parent = emptyenv())
load("external/zauner_position/data/prepared_metrics.RData", envir = up)
load(file.path("results", "core", "cache", "upstream", "zauner_metrics.RData"), envir = ours)

sites <- intersect(unique(ours$metrics$site), unique(up$metrics$site))
reference <- up$metrics |> filter(.data$site %in% sites)
candidate <- ours$metrics
keys_equal <- identical(reference[, 1:5], candidate[, 1:5])

tol <- 1e-12
delta <- abs(reference$value - candidate$value)
same_missing <- is.na(reference$value) & is.na(candidate$value)
value_match <- same_missing | (!is.na(reference$value) & !is.na(candidate$value) & delta <= tol)
value_equal <- all(value_match)

metric_types <- read_excel("external/zauner_position/data/metric_types.xlsx")
validation <- data.frame(
  check = c("rows", "ordered_keys", sprintf("numerical_differences_above_%g_recorded", tol), "metric_fields", "metric_classes"),
  observed = c(nrow(candidate), keys_equal, sum(!value_match), n_distinct(candidate$metric), n_distinct(metric_types[[3]])),
  expected = c(as.character(nrow(reference)), "TRUE", "recorded_not_blocking", "54", "6")
)
dir.create("results/diagnostics", recursive = TRUE, showWarnings = FALSE)
write.csv(validation, "results/diagnostics/upstream_validation.csv", row.names = FALSE)
comparison <- data.frame(metric = candidate$metric, delta = delta, match = value_match) |>
  group_by(metric) |>
  summarise(n = n(), n_mismatch = sum(!match), max_abs_delta = if (all(is.na(delta))) NA_real_ else max(delta, na.rm = TRUE), .groups = "drop")
write.csv(comparison, "results/diagnostics/upstream_value_comparison.csv", row.names = FALSE, na = "")

if (!value_equal) message(sprintf("Recorded %d numerical differences above %g; authorized to proceed with RQ1.", sum(!value_match), tol))
if (!keys_equal || nrow(candidate) != nrow(reference) ||
    n_distinct(candidate$metric) != 54L || n_distinct(metric_types[[3]]) != 6L) {
  stop("Upstream reproduction validation failed; inspect results/diagnostics/upstream_validation.csv and upstream_value_comparison.csv")
}
