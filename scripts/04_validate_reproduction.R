suppressPackageStartupMessages({ library(dplyr); library(readxl) })

up <- new.env(parent = emptyenv())
ours <- new.env(parent = emptyenv())
load("external/zauner_position/data/prepared_metrics.RData", envir = up)
load("data/derived/zauner_metrics.RData", envir = ours)

sites <- intersect(unique(ours$metrics$site), unique(up$metrics$site))
reference <- up$metrics |> filter(.data$site %in% sites)
candidate <- ours$metrics
keys_equal <- identical(reference[, 1:5], candidate[, 1:5])
delta <- abs(reference$value - candidate$value)
same_missing <- is.na(reference$value) & is.na(candidate$value)
value_match <- same_missing | (!is.na(reference$value) & !is.na(candidate$value) & delta <= 1e-12)
value_equal <- all(value_match)

metric_types <- read_excel("external/zauner_position/data/metric_types.xlsx")
validation <- data.frame(
  check = c("rows", "ordered_keys", "values_tolerance_1e-12", "metric_fields", "metric_classes"),
  observed = c(nrow(candidate), keys_equal, value_equal, n_distinct(candidate$metric), n_distinct(metric_types[[3]])),
  expected = c(as.character(nrow(reference)), "TRUE", "diagnostic_only", "54", "6")
)
dir.create("results/diagnostics", recursive = TRUE, showWarnings = FALSE)
write.csv(validation, "results/diagnostics/upstream_validation.csv", row.names = FALSE)
comparison <- data.frame(metric = candidate$metric, delta = delta, match = value_match) |>
  group_by(metric) |>
  summarise(n = n(), n_mismatch = sum(!match), max_abs_delta = if (all(is.na(delta))) NA_real_ else max(delta, na.rm = TRUE), .groups = "drop")
write.csv(comparison, "results/diagnostics/upstream_value_comparison.csv", row.names = FALSE, na = "")

if (!keys_equal || nrow(candidate) != nrow(reference) || n_distinct(candidate$metric) != 54L || n_distinct(metric_types[[3]]) != 6L) {
  stop("Upstream reproduction validation failed; inspect results/diagnostics/upstream_validation.csv")
}
