# Complete-day windows and their adjacency are checked against independent date
# membership, including short runs, run ends and a non-default window limit.
suppressPackageStartupMessages(library(dplyr))
source("scripts/utils/protocol_windows.R")

days <- bind_rows(
  tibble(support_id = "eye_medi", site = "test", Id = "long",
         Date = as.Date("2025-01-01") + 0:7),
  tibble(support_id = "eye_medi", site = "test", Id = "split",
         Date = as.Date("2025-01-01") + c(0:2, 5:6)),
  tibble(support_id = "eye_wrist_medi", site = "test", Id = "long",
         Date = as.Date("2025-01-01") + 0:1)
)

for (limit in c(3L, 6L, 8L)) {
  windows <- duration_window_manifest(days, max_days = limit)
  expected_count <- sum(vapply(c(8L, 3L, 2L, 2L), function(n) {
    sum(n - seq_len(min(n, limit)) + 1L)
  }, numeric(1)))
  stopifnot(nrow(windows) == expected_count, !anyDuplicated(windows$window_id))
  for (i in seq_len(nrow(windows))) {
    w <- windows[i, ]
    candidates <- windows |>
      filter(support_id == w$support_id, site == w$site, Id == w$Id,
             run_id == w$run_id, window_start == w$window_start)
    for (direction in c(-1L, 1L)) {
      target <- candidates |> filter(n_days == w$n_days + direction)
      id <- if (direction == 1L) w$adjacent_higher_window_id else w$adjacent_lower_window_id
      if (!nrow(target)) {
        stopifnot(is.na(id))
      } else {
        stopifnot(nrow(target) == 1L, identical(id, target$window_id))
        smaller <- if (direction == 1L) w$member_dates[[1]] else target$member_dates[[1]]
        larger <- if (direction == 1L) target$member_dates[[1]] else w$member_dates[[1]]
        stopifnot(all(smaller %in% larger), length(larger) == length(smaller) + 1L)
      }
    }
  }
}
cat("PASS: every contiguous window retained; adjacency resolves within the same run/support\n")
