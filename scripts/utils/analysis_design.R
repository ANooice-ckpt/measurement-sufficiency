# Frozen analysis-domain definitions shared by core, RQ1-RQ3, and figures.
# Keep measurement-state choices here so a lattice change cannot silently leave
# stale hard-coded levels in downstream analyses or plots.

ms_primary_temporal_s <- function() c(10L, 20L, 30L, 40L, 60L, 120L)

# 5 min is retained only as an intentionally coarse sensitivity state. Cadences
# coarser than 5 min are outside the active wearable-logging design domain.
ms_reserve_temporal_s <- function() c(300L)

ms_all_temporal_s <- function() sort(unique(c(ms_primary_temporal_s(), ms_reserve_temporal_s())))

ms_primary_duration_days <- function() 1:6

ms_analysis_design_id <- function() {
  paste0(
    "t", paste(ms_primary_temporal_s(), collapse = "-"),
    "__d", min(ms_primary_duration_days()), "-", max(ms_primary_duration_days())
  )
}

ms_core_design_id <- function() {
  reserve <- ms_reserve_temporal_s()
  reserve_id <- if (length(reserve)) paste(reserve, collapse = "-") else "none"
  paste0(ms_analysis_design_id(), "__reserve", reserve_id)
}

ms_temporal_label <- function(x) {
  x <- as.integer(x)
  ifelse(
    x < 60L,
    paste0(x, " s"),
    ifelse(x %% 60L == 0L, paste0(x %/% 60L, " min"), paste0(x, " s"))
  )
}

ms_temporal_transition_table <- function(resolutions = ms_primary_temporal_s()) {
  r <- sort(unique(as.integer(resolutions)))
  if (length(r) < 2L) {
    return(data.frame(coarse_s = integer(), fine_s = integer(), transition = character()))
  }
  rr <- rev(r)
  coarse <- rr[-length(rr)]
  fine <- rr[-1L]
  data.frame(
    coarse_s = coarse,
    fine_s = fine,
    transition = paste(ms_temporal_label(coarse), "to", ms_temporal_label(fine)),
    stringsAsFactors = FALSE
  )
}

ms_temporal_transition_labels <- function(resolutions = ms_primary_temporal_s()) {
  ms_temporal_transition_table(resolutions)$transition
}

ms_temporal_requirement_rank <- function(resolution_s, resolutions = ms_primary_temporal_s()) {
  match(as.integer(resolution_s), rev(sort(unique(as.integer(resolutions)))))
}
