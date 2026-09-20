# One maximal stored support per placement/optical/metric facet. Reference eye
# rows on these supports remain available separately for scale construction.
rq3_support_filter <- function(df) {
  base <- unname(c(eye = "eye", chest = "eye_chest", wrist = "eye_wrist")[df$placement])
  full <- df$optical == "LIGHT" | df$metric %in% c("MDER", "nvRD")
  expected <- paste0(base, ifelse(full, "_full", "_medi"))
  keep <- !is.na(base) & df$support_id == expected &
    !(df$optical == "LIGHT" & df$metric %in% c("MDER", "nvRD"))
  df[which(keep), , drop = FALSE]
}
