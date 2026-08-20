# Sparse temporal sampling on the native phase of the harmonized 10-s source grid.
# This file is sourced after core_artifacts.R so the corrected operator replaces
# the earlier epoch-zero implementation without changing any support artifacts.

core_source_grid_phase <- function(site, Id, sec_round) {
  group_key <- paste(site, Id, sep = "|")
  first <- !duplicated(group_key)
  phase_by_group <- sec_round[first] %% 10L
  names(phase_by_group) <- group_key[first]
  phase <- unname(phase_by_group[group_key])
  if (anyNA(phase) || any(((sec_round - phase) %% 10L) != 0L)) {
    stop("Core source timestamps do not share a stable 10-s grid phase within participant")
  }
  phase
}

core_make_series <- function(support, placement, optical, resolution_s) {
  resolution_s <- as.integer(resolution_s)
  if (!resolution_s %in% core_all_resolutions()) stop("Unsupported resolution: ", resolution_s)
  if (resolution_s %% 10L != 0L) stop("Resolution must be an integer multiple of the 10-s source grid")

  med_nm <- paste0("MEDI_", placement)
  light_nm <- paste0("LIGHT_", placement)
  if (!med_nm %in% names(support)) stop("Missing channel: ", med_nm)
  if (!light_nm %in% names(support)) stop("Missing channel: ", light_nm)

  x <- support |>
    dplyr::transmute(
      site, Id, Date, Datetime,
      MEDI = if (optical == "MEDI") .data[[med_nm]] else .data[[light_nm]],
      LIGHT = if (optical == "MEDI") .data[[light_nm]] else NA_real_
    )

  sec <- as.numeric(x$Datetime)
  if (any(!is.finite(sec))) stop("Non-finite timestamp in core support")
  sec_round <- round(sec)
  if (any(abs(sec - sec_round) > 1e-6)) stop("Core source timestamps are not on integer seconds")
  phase10 <- core_source_grid_phase(x$site, x$Id, sec_round)
  if (anyDuplicated(paste(x$site, x$Id, sec_round, sep = "|"))) {
    stop("Duplicate source timestamps in core_make_series")
  }
  if (resolution_s == 10L) return(x)

  keep <- ((sec_round - phase10) %% resolution_s) == 0L
  out <- x[keep, , drop = FALSE]
  if (!nrow(out)) stop("Sparse sampling produced no rows at ", resolution_s, " s")

  src_idx <- match(
    paste(out$site, out$Id, as.numeric(out$Datetime), sep = "|"),
    paste(x$site, x$Id, as.numeric(x$Datetime), sep = "|")
  )
  if (anyNA(src_idx)) stop("Sparse-sampled timestamp is not a source timestamp")
  same_num <- function(a, b) all((is.na(a) & is.na(b)) | (!is.na(a) & !is.na(b) & a == b))
  if (!same_num(out$MEDI, x$MEDI[src_idx]) || !same_num(out$LIGHT, x$LIGHT[src_idx])) {
    stop("Sparse sampling altered retained measurement values")
  }
  out
}
