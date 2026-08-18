source("scripts/utils/melidos_io.R")
suppressPackageStartupMessages({ library(dplyr); library(tidyr) })

files <- list.files("data/raw/melidos", pattern = "[.]RData$", full.names = TRUE)
if (!length(files)) stop("No source files found; run scripts/01_download_melidos.R first")

inventory <- lapply(files, function(path) {
  bits <- strsplit(tools::file_path_sans_ext(basename(path)), "__", fixed = TRUE)[[1]]
  site <- bits[[1]]; modality <- bits[[2]]; x <- load_raw_file(path, modality)
  ids <- if ("Id" %in% names(x)) unique(x$Id) else character()
  dt <- if ("Datetime" %in% names(x)) x$Datetime else as.POSIXct(character())
  numeric_cols <- names(x)[vapply(x, is.numeric, logical(1))]
  epochs <- if (length(dt) > 1L) as.numeric(diff(sort(unique(dt))), units = "secs") else numeric()
  data.frame(site, modality, n_participants = length(ids), n_rows = nrow(x),
    date_min = if (length(dt)) as.character(min(dt, na.rm = TRUE)) else NA_character_,
    date_max = if (length(dt)) as.character(max(dt, na.rm = TRUE)) else NA_character_,
    n_participant_days = participant_days(x), columns = paste(names(x), collapse = "|"),
    missing_numeric_fraction = if (length(numeric_cols)) mean(is.na(as.matrix(x[numeric_cols]))) else NA_real_,
    median_epoch_seconds = if (length(epochs)) median(epochs[is.finite(epochs) & epochs > 0], na.rm = TRUE) else NA_real_,
    bytes = file.info(path)$size)
}) |> bind_rows()

ids_for <- function(site, modality) {
  path <- raw_data_path(site, modality)
  if (!file.exists(path)) return(character())
  unique(load_raw_file(path, modality)$Id)
}
intersections <- lapply(sort(unique(inventory$site)), function(site) {
  e <- ids_for(site, "light_glasses"); c <- ids_for(site, "light_chest"); w <- ids_for(site, "light_wrist")
  data.frame(site, e_and_c = length(intersect(e, c)), e_and_w = length(intersect(e, w)),
             e_and_c_and_w = length(Reduce(intersect, list(e, c, w))))
}) |> bind_rows()

dir.create("logs", showWarnings = FALSE, recursive = TRUE)
write.csv(inventory, "logs/data_inventory.csv", row.names = FALSE, na = "")
write.csv(intersections, "logs/sample_intersections.csv", row.names = FALSE)

shown <- inventory[, c("site", "modality", "n_participants", "n_rows", "date_min", "date_max", "n_participant_days", "missing_numeric_fraction", "median_epoch_seconds")]
lines <- c("# Data inventory", "", sprintf("Generated on %s by `scripts/02_inventory.R`.", format(Sys.time())), "",
  "Machine-readable outputs: `logs/data_inventory.csv` and `logs/sample_intersections.csv`.", "",
  "## Site × modality", "", paste0("| ", paste(names(shown), collapse = " | "), " |"),
  paste0("|", paste(rep("---", ncol(shown)), collapse = "|"), "|"),
  apply(shown, 1, function(z) paste0("| ", paste(z, collapse = " | "), " |")), "",
  "## Placement participant intersections", "", "| site | E∩C | E∩W | E∩C∩W |", "|---|---:|---:|---:|",
  apply(intersections, 1, function(z) paste0("| ", paste(z, collapse = " | "), " |")), "",
  "MPI has no comparable ActLumus chest/wrist modalities. Intersections are descriptive only and are not imposed on later measurement operators.")
writeLines(lines, "docs/DATA_INVENTORY.md")
