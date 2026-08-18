source("scripts/utils/melidos_io.R")
options(timeout = max(3600, getOption("timeout")))

sites <- melidos_sites()
modalities <- c("light_glasses", "light_chest", "light_wrist", "wearlog", "sleepdiaries")
site_override <- Sys.getenv("MELIDOS_SITES", "")
modality_override <- Sys.getenv("MELIDOS_MODALITIES", "")
if (nzchar(site_override)) sites <- trimws(strsplit(site_override, ",", fixed = TRUE)[[1]])
if (nzchar(modality_override)) modalities <- trimws(strsplit(modality_override, ",", fixed = TRUE)[[1]])

dir.create("data/raw/melidos", recursive = TRUE, showWarnings = FALSE)
dir.create("logs", recursive = TRUE, showWarnings = FALSE)
total <- sum(vapply(modalities, function(x) length(sites_for_modality(sites, x)), integer(1)))
rows <- list(); k <- 0L
for (modality in modalities) for (site in sites_for_modality(sites, modality)) {
  k <- k + 1L
  destination <- raw_data_path(site, modality)
  url <- melidos_url(site, modality)
  status <- "existing"
  message(sprintf("[%s/%s] %s %s", k, total, site, modality))
  if (!file.exists(destination)) {
    tmp <- paste0(destination, ".part")
    if (file.exists(tmp)) unlink(tmp)
    tryCatch({
      utils::download.file(url, tmp, mode = "wb", quiet = FALSE)
      load_raw_file(tmp, modality)
      if (!file.rename(tmp, destination)) stop("could not finalize downloaded file")
      status <- "downloaded"
    }, error = function(e) {
      if (file.exists(tmp)) unlink(tmp)
      status <<- paste0("failed: ", conditionMessage(e))
    })
  }
  rows[[k]] <- data.frame(site, modality, url, path = destination, status,
                          bytes = if (file.exists(destination)) file.info(destination)$size else NA_real_)
}
manifest <- do.call(rbind, rows)
write.csv(manifest, "logs/download_manifest.csv", row.names = FALSE, na = "")
if (any(grepl("^failed", manifest$status))) quit(status = 1L)
