melidos_repositories <- c(
  BAUA = "BroszioEtAl_Dataset_2025", FUSPCEU = "BaezaEtAl_Dataset_2025",
  IZTECH = "DidikogluEtAl_Dataset_2025", KNUST = "AkuffoEtAl_Dataset_2025",
  MPI = "GuidolinEtAl_Dataset_2025", RISE = "NilssonTengelinEtAl_Dataset_2026",
  THUAS = "AertsEtAl_Dataset_2025", TUM = "HildenEtAl_Dataset_2025",
  UCR = "Sancho-SalasEtAl_Dataset_2025"
)

melidos_sites <- function() names(melidos_repositories)

sites_for_modality <- function(sites, modality) {
  if (modality %in% c(
    "light_chest", "light_wrist",
    "light_chest_1minute", "light_wrist_1minute"
  )) setdiff(sites, "MPI") else sites
}

melidos_url <- function(site, modality) {
  repo <- unname(melidos_repositories[[site]])
  if (is.null(repo) || !nzchar(repo)) stop("Unknown MeLiDos site: ", site)

  if (identical(modality, "trial_times")) {
    return(sprintf(
      "https://raw.githubusercontent.com/MeLiDosProject/%s/main/data/imported/trial_times.RData",
      repo
    ))
  }

  folder <- if (grepl("^light_", modality)) "light" else "continuous"
  sprintf(
    "https://raw.githubusercontent.com/MeLiDosProject/%s/main/data/imported/%s/%s.RData",
    repo, folder, modality
  )
}

raw_data_path <- function(site, modality) {
  file.path("data", "raw", "melidos", paste0(site, "__", modality, ".RData"))
}

object_name_for_modality <- function(modality) {
  if (identical(modality, "sleepdiaries")) return("sleepdiary")
  sub("1minute$", "1min", modality)
}

load_raw_file <- function(path, modality) {
  env <- new.env(parent = emptyenv())
  load(path, envir = env)
  object_name <- object_name_for_modality(modality)
  if (!exists(object_name, envir = env, inherits = FALSE)) {
    stop(sprintf(
      "Expected '%s' in %s; found %s",
      object_name, path, paste(ls(env), collapse = ", ")
    ))
  }
  env[[object_name]]
}

participant_days <- function(x) {
  if (!all(c("Id", "Datetime") %in% names(x))) return(NA_integer_)
  length(unique(paste(x$Id, as.Date(x$Datetime))))
}
