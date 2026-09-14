#!/usr/bin/env Rscript

# ================================================================
# 16_appendix_figures.R
# Appendix figures for the measurement-sufficiency project
#
# Current contents:
#   Fig. S16  MeLiDos sites and sample distribution (Nature-style map)
#
# Usage (from project root):
#   source('16_appendix_figures.R')
#   make_appendix_figures()
#
# Output:
#   figures/appendix/FigS16_MeLiDos_sites_map.png
#   figures/appendix/FigS16_MeLiDos_sites_map.pdf
# ================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(sf)
  library(patchwork)
  library(ggrepel)
  library(readr)
  library(stringr)
  library(tibble)
  library(purrr)
  library(scales)
  library(rnaturalearth)
  library(rnaturalearthdata)
})

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0 || all(is.na(x))) y else x

# ----------------------------------------------------------------
# 1. Canonical site metadata from melidosData documentation
# ----------------------------------------------------------------
melidos_sites_reference <- function() {
  tibble::tribble(
    ~site_abbr, ~site_name, ~city, ~country,           ~lat,      ~lon,
    "RISE",    "RISE",    "Borås",   "Sweden",           57.71567,  12.89087,
    "THUAS",   "THUAS",   "Delft",   "Netherlands",      52.01160,   4.35710,
    "BAUA",    "BAUA",    "Dortmund","Germany",          51.498204,  7.416708,
    "MPI",     "MPI",     "Tübingen","Germany",          48.52160,   9.05760,
    "TUM",     "TUM",     "Munich",  "Germany",          48.13330,  11.56670,
    "FUSPCEU", "FUSPCEU", "Madrid",  "Spain",            40.41650,  -3.70256,
    "IZTECH",  "IZTECH",  "Izmir",   "Turkey",           38.32000,  26.63000,
    "UCR",     "UCR",     "San José", "Costa Rica",       9.93720, -84.05090,
    "KNUST",   "KNUST",   "Kumasi",  "Ghana",             6.675007, -1.572644
  ) %>%
    mutate(region = case_when(
      country %in% c("Sweden", "Netherlands", "Germany", "Spain", "Turkey") ~ "Europe / Mediterranean",
      country == "Ghana" ~ "West Africa",
      country == "Costa Rica" ~ "Central America",
      TRUE ~ "Other"
    ))
}

# ----------------------------------------------------------------
# 2. Try to recover per-site sample sizes from local project files
# ----------------------------------------------------------------

candidate_site_tables <- function(project_root = ".") {
  c(
    file.path(project_root, "data", "derived", "site_summary.csv"),
    file.path(project_root, "data", "derived", "site_counts.csv"),
    file.path(project_root, "data", "derived", "melidos_site_counts.csv"),
    file.path(project_root, "data", "derived", "participant_day_site_summary.csv"),
    file.path(project_root, "data", "analysis", "site_summary.csv"),
    file.path(project_root, "derived", "site_summary.csv"),
    file.path(project_root, "outputs", "site_summary.csv")
  )
}

# Heuristic reader: search common flattened analysis tables and infer site counts.
# The goal is robustness rather than dependence on one repo-specific filename.
infer_site_counts_from_project <- function(project_root = ".") {

  ref <- melidos_sites_reference()

  # 2a. Direct site summary files if they exist
  direct_paths <- candidate_site_tables(project_root)
  direct_hit <- direct_paths[file.exists(direct_paths)]

  if (length(direct_hit) > 0) {
    message("Using existing site summary file: ", direct_hit[1])
    x <- readr::read_csv(direct_hit[1], show_col_types = FALSE)
    nm <- names(x)

    site_col <- nm[str_detect(tolower(nm), "site|center|centre|study_site|abbr")][1] %||% nm[1]
    n_col    <- nm[str_detect(tolower(nm), "^n$|participants|participant_n|n_participants|sample")][1]
    day_col  <- nm[str_detect(tolower(nm), "days|participant_days|n_days|records")][1]

    out <- x %>%
      transmute(
        site_abbr = as.character(.data[[site_col]]),
        participants = if (!is.na(n_col)) suppressWarnings(as.numeric(.data[[n_col]])) else NA_real_,
        participant_days = if (!is.na(day_col)) suppressWarnings(as.numeric(.data[[day_col]])) else NA_real_
      )

    out$site_abbr <- toupper(out$site_abbr)
    out <- ref %>% left_join(out, by = "site_abbr")
    return(out)
  }

  # 2b. Search for a participant-level or day-level flat file and derive counts
  search_paths <- list.files(project_root, recursive = TRUE, full.names = TRUE,
                             pattern = "\\.(csv|tsv|rds|qs|parquet)$")

  # Restrict to plausible analysis tables
  search_paths <- search_paths[str_detect(tolower(search_paths),
                                          "participant|daily|analysis|flatten|merged|master|final|harmoni|melidos|exposure")]

  if (length(search_paths) == 0) {
    warning("No plausible local data table found. Falling back to site-only map without sample counts.")
    return(ref %>% mutate(participants = NA_real_, participant_days = NA_real_))
  }

  message("Trying to infer site counts from local project tables ...")

  inferred <- NULL

  for (pth in search_paths) {
    dat <- tryCatch({
      if (str_detect(pth, "\\.csv$")) readr::read_csv(pth, show_col_types = FALSE, progress = FALSE)
      else if (str_detect(pth, "\\.tsv$")) readr::read_tsv(pth, show_col_types = FALSE, progress = FALSE)
      else if (str_detect(pth, "\\.rds$")) readRDS(pth)
      else NULL
    }, error = function(e) NULL)

    if (is.null(dat) || !is.data.frame(dat)) next

    nm_low <- tolower(names(dat))

    site_col <- names(dat)[which(nm_low %in% c("site", "site_abbr", "study_site", "site_code", "centre", "center", "study_center"))][1] %||%
      names(dat)[str_detect(nm_low, "site|centre|center")][1]

    pid_col <- names(dat)[which(nm_low %in% c("participant_id", "participant", "pid", "id_participant", "subject_id", "subject"))][1] %||%
      names(dat)[str_detect(nm_low, "participant|subject")][1]

    day_col <- names(dat)[which(nm_low %in% c("day", "study_day", "analysis_day", "date", "local_date"))][1] %||%
      names(dat)[str_detect(nm_low, "study_day|analysis_day|local_date|date|\\bday\\b")][1]

    if (is.na(site_col) || is.na(pid_col)) next

    tmp <- dat %>%
      mutate(
        .site = toupper(as.character(.data[[site_col]])),
        .pid  = as.character(.data[[pid_col]])
      )

    if (!is.na(day_col)) {
      tmp <- tmp %>% mutate(.day = as.character(.data[[day_col]]))
    } else {
      tmp <- tmp %>% mutate(.day = NA_character_)
    }

    site_levels_found <- unique(tmp$.site)
    overlap <- sum(ref$site_abbr %in% site_levels_found)

    if (overlap < 5) next

    inferred <- tmp %>%
      filter(.site %in% ref$site_abbr) %>%
      group_by(.site) %>%
      summarise(
        participants = n_distinct(.pid),
        participant_days = if (all(is.na(.day))) NA_real_ else n_distinct(paste(.pid, .day, sep = "__")),
        .groups = "drop"
      ) %>%
      rename(site_abbr = .site)

    message("Counts inferred from: ", pth)
    break
  }

  if (is.null(inferred)) {
    warning("Could not infer site counts from local files. Falling back to site-only map.")
    return(ref %>% mutate(participants = NA_real_, participant_days = NA_real_))
  }

  ref %>% left_join(inferred, by = "site_abbr")
}

# ----------------------------------------------------------------
# 3. Nature-style site figure
# ----------------------------------------------------------------
make_melidos_site_map <- function(project_root = ".",
                                  out_dir = file.path("figures", "appendix"),
                                  width = 11,
                                  height = 6.8,
                                  dpi = 320) {

  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  sites <- infer_site_counts_from_project(project_root)

  # Fallback if participant counts are unavailable
  counts_available <- !all(is.na(sites$participants))

  if (!counts_available) {
    sites <- sites %>% mutate(participants = 1)
    radius_note <- "Point size not scaled: participant counts unavailable"
  } else {
    radius_note <- "Point area scaled by number of participants"
  }

  # Nice label strings
  sites <- sites %>%
    mutate(
      label = ifelse(
        !counts_available,
        paste0(city, "\n", country),
        paste0(city, "\n", country, "  (n=", participants,
               ifelse(!is.na(participant_days), paste0(", ", participant_days, " d"), ""), ")")
      ),
      label_short = ifelse(!counts_available, city, paste0(city, "  n=", participants))
    )

  # World basemap
  world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
  world <- st_transform(world, 4326)

  # Main extent: Atlantic-centered view capturing Costa Rica + Europe/Africa
  main_xlim <- c(-95, 35)
  main_ylim <- c(0, 62)

  # Inset extent for Europe cluster
  eur_xlim <- c(-12, 30)
  eur_ylim <- c(35, 60)

  # Color scheme: restrained, Nature-like
  fills <- c("Europe / Mediterranean" = "#3C78A8",
             "West Africa" = "#5BA27C",
             "Central America" = "#CC7A33")

  # Clean segments for context grouping (schematic, not geographic routing)
  hub <- tibble(lon = -22, lat = 33)  # visual anchor in eastern Atlantic
  segments <- sites %>%
    transmute(x = hub$lon, y = hub$lat, xend = lon, yend = lat, region)

  p_main <- ggplot() +
    geom_sf(data = world, fill = "grey97", color = "grey82", linewidth = 0.25) +
    geom_curve(data = segments,
               aes(x = x, y = y, xend = xend, yend = yend, color = region),
               curvature = 0.14, linewidth = 0.35, alpha = 0.55,
               arrow = arrow(length = unit(0.07, "inches"), type = "closed")) +
    geom_point(data = sites,
               aes(lon, lat, size = participants, fill = region),
               shape = 21, color = "white", stroke = 0.5, alpha = 0.96) +
    ggrepel::geom_label_repel(
      data = sites,
      aes(lon, lat, label = label_short),
      size = 3.0,
      fill = alpha("white", 0.95),
      color = "black",
      label.size = 0.15,
      min.segment.length = 0,
      seed = 123,
      box.padding = 0.18,
      point.padding = 0.15,
      segment.color = alpha("grey35", 0.7),
      segment.linewidth = 0.25,
      max.overlaps = Inf
    ) +
    scale_fill_manual(values = fills, drop = FALSE) +
    scale_color_manual(values = fills, guide = "none") +
    scale_size_area(max_size = 11, breaks = pretty_breaks(n = 4)) +
    coord_sf(xlim = main_xlim, ylim = main_ylim, expand = FALSE) +
    labs(
      title = "MeLiDos field-study sites",
      subtitle = "Nine sites across seven countries; point area reflects site sample size",
      x = NULL, y = NULL
    ) +
    theme_minimal(base_size = 10) +
    theme(
      panel.grid.major = element_line(color = "grey90", linewidth = 0.25),
      panel.grid.minor = element_blank(),
      axis.text = element_blank(),
      plot.title = element_text(face = "bold", size = 13, hjust = 0),
      plot.subtitle = element_text(size = 9.5, color = "grey25"),
      legend.position = "right",
      legend.title = element_text(size = 9),
      legend.text = element_text(size = 8.5),
      panel.background = element_rect(fill = "white", color = NA),
      plot.background = element_rect(fill = "white", color = NA)
    ) +
    guides(
      fill = guide_legend(title = "Region", override.aes = list(size = 4)),
      size = guide_legend(title = "Participants")
    )

  europe_sites <- sites %>% filter(region == "Europe / Mediterranean")

  p_inset <- ggplot() +
    geom_sf(data = world, fill = "grey97", color = "grey83", linewidth = 0.23) +
    geom_point(data = europe_sites,
               aes(lon, lat, size = participants),
               shape = 21, fill = fills[["Europe / Mediterranean"]],
               color = "white", stroke = 0.45, alpha = 0.96) +
    ggrepel::geom_label_repel(
      data = europe_sites,
      aes(lon, lat, label = paste0(city, ifelse(is.na(participants), "", paste0("  n=", participants)))),
      size = 2.8,
      fill = alpha("white", 0.96),
      color = "black",
      label.size = 0.14,
      min.segment.length = 0,
      seed = 321,
      box.padding = 0.15,
      point.padding = 0.12,
      segment.color = alpha("grey35", 0.75),
      segment.linewidth = 0.24,
      max.overlaps = Inf
    ) +
    scale_size_area(max_size = 8, guide = "none") +
    coord_sf(xlim = eur_xlim, ylim = eur_ylim, expand = FALSE) +
    labs(title = "Europe / Mediterranean cluster") +
    theme_void(base_size = 9) +
    theme(
      plot.title = element_text(face = "bold", size = 10, hjust = 0),
      panel.background = element_rect(fill = "white", color = "grey85", linewidth = 0.25),
      plot.background = element_rect(fill = "white", color = NA)
    )

  # Summary block
  total_n <- suppressWarnings(sum(sites$participants, na.rm = TRUE))
  total_d <- suppressWarnings(sum(sites$participant_days, na.rm = TRUE))
  summary_lines <- c(
    paste0("Sites: ", nrow(sites)),
    paste0("Countries: ", n_distinct(sites$country)),
    if (is.finite(total_n) && total_n > 0) paste0("Participants: ", comma(total_n)) else NULL,
    if (is.finite(total_d) && total_d > 0) paste0("Participant-days: ", comma(total_d)) else NULL,
    radius_note
  )

  p_note <- ggplot() +
    annotate("text", x = 0, y = 1,
             hjust = 0, vjust = 1, family = "sans", size = 3.4,
             label = paste(summary_lines, collapse = "\n")) +
    coord_cartesian(xlim = c(0, 1), ylim = c(0, 1), expand = FALSE, clip = "off") +
    theme_void() +
    theme(
      panel.border = element_rect(color = "grey80", fill = NA, linewidth = 0.35),
      plot.margin = margin(8, 8, 8, 8)
    )

  final_plot <- p_main + ((p_inset / p_note) + plot_layout(heights = c(3.0, 1.1))) +
    plot_layout(widths = c(3.8, 1.9)) +
    plot_annotation(
      caption = paste(
        "Site coordinates follow the MeLiDos / melidosData site documentation.",
        "Per-site sample counts are inferred from local project data when available; otherwise sites are shown without count scaling.",
        sep = " "
      )
    ) &
    theme(plot.caption = element_text(size = 8, color = "grey35", hjust = 0))

  png_path <- file.path(out_dir, "FigS16_MeLiDos_sites_map.png")
  pdf_path <- file.path(out_dir, "FigS16_MeLiDos_sites_map.pdf")

  ggsave(png_path, final_plot, width = width, height = height, dpi = dpi, bg = "white")
  ggsave(pdf_path, final_plot, width = width, height = height, device = cairo_pdf, bg = "white")

  message("Saved: ", png_path)
  message("Saved: ", pdf_path)

  invisible(list(plot = final_plot, sites = sites, png = png_path, pdf = pdf_path))
}

# ----------------------------------------------------------------
# 4. Project entry point
# ----------------------------------------------------------------
make_appendix_figures <- function(project_root = ".") {
  make_melidos_site_map(project_root = project_root)
}

if (sys.nframe() == 0) {
  make_appendix_figures(project_root = ".")
}
