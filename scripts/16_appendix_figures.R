#!/usr/bin/env Rscript

# Supplementary / appendix figures.
# Run from the repository root:
#   Rscript scripts/16_appendix_figures.R
# Output:
#   results/figures/FigS_MeLiDos_sites.png

.ms_file <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
if (length(.ms_file)) {
  .ms_script <- normalizePath(sub("^--file=", "", .ms_file[[1]]), winslash = "/", mustWork = TRUE)
  .ms_root <- normalizePath(file.path(dirname(.ms_script), ".."), winslash = "/", mustWork = TRUE)
  setwd(.ms_root)
}
rm(.ms_file)
if (exists(".ms_script")) rm(.ms_script)
if (exists(".ms_root")) rm(.ms_root)

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(sf)
  library(tibble)
  library(scales)
})

source("scripts/utils/figure_style.R")
source("scripts/utils/melidos_io.R")

OUT_DIR <- file.path("results", "figures")
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

# -----------------------------------------------------------------------------
# Fig. S — MeLiDos field-study sites and sample distribution
# -----------------------------------------------------------------------------

# Label anchors are specified in lon/lat and then projected together with the
# site coordinates. The European cluster is deliberately spread in four
# directions to keep leader lines short and avoid label collisions.
site_reference <- tibble::tribble(
  ~site,      ~city,       ~country,        ~lat,      ~lon,      ~label_lon, ~label_lat, ~hjust,
  "RISE",     "Borås",     "Sweden",         57.71567,   12.89087,   22.0,       60.2,       0.0,
  "THUAS",    "Delft",     "Netherlands",    52.01160,    4.35710,  -11.5,       55.0,       0.0,
  "BAUA",     "Dortmund",  "Germany",        51.49820,    7.41671,   24.0,       54.0,       0.0,
  "MPI",      "Tübingen",  "Germany",        48.52160,    9.05760,  -11.5,       46.7,       0.0,
  "TUM",      "Munich",    "Germany",        48.13330,   11.56670,   24.5,       46.2,       0.0,
  "FUSPCEU",  "Madrid",    "Spain",          40.41650,   -3.70256,  -16.5,       38.0,       0.0,
  "IZTECH",   "Izmir",     "Türkiye",        38.32000,   26.63000,   34.0,       34.2,       0.0,
  "UCR",      "San José",  "Costa Rica",      9.93720,  -84.05090,  -69.5,       14.0,       0.0,
  "KNUST",    "Kumasi",    "Ghana",           6.67501,   -1.57264,   10.0,        9.8,       0.0
)

# Count every participant represented anywhere in the locally downloaded MeLiDos
# source data. This deliberately does NOT condition on eye, chest, wrist, diary,
# or any other modality: the site bubble is a study-sample descriptor, not an
# analysis-specific complete-case count.
read_site_counts <- function() {
  raw_dir <- file.path("data", "raw", "melidos")
  if (!dir.exists(raw_dir)) {
    stop("Missing ", raw_dir, ". Run scripts/01_download_melidos.R first.")
  }

  rows <- lapply(melidos_sites(), function(site) {
    paths <- list.files(
      raw_dir,
      pattern = paste0("^", site, "__.*[.]RData$"),
      full.names = TRUE
    )

    if (!length(paths)) {
      stop("No local MeLiDos source files found for site ", site, " in ", raw_dir)
    }

    ids <- character()
    files_with_ids <- 0L

    for (path in paths) {
      env <- new.env(parent = emptyenv())
      ok <- tryCatch({
        load(path, envir = env)
        TRUE
      }, error = function(e) FALSE)
      if (!ok) next

      object_names <- ls(env, all.names = TRUE)
      for (object_name in object_names) {
        obj <- env[[object_name]]
        if (is.data.frame(obj) && "Id" %in% names(obj)) {
          x <- as.character(obj$Id)
          x <- x[!is.na(x) & nzchar(x)]
          if (length(x)) {
            ids <- union(ids, unique(x))
            files_with_ids <- files_with_ids + 1L
          }
        }
      }
    }

    if (!length(ids)) {
      stop("No participant IDs could be recovered from any local source file for site ", site)
    }

    tibble(
      site = site,
      participants = length(ids),
      source_files = length(paths),
      files_with_ids = files_with_ids
    )
  })

  out <- bind_rows(rows)
  message(
    "Site sample sizes derived from the union of participant IDs across all local MeLiDos source files"
  )
  print(out, n = nrow(out))
  out
}

melidos_sites_df <- site_reference |>
  left_join(read_site_counts(), by = "site") |>
  mutate(label = paste0(city, " (", site, ")\n", "n = ", participants))

stopifnot(
  nrow(melidos_sites_df) == 9L,
  dplyr::n_distinct(melidos_sites_df$country) == 7L,
  all(is.finite(melidos_sites_df$participants)),
  all(melidos_sites_df$participants > 0)
)

n_total <- sum(melidos_sites_df$participants)
if (n_total != 191L) {
  warning(
    "The union of participant IDs across all locally downloaded MeLiDos source files is ",
    n_total, " rather than the reported full MeLiDos total of 191. ",
    "This usually means the local download does not exactly match the final study cohort. ",
    "The map uses all participants currently represented anywhere in data/raw/melidos."
  )
}

# -----------------------------------------------------------------------------
# Natural Earth administrative basemap
# -----------------------------------------------------------------------------
# Administrative boundaries: Natural Earth Admin-0, 1:50m.
# If automatic download fails, download this file manually:
# https://raw.githubusercontent.com/nvkelso/natural-earth-vector/master/geojson/ne_50m_admin_0_countries.geojson
# and save it as:
# external/ne_50m_admin_0_countries.geojson

MAP_FILE <- file.path("external", "ne_50m_admin_0_countries.geojson")
MAP_URL <- paste0(
  "https://raw.githubusercontent.com/nvkelso/natural-earth-vector/",
  "master/geojson/ne_50m_admin_0_countries.geojson"
)

dir.create(dirname(MAP_FILE), recursive = TRUE, showWarnings = FALSE)

if (!file.exists(MAP_FILE) || file.info(MAP_FILE)$size < 10000) {
  message("Natural Earth basemap not found locally; downloading once to ", MAP_FILE)
  ok <- tryCatch({
    utils::download.file(MAP_URL, MAP_FILE, mode = "wb", quiet = FALSE, method = "libcurl")
    file.exists(MAP_FILE) && file.info(MAP_FILE)$size > 10000
  }, error = function(e) FALSE)

  if (!ok) {
    stop(
      "Could not download the Natural Earth administrative basemap.\n",
      "Please download:\n  ", MAP_URL, "\n",
      "and save it as:\n  ", MAP_FILE, "\n",
      "Then rerun: Rscript scripts/16_appendix_figures.R"
    )
  }
}

.old_s2 <- sf::sf_use_s2()
sf::sf_use_s2(FALSE)
on.exit(sf::sf_use_s2(.old_s2), add = TRUE)

world <- suppressWarnings(sf::st_read(MAP_FILE, quiet = TRUE))
world <- sf::st_transform(world, 4326)
world <- suppressWarnings(sf::st_make_valid(world))
world <- world[!sf::st_is_empty(world), , drop = FALSE]

# Crop before projection so geometries around the antimeridian never enter the
# plotting object. The resulting Atlantic-centred regional frame still retains
# a recognisable world-map context around all study sites.
view_bbox <- sf::st_bbox(
  c(xmin = -95, ymin = -2, xmax = 40, ymax = 65),
  crs = sf::st_crs(4326)
)
world_view <- suppressWarnings(sf::st_crop(world, view_bbox))
world_view <- world_view[!sf::st_is_empty(world_view), , drop = FALSE]

country_field <- intersect(c("ADMIN", "NAME_EN", "NAME", "SOVEREIGNT"), names(world_view))
if (!length(country_field)) stop("Could not identify a country-name field in Natural Earth data")
country_field <- country_field[[1]]

study_country_names <- c(
  "Sweden", "Netherlands", "Germany", "Spain", "Turkey", "Türkiye",
  "Costa Rica", "Ghana"
)
world_view <- world_view |>
  mutate(study_country = .data[[country_field]] %in% study_country_names)

# Robinson projection, centred on the Atlantic, reduces the large empty ocean
# block in the raw lon/lat view and gives the figure a less GIS-like appearance.
robin_crs <- "+proj=robin +lon_0=-20 +datum=WGS84 +units=m +no_defs"
world_robin <- suppressWarnings(sf::st_transform(world_view, robin_crs))

site_sf <- sf::st_as_sf(melidos_sites_df, coords = c("lon", "lat"), crs = 4326)
site_sf <- sf::st_transform(site_sf, robin_crs)
site_xy <- sf::st_coordinates(site_sf)

label_sf <- sf::st_as_sf(melidos_sites_df, coords = c("label_lon", "label_lat"), crs = 4326)
label_sf <- sf::st_transform(label_sf, robin_crs)
label_xy <- sf::st_coordinates(label_sf)

plot_sites <- melidos_sites_df |>
  mutate(
    site_x = site_xy[, 1],
    site_y = site_xy[, 2],
    label_x = label_xy[, 1],
    label_y = label_xy[, 2]
  )

subtitle_text <- paste0(
  "Nine study sites across seven countries · ",
  scales::comma(n_total), " participants"
)

p_sites <- ggplot() +
  geom_sf(
    data = world_robin,
    aes(fill = study_country),
    colour = "#C5CACC", linewidth = .24
  ) +
  scale_fill_manual(
    values = c(`FALSE` = "#F6F6F3", `TRUE` = "#E1EBF1"),
    guide = "none"
  ) +
  geom_segment(
    data = plot_sites,
    aes(x = site_x, y = site_y, xend = label_x, yend = label_y),
    inherit.aes = FALSE,
    colour = "#8F989D", linewidth = .26, lineend = "round"
  ) +
  geom_point(
    data = plot_sites,
    aes(site_x, site_y, size = participants),
    inherit.aes = FALSE,
    shape = 21, fill = MS_PRIMARY, colour = "white", stroke = .52
  ) +
  geom_text(
    data = plot_sites,
    aes(label_x, label_y, label = label, hjust = hjust),
    inherit.aes = FALSE,
    family = MS_FONT, size = 2.72, lineheight = .94, colour = "#202427"
  ) +
  scale_size_area(
    max_size = 7.3,
    breaks = scales::pretty_breaks(n = 4),
    name = "Participants"
  ) +
  coord_sf(expand = FALSE, datum = NA) +
  labs(
    title = "MeLiDos field-study network",
    subtitle = subtitle_text,
    x = NULL, y = NULL,
    caption = paste0(
      "Administrative boundaries: Natural Earth 1:50m. ",
      "Point area represents the union of participants appearing in any locally downloaded MeLiDos source modality."
    )
  ) +
  theme_ms(base_size = 7.8, legend_position = "bottom") +
  theme(
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    plot.title = element_text(size = 10.4, face = "bold", margin = margin(b = 1)),
    plot.subtitle = element_text(size = 7.8, colour = "#5F686D", margin = margin(b = 4.5)),
    plot.caption = element_text(size = 6.3, colour = "#70787C", hjust = 0, margin = margin(t = 3.5)),
    legend.position = "bottom",
    legend.justification = "left",
    legend.box.just = "left",
    legend.title = element_text(size = 6.9),
    legend.text = element_text(size = 6.7),
    legend.key.width = grid::unit(4.6, "mm"),
    legend.margin = margin(t = -2, b = -2),
    plot.margin = margin(5, 6, 4, 6)
  ) +
  guides(size = guide_legend(
    title.position = "left",
    override.aes = list(fill = MS_PRIMARY, colour = "white")
  ))

png_path <- file.path(OUT_DIR, "FigS_MeLiDos_sites.png")

ggsave(
  png_path, p_sites,
  width = 10.4, height = 5.9,
  dpi = MS_RASTER_DPI, bg = "white"
)

message("Supplementary map written:")
message("  ", png_path)
