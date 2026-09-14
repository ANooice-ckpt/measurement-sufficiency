#!/usr/bin/env Rscript

# Supplementary / appendix figures.
# Run from the repository root:
#   Rscript scripts/16_appendix_figures.R
# Outputs are written directly to results/figures/.

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

site_reference <- tibble::tribble(
  ~site,      ~city,       ~country,        ~lat,      ~lon,      ~label_lon, ~label_lat, ~hjust,
  "RISE",     "Borås",     "Sweden",         57.71567,   12.89087,   20.0,       60.0,       0.0,
  "THUAS",    "Delft",     "Netherlands",    52.01160,    4.35710,  -12.0,       55.7,       0.0,
  "BAUA",     "Dortmund",  "Germany",        51.49820,    7.41671,   17.0,       54.5,       0.0,
  "MPI",      "Tübingen",  "Germany",        48.52160,    9.05760,  -10.0,       46.0,       0.0,
  "TUM",      "Munich",    "Germany",        48.13330,   11.56670,   18.5,       46.7,       0.0,
  "FUSPCEU",  "Madrid",    "Spain",          40.41650,   -3.70256,  -17.0,       37.8,       0.0,
  "IZTECH",   "Izmir",     "Türkiye",        38.32000,   26.63000,   18.0,       33.8,       0.0,
  "UCR",      "San José",  "Costa Rica",      9.93720,  -84.05090,  -73.0,       12.8,       0.0,
  "KNUST",    "Kumasi",    "Ghana",           6.67501,   -1.57264,    7.0,        8.8,       0.0
)

# Use the actual local MeLiDos files rather than hard-coding site sample sizes.
# First reuse the inventory if it exists; otherwise derive counts from light_glasses.
read_site_counts <- function() {
  inventory_path <- file.path("logs", "data_inventory.csv")

  if (file.exists(inventory_path)) {
    inv <- utils::read.csv(inventory_path, stringsAsFactors = FALSE, check.names = FALSE)
    required <- c("site", "modality", "n_participants")
    if (all(required %in% names(inv))) {
      out <- inv |>
        filter(modality == "light_glasses") |>
        group_by(site) |>
        summarise(participants = max(n_participants, na.rm = TRUE), .groups = "drop")
      if (nrow(out) == length(melidos_sites()) && all(melidos_sites() %in% out$site)) {
        message("Site sample sizes read from logs/data_inventory.csv")
        return(out)
      }
    }
  }

  message("Deriving site sample sizes from local light_glasses files")
  rows <- lapply(melidos_sites(), function(site) {
    path <- raw_data_path(site, "light_glasses")
    if (!file.exists(path)) {
      stop(
        "Missing ", path,
        ". Run scripts/01_download_melidos.R (and optionally scripts/02_inventory.R) first."
      )
    }
    x <- load_raw_file(path, "light_glasses")
    if (!"Id" %in% names(x)) stop("Missing Id column in ", path)
    tibble(site = site, participants = dplyr::n_distinct(x$Id[!is.na(x$Id)]))
  })
  bind_rows(rows)
}

melidos_sites_df <- site_reference |>
  left_join(read_site_counts(), by = "site") |>
  mutate(
    label = paste0(site, " · ", city, "\n", "n = ", participants)
  )

stopifnot(
  nrow(melidos_sites_df) == 9L,
  dplyr::n_distinct(melidos_sites_df$country) == 7L,
  all(is.finite(melidos_sites_df$participants)),
  all(melidos_sites_df$participants > 0)
)

n_total <- sum(melidos_sites_df$participants)
if (n_total != 191L) {
  warning(
    "Local light_glasses files contain ", n_total,
    " unique participants rather than the reported MeLiDos total of 191. ",
    "The figure uses the local project data."
  )
}

# -----------------------------------------------------------------------------
# Natural Earth basemap
# -----------------------------------------------------------------------------
# No extra mapping package is required. We try the official Natural Earth S3
# archive first, then the GitHub GeoJSON mirror. A missing basemap is treated as
# an error rather than silently producing a blank map.

download_binary <- function(url, destination) {
  if (file.exists(destination)) unlink(destination)

  ok <- tryCatch({
    suppressWarnings(utils::download.file(
      url, destination, mode = "wb", quiet = TRUE, method = "libcurl"
    ))
    file.exists(destination) && is.finite(file.info(destination)$size) &&
      file.info(destination)$size > 1000
  }, error = function(e) FALSE)

  if (ok) return(TRUE)

  curl_bin <- Sys.which("curl")
  if (nzchar(curl_bin)) {
    status <- tryCatch(
      suppressWarnings(system2(
        curl_bin,
        c("-L", "--fail", "--silent", "--show-error", "-o",
          shQuote(normalizePath(destination, winslash = "/", mustWork = FALSE)),
          shQuote(url)),
        stdout = FALSE, stderr = FALSE
      )),
      error = function(e) 1L
    )
    if (identical(status, 0L) && file.exists(destination) && file.info(destination)$size > 1000) {
      return(TRUE)
    }
  }

  FALSE
}

load_world_map <- function() {
  td <- tempfile("natural_earth_")
  dir.create(td, recursive = TRUE)
  on.exit(unlink(td, recursive = TRUE, force = TRUE), add = TRUE)

  zip_path <- file.path(td, "ne_110m_admin_0_countries.zip")
  zip_url <- "https://naturalearth.s3.amazonaws.com/110m_cultural/ne_110m_admin_0_countries.zip"

  if (download_binary(zip_url, zip_path)) {
    unzip(zip_path, exdir = td)
    shp <- list.files(td, pattern = "[.]shp$", full.names = TRUE)
    if (length(shp)) {
      world <- tryCatch(suppressWarnings(sf::st_read(shp[[1]], quiet = TRUE)), error = function(e) NULL)
      if (!is.null(world) && nrow(world) > 0) return(world)
    }
  }

  geojson_path <- file.path(td, "ne_110m_admin_0_countries.geojson")
  geojson_url <- paste0(
    "https://raw.githubusercontent.com/nvkelso/natural-earth-vector/",
    "master/geojson/ne_110m_admin_0_countries.geojson"
  )

  if (download_binary(geojson_url, geojson_path)) {
    world <- tryCatch(
      suppressWarnings(sf::st_read(geojson_path, quiet = TRUE)),
      error = function(e) NULL
    )
    if (!is.null(world) && nrow(world) > 0) return(world)
  }

  stop(
    "Natural Earth basemap could not be downloaded from either source. ",
    "Check network access to naturalearth.s3.amazonaws.com or raw.githubusercontent.com."
  )
}

world <- load_world_map()
world <- sf::st_transform(world, 4326)

country_field <- intersect(c("ADMIN", "NAME_EN", "NAME", "SOVEREIGNT"), names(world))
if (!length(country_field)) stop("Could not identify a country-name field in Natural Earth data")
country_field <- country_field[[1]]

study_country_names <- c(
  "Sweden", "Netherlands", "Germany", "Spain", "Turkey", "Türkiye",
  "Costa Rica", "Ghana"
)

world <- world |>
  mutate(study_country = .data[[country_field]] %in% study_country_names)

# Restrict the geographic frame to the MeLiDos study region while retaining a
# recognisable world-map context spanning Central America, Europe and West Africa.
world_view <- suppressWarnings(sf::st_crop(
  world,
  xmin = -100, xmax = 40, ymin = -5, ymax = 65
))

summary_label <- paste0(
  "9 sites   ·   7 countries   ·   ",
  scales::comma(n_total), " participants"
)

p_sites <- ggplot() +
  geom_sf(
    data = world_view,
    aes(fill = study_country),
    colour = "#CED3D6", linewidth = .22
  ) +
  scale_fill_manual(
    values = c(`FALSE` = "#F4F4F1", `TRUE` = "#DCE8EF"),
    guide = "none"
  ) +
  geom_segment(
    data = melidos_sites_df,
    aes(x = lon, y = lat, xend = label_lon, yend = label_lat),
    inherit.aes = FALSE,
    colour = "#8D969B", linewidth = .30, lineend = "round"
  ) +
  geom_point(
    data = melidos_sites_df,
    aes(lon, lat, size = participants),
    inherit.aes = FALSE,
    shape = 21, fill = MS_PRIMARY, colour = "white", stroke = .58
  ) +
  geom_text(
    data = melidos_sites_df,
    aes(label_lon, label_lat, label = label, hjust = hjust),
    inherit.aes = FALSE,
    family = MS_FONT, size = 3.05, lineheight = .92, colour = "#202427"
  ) +
  annotate(
    "label", x = -97, y = 63.0,
    label = summary_label,
    hjust = 0, vjust = 1,
    family = MS_FONT, size = 3.30,
    fill = scales::alpha("white", .94), colour = "#202427",
    label.size = 0, label.padding = grid::unit(2.0, "mm")
  ) +
  scale_size_area(
    max_size = 8.4,
    breaks = scales::pretty_breaks(n = 4),
    name = "Participants"
  ) +
  coord_sf(
    xlim = c(-100, 40), ylim = c(-5, 65),
    expand = FALSE,
    default_crs = sf::st_crs(4326), datum = sf::st_crs(4326)
  ) +
  labs(
    title = "MeLiDos field-study network",
    subtitle = "Geographic coverage and site sample size",
    x = NULL, y = NULL,
    caption = "Point area represents the number of participants in the local MeLiDos light_glasses data."
  ) +
  theme_ms(base_size = 8.0, legend_position = "bottom") +
  theme(
    panel.border = element_blank(),
    panel.grid.major = element_line(colour = "#E5E8E9", linewidth = .22),
    panel.grid.minor = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    plot.title = element_text(size = 11.0, face = "bold", margin = margin(b = 2)),
    plot.subtitle = element_text(size = 8.0, colour = "#5E6569", margin = margin(b = 5)),
    plot.caption = element_text(size = 6.6, colour = "#6B7276", hjust = 0, margin = margin(t = 4)),
    legend.title = element_text(size = 7.0),
    legend.text = element_text(size = 6.8),
    legend.key.width = grid::unit(5, "mm"),
    legend.margin = margin(t = -2, b = 0),
    plot.margin = margin(5, 6, 4, 6)
  ) +
  guides(size = guide_legend(
    title.position = "left",
    override.aes = list(fill = MS_PRIMARY, colour = "white")
  ))

png_path <- file.path(OUT_DIR, "FigS_MeLiDos_sites.png")
pdf_path <- file.path(OUT_DIR, "FigS_MeLiDos_sites.pdf")

ggsave(png_path, p_sites, width = 10.6, height = 6.1, dpi = MS_RASTER_DPI, bg = "white")
ggsave(pdf_path, p_sites, width = 10.6, height = 6.1, bg = "white")

message("Supplementary map written:")
message("  ", png_path)
message("  ", pdf_path)
