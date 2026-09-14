#!/usr/bin/env Rscript

# Supplementary / appendix figures.
# Run from the repository root:
#   Rscript scripts/16_appendix_figures.R
# Outputs are written directly to figures/.

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

OUT_DIR <- "figures"
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

# -----------------------------------------------------------------------------
# Fig. S — MeLiDos field-study sites and sample distribution
# -----------------------------------------------------------------------------
# Site coordinates follow melidosData. Participant and participant-day totals
# follow the current MeLiDos multi-country descriptive table (191 participants,
# 1,480 participant-days across nine sites).

melidos_sites <- tibble::tribble(
  ~site,      ~city,       ~country,        ~lat,      ~lon,      ~participants, ~participant_days, ~label_lon, ~label_lat, ~hjust,
  "RISE",     "Borås",     "Sweden",         57.71567,   12.89087, 17,            137,               21.0,       60.0,       0.0,
  "THUAS",    "Delft",     "Netherlands",    52.01160,    4.35710, 20,            125,               -8.0,       55.1,       0.0,
  "BAUA",     "Dortmund",  "Germany",        51.49820,    7.41671, 24,            176,               16.0,       54.0,       0.0,
  "MPI",      "Tübingen",  "Germany",        48.52160,    9.05760, 26,            208,               -8.0,       46.8,       0.0,
  "TUM",      "Munich",    "Germany",        48.13330,   11.56670, 10,             80,               18.0,       47.3,       0.0,
  "FUSPCEU",  "Madrid",    "Spain",          40.41650,   -3.70256, 23,            182,              -15.0,       38.0,       0.0,
  "IZTECH",   "Izmir",     "Türkiye",        38.32000,   26.63000, 17,            140,               17.5,       33.8,       0.0,
  "UCR",      "San José",  "Costa Rica",      9.93720,  -84.05090, 39,            312,              -74.0,       13.0,       0.0,
  "KNUST",    "Kumasi",    "Ghana",           6.67501,   -1.57264, 15,            120,                7.0,        8.7,       0.0
) |>
  mutate(
    label = paste0(site, " · ", city, "\n",
                   "n = ", participants, " · ", participant_days, " participant-days")
  )

stopifnot(
  nrow(melidos_sites) == 9L,
  dplyr::n_distinct(melidos_sites$country) == 7L,
  sum(melidos_sites$participants) == 191L,
  sum(melidos_sites$participant_days) == 1480L
)

load_world_map <- function() {
  # Avoid adding mapping packages to the project environment. The existing sf
  # dependency reads the public Natural Earth 110 m GeoJSON into a temporary file.
  url <- paste0(
    "https://raw.githubusercontent.com/nvkelso/natural-earth-vector/",
    "master/geojson/ne_110m_admin_0_countries.geojson"
  )
  tmp <- tempfile(fileext = ".geojson")
  on.exit(unlink(tmp), add = TRUE)

  ok <- tryCatch({
    suppressWarnings(utils::download.file(url, tmp, mode = "wb", quiet = TRUE))
    file.exists(tmp) && file.info(tmp)$size > 1000
  }, error = function(e) FALSE)

  if (!ok) {
    warning("Natural Earth basemap could not be downloaded; drawing the site network without land polygons.")
    return(NULL)
  }

  suppressWarnings(sf::st_read(tmp, quiet = TRUE))
}

world <- load_world_map()

if (!is.null(world)) {
  country_field <- intersect(c("ADMIN", "NAME_EN", "NAME", "SOVEREIGNT"), names(world))[[1]]
  study_names <- c("Sweden", "Netherlands", "Germany", "Spain", "Turkey", "Türkiye", "Costa Rica", "Ghana")
  world <- world |>
    mutate(study_country = .data[[country_field]] %in% study_names)
}

map_base <- ggplot()
if (!is.null(world)) {
  map_base <- map_base +
    geom_sf(
      data = world,
      aes(fill = study_country),
      colour = "#D5D9DB", linewidth = .18
    ) +
    scale_fill_manual(
      values = c(`FALSE` = "#F5F5F2", `TRUE` = "#DCE8EF"),
      guide = "none"
    )
}

p_sites <- map_base +
  geom_segment(
    data = melidos_sites,
    aes(x = lon, y = lat, xend = label_lon, yend = label_lat),
    colour = "#9AA1A5", linewidth = .28, lineend = "round"
  ) +
  geom_point(
    data = melidos_sites,
    aes(lon, lat, size = participants),
    shape = 21, fill = MS_PRIMARY, colour = "white", stroke = .55
  ) +
  geom_text(
    data = melidos_sites,
    aes(label_lon, label_lat, label = label, hjust = hjust),
    family = MS_FONT, size = 3.05, lineheight = .92, colour = "#232629"
  ) +
  annotate(
    "label", x = -91, y = 61.0,
    label = "9 sites   ·   7 countries   ·   191 participants   ·   1,480 participant-days",
    hjust = 0, vjust = 1, family = MS_FONT, size = 3.25,
    fill = scales::alpha("white", .94), colour = "#232629",
    label.size = 0, label.padding = grid::unit(2.0, "mm")
  ) +
  scale_size_area(
    max_size = 8.2,
    limits = c(10, 40),
    breaks = c(10, 20, 30, 40),
    name = "Participants"
  ) +
  coord_sf(
    xlim = c(-94, 34), ylim = c(0, 64),
    expand = FALSE, default_crs = sf::st_crs(4326), datum = sf::st_crs(4326)
  ) +
  labs(
    title = "MeLiDos field-study network",
    subtitle = "Site sample size and geographic coverage",
    x = NULL, y = NULL,
    caption = "Point area represents participants; labels also report participant-days."
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

ggsave(png_path, p_sites, width = 10.4, height = 6.0, dpi = MS_RASTER_DPI, bg = "white")
ggsave(pdf_path, p_sites, width = 10.4, height = 6.0, bg = "white")

message("Supplementary map written:")
message("  ", png_path)
message("  ", pdf_path)
