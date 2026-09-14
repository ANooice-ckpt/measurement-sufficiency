#!/usr/bin/env Rscript

# Supplementary / appendix figures.
# Run from the repository root:
#   Rscript scripts/16_appendix_figures.R
# Outputs:
#   results/figures/FigS_MeLiDos_sites.png
#   results/figures/FigS_measurement_configuration.png
#   results/figures/FigS_representation_to_sufficiency.png

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

# =============================================================================
# Fig. S — MeLiDos field-study sites and sample distribution
# =============================================================================

site_reference <- tibble::tribble(
  ~site,      ~city,       ~country,        ~lat,      ~lon,      ~label_lon, ~label_lat, ~hjust,
  "RISE",     "Borås",     "Sweden",         57.71567,   12.89087,   22.0,       60.2,       0.0,
  "THUAS",    "Delft",     "Netherlands",    52.01160,    4.35710,  -11.5,       55.0,       0.0,
  "BAUA",     "Dortmund",  "Germany",        51.49820,    7.41671,   24.0,       54.0,       0.0,
  "MPI",      "Tübingen",  "Germany",        48.52160,    9.05760,  -11.5,       46.7,       0.0,
  "TUM",      "Munich",    "Germany",        48.13330,   11.56670,   24.5,       46.2,       0.0,
  "FUSPCEU",  "Madrid",    "Spain",          40.41650,   -3.70256,  -16.5,       38.0,       0.0,
  "IZTECH",   "Izmir",     "Türkiye",        38.32000,   26.63000,   36.0,       34.2,       1.0,
  "UCR",      "San José",  "Costa Rica",      9.93720,  -84.05090,  -69.5,       14.0,       0.0,
  "KNUST",    "Kumasi",    "Ghana",           6.67501,   -1.57264,   10.0,        9.8,       0.0
)

read_site_counts <- function() {
  raw_dir <- file.path("data", "raw", "melidos")
  if (!dir.exists(raw_dir)) {
    stop("Missing ", raw_dir, ". Run scripts/01_download_melidos.R first.")
  }

  rows <- lapply(melidos_sites(), function(site) {
    paths <- list.files(raw_dir, pattern = paste0("^", site, "__.*[.]RData$"), full.names = TRUE)
    if (!length(paths)) stop("No local MeLiDos source files found for site ", site)

    ids <- character()
    files_with_ids <- 0L
    for (path in paths) {
      env <- new.env(parent = emptyenv())
      ok <- tryCatch({ load(path, envir = env); TRUE }, error = function(e) FALSE)
      if (!ok) next
      for (object_name in ls(env, all.names = TRUE)) {
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
    if (!length(ids)) stop("No participant IDs recovered for site ", site)
    tibble(site = site, participants = length(ids), source_files = length(paths), files_with_ids = files_with_ids)
  })

  out <- bind_rows(rows)
  message("Site sample sizes derived from participant-ID unions across all local MeLiDos source files")
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
    "The union of participant IDs across all local MeLiDos source files is ", n_total,
    " rather than the reported full MeLiDos total of 191. The map uses the local source-file union."
  )
}

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
      "Download:\n  ", MAP_URL, "\nSave as:\n  ", MAP_FILE,
      "\nThen rerun scripts/16_appendix_figures.R"
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

view_bbox <- sf::st_bbox(c(xmin = -95, ymin = -2, xmax = 45, ymax = 65), crs = sf::st_crs(4326))
world_view <- suppressWarnings(sf::st_crop(world, view_bbox))
world_view <- world_view[!sf::st_is_empty(world_view), , drop = FALSE]

country_field <- intersect(c("ADMIN", "NAME_EN", "NAME", "SOVEREIGNT"), names(world_view))
if (!length(country_field)) stop("Could not identify a country-name field in Natural Earth data")
country_field <- country_field[[1]]
study_country_names <- c("Sweden", "Netherlands", "Germany", "Spain", "Turkey", "Türkiye", "Costa Rica", "Ghana")
world_view <- world_view |>
  mutate(study_country = .data[[country_field]] %in% study_country_names)

map_crs <- "+proj=eqc +lat_ts=30 +lat_0=0 +lon_0=-20 +datum=WGS84 +units=m +no_defs"
world_map <- suppressWarnings(sf::st_transform(world_view, map_crs))

site_sf <- sf::st_transform(sf::st_as_sf(melidos_sites_df, coords = c("lon", "lat"), crs = 4326), map_crs)
label_sf <- sf::st_transform(sf::st_as_sf(melidos_sites_df, coords = c("label_lon", "label_lat"), crs = 4326), map_crs)
site_xy <- sf::st_coordinates(site_sf)
label_xy <- sf::st_coordinates(label_sf)

plot_sites <- melidos_sites_df |>
  mutate(
    site_x = site_xy[, 1], site_y = site_xy[, 2],
    label_x = label_xy[, 1], label_y = label_xy[, 2]
  )

subtitle_text <- paste0("Nine study sites across seven countries · ", scales::comma(n_total), " participants")

p_sites <- ggplot() +
  geom_sf(data = world_map, aes(fill = study_country), colour = "#C5CACC", linewidth = .24) +
  scale_fill_manual(values = c(`FALSE` = "#F6F6F3", `TRUE` = "#E1EBF1"), guide = "none") +
  geom_segment(
    data = plot_sites,
    aes(x = site_x, y = site_y, xend = label_x, yend = label_y),
    inherit.aes = FALSE, colour = "#8F989D", linewidth = .26, lineend = "round"
  ) +
  geom_point(
    data = plot_sites, aes(site_x, site_y, size = participants), inherit.aes = FALSE,
    shape = 21, fill = MS_PRIMARY, colour = "white", stroke = .52
  ) +
  geom_text(
    data = plot_sites, aes(label_x, label_y, label = label, hjust = hjust), inherit.aes = FALSE,
    family = MS_FONT, size = 2.72, lineheight = .94, colour = "#202427"
  ) +
  scale_size_area(max_size = 7.3, breaks = scales::pretty_breaks(n = 4), name = "Participants") +
  coord_sf(expand = FALSE, datum = NA, crs = sf::st_crs(map_crs)) +
  labs(
    title = "MeLiDos field-study network", subtitle = subtitle_text, x = NULL, y = NULL,
    caption = paste0(
      "Administrative boundaries: Natural Earth 1:50m. ",
      "Point area represents the union of participants appearing in any locally downloaded MeLiDos source modality."
    )
  ) +
  theme_ms(base_size = 7.8, legend_position = "bottom") +
  theme(
    panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
    axis.text = element_blank(), axis.ticks = element_blank(),
    plot.title = element_text(size = 10.4, face = "bold", margin = margin(b = 1)),
    plot.subtitle = element_text(size = 7.8, colour = "#5F686D", margin = margin(b = 4.5)),
    plot.caption = element_text(size = 6.3, colour = "#70787C", hjust = 0, margin = margin(t = 3.5)),
    legend.position = "bottom", legend.justification = "left", legend.box.just = "left",
    legend.title = element_text(size = 6.9), legend.text = element_text(size = 6.7),
    legend.key.width = grid::unit(4.6, "mm"), legend.margin = margin(t = -2, b = -2),
    plot.margin = margin(5, 6, 4, 6)
  ) +
  guides(size = guide_legend(title.position = "left", override.aes = list(fill = MS_PRIMARY, colour = "white")))

png_path <- file.path(OUT_DIR, "FigS_MeLiDos_sites.png")
ggsave(png_path, p_sites, width = 10.4, height = 5.9, dpi = MS_RASTER_DPI, bg = "white")
message("Supplementary map written:\n  ", png_path)

# =============================================================================
# Fig. S — Measurement configuration space
# =============================================================================

panel_bg <- tibble::tribble(
  ~xmin, ~xmax, ~ymin, ~ymax,
   3,     49,    53,    94,
  51,     97,    53,    94,
   3,     49,     5,    49,
  51,     97,     5,    49
)

# Placement: repeated upper-body icons; the marker is placed at the actual
# eye/chest/wrist location rather than adjacent to a generic stick figure.
placement_people <- tibble(
  x = c(13, 26, 39),
  label = c("Eye", "Chest", "Wrist"),
  sensor_x = c(13.55, 26.00, 42.30),
  sensor_y = c(81.20, 74.70, 70.25)
)

human_segments <- dplyr::bind_rows(lapply(placement_people$x, function(x0) {
  tibble::tribble(
    ~x, ~y, ~xend, ~yend,
    x0, 77.8, x0, 69.8,
    x0 - 2.5, 76.0, x0 + 2.5, 76.0,
    x0 - 2.5, 76.0, x0 - 3.3, 70.2,
    x0 + 2.5, 76.0, x0 + 3.3, 70.2,
    x0, 69.8, x0 - 1.8, 65.8,
    x0, 69.8, x0 + 1.8, 65.8
  )
}))
human_heads <- placement_people |> transmute(x = x, y = 81.0)

# Optical representation: one source optical record branches to the two retained
# representations. The small bar motif is deliberately schematic, not a claimed spectrum.
optical_cards <- tibble::tribble(
  ~xmin, ~xmax, ~ymin, ~ymax, ~label, ~fill,
  68.0, 79.0, 68.5, 79.0, "MEDI",  MS_PRIMARY,
  84.0, 95.0, 68.5, 79.0, "LIGHT", MS_SECONDARY
)
optical_bars <- tibble(
  xmin = c(55.0, 56.4, 57.8, 59.2, 60.6),
  xmax = c(55.8, 57.2, 58.6, 60.0, 61.4),
  ymin = 70.1,
  ymax = c(73.2, 76.4, 78.3, 75.0, 72.1)
)

# Temporal resolution: exact points retained from the same 10-s source grid.
temporal_steps <- c(10, 20, 30, 40, 60, 120)
temporal_rows <- tibble(step = temporal_steps, y = seq(39.0, 18.5, length.out = 6))
temporal_points <- bind_rows(lapply(seq_len(nrow(temporal_rows)), function(i) {
  step <- temporal_rows$step[[i]]
  times <- seq(0, 120, by = step)
  tibble(step = step, y = temporal_rows$y[[i]], x = 15.0 + (times / 120) * 29.0)
}))

# Monitoring duration: nested examples from a common start; all contiguous windows
# are evaluated in the actual analysis.
duration_rows <- tibble(duration = 1:6, y = seq(39.0, 18.5, length.out = 6))
duration_boxes <- tidyr::expand_grid(duration = 1:6, day = 1:6) |>
  left_join(duration_rows, by = "duration") |>
  mutate(
    xmin = 63.0 + (day - 1) * 4.55,
    xmax = xmin + 3.75,
    ymin = y - 1.25,
    ymax = y + 1.25,
    active = day <= duration
  )

p_config <- ggplot() +
  geom_rect(
    data = panel_bg, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
    inherit.aes = FALSE, fill = "#FAFAF8", colour = "#E0E3E4", linewidth = .32
  ) +

  # a Placement
  annotate("text", x = 5.1, y = 90.8, label = "a  Placement",
           hjust = 0, family = MS_FONT, fontface = "bold", size = 4.0, colour = "#202427") +
  geom_segment(
    data = human_segments, aes(x = x, y = y, xend = xend, yend = yend), inherit.aes = FALSE,
    colour = "#747C80", linewidth = .72, lineend = "round"
  ) +
  geom_point(
    data = human_heads, aes(x, y), inherit.aes = FALSE,
    shape = 21, size = 6.0, stroke = .68, fill = "white", colour = "#747C80"
  ) +
  geom_point(
    data = placement_people, aes(sensor_x, sensor_y), inherit.aes = FALSE,
    shape = 21, size = 3.0, stroke = .5, fill = MS_PRIMARY, colour = "white"
  ) +
  geom_text(
    data = placement_people, aes(x, 61.4, label = label), inherit.aes = FALSE,
    family = MS_FONT, size = 3.15, colour = "#30363A"
  ) +
  annotate("text", x = 26, y = 56.2, label = "Three alternative wearing positions",
           family = MS_FONT, size = 2.55, colour = "#727A7E") +

  # b Optical representation
  annotate("text", x = 53.1, y = 90.8, label = "b  Optical representation",
           hjust = 0, family = MS_FONT, fontface = "bold", size = 4.0, colour = "#202427") +
  annotate("rect", xmin = 53.5, xmax = 63.0, ymin = 67.8, ymax = 81.0,
           fill = "white", colour = "#C9CED1", linewidth = .45) +
  geom_rect(
    data = optical_bars, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
    inherit.aes = FALSE, fill = "#7E878C", colour = NA
  ) +
  annotate("text", x = 58.25, y = 65.2, label = "Source optical record",
           family = MS_FONT, size = 2.6, colour = "#5F686D") +
  annotate("segment", x = 63.0, y = 74.4, xend = 65.5, yend = 74.4,
           colour = "#929A9E", linewidth = .45) +
  annotate("segment", x = 65.5, y = 74.4, xend = 65.5, yend = 76.4,
           colour = "#929A9E", linewidth = .45) +
  annotate("segment", x = 65.5, y = 76.4, xend = 67.3, yend = 76.4,
           colour = "#929A9E", linewidth = .45,
           arrow = grid::arrow(length = grid::unit(1.5, "mm"), type = "closed")) +
  annotate("segment", x = 65.5, y = 74.4, xend = 65.5, yend = 71.3,
           colour = "#929A9E", linewidth = .45) +
  annotate("segment", x = 65.5, y = 71.3, xend = 83.3, yend = 71.3,
           colour = "#929A9E", linewidth = .45,
           arrow = grid::arrow(length = grid::unit(1.5, "mm"), type = "closed")) +
  geom_rect(
    data = optical_cards,
    aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = fill),
    inherit.aes = FALSE, colour = "white", linewidth = .65
  ) +
  scale_fill_identity() +
  geom_text(
    data = optical_cards,
    aes(x = (xmin + xmax) / 2, y = (ymin + ymax) / 2, label = label),
    inherit.aes = FALSE, family = MS_FONT, fontface = "bold", size = 3.8, colour = "white"
  ) +
  annotate("text", x = 80, y = 62.2, label = "Two retained representations of the same source record",
           family = MS_FONT, size = 2.55, colour = "#5F686D") +

  # c Temporal resolution
  annotate("text", x = 5.1, y = 45.5, label = "c  Temporal resolution",
           hjust = 0, family = MS_FONT, fontface = "bold", size = 4.0, colour = "#202427") +
  geom_segment(
    data = temporal_rows, aes(x = 15.0, xend = 44.0, y = y, yend = y), inherit.aes = FALSE,
    colour = "#D5D9DB", linewidth = .45
  ) +
  geom_point(
    data = temporal_points, aes(x, y), inherit.aes = FALSE,
    shape = 21, size = 2.15, stroke = .35, fill = MS_PRIMARY, colour = "white"
  ) +
  geom_text(
    data = temporal_rows, aes(x = 12.0, y = y, label = paste0(step, " s")), inherit.aes = FALSE,
    hjust = 1, family = MS_FONT, size = 2.65, colour = "#30363A"
  ) +
  annotate("text", x = 29.5, y = 11.3, label = "Exact source rows retained · no averaging",
           family = MS_FONT, size = 2.55, colour = "#727A7E") +

  # d Monitoring duration
  annotate("text", x = 53.1, y = 45.5, label = "d  Monitoring duration",
           hjust = 0, family = MS_FONT, fontface = "bold", size = 4.0, colour = "#202427") +
  geom_rect(
    data = duration_boxes, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
    inherit.aes = FALSE, fill = "white", colour = "#C9CED1", linewidth = .38
  ) +
  geom_rect(
    data = duration_boxes |> filter(active), aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
    inherit.aes = FALSE, fill = MS_PRIMARY, colour = "white", linewidth = .35
  ) +
  geom_text(
    data = duration_rows, aes(x = 59.5, y = y, label = paste0(duration, " d")), inherit.aes = FALSE,
    hjust = 1, family = MS_FONT, size = 2.65, colour = "#30363A"
  ) +
  annotate("text", x = 76.7, y = 11.3, label = "Complete consecutive days · all contiguous windows",
           family = MS_FONT, size = 2.48, colour = "#727A7E") +

  coord_cartesian(xlim = c(0, 100), ylim = c(0, 100), expand = FALSE, clip = "off") +
  labs(
    title = "Measurement configuration space",
    subtitle = "Each evaluated configuration combines one state from four independent facets",
    caption = "High-information anchors are eye placement, MEDI and 10-s sampling; monitoring duration is evaluated over 1–6 complete consecutive days."
  ) +
  theme_void(base_family = MS_FONT) +
  theme(
    plot.title = element_text(size = 11.0, face = "bold", colour = "#202427", margin = margin(b = 1.5)),
    plot.subtitle = element_text(size = 8.0, colour = "#5F686D", margin = margin(b = 6)),
    plot.caption = element_text(size = 6.4, colour = "#70787C", hjust = 0, margin = margin(t = 4)),
    plot.margin = margin(6, 7, 5, 7)
  )

config_path <- file.path(OUT_DIR, "FigS_measurement_configuration.png")
ggsave(config_path, p_config, width = 10.4, height = 6.3, dpi = MS_RASTER_DPI, bg = "white")
message("Measurement configuration schematic written:\n  ", config_path)

# =============================================================================
# Fig. S — From representation change to observed sufficiency
# =============================================================================
# Conceptual only: no empirical values are plotted. The final panel uses temporal
# resolution as an ordered-axis example and retains the actual RQ3 rule that the
# highest observed requirement is boundary-unresolved rather than automatically
# sufficient.

concept_panels <- tibble::tribble(
  ~xmin, ~xmax, ~ymin, ~ymax,
   3,     34,    12,    88,
  35.5,   65.5,  12,    88,
  67,     97,    12,    88
)

paired_example <- tibble(
  id = 1:7,
  y_alt = c(35, 46, 52, 61, 68, 73, 80),
  y_ref = c(39, 43, 57, 59, 72, 77, 83)
)

summary_cards <- tibble::tribble(
  ~xmin, ~xmax, ~ymin, ~ymax, ~title, ~subtitle,
  39.0, 62.0, 61.0, 78.0, "Magnitude", "A · how much values move",
  39.0, 62.0, 40.0, 57.0, "Direction", "B/A · whether shifts align",
  39.0, 62.0, 19.0, 36.0, "Rank loss", "1 − ρ · whether ordering changes"
)

sufficiency_example <- tibble(
  state = factor(c("120 s", "60 s", "40 s", "30 s", "20 s", "10 s"),
                 levels = c("120 s", "60 s", "40 s", "30 s", "20 s", "10 s")),
  x = 1:6,
  residual = c(.43, .33, .22, .24, .12, NA_real_),
  status = c("above", "above", "sufficient", "sufficient", "sufficient", "unresolved")
)

p_concept <- ggplot() +
  geom_rect(
    data = concept_panels, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
    inherit.aes = FALSE, fill = "#FAFAF8", colour = "#E0E3E4", linewidth = .32
  ) +
  annotate("segment", x = 33.0, y = 50, xend = 36.0, yend = 50,
           colour = "#9BA2A6", linewidth = .5,
           arrow = grid::arrow(length = grid::unit(1.7, "mm"), type = "closed")) +
  annotate("segment", x = 64.5, y = 50, xend = 67.5, yend = 50,
           colour = "#9BA2A6", linewidth = .5,
           arrow = grid::arrow(length = grid::unit(1.7, "mm"), type = "closed")) +

  # a Paired representations
  annotate("text", x = 5.0, y = 83.5, label = "a  Compare paired representations",
           hjust = 0, family = MS_FONT, fontface = "bold", size = 3.8, colour = "#202427") +
  annotate("rect", xmin = 7.0, xmax = 17.0, ymin = 66.0, ymax = 77.0,
           fill = "#F1F3F4", colour = "#C9CED1", linewidth = .4) +
  annotate("rect", xmin = 20.0, xmax = 30.0, ymin = 66.0, ymax = 77.0,
           fill = "#E3ECF2", colour = "#BFCAD0", linewidth = .4) +
  annotate("text", x = 12.0, y = 73.8, label = "Alternative", family = MS_FONT,
           fontface = "bold", size = 2.8, colour = "#30363A") +
  annotate("text", x = 12.0, y = 69.2, label = "e.g. wrist · LIGHT · 60 s",
           family = MS_FONT, size = 2.25, colour = "#687176") +
  annotate("text", x = 25.0, y = 73.8, label = "Anchor", family = MS_FONT,
           fontface = "bold", size = 2.8, colour = "#30363A") +
  annotate("text", x = 25.0, y = 69.2, label = "eye · MEDI · 10 s",
           family = MS_FONT, size = 2.25, colour = "#687176") +
  geom_segment(
    data = paired_example,
    aes(x = 13.2, y = y_alt - 25, xend = 23.8, yend = y_ref - 25),
    inherit.aes = FALSE, colour = "#B4BABD", linewidth = .45
  ) +
  geom_point(
    data = paired_example, aes(x = 13.2, y = y_alt - 25), inherit.aes = FALSE,
    size = 2.4, shape = 21, fill = MS_SECONDARY, colour = "white", stroke = .35
  ) +
  geom_point(
    data = paired_example, aes(x = 23.8, y = y_ref - 25), inherit.aes = FALSE,
    size = 2.4, shape = 21, fill = MS_PRIMARY, colour = "white", stroke = .35
  ) +
  annotate("text", x = 18.5, y = 18.5,
           label = "Each line is the same participant / support unit",
           family = MS_FONT, size = 2.35, colour = "#727A7E") +

  # b Summaries
  annotate("text", x = 37.5, y = 83.5, label = "b  Summarize representation change",
           hjust = 0, family = MS_FONT, fontface = "bold", size = 3.8, colour = "#202427") +
  geom_rect(
    data = summary_cards, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
    inherit.aes = FALSE, fill = "white", colour = "#D4D8DA", linewidth = .4
  ) +
  geom_text(
    data = summary_cards, aes(x = xmin + 1.8, y = ymax - 4.5, label = title),
    inherit.aes = FALSE, hjust = 0, family = MS_FONT, fontface = "bold", size = 3.0, colour = "#202427"
  ) +
  geom_text(
    data = summary_cards, aes(x = xmin + 1.8, y = ymin + 4.5, label = subtitle),
    inherit.aes = FALSE, hjust = 0, family = MS_FONT, size = 2.35, colour = "#687176"
  ) +
  annotate("segment", x = 42.0, y = 68.0, xend = 48.0, yend = 68.0,
           colour = MS_PRIMARY, linewidth = 1.0, lineend = "round") +
  annotate("segment", x = 42.0, y = 47.0, xend = 49.0, yend = 47.0,
           colour = "#B5BABD", linewidth = .55,
           arrow = grid::arrow(length = grid::unit(1.4, "mm"), ends = "both", type = "closed")) +
  annotate("point", x = 47.0, y = 47.0, shape = 21, size = 3.0,
           fill = MS_SECONDARY, colour = "white", stroke = .35) +
  annotate("segment", x = 42.0, y = 27.0, xend = 49.0, yend = 31.0,
           colour = "#9CA3A7", linewidth = .45) +
  annotate("segment", x = 42.0, y = 31.0, xend = 49.0, yend = 27.0,
           colour = "#9CA3A7", linewidth = .45) +

  # c Observed sufficiency
  annotate("text", x = 69.0, y = 83.5, label = "c  Test observed sufficiency",
           hjust = 0, family = MS_FONT, fontface = "bold", size = 3.8, colour = "#202427") +
  annotate("text", x = 82.0, y = 77.8, label = "Example ordered axis: temporal resolution",
           family = MS_FONT, size = 2.4, colour = "#687176") +
  annotate("segment", x = 72.0, y = 34.0, xend = 94.0, yend = 34.0,
           colour = "#BFC5C8", linewidth = .5) +
  annotate("segment", x = 72.0, y = 34.0, xend = 72.0, yend = 70.0,
           colour = "#BFC5C8", linewidth = .5) +
  annotate("segment", x = 72.0, y = 51.0, xend = 94.0, yend = 51.0,
           colour = MS_SECONDARY, linewidth = .55, linetype = "22") +
  annotate("text", x = 93.8, y = 52.6, label = "tolerance ε",
           hjust = 1, family = MS_FONT, size = 2.25, colour = MS_SECONDARY) +
  geom_line(
    data = sufficiency_example |> filter(!is.na(residual)),
    aes(x = 72 + (x - 1) * 4.4, y = 34 + residual * 72),
    inherit.aes = FALSE, colour = "#8B9498", linewidth = .65
  ) +
  geom_point(
    data = sufficiency_example |> filter(!is.na(residual)),
    aes(x = 72 + (x - 1) * 4.4, y = 34 + residual * 72, fill = status),
    inherit.aes = FALSE, shape = 21, size = 3.2, stroke = .45, colour = "white"
  ) +
  scale_fill_manual(values = c(above = "#9CA4A8", sufficient = MS_PRIMARY), guide = "none") +
  annotate("point", x = 94.0, y = 38.0, shape = 21, size = 3.3,
           fill = "white", colour = "#8D9599", stroke = .6) +
  annotate("text", x = 94.0, y = 42.0, label = "boundary\nunresolved",
           family = MS_FONT, size = 2.05, colour = "#687176") +
  geom_text(
    data = sufficiency_example,
    aes(x = 72 + (x - 1) * 4.4, y = 30.8, label = as.character(state)),
    inherit.aes = FALSE, family = MS_FONT, size = 2.05, colour = "#40474B"
  ) +
  annotate("rect", xmin = 80.3, xmax = 91.8, ymin = 55.5, ymax = 59.5,
           fill = scales::alpha("#DCE8EF", .75), colour = NA) +
  annotate("text", x = 86.05, y = 57.5, label = "resolved sufficient region",
           family = MS_FONT, size = 2.15, colour = MS_PRIMARY) +
  annotate("text", x = 83.0, y = 18.5,
           label = "Sufficient when no observed higher-requirement state\ncan change the representation beyond ε",
           family = MS_FONT, size = 2.3, lineheight = 1.0, colour = "#5F686D") +

  coord_cartesian(xlim = c(0, 100), ylim = c(0, 100), expand = FALSE, clip = "off") +
  labs(
    title = "From representation change to observed sufficiency",
    subtitle = "Pair configurations, quantify what changes, then ask whether additional measurement can still exceed a chosen tolerance",
    caption = "Conceptual schematic only. RQ3 does not force monotonicity; the highest observed requirement remains boundary-unresolved because no higher observed state exists."
  ) +
  theme_void(base_family = MS_FONT) +
  theme(
    plot.title = element_text(size = 11.0, face = "bold", colour = "#202427", margin = margin(b = 1.5)),
    plot.subtitle = element_text(size = 7.8, colour = "#5F686D", margin = margin(b = 6)),
    plot.caption = element_text(size = 6.3, colour = "#70787C", hjust = 0, margin = margin(t = 4)),
    plot.margin = margin(6, 7, 5, 7)
  )

concept_path <- file.path(OUT_DIR, "FigS_representation_to_sufficiency.png")
ggsave(concept_path, p_concept, width = 10.4, height = 5.8, dpi = MS_RASTER_DPI, bg = "white")
message("Representation-to-sufficiency schematic written:\n  ", concept_path)
