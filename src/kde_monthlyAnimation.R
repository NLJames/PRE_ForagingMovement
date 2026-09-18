# Load packages
library(parallel)
library(amt)
library(sf)
library(ggplot2)
library(gganimate)
library(terra)
library(gifski)

rm(list = ls())


colPal = c(
  'navy',
  '#0c6980',  # Deep teal
  '#7eaba6',  # Seafoam
  '#e3bc5d',  # Soft amber
  '#ffd966',  # Pale gold
  '#ff8d8d',  # Coral pink
  '#6e2f2f',  # Deep burgundy brown (contrasts well with light tones)
  '#344e41'   # Olive forest green (earthy and complementary)
)

Sexcols <- colPal[c(7,5)]

# grab custom ggplot theme + theme_nature()
source("../CH2_BNForg/Scripts-HMM/src/natureTheme.R")

# Clear environment

# Load and prepare data
dd <- readRDS("~/Library/CloudStorage/OneDrive-JamesCookUniversity/Projects/THESIS_PHD_NLJ_2024/CH5_PREForaging/src_outputs/finalTemp.rds") %>%
  dplyr::mutate(
    x = lon, y = lat,
    YM = paste0(lubridate::year(dateTimeAEST), "_", sprintf("%02d", lubridate::month(dateTimeAEST))),
    featType = dplyr::if_else(is.na(featType), "NonIsland", featType)
  ) %>%
  sf::st_as_sf(coords = c("x", "y"), crs = 4326) %>%
  sf::st_transform(32756) %>%
  dplyr::mutate(
    UTMx = sf::st_coordinates(.)[, 1],
    UTMy = sf::st_coordinates(.)[, 2]
  ) %>%
  sf::st_drop_geometry()

# Keep trips with >2 points per month
keepers <- dd %>%
  dplyr::group_by(BandNumber, YM) %>%
  dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
  dplyr::filter(n > 2) %>%
  dplyr::select(BandNumber, YM)

# Create amt track object
trks <- dplyr::left_join(keepers, dd, by = c("BandNumber", "YM")) %>%
  amt::make_track(.x = UTMx, .y = UTMy, .t = dateTimeAEST, id = BandNumber, crs = 32756, all_cols = TRUE)

# List of individuals
ls.bn <- unique(trks$BandNumber)

# Set up parallel cluster
cores <- parallel::detectCores()
cl <- parallel::makeCluster(cores - 1)
parallel::clusterExport(cl, varlist = c("trks", "ls.bn"))
parallel::clusterEvalQ(cl, {
  library(dplyr)
  library(purrr)
  library(amt)
  library(sf)
  library(terra)
})

# KDE function per individual
kde_polygons_list <- parallel::parLapply(cl, ls.bn, function(bn) {
  trk_all <- dplyr::filter(trks, BandNumber == bn)
  trk_off <- dplyr::filter(trk_all, featType == "NonIsland")
  
  if (nrow(trk_all) < 5 | nrow(trk_off) < 5) return(NULL)
  
  tRast <- amt::make_trast(trk_all, res = 250)
  
  process_kde_poly <- function(trk_df, source_label) {
    if (nrow(trk_df) < 5) return(NULL)
    
    trk_nested <- trk_df %>%
      tidyr::nest(data = -YM) %>%
      dplyr::mutate(
        kde = purrr::map(data, ~ tryCatch(amt::hr_kde(.x, trast = tRast, levels = 0.95), error = function(e) NULL)),
        iso = purrr::map(kde, ~ tryCatch(amt::hr_isopleths(.x), error = function(e) NULL))
      ) %>%
      dplyr::filter(!purrr::map_lgl(iso, is.null)) %>%
      dplyr::mutate(
        iso = purrr::map2(iso, YM, ~ dplyr::mutate(.x, YM = .y, source = source_label, BandNumber = bn))
      )
    
    dplyr::bind_rows(trk_nested$iso)
  }
  
  dplyr::bind_rows(
    process_kde_poly(trk_all, "All"),
    process_kde_poly(trk_off, "OffIsland")
  )
})

# Stop cluster
parallel::stopCluster(cl)

# Combine and validate
kde_sf_all <- dplyr::bind_rows(kde_polygons_list) %>%
  sf::st_as_sf() %>%
  dplyr::mutate(valid = sf::st_is_valid(geometry)) %>%
  dplyr::mutate(geometry = dplyr::if_else(!valid, sf::st_make_valid(geometry), geometry)) %>%
  dplyr::select(-valid)%>%
  dplyr::mutate(source = factor(source, levels = c("All","OffIsland")))

# Load GBR shapefile (optional)
gbr <- sf::st_read("./data/GBR_FEATURES/egFeats.shp")

# Bounding box
bbox_kde <- sf::st_bbox(kde_sf_all)

islands_df <- data.frame(
  name = c("Heron Island", "One Tree Island"),
  lon = c(151.9149, 152.0917),
  lat = c(-23.4421, -23.5083),
  shape = c(15, 8)  # square, star
)

# Convert to sf and transform to UTM Zone 56S
islands_sf <- sf::st_as_sf(islands_df, coords = c("lon", "lat"), crs = 4326) %>%
  sf::st_transform(32756)

# Create animation
anim <- ggplot2::ggplot() +
  ggplot2::geom_sf(data = gbr, fill = "grey90", colour = "grey70") +
  ggplot2::geom_sf(data = kde_sf_all, ggplot2::aes(fill = source), colour = "black", alpha = 0.7) +
  ggplot2::geom_sf(data=islands_sf, aes(shape=name), colour='black', fill='darkgrey', size=1.7, stroke=1.2)+
  ggplot2::coord_sf(xlim = c(bbox_kde["xmin"], bbox_kde["xmax"]),
                    ylim = c(bbox_kde["ymin"], bbox_kde["ymax"])) +
  ggplot2::facet_wrap(~ BandNumber) +
  ggplot2::labs(
    title = "Monthly KDE: All vs Off-Island Points",
    subtitle = "{closest_state}",
    fill = "Data set"
  ) +
  ggplot2::scale_fill_manual(values = c("All" = "steelblue", "OffIsland" = "darkorange")) +
  ggplot2::scale_shape_manual(values = c("Heron Island" = 15, "One Tree Island" = 8)) +
  gganimate::transition_states(YM, transition_length = 3, state_length = 1) +
  gganimate::ease_aes('cubic-in-out')+ 
  ggplot2::theme(axis.text = ggplot2::element_text(size = 8)) + 
  ggplot2::scale_x_continuous(
    breaks = c(151.5, 152,151.5 )) +
  ggplot2::scale_y_continuous(
    breaks = c(-23.50,-23.40)) +
  theme_nature()

# Save animation
gganimate::animate(anim, width = 1000, height = 800, fps = 2, duration = 30,
                   renderer = gifski_renderer("./src_outputs/kde_overlay_animation.gif"))

# Save animation
gganimate::animate(anim, width = 1000, height = 800, fps = 2, duration = 30,
                   renderer = gifski_renderer("../../THESIS_SEABIRDS_2025/figures/kde_overlay_animation.gif"))

# Local copy for self-contained project folder (for sharing with reviewers)
dir.create("./figures", recursive = TRUE, showWarnings = FALSE)
file.copy("./src_outputs/kde_overlay_animation.gif", "./figures/kde_overlay_animation.gif", overwrite = TRUE)

