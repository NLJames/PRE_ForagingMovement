library(amt)
library(sf)
library(adehabitatHR)
library(dplyr)
library(purrr)
library(furrr)
library(progressr)
library(fs)

# Create directory for saving KDE results
dir_create("./kde_results/")

# Load data
tempPoints <- readRDS("./src_outputs/finalTemp.rds") %>%
  dplyr::filter(benthicCover != "Island", geomorphicCover != "Island") %>%
  mutate(MonYr = substr(dateTimeAEST, 1, 7),
         x = lon, y = lat,
         week = floor_date(dateTimeAEST, "week"),
         weekID = paste0(BandNumber, "_", as.character(week))) %>%
  dplyr::select(BandNumber, MonYr, week, weekID, dateTimeAEST, x, y, lon, lat) %>%
  distinct(BandNumber, week, MonYr, dateTimeAEST, x, y, .keep_all = TRUE) %>%
  st_as_sf(coords = c('x', 'y'), crs = st_crs(4326)) %>%
  st_transform(32756) %>%  # Convert to UTM
  mutate(UTMx = unlist(map(.$geometry, 1)),
         UTMy = unlist(map(.$geometry, 2))) %>%
  st_drop_geometry() %>%
  droplevels()

# Filter individuals with sufficient points per week (min 5 points)
trks <- tempPoints %>%
  arrange(BandNumber, dateTimeAEST) %>%
  make_track(.x = UTMx, .y = UTMy, .t = dateTimeAEST, id = BandNumber, crs = 32756, all_cols = TRUE) %>%
  group_by(weekID) %>%
  dplyr::filter(n() > 4) %>%
  ungroup() %>%
  droplevels()

# Unique individual-week pairs
indiv_weeks <- trks %>%
  distinct(BandNumber, weekID)

# Function to calculate KDE and save per individual-week
calculate_and_save_kde <- function(i, progress) {
  progress()  # Update progress
  entry <- indiv_weeks[i, ]  # Get individual-week pair
  
  trk_subset <- filter(trks, BandNumber == entry$BandNumber, weekID == entry$weekID)
  
  if (nrow(trk_subset) < 5) return(NULL)  # Ensure enough points for KDE
  
  trk_sp <- trk_subset %>%
    dplyr::select(x = x_, y = y_)  # Ensure correct column names
  
  coordinates(trk_sp) <- ~ x + y
  proj4string(trk_sp) <- CRS("epsg:32756")
  
  kde <- kernelUD(trk_sp, h = "href", extent = 10, grid = 1000)
  
  kde_poly <- getverticeshr(kde, percent = 50)
  kde_sf <- st_as_sf(kde_poly)  # Convert to sf for spatial operations
  
  kde_sf <- kde_sf %>%
    mutate(BandNumber = entry$BandNumber, weekID = entry$weekID)
  
  # Define filename
  filename <- paste0("./kde_results/", entry$BandNumber, "_", entry$weekID, ".rds")
  
  # Save as RDS file
  saveRDS(kde_sf, filename)
  
  return(filename)  # Return filename to track saved results
}

# Run KDE calculations in parallel with progress tracking
num_cores <- detectCores()  # Leave one core free

saved_files <- with_progress({
  progress <- progressor(steps = nrow(indiv_weeks))
  future_map(1:nrow(indiv_weeks), ~ calculate_and_save_kde(.x, progress), 
             .progress = FALSE, .options = furrr_options(seed = TRUE), .workers = num_cores)
})

# Print message confirming completion
cat("KDE calculations completed. Results saved in './kde_results/'\n")


library(ggplot2)
library(sf)

# List all KDE files
# List all KDE files
kde_files <- list.files("./kde_results/", pattern = "\\.rds$", full.names = TRUE)

# Load all KDEs
kde_list <- lapply(kde_files, readRDS)

# Combine into one sf object
kde_combined <- do.call(rbind, kde_list)%>%
  mutate(BandNumber = gsub("./kde_results/([^_]*)_.*\\.rds", "\\1", kde_files),
         week_date = (gsub("^.*_(.*)\\.rds$", "\\1", kde_files)))%>%
  arrange(week_date)%>%
  st_transform(4326)
  

library(wesanderson)

num_individuals <- wes_palette("Zissou1", n= length(unique(kde_combined$BandNumber), type='continuous'))

# Use a Brewer palette that supports more colors
palette_colors <- colorRampPalette(brewer.pal(8, "Dark2"))(num_individuals)

p <- ggplot() +
  geom_sf(data = st_read("./data/GBR_FEATURES/egFeats.shp") %>% st_transform(4326)) +
  geom_sf(data = kde_combined, aes(fill = BandNumber, color = BandNumber), alpha = 0.4) +
  scale_fill_manual(values = palette_colors) +
  scale_color_manual(values = palette_colors) +
  theme_minimal() +
  labs(title = "KDE Animation for All Individuals - {closest_state}") +
  transition_states(week_date, transition_length = 3, state_length = 2) +
  ease_aes('linear') +
  theme(legend.position = "none") +
  ylim(c(-23.55, -23.4)) +
  xlim(c(151.8, 152.13))



# Adjust the frame rate (fps) and duration per frame
animate(p, 
        renderer = gifski_renderer("all_individuals_kde.gif"), 
        width = 1000, height = 700, 
        fps = 5,  # Lower fps = slower animation (try 3-5 for best effect)
        duration = length(unique(kde_combined$week_date)) * 0.8)  # Control total animation time

library(ggmap)
library(ggplot2)
library(sf)
library(gganimate)
library(mapedit)
install.packages("basemaps")
library(basemaps)

ext<-draw_ext()
bm<-basemap(ext, map_service = "mapbox", map_type = "satellite")

# Define bounding box (change based on study area)
bbox <- c(left = 151.8, bottom = -23.55, right = 152.2, top = -23.4)
ggmap::register_stadiamaps()

devtools::install_github("16EAGLE/basemaps")

set_defaults(map_service = "mapbox", map_type = "satellite")
basemap_magick(ext)

# Get ESRI World Imagery basemap
esri_basemap <- get_stadiamap(bbox, zoom = 10, source = "esri")

# Plot basemap to check
ggmap(esri_basemap)


p <- ggmap(esri_basemap) +
  geom_sf(data = kde_combined, aes(fill = BandNumber, color = BandNumber), alpha = 0.4, inherit.aes = FALSE) +
  scale_fill_viridis_d() +
  scale_color_viridis_d() +
  theme_minimal() +
  labs(title = "KDE Animation for All Individuals - {closest_state}") +
  transition_states(week_date, transition_length = 3, state_length = 2) +
  ease_aes('linear') +
  theme(legend.position = "none")+
  ylim(c(-23.55, -23.4)) +
  xlim(c(151.8, 152.2))


# Save as GIF
animate(p, renderer = gifski_renderer("kde_esri_basemap.gif"), width = 1000, height = 700, fps = 5)


