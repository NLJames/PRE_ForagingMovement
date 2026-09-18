

library(dplyr)
library(lubridate)
library(geosphere)

setwd("~/Library/CloudStorage/OneDrive-JamesCookUniversity/Projects/THESIS_PHD_NLJ_2024/CH5_PREForaging")


# proportion of points off island when light for various tide categories or values?
# when on island do they seem to gravitate to one spot then spread out then come back or is it random?
# Load and process tracking points, grouped by week
prePoints <- readRDS("./src_outputs/finalColDists.rds")%>%
  mutate(tideValueCats = cut(nearestTide, breaks=c(0,0.5,1,1.5,2,2.5,3,3.6), labels=c("0-0.5","0.5-1","1-1.5","1.5-2","2-2.5","2.5-3","3-3.6")),
         moonBrightness = cut(moonFraction, breaks=c(0,0.2,0.4,0.6,0.8,1), labels=c("0-0.2","0.2-0.4","0.4-0.6","0.6-0.8","0.8-1")))




# 5. Create daily summary per bird
daily_summary <- prePoints %>%
  dplyr::group_by(BandNumber, date) %>%
  dplyr::arrange(dateTimeAEST, .by_group = TRUE) %>%
  dplyr::summarise(
    LOCBANDED = dplyr::first(LOCBANDED),
    n_dayFixes = dplyr::n(),
    n_on_colony = sum(onColony, na.rm = TRUE),
    n_off_colony = n_dayFixes-n_on_colony,
    first_off_colony=if (any(onColony==0, na.rm = TRUE)) {
      min(dateTimeAEST[onColony==0], na.rm = TRUE)
    } else {
      NA
    },
    last_off_colony=if (any(onColony==0, na.rm = TRUE)) {
      max(dateTimeAEST[onColony==0], na.rm = TRUE)
    } else {
      NA
    })



light_summary <- prePoints %>%
  dplyr::filter(lightPeriod == "Light") %>%
  dplyr::group_by(BandNumber, date, nearestTideCat) %>%
  dplyr::summarise(
    n_dayFixes = dplyr::n(),
    n_off_colony = sum(onColony == 0, na.rm = TRUE),
    prop_off_colony = n_off_colony / n_dayFixes,
    .groups = "drop"
  )

tide_effects <- light_summary %>%
  dplyr::group_by(nearestTideCat) %>%
  dplyr::summarise(
    mean_prop_off = mean(prop_off_colony, na.rm = TRUE),
    sd_prop_off = sd(prop_off_colony, na.rm = TRUE),
    n_days = dplyr::n()
  )


ggplot(tide_effects, aes(x = nearestTideCat, y = mean_prop_off)) +
  geom_col() +
  geom_errorbar(aes(ymin = mean_prop_off - sd_prop_off,
                    ymax = mean_prop_off + sd_prop_off), width = 0.2) +
  labs(y = "Proportion Off Colony (Daylight)", x = "Tide Category")


on_island_spread <- prePoints %>%
  dplyr::filter(onColony == 1) %>%
  dplyr::group_by(BandNumber, date) %>%
  dplyr::summarise(
    n_on = dplyr::n(),
    max_dist = max(colDist, na.rm = TRUE),
    mean_dist = mean(colDist, na.rm = TRUE),
    sd_dist = sd(colDist, na.rm = TRUE),
    .groups = "drop"
  )

hull_areas <- prePoints %>%
  dplyr::filter(onColony == 1) %>%
  dplyr::group_by(BandNumber, date) %>%
  dplyr::summarise(
    geometry = sf::st_combine(sf::st_sfc(sf::st_multipoint(cbind(lon, lat)), crs = 4326)),
    .groups = "drop"
  ) %>%
  dplyr::mutate(
    geometry = sf::st_convex_hull(geometry),
    area_km2 = as.numeric(sf::st_area(sf::st_transform(geometry, 32756))) / 1e6
  )

on_island_tide <- prePoints %>%
  dplyr::filter(onColony == 1) %>%
  dplyr::group_by(BandNumber, date, nearestTideCat) %>%
  dplyr::summarise(
    n_on = dplyr::n(),
    max_dist = max(colDist, na.rm = TRUE),
    mean_dist = mean(colDist, na.rm = TRUE),
    sd_dist = sd(colDist, na.rm = TRUE),
    .groups = "drop"
  )

ggplot(on_island_tide, aes(x = nearestTideCat, y = mean_dist)) +
  geom_boxplot() +
  labs(
    x = "Tide Category",
    y = "Mean Distance from Colony (m) when On-Island",
    title = "Spatial Spread of On-Island Locations by Tide Category"
  )



library(ggplot2)

# Filter to on-colony points
on_island_pointsHI <- prePoints %>%
  dplyr::filter(onColony == 1,
                LOCBANDED=="HER")

table(on_island_pointsHI$BandNumber)

HIshp<-st_read("./data/GBR_FEATURES/egFeats.shp")%>%
  dplyr::filter(GBR_NAME=="Heron Island")%>%
  st_transform(4326)

ggplot() +
  geom_sf(data=HIshp)+
  geom_point(data=on_island_pointsHI,aes(x = lon, y = lat, col=nearestTideCat) )+
  #stat_density_2d(data=on_island_pointsHI, aes(x = lon, y = lat, fill = ..level..), geom = "polygon", contour = TRUE, alpha = 1) +
  #scale_fill_viridis_c() +
  labs(
    x = "Longitude",
    y = "Latitude",
    title = "Heatmap of On-Island Locations"
  )+
  facet_wrap(~BandNumber+nearestTideCat)

# Filter to on-colony points
off_island_pointsHI <- prePoints %>%
  dplyr::filter(onColony == 0,
                LOCBANDED=="HER")

Hreefshp<-st_read("./data/GBR_FEATURES/egFeats.shp")%>%
  dplyr::filter(GBR_NAME%in% c("Heron Reef", "Wistari Reef"))%>%
  st_transform(4326)

ggplot() +
  geom_sf(data=Hreefshp)+
  geom_point(data=off_island_pointsHI,aes(x = lon, y = lat, col=nearestTideCat) )+
  #stat_density_2d(data=on_island_pointsHI, aes(x = lon, y = lat, fill = ..level..), geom = "polygon", contour = TRUE, alpha = 1) +
  #scale_fill_viridis_c() +
  labs(
    x = "Longitude",
    y = "Latitude",
    title = "Heatmap of Off-Island Locations"
  )+
  facet_wrap(~tideValueCats)



on_island_pointsOTI <- prePoints %>%
  dplyr::filter(onColony == 1,
                LOCBANDED=="OTI")

OTIshp<-st_read("./data/GBR_FEATURES/egFeats.shp")%>%
  dplyr::filter(GBR_NAME=="One Tree Island")%>%
  st_transform(4326)

ggplot() +
  geom_sf(data=OTIshp)+
  geom_point(data=on_island_pointsOTI,aes(x = lon, y = lat, col=nearestTideCat) )+
  #stat_density_2d(data=on_island_pointsHI, aes(x = lon, y = lat, fill = ..level..), geom = "polygon", contour = TRUE, alpha = 1) +
  #scale_fill_viridis_c() +
  labs(
    x = "Longitude",
    y = "Latitude",
    title = "Heatmap of On-Island Locations"
  )+
  facet_wrap(~tideValueCats)


off_island_pointsOTI <- prePoints %>%
  dplyr::filter(onColony == 0,
                LOCBANDED=="OTI")

OTIshp<-st_read("./data/GBR_FEATURES/egFeats.shp")%>%
  dplyr::filter(GBR_NAME%in% c("One Tree Island Reef", "Sykes Reef"))%>%
  st_transform(4326)

ggplot() +
  geom_sf(data=OTIshp)+
  geom_point(data=off_island_pointsOTI,aes(x = lon, y = lat, col=nearestTideCat) )+
  #stat_density_2d(data=on_island_pointsHI, aes(x = lon, y = lat, fill = ..level..), geom = "polygon", contour = TRUE, alpha = 1) +
  #scale_fill_viridis_c() +
  labs(
    x = "Longitude",
    y = "Latitude",
    title = "Heatmap of On-Island Locations"
  )+
  facet_wrap(~tideValueCats)




ggplot(on_island_tide, aes(x = nearestTideCat, y = mean_dist, fill = LOCBANDED)) +
  geom_boxplot(position = position_dodge(width = 0.75)) +
  labs(
    x = "Tide Category",
    y = "Mean Distance from Colony (m) when On-Island",
    fill = "Colony",
    title = "Spatial Spread by Tide Category and Colony"
  ) +
  theme_minimal()
